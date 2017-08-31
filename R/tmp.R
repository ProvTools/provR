
library(methods)

setClass("DDGStatementPos", slots = list(startLine = "numeric", startCol = "numeric", 
    endLine = "numeric", endCol = "numeric"))

setMethod("initialize", "DDGStatementPos", function(.Object, parseData) {
    if (length(parseData) == 1 && is.na(parseData)) {
        .Object@startLine <- -1
        .Object@startCol <- -1
        .Object@endLine <- -1
        .Object@endCol <- -1
    } else {
        
        .Object@startLine <- parseData$line1
        .Object@startCol <- parseData$col1
        .Object@endLine <- parseData$line2
        .Object@endCol <- parseData$col2
    }
    return(.Object)
})






setClass("DDGStatement", slots = list(text = "character", parsed = "expression", 
    abbrev = "character", annotated = "expression", vars.used = "character", vars.set = "character", 
    vars.possibly.set = "character", isDdgFunc = "logical", readsFile = "logical", 
    writesFile = "logical", createsGraphics = "logical", updatesGraphics = "logical", 
    has.dev.off = "logical", pos = "DDGStatementPos", script.num = "numeric", is.breakpoint = "logical", 
    contained = "list"))

setMethod("initialize", "DDGStatement", function(.Object, parsed, pos, script.name, 
    script.num, breakpoints, parseData) {
    .Object@parsed <- parsed
    
    .Object@text <- paste(deparse(.Object@parsed[[1]]), collapse = "")
    
    .Object@abbrev <- if (grepl("^ddg.eval", .Object@text)) {
        .ddg.abbrev.cmd(.Object@parsed[[1]][[2]])
    } else {
        .ddg.abbrev.cmd(.Object@text)
    }
    
    vars.used <- .ddg.find.var.uses(.Object@parsed[[1]])
    
    if (length(parsed) > 0 && !is.symbol(parsed[[1]]) && parsed[[1]][[1]] == "for") {
        index.var <- c(parsed[[1]][[2]])
        vars.used <- vars.used[!vars.used %in% index.var]
    }
    
    .Object@vars.used <- vars.used
    
    .Object@vars.set <- .ddg.find.simple.assign(.Object@parsed[[1]])
    
    .Object@vars.possibly.set <- .ddg.find.assign(.Object@parsed[[1]])
    
    .Object@isDdgFunc <- grepl("^ddg.", .Object@text) & !grepl("^ddg.eval", .Object@text)
    
    .Object@readsFile <- .ddg.reads.file(.Object@parsed[[1]])
    .Object@writesFile <- .ddg.writes.file(.Object@parsed[[1]])
    .Object@createsGraphics <- .ddg.creates.graphics(.Object@parsed[[1]])
    .Object@updatesGraphics <- .ddg.updates.graphics(.Object@parsed[[1]])
    .Object@has.dev.off <- .ddg.has.call.to(.Object@parsed[[1]], "dev.off")
    
    .Object@pos <- if (is.object(pos)) {
        pos
    } else {
        null.pos()
    }
    
    .Object@script.num <- if (is.na(script.num)) 
        -1 else script.num
    
    .Object@is.breakpoint <- if (is.object(breakpoints)) {
        if (.ddg.is.assign(.Object@parsed[[1]]) && .ddg.is.functiondecl(.Object@parsed[[1]][[3]])) {
            is.breakpoint <- any(breakpoints$lnum == .Object@pos@startLine)
        } else {
            is.breakpoint <- any(breakpoints$lnum >= .Object@pos@startLine & breakpoints$lnum <= 
                .Object@pos@endLine)
        }
    } else {
        is.breakpoint <- FALSE
    }
    
    .Object@contained <- .ddg.parse.contained(.Object, script.name, parseData)
    
    .Object@annotated <- if (grepl("^ddg.eval", .Object@text)) {
        parse(text = .Object@parsed[[1]][[2]])
    } else {
        .ddg.add.annotations(.Object)
    }
    return(.Object)
})

null.pos <- function() {
    return(new(Class = "DDGStatementPos", NA))
}

.ddg.construct.DDGStatement <- function(expr, pos, script.name, script.num, breakpoints, 
    parseData) {
    if (is.numeric(expr)) 
        expr <- parse(text = expr)
    
    return(new(Class = "DDGStatement", parsed = expr, pos, script.name, script.num, 
        breakpoints, parseData))
}



.ddg.abbrev.cmd <- function(cmd, len = 60) {
    if (length(cmd) > 1) {
        cmd <- paste(cmd, collapse = " ")
    }
    
    if (file.exists(cmd)) 
        basename(cmd) else if (nchar(cmd) <= len) 
        cmd else if (substr(cmd, len, len) != "\\") 
        substr(cmd, 1, len) else if (substr(cmd, len - 1, len) == "\\\\") 
        substr(cmd, 1, len) else substr(cmd, 1, len - 1)
}



.ddg.find.var.uses <- function(main.object) {
    .ddg.find.var.uses.rec <- function(obj) {
        
        if (is.atomic(obj)) {
            return(character())
        }
        
        if (is.name(obj)) {
            if (nchar(obj) == 0) 
                return(character())
            
            if (nchar(obj) == 1 && !grepl("[[:alpha:]]", obj)) 
                return(character())
            return(deparse(obj))
        }
        
        if (!is.recursive(obj)) 
            return(character())
        
        if (.ddg.is.functiondecl(obj)) 
            return(character())
        
        tryCatch({
            if (.ddg.is.assign(obj)) {
                
                
                if (is.symbol(obj[[2]])) {
                  unique(unlist(.ddg.find.var.uses.rec(obj[[3]])))
                } else if (is.call(obj[[2]])) {
                  variables <- c(.ddg.find.var.uses.rec(obj[[2]][[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]])))
                  
                  if (obj[[2]][[1]] == "[") 
                    append(variables, .ddg.find.var.uses.rec(obj[[2]][[3]]))
                  
                  unique(variables)
                } else if (is.character(obj[[2]])) {
                  unique(c(unlist(.ddg.find.var.uses.rec(parse(text = obj[[2]])[[1]])), 
                    unlist(.ddg.find.var.uses.rec(parse(text = obj[[3]])[[1]]))))
                } else {
                  unique(c(.ddg.find.var.uses.rec(obj[[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
                }
            } else {
                unique(unlist(lapply(obj[1:length(obj)], .ddg.find.var.uses.rec)))
            }
        }, error = function(e) {
            print(paste(".ddg.find.var.uses.rec:  Error analyzing", deparse(obj)))
            character()
        })
    }
    return(.ddg.find.var.uses.rec(main.object))
}




.ddg.find.simple.assign <- function(obj) {
    if (.ddg.is.assign(obj)) {
        .ddg.get.var(obj[[2]])
    } else {
        ""
    }
}




.ddg.is.assign <- function(expr) {
    if (is.call(expr)) {
        if (identical(expr[[1]], as.name("<-"))) 
            return(TRUE) else if (identical(expr[[1]], as.name("<<-"))) 
            return(TRUE) else if (identical(expr[[1]], as.name("="))) 
            return(TRUE) else if (identical(expr[[1]], as.name("assign"))) 
            return(TRUE)
    }
    return(FALSE)
}




.ddg.get.var <- function(lvalue) {
    if (is.symbol(lvalue)) 
        deparse(lvalue) else if (is.character(lvalue)) 
        .ddg.get.var(parse(text = lvalue)[[1]]) else .ddg.get.var(lvalue[[2]])
}




.ddg.find.assign <- function(obj) {
    if (!is.recursive(obj)) 
        return(character())
    
    if (.ddg.is.assign(obj)) {
        var <- .ddg.get.var(obj[[2]])
        
        if (!(is.null(obj[[3]]))) {
            if (.ddg.is.functiondecl(obj[[3]])) 
                var else c(var, unlist(lapply(obj[[3]], .ddg.find.assign)))
        } else var
    } else {
        unique(unlist(lapply(obj, .ddg.find.assign)))
    }
}




.ddg.is.functiondecl <- function(expr) {
    if (is.symbol(expr) || !is.language(expr)) 
        return(FALSE)
    if (is.null(expr[[1]]) || !is.language(expr[[1]])) 
        return(FALSE)
    return(expr[[1]] == "function")
}


.ddg.get.statement.type <- function(parsed.command) {
    if (length(parsed.command) > 1) 
        return(parsed.command[[1]])
    return(0)
}


.ddg.add.annotations <- function(command) {
    parsed.command <- command@parsed[[1]]
    
    if (length(parsed.command) == 0) 
        return(command@parsed)
    
    if (is.call(parsed.command) && parsed.command[[1]] == "source") {
        return(.ddg.add.ddg.source(parsed.command))
    }
    
    if (.ddg.is.assign(parsed.command) && .ddg.is.functiondecl(parsed.command[[3]])) {
        return(.ddg.add.function.annotations(command))
    }
    
    statement.type <- as.character(.ddg.get.statement.type(parsed.command))
    loop.types <- list("for", "while", "repeat")
    if (length(statement.type > 0) && !is.null(statement.type)) {
        if (statement.type == "if") {
            return(.ddg.annotate.if.statement(command))
        } else if (statement.type %in% loop.types) {
            return(.ddg.annotate.loop.statement(command, statement.type))
        } else if (statement.type == "{") {
            return(.ddg.annotate.simple.block(command))
        }
    }
    
    return(command@parsed)
}


.ddg.parse.contained <- function(cmd, script.name, parseData) {
    parsed.cmd <- cmd@parsed[[1]]
    
    if (.ddg.is.assign(parsed.cmd) && .ddg.is.functiondecl(parsed.cmd[[3]])) {
        return(.ddg.parse.contained.function(cmd, script.name, parseData, parsed.cmd[[3]][[3]]))
    } else if (ddg.max.loops() == 0) {
        return(list())
    }
    
    st.type <- as.character(.ddg.get.statement.type(parsed.cmd))
    
    if (st.type == "if") {
        return(.ddg.parse.contained.if(cmd, script.name, parseData, parsed.cmd))
    } else {
        control.types <- list("for", "while", "repeat", "{")
        if (length(st.type) > 0 && !is.null(st.type) && (st.type %in% control.types)) {
            return(.ddg.parse.contained.control(cmd, script.name, parseData, parsed.cmd, 
                st.type))
        }
    }
    
    return(list())
}

.ddg.parse.contained.function <- function(cmd, script.name, parseData, func.body) {
    if (func.body[[1]] == "{") {
        func.stmts <- func.body[2:length(func.body)]
    } else {
        func.stmts <- list(func.body)
    }
    
    return(.ddg.create.DDGStatements(func.stmts, script.name, cmd@script.num, parseData, 
        cmd@pos))
}

.ddg.parse.contained.if <- function(cmd, script.name, parseData, parent) {
    block.stmts <- list()
    
    while (!is.symbol(parent) && parent[[1]] == "if") {
        block <- parent[[3]]
        block <- .ddg.ensure.in.block(block)
        
        for (i in 2:(length(block))) {
            block.stmts <- c(block.stmts, block[[i]])
        }
        
        if (length(parent) == 4) {
            final.else <- TRUE
        } else {
            final.else <- FALSE
        }
        
        parent <- parent[[(length(parent))]]
    }
    
    if (final.else) {
        block <- parent
        block <- .ddg.ensure.in.block(block)
        
        for (i in 2:(length(block))) {
            block.stmts <- c(block.stmts, block[[i]])
        }
    }
    
    return(.ddg.create.DDGStatements(block.stmts, script.name, cmd@script.num, parseData, 
        cmd@pos))
}

.ddg.ensure.in.block <- function(block) {
    if (is.symbol(block) || block[[1]] != "{") 
        call("{", block) else block
}


.ddg.parse.contained.control <- function(cmd, script.name, parseData, parsed.cmd, 
    st.type) {
    block.stmts <- list()
    
    if (st.type == "for") 
        block <- parsed.cmd[[4]] else if (st.type == "while") 
        block <- parsed.cmd[[3]] else if (st.type == "repeat") 
        block <- parsed.cmd[[2]] else if (st.type == "{") 
        block <- parsed.cmd
    
    block <- .ddg.ensure.in.block(block)
    
    for (i in 2:length(block)) {
        block.stmts <- c(block.stmts, block[[i]])
    }
    
    return(.ddg.create.DDGStatements(block.stmts, script.name, cmd@script.num, parseData, 
        cmd@pos))
}


.ddg.add.ddg.source <- function(parsed.command) {
    script.name <- deparse(parsed.command[[2]])
    parsed.command.txt <- paste("ddg.source(", script.name, ")", sep = "")
    return(parse(text = parsed.command.txt))
}


.ddg.add.function.annotations <- function(function.decl) {
    parsed.function.decl <- function.decl@parsed[[1]]
    
    func.name <- toString(parsed.function.decl[[2]])
    
    func.definition <- parsed.function.decl[[3]]
    
    if (func.definition[[3]][[1]] != "{") {
        func.definition <- .ddg.create.function.block(func.definition)
    }
    
    func.definition <- .ddg.add.conditional.statement(func.definition, func.name)
    
    if (!.ddg.has.call.to(func.definition[[3]], "ddg.function")) {
        func.definition <- .ddg.insert.ddg.function(func.definition)
    }
    
    if (!.ddg.has.call.to(func.definition[[3]], "ddg.return.value")) {
        func.definition <- .ddg.wrap.all.return.parameters(func.definition, function.decl@contained)
    }
    
    last.statement <- .ddg.find.last.statement(func.definition)
    
    if (!.ddg.is.call.to(last.statement, "ddg.return.value") & !.ddg.is.call.to(last.statement, 
        "return") & !.ddg.is.call.to.ddg.function(last.statement)) {
        func.definition <- .ddg.wrap.last.line(func.definition, function.decl@contained)
    }
    
    if (!.ddg.has.call.to(func.definition, "ddg.eval")) {
        func.definition <- .ddg.wrap.with.ddg.eval(func.definition, function.decl@contained)
    }
    
    return(as.expression(call("<-", as.name(func.name), func.definition)))
}


.ddg.create.function.block <- function(func.definition) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    new.func.body <- call("{", func.body)
    return(call("function", func.params, as.call(new.func.body)))
}


.ddg.add.conditional.statement <- function(func.definition, func.name) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    pos <- length(func.body)
    
    new.func.body.txt <- c(paste("if (ddg.should.run.annotated(\"", func.name, "\")) {", 
        sep = ""), as.list(func.body[2:pos]), paste("} else {", sep = ""), as.list(func.body[2:pos]), 
        paste("}", sep = ""))
    
    new.func.expr <- parse(text = new.func.body.txt)
    new.func.body <- new.func.expr[[1]]
    
    return(call("function", func.params, call("{", new.func.body)))
}


.ddg.insert.ddg.function <- function(func.definition) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    block <- func.body[[2]][[3]]
    pos <- length(block)
    
    inserted.statement <- call("ddg.function")
    new.statements.txt <- c(as.list("{"), inserted.statement, as.list(block[2:pos]), 
        as.list("}"))
    block <- parse(text = new.statements.txt)[[1]]
    
    func.body[[2]][[3]] <- block
    
    return(call("function", func.params, as.call(func.body)))
}


.ddg.wrap.return.parameters <- function(block, parsed.stmts) {
    pos <- length(block)
    
    for (i in 1:pos) {
        statement <- block[[i]]
        if (.ddg.has.call.to(statement, "return")) {
            if (.ddg.is.call.to(statement, "return")) {
                if (length(statement) == 1) {
                  ret.params <- ""
                } else {
                  ret.params <- statement[[2]]
                }
                if (is.list(parsed.stmts)) {
                  parsed.stmt <- parsed.stmts[[i - 2]]
                } else {
                  parsed.stmt <- parsed.stmts
                }
                if (.ddg.has.call.to(ret.params, "return")) {
                  ret.params <- .ddg.wrap.return.parameters(ret.params, parsed.stmt)
                }
                
                new.ret.params <- .ddg.create.ddg.return.call(ret.params, parsed.stmt)
                new.statement <- call("return", new.ret.params)
                block[[i]] <- new.statement
                
            } else {
                if (is.list(parsed.stmts)) {
                  parsed.stmt <- parsed.stmts[[i - 2]]
                } else {
                  parsed.stmt <- parsed.stmts
                }
                block[[i]] <- .ddg.wrap.return.parameters(statement, parsed.stmt)
            }
        }
    }
    
    return(block)
}


.ddg.wrap.all.return.parameters <- function(func.definition, parsed.stmts) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    block <- func.body[[2]][[3]]
    pos <- length(block)
    
    block <- .ddg.wrap.return.parameters(block, parsed.stmts)
    
    func.body[[2]][[3]] <- block
    
    return(call("function", func.params, as.call(func.body)))
}


.ddg.find.last.statement <- function(func.definition) {
    func.body <- func.definition[[3]]
    
    block <- func.body[[2]][[3]]
    pos <- length(block)
    
    return(block[[pos]])
}


.ddg.wrap.last.line <- function(func.definition, parsed.stmts) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    block <- func.body[[2]][[3]]
    pos <- length(block)
    
    last.statement <- block[[pos]]
    parsed.stmt <- parsed.stmts[[length(parsed.stmts)]]
    
    wrapped.statement <- .ddg.create.ddg.return.call(last.statement, parsed.stmt)
    func.body[[2]][[3]][[pos]] <- wrapped.statement
    
    return(call("function", func.params, as.call(func.body)))
}


.ddg.create.ddg.return.call <- function(last.statement, parsed.stmt) {
    force(parsed.stmt)
    if (.ddg.has.call.to(last.statement, "return")) {
        return(call("ddg.return.value", last.statement, function() parsed.stmt))
    } else {
        eval.cmd <- .ddg.construct.DDGStatement(parse(text = deparse(last.statement)), 
            pos = NA, script.num = NA, breakpoints = NA, parseData = NULL)
        new.statement <- .ddg.create.ddg.eval.call(last.statement, parsed.stmt)
        return(call("ddg.return.value", new.statement, function() parsed.stmt))
    }
}


.ddg.create.ddg.eval.call <- function(statement, parsed.stmt) {
    force(parsed.stmt)
    
    return(call("ddg.eval", paste(deparse(statement), collapse = ""), function() parsed.stmt))
}


.ddg.wrap.with.ddg.eval <- function(func.definition, parsed.stmts) {
    func.params <- func.definition[[2]]
    
    func.body <- func.definition[[3]]
    
    block <- func.body[[2]][[3]]
    pos <- length(block)
    
    for (i in 2:pos) {
        statement <- block[[i]]
        if (!grepl("^ddg.", statement[1]) & !.ddg.has.call.to(statement, "ddg.return.value")) {
            parsed.stmt <- parsed.stmts[[i - 2]]
            new.statement <- .ddg.create.ddg.eval.call(statement, parsed.stmt)
            func.body[[2]][[3]][[i]] <- new.statement
        }
    }
    
    return(call("function", func.params, as.call(func.body)))
}


.ddg.create.block.ddg.eval.call <- function(statement, parsed.stmt) {
    .ddg.inc("ddg.statement.num")
    num <- .ddg.statement.num()
    .ddg.add.ddgstatement(parsed.stmt)
    
    return(call("ddg.eval", paste(deparse(statement), collapse = ""), num))
}


.ddg.wrap.block.with.ddg.eval <- function(block, parsed.stmts) {
    for (i in 2:length(block)) {
        statement <- block[[i]]
        if (!grepl("^ddg.", statement) && !.ddg.has.call.to(statement, "ddg.return.value")) {
            parsed.stmt <- parsed.stmts[[i - 1]]
            
            new.statement <- .ddg.create.block.ddg.eval.call(statement, parsed.stmt)
            block[[i]] <- new.statement
        }
    }
    return(block)
}


.ddg.add.block.start.finish <- function(block, pname) {
    start.statement <- deparse(call("ddg.start", pname))
    finish.statement <- deparse(call("ddg.finish", pname))
    
    pos <- length(block)
    statements <- deparse(block[[2]])
    if (pos > 2) {
        for (i in 3:pos) {
            statements <- append(statements, deparse(block[[i]]))
        }
    }
    
    block.txt <- paste(c("{", start.statement, statements, finish.statement, "}"), 
        collapse = "\n")
    block.parsed <- parse(text = block.txt)
    
    return(block.parsed[[1]])
}


.ddg.insert.ddg.forloop <- function(block, index.var) {
    pos <- length(block)
    inserted.statement <- call("ddg.forloop", index.var)
    
    if (pos == 2) {
        new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2]))
        return(as.call(new.statements))
    } else {
        new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2:pos]))
        return(as.call(new.statements))
    }
}


.ddg.insert.ddg.loop.annotate <- function(block, var) {
    pos <- length(block)
    if (var == "on") 
        inserted.statement <- call("ddg.loop.annotate.on") else if (var == "off") 
        inserted.statement <- call("ddg.loop.annotate.off")
    
    if (pos == 2) {
        new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2]))
        return(as.call(new.statements))
    } else {
        new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2:pos]))
        return(as.call(new.statements))
    }
}


.ddg.annotate.if.statement <- function(command) {
    if (ddg.max.loops() == 0) {
        parsed.command.txt <- deparse(command@parsed[[1]])
    } else {
        parsed.command <- command@parsed[[1]]
        parsed.stmts <- command@contained
        
        bnum <- 1
        ptr <- 0
        parent <- parsed.command
        parsed.command.txt <- vector()
        
        while (!is.symbol(parent) && parent[[1]] == "if") {
            block <- parent[[3]]
            block <- .ddg.ensure.in.block(block)
            
            block.stmts <- list()
            for (i in 1:(length(block) - 1)) {
                block.stmts <- c(block.stmts, parsed.stmts[[i + ptr]])
            }
            
            ptr <- ptr + length(block) - 1
            
            block <- .ddg.wrap.block.with.ddg.eval(block, block.stmts)
            
            block <- .ddg.add.block.start.finish(block, "if")
            
            cond <- paste(deparse(parent[[2]]), collapse = "")
            if (bnum == 1) {
                statement.txt <- paste(c(paste("if (", cond, ")", sep = ""), deparse(block), 
                  collapse = "\n"))
            } else {
                statement.txt <- paste(c(paste("} else if (", cond, ")", sep = ""), 
                  deparse(block), collapse = "\n"))
            }
            
            if (bnum > 1) {
                last <- length(parsed.command.txt) - 2
                parsed.command.txt <- parsed.command.txt[c(1:last)]
            }
            parsed.command.txt <- append(parsed.command.txt, statement.txt)
            
            if (length(parent) == 4) {
                final.else <- TRUE
            } else {
                final.else <- FALSE
            }
            
            bnum <- bnum + 1
            parent <- parent[[(length(parent))]]
        }
        
        if (final.else) {
            block <- parent
            block <- .ddg.ensure.in.block(block)
            
            block.stmts <- list()
            for (i in 1:(length(block) - 1)) {
                block.stmts <- c(block.stmts, parsed.stmts[[i + ptr]])
            }
            
            block <- .ddg.wrap.block.with.ddg.eval(block, block.stmts)
            
            block <- .ddg.add.block.start.finish(block, "if")
            
            statement.txt <- paste(c(paste("} else", sep = ""), deparse(block), collapse = ""))
            
            last <- length(parsed.command.txt) - 2
            parsed.command.txt <- parsed.command.txt[c(1:last)]
            parsed.command.txt <- append(parsed.command.txt, statement.txt)
        }
    }
    
    parsed.command.txt <- append(parsed.command.txt, "ddg.set.inside.loop()", after = 0)
    parsed.command.txt <- append(parsed.command.txt, "ddg.not.inside.loop()")
    return(parse(text = parsed.command.txt))
}


.ddg.annotate.loop.statement <- function(command, loop.type) {
    if (ddg.max.loops() == 0) {
        parsed.command.txt <- deparse(command@parsed[[1]])
    } else {
        
        parsed.command <- command@parsed[[1]]
        
        .ddg.add.loop()
        .ddg.inc("ddg.loop.num")
        ddg.loop.num <- .ddg.loop.num()
        
        if (loop.type == "for") {
            block <- parsed.command[[4]]
        } else if (loop.type == "while") {
            block <- parsed.command[[3]]
        } else {
            block <- parsed.command[[2]]
        }
        
        block <- .ddg.ensure.in.block(block)
        
        annotated.block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)
        
        if (loop.type == "for") {
            index.var <- parsed.command[[2]]
            annotated.block <- .ddg.insert.ddg.forloop(annotated.block, index.var)
        }
        
        annotated.block <- .ddg.add.block.start.finish(annotated.block, paste(loop.type, 
            "loop"))
        
        block <- .ddg.insert.ddg.loop.annotate(block, "off")
        
        block.txt <- deparse(block)
        annotated.block.txt <- deparse(annotated.block)
        
        if (loop.type == "for") {
            firstLine <- paste("for (", deparse(parsed.command[[2]]), " in ", deparse(parsed.command[[3]]), 
                ") {", sep = "")
        } else if (loop.type == "while") {
            firstLine <- paste("while (", deparse(parsed.command[[2]]), ") {", sep = "")
        } else {
            firstLine <- paste("repeat {", sep = "")
        }
        
        parsed.command.txt <- paste(c(firstLine, paste("if (ddg.loop.count.inc(", 
            ddg.loop.num, ") >= ddg.first.loop() && ddg.loop.count(", ddg.loop.num, 
            ") <= ddg.first.loop() + ddg.max.loops() - 1)", sep = ""), annotated.block.txt, 
            paste("else", sep = ""), block.txt, paste("}", sep = ""), paste("if (ddg.loop.count(", 
                ddg.loop.num, ") > ddg.first.loop() + ddg.max.loops() - 1) ddg.details.omitted()", 
                sep = ""), paste("ddg.reset.loop.count(", ddg.loop.num, ")", sep = ""), 
            paste("if (ddg.max.loops() != 0) ddg.loop.annotate.on()"), collapse = "\n"))
    }
    
    parsed.command.txt <- append(parsed.command.txt, "ddg.set.inside.loop()", after = 0)
    parsed.command.txt <- append(parsed.command.txt, "ddg.not.inside.loop()")
    return(parse(text = parsed.command.txt))
}


.ddg.annotate.simple.block <- function(command) {
    parsed.command <- command@parsed[[1]]
    
    block <- parsed.command
    
    block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)
    
    block <- .ddg.add.block.start.finish(block, "block")
    
    block.txt <- deparse(block)
    
    return(parse(text = block.txt))
}


.ddg.is.call.to <- function(parsed.expr, func.name) {
    if (is.call(parsed.expr)) {
        if (parsed.expr[[1]] == func.name) {
            return(TRUE)
        }
    }
    return(FALSE)
}


.ddg.has.call.to <- function(parsed.expr, func.name) {
    if (!is.recursive(parsed.expr)) 
        return(FALSE)
    
    if (.ddg.is.functiondecl(parsed.expr)) 
        return(FALSE)
    
    if (.ddg.is.call.to(parsed.expr, func.name)) {
        return(TRUE)
    } else {
        return(any(sapply(parsed.expr, function(parsed.expr) {
            return(.ddg.has.call.to(parsed.expr, func.name))
        })))
    }
}


.ddg.is.call.to.ddg.function <- function(parsed.expr) {
    if (is.call(parsed.expr)) {
        if (grepl("^ddg.", parsed.expr[1])) {
            return(TRUE)
        }
    }
    return(FALSE)
}

.ddg.reads.file <- function(parsed.statement) {
    .ddg.file.read.functions.df <- .ddg.get(".ddg.file.read.functions.df")
    reading.functions <- .ddg.file.read.functions.df$function.names
    return(TRUE %in% (lapply(reading.functions, function(fun.name) {
        return(.ddg.has.call.to(parsed.statement, fun.name))
    })))
}

.ddg.writes.file <- function(parsed.statement) {
    .ddg.file.write.functions.df <- .ddg.get(".ddg.file.write.functions.df")
    writing.functions <- .ddg.file.write.functions.df$function.names
    return(TRUE %in% (lapply(writing.functions, function(fun.name) {
        return(.ddg.has.call.to(parsed.statement, fun.name))
    })))
}

.ddg.creates.graphics <- function(parsed.statement) {
    .ddg.graphics.functions.df <- .ddg.get(".ddg.graphics.functions.df")
    graphics.functions <- .ddg.graphics.functions.df$function.names
    if (TRUE %in% (lapply(graphics.functions, function(fun.name) {
        return(.ddg.has.call.to(parsed.statement, fun.name))
    }))) {
        return(TRUE)
    }
    return(FALSE)
}

.ddg.updates.graphics <- function(parsed.statement) {
    graphics.update.functions <- .ddg.get(".ddg.graphics.update.functions")
    if (TRUE %in% (lapply(graphics.update.functions, function(fun.name) {
        return(.ddg.has.call.to(parsed.statement, fun.name))
    }))) {
        return(TRUE)
    }
    return(FALSE)
}
