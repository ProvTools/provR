# Copyright (C) 2017 Harvard University, Mount Holyoke College
#
# This file is part of ProvR.
#
# ProvR is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# ProvR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ProvR; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
# This package was forked from <https://github.com/End-to-end-provenance/RDataTracker>
#
# Contact: Matthew Lau <matthewklau@fas.harvard.edu>

# Create DDG environment variable.
.ddg.env <- new.env(parent = emptyenv())

# Set the number of lines the history file keeps (and therefore can be analyzed).
# Note: this setting has no effect on some systems.
ddg.MAX_HIST_LINES <- 2^14

# .ddg.init.environ() sets up the filesystem and R environments for use.
.ddg.init.environ <- function() {
    dir.create(.global.get("ddg.path"), showWarnings = FALSE)
    dir.create(paste(.global.get("ddg.path"), "/data", sep = ""), showWarnings = FALSE)
    dir.create(paste(.global.get("ddg.path"), "/debug", sep = ""), showWarnings = FALSE)
    dir.create(paste(.global.get("ddg.path"), "/scripts", sep = ""), showWarnings = FALSE)
    if (interactive() && .global.get(".ddg.enable.console")) {
        .global.set("ddg.original.hist.size", Sys.getenv("R_HISTSIZE"))
        Sys.setenv(R_HISTSIZE = ddg.MAX_HIST_LINES)
    }
}

# .ddg.sourced.script.names returns a string containing the names of sourced
# scripts, if any. If no scripts were sourced it returns an empty string.

.ddg.sourced.script.names <- function() {
    ss <- .global.get(".ddg.sourced.scripts")
    # First row is main script.
    if (nrow(ss) == 1)
        snames <- "" else {
        snames <- ss[ss$snum >= 1, "sname"]
        snames <- paste0(snames, collapse = ",")
    }
    return(snames)
}

# ddg.installedpackages() returns information on packages installed at the time
# of execution and their versions.
.ddg.installedpackages <- function() {
    packages <- devtools::session_info()
    packages <- packages[[2]]
    installed <- packages[packages[, 2] == "*", ]
    installed <- installed[, c(1, 3)]
    return(installed)
}

# .ddg.is.simple returns TRUE if the value passed in is a simple data value which
# should be saved locally as opposed to stored in a separate file. The assumption
# is that the value passed in has already been declared not to be a graphic.

# value - input value.

.ddg.is.simple <- function(value) {
    # Note that is.vector returns TRUE for lists, so we need to check lists
    # separately.  Since every value in a list can have a different type, if it is a
    # list, we will assume the value is complex. We consider NULL values to be
    # simple.
    return((!.ddg.is.graphic(value) && !is.list(value) && is.vector(value) && length(value) ==
        1) || is.null(value))
}

# .ddg.is.csv returns TRUE if the value passed in should be saved as a csv file,
# i.e. if it is a vector, matrix, or data frame.  Note that is.vector returns
# TRUE for lists.

# value - input value.

.ddg.is.csv <- function(value) {
    return(!.ddg.is.simple(value) && ((is.vector(value) && !is.list(value)) || is.matrix(value) ||
        is.data.frame(value)))
}


# .ddg.is.object returns TRUE if the value is determined to be an object by our
# standards.

# value - input value.

.ddg.is.object <- function(value) {
    return(is.object(value) || is.environment(value))
}

# .ddg.is.function returns TRUE if the value is determined to be a function or we
# want to save it as a function.

# value - input value.

.ddg.is.function <- function(value) {
    return(is.function(value))
}

# .ddg.dev.change determines whether or not a new graphic device has become
# active and whether or not we should capture the previous graphic device. It
# returns the device number we should capture (0 means we shouldn't capture any
# device).

.ddg.dev.change <- function() {
    prev.device <- .global.get("prev.device")
    curr.device <- dev.cur()
    device.list <- dev.list()
    # We've switched devices .
    if (prev.device != curr.device) {
        # Update device.
        .global.set("prev.device", curr.device)
        # Previous device still accessible.
        if (prev.device %in% device.list)
            return(prev.device)
    }
    # No switching, or previous is not accessible (NULL or removed).
    return(0)
}



# .ddg.write.csv takes as input a name-value pair for a variable and attempts to
# save the data as a csv file. It does not create any edges but does add the node
# to the DDG. Edge creation should occur from wherever this function is called.

# name - data node name.  value - data node value.  scope - data node scope.

.ddg.write.csv <- function(name, value, scope = NULL, from.env = FALSE) {
    tryCatch({
        .snapshot.node(name, "csv", value, dscope = scope, from.env = from.env)
    }, error = function(e) {
        .snapshot.node(name, "txt", value, save.object = TRUE, dscope = scope,
            from.env = from.env)
    })
}

# Returns a string representation of the type information of the given value.
.ddg.get.val.type.string <- function(value) {
    val.type <- .ddg.get.val.type(value)
    if (is.null(val.type))
        return("null")
    # list, object, environment, function, language
    if (length(val.type) == 1)
        return(paste("\"", val.type, "\"", sep = ""))
    # vector, matrix, array, data frame type information recorded in a list of 3
    # vectors (container,dimension,type)
    container <- val.type[[1]]
    dimension <- val.type[[2]]
    type <- val.type[[3]]
    # vector: a 1-dimensional array (uniform typing)
    if (identical(container, "vector"))
        return(paste("{\"container\":\"vector\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    # matrix: a 2-dimensional array (uniform typing)
    if (identical(container, "matrix")) {
        dimension <- paste(dimension, collapse = ",")
        return(paste("{\"container\":\"matrix\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    }
    # array: n-dimensional (uniform typing)
    if (identical(container, "array")) {
        dimension <- paste(dimension, collapse = ",")
        return(paste("{\"container\":\"array\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    }
    # data frame: is a type of list
    dimension <- paste(dimension, collapse = ",")
    type <- paste(type, collapse = "\",\"")
    return(paste("{\"container\":\"data_frame\", \"dimension\":[", dimension, "], \"type\":[\"",
        type, "\"]}", sep = ""))
}


# Returns the type information of the value of the given variable.  Does not
# contain information on dimensions.

.ddg.get.val.type.from.var <- function(var) {
    val.type <- .ddg.get.val.type(get(var))
    # remove dimension information, if any
    if (length(val.type) > 1)
        val.type[2] <- NULL
    return(val.type)
}


# Returns the type information of the given value, broken into its parts and
# returned in a vecctor or a list.

.ddg.get.val.type <- function(value) {
    # vector: a 1-dimensional array (uniform typing)
    if (is.vector(value))
        return(list("vector", length(value), class(value)))
    # matrix: a 2-dimensional array (uniform typing)
    if (is.matrix(value))
        return(list("matrix", dim(value), class(value[1])))
    # array: n-dimensional (uniform typing)
    if (is.array(value))
        return(list("array", dim(value), class(value[1])))
    # data frame: is a type of list
    if (is.data.frame(value)) {
        types <- unname(sapply(value, class))
        return(unname(list("data_frame", dim(value), types)))
    }
    # a list
    if (is.list(value))
        return("list")
    # an object
    if (is.object(value))
        return("object")
    # envrionment, function, language
    if (is.environment(value))
        return("environment")
    if (is.function(value))
        return("function")
    if (is.language(value))
        return("language")
    # none of the above - null is a character, not NULL or NA
    return(NULL)
}

# .ddg.is.nonlocal.assign returns TRUE if the object passed is an expression
# object containing a non-local assignment.
# expr - input expression.
.ddg.is.nonlocal.assign <- function(expr) {
    # <<- or ->> means that the assignment is non-local
    if (is.call(expr) && identical(expr[[1]], as.name("<<-"))) {
      return(TRUE)
    }
    return(FALSE)
}


# .ddg.create.empty.vars.set creates an empty data frame initialized to contain
# information about variable assignments.  The difference between first.writer
# and possible.first.writer is that first.writer is for simple assignments (like
# a <- 1), while possible.first.writer is for situations where the assignment
# might not have occurred, like 'if (foo) a <- 1'.

# The data frame is structured as follows - the variable name.  - the position of
# the statement that wrote the variable first.  - the position of the statement
# that wrote the variable last.  - the position of the first statement that may
# have assigned to a variable .  - the position of the last statement that may
# have assigned to a variable.

# var.table.size - desired size of the data frame. Negative values and 0 are
# coerced to 1.

.ddg.create.empty.vars.set <- function(var.table.size = 1) {
    if (var.table.size <= 0)
        var.table.size <- 1
    vars.set <- data.frame(variable = character(var.table.size), first.writer = numeric(var.table.size),
        last.writer = numeric(var.table.size), possible.first.writer = numeric(var.table.size),
        possible.last.writer = numeric(var.table.size), stringsAsFactors = FALSE)
    # Initialize first writer.
    vars.set$first.writer <- var.table.size + 1
    vars.set$possible.first.writer <- var.table.size + 1
    return(vars.set)
}

# .ddg.increase.vars.set simply doubles the size of a variable assignment data
# frame and returns the new one.

# vars.set - data frame containing variable assignments.  size (optional) -
# number of rows in data frame.

.ddg.double.vars.set <- function(vars.set, size = nrow(vars.set)) {
    # Create the right size data frame from input frame.
    new.vars.set <- rbind(vars.set, .ddg.create.empty.vars.set(size))
    # Update first/last writer.
    new.vars.set$first.writer <- ifelse(new.vars.set$first.writer == size + 1, size *
        2 + 1, new.vars.set$first.writer)
    new.vars.set$possible.first.writer <- ifelse(new.vars.set$possible.first.writer ==
        size + 1, size * 2 + 1, new.vars.set$possible.first.writer)
    return(new.vars.set)
}

# .ddg.add.to.vars.set parses a command and adds the new variable information to
# the variable assignment data frame. Note that var.num is a global variable! It
# should be intialized when vars.set is first created.

# vars.set - variable assignment data frame.  cmd.expr - command expression.  i -
# position of variable in data frame.

.ddg.add.to.vars.set <- function(vars.set, cmd, i) {
    # Find out the variable being assigned to by a simple assignment statement.
    main.var.assigned <- cmd@vars.set
    # Find all the variables that may be assigned in the statement.
    vars.assigned <- cmd@vars.possibly.set
    for (var in vars.assigned) {
        nRow <- which(vars.set$variable == var)
        # If the variable is already in the table, update its entry.
        if (length(nRow) > 0) {
            if (!is.null(main.var.assigned) && var == main.var.assigned) {
                vars.set$last.writer[nRow] <- i
            } else {
                vars.set$possible.last.writer[nRow] <- i
            }
        } else {
            # The variable was not in the table. Add a new line for this variable.
            # Find the first empty row
            empty.rows <- which(vars.set$variable == "")
            if (length(empty.rows) == 0) {
                vars.set <- .ddg.double.vars.set(vars.set, nrow(vars.set))
                empty.rows <- which(vars.set$variable == "")
            }
            var.num <- empty.rows[1]
            # Set the variable.
            vars.set$variable[var.num] <- var
            if (!is.null(main.var.assigned) && var == main.var.assigned) {
                vars.set$first.writer[var.num] <- i
                vars.set$last.writer[var.num] <- i
            } else {
                vars.set$possible.first.writer[var.num] <- i
                vars.set$possible.last.writer[var.num] <- i
            }
        }
    }
    return(vars.set)
}


## .ddg.find.var.assigments finds the possible variable assignments for a fixed
## set of parsed commands. See .ddg.create.empty.vars.set for more information on
## the structure of the returned data frame.  parsed.commands - a list of parsed
## commands.
.ddg.find.var.assignments <- function(cmds) {
    if (length(cmds) == 0)
        return(data.frame())
    # Make it big so we don't run out of space.
    var.table.size <- length(cmds)
    vars.set <- .ddg.create.empty.vars.set(var.table.size)
    # Build the table recording where variables are assigned to or may be assigned
    # to.
    for (i in 1:length(cmds)) {
        cmd.expr <- cmds[[i]]
        vars.set <- .ddg.add.to.vars.set(vars.set, cmd.expr, i)
    }
    return(vars.set)
}





# .ddg.create.data.use.edges.for.console.cmd creates a data flow edge from the
# node for each variable used in cmd.expr to the procedural node labeled cmd, as
# long as the value would either be one that exists prior to starting the console
# block, or corresponds to the last setting of this variable in the console
# block.

# vars.set - variable assignment data frame.  cmd - name of procedure node.
# cmd.expr - command expression.  cmd.pos - position of command.

.ddg.create.data.use.edges.for.console.cmd <- function(vars.set, cmd, cmd.pos, for.caller) {
    # Find all the variables used in this command.
    vars.used <- cmd@vars.used
    for (var in vars.used) {
        # Make sure there is a node we could connect to.
        scope <- .ddg.get.scope(var, for.caller)
        if (.ddg.data.node.exists(var, scope)) {
            nRow <- which(vars.set$variable == var)
            # check if the node is written in the console block.
            if (length(nRow) > 0) {
                first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
                last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
                # Draw the edge if we will connect to a node that exists before the console block
                # or to the last writer of this variable within the console block.
                if (cmd.pos <= first.writer || cmd.pos >= last.writer) {
                  .ddg.data2proc(var, scope, cmd@abbrev)
                }
                # TODO - add some sort of warning to the user that the node is not being created
            } else {
                # The variable is not set at all in this console block.  Connect to a
                # pre-existing data node.
                .ddg.data2proc(var, scope, cmd@abbrev)
            }
        } else {
            # TODO - add some sort of warning that the data node was NOT found.
        }
    }
}

# .ddg.create.data.set.edges.for.cmd creates edges that correspond to a console
# command assigning to a variable.

# vars.set - variable assignment data frame.  cmd.abbrev - name of procedure
# node.  cmd.expr - command expression.  cmd.pos - position of command.  env -
# environment to use for evaluating variable.  for.finish.node (optional) - if
# TRUE, data edge is for finish node.  scope (optional) - scope of variable.
# stack (optional) - stack to use for evaluating variable.

.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd, cmd.pos, env, for.finish.node = FALSE,
    scope = NULL, stack = NULL) {
    vars.assigned <- cmd@vars.set
    for (var in vars.assigned) {
        whichRows <- which(vars.set$variable == var)
        # Only create a node edge for the last place that a variable is set within a
        # console block.
        if ((length(whichRows) > 0 && vars.set$last.writer[whichRows] == cmd.pos &&
            vars.set$possible.last.writer[whichRows] <= vars.set$last.writer[whichRows]) ||
            for.finish.node) {
            if (is.null(env)) {
                env <- .ddg.get.env(var, calls = stack)
            }
            scope <- .ddg.get.scope(var, calls = stack, env = env)
            # Special operators are defined by enclosing the name in `.  However, the R
            # parser drops those characters when we deparse, so when we parse here they are
            # missing and we get an error about unexpected SPECIAL characters.  The first
            # tryCatch, puts the ` back in and parses again.  The second tryCatch handles
            # errors associated with evaluated the variable.
            parsed <- tryCatch(parse(text = var), error = function(e) parse(text = paste("`",
                var, "`", sep = "")))
            val <- tryCatch(eval(parsed, env), error = function(e) {
                eval(parse(text = var), parent.env(env))
            })
            tryCatch(.ddg.save.data(var, val, fname = ".ddg.create.data.set.edges.for.cmd",
                error = TRUE, scope = scope, stack = stack, env = env), error = function(e) {
                .ddg.data.node("Data", var, "complex", scope)
            })
            .ddg.proc2data(cmd@abbrev, var, scope)
        }
    }
}


# .ddg.create.data.node.for.possible.writes creates a data node for each variable
# that might have been set in something other than a simple assignment.  An edge
# is created from the last node in the console block.

# vars.set - variable assignment data frame.  last.command - last command in
# console block.

.ddg.create.data.node.for.possible.writes <- function(vars.set, last.command, env = NULL) {
    environment <- if (is.environment(env))
        env else .GlobalEnv
    for (i in 1:nrow(vars.set)) {
        if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
            value <- tryCatch(eval(parse(text = vars.set$variable[i]), environment),
                error = function(e) {
                  NULL
                })
            # Only create the node and edge if we were successful in looking up the value.
            if (!is.null(value)) {
                envName <- environmentName(environment)
                if (envName == "")
                  envName <- .ddg.get.scope(vars.set$variable[i])
                .ddg.data.node("Data", vars.set$variable[i], value, envName)
                .ddg.proc2data(last.command@abbrev, vars.set$variable[i], envName)
            }
        }
    }
}


# Given a parse tree, this function returns a list containing the expressions
# that correspond to the filename argument of the calls to functions that read or
# write the files.  If there are none, it returns NULL.  main.object - the parsed
# expression to search through func.df - the data frame describing the functions
# with file arguments
.ddg.find.files <- function(main.object, func.df, env = NULL) {
    environment <- if (is.environment(env))
        env else .GlobalEnv
    # Recursive helper function.
    find.files.rec <- function(obj) {
        # Base cases.
        if (!is.recursive(obj)) {
            return(NULL)
        }
        if (length(obj) == 0) {
            return(NULL)
        }
        ## It might be useful to record somehow that this function reads a file, but we
        ## wouldn't actually do the reading until the function is called, not here where
        ## it is being declared.
        if (.is.functiondecl(obj))
            return(NULL)
        if (is.call(obj)) {
            # Call has no arguments, so it can't be reading a function.  Recurse on the first
            # part, in case it is more than just a symbol.
            if (length(obj) == 1)
                return(find.files.rec(obj[[1]])) else if (is.symbol(obj[[1]])) {
                # Call with arguments Is this is file reading function?
                read.func.pos <- match(as.character(obj[[1]]), func.df$function.names)
                if (!is.na(read.func.pos)) {
                  # Find the file argument.
                  arg.name <- func.df$param.names[read.func.pos]
                  # Find a matching parameter passed by name
                  file.name.arg.matches <- unlist(lapply(names(obj), function(arg) {
                    return(pmatch(arg, arg.name))
                  }))
                  match.pos <- match(1, file.name.arg.matches)
                  # If no argument qualified by the file parameter name, use the argument in the
                  # expected position
                  if (is.na(match.pos)) {
                    file.name <- tryCatch(eval(obj[[func.df$param.pos[read.func.pos] +
                      1]], environment), error = function(e) NULL)
                  } else {
                    file.name <- tryCatch(eval(obj[[match.pos]], environment), error = function(e) NULL)
                  }
                  # Recurse over the arguments to the function.  We can't just skip over the 2nd
                  # element since the filename parameter is not necessarily there if it was passed
                  # by name.
                  funcs <- find.files.rec(obj[2:length(obj)])
                  # Add this file name to the list of files being read.  Make sure the file name
                  # could be evaluated and that it results in a name, not a connection.
                  if (!is.null(file.name) && is.character(file.name)) {
                    unique(c(file.name, funcs))
                  }
                } else {
                  # Not a file reading function.  Recurse over the arguments.
                  find.files.rec(obj[2:length(obj)])
                }
            } else {
                # Function call, but the first list element is not simply a function name.
                # Recurse over all the list elements.
                unique(append(find.files.rec(obj[[1]]), find.files.rec(obj[2:length(obj)])))
            }
        } else if (length(obj) == 1) {
            # A recursive structure that is not a call.  Not sure if there are any, but just
            # in case...
            unique(find.files.rec(obj[[1]]))
        } else {
            unique(append(find.files.rec(obj[[1]]), find.files.rec(obj[2:length(obj)])))
        }
    }
    return(find.files.rec(main.object@parsed))
}

# Creates file nodes and data in edges for any files that are read in this cmd
# cmd - text command cmd.expr - parsed command
.ddg.create.file.read.nodes.and.edges <- function(cmd, env) {
    # Find all the files potentially read in this command.  This may include files
    # that are not actually read if the read are within an if-statement, for example.
    files.read <- .ddg.find.files(cmd, .global.get(".ddg.file.read.functions.df"), env)
    for (file in files.read) {
        # Only create the node and edge if there actually is a file then if the file
        # exists, it is possible that it was not read here
        if (file.exists(file)) {
            # Create the file node and edge
            ddg.file(file)
            ddg.data.in(basename(file), pname = cmd@abbrev)
        } else if (grepl("^http", file) || grepl("^ftp", file)) {
            scope <- environmentName(.GlobalEnv)
            .ddg.data.node("URL", file, file, scope)
            .ddg.data2proc(file, scope, cmd@abbrev)
        }
    }
}

# Creates file nodes and data in edges for any files that are written in this cmd
# cmd - text command cmd.expr - parsed command
.ddg.create.file.write.nodes.and.edges <- function(cmd, env) {
    # Find all the files potentially written in this command.  This may include files
    # that are not actually written if the write calls are within an if-statement,
    # for example.
    files.written <- .ddg.find.files(cmd, .global.get(".ddg.file.write.functions.df"), env)
    for (file in files.written) {
        # check that the file exists.  If it does, we will assume that it was created by
        # the write call that we just found.
        if (file.exists(file)) {
            # Create the file node and edge
            ddg.file.out(file, pname = cmd@abbrev)
        }
    }
}

# .ddg.link.function.returns determines if the command calls a function for which
# ddg.return has created a node for the return value.  If so, a data flow edge is
# created from the return value data node to the finish node for the command.
# Note that if the assignment is an expression, like 'd <- f(a) + f(b)', there
# may be multiple return value nodes to link to.

# command - input command.

.ddg.link.function.returns <- function(command) {
    # Find the functions that have completed but whose returns have not been used
    # yet.
    returns <- .global.get(".ddg.ret.values")
    if (!is.na(command@pos@startLine)) {
        unused.returns <- returns[!returns$ret.used & returns$ret.node.id >
            0 & !is.na(returns$line) & returns$line == command@pos@startLine, ]
    } else {
        unused.returns <- returns[!returns$ret.used & returns$ret.node.id >
            0, ]
    }
    if (nrow(unused.returns) == 0)
        return()
    # See which of these are called from the command we are processing now.
    unused.calls <- unused.returns$ddg.call
    command.text <- gsub(" ", "", command@text)
    uses <- sapply(unused.calls, function(call) {
        grepl(call, command.text, fixed = TRUE)
    })
    # The following line is here to get around R CMD check, which otherwise reports:
    # no visible binding for global variable.  Note that ret.node.id is not a
    # variable in the subset call, but the name of a column in the data frame being
    # subsetted.
    ret.node.id <- NULL
    # Extracts for the return value nodes.
    new.uses <- subset(unused.returns, uses, ret.node.id)
    # Create an edge from each of these to the last procedure node.
    lapply(new.uses$ret.node.id, function(data.num) {
        proc.num <- .global.get("ddg.pnum")
        # Record in edges table
        etype <- "df.in"
        node1 <- paste("d", data.num, sep = "")
        node2 <- paste("p", proc.num, sep = "")
        .ddg.record.edge(etype, node1, node2)

        if (.global.get("ddg.debug.lib")) {
            print(paste(".ddg.link.function.returns:", command@abbrev))
            print(paste("DF ", node1, " ", node2, sep = ""))
        }
        # Set the return value as being used.
        returns$ret.used[returns$ret.node.id == data.num] <- TRUE
        .global.set(".ddg.ret.values", returns)
    })
}

# Create the DDGStatement list for a list of parsed expressions.

# exprs - a list of parsed expressions script.name - the name of the script the
# expressions come from parseData - information provided by the parser that we
# use to find line numbers enclosing.pos - if exprs are statements within a
# function definition, enclosing.pos is the source position information of the
# entire function declaration

# Returns a list of DDGStatement objects

.ddg.create.DDGStatements <- function(exprs, script.name, script.num, parseData = NULL,
    enclosing.pos = NULL) {
    # The parse data gives us line number information
    if (is.null(parseData)) {
        parseData <- getParseData(exprs, includeText = TRUE)
        if (is.null(parseData)) {
            # In this case there is no line number information available
            cmds <- vector("list", (length(exprs)))
            for (i in 1:length(exprs)) {
                expr <- as.expression(exprs[i])
                cmds[[i]] <- .construct.DDGStatement(expr, NA, script.name, script.num, parseData)
            }
            return(cmds)
        }
        non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
        if (nrow(non.comment.parse.data) == 0) {
            return(list())
        }
        # Start at the first non-comment expression in parseData
        next.parseData <- 1
    } else {
        non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
        # Start at the first entry in parse data that begins after the enclosing function
        # begins, ends before the enclosing function ends, and matches the text of the
        # first expression.
        next.parseData <- which(non.comment.parse.data$line1 >= enclosing.pos@startLine &
            non.comment.parse.data$line2 <= enclosing.pos@endLine & non.comment.parse.data$text ==
            paste(deparse(exprs[[1]]), collapse = "\n"))[1]
    }
    # Create the DDGStatements
    cmds <- vector("list", (length(exprs)))
    next.cmd <- 1
    for (i in 1:length(exprs)) {
        expr <- as.expression(exprs[i][[1]])
        next.expr.pos <- new(Class = "DDGStatementPos", non.comment.parse.data[next.parseData,
            ])
        cmds[[next.cmd]] <- .construct.DDGStatement(expr, next.expr.pos, script.name,
            script.num, parseData)
        next.cmd <- next.cmd + 1
        # If there are more expressions, determine where to look next in the parseData
        if (i < length(exprs)) {
            last.ending.line <- non.comment.parse.data[next.parseData, ]$line2
            last.parent <- non.comment.parse.data[next.parseData, "parent"]
            last.id <- non.comment.parse.data[next.parseData, "id"]

            # Find the first entry in parseData that has the same parent as the previous
            # expression and starts after the previous expression.
            next.parseData <- which(non.comment.parse.data$parent == last.parent &
                non.comment.parse.data$line1 >= last.ending.line & non.comment.parse.data$id >
                last.id)[1]
        }
    }
    return(cmds)
}

# .ddg.save.annotated.script saves a copy of the annotated script to the debug
# directory.

.ddg.save.annotated.script <- function(cmds, script.name) {
    for (i in 1:length(cmds)) {
        expr <- cmds[[i]]@annotated
        for (j in 1:length(expr)) {
            line <- deparse(expr[[j]])
            if (i == 1 && j == 1)
                script <- line else script <- append(script, line)
        }
    }
    fileout <- file(paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/annotated-", script.name, sep = ""))
    write(script, fileout)
    close(fileout)
}

# .ddg.file.node creates a node of type File. File nodes are used for files
# written to the DDG directory by capturing output from the script or by copying
# a file that is written by the script.  Returns the path where the file
# referenced by the node is stored.

# dtype - type of data node.  fname - path and name of original file.  dname -
# name of data node.  dscope (optional) - scope of data node.

.ddg.file.node <- function(dtype, fname, dname, dscope = NULL) {
    # Get original file location.
    file.name <- basename(fname)
    file.loc <- normalizePath(fname, winslash = "/", mustWork = FALSE)
    loc.value <- if (dtype == "File")
        paste(" Location=\"", file.loc, "\"", sep = "") else ""
    # Add number to file name.
    dfile <- paste(.global.get("ddg.dnum") + 1, "-", file.name, sep = "")
    # Calculate the path to the file relative to the ddg directory.  This is the
    # value stored in the node.
    dpfile <- paste("data", dfile, sep = "/")
    dtime <- .format.time(Sys.time())
    # Set the node label.
    if (is.null(dname))
        dname <- file.name
    # Get scope if necessary.
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    # Not from environment.
    from.env <- FALSE
    # Record in data node table
    .ddg.record.data(dtype, dname, dpfile, dpfile, dscope, from.env = from.env, dtime,
        file.loc)
    # Get path plus file name to where the file will be copied
    dpath <- paste(paste(.global.get("ddg.path"), "/data", sep = ""), "/", dfile, sep = "")
    return(dpath)
}

# .ddg.file.copy creates a data node of type File. File nodes are used for files
# written by the main script. A copy of the file is written to the DDG directory.

# dtype - type of data node.  fname - path and name of original file.  dname -
# name of data node.  dscope - scope of data node.

.ddg.file.copy <- function(dtype, fname, dname, dscope) {
    # Calculate location of original file.
    file.loc <- normalizePath(fname, winslash = "/", mustWork = FALSE)
    # Copy file.
    if (file.exists(file.loc)) {
        # Create file node in DDG.
        dpfile.out <- .ddg.file.node(dtype, fname, dname, dscope)
        if (.global.get("ddg.save.to.disk")) {
            file.copy(file.loc, dpfile.out, overwrite = TRUE)
        }
    } else {
        error.msg <- paste("File to copy does not exist:", fname)
        .ddg.insert.error.message(error.msg)
        return(NULL)
    }
    if (.global.get("ddg.debug.lib"))
        print(paste("file.copy: ", dtype, " ", file.loc))
    return(dpfile.out)
}

# .ddg.insert.error.message issues a warning and inserts an exception node after
# the last procedure step. The name of the node is 'error.msg' and the value is
# the error message passed to this function.

# msg - error message.  msg.type - error or warning scope - scope for evaluating
# any data doWarn - if true, this function displays a warning

.ddg.insert.error.message <- function(msg, msg.type = "error.msg", scope = "ddg.library",
    doWarn = TRUE) {
    if (doWarn) {
        warning(msg)
    }
    .ddg.data.node("Exception", msg.type, msg, scope)
    .ddg.lastproc2data(msg.type, dscope = scope)
}

# .ddg.delete.temp deletes any temporary files created during the processing of a
# script. These include the temporary history file.

.ddg.delete.temp <- function() {
    # Delete the temporary history file if we made it.
    if (.global.is.set("ddg.history.file"))
        unlink(.global.get("ddg.history.file"))
    # Clear the environment.
    .ddg.env <- new.env(parent = emptyenv())
}

# .ddg.create.output.nodes creates output nodes for ddg.function and
# ddg.procedure. Outs values must be passed as strings, not names, unless the
# value is a file name.

# fname - the name of the function calling .ddg.create.output.nodes.  pname - the
# name of the procedure node.  outs.graphic - the name of a snapshot node to be
# used as a file name.  A graphical snapshot is simply a captured image of the
# graphic device active at the time of the call to ddg.function or ddg.procedure.
# outs.data - a list of names of data nodes.  outs.exception - a list of names of
# exception nodes.  outs.url - a list of names of url nodes.  outs.file - a list
# of names of file nodes. Supported file extensions include: .csv, .jpg, .jpeg,
# .pdf, and .txt.  graphic.fext - the file extension to be used when saving the
# captured graphic. Supported extensions are .jpg, .jpeg, .pdf.

.ddg.create.output.nodes <- function(fname, pname, outs.graphic, outs.data, outs.exception,
    outs.url, outs.file, graphic.fext, env) {
    # Capture graphics device.
    if (is.character(outs.graphic)) {
        name <- outs.graphic
        gfext <- as.character(graphic.fext)
        .ddg.write.graphic(name, "Graphical Plot. Not saved in script.", fext = gfext)  # value is ignored
        .ddg.proc2data(pname, name)
    }

    # Create output nodes and edges if outs list provided.
    # Exception node.
    if (!is.null(outs.exception)) {
        stack <- sys.calls()
        lapply(outs.exception, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)

            # Exception node.
            scope <- .ddg.get.scope(param, calls = stack)
            .ddg.data.node("Exception", name, value, scope)
            .ddg.proc2data(pname, name, scope)
        })
    }

    # URL node.
    if (!is.null(outs.url)) {
        stack <- sys.calls()
        lapply(outs.url, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)
            # URL node.
            scope <- .ddg.get.scope(param, calls = stack)
            .ddg.data.node("URL", name, value, scope)
            .ddg.proc2data(pname, name, scope)
        })
    }

    # Generalized data node (includes simple data values as well as snapshots)
    if (!is.null(outs.data)) {
        stack <- sys.calls()
        lapply(outs.data, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)

            tryCatch({
                if (!is.character(name))
                  name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls = stack)
                .ddg.save.data(name, value, fname, error = TRUE, scope = scope)
                .ddg.proc2data(pname, name, scope)
            }, error = function(e) {
                .ddg.insert.error.message(e)
            })
        })
    }
    # File node.
    if (!is.null(outs.file)) {
        stack <- sys.calls()
        lapply(outs.file, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)
            scope <- .ddg.get.scope(param, calls = stack)

            if (value == "") {
                # Filename passed as value.
                .ddg.file.copy("File", name, name, scope)
                .ddg.proc2data(pname, name, scope)
            } else {
                # Filename passed as name.
                .ddg.file.copy("File", value, name, scope)
                .ddg.proc2data(pname, name, scope)
            }
        })
    }
}

# .ddg.create.function.nodes creates the procedure node, input binding nodes, and
# output nodes for the function.

# pname - name of procedure node.  full.call - full function call.  outs.data,
# etc (optional) - output nodes.  auto.created - TRUE if the function node is
# created automatically when a return is found env (optional) - the environment
# local to the function

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic = NULL,
    outs.data = NULL, outs.exception = NULL, outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg",
    auto.created = FALSE, env = NULL) {
    # Create the start node
    if (typeof(call[[1]]) == "closure") {
        .add.abstract.node("Start", node.name = pname, env = env)
    } else {
        .add.abstract.node("Start", node.name = paste(deparse(call), collapse = ""),
            env = env)
    }
    # Tokens will contain the function name and the argument expressions.
    # Get parameters and create edges.
    if (length(full.call) > 1) {
        # args contains the names of the variable that was passed into the function.
        args <- full.call[2:length(full.call)]
        # param,names contains the names of the parameters (this is what the variable is
        # known as inside the function).
        param.names <- names(full.call)
        param.names <- param.names[2:length(param.names)]
        stack <- sys.calls()
        # scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
        bindings <- list()
        for (i in 1:length(args)) bindings[[i]] <- list(args[[i]], param.names[[i]])
        missing.params <- character()

        lapply(bindings, function(binding) {
            # Here, arg is the arguments passed IN.
            arg <- binding[[1]]
            # formal is the paramenter name of the function (what is the variable known as
            # inside?).
            formal <- binding[[2]][[1]]
            if (is.null(formal) || formal == "")
                formal <- "..."
            # Find all the variables used in this parameter.  If the argument is a string
            # constant, don't bother looking for variables.  Also add quotes around it in the
            # node name.
            if (is.character(arg)) {
                vars.used <- character()
                binding.node.name <- paste(formal, " <- \"", arg, "\"", sep = "")
            } else {
                vars.used <- .find.var.uses(arg)
                binding.node.name <- paste(formal, " <- ", paste(deparse(arg), collapse = " "))
            }

            .proc.node("Binding", binding.node.name)
            .ddg.proc2proc()
            for (var in vars.used) {
                param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls = stack)
                if (.ddg.data.node.exists(var, param.scope)) {
                  .ddg.data2proc(as.character(var), param.scope, binding.node.name)
                  if (.global.get("ddg.debug.lib"))
                    print(paste("param:", var))
                }
            }
            if (formal != "...") {
                formal.scope <- .ddg.get.scope(formal, calls = stack)
                formal.env <- .ddg.get.env(formal, calls = stack)
                # If we can evaluate the argument without an error, we record the value. If an
                # error occurs, we do not record the value as it's possible that the function
                # never actually uses it.
                tryCatch({
                  .ddg.save.data(formal, eval(parse(text = formal), formal.env),
                    fname = ".ddg.save.data", scope = formal.scope, stack = stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)
                }, error = function(e) {
                })

            }
        })
    }
    .proc.node("Operation", pname, pname, auto.created = auto.created)
    # Link to the definition of the function if the function is defined in this
    # script.
    if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
        .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
    }
    if (length(full.call) > 1) {
        lapply(bindings, function(binding) {
            formal <- binding[[2]][[1]]
            # Formal will be NULL if declared as ...  Don't create the data node in that
            # case.
            if (!is.null(formal) && formal != "") {
                formal.scope <- .ddg.get.scope(formal, calls = stack)
                if (.ddg.data.node.exists(formal, formal.scope)) {
                  .ddg.data2proc(formal, formal.scope, pname)
                }
            }
        })
    }
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # create output nodes
    .ddg.create.output.nodes(fname = "ddg.function", pname, outs.graphic, outs.data,
        outs.exception, outs.url, outs.file, graphic.fext, parent.frame(2))

}

# .ddg.get.frame.number gets the frame number of the closest non-library calling
# function.

# calls - system calls.  for.caller (optional) - if TRUE, go up one level before
# searching.

.ddg.get.frame.number <- function(calls, for.caller = FALSE) {
    if (is.null(calls))
        calls <- sys.calls()
    script.func.found <- FALSE
    nframe <- length(calls)
    for (i in nframe:1) {
        call <- sys.call(i)[[1]]
        # Guess that if we have a closure it is a user-defined function and not a ddg
        # function Is this a good assumption ????
        if (typeof(call) == "closure") {
            if (for.caller && !script.func.found) {
                script.func.found <- TRUE
            } else {
                return(i)
            }
        } else {
            call.func <- as.character(call)
            # Ignore calls to ddg functions or to the functions that get called from the
            # outermost tryCatch to ddg code.
            if (substr(call.func, 1, 4) != ".ddg" && substr(call.func, 1, 3) != "ddg" &&
                substr(call.func, 1, 10) != "doTryCatch" && substr(call.func, 1,
                11) != "tryCatchOne" && substr(call.func, 1, 12) != "tryCatchList" &&
                substr(call.func, 1, 8) != "tryCatch") {
                if (for.caller && !script.func.found) {
                  script.func.found <- TRUE
                } else {
                  return(i)
                }
            }
        }
    }
    return(0)
}


# .ddg.where looks up the environment for the variable specified by name.
# Adapted from Hadley Wickham, Advanced R programming.

# name - name of variable.  env (optional) - environment in which to look for
# variable.  warning (optional) - set to TRUE if a warning should be thrown when
# a variable is not found.

.ddg.where <- function(name, env = parent.frame(), warning = TRUE) {
    stopifnot(is.character(name), length(name) == 1)

    if (identical(env, emptyenv())) {
        if (warning)
            warning("Can't find ", name)

        return("undefined")
    }
    if (exists(name, env, inherits = FALSE)) {
        env
    } else {
        .ddg.where(name, parent.env(env), warning)
    }
}


# .ddg.get.env gets the environment in which name is declared.

# name - variable name.  for.caller (optional) - if TRUE, go up one level before
# searching.  calls (optional) - system calls.

.ddg.get.env <- function(name, for.caller = FALSE, calls = NULL) {
    if (is.null(calls))
        calls <- sys.calls()
    fnum <- .ddg.get.frame.number(calls, for.caller)
    stopifnot(!is.null(fnum))
    # This statement was broken into two statements so that we can add print
    # statements to .ddg.where or step through it with a debugger without breaking
    # it.  If we don't do that the print output gets captured by capture.output and
    # does not display to the user and also causes the subsequent grepl call in this
    # function to fail.
    tryCatch(if (!exists(name, sys.frame(fnum), inherits = TRUE))
        return(NULL), error = function(e) {
    })
    env <- .ddg.where(name, sys.frame(fnum))
    return(env)
}

# .ddg.get.scope gets the id of the closest non-library environment.

# name - name of variable.  for.caller (optional) - if TRUE, go up one level
# before searching.  calls (optional) - system calls.  env (optional) - the
# environment to get the scope for

.ddg.get.scope <- function(name, for.caller = FALSE, calls = NULL, env = NULL) {
    # Get the environment for the variable call.
    if (is.null(env)) {
        env <- .ddg.get.env(name, for.caller, calls)
    }
    # If no environment found, name does not exist, so scope is undefined.
    if (is.null(env))
        return("undefined")
    scope <- sub("^<environment: (.*)>$", "\\1", capture.output(env)[1])
    if (grepl("undefined", scope))
        scope <- "undefined"
    return(scope)
}

# .ddg.is.local returns TRUE if the specified name is local in the specified
# scope.

# name of variable.  scope of variable.

.ddg.is.local <- function(name, scope) {
    return(exists(name, scope, inherits = FALSE))
}

# Creates a start node for the current command if one has not been created
# already.
.ddg.create.start.for.cur.cmd <- function(call, caller.env) {
    if (.global.is.set(".ddg.cur.cmd")) {
        .ddg.cur.cmd <- .global.get(".ddg.cur.cmd")
        .ddg.cur.cmd.stack <- .global.get(".ddg.cur.cmd.stack")
        stack.length <- length(.ddg.cur.cmd.stack)
        if (stack.length >= 1) {
            last.created <- .ddg.cur.cmd.stack[stack.length]
            # Only create a start node for the current command if we have not already created
            # one and the command is more than just the call to this function
            if (last.created[[1]] == "FALSE") {
                if (.ddg.cur.cmd@text != paste(deparse(call), collapse = "")) {
                  cmd.abbrev <- .add.abstract.node("Start", .ddg.cur.cmd, caller.env)
                  .ddg.cur.expr.stack <- .global.get(".ddg.cur.expr.stack")
                  st.type <- .get.statement.type(.ddg.cur.cmd@parsed[[1]])
                  loop.statement <- (st.type == "for" || st.type == "while" || st.type ==
                    "repeat")
                  control.statement <- loop.statement || st.type == "if"
                  .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(),
                    .ddg.cur.cmd, 0, for.caller = !control.statement)
                  # Add Details Omitted node before annotated loops if needed.
                  if (loop.statement && ddg.first.loop() > 1) {
                    ddg.details.omitted()
                  }
                  # Mark the start node as created on the stack.  Mark it even if we did not create
                  # the abstract node above, because we will create it below.
                  .global.set(".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length -
                    1], TRUE))
                } else {
                  .global.set(".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length -
                    1], "MATCHES_CALL"))
                }
            }
        }
    }
}

# .ddg.get.last.cmd returns the last command on the stack.

.ddg.get.last.cmd <- function() {
    .ddg.cur.cmd.stack <- .global.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)
    cmd <- .ddg.cur.cmd.stack[stack.length - 1][[1]]
}

# .ddg.remove.last.cmd.start.created removes the last command and start.created
# from the stack.

.ddg.remove.last.cmd.start.created <- function() {
    .ddg.cur.cmd.stack <- .global.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)

    if (stack.length == 2) {
        .global.set(".ddg.cur.cmd.stack", vector())
    } else {
        .global.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length - 2)])
    }
}

# .ddg.break.statement creates a procedure node for a break statement in a for,
# repeat, or while statement. It also adds a finish node for the if statement (if
# any) where the break occurs, adds a finish node for the for, repeat, or while
# loop where the break occurs, and adds a finish node for the for, repeat, or
# while statement.

.ddg.break.statement <- function() {
    # Create procedure node for break statement.
    .proc.node("Operation", "break", "break")
    .ddg.proc2proc()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
    # Create finish nodes if break occurs in if statement.
    if (loop.type == "if") {
        # Create finish node for if loop.
        ddg.finish("if")
        # Create finish node for if statement.
        .add.abstract.node("Finish", cmd, parent.frame())
        # Remove last command & start.created from stack.
        .ddg.remove.last.cmd.start.created()
        # Get last command from stack.
        cmd <- .ddg.get.last.cmd()
        # Get loop type.
        loop.type <- as.character(cmd@parsed[[1]][[1]])
    }
    # Create finish node for for, repeat, or while loop.
    loop.name <- paste(loop.type, "loop")
    ddg.finish(loop.name)
    # Create finish node for for, while, or repeat statement.
    .add.abstract.node("Finish", cmd, parent.frame())
    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
}

# .ddg.next.statement creates a procedure node for a next statement in a for,
# repeat, or while statement. It also adds a finish node for the if statement (if
# any) where the next occurs and adds a finish node for the for, while, or repeat
# loop where the next occurs.

.ddg.next.statement <- function() {
    # Create procedure node for next statement.
    .proc.node("Operation", "next", "next")
    .ddg.proc2proc()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
    # Create finish nodes if break occurs in if statement.
    if (loop.type == "if") {
        # Create finish node for if loop.
        ddg.finish("if")
        # Create finish node for if statement.
        .add.abstract.node("Finish", cmd, parent.frame())

        # Remove last command & start.created from stack.
        .ddg.remove.last.cmd.start.created()
        # Get last command from stack.
        cmd <- .ddg.get.last.cmd()
        # Get loop type.
        loop.type <- as.character(cmd@parsed[[1]][[1]])
    }
    # Create finish node for for, repeat, or while loop.
    loop.name <- paste(loop.type, "loop")
    ddg.finish(loop.name)
}

# .ddg.markdown takes a Rmd file and extracts the R code and text through the
# purl function in the knitr library. It then annotates the R script to insert
# start and finish nodes based on the chunks the user already created. If eval =
# false, then the chunk will not be added to the DDG. If the user has a name for
# the chunk, then that name will be used, else a chunk name 'ddg.chunk_1' and
# higher numbers will be generated.  Important: If in a code chunk, there is an
# empty line followed by '# ----' or '#'', then an extra finish node will be
# inserted, causing an error.  r.script.path is the path of the original Rmd file
# output.path is the path of the generated R script

.ddg.markdown <- function(r.script.path = NULL, output.path = NULL) {
    # generates R script file from markdown file
    knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)
    # moves file to ddg directory
    file.rename(from = paste(getwd(), "/", basename(tools::file_path_sans_ext(r.script.path)),
        ".R", sep = ""), to = output.path)
    script <- readLines(output.path)
    skip <- FALSE
    name <- "ddg.chunk"
    annotated <- character(0)
    index <- 1
    # This for loop goes through the script line by line and searches for patterns to
    # insert the start and finish nodes
    for (i in 1:length(script)) {
        # eval = false means we skip this code chunk, therefore skip = TRUE
        if (regexpr("eval+(\\s*)+=+(\\s*)+FALSE", script[i]) != -1) {
            skip <- TRUE
            annotated <- append(annotated, script[i])
        } else if (regexpr("## ----", script[i]) != -1) {
            # if no options in the line, then generate default name.
            if (regexpr("## -----", script[i]) == -1) {
                if (regexpr("=", script[i]) == -1) {
                  end <- regexpr("-----", script[i])
                  name <- substring(script[i], 8, last = end - 1)
                } else if (regexpr(",", script[i]) != -1) {
                  comma <- regexpr(",", script[i])
                  name <- substring(script[i], 8, last = comma - 1)
                } else {
                  name <- paste("ddg.chunk_", index, sep = "")
                  index <- index + 1
                }
            } else {
                name <- paste("ddg.chunk_", index, sep = "")
                index <- index + 1
            }
            name <- stringr::str_trim(name, side = "both")
            annotated <- append(annotated, paste("ddg.start(\"", name, "\")", sep = ""))
        } else if (nchar(script[i]) == 0 && (regexpr("#'", script[i + 1]) != -1 ||
            i == length(script) || regexpr("## ----", script[i + 1]) != -1)) {
            if (skip) {
                annotated <- append(annotated, script[i])
                skip <- FALSE
            } else {
                annotated <- append(annotated, paste("ddg.finish(\"", name, "\")",
                  sep = ""))
            }
        } else {
            annotated <- append(annotated, script[i])
        }
    }
    writeLines(annotated, output.path)
    r.script.path
}

# .ddg.save.debug.files saves debug files to the debug directory.

.ddg.save.debug.files <- function() {
    # Save initial environment table to file.
    fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/initial-environment.csv", sep = "")
    ddg.initial.env <- .global.get("ddg.initial.env")
    write.csv(ddg.initial.env, fileout, row.names = FALSE)
    # Save procedure nodes table to file.
    fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/procedure-nodes.csv", sep = "")
    ddg.proc.nodes <- .global.get("ddg.proc.nodes")
    ddg.proc.nodes <- ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ]
    write.csv(ddg.proc.nodes, fileout, row.names = FALSE)
    # Save data nodes table to file.
    fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/data-nodes.csv", sep = "")
    ddg.data.nodes <- .global.get("ddg.data.nodes")
    ddg.data.nodes2 <- ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ]
    write.csv(ddg.data.nodes2, fileout, row.names = FALSE)
    # Save edges table to file.
    fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/edges.csv", sep = "")
    ddg.edges <- .global.get("ddg.edges")
    ddg.edges2 <- ddg.edges[ddg.edges$ddg.num > 0, ]
    write.csv(ddg.edges2, fileout, row.names = FALSE)
    # Save function return table to file.
    fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/function-returns.csv", sep = "")
    ddg.returns <- .global.get(".ddg.ret.values")
    ddg.returns2 <- ddg.returns[ddg.returns$ret.node.id > 0, ]
    write.csv(ddg.returns2, fileout, row.names = FALSE)
    # Save if script is sourced.
    if (.global.get(".ddg.is.sourced")) {
        # Save sourced script table to file.
        fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/sourced-scripts.csv", sep = "")
        ddg.sourced.scripts <- .global.get(".ddg.sourced.scripts")
        ddg.sourced.scripts2 <- ddg.sourced.scripts[ddg.sourced.scripts$snum >= 0,
            ]
        write.csv(ddg.sourced.scripts2, fileout, row.names = FALSE)
        # Save data object table to file.
        fileout <- paste(paste(.global.get("ddg.path"), "/debug", sep = ""), "/data-objects.csv", sep = "")
        ddg.data.objects <- .ddg.data.objects()
        write.csv(ddg.data.objects, fileout, row.names = FALSE)
    }
}

#--------------------USER FUNCTIONS-----------------------#
# .ddg.find.ddg.ret.value.caller.frame.number returns the frame number of the
# first caller to ddg.ret.value.  If ddg.ret.value is called recursively,
# this will give us the position of the earliest one called.

.ddg.find.ddg.ret.value.caller.frame.number <- function() {
    # Get the stack
    calls <- sys.calls()
    # Find the calls to ddg.ret.value
    ddg.funcs <- unlist(lapply(calls, function(call) return(grepl("^ddg|.ddg", deparse(call)[[1]]))))
    calls.to.ddg.ret.value <- unlist(lapply(calls, function(call) return(.ddg.is.call.to(call,
        as.name("ddg.ret.value")))))
    non.ddg.calls.to.ddg.ret.value <- !(ddg.funcs[1:length(ddg.funcs) - 1]) &
        calls.to.ddg.ret.value[2:length(calls.to.ddg.ret.value)]
    which.frame <- Position(function(call) return(call), non.ddg.calls.to.ddg.ret.value,
        right = TRUE)
    # Return the frame number of the caller to ddg.ret.value
    return(which.frame)
}

# Returns true if we should run the annotated version of a function and false if
# we should run the unannotated version.
ddg.should.run.annotated <- function(func.name) {
    # check if we are in a loop and loop annotations are off
    if (!.global.get("ddg.loop.annotate") && .global.get("ddg.inside.loop") > 0)
        return(FALSE)
    # Make sure this specific function has not been disabled
    if (!is.null(.global.get("ddg.annotate.off")) & func.name %in% .global.get("ddg.annotate.off"))
        return(FALSE)
    # Not annotating functions in general check if this specific function should be
    # annotated
    if (!is.null(.global.get("ddg.annotate.on")) & func.name %in% .global.get("ddg.annotate.on"))
        return(TRUE)
    # If we do not know anything specific about this function, follow the general
    # rule
    return(.global.get("ddg.annotate.inside"))
}

# .ddg.console.off turns off the console mode of DDG construction.
.ddg.console.off <- function() {
    if (!(.global.is.set(".ddg.initialized") && .global.get(".ddg.initialized")))
        return(invisible())
    # Capture history if console was on up to this point.
    if (interactive() && .global.get(".ddg.enable.console")) {
        .ddg.console.node()
    }
    # Set the console to off.
    .global.set(".ddg.enable.console", FALSE)
}

# .ddg.console.on turns on the console mode of DDG construction.

.ddg.console.on <- function() {
    if (!(.global.is.set(".ddg.initialized") && .global.get(".ddg.initialized")))
        return(invisible())
    # Write a new timestamp if we're turning on the console so we only capture
    # history from this point forward.
    if (!.global.get(".ddg.enable.console"))
        .write.timestamp.to.history()
    .global.set(".ddg.enable.console", TRUE)
}

# .ddg.delete.save removes all files from the DDG directories unless the the DDG
# directory is the working directory. If no DDG directory is specified, the
# current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

.ddg.delete.save <- function(ddg.path = NULL) {
    # Use current DDG directories if no directory is specified.
    if (is.null(ddg.path)) {
        ddg.path <- .global.get("ddg.path")
        ddg.path.data <- paste(.global.get("ddg.path"), "/data", sep = "")
        ddg.path.debug <- paste(.global.get("ddg.path"), "/debug", sep = "")
        ddg.path.scripts <- paste(.global.get("ddg.path"), "/scripts", sep = "")
    }
    # Remove files unless the DDG directory is the working directory.
    if (ddg.path != getwd()) {
        unlink(paste(ddg.path, "*.*", sep = "/"))
        unlink(paste(ddg.path.data, "*.*", sep = "/"))
        unlink(paste(ddg.path.data, ".ddghistory", sep = "/"))
        unlink(paste(ddg.path.debug, "*.*", sep = "/"))
        unlink(paste(ddg.path.scripts, "*.*", sep = "/"))
    }
    invisible()
}
