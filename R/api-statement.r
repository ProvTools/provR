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

# ddg.function creates a procedure node of type Operation for procedures
# implemented as functions in the original R script.  The function name and input
# parameters are obtained automatically from the calling environment. The outs
# parameters may be used optionally to create output data nodes. These MUST be
# passed as a list of strings, not names, unless the value is a file name.  Users
# can right-click on the procedure node in DDG Explorer to see the code for the
# function in the original script. For more details on outs parameters, see
# .ddg.create.output.nodes.
# outs (optional) - a list of names of data nodes to be created as outputs to
# this procedure node. These MUST be passed as a list of strings, not names,
# unless the value is a file name.  graphic.fext (optional) - the file extension
# for a graphics file.

ddg.function <- function(outs.graphic = NULL, outs.data = NULL, outs.exception = NULL,
    outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg") {
    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.inc(".ddg.func.depth")
    pname <- NULL
    .ddg.lookup.function.name(pname)
    if (interactive() && .ddg.get(".ddg.enable.console"))
        .ddg.console.node()
    # Look up input parameters from calling environment.
    call <- sys.call(-1)
    # Try to find the full call so that we can bind the parameters by name in the
    # DDG.  In the case that the function being executed has been passed as a
    # parameter to another function and is being called from the context (for
    # example, with lapply and other higher-order functions), the match.call will
    # fail.  In that case, we will use the call as it appears in side the
    # higher-order function.
    full.call <- tryCatch(match.call(sys.function(-1), call = call), error = function(e) call)
    # Create start node for the calling statement if one is not already created.
    .ddg.create.start.for.cur.cmd(call, sys.frame(-1))
    .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, outs.exception,
        outs.url, outs.file, graphic.fext, env = sys.frame(.ddg.get.frame.number(sys.calls())))
    invisible()
}

# ddg.procedure creates a procedure node of type Operation for procedures not
# implemented as functions in the original R script.  For more details on outs
# parameters, see .ddg.create.output.nodes.
# pname - the label for the node. Can be passed in as a string or as a name.  ins
# (optional) - a list of names of data nodes to be linked as inputs to this
# procedure node. These MUST be passed as as a list of strings, not names, unless
# the value is a file name.  outs (optional) - a list of names of data nodes to
# be created as outputs to this procedure node. These MUST be passed as a list of
# strings, not names, unless the value is a file name.  graphic.fext (optional) -
# file extension for graphics file.  env (optional) - the environment in which
# the procedure occurs.  It defaults to the global environment.

ddg.procedure <- function(pname, ins = NULL, outs.graphic = NULL, outs.data = NULL,
    outs.exception = NULL, outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg") {

    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)
    .proc.node("Operation", pname, pname)
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # Create the input edges if ins list provided.
    if (!is.null(ins)) {
        stack <- sys.calls()
        function.scope <- parent.frame()
        lapply(ins, function(param) {
            # First see if param is defined in the function where ddg.procedure is called. If
            # that does not find it, look in the scope of the function that calls the
            # function that calls ddg.procedure.  The difference probably depends on whether
            # ddg.procedure is used to annotate a chunk of code where param is really in the
            # local scope but used in the annotated chunk (the first case) or to annotate an
            # entire function and param is an actual funciton parameter (the second case).
            scope <- .ddg.get.scope(param, calls = stack)
            if (.ddg.is.local(param, function.scope)) {
                if (.ddg.data.node.exists(param, scope)) {
                  .ddg.data2proc(param, scope, pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else {
                  error.msg <- paste("No data node found for local", param)
                  .ddg.insert.error.message(error.msg)
                }
            } else if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
                .ddg.data2proc(param, scope, pname)
                if (.ddg.get("ddg.debug.lib"))
                  print(paste("param:", param))
            } else {
                scope <- .ddg.get.scope(param, for.caller = TRUE, calls = stack)
                if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
                  .ddg.data2proc(param, scope, pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else if (.ddg.data.node.exists(param, "undefined")) {
                  # This could be the case if the parameter is the name of a file rather than a
                  # variable in the program.
                  .ddg.data2proc(param, "undefined", pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else {
                  error.msg <- paste("No data node found for", param)
                  .ddg.insert.error.message(error.msg)
                }
            }
        })
    }
    # create output nodes
    .ddg.create.output.nodes(fname = "ddg.procedure", pname, outs.graphic, outs.data,
        outs.exception, outs.url, outs.file, graphic.fext, parent.frame())
    invisible()
}

# ddg.start creates a procedure node of type Start called pname.  Users can
# right-click on a start node in DDG Explorer and see the code between start and
# finish nodes in the original script.
# pname (optional) - the label for the node.  This can be passed as a string or
# as a name. It can be omitted if ddg.start is called by a function, in which
# case the name of the function will be used.
ddg.start <- function(pname = NULL) {
    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)
    # check for NULL.
    if (is.null(pname)) {
        msg <- "Cannot call ddg.start with NULL value from top-level."
        .ddg.insert.error.message(msg)
        return
    }
    # Create start node for the calling statement if one is not already created.
    frame.number <- .ddg.get.frame.number(sys.calls())
    env <- sys.frame(frame.number)
    call <- sys.call(frame.number)
    .ddg.create.start.for.cur.cmd(env)
    # Create start non-operational step.
    .proc.node("Start", pname, pname)
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
}

# ddg.finish creates a procedure node of type Finish called pname.  Users can
# right-click on a finish node in DDG Explorer and see the code between start and
# finish nodes in the original script.
# pname (optional) - the label for the node. This can be passed as a string or as
# a name. It can be omitted if ddg.finish is called by a function, in which case
# the name of the function will be used.
ddg.finish <- function(pname = NULL) {
    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)
    # check for NULL.
    if (is.null(pname)) {
        msg <- "Cannot call ddg.finish with NULL value from top-level."
        .ddg.insert.error.message(msg)
    }
    # Create finish non-operational step.
    .proc.node("Finish", pname, pname)
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # ddg.finish is added to the end of blocks.  We want the block to return the
    # value of the last R statement.
    return(.ddg.get(".ddg.last.R.value"))
}

# ddg.ret.value creates a data node for a function's return value. If the
# function is called from a console command and console mode is enabled, a data
# flow edge will be created linking this node to the console command that uses
# the value. ddg.ret.value returns the same value as the function (expr) and
# can be used in place of the function's normal return statement(s) if it is the
# last statement in the function.  Otherwise, it should be a parameter to return,
# as in return(ddg.ret.value(expr)). If expr is an assignment, nodes and edges
# are created for the assignment.
# expr - the value returned by the function.
ddg.ret.value <- function(expr = NULL, cmd.func = NULL) {
    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(expr)
    dev.file <- NULL
    parsed.stmt <- NULL
    # Capture graphics if dev.off is about to be called.
    if (!is.null(cmd.func)) {
        parsed.stmt <- cmd.func()
        if (parsed.stmt@has.dev.off) {
            if (.ddg.is.call.to(parsed.stmt@parsed[[1]], "dev.off") || !.ddg.get("ddg.loop.annotate")) {
                dev.file <- .ddg.capture.graphics(NULL)
                dev.node.name <- paste0("dev.", dev.cur())
            }
        }
    }
    # If expr is an assignment, create nodes and edges for the assignment.
    orig.expr <- substitute(expr)
    frame.num <- .ddg.get.frame.number(sys.calls())
    env <- sys.frame(frame.num)
    orig.return <- paste("return(", deparse(orig.expr), ")", sep = "")
    pname <- NULL
    .ddg.lookup.function.name(pname)
    # If this is a recursive call to ddg.ret.value, find the caller of the first
    # ddg.ret.value
    if (grepl("(^ddg|.ddg)", pname)) {
        caller.frame <- .ddg.find.ddg.ret.value.caller.frame.number()
        pname <- as.character(sys.call(caller.frame)[[1]])
    } else {
        caller.frame <- -1
    }
    # Prints the call & arguments.  expr forces evaluation of the function early.  I
    # think that causes some examples to work with debugging on but not off.
    # checking.  (6/26/2015 - Barb).  Yes, ReturnTest.R fails on the recursive f5
    # function
    ddg.ret.values <- .ddg.get(".ddg.ret.values")
    ddg.num.returns <- .ddg.get(".ddg.num.returns")
    if (nrow(ddg.ret.values) == ddg.num.returns) {
        size = 100
        new.rows <- data.frame(ddg.call = character(size), line = integer(size),
            ret.used = logical(size), ret.node.id = integer(size), stringsAsFactors = FALSE)
        .ddg.add.rows(".ddg.ret.values", new.rows)
        ddg.ret.values <- .ddg.get(".ddg.ret.values")
    }
    # If this is not a recursive call to ddg.ret.value and ddg.function was not
    # called, create the function nodes that it would have created.
    call <- sys.call(caller.frame)
    if (!.proc.node.exists(pname)) {
        full.call <- match.call(sys.function(caller.frame), call = call)
        .ddg.create.function.nodes(pname, call, full.call, auto.created = TRUE, env = sys.frame(.ddg.get.frame.number(sys.calls())))
    } else {
        .ddg.dec(".ddg.func.depth")
    }
    if (is.null(cmd.func)) {
        ret.stmt <- .construct.DDGStatement(parse(text = orig.return), pos = NA,
            script.num = NA)
    } else {
        ret.stmt <- cmd.func()
        parsed.statement <- ret.stmt@parsed
    }
    # Create a data node for the return value. We want the scope of the function that
    # called the function that called ddg.ret.
    call.text <- gsub(" ", "", deparse(call, nlines = 1))
    ret.node.name <- paste(call.text, "return")
    ret.node.name <- gsub("\"", "\\\\\"", ret.node.name)

    ret.node.scope <- environmentName(if (sys.nframe() == 2)
        .GlobalEnv else parent.env(sys.frame(caller.frame)))
    .ddg.save.data(ret.node.name, expr, fname = "ddg.return", scope = ret.node.scope)
    caller.env = sys.frame(caller.frame)
    # check if there is a return call within this call to ddg.ret.
    if (.has.call.to(parsed.stmt, "return")) {
        .proc.node("Operation", ret.stmt@abbrev, ret.stmt@abbrev, console = TRUE, cmd = ret.stmt)
        # Create control flow edge from preceding procedure node.
        .ddg.proc2proc()
        # Create an edge from the return statement to its return value.
        .ddg.proc2data(ret.stmt@abbrev, ret.node.name, ret.node.scope, ret.value = TRUE)
        if (!is.null(dev.file)) {
            ddg.file.out(dev.file, pname = ret.stmt@abbrev)
            # Remove the temporary file
            file.remove(dev.file)
            # Add an input edge from the current device
            .ddg.data2proc(dev.node.name, NULL, ret.stmt@abbrev)
        }
    } else {
        .ddg.lastproc2data(ret.node.name, dscope = ret.node.scope)
    }
    # Update the table.
    ddg.num.returns <- ddg.num.returns + 1
    ddg.ret.values$ddg.call[ddg.num.returns] <- call.text
    ddg.ret.values$ret.used[ddg.num.returns] <- FALSE
    ddg.ret.values$ret.node.id[ddg.num.returns] <- .ddg.get("ddg.dnum")
    ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    ddg.ret.values$line[ddg.num.returns] <- if (length(ddg.cur.cmd.stack) == 0)
        NA else ddg.cur.cmd.stack[length(ddg.cur.cmd.stack) - 1][[1]]@pos@startLine
    .global.set(".ddg.ret.values", ddg.ret.values)
    .global.set(".ddg.num.returns", ddg.num.returns)
    # If it does not have return, then its parameter was a call to ddg.eval and this
    # stuff has been done already.
    if (.has.call.to(parsed.stmt, "return")) {
        # Create edges from variables used in the return statement
        vars.used <- ret.stmt@vars.used
        for (var in vars.used) {
            # Make sure there is a node we could connect to.
            scope <- .ddg.get.scope(var)
            if (.ddg.data.node.exists(var, scope)) {
                .ddg.data2proc(var, scope, ret.stmt@abbrev)
            }
        }
        for (var in ret.stmt@vars.set) {
            if (var != "") {
                # Create output data node.
                dvalue <- eval(as.symbol(var), envir = env)
                # check for non-local assignment
                if (.ddg.is.nonlocal.assign(ret.stmt@parsed[[1]])) {
                  env <- .ddg.where(var, env = parent.env(parent.frame()), warning = FALSE)
                  if (identical(env, "undefined"))
                    env <- globalenv()
                }
                dscope <- .ddg.get.scope(var, env = env)
                .ddg.save.data(var, dvalue, scope = dscope)
                # Create an edge from procedure node to data node.
                .ddg.proc2data(ret.stmt@abbrev, var, dscope = dscope, ret.value = FALSE)
            }
        }
        # Create nodes and edges dealing with reading and writing files
        .ddg.create.file.read.nodes.and.edges(ret.stmt, env)
        .ddg.create.file.write.nodes.and.edges(ret.stmt, env)
        if (ret.stmt@createsGraphics) {
            .global.set.graphics.files(ret.stmt, env)
        }
    }
    # Create the finish node for the function
    if (typeof(call[[1]]) == "closure") {
        .add.abstract.node("Finish", node.name = pname, env = caller.env)
    } else {
        .add.abstract.node("Finish", node.name = paste(deparse(call), collapse = ""),
            env = caller.env)
    }
    return(expr)
}

# ddg.eval evaluates a statement and creates data flow edges from variable and
# function return nodes that are used in the statement. If the statement is an
# assignment statement, it also creates a data node for the variable assigned and
# a corresponding data flow edge. If ddg.eval is called from inside a function,
# cmd.func is a function that returns the corresponding DDGStatement object.  If
# ddg.eval is called from inside a control block, cmd.func is an integer that
# points to the corresponding DDGStatement object stored in the list
# .ddg.statements.
# statement - the statement to evaluate.
ddg.eval <- function(statement, cmd.func = NULL) {
    # Statement at top level.
    if (is.null(cmd.func)) {
        parsed.statement <- parse(text = statement)
        cmd <- NULL
    } else if (is.numeric(cmd.func)) {
        # Statement inside control block.
        num <- cmd.func
        statements <- .ddg.get("ddg.statements")
        cmd <- statements[[num]]
        parsed.statement <- cmd@parsed
        # Statement inside function.
    } else {
        cmd <- cmd.func()
        parsed.statement <- cmd@parsed
    }
    if (.ddg.get("ddg.debug.lib"))
        print(paste("ddg.eval: statement =", statement))
    frame.num <- .ddg.get.frame.number(sys.calls())
    env <- sys.frame(frame.num)
    if (!(.global.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))) {
        return(eval(parsed.statement, env))
    }
    if (interactive() && .ddg.get(".ddg.enable.console") && !(.global.is.set("from.source") && .ddg.get("from.source"))) {
        .ddg.console.node()
    }

    # If break statement, create procedure node and close open start nodes.
    if (!is.null(cmd) && cmd@text == "break") {
        .ddg.break.statement()
    }

    # If next statement, create procedure node and close open start nodes.
    if (!is.null(cmd) && cmd@text == "next") {
        .ddg.next.statement()
    }
    ret.value <- .ddg.parse.commands(parsed.statement, environ = env, run.commands = TRUE,
        node.name = statement, called.from.ddg.eval = TRUE, cmds = list(cmd))

    if (.ddg.get(".ddg.func.depth")) {
        if (!is.null(cmd)) {
            .ddg.link.function.returns(cmd)
        }
    }
    return(ret.value)
}
