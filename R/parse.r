# Copyright (C) 2017 T. Pasquier M. Lau This program is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  This program is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
# the GNU General Public License for more details.  You should have received a
# copy of the GNU General Public v2 License along with this program.  If not, see
# <https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html>.  This package was
# forked from <https://github.com/End-to-end-provenance/RDataTracker>

# .ddg.parse.commands takes as input a list of parsed expressions from
# an R script and creates DDG nodes for each command. If environ is an
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that
# command, and then creates the data nodes based on the information
# available in the environment. If environ is not NULL, calls to
# ddg.* are not executed so only the clean script is processed.
# If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.ret.value
# are added to each function definition and ddg.eval is added to control
# statements before commands are processed. If save.debug is TRUE,
# changes to the script are saved in the ddg/debug directory.
# ddg.annotate.on and ddg.annotate.off may be used to limit the
# functions that are annotated or not annotated, respectively.
#
# If run.commands is false, the commands are not executed.  This allows
# us to build ddgs for commands run from the console as those commands
# have already been executed.

# exprs - list of parsed R statements
# script.name - name of the script the statements came from
# script.num - the number of the script in the sourced scripts table
# environ - environment in which commands should be
#   executed.
# ignore.patterns (optional) - a vector of regular expressions.
#   Any commands matching these expressions will not be parsed
#   (i.e. no nodes will be created for them).
# node.name (optional) - name for the collapsible node under which
#   this DDG should be stored.
# run.commands (optional) - commands are executed only when environ
#   is an environment and run.commands is TRUE.
# echo (optional) - print each expression after parsing
# print.eval (optional) - print result of each evaluation.
# max.deparse.length (optional) - maximum number of characters
#   output for deparse of a single expression.
# called.from.ddg.eval(optional) - whether called from ddg.eval
# cmds - list of DDG Statements that correspond to the exprs passed in.  This is
#   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#   creates the DDG Statement objects.

# .ddg.close.last.command.node closes the last created collapsible node stored in
# .ddg.last.cmd properly.

# env - the environment in which the close is occurring called (optional) - used
# in debugging to identify the function which called .ddg.close.previous.command.
# initial (optional) - if TRUE, try to close previous command node.

.ddg.close.last.command.node <- function(env, called = ".ddg.parse.commands", initial = FALSE) {
    # Get both the last command and new commands.
    .ddg.last.cmd <- if (.ddg.is.set(".ddg.last.cmd")) {
        .ddg.get(".ddg.last.cmd")
    } else {
        NULL
    }
    .ddg.possible.last.cmd <- if (.ddg.is.set(".ddg.possible.last.cmd")) {
        .ddg.get(".ddg.possible.last.cmd")
    } else {
        NULL
    }
    # Only create a finish node if a new command exists (i.e., we've parsed some
    # lines of code).
    if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
        cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd, env = env,
            called = paste(called, "-> .ddg.close.last.command.node"))
        # Add link from a function return node if there is one.
        .ddg.link.function.returns(.ddg.last.cmd)
        # No previous command.
        .ddg.set(".ddg.last.cmd", NULL)
    }
}

# .ddg.open.new.command.node opens a new collapsible command node depending on
# the information stored in .ddg.last.cmd.

# env - the environment in which the command occurs called (optional) - name of
# calling function.

.ddg.open.new.command.node <- function(env, called = ".ddg.parse.commands") {
    new.command <- .ddg.get(".ddg.possible.last.cmd")
    if (!is.null(new.command)) {
        .ddg.add.abstract.node("Start", new.command, env, called = paste(called,
            "-> .ddg.open.new.command.node"))
        # Now the new command becomes the last command, and new command is null.
        .ddg.set(".ddg.last.cmd", new.command)
        .ddg.set(".ddg.possible.last.cmd", NULL)
    }
}

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {
  ret.value <- NULL
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)

    if (.ddg.get("ddg.save.debug")) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)
  # Figure out if we will execute commands or not.
  execute <- run.commands & !is.null(environ) & is.environment(environ)
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)
  # Attempt to close the previous collapsible command node if a ddg
  # exists
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)
  # Get the last command in the new commands and check to see if
  # we need to create a new .ddg.last.cmd node for future reference.
  if (!inside.func) {
      .ddg.last.cmd <- cmds[[num.cmds]]
    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
    }
    else if (!execute) {
      cmds <- cmds[1:num.cmds-1]
    }
  }
  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""
  if (num.cmds > 0 && (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !inside.func && !called.from.ddg.eval) {
    .ddg.add.abstract.node("Start", node.name = node.name, env = environ)
    named.node.set <- TRUE
    start.node.created <- node.name
  }
  # Don't set .ddg.last.cmd.  We want it to have the value from
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if execute is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {
    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(cmds)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }
    # Loop over the commands as well as their string representations.
    for (i in 1:length(cmds)) {
      cmd <- cmds[[i]]
      if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))
      if ((.ddg.is.set("from.source") && .ddg.get("from.source")) && grepl("^ddg.eval", cmd@text) && .ddg.get(".ddg.enable.console")) {
        if (is.null(.ddg.last.cmd)) {
          .ddg.last.cmd <- cmd
        }
      }
      # Get environment for output data node.
      d.environ <- environ
      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
        if( identical(d.environ,"undefined") )
          d.environ <- globalenv()
      }
      # Check for control & loop statements.
      statement.type <- .ddg.get.statement.type(cmd@parsed[[1]])
      control.statement <- (statement.type == "if" || statement.type == "for" || statement.type == "while" || statement.type == "repeat" || statement.type == "{")
      loop.statement <- (statement.type == "for" || statement.type == "while" || statement.type == "repeat")
      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.
      create <- !cmd@isDdgFunc && (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && .ddg.get(".ddg.enable.console") && !(control.statement && .ddg.get("ddg.loop.annotate") && ddg.max.loops() > 0)
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE
      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {
        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd@text)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd@text, 1L, if (do.trunc)
                          max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }
          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)
            # Remember the current statement on the stack so that we
            # will be able to create a corresponding Finish node later
            # if needed.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")

            if (length(.ddg.cur.cmd.stack) == 0) {
              .ddg.cur.cmd.stack <- c(cmd, FALSE)
            }
            else {
              .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd, FALSE)
            }
            .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
          } else if (grepl("^ddg.(procedure|start|finish|restore)", cmd@text)) {
            # is procedure cmd?
            .ddg.set(".ddg.possible.last.cmd", NULL)
          }
          # Need to get this number before evaluating the command so that
          # when we evaluate a dev.off call we know which device was closed
          .ddg.set(".ddg.dev.number", dev.cur())
          if (cmd@has.dev.off && !cmd@createsGraphics && is.null(.ddg.get ("possible.graphics.files.open"))) {
            dev.file.created <- .ddg.capture.current.graphics()
          }
          else {
            dev.file.created <- NULL
          }
          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.
          if (.ddg.get("ddg.debug.lib")) print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))

          result <- withCallingHandlers(
              {
                for (annot in cmd@annotated) {
                  # Don't set ret.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || as.character(.ddg.get.statement.type(annot)) == "if") {
                    eval(annot, environ, NULL)
                  }
                  else {
                    ret.value <- eval(annot, environ, NULL)
                    .ddg.set (".ddg.last.R.value", ret.value)
                  }
                }
              },
            warning = function(w)
            {
              .ddg.set(".ddg.warning", w)
            },
            error = function(e)
            {
              # create procedure node for the error-causing operation
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
              .ddg.proc2proc()
              # create input edges by adding variables to set
              vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
              if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)
              # create and link to an error node
              ddg.exception.out("error.msg", toString(e) , cmd@abbrev)
            }
          )
          if (.ddg.get("ddg.debug.lib")) print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]
            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.abstract.node("Finish", cmd, environ)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd)
              # If the number of loop iterations exceeds max.loops, add
              # output data nodes containing final values to the finish node.
              if (loop.statement && .ddg.get("details.omitted")) {
                vars.set2 <- .ddg.add.to.vars.set(vars.set, cmd, i)
                .ddg.create.data.node.for.possible.writes(vars.set2, cmd, environ)
                .ddg.set("details.omitted", FALSE)
              }
            }
            # Remove the last command & start.created values pushed
            # onto the stack
            cur.cmd.closed <- (.ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            if (stack.length == 2) {
              .ddg.set(".ddg.cur.cmd.stack", vector())
            }
            else {
              .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
            }
          }
          # Print evaluation.
          if (print.eval) print(result)
        }
        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.
        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd@text)
        # We want to create a procedure node for this command.
        if (create.procedure) {
          # Create the procedure node.
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding operation node for", cmd@abbrev))

          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
          .ddg.proc2proc()
          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if ((.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          last.proc.node <- cmd
          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
            if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }
          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)
          if (cmd@readsFile) .ddg.create.file.read.nodes.and.edges(cmd, environ)

          .ddg.link.function.returns(cmd)
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, d.environ)
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))
          if (cmd@writesFile) .ddg.create.file.write.nodes.and.edges (cmd, environ)
          if (cmd@createsGraphics) .ddg.set.graphics.files (cmd, environ)
          if (cmd@updatesGraphics) .ddg.add.graphics.io (cmd)
          if (cmd@has.dev.off) {
            .ddg.capture.graphics(cmd)
            if (!is.null(dev.file.created)) {
              file.remove(dev.file.created)
            }
          }
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node(environ, initial=TRUE)
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, environ)
          }
        }
        if (create.procedure && execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }
     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!execute) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }
  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)
  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && named.node.set && !inside.func) {
      .ddg.add.abstract.node("Finish", node.name = node.name, env=environ)
  }
  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {
    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node(environ)
  }
  # Write time stamp to history.
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !.ddg.get(".ddg.is.sourced")) .write.timestamp.to.history()
  ret.value <- .ddg.get (".ddg.last.R.value")
  return(ret.value)
}

# .ddg.parse.lines takes as input a set of lines corresponding to the history of
# an R script or to an R script itself. It parses and converts them to executable
# commands. Each command might span multiple lines. The function returns a named
# list of commands.

# The contents of the list are: text - each entry is the full text string of a
# single command.  commands - each entry is the parsed command.

# lines - set of lines from command history or R script.

.ddg.parse.lines <- function(lines) {
    # No new lines passed in, so return NULL.
    if (length(lines) == 0)
        return(NULL)
    # Parse the new lines.
    parsed.commands <- parse(text = lines)
    parsed.commands <- Filter(function(cmd) {
        return(!is.call(cmd) || !grepl("^ddg.eval", cmd[[1]]))
    }, parsed.commands)
    return(parsed.commands)
}

# .ddg.parse.commands takes as input a list of parsed expressions from
# an R script and creates DDG nodes for each command. If environ is an
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that
# command, and then creates the data nodes based on the information
# available in the environment. If environ is not NULL, calls to
# ddg.* are not executed so only the clean script is processed.
# If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.ret.value
# are added to each function definition and ddg.eval is added to control
# statements before commands are processed. If save.debug is TRUE,
# changes to the script are saved in the ddg/debug directory.
# ddg.annotate.on and ddg.annotate.off may be used to limit the
# functions that are annotated or not annotated, respectively.
#
# If run.commands is false, the commands are not executed.  This allows
# us to build ddgs for commands run from the console as those commands
# have already been executed.

# exprs - list of parsed R statements
# script.name - name of the script the statements came from
# script.num - the number of the script in the sourced scripts table
# environ - environment in which commands should be
#   executed.
# ignore.patterns (optional) - a vector of regular expressions.
#   Any commands matching these expressions will not be parsed
#   (i.e. no nodes will be created for them).
# node.name (optional) - name for the collapsible node under which
#   this DDG should be stored.
# run.commands (optional) - commands are executed only when environ
#   is an environment and run.commands is TRUE.
# echo (optional) - print each expression after parsing
# print.eval (optional) - print result of each evaluation.
# max.deparse.length (optional) - maximum number of characters
#   output for deparse of a single expression.
# called.from.ddg.eval(optional) - whether called from ddg.eval
# cmds - list of DDG Statements that correspond to the exprs passed in.  This is
#   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#   creates the DDG Statement objects.

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {
  ret.value <- NULL
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)

    if (.ddg.get("ddg.save.debug")) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)
  # Figure out if we will execute commands or not.
  execute <- run.commands & !is.null(environ) & is.environment(environ)
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)
  # Attempt to close the previous collapsible command node if a ddg
  # exists
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)
  # Get the last command in the new commands and check to see if
  # we need to create a new .ddg.last.cmd node for future reference.
  if (!inside.func) {
      .ddg.last.cmd <- cmds[[num.cmds]]
    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
    }
    else if (!execute) {
      cmds <- cmds[1:num.cmds-1]
    }
  }
  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""
  if (num.cmds > 0 && (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !inside.func && !called.from.ddg.eval) {
    .ddg.add.abstract.node("Start", node.name = node.name, env = environ)
    named.node.set <- TRUE
    start.node.created <- node.name
  }
  # Don't set .ddg.last.cmd.  We want it to have the value from
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if execute is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {
    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(cmds)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }
    # Loop over the commands as well as their string representations.
    for (i in 1:length(cmds)) {
      cmd <- cmds[[i]]
      if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))
      if ((.ddg.is.set("from.source") && .ddg.get("from.source")) && grepl("^ddg.eval", cmd@text) && .ddg.get(".ddg.enable.console")) {
        if (is.null(.ddg.last.cmd)) {
          .ddg.last.cmd <- cmd
        }
      }
      # Get environment for output data node.
      d.environ <- environ
      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
        if( identical(d.environ,"undefined") )
          d.environ <- globalenv()
      }
      # Check for control & loop statements.
      st.type <- .ddg.get.statement.type(cmd@parsed[[1]])
      control.statement <- (st.type == "if" || st.type == "for" || st.type == "while" || st.type == "repeat" || st.type == "{")
      loop.statement <- (st.type == "for" || st.type == "while" || st.type == "repeat")
      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.
      create <- !cmd@isDdgFunc && (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && .ddg.get(".ddg.enable.console") && !(control.statement && .ddg.get("ddg.loop.annotate") && ddg.max.loops() > 0)
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE
      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {
        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd@text)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd@text, 1L, if (do.trunc)
                          max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }
          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)
            # Remember the current statement on the stack so that we
            # will be able to create a corresponding Finish node later
            # if needed.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")

            if (length(.ddg.cur.cmd.stack) == 0) {
              .ddg.cur.cmd.stack <- c(cmd, FALSE)
            }
            else {
              .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd, FALSE)
            }
            .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
          } else if (grepl("^ddg.(procedure|start|finish|restore)", cmd@text)) {
            # is procedure cmd?
            .ddg.set(".ddg.possible.last.cmd", NULL)
          }
          # Need to get this number before evaluating the command so that
          # when we evaluate a dev.off call we know which device was closed
          .ddg.set(".ddg.dev.number", dev.cur())
          if (cmd@has.dev.off && !cmd@createsGraphics && is.null(.ddg.get ("possible.graphics.files.open"))) {
            dev.file.created <- .ddg.capture.current.graphics()
          }
          else {
            dev.file.created <- NULL
          }
          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.
          if (.ddg.get("ddg.debug.lib")) print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))

          result <- withCallingHandlers(

              {
                for (annot in cmd@annotated) {
                  # Don't set ret.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || as.character(.ddg.get.statement.type(annot)) == "if") {
                    eval(annot, environ, NULL)
                  }
                  else {
                    ret.value <- eval(annot, environ, NULL)
                    .ddg.set (".ddg.last.R.value", ret.value)
                  }
                }
              },
            warning = function(w)
            {
                .ddg.set(".ddg.warning", w)
            },
            error = function(e)
            {
              # create procedure node for the error-causing operation
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
              .ddg.proc2proc()
              # create input edges by adding variables to set
              vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
              if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)
              # create and link to an error node
              ddg.exception.out("error.msg", toString(e) , cmd@abbrev)
            }
          )
          if (.ddg.get("ddg.debug.lib")) print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]
            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.abstract.node("Finish", cmd, environ)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd)
              # If the number of loop iterations exceeds max.loops, add
              # output data nodes containing final values to the finish node.
              if (loop.statement && .ddg.get("details.omitted")) {
                vars.set2 <- .ddg.add.to.vars.set(vars.set, cmd, i)
                .ddg.create.data.node.for.possible.writes(vars.set2, cmd, environ)
                .ddg.set("details.omitted", FALSE)
              }
            }
            # Remove the last command & start.created values pushed
            # onto the stack
            cur.cmd.closed <- (.ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            if (stack.length == 2) {
              .ddg.set(".ddg.cur.cmd.stack", vector())
            }
            else {
              .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
            }
          }
          # Print evaluation.
          if (print.eval) print(result)
        }
        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.
        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd@text)
        # We want to create a procedure node for this command.
        if (create.procedure) {
          # Create the procedure node.
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding operation node for", cmd@abbrev))

          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
          .ddg.proc2proc()
          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if ((.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          last.proc.node <- cmd
          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
            if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }
          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)
          if (cmd@readsFile) .ddg.create.file.read.nodes.and.edges(cmd, environ)

          .ddg.link.function.returns(cmd)
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, d.environ)
          if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))
          if (cmd@writesFile) .ddg.create.file.write.nodes.and.edges (cmd, environ)
          if (cmd@createsGraphics) .ddg.set.graphics.files (cmd, environ)
          if (cmd@updatesGraphics) .ddg.add.graphics.io (cmd)
          if (cmd@has.dev.off) {
            .ddg.capture.graphics(cmd)
            if (!is.null(dev.file.created)) {
              file.remove(dev.file.created)
            }
          }
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node(environ, initial=TRUE)
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.get("ddg.debug.lib")) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, environ)
          }
        }
        if (create.procedure && execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }
     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!execute) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }
  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)
  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && named.node.set && !inside.func) {
      .ddg.add.abstract.node("Finish", node.name = node.name, env=environ)
  }
  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {
    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node(environ)
  }
  # Write time stamp to history.
  if ((.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && !.ddg.get(".ddg.is.sourced")) .write.timestamp.to.history()
  ret.value <- .ddg.get (".ddg.last.R.value")
  return(ret.value)
}

# .ddg.console.node creates a console node.
.ddg.console.node <- function() {
    # Don't do anything if sourcing, because history isn't necessary in this case.
    if ((.ddg.is.set("from.source") && .ddg.get("from.source")))
        return(NULL)
    ddg.history.file = .ddg.get(".ddg.history.file")
    ddg.history.timestamp = .ddg.get(".ddg.history.timestamp")
    # Only continue if these values exists.
    if (!(is.null(ddg.history.file) || is.null(ddg.history.timestamp))) {
        # Grab any new commands that might still be in history.
        tryCatch({
            # Saving history is not supported on all platforms.
            .ddg.save.history(ddg.history.file)
            # Load from extended history since last time we wrote out a console node.
            new.lines <- .ddg.load.history(ddg.history.file, ddg.history.timestamp)
            # Parse the lines into individual commands.
            parsed.commands <- .ddg.parse.lines(new.lines)
            # New commands since last timestamp.
            if (!is.null(parsed.commands) && length(parsed.commands) > 0) {
                .ddg.parse.commands(parsed.commands, environ = .GlobalEnv, run.commands = FALSE)
            }
        }, error = function(e) {
        })

    }
}
