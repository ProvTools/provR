# .ddg.proc.node creates a procedure node.

# ptype - type of procedure node.  pname - name of procedure node.  pvalue
# (optional) - value of procedure node.  console (optional) - if TRUE, console
# mode is enabled.  auto.created - TRUE means that the node is being
# automatically created when a return call is found ptime - elapsed time env -
# the environment in which the procedure occurs

# CHECK!  Looks like env parameter is not needed!
.ddg.proc.node <- function(ptype, pname, pvalue = "", console = FALSE, auto.created = FALSE,
    env = sys.frame(.ddg.get.frame.number(sys.calls())), cmd = NULL) {
    if (.ddg.get("ddg.debug.lib")) {
        if (length(pname) > 1) {
            print(sys.calls())
        }
    }
    # We're not in a console node but we're capturing data automatically.
    if (.ddg.get(".ddg.enable.console")) {
        # Capture graphic output of previous procedure node.  Comment out this
        # function??? .ddg.auto.graphic.node()
        if (!console && !(.ddg.is.set("from.source") && .ddg.get("from.source")) && interactive()) {
            .ddg.console.node()
        }
    }
    if (is.null(cmd)) {
        snum <- NA
        pos <- NA
    } else {
        snum <- cmd@script.num
        pos <- cmd@pos
    }
    # Record start & finish information
    if (ptype == "Start") {
        .ddg.starts.open <- .ddg.get(".ddg.starts.open")
        .ddg.starts.open <- c(.ddg.starts.open, pname)
        .ddg.set(".ddg.starts.open", .ddg.starts.open)
    } else if (ptype == "Finish") {
        .ddg.starts.open <- .ddg.get(".ddg.starts.open")
        num.starts.open <- length(.ddg.starts.open)
        if (num.starts.open > 0) {
            last.start.open <- .ddg.starts.open[num.starts.open]
            if (num.starts.open > 1) {
                .ddg.starts.open <- .ddg.starts.open[1:num.starts.open - 1]
            } else {
                .ddg.starts.open <- vector()
            }
            .ddg.set(".ddg.starts.open", .ddg.starts.open)
            if (last.start.open != pname) {
                .ddg.insert.error.message("Start and finish nodes do not match")
            }
        } else {
            .ddg.insert.error.message("Attempting to create a finish node when there are no open blocks")
        }
    }
    .ddg.set(".ddg.last.proc.node.created", paste(ptype, pname))
    ptime <- .elapsed.time()
    # Record in procedure node table
    .ddg.record.proc(ptype, pname, pvalue, auto.created, ptime, snum, pos)
    # if (ptype == 'Finish') print(sys.calls())
    if (.ddg.get("ddg.debug.lib"))
        print(paste("proc.node:", ptype, pname))
}

# .ddg.is.proc.node returns TRUE if the specified type supports input and output
# edges in an expanded DDG. Currently this includes all procedure node types
# except Start.

# type - procedure node type.

.ddg.is.proc.node <- function(type) {
    return(type == "Operation" | type == "Restore" | type == "Start" | type == "Finish" |
        type == "Binding")
}

# .ddg.proc.node.exists returns true if there is a procedure node with the given
# name

.ddg.proc.node.exists <- function(pname) {
    ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname & !ddg.proc.nodes$ddg.ret.linked[i] &
            !ddg.proc.nodes$ddg.auto.created[i]) {
            return(TRUE)
        }
    }
    return(FALSE)
}

# .ddg.proc.number gets the number of the nearest preceding matching Operation,
# or Restore node. It returns zero if no match is found.

# pname - name of procedure node.  find.unreturned.function - if true, only
# return the number if the procedure has not previously been linked to a return
# value

.ddg.proc.number <- function(pname, find.unreturned.function = FALSE) {
    ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
            if (!find.unreturned.function) {
                return(ddg.proc.nodes$ddg.num[i])
            }

            if (find.unreturned.function & !ddg.proc.nodes$ddg.ret.linked[i]) {
                return(ddg.proc.nodes$ddg.num[i])
            }
        }
    }
    # Error message if no match is found.
    error.msg <- paste("No procedure node found for", pname)
    if (.ddg.get("ddg.debug.lib"))
        print(sys.calls())
    .ddg.insert.error.message(error.msg)
    return(0)
}

# .ddg.last.proc.number returns the node number of the last procedure node in the
# ddg procedure node table. Procedure nodes are determined as defined in
# .ddg.is.proc.node above.

.ddg.last.proc.number <- function() {
    ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.ddg.is.proc.node(type))
            return(i)
    }
    .ddg.insert.error.message("No final procedure nodes")
    return(0)
}

# .ddg.proc.name returns the name of a procedure node. It returns a empty string
# if no match is found.

# pnum - node number in procedure node table.

.ddg.proc.name <- function(pnum) {
    if (pnum < 1 || pnum > .ddg.get("ddg.pnum")) {
        error.msg <- paste("No name found for procedure number", pnum)
        .ddg.insert.error.message(error.msg)
        return("")
    }
    return(.ddg.get("ddg.proc.nodes")$ddg.name[pnum])
}

# ddg.start creates a procedure node of type Start called pname.  Users can
# right-click on a start node in DDG Explorer and see the code between start and
# finish nodes in the original script.

# pname (optional) - the label for the node.  This can be passed as a string or
# as a name. It can be omitted if ddg.start is called by a function, in which
# case the name of the function will be used.

ddg.start <- function(pname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
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
    .ddg.proc.node("Start", pname, pname)
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
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)
    # check for NULL.
    if (is.null(pname)) {
        msg <- "Cannot call ddg.finish with NULL value from top-level."
        .ddg.insert.error.message(msg)
    }
    # Create finish non-operational step.
    .ddg.proc.node("Finish", pname, pname)
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # ddg.finish is added to the end of blocks.  We want the block to return the
    # value of the last R statement.
    return(.ddg.get(".ddg.last.R.value"))
}

# ddg.add.abstract.node is exclusively used in .ddg.parse.commands (so far) and
# simply serves to avoid repetition of code.

# type - type of procedure node.  cmd - command string.  called (optional) - name
# of calling function.

.ddg.add.abstract.node <- function(type, cmd = NULL, env, called = ".ddg.parse.commands",
    node.name = "") {
    if (node.name == "") {
        if (is.null(cmd)) {
            node.name <- .ddg.abbrev.cmd(cmd)
        } else {
            node.name <- cmd@abbrev
        }
    }
    if (.ddg.get("ddg.debug.lib"))
        print(paste(called, ":  Adding", node.name, type, "node"))
    .ddg.proc.node(type, node.name, node.name, TRUE, env = env, cmd = cmd)
    .ddg.proc2proc()
    return(node.name)
}

# .ddg.record.proc records a procedure node in the procedure node table.
# ptype - procedure node type.  pname - procedure node name.  pvalue - procedure
# node value.  auto.created - TRUE means the node is being created automatically
# when a return is found ptime - elapsed time snum - number of sourced script
# (main script = 0) pos - starting and ending lines and columns in source code
# (if available)
.ddg.record.proc <- function(ptype, pname, pvalue, auto.created = FALSE, ptime, snum = NA,
    pos = NA) {
    # Increment procedure node counter.
    .ddg.inc("ddg.pnum")
    ddg.pnum <- .ddg.get("ddg.pnum")
    # If the table is full, make it bigger.
    ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    if (nrow(ddg.proc.nodes) < ddg.pnum) {
        size = 100
        new.rows <- data.frame(ddg.type = character(size), ddg.num = numeric(size),
            ddg.name = character(size), ddg.value = character(size), ddg.ret.linked = logical(size),
            ddg.auto.created = logical(size), ddg.time = numeric(size), ddg.snum = numeric(size),
            ddg.startLine = numeric(size), ddg.startCol = numeric(size), ddg.endLine = numeric(size),
            ddg.endCol = numeric(size), stringsAsFactors = FALSE)
        .ddg.add.rows("ddg.proc.nodes", new.rows)
        ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    }

    ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
    ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
    ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
    ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
    ddg.proc.nodes$ddg.auto.created[ddg.pnum] <- auto.created
    ddg.proc.nodes$ddg.time[ddg.pnum] <- ptime
    ddg.proc.nodes$ddg.snum[ddg.pnum] <- snum

    if (is.object(pos) && length(pos@startLine == 1)) {
        ddg.proc.nodes$ddg.startLine[ddg.pnum] <- pos@startLine
        ddg.proc.nodes$ddg.startCol[ddg.pnum] <- pos@startCol
        ddg.proc.nodes$ddg.endLine[ddg.pnum] <- pos@endLine
        ddg.proc.nodes$ddg.endCol[ddg.pnum] <- pos@endCol
    } else {
        ddg.proc.nodes$ddg.startLine[ddg.pnum] <- NA
        ddg.proc.nodes$ddg.startCol[ddg.pnum] <- NA
        ddg.proc.nodes$ddg.endLine[ddg.pnum] <- NA
        ddg.proc.nodes$ddg.endCol[ddg.pnum] <- NA
    }
    .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
    # Output procedure node.
    .ddg.output.procedure.node(ptype, pname, pvalue, auto.created, ptime, snum, pos)
    if (.ddg.get("ddg.debug.lib")) {
        print(paste("Adding procedure node", ddg.pnum, "named", pname))
    }
}
