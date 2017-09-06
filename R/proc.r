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
    ptime <- .ddg.elapsed.time()
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
