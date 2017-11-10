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

# .proc.node creates a procedure node.
# ptype - type of procedure node.  pname - name of procedure node.  pvalue
# (optional) - value of procedure node.  console (optional) - if TRUE, console
# mode is enabled.  auto.created - TRUE means that the node is being
# automatically created when a return call is found ptime - elapsed time env -
# the environment in which the procedure occurs
.proc.node <- function(ptype, pname, pvalue = "", console = FALSE, auto.created = FALSE, cmd = NULL) {
    if (.global.get("ddg.debug.lib")) {
        if (length(pname) > 1) {
            print(sys.calls())
        }
    }
    # We're not in a console node but we're capturing data automatically.
    if (.global.get(".ddg.enable.console")) {
        # Capture graphic output of previous procedure node.  Comment out this
        # function??? .ddg.auto.graphic.node()
        if (!console && !(.global.is.set("from.source") && .global.get("from.source")) && interactive()) {
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
        .ddg.starts.open <- .global.get(".ddg.starts.open")
        .ddg.starts.open <- c(.ddg.starts.open, pname)
        .global.set(".ddg.starts.open", .ddg.starts.open)
    } else if (ptype == "Finish") {
        .ddg.starts.open <- .global.get(".ddg.starts.open")
        num.starts.open <- length(.ddg.starts.open)
        if (num.starts.open > 0) {
            last.start.open <- .ddg.starts.open[num.starts.open]
            if (num.starts.open > 1) {
                .ddg.starts.open <- .ddg.starts.open[1:num.starts.open - 1]
            } else {
                .ddg.starts.open <- vector()
            }
            .global.set(".ddg.starts.open", .ddg.starts.open)
            if (last.start.open != pname) {
                .ddg.insert.error.message("Start and finish nodes do not match")
            }
        } else {
            .ddg.insert.error.message("Attempting to create a finish node when there are no open blocks")
        }
    }
    .global.set(".ddg.last.proc.node.created", paste(ptype, pname))
    ptime <- .elapsed.time()
    # Record in procedure node table
    .record.proc(ptype, pname, pvalue, auto.created, ptime, snum, pos)
    # if (ptype == 'Finish') print(sys.calls())
    if (.global.get("ddg.debug.lib"))
        print(paste("proc.node:", ptype, pname))
}

# .is.proc.node returns TRUE if the specified type supports input and output
# edges in an expanded DDG. Currently this includes all procedure node types
# except Start.

# type - procedure node type.

.is.proc.node <- function(type) {
    return(type == "Operation" | type == "Restore" | type == "Start" | type == "Finish" |
        type == "Binding")
}

# .proc.node.exists returns true if there is a procedure node with the given
# name

.proc.node.exists <- function(pname) {
    ddg.proc.nodes <- .global.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname & !ddg.proc.nodes$ddg.ret.linked[i] &
            !ddg.proc.nodes$ddg.auto.created[i]) {
            return(TRUE)
        }
    }
    return(FALSE)
}

# .proc.number gets the number of the nearest preceding matching Operation,
# or Restore node. It returns zero if no match is found.

# pname - name of procedure node.  find.unreturned.function - if true, only
# return the number if the procedure has not previously been linked to a return
# value

.proc.number <- function(pname, find.unreturned.function = FALSE) {
    ddg.proc.nodes <- .global.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
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
    if (.global.get("ddg.debug.lib"))
        print(sys.calls())
    .ddg.insert.error.message(error.msg)
    return(0)
}

# .last.proc.number returns the node number of the last procedure node in the
# ddg procedure node table. Procedure nodes are determined as defined in
# .is.proc.node above.

.last.proc.number <- function() {
    ddg.proc.nodes <- .global.get("ddg.proc.nodes")
    rows <- nrow(ddg.proc.nodes)
    for (i in rows:1) {
        type <- ddg.proc.nodes$ddg.type[i]
        if (.is.proc.node(type))
            return(i)
    }
    .ddg.insert.error.message("No final procedure nodes")
    return(0)
}

# .proc.name returns the name of a procedure node. It returns a empty string
# if no match is found.

# pnum - node number in procedure node table.

.proc.name <- function(pnum) {
    if (pnum < 1 || pnum > .global.get("ddg.pnum")) {
        error.msg <- paste("No name found for procedure number", pnum)
        .ddg.insert.error.message(error.msg)
        return("")
    }
    return(.global.get("ddg.proc.nodes")$ddg.name[pnum])
}

# ddg.add.abstract.node is exclusively used in .ddg.parse.commands (so far) and
# simply serves to avoid repetition of code.

# type - type of procedure node.  cmd - command string.  called (optional) - name
# of calling function.

.add.abstract.node <- function(type, cmd = NULL, env, called = ".ddg.parse.commands",
    node.name = "") {
    if (node.name == "") {
        if (is.null(cmd)) {
            node.name <- .abbrev.cmd(cmd)
        } else {
            node.name <- cmd@abbrev
        }
    }
    if (.global.get("ddg.debug.lib"))
        print(paste(called, ":  Adding", node.name, type, "node"))
    .proc.node(type, node.name, node.name, TRUE, cmd = cmd)
    .ddg.proc2proc()
    return(node.name)
}

# .record.proc records a procedure node in the procedure node table.
# ptype - procedure node type.  pname - procedure node name.  pvalue - procedure
# node value.  auto.created - TRUE means the node is being created automatically
# when a return is found ptime - elapsed time snum - number of sourced script
# (main script = 0) pos - starting and ending lines and columns in source code
# (if available)
.record.proc <- function(ptype, pname, pvalue, auto.created = FALSE, ptime, snum = NA,
    pos = NA) {
    # Increment procedure node counter.
    .ddg.inc("ddg.pnum")
    ddg.pnum <- .global.get("ddg.pnum")
    # If the table is full, make it bigger.
    ddg.proc.nodes <- .global.get("ddg.proc.nodes")
    if (nrow(ddg.proc.nodes) < ddg.pnum) {
        size = 100
        new.rows <- data.frame(ddg.type = character(size), ddg.num = numeric(size),
            ddg.name = character(size), ddg.value = character(size), ddg.ret.linked = logical(size),
            ddg.auto.created = logical(size), ddg.time = numeric(size), ddg.snum = numeric(size),
            ddg.startLine = numeric(size), ddg.startCol = numeric(size), ddg.endLine = numeric(size),
            ddg.endCol = numeric(size), stringsAsFactors = FALSE)
        .ddg.add.rows("ddg.proc.nodes", new.rows)
        ddg.proc.nodes <- .global.get("ddg.proc.nodes")
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
    .global.set("ddg.proc.nodes", ddg.proc.nodes)
    pname <- gsub("\\\"", "\\\\\"", pname)
    # Output procedure node.
    .json.procedure.node(ddg.pnum, pname, ptype, ptime, snum, pos)
    if (.global.get("ddg.debug.lib")) {
        print(paste("Adding procedure node", ddg.pnum, "named", pname))
    }
}
