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

# ddg.first.loop returns the value of the parameter first.loop.

ddg.first.loop <- function() {
    return(.ddg.get("ddg.first.loop"))
}

# ddg.max.loops returns the value of the parameter max.loops.

ddg.max.loops <- function() {
    return(.ddg.get("ddg.max.loops"))
}

# ddg.loop.annotate.on turns on loop annotation.
ddg.loop.annotate.on <- function() {
    .ddg.set("ddg.loop.annotate", TRUE)
}

# ddg.loop.annotate.off turns off loop annotation.
ddg.loop.annotate.off <- function() {
    .ddg.set("ddg.loop.annotate", FALSE)
}

ddg.set.inside.loop <- function() {
    if (!.ddg.is.set("ddg.inside.loop")) {
        .ddg.set("ddg.inside.loop", 0)
    } else {
        .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") + 1)
    }
}

ddg.not.inside.loop <- function() {
    .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") - 1)
}

# ddg.loop.count returns the current count for the specified loop.

ddg.loop.count <- function(loop.num) {
    ddg.loops <- .ddg.get("ddg.loops")
    return(ddg.loops[[loop.num]])
}

# ddg.loop.count.inc increments the current count for the specified loop and
# returns the incremented value.

ddg.loop.count.inc <- function(loop.num) {
    ddg.loops <- .ddg.get("ddg.loops")
    ddg.loops[[loop.num]] <- ddg.loops[[loop.num]] + 1
    .ddg.set("ddg.loops", ddg.loops)
    return(ddg.loops[[loop.num]])
}

# ddg.reset.loop.count sets the current count for the specified loop to zero.

ddg.reset.loop.count <- function(loop.num) {
    ddg.loops <- .ddg.get("ddg.loops")
    ddg.loops[loop.num] <- 0
    .ddg.set("ddg.loops", ddg.loops)
}

# ddg.for.loop inserts a procedure node and a data node in a for loop, indicating
# the value currently assigned to the index variable.

ddg.forloop <- function(index.var) {
    index.name <- as.character(deparse(substitute(index.var)))
    pnode.name <- paste(index.name, "<-", index.var)
    dscope <- .ddg.get.scope(index.name)
    .proc.node("Operation", pnode.name, pnode.name)
    .ddg.proc2proc()
    .ddg.data.node("Data", index.name, index.var, dscope, from.env = FALSE)
    .ddg.proc2data(pnode.name, index.name)
}

# ddg.details.omitted inserts an operational node called 'Details Omitted' in
# cases where not all iterations of a loop are annotated.  This may happen if the
# number of the first loop to be annotaed (first.loop) is greater than 1 and/or
# if the total number of loops to be annotated is less than the actual number of
# iterations.  It also sets a variable to remember that the last construct is
# incomplete so that the right data nodes get created.

ddg.details.omitted <- function() {
    pnode.name <- "Details Omitted"
    .proc.node("Incomplete", pnode.name, pnode.name)
    .ddg.proc2proc()
    .ddg.set("details.omitted", TRUE)
    if (.ddg.get("ddg.debug.lib")) {
        print("Adding Details Omitted node")
    }
}
