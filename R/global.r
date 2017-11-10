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

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead, we need to
# place the variables in our own environment.  These functions make that
# environment easier to use.
.onLoad <- function(libname, pkgname) {
    .ddg.init.tables()
}

.global.set <- function(var, value) {
    .ddg.env[[var]] <- value
    return(invisible(.ddg.env[[var]]))
}

.global.is.set <- function(var) {
    return(exists(var, envir = .ddg.env))
}

.global.get <- function(var) {
    if (!.global.is.set(var)) {
        error.msg <- paste("No binding for", var, ". DDG may be incorrect!")
        .ddg.insert.error.message(error.msg)
        return(NULL)
    } else {
        return(.ddg.env[[var]])
    }
}

# .ddg.clear removes all objects from the .ddg.env environment.
.ddg.clear <- function() {
    # reinitialize tables
    .ddg.init.tables()
}

##### Mutators for specific common actions
.ddg.inc <- function(var) {
    value <- .global.get(var)
    .global.set(var, value + 1)
}

.ddg.dec <- function(var) {
    value <- .global.get(var)
    .global.set(var, value - 1)
}

.ddg.append.activity <- function(...) {
    text <- .global.get("ddg.activity")
    if (text != "") {
        text <- paste(text, ",\n")
    }
    .global.set("ddg.activity", paste(text, ..., sep = ""))
}

.ddg.append.entity <- function(...) {
    text <- .global.get("ddg.entity")
    if (text != "") {
        text <- paste(text, ",\n")
    }
    .global.set("ddg.entity", paste(text, ..., sep = ""))
}

.ddg.append.wasInformedBy <- function(...) {
    text <- .global.get("ddg.wasInformedBy")
    if (text != "") {
        text <- paste(text, ",\n")
    }
    .global.set("ddg.wasInformedBy", paste(text, ..., sep = ""))
}

.ddg.append.wasGeneratedBy <- function(...) {
    text <- .global.get("ddg.wasGeneratedBy")
    if (text != "") {
        text <- paste(text, ",\n")
    }
    .global.set("ddg.wasGeneratedBy", paste(text, ..., sep = ""))
}

.ddg.append.used <- function(...) {
    text <- .global.get("ddg.used")
    if (text != "") {
        text <- paste(text, ",\n")
    }
    .global.set("ddg.used", paste(text, ..., sep = ""))
}

.ddg.add.rows <- function(df, new.rows) {
    table <- .global.get(df)
    .global.set(df, rbind(table, new.rows))
}

.ddg.push <- function(x, value) {
    return(assign(as.character(substitute(x)), c(x, value), parent.frame()))
}

.ddg.pop <- function(x) {
    return(assign(as.character(substitute(x)), x[-length(x)], parent.frame()))
}
#-------------------BASIC FUNCTIONS-----------------------#

# .ddg.get.initial.env creates a table of non-ddg objects present in the R
# environment before the script is executed.

.ddg.get.initial.env <- function() {
    e <- globalenv()
    e.ls <- ls(e, all.names = TRUE)
    not.ddg.func <- function(name) {
        return(!grepl("ddg", name) && name != ".onLoad")
    }
    x <- Filter(not.ddg.func, e.ls)
    ddg.initial.env <- data.frame(x)
    colnames(ddg.initial.env) <- "ddg.name"
    .global.set("ddg.initial.env", ddg.initial.env)
}

# .ddg.init.tables creates data frames to store the initial environment,
# procedure nodes, data nodes, edges, function return values, and checkpoints. It
# also initializes selected constants and variables.  Tables are saved as
# tab-delimited files in ddg.save.

.ddg.init.tables <- function() {
    size <- 100
    .ddg.get.initial.env()
    .global.set("ddg.proc.nodes", data.frame(ddg.type = character(size), ddg.num = numeric(size),
        ddg.name = character(size), ddg.value = character(size), ddg.ret.linked = logical(size),
        ddg.auto.created = logical(size), ddg.time = numeric(size), ddg.snum = numeric(size),
        ddg.startLine = numeric(size), ddg.startCol = numeric(size), ddg.endLine = numeric(size),
        ddg.endCol = numeric(size), stringsAsFactors = FALSE))
    .global.set("ddg.data.nodes", data.frame(ddg.type = character(size), ddg.num = numeric(size),
        ddg.name = character(size), ddg.value = character(size), ddg.val.type = character(size),
        ddg.scope = character(size), ddg.from.env = logical(size), ddg.time = character(size),
        ddg.loc = character(size), ddg.current = logical(size), stringsAsFactors = FALSE))
    .global.set("ddg.edges", data.frame(ddg.num = numeric(size), ddg.type = character(size),
        ddg.from = character(size), ddg.to = character(size), stringsAsFactors = FALSE))
    # Create procedure and data node counters.
    .global.set("ddg.pnum", 0)
    .global.set("ddg.dnum", 0)
    .global.set("ddg.enum", 0)
    # Create strings used to generate the JSON file.
    .global.set("ddg.activity", "")
    .global.set("ddg.entity", "")
    .global.set("ddg.wasInformedBy", "")
    .global.set("ddg.wasGeneratedBy", "")
    .global.set("ddg.used", "")
    # Used to control debugging output.  If already defined, don't change its value.
    if (!.global.is.set("ddg.debug.lib"))
        .global.set("ddg.debug.lib", FALSE)
    # Used to control script debugging.
    .global.set("ddg.break", FALSE)
    .global.set("ddg.break.ignore", FALSE)
    # Used to control sourcing. If already defined, don't change its value.
    if (!.global.is.set("from.source"))
        .global.set("from.source", FALSE)
    # Set current number of checkpoints.
    .global.set("ddg.checkpoint.num", 0)
    # Record last command from the preceding console block.
    .global.set(".ddg.last.cmd", NULL)
    # Record value returned by calls to ddg.ret.  ddg.call - the string
    # representing the call, like 'f(a)'.  line - the line where the function is
    # called that is now returning ret.used - remembers if this function return
    # value has been linked to the caller.  ret.node.id - the id of the data node
    # that holds the return value.
    .global.set(".ddg.ret.values", data.frame(ddg.call = character(size), line = integer(size),
        ret.used = logical(size), ret.node.id = integer(size), stringsAsFactors = FALSE))
    .global.set(".ddg.num.returns", 0)
    # Record the current command to be opened during console execution (used when
    # executing a script using ddg.source).
    .global.set(".ddg.possible.last.cmd", NULL)
    # Keep track of history.
    .global.set(".ddg.history.timestamp", NULL)
    # Keep track of the last device seen (0 implies NULL).
    .global.set("prev.device", 0)
    # Store path of current script.
    .global.set("ddg.r.script.path", NULL)
    # Store path of current ddg.
    .global.set("ddg.path", NULL)
    # No ddg initialized.
    .global.set(".ddg.initialized", FALSE)
    # No history file.
    .global.set(".ddg.history.file", NULL)
    # Console is disabled.
    .global.set(".ddg.enable.console", FALSE)
    # Functions to be annotated.
    .global.set("ddg.annotate.on", NULL)
    # Functions not to be annotated.
    .global.set("ddg.annotate.off", NULL)
    # Script sourced with ddg.source
    .global.set(".ddg.is.sourced", FALSE)
    # Number of first sourced script (main script).
    .global.set(".ddg.next.script.num", 0)
    # Table of sourced scripts
    .global.set(".ddg.sourced.scripts", NULL)
    # Save debug files on debug directory
    .global.set("ddg.save.debug", FALSE)
    # DDGStatement number
    .global.set("ddg.statement.num", 0)
    # DDGStatements list
    .global.set("ddg.statements", list())
    # Control loop number
    .global.set("ddg.loop.num", 0)
    # Control loop list
    .global.set("ddg.loops", list())
    # Loop annotation
    .global.set("ddg.loop.annotate", TRUE)
    # Set max.snapshot.size for console mode.
    if (!.global.is.set("ddg.max.snapshot.size")) {
        .global.set("ddg.max.snapshot.size", 100)
    }
    # List of files read and written
    .global.set("ddg.infilenodes", list())
    .global.set("ddg.outfilenodes", list())
    # Data frame containing file reads and writes
    .global.set("ddg.hashtable", data.frame())
}

# Initialize the information about functions that read from files
.ddg.create.file.read.functions.df <- function() {
    # Functions that read files
    function.names <- c("source", "read.csv", "read.csv2", "read.delim", "read.delim2",
        "read.table", "read.xls", "file", "readLines")
    # The argument that represents the file name
    param.names <- c("file", "file", "file", "file", "file", "file", "xls", "description",
        "con")
    # Position of the file parameter if it is passed by position
    param.pos <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    return(data.frame(function.names, param.names, param.pos, stringsAsFactors = FALSE))
}

.global.set(".ddg.file.read.functions.df", .ddg.create.file.read.functions.df())

# Initialize the information about functions that read from files
.ddg.create.file.write.functions.df <- function() {
    # Functions that read files
    function.names <- c("write.csv", "write.csv2", "write.table", "ggsave")
    # The argument that represents the file name
    param.names <- c("file", "file", "file", "filename")
    # Position of the file parameter if it is passed by position
    param.pos <- c(2, 2, 2, 1)
    return(data.frame(function.names, param.names, param.pos, stringsAsFactors = FALSE))
}

.global.set(".ddg.file.write.functions.df", .ddg.create.file.write.functions.df())

# Initialize the information about functions that initialize graphics devices
.ddg.create.graphics.functions.df <- function() {
    # Functions that read files
    function.names <- c("pdf", "postscript", "bmp", "jpeg", "png", "tiff", "X11")
    # The argument that represents the file name
    param.names <- c("file", "file", "filename", "filename", "filename", "filename",
        NA)
    # Position of the file parameter if it is passed by position
    param.pos <- c(1, 1, 1, 1, 1, 1, NA)
    return(data.frame(function.names, param.names, param.pos, stringsAsFactors = FALSE))
}

.global.set(".ddg.graphics.functions.df", .ddg.create.graphics.functions.df())

# Initialize the information about functions that update graphics
.global.set(".ddg.graphics.update.functions", ls(which(search() == "package:graphics")))
