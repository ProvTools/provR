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

# Global variables cannot be used directly in a library.  Instead, we need to
# place the variables in our own environment.  These functions make that
# environment easier to use.
.onLoad <- function(libname, pkgname) {
    .init.globals()
}

# .init.globals creates data frames to store the initial environment,
# procedure nodes, data nodes, edges, function return values, and checkpoints. It
# also initializes selected constants and variables.  Tables are saved as
# tab-delimited files in ddg.save.
.init.globals <- function() {
    # creates a table of non-ddg objects present in the R environment before
    # the script is executed.
    e <- globalenv()
    e.ls <- ls(e, all.names = TRUE)
    not.ddg.func <- function(name) {
        return(!grepl("ddg", name) && name != ".onLoad")
    }
    x <- Filter(not.ddg.func, e.ls)
    ddg.initial.env <- data.frame(x)
    colnames(ddg.initial.env) <- "ddg.name"
    .global.set("ddg.initial.env", ddg.initial.env)

    size <- 100
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

# prov.init intializes a new DDG.
# r.script.path (optional) - the full path to the R script file that is being
# executed. If provided, a copy of the script will be saved with the DDG.  ddgdir
# (optional) - the directory where the DDG should be saved.  If not provided, the
# DDG will be saved in a subdirectory called 'ddg' in the current working
# directory.  enable.console (optional) - if TRUE, console mode is turned on.
# annotate.inside.functions (optional) - if TRUE, functions are annotated.
# first.loop (optional) - the first loop to annotate in a for, while, or repeat
# statement.  max.loops (optional) - the maximum number of loops to annotate in a
# for, while, or repeat statement. If max.loops = -1 there is no limit.  If
# max.loops = 0, no loops are annotated.  If non-zero, if-statements are also
# annotated.  max.snapshot.size (optional) - the maximum size for objects that
# should be output to snapshot files. If 0, no snapshot files are saved.  If -1,
# all snapshot files are saved. Size in kilobytes.  Note that this tests the size
# of the object that will be turned into a snapshot, not the size of the
# resulting snapshot.  Addition : overwrite (optional) - default TRUE, if FALSE,
# generates timestamp for ddg directory
prov.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE,
    annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10) {
    .init.globals()
    # Setting the path for the ddg
    if (is.null(ddgdir)) {
        # Default is the file where the script is located
        if (!is.null(r.script.path)) {
            ddg.path <- paste(dirname(r.script.path), "/", basename(tools::file_path_sans_ext(r.script.path)),
                "_ddg", sep = "")
        } else {
            ddg.path <- paste(getwd(), "/", "ddg", sep = "")
        }
    } else ddg.path <- normalizePath(ddgdir, winslash = "/", mustWork = FALSE)

    # Reset r.script.path if RMarkdown file
    if (!is.null(r.script.path) && tools::file_ext(r.script.path) == "Rmd") {
        output.path <- paste(paste(.global.get("ddg.path"), "/scripts", sep = ""), "/", basename(tools::file_path_sans_ext(r.script.path)),
            ".R", sep = "")
        .ddg.markdown(r.script.path, output.path)
        .global.set("ddg.r.script.path", output.path)
    } else {
        .global.set("ddg.r.script.path", if (is.null(r.script.path))
            NULL else normalizePath(r.script.path, winslash = "/"))
    }
    # Set environment constants.
    .global.set(".ddg.enable.console", enable.console)
    .global.set(".ddg.func.depth", 0)
    .global.set(".ddg.explorer.port", 6096)
    .global.set("details.omitted", FALSE)
    # Initialize the information about the open start-finish blocks
    .global.set(".ddg.starts.open", vector())
    # Initialize the stack of commands and environments being executed in active
    # functions
    .global.set(".ddg.cur.cmd.stack", vector())
    .global.set(".ddg.cur.expr.stack", vector())
    # Mark graph as initilized.
    .global.set(".ddg.initialized", TRUE)
    # Store the starting graphics device.
    .global.set("prev.device", dev.cur())
    .global.set("possible.graphics.files.open", NULL)
    .global.set("ddg.open.devices", vector())
    # Store value of annotate.inside.
    .global.set("ddg.annotate.inside", annotate.inside.functions)
    # Store maximum number of loops to annotate.
    if (max.loops < 0)
        max.loops <- 10^10
    .global.set("ddg.max.loops", max.loops)
    # Store maximum snapshot size.
    .global.set("ddg.max.snapshot.size", max.snapshot.size)
    # If loops are not annotated, do not annotate functions called from inside a
    # loop.
    if (max.loops == 0)
        ddg.loop.annotate.off()
    # Initialize the counter that keeps track of nested levels of ifs and loops
    ddg.set.inside.loop()
    # Set number of first loop.
    .global.set("ddg.first.loop", first.loop)
    .global.set(".ddg.proc.start.time", .elapsed.time())
    # Store time when script begins execution.
    .global.set("ddg.start.time", .format.time(Sys.time()))
    invisible()
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
.init.file.write.functions.df <- function() {
    # Functions that read files
    function.names <- c("write.csv", "write.csv2", "write.table", "ggsave")
    # The argument that represents the file name
    param.names <- c("file", "file", "file", "filename")
    # Position of the file parameter if it is passed by position
    param.pos <- c(2, 2, 2, 1)
    return(data.frame(function.names, param.names, param.pos, stringsAsFactors = FALSE))
}

.global.set(".ddg.file.write.functions.df", .init.file.write.functions.df())

# Initialize the information about functions that initialize graphics devices
.init.graphics.functions.df <- function() {
    # Functions that read files
    function.names <- c("pdf", "postscript", "bmp", "jpeg", "png", "tiff", "X11")
    # The argument that represents the file name
    param.names <- c("file", "file", "filename", "filename", "filename", "filename",
        NA)
    # Position of the file parameter if it is passed by position
    param.pos <- c(1, 1, 1, 1, 1, 1, NA)
    return(data.frame(function.names, param.names, param.pos, stringsAsFactors = FALSE))
}

.global.set(".ddg.graphics.functions.df", .init.graphics.functions.df())

# Initialize the information about functions that update graphics
.global.set(".ddg.graphics.update.functions", ls(which(search() == "package:graphics")))
