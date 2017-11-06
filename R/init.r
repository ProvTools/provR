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
    annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10,
    save = TRUE) {
    .ddg.init.tables()
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

    if (save) {
        .ddg.set("ddg.save.to.disk", TRUE)
        # Overwrite default is
        if (!overwrite) {
            no.overwrite.folder <- paste(ddg.path, "_timestamps", sep = "")
            if (!dir.exists(no.overwrite.folder)) {
                dir.create(no.overwrite.folder)
            }
            ddg.path <- paste(no.overwrite.folder, "/", basename(tools::file_path_sans_ext(r.script.path)),
                "_ddg_", .format.time(Sys.time()), sep = "")
        }
        .ddg.set("ddg.path", ddg.path)
        # Remove files from DDG directory
        .ddg.delete.save()
        # Create DDG directories
        .ddg.init.environ()
        # Save copy of original script.
        file.copy(r.script.path, paste(paste(.ddg.get("ddg.path"), "/scripts", sep = ""), "/", basename(r.script.path),
            sep = ""))
    } else {
        .ddg.set("ddg.save.to.disk", FALSE)
    }
    # Reset r.script.path if RMarkdown file
    if (!is.null(r.script.path) && tools::file_ext(r.script.path) == "Rmd") {
        output.path <- paste(paste(.ddg.get("ddg.path"), "/scripts", sep = ""), "/", basename(tools::file_path_sans_ext(r.script.path)),
            ".R", sep = "")
        .ddg.markdown(r.script.path, output.path)
        .ddg.set("ddg.r.script.path", output.path)
    } else {
        .ddg.set("ddg.r.script.path", if (is.null(r.script.path))
            NULL else normalizePath(r.script.path, winslash = "/"))
    }
    # Set environment constants.
    .ddg.set(".ddg.enable.console", enable.console)
    .ddg.set(".ddg.func.depth", 0)
    .ddg.set(".ddg.explorer.port", 6096)
    .ddg.set("details.omitted", FALSE)
    # Initialize the information about the open start-finish blocks
    .ddg.set(".ddg.starts.open", vector())
    # Initialize the stack of commands and environments being executed in active
    # functions
    .ddg.set(".ddg.cur.cmd.stack", vector())
    .ddg.set(".ddg.cur.expr.stack", vector())
    # Mark graph as initilized.
    .ddg.set(".ddg.initialized", TRUE)
    # Store the starting graphics device.
    .ddg.set("prev.device", dev.cur())
    .ddg.set("possible.graphics.files.open", NULL)
    .ddg.set("ddg.open.devices", vector())
    if (interactive() && .ddg.get(".ddg.enable.console") && save) {
        ddg.history.file <- paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/.ddghistory", sep = "")
        .ddg.set(".ddg.history.file", ddg.history.file)
        # Empty file if it already exists, do the same with tmp file.
        file.create(ddg.history.file, showWarnings = FALSE)
        # One timestamp keeps track of last .ddg.save (the default).
        .write.timestamp.to.history()
        # Save the history if the platform supports it.
        tryCatch(savehistory(ddg.history.file), error = function(e) {
        })
    }
    # Store value of annotate.inside.
    .ddg.set("ddg.annotate.inside", annotate.inside.functions)
    # Store maximum number of loops to annotate.
    if (max.loops < 0)
        max.loops <- 10^10
    .ddg.set("ddg.max.loops", max.loops)
    # Store maximum snapshot size.
    .ddg.set("ddg.max.snapshot.size", max.snapshot.size)
    # If loops are not annotated, do not annotate functions called from inside a
    # loop.
    if (max.loops == 0)
        ddg.loop.annotate.off()
    # Initialize the counter that keeps track of nested levels of ifs and loops
    ddg.set.inside.loop()
    # Set number of first loop.
    .ddg.set("ddg.first.loop", first.loop)
    .ddg.set(".ddg.proc.start.time", .elapsed.time())
    # Store time when script begins execution.
    .ddg.set("ddg.start.time", .format.time(Sys.time()))
    invisible()
}
