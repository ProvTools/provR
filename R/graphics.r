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

# .ddg.supported.graphic - the sole purpose of this function is to verify that
# the input file extension is a supported graphic type. Currently supported
# graphics types inlude: jpg, jpeg, bmp, png, tiff.
# ext - file extension.
.ddg.supported.graphic <- function(ext) {
    return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# .ddg.is.graphic tries to decipher if the value snapshot should be written to
# file directly from the data or if it is a graphic which can be captured from
# the image device. This function, as written, is basically a hack. There must be
# a better way to implement it.
# value - input value.
.ddg.is.graphic <- function(value) {
    # Matching any of these classes automatically classifies the object as a graphic.
    graph.classes <- list("gg", "ggplot")
    return(is.object(value) && any(class(value) %in% graph.classes))
}

# .ddg.auto.graphic.node attempts to figure out if a new graphics device has been
# created and take a snapshot of a previously active device, setting the snapshot
# node to be the output of the specified command.
# cmd.abbrev (optional) - name of procedure node.  dev.to.capture (optional) -
# function specifying which device should be captured, where zero indicates no
# device and negative values are ignored.
.ddg.auto.graphic.node <- function(cmd.abbrev = NULL, dev.to.capture = .ddg.dev.change) {
    num.dev.to.capture <- dev.to.capture()
    if (num.dev.to.capture > 1) {
        # Make the capture device active (store info on previous device).
        prev.device <- dev.cur()
        dev.set(num.dev.to.capture)
        # Capture it as a jpeg.

        if (!is.null(cmd.abbrev) && cmd.abbrev != "")
          name <- paste0("graphic", substr(cmd.abbrev, 1, 10))
        else
          name <- "graphic"
        .snapshot.node(name, fext = "jpeg", data = NULL)
        # Make the previous device active again.
        dev.set(prev.device)
        # We're done, so create the edge.
        if (is.null(cmd.abbrev))
            .ddg.lastproc2data(name, all = FALSE) else .ddg.proc2data(cmd.abbrev, name)
    }
}

# Given a parse tree, this function returns a list containing the expressions
# that correspond to the filename argument of the calls to functions that create
# graphics devices.  If there are none, it returns NULL.
.global.set.graphics.files <- function(main.object, env) {
    # Allows dev.print to work when we want to save the plot.
    tryCatch(dev.control("enable"), error = function(e) return())
    # Add the newly-opened graphics device to the list of open devices
    .global.set("ddg.open.devices", union(.global.get("ddg.open.devices"), dev.cur()))
    # Find all the graphics files that have potentially been opened.  Remember these
    # file names until we find the dev.off call and then determine which was written.
    new.possible.graphics.files.open <- .ddg.find.files(main.object, .global.get(".ddg.graphics.functions.df"),
        env)
    if (!is.null(new.possible.graphics.files.open)) {
        if (!is.null(.global.get("possible.graphics.files.open"))) {
            possible.graphics.files.open <- .global.get("possible.graphics.files.open")
            .global.set("possible.graphics.files.open", c(new.possible.graphics.files.open,
                possible.graphics.files.open))
        } else {
            .global.set("possible.graphics.files.open", new.possible.graphics.files.open)
        }
    }
    dev.node.name <- paste0("dev.", dev.cur())
    .ddg.data.node("Data", dev.node.name, "graph", NULL)
    .ddg.proc2data(main.object@abbrev, dev.node.name)
}

# Add data in and data out nodes that represent the current device.  cmd -
# Assumed to be a function that modifies the graphics device, such as a function
# in the base graphics package.
.ddg.add.graphics.io <- function(cmd) {
    # Try adding the input edge.  It is not a problem if the node can't be found.  It
    # means that the output is going to the RStudio window, not a file, so there has
    # been no call like pdf or jpg that would have created the data node.
    dev.node.name <- paste0("dev.", dev.cur())
    if (dev.cur() %in% .global.get("ddg.open.devices")) {
        .ddg.data2proc(dev.node.name, NULL, cmd@abbrev)
    } else {
        # Add the newly-opened graphics device to the list of open devices
        .global.set("ddg.open.devices", union(.global.get("ddg.open.devices"), dev.cur()))
    }
    # Add an output node with the same name
    .ddg.data.node("Data", dev.node.name, "graph", NULL)
    .ddg.proc2data(cmd@abbrev, dev.node.name)

}

.ddg.capture.graphics <- function(cmd, called.from.save = FALSE) {
    if (is.null(cmd))
      proc.node.name <- NULL
    else if (is.character(cmd))
      proc.node.name <- cmd
    else
      proc.node.name <- cmd@abbrev
    dev.number <- .global.get(".ddg.dev.number")
    .global.set("ddg.open.devices", setdiff(.global.get("ddg.open.devices"), dev.number))
    if (!is.null(.global.get("possible.graphics.files.open")) && !is.null(proc.node.name)) {
        possible.graphics.files.open <- .global.get("possible.graphics.files.open")
        # Find the most recent file
        graphics.file.info <- file.info(possible.graphics.files.open)
        latest.file.date.row <- which.max(graphics.file.info$mtime)
        # check if the device is still open and close it if it is We need to do this so
        # that the file.out call can copy the file.
        if (dev.number %in% dev.list())
            dev.off(dev.number)
        if (!is.null(proc.node.name)) {
            ddg.file.out(possible.graphics.files.open[latest.file.date.row], pname = proc.node.name)
            # Add an input edge from the current device
            dev.node.name <- paste0("dev.", dev.number)
            # If the device was opened but never written to there will be no node.
            if (.data.node.exists(dev.node.name)) {
                .ddg.data2proc(dev.node.name, NULL, proc.node.name)
            }
            .global.set("possible.graphics.files.open", NULL)
        }
        return(possible.graphics.files.open[latest.file.date.row])
    }
    # Output is going to the display, so we need to make up a name
    dev.file <- .ddg.capture.current.graphics(proc.node.name)
    if (called.from.save) {
        ddg.file.out(dev.file, pname = proc.node.name)
        # Remove the temporary file
        file.remove(dev.file)
        # Add an input edge from the current device
        dev.node.name <- paste0("dev.", dev.cur())
        # If the device was opened but never written to there will be no node.
        if (.data.node.exists(dev.node.name)) {
            .ddg.data2proc(dev.node.name, NULL, proc.node.name)
        }
    }
    return(dev.file)
}

# Captures what is on the current display to a file, creates a file node and
# connects to the ddg.
.ddg.capture.current.graphics <- function(proc.node.name, file = NULL) {
    if (is.null(file)) {
        file <- paste0("dev.off.", .global.get("ddg.dnum") + 1, ".pdf")
    }
    # Save the graphic to a file temporarily
    dev.print(device = pdf, file = file)
    .global.set("possible.graphics.files.open", file)
    return(file)
}

# .ddg.write.graphic takes as input the name of a variable as well as its value
# and attempts to write it out as a graphics file. If all else fails, it writes
# out the information as a text file and also writes out an RData Object which
# can later be read back into the system.
# name - data node name.  value - data node value.  fext - file extension.  scope
# - data node scope.
.ddg.write.graphic <- function(name, value = NULL, fext = "jpeg", scope = NULL, from.env = FALSE) {
    # Try to output graphic value.
    tryCatch({
        .snapshot.node(name, fext, NULL, dscope = scope, from.env = from.env)
    }, error = function(e) {
        tryCatch({
            .snapshot.node(name, "jpeg", NULL, dscope = scope, from.env = from.env)
        }, error = function(e) {
            .snapshot.node(name, "txt", value, dscope = scope, from.env = from.env)
        })
    })
}
