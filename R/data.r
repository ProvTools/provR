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

# .data.node.exists searches the data node table for a matching data node and
# returns TRUE if a match is found. Otherwise it searches the initial environment
# table and, if a match is found, creates a data node and returns TRUE. Otherwise
# it returns FALSE.

# dname - data node name.  dscope - data node scope.

.data.node.exists <- function(dname, dscope = NULL) {
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    # Search data nodes table.
    ddg.data.nodes <- .global.get("ddg.data.nodes")
    rows <- nrow(ddg.data.nodes)
    for (i in rows:1) {
        if (ddg.data.nodes$ddg.current[i]) {
            if (ddg.data.nodes$ddg.name[i] == dname) {
                if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] ==
                  dscope) {
                  return(TRUE)
                }
            }
        }
    }
    # Search initial environment table.
    if (dscope == "R_GlobalEnv") {
        if (exists(dname, globalenv())) {
            dvalue <- get(dname, envir = globalenv())
            if (!is.function(dvalue)) {
                .ddg.save.data(dname, dvalue, scope = dscope, from.env = TRUE)
                return(TRUE)
            }
        }

    }
    return(FALSE)
}

# .ddg.data.number retrieves the number of the nearest preceding current matching
# data node. It returns zero if no match is found.

# dname - data node name.  dscope (optional) - data node scope.

.ddg.data.number <- function(dname, dscope = NULL) {
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    ddg.data.nodes <- .global.get("ddg.data.nodes")
    rows <- nrow(ddg.data.nodes)
    for (i in rows:1) {
        if (ddg.data.nodes$ddg.current[i]) {
            if (ddg.data.nodes$ddg.name[i] == dname) {
                if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] ==
                  dscope)
                  return(ddg.data.nodes$ddg.num[i])
            }
        }
    }
    # Error message if no match found.
    error.msg <- paste("No data node found for", dname)
    .ddg.insert.error.message(error.msg)
    return(0)
}

# .ddg.replace.quotes quotes quotation characters. It also replaces return,
# newline and tab characters with spaces.
# str - input string.
.ddg.replace.quotes <- function(str) {
    if (!is.character(str))
        return(str)
    str <- paste("\"", str, "\"", sep = "")
    str <- gsub("\"", "\\\\\"", str)
    # Replace returns, new lines, and tabs with spaces.
    str <- gsub("\r", " ", str)
    str <- gsub("\n", " ", str)
    str <- gsub("\t", " ", str)
}

# .ddg.data.node creates a data node of type Data. Data nodes are used for single
# data values. The value (dvalue) is stored in the DDG.

# dtype - type of data node.  dname - name of data node.  dvalue - value of data
# node.  dscope - scope of data node.  from.env - if object is from initial
# environment

.ddg.data.node <- function(dtype, dname, dvalue, dscope, from.env = FALSE) {
    # If object or a long list, try to create snapshot node.
    if (is.object(dvalue)) {
        tryCatch({
            .snapshot.node(dname, "txt", dvalue, dscope = dscope, from.env = from.env)
            return(NULL)
        }, error = function(e) {
            error.msg <- paste("Unable to create snapshot node for", dname, "Details:",
                e)
            .ddg.insert.error.message(error.msg)
            return(.ddg.data.node(dtype, dname, "complex", dscope, from.env = from.env))
        })

    } else if (is.matrix(dvalue) || (is.vector(dvalue) && length(dvalue) > 20)) {
        .snapshot.node(dname, "csv", dvalue, dscope = dscope, from.env = from.env)
        return(NULL)
    }
    # Convert value to a string.
    val <- if (is.list(dvalue)) {
        tryCatch({
            .convert.list.to.string(dvalue)
        }, error = function(e) {
            error.msg <- paste("Unable to convert value of", dname, "to a string.")
            .ddg.insert.error.message(error.msg)
            "complex"
        })
    } else if (typeof(dvalue) == "closure")
        "#ddg.function" else if (length(dvalue) > 1 || !is.atomic(dvalue)) {
        tryCatch(paste(.ddg.replace.quotes(dvalue), collapse = ","), error = function(e) {
            "complex"
        })
    } else if (is.null(dvalue))
        "NULL" else if (length(dvalue) == 0)
        "Empty" else if (is.na(dvalue))
        "NA" else if (dvalue == "complex" || dvalue == "#ddg.function")
        dvalue else if (is.character(dvalue) && dvalue == "")
        "NotRecorded" else {
        # Replace double quotes with single quotes.
        .ddg.replace.quotes(dvalue)
    }
    if (grepl("\n", val)) {
        # Create snapshot node.
        .snapshot.node(dname, "txt", val, from.env = from.env)
        return
    } else {
        # Get scope if necessary.
        if (is.null(dscope))
            dscope <- .ddg.get.scope(dname)
        # Record in data node table
        .ddg.record.data(dtype, dname, val, val, dscope, from.env = from.env)

        if (.global.get("ddg.debug.lib"))
            print(paste("data.node:", dtype, dname))
    }
    invisible()
}

# .ddg.data.objects returns a list of data objects used or created by the script.
# The list includes node number, name, value, type, scope, line number (if any)
# where the object was created, and line numbers(s) (if any) where the object was
# used. The scope is set to ENV if the object was not created by the script and
# was taken from the pre -existing environment.

.ddg.data.objects <- function() {
    # Get data node, procedure node, and edge tables.
    dnodes <- .global.get("ddg.data.nodes")
    # Subset data node table
    dnum <- .global.get("ddg.dnum")
    dinv <- dnodes[1:dnum, c("ddg.num", "ddg.name", "ddg.value", "ddg.type", "ddg.scope")]
    # Replace scope with ENV if from initial environment
    index <- which(dnodes$ddg.from.env == TRUE)
    if (length(index) > 0) {
        dinv$ddg.scope[index] <- "ENV"
    }
    # Rename columns
    colnames(dinv) <- c("node", "name", "value", "type", "scope")
    return(dinv)
}


# Create the warning node for the saved warning and attach it to the node that
# created the warning

.ddg.record.warning <- function() {
    # Get the saved warning
    w <- .global.get(".ddg.warning")
    # Create a message that looks like the one R creates
    callStr <- ""
    if (!is.null(w$call))
      callStr <- paste("In ", head(deparse(w$call)), ": ")
    warningMessage <- paste(callStr, w$message)
    # Create the warning node
    .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)
    # Clear the saved warning
    .global.set(".ddg.warning", NA)
}

# .ddg.record.data records a data node in the data node table.

# dtype - data node type.  dname - data node name.  dvalue - data node value.
# value - the value of the data dscope - data node scope.  from.env - if object
# is from initial environment.  dhash - the MD5 hash of original file.  drw -
# whether the file was read or written.  dtime (optional) - timestamp of original
# file.  dloc (optional) - path and name of original file.

.ddg.record.data <- function(dtype, dname, dvalue, value, dscope, from.env = FALSE,
    dtime = "", dloc = "", dhash = "", drw = "", dscriptpath = "") {
    # Increment data node counter.
    .global.inc("ddg.dnum")
    ddg.dnum <- .global.get("ddg.dnum")
    # Initialize dscriptpath
    if (!is.null(.global.get("ddg.r.script.path"))) {
        dscriptpath <- .global.get("ddg.r.script.path")
    }
    # If the table is full, make it bigger.
    ddg.data.nodes <- .global.get("ddg.data.nodes")
    if (nrow(ddg.data.nodes) < ddg.dnum) {
        size = 100
        new.rows <- data.frame(ddg.type = character(size), ddg.num = numeric(size),
            ddg.name = character(size), ddg.path = character(size), ddg.value = character(size),
            ddg.val.type = character(size), ddg.scope = character(size), ddg.from.env = logical(size),
            ddg.time = character(size), ddg.hash = character(size), ddg.rw = character(size),
            ddg.loc = character(size), ddg.current = logical(size), stringsAsFactors = FALSE)
        .ddg.add.rows("ddg.data.nodes", new.rows)
        ddg.data.nodes <- .global.get("ddg.data.nodes")
    }
    if (length(dvalue) > 1 || !is.atomic(dvalue))
        dvalue2 <- "complex" else if (!is.null(dvalue))
        dvalue2 <- dvalue else dvalue2 <- ""
    # get value type
    val.type <- .ddg.get.val.type.string(value)

    ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
    ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
    ddg.data.nodes$ddg.path[ddg.dnum] <- dscriptpath
    ddg.data.nodes$ddg.name[ddg.dnum] <- dname
    ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue2
    ddg.data.nodes$ddg.val.type[ddg.dnum] <- val.type
    ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
    ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
    ddg.data.nodes$ddg.hash[ddg.dnum] <- dhash
    ddg.data.nodes$ddg.rw[ddg.dnum] <- drw
    ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
    ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc

    if (dtype == "File") {
        dhash <- md5sum(dname)
        ddg.data.nodes$ddg.hash[ddg.dnum] <- dhash
        longpath <- paste0(getwd(), substring(.global.get("ddg.path"), 2))
    }
    ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
    .global.set("ddg.data.nodes", ddg.data.nodes)

    # Prepare values
    if (from.env)
        dname <- paste(dname, " [ENV]", sep = "")
      # Output data node.
    .json.data.node(ddg.dnum, dname, dvalue2, val.type, dtype, dscope, from.env,
        dhash, dtime, dloc)
    if (.global.get("ddg.debug.lib")) {
        if (dtype != "File") {
            print(paste("Adding data node", ddg.dnum, "named", dname, "with scope",
                dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
        } else {
            print(paste("Adding data node", ddg.dnum, "named", dname, "with scope",
                dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum], " that hashes to ",
                dhash, " and performs a file ", drw))
        }
    }
}

# .ddg.is.simple returns TRUE if the value passed in is a simple data value which
# should be saved locally as opposed to stored in a separate file. The assumption
# is that the value passed in has already been declared not to be a graphic.
# value - input value.
.ddg.is.simple <- function(value) {
    # Note that is.vector returns TRUE for lists, so we need to check lists
    # separately.  Since every value in a list can have a different type, if it is a
    # list, we will assume the value is complex. We consider NULL values to be
    # simple.
    return((!.ddg.is.graphic(value) && !is.list(value) && is.vector(value) && length(value) ==
        1) || is.null(value))
}

# .ddg.is.csv returns TRUE if the value passed in should be saved as a csv file,
# i.e. if it is a vector, matrix, or data frame.  Note that is.vector returns
# TRUE for lists.
# value - input value.
.ddg.is.csv <- function(value) {
    return(!.ddg.is.simple(value) && ((is.vector(value) && !is.list(value)) || is.matrix(value) ||
        is.data.frame(value)))
}

# .ddg.save.data takes as input the name and value of a data node that needs to
# be created. It determines how the data should be output (or saved) and saves it
# in that format.
# name - name of created node.  value - value of created node.  from.env - if
# node is from initial environment fname (optional) - name of calling function.
# Used to generate helpful error messages if something goes wrong.  graphic.fext
# (optional) - file extension for graphic file.  error (optional) - if TRUE,
# raise an R error rather than a DDG error.  scope (optional) - scope of node.
# stack (optional) - stack to use in determing scope.
.ddg.save.data <- function(name, value, fname = ".ddg.save.data", graphic.fext = "jpeg",
    error = FALSE, scope = NULL, from.env = FALSE, stack = NULL, env = NULL) {
    if (is.null(scope)) {
        scope <- .ddg.get.scope(name, calls = stack, env = env)
    }
    # Determine type for value, and save accordingly.
    if (.ddg.is.graphic(value))
      .ddg.write.graphic(name, value, graphic.fext, scope = scope, from.env = from.env)
    else if (.ddg.is.simple(value))
      .ddg.data.node("Data", name, value, scope, from.env)
    else if (.ddg.is.csv(value))
      .ddg.data.node("CSV", name, value, scope, from.env)
    else if (is.list(value) || is.array(value))
      .snapshot.node(name, "txt", value, dscope = scope, from.env = from.env)
    else if (is.object(value) || is.environment(value))
      .snapshot.node(name, "txt", value, dscope = scope, from.env = from.env)
    else if (is.function(value))
      .ddg.data.node("Function", name, "#ddg.function", scope, from.env)
    else if (error)
      stop("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
    else {
      error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
      .ddg.insert.error.message(error.msg)
    }
    invisible()
}
