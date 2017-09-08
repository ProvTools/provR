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

# .ddg.data.node.exists searches the data node table for a matching data node and
# returns TRUE if a match is found. Otherwise it searches the initial environment
# table and, if a match is found, creates a data node and returns TRUE. Otherwise
# it returns FALSE.

# dname - data node name.  dscope - data node scope.

.ddg.data.node.exists <- function(dname, dscope = NULL) {
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    # Search data nodes table.
    ddg.data.nodes <- .ddg.get("ddg.data.nodes")
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
    ddg.data.nodes <- .ddg.get("ddg.data.nodes")
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
            .ddg.snapshot.node(dname, "txt", dvalue, dscope = dscope, from.env = from.env)
            return(NULL)
        }, error = function(e) {
            error.msg <- paste("Unable to create snapshot node for", dname, "Details:",
                e)
            .ddg.insert.error.message(error.msg)
            return(.ddg.data.node(dtype, dname, "complex", dscope, from.env = from.env))
        })

    } else if (is.matrix(dvalue) || (is.vector(dvalue) && length(dvalue) > 20)) {
        .ddg.snapshot.node(dname, "csv", dvalue, dscope = dscope, from.env = from.env)
        return(NULL)
    }
    # Convert value to a string.
    val <- if (is.list(dvalue)) {
        tryCatch({
            .ddg.convert.list.to.string(dvalue)
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
        .ddg.snapshot.node(dname, "txt", val, from.env = from.env)
        return
    } else {
        # Get scope if necessary.
        if (is.null(dscope))
            dscope <- .ddg.get.scope(dname)
        # Record in data node table
        .ddg.record.data(dtype, dname, val, val, dscope, from.env = from.env)

        if (.ddg.get("ddg.debug.lib"))
            print(paste("data.node:", dtype, dname))
    }
    invisible()
}

# ddg.data creates a data node for a single or complex data value.  If the value
# is omitted, the argument passed in for dname is evaluated in the calling
# environment to determine the value.  If the value is determined to be complex,
# the output data is written out to a csv if possible. Otherwise, the data are
# written out as a .txt file if the variable is determined to be an object.

# dname - the label for the node. This can be passed as a string, name, or
# expression.  dvalue (optional) - the value of the node.  graphic.fext
# (optional) - the file extention to be used for saving the variable if it is a
# graphical output. Otherwise ignored. Default is jpeg.


ddg.data <- function(dname, dvalue = NULL, graphic.fext = "jpeg") {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # Look up the value if one was not provided.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.data")
    # Save the value appropriately.  If the name is not a string, use the argument
    # instead of the value.
    if (!is.character(dname))
        dname <- deparse(substitute(dname))
    .ddg.save.data(dname, dvalue, "ddg.data", graphic.fext, env = env)
}

# ddg.exception creates a data node for an exception.

# dname - the label for the node. This can be passed as a string, name, or
# expression.  dvalue (optional) - the value of the node.  If the value is
# omitted, the argument passed in for dname is evaluated in the calling
# environment to determine the value.

ddg.exception <- function(dname, dvalue = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # Look up the value if one was not provided.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.exception")
    if (is.character(dname)) {
        if (exists(dname, env, inherits = TRUE)) {
            dscope = .ddg.get.scope(dname)
        } else {
            dscope = environmentName(.GlobalEnv)
        }
    } else {
        # If dname is not a string, use its name rather than its value.
        dname <- deparse(substitute(dname))
        dscope <- .ddg.get.scope(dname)
    }
    # Create input exception node.
    .ddg.data.node("Exception", dname, dvalue, dscope)
}

# ddg.url creates a data node for a URL.

# dname - the label for the node.  dvalue (optional) - the value of the node.  If
# the value is omitted, the argument passed in for dname is evaluated in the
# calling environment to determine the value.

ddg.url <- function(dname, dvalue = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # Look up the value if one was not provided.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.url")
    if (is.character(dname)) {
        dscope = environmentName(.GlobalEnv)
    } else {
        # If dname is not a string, use its name rather than its value.
        dname <- deparse(substitute(dname))
        dscope <- .ddg.get.scope(dname)
    }
    # Create input URL node.
    .ddg.data.node("URL", dname, dvalue, dscope)
}

# ddg.file creates a data node of type File by copying an existing file to the
# DDG directory.

# filename - the name of the file to copy, including path to the file if it is
# not in the working directory.  dname (optional) - the label for the node. If
# omitted, the filename, minus the directory path, is used as the label.

ddg.file <- function(filename, dname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    scope <- if (!is.null(dname))
        .ddg.get.scope(dname) else NULL
    invisible(.ddg.file.copy("File", filename, dname, scope))
}

# ddg.data.in creates a data flow edge from data node dname to procedure node
# pname.

# dname - the name of the data node.  This can be passed as a string, name, or
# expression.  pname (optional) - the name of the procedure that created this
# data value.  This can be passed as a string or as a name. It may be omitted if
# ddg.data.in is called by a function, in which case the name of the function
# will be used.

ddg.data.in <- function(dname, pname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)

    arg <- substitute(dname)
    if (!is.character(arg)) {
        argname <- deparse(arg)
        dscope <- .ddg.get.scope(argname)
        if (.ddg.data.node.exists(argname, dscope)) {
            dname <- argname
        } else {
            dscope <- .ddg.get.scope(argname, for.caller = TRUE)
            if (.ddg.data.node.exists(argname, dscope)) {
                dname <- argname
            } else {
                # This case is for file names.  The table records the file name, using the scope
                # 'undefined'.
                dscope <- "undefined"
                if (!is.character(dname) || !.ddg.data.node.exists(dname, dscope)) {
                  error.msg <- paste("No data node found for", arg)
                  .ddg.insert.error.message(error.msg)
                  return()
                }
            }
        }
    } else if (exists(arg, envir = parent.frame(), inherits = TRUE)) {
        dscope <- .ddg.get.scope(dname)
    } else if (exists(arg, envir = parent.frame(2), inherits = TRUE)) {
        dscope <- .ddg.get.scope(dname, for.caller = TRUE)
    } else {
        dscope <- environmentName(.GlobalEnv)
    }
    # Create data flow edge from data node to operation node.
    .ddg.data2proc(dname, dscope, pname)
}

# ddg.data.out creates a data or snapshot node of type Data.  It also creates a
# data flow edge from procedure node pname to the output node. Used for simple or
# complex data values.

# dname - the label for the data node being created. This can be passed as a
# string, name, or expression. Complex data are written to the file dname.
# dvalue (optional) - the value to associate with the node.  If no value is
# given, the argument passed in for dname is evaluated in the calling
# environment.  pname (optional) - the name of the procedure that created the
# data. This can be passed as a string or name. It may be omitted if ddg.data.out
# is called by a function, in which case the name of the function will be used.
# graphic.fext (optional) - the file extension that should be used when saving a
# graphics file. Ignored unless the value to be saved is determined to be a
# graphic.

ddg.data.out <- function(dname, dvalue = NULL, pname = NULL, graphic.fext = "jpeg") {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # If no value is provided, get value in calling environment.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.data.out")
    # Convert name to a string if necessary.
    if (!is.character(dname))
        dname <- deparse(substitute(dname))
    # Save the complex data in appropriate format.
    .ddg.save.data(dname, dvalue, "ddg.data.out", graphic.fext, env = env)
    .ddg.lookup.function.name(pname)
    # Create data flow edge from operation node to data node.
    .ddg.proc2data(pname, dname)
}

# ddg.exception.out creates a data node of type Exception. It also creates a data
# flow edge from the procedure node pname to this node.

# dname - the label for the exception node being created.  This can be passed as
# a string or name.  dvalue (optional) - the value to associate with the node.
# If no value is given, the argument passed in for dname is evaluated in the
# calling environment.  pname (optional) - the name of the procedure that created
# this exception. This can be passed as a string or as name. It may be ommited if
# ddg.exception.out is called by a function, in which case the name of the
# function will be used.

ddg.exception.out <- function(dname, dvalue = NULL, pname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # If no value is provided, get value in calling environment.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.exception.out")
    # Create output exception node.
    .ddg.data.node("Exception", dname, dvalue, "ddg.library")
    .ddg.lookup.function.name(pname)
    # Create data flow edge from procedure node to exception node.
    .ddg.proc2data(pname, dname)
}

# ddg.url.out creates a data node of type URL called dname with address dvalue.
# It also creates a data flow edge from procedure node pname to the URL node
# dname. Use for URL addresses.

# dname - the label for the data node being created.  dvalue (optional) - the
# full URL. If a value is not provided, the argument passed in for dname is
# evaluated in the calling environment to determine the value.  pname (optional)
# - the name of the procedure that created this URL node. This can be passed as a
# string or as a name. It may be omitted if ddg.url.out is called by a function,
# in which case the name of the function will be used.

ddg.url.out <- function(dname, dvalue = NULL, pname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # If no value is provided, get value in calling environment.
    env <- parent.frame()
    .ddg.lookup.value(dname, dvalue, env, "ddg.url.out")
    # URL labels are not necessarily variables, so make sure it is a variable before
    # trying to determine its scope.
    if (exists(dname, inherits = TRUE)) {
        dscope <- .ddg.get.scope(dname)
    } else {
        dscope <- environmentName(.GlobalEnv)
    }
    # Create output URL node where dvalue = address.
    .ddg.data.node("URL", dname, dvalue, dscope)
    .ddg.lookup.function.name(pname)
    # Create data flow edge from operation node to URL node.
    .ddg.proc2data(pname, dname, dscope)
}

# ddg.file.out creates a data node of type File called dname by copying an
# existing file to the DDG directory. A data flow edge is also created from
# procedure node pname to data node dname.  Use for output files already created
# by the main script. Returns the full path to the file that is saved.

# filename - name of the file.  The name should include the path to the file if
# it is not in the working directory.  dname (optional) - the label for the node
# being created. If omitted, the filename, minus the directory path, is used as
# the label.  pname (optional) - the name of the procedure that created this
# node. This can be passed as a string or as a name. It may be omitted if
# ddg.file.out is called by a function, in which case the name of the function is
# used.

ddg.file.out <- function(filename, dname = NULL, pname = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    if (is.null(dname)) {
        dname <- basename(filename)
        scope <- NULL
    } else {
        scope <- .ddg.get.scope(dname)
    }
    # Create output file node called filename and copy file.
    saved.file <- .ddg.file.copy("File", filename, dname, scope)
    .ddg.lookup.function.name(pname)
    # Create data flow edge from operation node to file node.
    .ddg.proc2data(pname, dname, scope)
    return(saved.file)
}

# ddg.graphic.out creates a data node of type Snapshot called dname by capturing
# the current image in the active graphics device and saving it in the DDG
# directory. The name of the file is dname plus the extension specified by the
# fext parameter. Available extensions are bmp, jpeg, png, and tiff.  A data flow
# edge is also created from procedure pname to the data node dname.

# dname - the label for the node being created.  pname (optional) - the name of
# the procedure that created this node. This can be passed as a string or as a
# name. It may be omitted if ddg.graphic.out is called by a function, in which
# case the name of the function is used.  fext (optional) - the file extention to
# be used for the captured image file. If omitted, this value defaults to jpeg.

ddg.graphic.out <- function(dname, pname = NULL, graphic.fext = "jpeg") {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return
    # Write out the graphic.
    .ddg.write.graphic(dname, "Graphical Plot. Not saved in script.", graphic.fext)
    .ddg.lookup.function.name(pname)
    # Create the data flow edge from oepration node to the file node.
    .ddg.proc2data(pname, dname)
}

# .ddg.data.objects returns a list of data objects used or created by the script.
# The list includes node number, name, value, type, scope, line number (if any)
# where the object was created, and line numbers(s) (if any) where the object was
# used. The scope is set to ENV if the object was not created by the script and
# was taken from the pre -existing environment.

.ddg.data.objects <- function() {
    # Get data node, procedure node, and edge tables.
    dnodes <- .ddg.get("ddg.data.nodes")
    # Subset data node table
    dnum <- .ddg.get("ddg.dnum")
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
    w <- .ddg.get(".ddg.warning")
    # Create a message that looks like the one R creates
    callStr <- if (is.null(w$call))
        "" else paste("In ", head(deparse(w$call)), ": ")
    warningMessage <- paste(callStr, w$message)
    # Create the warning node
    .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)
    # Clear the saved warning
    .ddg.set(".ddg.warning", NA)
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
    .ddg.inc("ddg.dnum")
    ddg.dnum <- .ddg.get("ddg.dnum")
    # Initialize dscriptpath
    if (!is.null(.ddg.get("ddg.r.script.path"))) {
        dscriptpath <- .ddg.get("ddg.r.script.path")
    }
    # If the table is full, make it bigger.
    ddg.data.nodes <- .ddg.get("ddg.data.nodes")
    if (nrow(ddg.data.nodes) < ddg.dnum) {
        size = 100
        new.rows <- data.frame(ddg.type = character(size), ddg.num = numeric(size),
            ddg.name = character(size), ddg.path = character(size), ddg.value = character(size),
            ddg.val.type = character(size), ddg.scope = character(size), ddg.from.env = logical(size),
            ddg.time = character(size), ddg.hash = character(size), ddg.rw = character(size),
            ddg.loc = character(size), ddg.current = logical(size), stringsAsFactors = FALSE)
        .ddg.add.rows("ddg.data.nodes", new.rows)
        ddg.data.nodes <- .ddg.get("ddg.data.nodes")
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
        longpath <- paste0(getwd(), substring(.ddg.get("ddg.path"), 2))
    }
    ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
    .ddg.set("ddg.data.nodes", ddg.data.nodes)
    # Output data node.
    .ddg.output.data.node(dscriptpath, dtype, dname, dvalue2, val.type, dscope, from.env,
        dhash, drw, dtime, dloc)
    if (.ddg.get("ddg.debug.lib")) {
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
