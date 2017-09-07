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

# Create DDG environment variable.
.ddg.env <- new.env(parent = emptyenv())

# Set the number of lines the history file keeps (and therefore can be analyzed).
# Note: this setting has no effect on some systems.
ddg.MAX_HIST_LINES <- 2^14

# .ddg.init.environ() sets up the filesystem and R environments for use.
.ddg.init.environ <- function() {
    dir.create(.ddg.get("ddg.path"), showWarnings = FALSE)
    dir.create(paste(.ddg.get("ddg.path"), "/data", sep = ""), showWarnings = FALSE)
    dir.create(paste(.ddg.get("ddg.path"), "/debug", sep = ""), showWarnings = FALSE)
    dir.create(paste(.ddg.get("ddg.path"), "/scripts", sep = ""), showWarnings = FALSE)
    if (interactive() && .ddg.get(".ddg.enable.console")) {
        .ddg.set("ddg.original.hist.size", Sys.getenv("R_HISTSIZE"))
        Sys.setenv(R_HISTSIZE = ddg.MAX_HIST_LINES)
    }
}

# .ddg.sourced.script.names returns a string containing the names of sourced
# scripts, if any. If no scripts were sourced it returns an empty string.

.ddg.sourced.script.names <- function() {
    ss <- .ddg.get(".ddg.sourced.scripts")
    # First row is main script.
    if (nrow(ss) == 1)
        snames <- "" else {
        snames <- ss[ss$snum >= 1, "sname"]
        snames <- paste0(snames, collapse = ",")
    }
    return(snames)
}

# ddg.installedpackages() returns information on packages installed at the time
# of execution and their versions.
.ddg.installedpackages <- function() {
    packages <- devtools::session_info()
    packages <- packages[[2]]
    installed <- packages[packages[, 2] == "*", ]
    installed <- installed[, c(1, 3)]
    return(installed)
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


# .ddg.is.object returns TRUE if the value is determined to be an object by our
# standards.

# value - input value.

.ddg.is.object <- function(value) {
    return(is.object(value) || is.environment(value))
}

# .ddg.is.function returns TRUE if the value is determined to be a function or we
# want to save it as a function.

# value - input value.

.ddg.is.function <- function(value) {
    return(is.function(value))
}

# .ddg.dev.change determines whether or not a new graphic device has become
# active and whether or not we should capture the previous graphic device. It
# returns the device number we should capture (0 means we shouldn't capture any
# device).

.ddg.dev.change <- function() {
    prev.device <- .ddg.get("prev.device")
    curr.device <- dev.cur()
    device.list <- dev.list()
    # We've switched devices .
    if (prev.device != curr.device) {
        # Update device.
        .ddg.set("prev.device", curr.device)
        # Previous device still accessible.
        if (prev.device %in% device.list)
            return(prev.device)
    }
    # No switching, or previous is not accessible (NULL or removed).
    return(0)
}

# .ddg.save.simple takes in a simple name-value pair and saves it to the DDG. It
# does not however create any edges. Extra long strings are saved as snapshots.

# name - data node name.  value - data node value.  scope - data node scope.

.ddg.save.simple <- function(name, value, scope = NULL, from.env = FALSE) {
    # Save extra long strings as snapshot.
    if (is.character(value) && nchar(value) > 200) {
        .ddg.snapshot.node(name, "txt", value, dscope = scope, from.env = from.env)
    } else {
        # Save the true value.
        .ddg.data.node("Data", name, value, scope, from.env = from.env)
    }
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
        .ddg.snapshot.node(name, fext, NULL, dscope = scope, from.env = from.env)
    }, error = function(e) {
        tryCatch({
            .ddg.snapshot.node(name, "jpeg", NULL, dscope = scope, from.env = from.env)
        }, error = function(e) {
            .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope = scope,
                from.env = from.env)
        })
    })
}

# .ddg.write.csv takes as input a name-value pair for a variable and attempts to
# save the data as a csv file. It does not create any edges but does add the node
# to the DDG. Edge creation should occur from wherever this function is called.

# name - data node name.  value - data node value.  scope - data node scope.

.ddg.write.csv <- function(name, value, scope = NULL, from.env = FALSE) {
    tryCatch({
        .ddg.snapshot.node(name, "csv", value, dscope = scope, from.env = from.env)
    }, error = function(e) {
        .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope = scope,
            from.env = from.env)
    })
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
        .ddg.write.graphic(name, value, graphic.fext, scope = scope, from.env = from.env) else if (.ddg.is.simple(value))
        .ddg.save.simple(name, value, scope = scope, from.env = from.env) else if (.ddg.is.csv(value))
        .ddg.write.csv(name, value, scope = scope, from.env = from.env) else if (is.list(value) || is.array(value))
        .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope = scope,
            from.env = from.env) else if (.ddg.is.object(value))
        .ddg.snapshot.node(name, "txt", value, dscope = scope, from.env = from.env) else if (.ddg.is.function(value))
        .ddg.save.simple(name, "#ddg.function", scope = scope, from.env = from.env) else if (error)
        stop("Unable to create data (snapshot) node. Non-Object value to", fname,
            ".") else {
        error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to",
            fname, ".")
        .ddg.insert.error.message(error.msg)
    }
    invisible()
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

# Returns a string representation of the type information of the given value.
.ddg.get.val.type.string <- function(value) {
    val.type <- .ddg.get.val.type(value)
    if (is.null(val.type))
        return("null")
    # list, object, environment, function, language
    if (length(val.type) == 1)
        return(paste("\"", val.type, "\"", sep = ""))
    # vector, matrix, array, data frame type information recorded in a list of 3
    # vectors (container,dimension,type)
    container <- val.type[[1]]
    dimension <- val.type[[2]]
    type <- val.type[[3]]
    # vector: a 1-dimensional array (uniform typing)
    if (identical(container, "vector"))
        return(paste("{\"container\":\"vector\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    # matrix: a 2-dimensional array (uniform typing)
    if (identical(container, "matrix")) {
        dimension <- paste(dimension, collapse = ",")
        return(paste("{\"container\":\"matrix\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    }
    # array: n-dimensional (uniform typing)
    if (identical(container, "array")) {
        dimension <- paste(dimension, collapse = ",")
        return(paste("{\"container\":\"array\", \"dimension\":[", dimension, "], \"type\":[\"",
            type, "\"]}", sep = ""))
    }
    # data frame: is a type of list
    dimension <- paste(dimension, collapse = ",")
    type <- paste(type, collapse = "\",\"")
    return(paste("{\"container\":\"data_frame\", \"dimension\":[", dimension, "], \"type\":[\"",
        type, "\"]}", sep = ""))
}


# Returns the type information of the value of the given variable.  Does not
# contain information on dimensions.

.ddg.get.val.type.from.var <- function(var) {
    val.type <- .ddg.get.val.type(get(var))
    # remove dimension information, if any
    if (length(val.type) > 1)
        val.type[2] <- NULL
    return(val.type)
}


# Returns the type information of the given value, broken into its parts and
# returned in a vecctor or a list.

.ddg.get.val.type <- function(value) {
    # vector: a 1-dimensional array (uniform typing)
    if (is.vector(value))
        return(list("vector", length(value), class(value)))
    # matrix: a 2-dimensional array (uniform typing)
    if (is.matrix(value))
        return(list("matrix", dim(value), class(value[1])))
    # array: n-dimensional (uniform typing)
    if (is.array(value))
        return(list("array", dim(value), class(value[1])))
    # data frame: is a type of list
    if (is.data.frame(value)) {
        types <- unname(sapply(value, class))
        return(unname(list("data_frame", dim(value), types)))
    }
    # a list
    if (is.list(value))
        return("list")
    # an object
    if (is.object(value))
        return("object")
    # envrionment, function, language
    if (is.environment(value))
        return("environment")
    if (is.function(value))
        return("function")
    if (is.language(value))
        return("language")
    # none of the above - null is a character, not NULL or NA
    return(NULL)
}


# .ddg.record.edge records a control flow edge or a data flow edge in the edges
# table.

# etype - type of edge node1 - name of first node node1 - name of second node

.ddg.record.edge <- function(etype, node1, node2) {
    # Increment edge counter.
    .ddg.inc("ddg.enum")
    ddg.enum <- .ddg.get("ddg.enum")
    # If the table is full, make it bigger.
    ddg.edges <- .ddg.get("ddg.edges")
    if (nrow(ddg.edges) < ddg.enum) {
        size = 100
        new.rows <- data.frame(ddg.num = numeric(size), ddg.type = character(size),
            ddg.from = character(size), ddg.to = character(size), stringsAsFactors = FALSE)
        .ddg.add.rows("ddg.edges", new.rows)
        ddg.edges <- .ddg.get("ddg.edges")
    }
    ddg.edges$ddg.num[ddg.enum] <- ddg.enum
    ddg.edges$ddg.type[ddg.enum] <- etype
    ddg.edges$ddg.from[ddg.enum] <- node1
    ddg.edges$ddg.to[ddg.enum] <- node2
    .ddg.set("ddg.edges", ddg.edges)
    # Output control flow or data flow edge.
    .ddg.output.edge(etype, node1, node2)

    if (.ddg.get("ddg.debug.lib")) {
        if (etype == "cf")
            etype.long <- "control flow" else if (etype == "df.in")
            etype.long <- "data flow in" else etype.long <- "data flow out"
        print(paste("Adding", etype.long, "edge", ddg.enum, "for", node1, "to", node2))
    }
}

# .ddg.is.nonlocal.assign returns TRUE if the object passed is an expression
# object containing a non-local assignment.

# expr - input expression.

.ddg.is.nonlocal.assign <- function(expr) {
    # <<- or ->> means that the assignment is non-local
    if (is.call(expr) && identical(expr[[1]], as.name("<<-"))) {
      return(TRUE)
    }
    return(FALSE)
}


# .ddg.create.empty.vars.set creates an empty data frame initialized to contain
# information about variable assignments.  The difference between first.writer
# and possible.first.writer is that first.writer is for simple assignments (like
# a <- 1), while possible.first.writer is for situations where the assignment
# might not have occurred, like 'if (foo) a <- 1'.

# The data frame is structured as follows - the variable name.  - the position of
# the statement that wrote the variable first.  - the position of the statement
# that wrote the variable last.  - the position of the first statement that may
# have assigned to a variable .  - the position of the last statement that may
# have assigned to a variable.

# var.table.size - desired size of the data frame. Negative values and 0 are
# coerced to 1.

.ddg.create.empty.vars.set <- function(var.table.size = 1) {
    if (var.table.size <= 0)
        var.table.size <- 1
    vars.set <- data.frame(variable = character(var.table.size), first.writer = numeric(var.table.size),
        last.writer = numeric(var.table.size), possible.first.writer = numeric(var.table.size),
        possible.last.writer = numeric(var.table.size), stringsAsFactors = FALSE)
    # Initialize first writer.
    vars.set$first.writer <- var.table.size + 1
    vars.set$possible.first.writer <- var.table.size + 1
    return(vars.set)
}

# .ddg.increase.vars.set simply doubles the size of a variable assignment data
# frame and returns the new one.

# vars.set - data frame containing variable assignments.  size (optional) -
# number of rows in data frame.

.ddg.double.vars.set <- function(vars.set, size = nrow(vars.set)) {
    # Create the right size data frame from input frame.
    new.vars.set <- rbind(vars.set, .ddg.create.empty.vars.set(size))
    # Update first/last writer.
    new.vars.set$first.writer <- ifelse(new.vars.set$first.writer == size + 1, size *
        2 + 1, new.vars.set$first.writer)
    new.vars.set$possible.first.writer <- ifelse(new.vars.set$possible.first.writer ==
        size + 1, size * 2 + 1, new.vars.set$possible.first.writer)
    return(new.vars.set)
}

# .ddg.add.to.vars.set parses a command and adds the new variable information to
# the variable assignment data frame. Note that var.num is a global variable! It
# should be intialized when vars.set is first created.

# vars.set - variable assignment data frame.  cmd.expr - command expression.  i -
# position of variable in data frame.

.ddg.add.to.vars.set <- function(vars.set, cmd, i) {
    # Find out the variable being assigned to by a simple assignment statement.
    main.var.assigned <- cmd@vars.set
    # Find all the variables that may be assigned in the statement.
    vars.assigned <- cmd@vars.possibly.set
    for (var in vars.assigned) {
        nRow <- which(vars.set$variable == var)
        # If the variable is already in the table, update its entry.
        if (length(nRow) > 0) {
            if (!is.null(main.var.assigned) && var == main.var.assigned) {
                vars.set$last.writer[nRow] <- i
            } else {
                vars.set$possible.last.writer[nRow] <- i
            }
        } else {
            # The variable was not in the table. Add a new line for this variable.
            # Find the first empty row
            empty.rows <- which(vars.set$variable == "")
            if (length(empty.rows) == 0) {
                vars.set <- .ddg.double.vars.set(vars.set, nrow(vars.set))
                empty.rows <- which(vars.set$variable == "")
            }
            var.num <- empty.rows[1]
            # Set the variable.
            vars.set$variable[var.num] <- var
            if (!is.null(main.var.assigned) && var == main.var.assigned) {
                vars.set$first.writer[var.num] <- i
                vars.set$last.writer[var.num] <- i
            } else {
                vars.set$possible.first.writer[var.num] <- i
                vars.set$possible.last.writer[var.num] <- i
            }
        }
    }
    return(vars.set)
}


## .ddg.find.var.assigments finds the possible variable assignments for a fixed
## set of parsed commands. See .ddg.create.empty.vars.set for more information on
## the structure of the returned data frame.  parsed.commands - a list of parsed
## commands.
.ddg.find.var.assignments <- function(cmds) {
    if (length(cmds) == 0)
        return(data.frame())
    # Make it big so we don't run out of space.
    var.table.size <- length(cmds)
    vars.set <- .ddg.create.empty.vars.set(var.table.size)
    # Build the table recording where variables are assigned to or may be assigned
    # to.
    for (i in 1:length(cmds)) {
        cmd.expr <- cmds[[i]]
        vars.set <- .ddg.add.to.vars.set(vars.set, cmd.expr, i)
    }
    return(vars.set)
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
        name <- if (!is.null(cmd.abbrev) && cmd.abbrev != "")
            paste0("graphic", substr(cmd.abbrev, 1, 10)) else "graphic"
        .ddg.snapshot.node(name, fext = "jpeg", data = NULL)
        # Make the previous device active again.
        dev.set(prev.device)
        # We're done, so create the edge.
        if (is.null(cmd.abbrev))
            .ddg.lastproc2data(name, all = FALSE) else .ddg.proc2data(cmd.abbrev, name)
    }
}


# .ddg.create.data.use.edges.for.console.cmd creates a data flow edge from the
# node for each variable used in cmd.expr to the procedural node labeled cmd, as
# long as the value would either be one that exists prior to starting the console
# block, or corresponds to the last setting of this variable in the console
# block.

# vars.set - variable assignment data frame.  cmd - name of procedure node.
# cmd.expr - command expression.  cmd.pos - position of command.

.ddg.create.data.use.edges.for.console.cmd <- function(vars.set, cmd, cmd.pos, for.caller) {
    # Find all the variables used in this command.
    vars.used <- cmd@vars.used
    for (var in vars.used) {
        # Make sure there is a node we could connect to.
        scope <- .ddg.get.scope(var, for.caller)
        if (.ddg.data.node.exists(var, scope)) {
            nRow <- which(vars.set$variable == var)
            # check if the node is written in the console block.
            if (length(nRow) > 0) {
                first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
                last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
                # Draw the edge if we will connect to a node that exists before the console block
                # or to the last writer of this variable within the console block.
                if (cmd.pos <= first.writer || cmd.pos >= last.writer) {
                  .ddg.data2proc(var, scope, cmd@abbrev)
                }
                # TODO - add some sort of warning to the user that the node is not being created
            } else {
                # The variable is not set at all in this console block.  Connect to a
                # pre-existing data node.
                .ddg.data2proc(var, scope, cmd@abbrev)
            }
        } else {
            # TODO - add some sort of warning that the data node was NOT found.
        }
    }
}

# .ddg.create.data.set.edges.for.cmd creates edges that correspond to a console
# command assigning to a variable.

# vars.set - variable assignment data frame.  cmd.abbrev - name of procedure
# node.  cmd.expr - command expression.  cmd.pos - position of command.  env -
# environment to use for evaluating variable.  for.finish.node (optional) - if
# TRUE, data edge is for finish node.  scope (optional) - scope of variable.
# stack (optional) - stack to use for evaluating variable.

.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd, cmd.pos, env, for.finish.node = FALSE,
    scope = NULL, stack = NULL) {
    vars.assigned <- cmd@vars.set
    for (var in vars.assigned) {
        whichRows <- which(vars.set$variable == var)
        # Only create a node edge for the last place that a variable is set within a
        # console block.
        if ((length(whichRows) > 0 && vars.set$last.writer[whichRows] == cmd.pos &&
            vars.set$possible.last.writer[whichRows] <= vars.set$last.writer[whichRows]) ||
            for.finish.node) {
            if (is.null(env)) {
                env <- .ddg.get.env(var, calls = stack)
            }
            scope <- .ddg.get.scope(var, calls = stack, env = env)
            # Special operators are defined by enclosing the name in `.  However, the R
            # parser drops those characters when we deparse, so when we parse here they are
            # missing and we get an error about unexpected SPECIAL characters.  The first
            # tryCatch, puts the ` back in and parses again.  The second tryCatch handles
            # errors associated with evaluated the variable.
            parsed <- tryCatch(parse(text = var), error = function(e) parse(text = paste("`",
                var, "`", sep = "")))
            val <- tryCatch(eval(parsed, env), error = function(e) {
                eval(parse(text = var), parent.env(env))
            })
            tryCatch(.ddg.save.data(var, val, fname = ".ddg.create.data.set.edges.for.cmd",
                error = TRUE, scope = scope, stack = stack, env = env), error = function(e) {
                .ddg.data.node("Data", var, "complex", scope)
            })
            .ddg.proc2data(cmd@abbrev, var, scope)
        }
    }
}


# .ddg.create.data.node.for.possible.writes creates a data node for each variable
# that might have been set in something other than a simple assignment.  An edge
# is created from the last node in the console block.

# vars.set - variable assignment data frame.  last.command - last command in
# console block.

.ddg.create.data.node.for.possible.writes <- function(vars.set, last.command, env = NULL) {
    environment <- if (is.environment(env))
        env else .GlobalEnv
    for (i in 1:nrow(vars.set)) {
        if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
            value <- tryCatch(eval(parse(text = vars.set$variable[i]), environment),
                error = function(e) {
                  NULL
                })
            # Only create the node and edge if we were successful in looking up the value.
            if (!is.null(value)) {
                envName <- environmentName(environment)
                if (envName == "")
                  envName <- .ddg.get.scope(vars.set$variable[i])
                .ddg.data.node("Data", vars.set$variable[i], value, envName)
                .ddg.proc2data(last.command@abbrev, vars.set$variable[i], envName)
            }
        }
    }
}


# Given a parse tree, this function returns a list containing the expressions
# that correspond to the filename argument of the calls to functions that read or
# write the files.  If there are none, it returns NULL.  main.object - the parsed
# expression to search through func.df - the data frame describing the functions
# with file arguments
.ddg.find.files <- function(main.object, func.df, env = NULL) {
    environment <- if (is.environment(env))
        env else .GlobalEnv
    # Recursive helper function.
    find.files.rec <- function(obj) {
        # Base cases.
        if (!is.recursive(obj)) {
            return(NULL)
        }
        if (length(obj) == 0) {
            return(NULL)
        }
        ## It might be useful to record somehow that this function reads a file, but we
        ## wouldn't actually do the reading until the function is called, not here where
        ## it is being declared.
        if (.ddg.is.functiondecl(obj))
            return(NULL)
        if (is.call(obj)) {
            # Call has no arguments, so it can't be reading a function.  Recurse on the first
            # part, in case it is more than just a symbol.
            if (length(obj) == 1)
                return(find.files.rec(obj[[1]])) else if (is.symbol(obj[[1]])) {
                # Call with arguments Is this is file reading function?
                read.func.pos <- match(as.character(obj[[1]]), func.df$function.names)
                if (!is.na(read.func.pos)) {
                  # Find the file argument.
                  arg.name <- func.df$param.names[read.func.pos]
                  # Find a matching parameter passed by name
                  file.name.arg.matches <- unlist(lapply(names(obj), function(arg) {
                    return(pmatch(arg, arg.name))
                  }))
                  match.pos <- match(1, file.name.arg.matches)
                  # If no argument qualified by the file parameter name, use the argument in the
                  # expected position
                  if (is.na(match.pos)) {
                    file.name <- tryCatch(eval(obj[[func.df$param.pos[read.func.pos] +
                      1]], environment), error = function(e) NULL)
                  } else {
                    file.name <- tryCatch(eval(obj[[match.pos]], environment), error = function(e) NULL)
                  }
                  # Recurse over the arguments to the function.  We can't just skip over the 2nd
                  # element since the filename parameter is not necessarily there if it was passed
                  # by name.
                  funcs <- find.files.rec(obj[2:length(obj)])
                  # Add this file name to the list of files being read.  Make sure the file name
                  # could be evaluated and that it results in a name, not a connection.
                  if (!is.null(file.name) && is.character(file.name)) {
                    unique(c(file.name, funcs))
                  }
                } else {
                  # Not a file reading function.  Recurse over the arguments.
                  find.files.rec(obj[2:length(obj)])
                }
            } else {
                # Function call, but the first list element is not simply a function name.
                # Recurse over all the list elements.
                unique(append(find.files.rec(obj[[1]]), find.files.rec(obj[2:length(obj)])))
            }
        } else if (length(obj) == 1) {
            # A recursive structure that is not a call.  Not sure if there are any, but just
            # in case...
            unique(find.files.rec(obj[[1]]))
        } else {
            unique(append(find.files.rec(obj[[1]]), find.files.rec(obj[2:length(obj)])))
        }
    }
    return(find.files.rec(main.object@parsed))
}

# Creates file nodes and data in edges for any files that are read in this cmd
# cmd - text command cmd.expr - parsed command
.ddg.create.file.read.nodes.and.edges <- function(cmd, env) {
    # Find all the files potentially read in this command.  This may include files
    # that are not actually read if the read are within an if-statement, for example.
    files.read <- .ddg.find.files(cmd, .ddg.get(".ddg.file.read.functions.df"), env)
    for (file in files.read) {
        # Only create the node and edge if there actually is a file then if the file
        # exists, it is possible that it was not read here
        if (file.exists(file)) {
            # Create the file node and edge
            ddg.file(file)
            ddg.data.in(basename(file), pname = cmd@abbrev)
        } else if (grepl("^http", file) || grepl("^ftp", file)) {
            scope <- environmentName(.GlobalEnv)
            .ddg.data.node("URL", file, file, scope)
            .ddg.data2proc(file, scope, cmd@abbrev)
        }
    }
}

# Creates file nodes and data in edges for any files that are written in this cmd
# cmd - text command cmd.expr - parsed command
.ddg.create.file.write.nodes.and.edges <- function(cmd, env) {
    # Find all the files potentially written in this command.  This may include files
    # that are not actually written if the write calls are within an if-statement,
    # for example.
    files.written <- .ddg.find.files(cmd, .ddg.get(".ddg.file.write.functions.df"), env)
    for (file in files.written) {
        # check that the file exists.  If it does, we will assume that it was created by
        # the write call that we just found.
        if (file.exists(file)) {
            # Create the file node and edge
            ddg.file.out(file, pname = cmd@abbrev)
        }
    }
}

# Given a parse tree, this function returns a list containing the expressions
# that correspond to the filename argument of the calls to functions that create
# graphics devices.  If there are none, it returns NULL.
.ddg.set.graphics.files <- function(main.object, env) {
    # Allows dev.print to work when we want to save the plot.
    tryCatch(dev.control("enable"), error = function(e) return())
    # Add the newly-opened graphics device to the list of open devices
    .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))
    # Find all the graphics files that have potentially been opened.  Remember these
    # file names until we find the dev.off call and then determine which was written.
    new.possible.graphics.files.open <- .ddg.find.files(main.object, .ddg.get(".ddg.graphics.functions.df"),
        env)
    if (!is.null(new.possible.graphics.files.open)) {
        if (!is.null(.ddg.get("possible.graphics.files.open"))) {
            possible.graphics.files.open <- .ddg.get("possible.graphics.files.open")
            .ddg.set("possible.graphics.files.open", c(new.possible.graphics.files.open,
                possible.graphics.files.open))
        } else {
            .ddg.set("possible.graphics.files.open", new.possible.graphics.files.open)
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
    if (dev.cur() %in% .ddg.get("ddg.open.devices")) {
        .ddg.data2proc(dev.node.name, NULL, cmd@abbrev)
    } else {
        # Add the newly-opened graphics device to the list of open devices
        .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))
    }
    # Add an output node with the same name
    .ddg.data.node("Data", dev.node.name, "graph", NULL)
    .ddg.proc2data(cmd@abbrev, dev.node.name)

}

.ddg.capture.graphics <- function(cmd, called.from.save = FALSE) {
    proc.node.name <- if (is.null(cmd))
        NULL else if (is.character(cmd))
        cmd else cmd@abbrev
    dev.number <- .ddg.get(".ddg.dev.number")
    .ddg.set("ddg.open.devices", setdiff(.ddg.get("ddg.open.devices"), dev.number))
    if (!is.null(.ddg.get("possible.graphics.files.open")) && !is.null(proc.node.name)) {
        possible.graphics.files.open <- .ddg.get("possible.graphics.files.open")
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
            if (.ddg.data.node.exists(dev.node.name)) {
                .ddg.data2proc(dev.node.name, NULL, proc.node.name)
            }
            .ddg.set("possible.graphics.files.open", NULL)
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
        if (.ddg.data.node.exists(dev.node.name)) {
            .ddg.data2proc(dev.node.name, NULL, proc.node.name)
        }
    }
    return(dev.file)
}


# Captures what is on the current display to a file, creates a file node and
# connects to the ddg.
.ddg.capture.current.graphics <- function(proc.node.name, file = NULL) {
    if (is.null(file)) {
        file <- paste0("dev.off.", .ddg.get("ddg.dnum") + 1, ".pdf")
    }
    # Save the graphic to a file temporarily
    dev.print(device = pdf, file = file)
    .ddg.set("possible.graphics.files.open", file)
    return(file)
}

# .ddg.loadhistory takes in the name of a history file, opens it, scans it for
# the last occurrence of the string specified by timestamp, and returns the lines
# from that point forward.

# hist.file - name of history file.  timestamp - timestamp string.

.ddg.loadhistory <- function(hist.file, timestamp) {
    # Read from specified file.
    history <- readLines(hist.file)
    history.lines <- length(history)
    # Find the timestamp specified in the history.  There may be more than one with
    # the same timestamp, so pick the last of these.
    history.timestamp.line <- tail(which(history == timestamp), 1)
    if (length(history.timestamp.line) == 0) {
        error.msg <- paste("Part of history is missing. DDG may be incomplete! Tried reading from",
            hist.file, "but could not find timestamp:", timestamp)

        .ddg.insert.error.message(error.msg)
        history.timestamp.line <- 0
    }
    # Need to check if the timestamp line is the last line in the file explicitly.
    # If we don't do that and take the vector, we will get the last line in the file
    # since R will create a descending sequence for the vector.
    if (history.timestamp.line == history.lines)
        return(vector())
    # NEED the paren around sum.
    return(history[(history.timestamp.line + 1):history.lines])
}

# .ddg.savehistory saves the current and unsaved R command history to the
# specified file if that file matches the DDG history file.  Note: the commented
# section of code appends the information to this file.

# savehistory is not supported on all R platforms.  If it is not supported, this
# will fail silently.

# hist.file - name of history file.

.ddg.savehistory <- function(hist.file) {
    # USED TO STORE ENTIRE HISTORY IN SEP. FILE.  Write history out to temporary file
    if (.ddg.is.set(".ddg.history.file") && is.character(.ddg.get(".ddg.history.file")) &&
        .ddg.get(".ddg.history.file") == hist.file) {
        savehistory(hist.file)
    }
}


# .ddg.link.function.returns determines if the command calls a function for which
# ddg.return has created a node for the return value.  If so, a data flow edge is
# created from the return value data node to the finish node for the command.
# Note that if the assignment is an expression, like 'd <- f(a) + f(b)', there
# may be multiple return value nodes to link to.

# command - input command.

.ddg.link.function.returns <- function(command) {
    # Find the functions that have completed but whose returns have not been used
    # yet.
    returns <- .ddg.get(".ddg.ret.values")
    if (!is.na(command@pos@startLine)) {
        unused.returns <- returns[!returns$ret.used & returns$ret.node.id >
            0 & !is.na(returns$line) & returns$line == command@pos@startLine, ]
    } else {
        unused.returns <- returns[!returns$ret.used & returns$ret.node.id >
            0, ]
    }
    if (nrow(unused.returns) == 0)
        return()
    # See which of these are called from the command we are processing now.
    unused.calls <- unused.returns$ddg.call
    command.text <- gsub(" ", "", command@text)
    uses <- sapply(unused.calls, function(call) {
        grepl(call, command.text, fixed = TRUE)
    })
    # The following line is here to get around R CMD check, which otherwise reports:
    # no visible binding for global variable.  Note that ret.node.id is not a
    # variable in the subset call, but the name of a column in the data frame being
    # subsetted.
    ret.node.id <- NULL
    # Extracts for the return value nodes.
    new.uses <- subset(unused.returns, uses, ret.node.id)
    # Create an edge from each of these to the last procedure node.
    lapply(new.uses$ret.node.id, function(data.num) {
        proc.num <- .ddg.get("ddg.pnum")
        # Record in edges table
        etype <- "df.in"
        node1 <- paste("d", data.num, sep = "")
        node2 <- paste("p", proc.num, sep = "")
        .ddg.record.edge(etype, node1, node2)

        if (.ddg.get("ddg.debug.lib")) {
            print(paste(".ddg.link.function.returns:", command@abbrev))
            print(paste("DF ", node1, " ", node2, sep = ""))
        }
        # Set the return value as being used.
        returns$ret.used[returns$ret.node.id == data.num] <- TRUE
        .ddg.set(".ddg.ret.values", returns)
    })
}

# .ddg.close.last.command.node closes the last created collapsible node stored in
# .ddg.last.cmd properly.

# env - the environment in which the close is occurring called (optional) - used
# in debugging to identify the function which called .ddg.close.previous.command.
# initial (optional) - if TRUE, try to close previous command node.

.ddg.close.last.command.node <- function(env, called = ".ddg.parse.commands", initial = FALSE) {
    # Get both the last command and new commands.
    .ddg.last.cmd <- if (.ddg.is.set(".ddg.last.cmd")) {
        .ddg.get(".ddg.last.cmd")
    } else {
        NULL
    }
    .ddg.possible.last.cmd <- if (.ddg.is.set(".ddg.possible.last.cmd")) {
        .ddg.get(".ddg.possible.last.cmd")
    } else {
        NULL
    }
    # Only create a finish node if a new command exists (i.e., we've parsed some
    # lines of code).
    if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
        cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd, env = env,
            called = paste(called, "-> .ddg.close.last.command.node"))
        # Add link from a function return node if there is one.
        .ddg.link.function.returns(.ddg.last.cmd)
        # No previous command.
        .ddg.set(".ddg.last.cmd", NULL)
    }
}

# .ddg.open.new.command.node opens a new collapsible command node depending on
# the information stored in .ddg.last.cmd.

# env - the environment in which the command occurs called (optional) - name of
# calling function.

.ddg.open.new.command.node <- function(env, called = ".ddg.parse.commands") {
    new.command <- .ddg.get(".ddg.possible.last.cmd")
    if (!is.null(new.command)) {
        .ddg.add.abstract.node("Start", new.command, env, called = paste(called,
            "-> .ddg.open.new.command.node"))
        # Now the new command becomes the last command, and new command is null.
        .ddg.set(".ddg.last.cmd", new.command)
        .ddg.set(".ddg.possible.last.cmd", NULL)
    }
}

# Create the DDGStatement list for a list of parsed expressions.

# exprs - a list of parsed expressions script.name - the name of the script the
# expressions come from parseData - information provided by the parser that we
# use to find line numbers enclosing.pos - if exprs are statements within a
# function definition, enclosing.pos is the source position information of the
# entire function declaration

# Returns a list of DDGStatement objects

.ddg.create.DDGStatements <- function(exprs, script.name, script.num, parseData = NULL,
    enclosing.pos = NULL) {
    # The parse data gives us line number information
    if (is.null(parseData)) {
        parseData <- getParseData(exprs, includeText = TRUE)
        if (is.null(parseData)) {
            # In this case there is no line number information available
            cmds <- vector("list", (length(exprs)))
            for (i in 1:length(exprs)) {
                expr <- as.expression(exprs[i])
                cmds[[i]] <- .ddg.construct.DDGStatement(expr, NA, script.name, script.num, parseData)
            }
            return(cmds)
        }
        non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
        if (nrow(non.comment.parse.data) == 0) {
            return(list())
        }
        # Start at the first non-comment expression in parseData
        next.parseData <- 1
    } else {
        non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
        # Start at the first entry in parse data that begins after the enclosing function
        # begins, ends before the enclosing function ends, and matches the text of the
        # first expression.
        next.parseData <- which(non.comment.parse.data$line1 >= enclosing.pos@startLine &
            non.comment.parse.data$line2 <= enclosing.pos@endLine & non.comment.parse.data$text ==
            paste(deparse(exprs[[1]]), collapse = "\n"))[1]
    }
    # Create the DDGStatements
    cmds <- vector("list", (length(exprs)))
    next.cmd <- 1
    for (i in 1:length(exprs)) {
        expr <- as.expression(exprs[i][[1]])
        next.expr.pos <- new(Class = "DDGStatementPos", non.comment.parse.data[next.parseData,
            ])
        cmds[[next.cmd]] <- .ddg.construct.DDGStatement(expr, next.expr.pos, script.name,
            script.num, parseData)
        next.cmd <- next.cmd + 1
        # If there are more expressions, determine where to look next in the parseData
        if (i < length(exprs)) {
            last.ending.line <- non.comment.parse.data[next.parseData, ]$line2
            last.parent <- non.comment.parse.data[next.parseData, "parent"]
            last.id <- non.comment.parse.data[next.parseData, "id"]

            # Find the first entry in parseData that has the same parent as the previous
            # expression and starts after the previous expression.
            next.parseData <- which(non.comment.parse.data$parent == last.parent &
                non.comment.parse.data$line1 >= last.ending.line & non.comment.parse.data$id >
                last.id)[1]
        }
    }
    return(cmds)
}

# .ddg.save.annotated.script saves a copy of the annotated script to the debug
# directory.

.ddg.save.annotated.script <- function(cmds, script.name) {
    for (i in 1:length(cmds)) {
        expr <- cmds[[i]]@annotated
        for (j in 1:length(expr)) {
            line <- deparse(expr[[j]])
            if (i == 1 && j == 1)
                script <- line else script <- append(script, line)
        }
    }
    fileout <- file(paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/annotated-", script.name, sep = ""))
    write(script, fileout)
    close(fileout)
}

# .ddg.console.node creates a console node.

.ddg.console.node <- function() {
    # Don't do anything if sourcing, because history isn't necessary in this case.
    if ((.ddg.is.set("from.source") && .ddg.get("from.source")))
        return(NULL)
    ddg.history.file = .ddg.get(".ddg.history.file")
    ddg.history.timestamp = .ddg.get(".ddg.history.timestamp")
    # Only continue if these values exists.
    if (!(is.null(ddg.history.file) || is.null(ddg.history.timestamp))) {
        # Grab any new commands that might still be in history.
        tryCatch({
            # Saving history is not supported on all platforms.
            .ddg.savehistory(ddg.history.file)
            # Load from extended history since last time we wrote out a console node.
            new.lines <- .ddg.loadhistory(ddg.history.file, ddg.history.timestamp)
            # Parse the lines into individual commands.
            parsed.commands <- .ddg.parse.lines(new.lines)
            # New commands since last timestamp.
            if (!is.null(parsed.commands) && length(parsed.commands) > 0) {
                .ddg.parse.commands(parsed.commands, environ = .GlobalEnv, run.commands = FALSE)
            }
        }, error = function(e) {
        })

    }
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

# .ddg.convert.list.to.string converts a list of values to a string by calling
# as.character on each element in the list.

# dvalue - a list of values.

.ddg.convert.list.to.string <- function(dvalue) {
    values <- .ddg.replace.quotes(lapply(dvalue, .ddg.as.character))
    positions <- 1:length(values)
    paste("[[", positions, "]]", values, collapse = "\n")
}

# .ddg.as.character wraps an exception handler around as.character The exception
# handler captures the print output for the value and returns that instead.
.ddg.as.character <- function(value) {
    tryCatch(as.character(value), error = function(e) {
        capture.output(print(value))
    })
}

# .ddg.supported.graphic - the sole purpose of this function is to verify that
# the input file extension is a supported graphic type. Currently supported
# graphics types inlude: jpg, jpeg, bmp, png, tiff.

# ext - file extension.

.ddg.supported.graphic <- function(ext) {
    return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# .ddg.graphic.snapshot provides factoring of snapshot code.

# fext - file extension.  dpfile - path and name of file.

.ddg.graphic.snapshot <- function(fext, dpfile) {
    # pdfs require a separate procedure.
    if (fext == "pdf")
        dev.copy2pdf(file = dpfile) else {
        # At the moment, all other graphic types can be done by constructing a similar
        # function.
        # If jpg, we need to change it to jpeg for the function call.
        fext = ifelse(fext == "jpg", "jpeg", fext)
        # First, we create a string, then convert it to an actual R expression and use
        # that as the function.
        strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep = "")
        parseFun <- function() {
            eval(parse(text = strFun))
        }
        dev.copy(parseFun)
        # Turn it off (this switches back to prev device).
        dev.off()
    }
}

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots are used for
# complex data values not written to file by the main script. The contents of
# data are written to the file dname.fext in the DDG directory. Snapshots are
# also used to capture output plots and other graphics generated by the R script.

# The user can control the size of the snapshot files by setting the
# max.snapshot.size parameter when calling ddg.init or ddg.run.  If the user
# passes in 0, no snapshots are saved.  Instead a data node will be created.  If
# the user passes in -1, there is no limit on the snapshot size.  If the user
# passes a value > 0, if the R value is larger than this size, only the head of
# the data will be saved.

# dname - name of data node.  fext - file extension.  data - value of data node.
# save.object (optional) - if TRUE, also save as an R object.  dscope (optional)
# - scope of data node.

.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE, dscope = NULL,
    from.env = FALSE) {
    orig.data <- data
    # Determine if we should save the entire data
    max.snapshot.size <- .ddg.get("ddg.max.snapshot.size")
    if (max.snapshot.size == 0) {
        return(.ddg.data.node("Data", dname, "", dscope, from.env = from.env))
    }
    # Snapshot name
    snapname <- dname
    # object.size returns bytes, but max.snapshot.size is in kilobytes
    if (max.snapshot.size == -1 || object.size(data) < max.snapshot.size * 1024) {
        full.snapshot <- TRUE

    } else if (is.vector(data) || is.list(data) || is.data.frame(data) || is.matrix(data) ||
        is.array(data)) {
        # Decide how much data to save
        element.size <- object.size(head(data, 1))
        num.elements.to.save <- ceiling(max.snapshot.size * 1024/element.size)
        if (num.elements.to.save < length(data)) {
            data <- head(data, num.elements.to.save)
            snapname <- paste(dname, "-PARTIAL", sep = "")
            full.snapshot <- FALSE
        } else {
            full.snapshot <- TRUE
        }
    } else {
        full.snapshot <- FALSE
        snapname <- paste(dname, "-PARTIAL", sep = "")
    }
    # Snapshot type
    dtype <- "Snapshot"
    # If the object is an environment, update the data to be the environment's name
    # followed by a list of the variables bound in the environment.
    if (is.environment(data)) {
        envHeader <- paste0("<environemnt: ", environmentName(data), ">")
        data <- c(envHeader, ls(data), recursive = TRUE)
    } else if ("XMLInternalDocument" %in% class(data)) {
        fext <- "xml"
    } else if (is.vector(data) || is.data.frame(data) || is.matrix(data) || is.array(data) ||
        is.list(data)) {
    } else if (!is.character(data)) {
        tryCatch(data <- as.character(data), error = function(e) {
            # Not sure if str or summary will give us the most useful information.
            data <- summary(data)
        })
    }
    # Default file extensions.
    dfile <- if (fext == "" || is.null(fext))
        paste(.ddg.get("ddg.dnum") + 1, "-", snapname, sep = "") else paste(.ddg.get("ddg.dnum") + 1, "-", snapname, ".", fext, sep = "")
    # Get path plus file name.
    dpfile <- paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/", dfile, sep = "")
    if (.ddg.get("ddg.debug.lib"))
        print(paste("Saving snapshot in ", dpfile))
    # Write to file .
    if (fext == "csv")
        write.csv(data, dpfile, row.names = FALSE) else if (fext == "xml")
        saveXML(data, dpfile) else if (.ddg.supported.graphic(fext)) {
        # Capture graphic.  Write out RData (this is old code, not sure if we need it).
        .ddg.graphic.snapshot(fext, dpfile)
    } else if (fext == "RData")
        file.rename(paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/", dname, sep = ""), dpfile) else if (fext == "txt" || fext == "") {
        # Write out text file for txt or empty fext.
        file.create(dpfile, showWarnings = FALSE)
        if (is.list(data) && length(data) > 0) {
            list.as.string <- .ddg.convert.list.to.string(data)
            write(list.as.string, dpfile)
        } else {
            tryCatch(write(as.character(data), dpfile), error = function(e) {
                capture.output(data, file = dpfile)
            })
        }
    } else {
        # Write out data node object if the file format is unsupported.
        error.msg <- paste("File extension", fext, "not recognized")
        .ddg.insert.error.message(error.msg)
        return(NULL)
    }
    # check to see if we want to save the object.
    if (save.object && full.snapshot)
        save(data, file = paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/", .ddg.get("ddg.dnum") + 1, "-", snapname,
            ".RObject", sep = ""), ascii = TRUE)
    dtime <- .ddg.format.time(Sys.time())
    # Get scope if necessary.
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    # Record in data node table
    .ddg.record.data(dtype, dname, paste("data", dfile, sep = "/"), orig.data,
        dscope, from.env = from.env, dtime)
    if (.ddg.get("ddg.debug.lib"))
        print(paste("snapshot.node: ", dname))
    return(dpfile)
}

# .ddg.file.node creates a node of type File. File nodes are used for files
# written to the DDG directory by capturing output from the script or by copying
# a file that is written by the script.  Returns the path where the file
# referenced by the node is stored.

# dtype - type of data node.  fname - path and name of original file.  dname -
# name of data node.  dscope (optional) - scope of data node.

.ddg.file.node <- function(dtype, fname, dname, dscope = NULL) {
    # Get original file location.
    file.name <- basename(fname)
    file.loc <- normalizePath(fname, winslash = "/", mustWork = FALSE)
    loc.value <- if (dtype == "File")
        paste(" Location=\"", file.loc, "\"", sep = "") else ""
    # Add number to file name.
    dfile <- paste(.ddg.get("ddg.dnum") + 1, "-", file.name, sep = "")
    # Calculate the path to the file relative to the ddg directory.  This is the
    # value stored in the node.
    dpfile <- paste("data", dfile, sep = "/")
    dtime <- .ddg.format.time(Sys.time())
    # Set the node label.
    if (is.null(dname))
        dname <- file.name
    # Get scope if necessary.
    if (is.null(dscope))
        dscope <- .ddg.get.scope(dname)
    # Not from environment.
    from.env <- FALSE
    # Record in data node table
    .ddg.record.data(dtype, dname, dpfile, dpfile, dscope, from.env = from.env, dtime,
        file.loc)
    # Get path plus file name to where the file will be copied
    dpath <- paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/", dfile, sep = "")
    return(dpath)
}

# .ddg.file.copy creates a data node of type File. File nodes are used for files
# written by the main script. A copy of the file is written to the DDG directory.

# dtype - type of data node.  fname - path and name of original file.  dname -
# name of data node.  dscope - scope of data node.

.ddg.file.copy <- function(dtype, fname, dname, dscope) {
    # Calculate location of original file.
    file.loc <- normalizePath(fname, winslash = "/", mustWork = FALSE)
    # Copy file.
    if (file.exists(file.loc)) {
        # Create file node in DDG.
        dpfile.out <- .ddg.file.node(dtype, fname, dname, dscope)
        if (.ddg.get("ddg.save.to.disk")) {
            file.copy(file.loc, dpfile.out, overwrite = TRUE)
        }
    } else {
        error.msg <- paste("File to copy does not exist:", fname)
        .ddg.insert.error.message(error.msg)
        return(NULL)
    }
    if (.ddg.get("ddg.debug.lib"))
        print(paste("file.copy: ", dtype, " ", file.loc))
    return(dpfile.out)
}

# .ddg.insert.error.message issues a warning and inserts an exception node after
# the last procedure step. The name of the node is 'error.msg' and the value is
# the error message passed to this function.

# msg - error message.  msg.type - error or warning scope - scope for evaluating
# any data doWarn - if true, this function displays a warning

.ddg.insert.error.message <- function(msg, msg.type = "error.msg", scope = "ddg.library",
    doWarn = TRUE) {
    if (doWarn) {
        warning(msg)
    }
    .ddg.data.node("Exception", msg.type, msg, scope)
    .ddg.lastproc2data(msg.type, dscope = scope)
}

# .ddg.delete.temp deletes any temporary files created during the processing of a
# script. These include the temporary history file.

.ddg.delete.temp <- function() {
    # Delete the temporary history file if we made it.
    if (.ddg.is.set("ddg.history.file"))
        unlink(.ddg.get("ddg.history.file"))
    # Clear the environment.
    .ddg.env <- new.env(parent = emptyenv())
}

# .ddg.create.output.nodes creates output nodes for ddg.function and
# ddg.procedure. Outs values must be passed as strings, not names, unless the
# value is a file name.

# fname - the name of the function calling .ddg.create.output.nodes.  pname - the
# name of the procedure node.  outs.graphic - the name of a snapshot node to be
# used as a file name.  A graphical snapshot is simply a captured image of the
# graphic device active at the time of the call to ddg.function or ddg.procedure.
# outs.data - a list of names of data nodes.  outs.exception - a list of names of
# exception nodes.  outs.url - a list of names of url nodes.  outs.file - a list
# of names of file nodes. Supported file extensions include: .csv, .jpg, .jpeg,
# .pdf, and .txt.  graphic.fext - the file extension to be used when saving the
# captured graphic. Supported extensions are .jpg, .jpeg, .pdf.

.ddg.create.output.nodes <- function(fname, pname, outs.graphic, outs.data, outs.exception,
    outs.url, outs.file, graphic.fext, env) {
    # Capture graphics device.
    if (is.character(outs.graphic)) {
        name <- outs.graphic
        gfext <- as.character(graphic.fext)
        .ddg.write.graphic(name, "Graphical Plot. Not saved in script.", fext = gfext)  # value is ignored
        .ddg.proc2data(pname, name)
    }

    # Create output nodes and edges if outs list provided.
    # Exception node.
    if (!is.null(outs.exception)) {
        stack <- sys.calls()
        lapply(outs.exception, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)

            # Exception node.
            scope <- .ddg.get.scope(param, calls = stack)
            .ddg.data.node("Exception", name, value, scope)
            .ddg.proc2data(pname, name, scope)
        })
    }

    # URL node.
    if (!is.null(outs.url)) {
        stack <- sys.calls()
        lapply(outs.url, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)
            # URL node.
            scope <- .ddg.get.scope(param, calls = stack)
            .ddg.data.node("URL", name, value, scope)
            .ddg.proc2data(pname, name, scope)
        })
    }

    # Generalized data node (includes simple data values as well as snapshots)
    if (!is.null(outs.data)) {
        stack <- sys.calls()
        lapply(outs.data, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)

            tryCatch({
                if (!is.character(name))
                  name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls = stack)
                .ddg.save.data(name, value, fname, error = TRUE, scope = scope)
                .ddg.proc2data(pname, name, scope)
            }, error = function(e) {
                .ddg.insert.error.message(e)
            })
        })
    }
    # File node.
    if (!is.null(outs.file)) {
        stack <- sys.calls()
        lapply(outs.file, function(param) {
            # Get value in calling environment.
            name <- param
            value <- NULL
            .ddg.lookup.value(name, value, env, fname, warn = FALSE)
            scope <- .ddg.get.scope(param, calls = stack)

            if (value == "") {
                # Filename passed as value.
                .ddg.file.copy("File", name, name, scope)
                .ddg.proc2data(pname, name, scope)
            } else {
                # Filename passed as name.
                .ddg.file.copy("File", value, name, scope)
                .ddg.proc2data(pname, name, scope)
            }
        })
    }
}

# .ddg.create.function.nodes creates the procedure node, input binding nodes, and
# output nodes for the function.

# pname - name of procedure node.  full.call - full function call.  outs.data,
# etc (optional) - output nodes.  auto.created - TRUE if the function node is
# created automatically when a return is found env (optional) - the environment
# local to the function

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic = NULL,
    outs.data = NULL, outs.exception = NULL, outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg",
    auto.created = FALSE, env = NULL) {
    # Create the start node
    if (typeof(call[[1]]) == "closure") {
        .ddg.add.abstract.node("Start", node.name = pname, env = env)
    } else {
        .ddg.add.abstract.node("Start", node.name = paste(deparse(call), collapse = ""),
            env = env)
    }
    # Tokens will contain the function name and the argument expressions.
    # Get parameters and create edges.
    if (length(full.call) > 1) {
        # args contains the names of the variable that was passed into the function.
        args <- full.call[2:length(full.call)]
        # param,names contains the names of the parameters (this is what the variable is
        # known as inside the function).
        param.names <- names(full.call)
        param.names <- param.names[2:length(param.names)]
        stack <- sys.calls()
        # scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
        bindings <- list()
        for (i in 1:length(args)) bindings[[i]] <- list(args[[i]], param.names[[i]])
        missing.params <- character()

        lapply(bindings, function(binding) {
            # Here, arg is the arguments passed IN.
            arg <- binding[[1]]
            # formal is the paramenter name of the function (what is the variable known as
            # inside?).
            formal <- binding[[2]][[1]]
            if (is.null(formal) || formal == "")
                formal <- "..."
            # Find all the variables used in this parameter.  If the argument is a string
            # constant, don't bother looking for variables.  Also add quotes around it in the
            # node name.
            if (is.character(arg)) {
                vars.used <- character()
                binding.node.name <- paste(formal, " <- \"", arg, "\"", sep = "")
            } else {
                vars.used <- .ddg.find.var.uses(arg)
                binding.node.name <- paste(formal, " <- ", paste(deparse(arg), collapse = " "))
            }

            .ddg.proc.node("Binding", binding.node.name, env = env)
            .ddg.proc2proc()
            for (var in vars.used) {
                param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls = stack)
                if (.ddg.data.node.exists(var, param.scope)) {
                  .ddg.data2proc(as.character(var), param.scope, binding.node.name)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", var))
                }
            }
            if (formal != "...") {
                formal.scope <- .ddg.get.scope(formal, calls = stack)
                formal.env <- .ddg.get.env(formal, calls = stack)
                # If we can evaluate the argument without an error, we record the value. If an
                # error occurs, we do not record the value as it's possible that the function
                # never actually uses it.
                tryCatch({
                  .ddg.save.data(formal, eval(parse(text = formal), formal.env),
                    fname = ".ddg.save.data", scope = formal.scope, stack = stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)
                }, error = function(e) {
                })

            }
        })
    }
    .ddg.proc.node("Operation", pname, pname, auto.created = auto.created, env = env)
    # Link to the definition of the function if the function is defined in this
    # script.
    if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
        .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
    }
    if (length(full.call) > 1) {
        lapply(bindings, function(binding) {
            formal <- binding[[2]][[1]]
            # Formal will be NULL if declared as ...  Don't create the data node in that
            # case.
            if (!is.null(formal) && formal != "") {
                formal.scope <- .ddg.get.scope(formal, calls = stack)
                if (.ddg.data.node.exists(formal, formal.scope)) {
                  .ddg.data2proc(formal, formal.scope, pname)
                }
            }
        })
    }
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # create output nodes
    .ddg.create.output.nodes(fname = "ddg.function", pname, outs.graphic, outs.data,
        outs.exception, outs.url, outs.file, graphic.fext, parent.frame(2))

}

# .ddg.get.frame.number gets the frame number of the closest non-library calling
# function.

# calls - system calls.  for.caller (optional) - if TRUE, go up one level before
# searching.

.ddg.get.frame.number <- function(calls, for.caller = FALSE) {
    if (is.null(calls))
        calls <- sys.calls()
    script.func.found <- FALSE
    nframe <- length(calls)
    for (i in nframe:1) {
        call <- sys.call(i)[[1]]
        # Guess that if we have a closure it is a user-defined function and not a ddg
        # function Is this a good assumption ????
        if (typeof(call) == "closure") {
            if (for.caller && !script.func.found) {
                script.func.found <- TRUE
            } else {
                return(i)
            }
        } else {
            call.func <- as.character(call)
            # Ignore calls to ddg functions or to the functions that get called from the
            # outermost tryCatch to ddg code.
            if (substr(call.func, 1, 4) != ".ddg" && substr(call.func, 1, 3) != "ddg" &&
                substr(call.func, 1, 10) != "doTryCatch" && substr(call.func, 1,
                11) != "tryCatchOne" && substr(call.func, 1, 12) != "tryCatchList" &&
                substr(call.func, 1, 8) != "tryCatch") {
                if (for.caller && !script.func.found) {
                  script.func.found <- TRUE
                } else {
                  return(i)
                }
            }
        }
    }
    return(0)
}


# .ddg.where looks up the environment for the variable specified by name.
# Adapted from Hadley Wickham, Advanced R programming.

# name - name of variable.  env (optional) - environment in which to look for
# variable.  warning (optional) - set to TRUE if a warning should be thrown when
# a variable is not found.

.ddg.where <- function(name, env = parent.frame(), warning = TRUE) {
    stopifnot(is.character(name), length(name) == 1)

    if (identical(env, emptyenv())) {
        if (warning)
            warning("Can't find ", name)

        return("undefined")
    }
    if (exists(name, env, inherits = FALSE)) {
        env
    } else {
        .ddg.where(name, parent.env(env), warning)
    }
}


# .ddg.get.env gets the environment in which name is declared.

# name - variable name.  for.caller (optional) - if TRUE, go up one level before
# searching.  calls (optional) - system calls.

.ddg.get.env <- function(name, for.caller = FALSE, calls = NULL) {
    if (is.null(calls))
        calls <- sys.calls()
    fnum <- .ddg.get.frame.number(calls, for.caller)
    stopifnot(!is.null(fnum))
    # This statement was broken into two statements so that we can add print
    # statements to .ddg.where or step through it with a debugger without breaking
    # it.  If we don't do that the print output gets captured by capture.output and
    # does not display to the user and also causes the subsequent grepl call in this
    # function to fail.
    tryCatch(if (!exists(name, sys.frame(fnum), inherits = TRUE))
        return(NULL), error = function(e) {
    })
    env <- .ddg.where(name, sys.frame(fnum))
    return(env)
}

# .ddg.get.scope gets the id of the closest non-library environment.

# name - name of variable.  for.caller (optional) - if TRUE, go up one level
# before searching.  calls (optional) - system calls.  env (optional) - the
# environment to get the scope for

.ddg.get.scope <- function(name, for.caller = FALSE, calls = NULL, env = NULL) {
    # Get the environment for the variable call.
    if (is.null(env)) {
        env <- .ddg.get.env(name, for.caller, calls)
    }
    # If no environment found, name does not exist, so scope is undefined.
    if (is.null(env))
        return("undefined")
    scope <- sub("^<environment: (.*)>$", "\\1", capture.output(env)[1])
    if (grepl("undefined", scope))
        scope <- "undefined"
    return(scope)
}

# .ddg.is.local returns TRUE if the specified name is local in the specified
# scope.

# name of variable.  scope of variable.

.ddg.is.local <- function(name, scope) {
    return(exists(name, scope, inherits = FALSE))
}

# Creates a start node for the current command if one has not been created
# already.
.ddg.create.start.for.cur.cmd <- function(call, caller.env) {
    if (.ddg.is.set(".ddg.cur.cmd")) {
        .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
        .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
        stack.length <- length(.ddg.cur.cmd.stack)
        if (stack.length >= 1) {
            last.created <- .ddg.cur.cmd.stack[stack.length]
            # Only create a start node for the current command if we have not already created
            # one and the command is more than just the call to this function
            if (last.created[[1]] == "FALSE") {
                if (.ddg.cur.cmd@text != paste(deparse(call), collapse = "")) {
                  cmd.abbrev <- .ddg.add.abstract.node("Start", .ddg.cur.cmd, caller.env)
                  .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
                  st.type <- .ddg.get.statement.type(.ddg.cur.cmd@parsed[[1]])
                  loop.statement <- (st.type == "for" || st.type == "while" || st.type ==
                    "repeat")
                  control.statement <- loop.statement || st.type == "if"
                  .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(),
                    .ddg.cur.cmd, 0, for.caller = !control.statement)
                  # Add Details Omitted node before annotated loops if needed.
                  if (loop.statement && ddg.first.loop() > 1) {
                    ddg.details.omitted()
                  }
                  # Mark the start node as created on the stack.  Mark it even if we did not create
                  # the abstract node above, because we will create it below.
                  .ddg.set(".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length -
                    1], TRUE))
                } else {
                  .ddg.set(".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length -
                    1], "MATCHES_CALL"))
                }
            }
        }
    }
}

# .ddg.get.last.cmd returns the last command on the stack.

.ddg.get.last.cmd <- function() {
    .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)
    cmd <- .ddg.cur.cmd.stack[stack.length - 1][[1]]
}

# .ddg.remove.last.cmd.start.created removes the last command and start.created
# from the stack.

.ddg.remove.last.cmd.start.created <- function() {
    .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)

    if (stack.length == 2) {
        .ddg.set(".ddg.cur.cmd.stack", vector())
    } else {
        .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length - 2)])
    }
}

# .ddg.break.statement creates a procedure node for a break statement in a for,
# repeat, or while statement. It also adds a finish node for the if statement (if
# any) where the break occurs, adds a finish node for the for, repeat, or while
# loop where the break occurs, and adds a finish node for the for, repeat, or
# while statement.

.ddg.break.statement <- function() {
    # Create procedure node for break statement.
    .ddg.proc.node("Operation", "break", "break")
    .ddg.proc2proc()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
    # Create finish nodes if break occurs in if statement.
    if (loop.type == "if") {
        # Create finish node for if loop.
        ddg.finish("if")
        # Create finish node for if statement.
        .ddg.add.abstract.node("Finish", cmd, parent.frame())
        # Remove last command & start.created from stack.
        .ddg.remove.last.cmd.start.created()
        # Get last command from stack.
        cmd <- .ddg.get.last.cmd()
        # Get loop type.
        loop.type <- as.character(cmd@parsed[[1]][[1]])
    }
    # Create finish node for for, repeat, or while loop.
    loop.name <- paste(loop.type, "loop")
    ddg.finish(loop.name)
    # Create finish node for for, while, or repeat statement.
    .ddg.add.abstract.node("Finish", cmd, parent.frame())
    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
}

# .ddg.next.statement creates a procedure node for a next statement in a for,
# repeat, or while statement. It also adds a finish node for the if statement (if
# any) where the next occurs and adds a finish node for the for, while, or repeat
# loop where the next occurs.

.ddg.next.statement <- function() {
    # Create procedure node for next statement.
    .ddg.proc.node("Operation", "next", "next")
    .ddg.proc2proc()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
    # Create finish nodes if break occurs in if statement.
    if (loop.type == "if") {
        # Create finish node for if loop.
        ddg.finish("if")
        # Create finish node for if statement.
        .ddg.add.abstract.node("Finish", cmd, parent.frame())

        # Remove last command & start.created from stack.
        .ddg.remove.last.cmd.start.created()
        # Get last command from stack.
        cmd <- .ddg.get.last.cmd()
        # Get loop type.
        loop.type <- as.character(cmd@parsed[[1]][[1]])
    }
    # Create finish node for for, repeat, or while loop.
    loop.name <- paste(loop.type, "loop")
    ddg.finish(loop.name)
}

# .ddg.markdown takes a Rmd file and extracts the R code and text through the
# purl function in the knitr library. It then annotates the R script to insert
# start and finish nodes based on the chunks the user already created. If eval =
# false, then the chunk will not be added to the DDG. If the user has a name for
# the chunk, then that name will be used, else a chunk name 'ddg.chunk_1' and
# higher numbers will be generated.  Important: If in a code chunk, there is an
# empty line followed by '# ----' or '#'', then an extra finish node will be
# inserted, causing an error.  r.script.path is the path of the original Rmd file
# output.path is the path of the generated R script

.ddg.markdown <- function(r.script.path = NULL, output.path = NULL) {
    # generates R script file from markdown file
    knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)
    # moves file to ddg directory
    file.rename(from = paste(getwd(), "/", basename(tools::file_path_sans_ext(r.script.path)),
        ".R", sep = ""), to = output.path)
    script <- readLines(output.path)
    skip <- FALSE
    name <- "ddg.chunk"
    annotated <- character(0)
    index <- 1
    # This for loop goes through the script line by line and searches for patterns to
    # insert the start and finish nodes
    for (i in 1:length(script)) {
        # eval = false means we skip this code chunk, therefore skip = TRUE
        if (regexpr("eval+(\\s*)+=+(\\s*)+FALSE", script[i]) != -1) {
            skip <- TRUE
            annotated <- append(annotated, script[i])
        } else if (regexpr("## ----", script[i]) != -1) {
            # if no options in the line, then generate default name.
            if (regexpr("## -----", script[i]) == -1) {
                if (regexpr("=", script[i]) == -1) {
                  end <- regexpr("-----", script[i])
                  name <- substring(script[i], 8, last = end - 1)
                } else if (regexpr(",", script[i]) != -1) {
                  comma <- regexpr(",", script[i])
                  name <- substring(script[i], 8, last = comma - 1)
                } else {
                  name <- paste("ddg.chunk_", index, sep = "")
                  index <- index + 1
                }
            } else {
                name <- paste("ddg.chunk_", index, sep = "")
                index <- index + 1
            }
            name <- stringr::str_trim(name, side = "both")
            annotated <- append(annotated, paste("ddg.start(\"", name, "\")", sep = ""))
        } else if (nchar(script[i]) == 0 && (regexpr("#'", script[i + 1]) != -1 ||
            i == length(script) || regexpr("## ----", script[i + 1]) != -1)) {
            if (skip) {
                annotated <- append(annotated, script[i])
                skip <- FALSE
            } else {
                annotated <- append(annotated, paste("ddg.finish(\"", name, "\")",
                  sep = ""))
            }
        } else {
            annotated <- append(annotated, script[i])
        }
    }
    writeLines(annotated, output.path)
    r.script.path
}

# .ddg.save.debug.files saves debug files to the debug directory.

.ddg.save.debug.files <- function() {
    # Save initial environment table to file.
    fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/initial-environment.csv", sep = "")
    ddg.initial.env <- .ddg.get("ddg.initial.env")
    write.csv(ddg.initial.env, fileout, row.names = FALSE)
    # Save procedure nodes table to file.
    fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/procedure-nodes.csv", sep = "")
    ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
    ddg.proc.nodes <- ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ]
    write.csv(ddg.proc.nodes, fileout, row.names = FALSE)
    # Save data nodes table to file.
    fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/data-nodes.csv", sep = "")
    ddg.data.nodes <- .ddg.get("ddg.data.nodes")
    ddg.data.nodes2 <- ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ]
    write.csv(ddg.data.nodes2, fileout, row.names = FALSE)
    # Save edges table to file.
    fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/edges.csv", sep = "")
    ddg.edges <- .ddg.get("ddg.edges")
    ddg.edges2 <- ddg.edges[ddg.edges$ddg.num > 0, ]
    write.csv(ddg.edges2, fileout, row.names = FALSE)
    # Save function return table to file.
    fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/function-returns.csv", sep = "")
    ddg.returns <- .ddg.get(".ddg.ret.values")
    ddg.returns2 <- ddg.returns[ddg.returns$ret.node.id > 0, ]
    write.csv(ddg.returns2, fileout, row.names = FALSE)
    # Save if script is sourced.
    if (.ddg.get(".ddg.is.sourced")) {
        # Save sourced script table to file.
        fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/sourced-scripts.csv", sep = "")
        ddg.sourced.scripts <- .ddg.get(".ddg.sourced.scripts")
        ddg.sourced.scripts2 <- ddg.sourced.scripts[ddg.sourced.scripts$snum >= 0,
            ]
        write.csv(ddg.sourced.scripts2, fileout, row.names = FALSE)
        # Save data object table to file.
        fileout <- paste(paste(.ddg.get("ddg.path"), "/debug", sep = ""), "/data-objects.csv", sep = "")
        ddg.data.objects <- .ddg.data.objects()
        write.csv(ddg.data.objects, fileout, row.names = FALSE)
    }
}

#--------------------USER FUNCTIONS-----------------------#

# ddg.function creates a procedure node of type Operation for procedures
# implemented as functions in the original R script.  The function name and input
# parameters are obtained automatically from the calling environment. The outs
# parameters may be used optionally to create output data nodes. These MUST be
# passed as a list of strings, not names, unless the value is a file name.  Users
# can right-click on the procedure node in DDG Explorer to see the code for the
# function in the original script. For more details on outs parameters, see
# .ddg.create.output.nodes.

# outs (optional) - a list of names of data nodes to be created as outputs to
# this procedure node. These MUST be passed as a list of strings, not names,
# unless the value is a file name.  graphic.fext (optional) - the file extension
# for a graphics file.

ddg.function <- function(outs.graphic = NULL, outs.data = NULL, outs.exception = NULL,
    outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg") {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.inc(".ddg.func.depth")
    pname <- NULL
    .ddg.lookup.function.name(pname)
    if (interactive() && .ddg.get(".ddg.enable.console"))
        .ddg.console.node()
    # Look up input parameters from calling environment.
    call <- sys.call(-1)
    # Try to find the full call so that we can bind the parameters by name in the
    # DDG.  In the case that the function being executed has been passed as a
    # parameter to another function and is being called from the context (for
    # example, with lapply and other higher-order functions), the match.call will
    # fail.  In that case, we will use the call as it appears in side the
    # higher-order function.
    full.call <- tryCatch(match.call(sys.function(-1), call = call), error = function(e) call)
    # Create start node for the calling statement if one is not already created.
    .ddg.create.start.for.cur.cmd(call, sys.frame(-1))
    .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, outs.exception,
        outs.url, outs.file, graphic.fext, env = sys.frame(.ddg.get.frame.number(sys.calls())))
    invisible()
}

# ddg.procedure creates a procedure node of type Operation for procedures not
# implemented as functions in the original R script.  For more details on outs
# parameters, see .ddg.create.output.nodes.

# pname - the label for the node. Can be passed in as a string or as a name.  ins
# (optional) - a list of names of data nodes to be linked as inputs to this
# procedure node. These MUST be passed as as a list of strings, not names, unless
# the value is a file name.  outs (optional) - a list of names of data nodes to
# be created as outputs to this procedure node. These MUST be passed as a list of
# strings, not names, unless the value is a file name.  graphic.fext (optional) -
# file extension for graphics file.  env (optional) - the environment in which
# the procedure occurs.  It defaults to the global environment.

ddg.procedure <- function(pname, ins = NULL, outs.graphic = NULL, outs.data = NULL,
    outs.exception = NULL, outs.url = NULL, outs.file = NULL, graphic.fext = "jpeg") {

    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    .ddg.lookup.function.name(pname)
    .ddg.proc.node("Operation", pname, pname)
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    # Create the input edges if ins list provided.
    if (!is.null(ins)) {
        stack <- sys.calls()
        function.scope <- parent.frame()
        lapply(ins, function(param) {
            # First see if param is defined in the function where ddg.procedure is called. If
            # that does not find it, look in the scope of the function that calls the
            # function that calls ddg.procedure.  The difference probably depends on whether
            # ddg.procedure is used to annotate a chunk of code where param is really in the
            # local scope but used in the annotated chunk (the first case) or to annotate an
            # entire function and param is an actual funciton parameter (the second case).
            scope <- .ddg.get.scope(param, calls = stack)
            if (.ddg.is.local(param, function.scope)) {
                if (.ddg.data.node.exists(param, scope)) {
                  .ddg.data2proc(param, scope, pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else {
                  error.msg <- paste("No data node found for local", param)
                  .ddg.insert.error.message(error.msg)
                }
            } else if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
                .ddg.data2proc(param, scope, pname)
                if (.ddg.get("ddg.debug.lib"))
                  print(paste("param:", param))
            } else {
                scope <- .ddg.get.scope(param, for.caller = TRUE, calls = stack)
                if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
                  .ddg.data2proc(param, scope, pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else if (.ddg.data.node.exists(param, "undefined")) {
                  # This could be the case if the parameter is the name of a file rather than a
                  # variable in the program.
                  .ddg.data2proc(param, "undefined", pname)
                  if (.ddg.get("ddg.debug.lib"))
                    print(paste("param:", param))
                } else {
                  error.msg <- paste("No data node found for", param)
                  .ddg.insert.error.message(error.msg)
                }
            }
        })
    }
    # create output nodes
    .ddg.create.output.nodes(fname = "ddg.procedure", pname, outs.graphic, outs.data,
        outs.exception, outs.url, outs.file, graphic.fext, parent.frame())
    invisible()
}

# .ddg.find.ddg.ret.value.caller.frame.number returns the frame number of the
# first caller to ddg.ret.value.  If ddg.ret.value is called recursively,
# this will give us the position of the earliest one called.

.ddg.find.ddg.ret.value.caller.frame.number <- function() {
    # Get the stack
    calls <- sys.calls()
    # Find the calls to ddg.ret.value
    ddg.funcs <- unlist(lapply(calls, function(call) return(grepl("^ddg|.ddg", deparse(call)[[1]]))))
    calls.to.ddg.ret.value <- unlist(lapply(calls, function(call) return(.ddg.is.call.to(call,
        as.name("ddg.ret.value")))))
    non.ddg.calls.to.ddg.ret.value <- !(ddg.funcs[1:length(ddg.funcs) - 1]) &
        calls.to.ddg.ret.value[2:length(calls.to.ddg.ret.value)]
    which.frame <- Position(function(call) return(call), non.ddg.calls.to.ddg.ret.value,
        right = TRUE)
    # Return the frame number of the caller to ddg.ret.value
    return(which.frame)
}

# ddg.ret.value creates a data node for a function's return value. If the
# function is called from a console command and console mode is enabled, a data
# flow edge will be created linking this node to the console command that uses
# the value. ddg.ret.value returns the same value as the function (expr) and
# can be used in place of the function's normal return statement(s) if it is the
# last statement in the function.  Otherwise, it should be a parameter to return,
# as in return(ddg.ret.value(expr)). If expr is an assignment, nodes and edges
# are created for the assignment.

# expr - the value returned by the function.

ddg.ret.value <- function(expr = NULL, cmd.func = NULL) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(expr)
    dev.file <- NULL
    parsed.stmt <- NULL
    # Capture graphics if dev.off is about to be called.
    if (!is.null(cmd.func)) {
        parsed.stmt <- cmd.func()
        if (parsed.stmt@has.dev.off) {
            if (.ddg.is.call.to(parsed.stmt@parsed[[1]], "dev.off") || !.ddg.get("ddg.loop.annotate")) {
                dev.file <- .ddg.capture.graphics(NULL)
                dev.node.name <- paste0("dev.", dev.cur())
            }
        }
    }
    # If expr is an assignment, create nodes and edges for the assignment.
    orig.expr <- substitute(expr)
    frame.num <- .ddg.get.frame.number(sys.calls())
    env <- sys.frame(frame.num)
    orig.return <- paste("return(", deparse(orig.expr), ")", sep = "")
    pname <- NULL
    .ddg.lookup.function.name(pname)
    # If this is a recursive call to ddg.ret.value, find the caller of the first
    # ddg.ret.value
    if (grepl("(^ddg|.ddg)", pname)) {
        caller.frame <- .ddg.find.ddg.ret.value.caller.frame.number()
        pname <- as.character(sys.call(caller.frame)[[1]])
    } else {
        caller.frame <- -1
    }
    # Prints the call & arguments.  expr forces evaluation of the function early.  I
    # think that causes some examples to work with debugging on but not off.
    # checking.  (6/26/2015 - Barb).  Yes, ReturnTest.R fails on the recursive f5
    # function
    ddg.ret.values <- .ddg.get(".ddg.ret.values")
    ddg.num.returns <- .ddg.get(".ddg.num.returns")
    if (nrow(ddg.ret.values) == ddg.num.returns) {
        size = 100
        new.rows <- data.frame(ddg.call = character(size), line = integer(size),
            ret.used = logical(size), ret.node.id = integer(size), stringsAsFactors = FALSE)
        .ddg.add.rows(".ddg.ret.values", new.rows)
        ddg.ret.values <- .ddg.get(".ddg.ret.values")
    }
    # If this is not a recursive call to ddg.ret.value and ddg.function was not
    # called, create the function nodes that it would have created.
    call <- sys.call(caller.frame)
    if (!.ddg.proc.node.exists(pname)) {
        full.call <- match.call(sys.function(caller.frame), call = call)
        .ddg.create.function.nodes(pname, call, full.call, auto.created = TRUE, env = sys.frame(.ddg.get.frame.number(sys.calls())))
    } else {
        .ddg.dec(".ddg.func.depth")
    }
    if (is.null(cmd.func)) {
        ret.stmt <- .ddg.construct.DDGStatement(parse(text = orig.return), pos = NA,
            script.num = NA)
    } else {
        ret.stmt <- cmd.func()
        parsed.statement <- ret.stmt@parsed
    }
    # Create a data node for the return value. We want the scope of the function that
    # called the function that called ddg.ret.
    call.text <- gsub(" ", "", deparse(call, nlines = 1))
    ret.node.name <- paste(call.text, "return")
    ret.node.name <- gsub("\"", "\\\\\"", ret.node.name)

    ret.node.scope <- environmentName(if (sys.nframe() == 2)
        .GlobalEnv else parent.env(sys.frame(caller.frame)))
    .ddg.save.data(ret.node.name, expr, fname = "ddg.return", scope = ret.node.scope)
    caller.env = sys.frame(caller.frame)
    # check if there is a return call within this call to ddg.ret.
    if (.ddg.has.call.to(parsed.stmt, "return")) {
        .ddg.proc.node("Operation", ret.stmt@abbrev, ret.stmt@abbrev, console = TRUE,
            env = caller.env, cmd = ret.stmt)
        # Create control flow edge from preceding procedure node.
        .ddg.proc2proc()
        # Create an edge from the return statement to its return value.
        .ddg.proc2data(ret.stmt@abbrev, ret.node.name, ret.node.scope, ret.value = TRUE)
        if (!is.null(dev.file)) {
            ddg.file.out(dev.file, pname = ret.stmt@abbrev)
            # Remove the temporary file
            file.remove(dev.file)
            # Add an input edge from the current device
            .ddg.data2proc(dev.node.name, NULL, ret.stmt@abbrev)
        }
    } else {
        .ddg.lastproc2data(ret.node.name, dscope = ret.node.scope)
    }
    # Update the table.
    ddg.num.returns <- ddg.num.returns + 1
    ddg.ret.values$ddg.call[ddg.num.returns] <- call.text
    ddg.ret.values$ret.used[ddg.num.returns] <- FALSE
    ddg.ret.values$ret.node.id[ddg.num.returns] <- .ddg.get("ddg.dnum")
    ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    ddg.ret.values$line[ddg.num.returns] <- if (length(ddg.cur.cmd.stack) == 0)
        NA else ddg.cur.cmd.stack[length(ddg.cur.cmd.stack) - 1][[1]]@pos@startLine
    .ddg.set(".ddg.ret.values", ddg.ret.values)
    .ddg.set(".ddg.num.returns", ddg.num.returns)
    # If it does not have return, then its parameter was a call to ddg.eval and this
    # stuff has been done already.
    if (.ddg.has.call.to(parsed.stmt, "return")) {
        # Create edges from variables used in the return statement
        vars.used <- ret.stmt@vars.used
        for (var in vars.used) {
            # Make sure there is a node we could connect to.
            scope <- .ddg.get.scope(var)
            if (.ddg.data.node.exists(var, scope)) {
                .ddg.data2proc(var, scope, ret.stmt@abbrev)
            }
        }
        for (var in ret.stmt@vars.set) {
            if (var != "") {
                # Create output data node.
                dvalue <- eval(as.symbol(var), envir = env)
                # check for non-local assignment
                if (.ddg.is.nonlocal.assign(ret.stmt@parsed[[1]])) {
                  env <- .ddg.where(var, env = parent.env(parent.frame()), warning = FALSE)
                  if (identical(env, "undefined"))
                    env <- globalenv()
                }
                dscope <- .ddg.get.scope(var, env = env)
                .ddg.save.data(var, dvalue, scope = dscope)
                # Create an edge from procedure node to data node.
                .ddg.proc2data(ret.stmt@abbrev, var, dscope = dscope, ret.value = FALSE)
            }
        }
        # Create nodes and edges dealing with reading and writing files
        .ddg.create.file.read.nodes.and.edges(ret.stmt, env)
        .ddg.create.file.write.nodes.and.edges(ret.stmt, env)
        if (ret.stmt@createsGraphics) {
            .ddg.set.graphics.files(ret.stmt, env)
        }
    }
    # Create the finish node for the function
    if (typeof(call[[1]]) == "closure") {
        .ddg.add.abstract.node("Finish", node.name = pname, env = caller.env)
    } else {
        .ddg.add.abstract.node("Finish", node.name = paste(deparse(call), collapse = ""),
            env = caller.env)
    }
    return(expr)
}

# Returns true if we should run the annotated version of a function and false if
# we should run the unannotated version.

ddg.should.run.annotated <- function(func.name) {
    # check if we are in a loop and loop annotations are off
    if (!.ddg.get("ddg.loop.annotate") && .ddg.get("ddg.inside.loop") > 0)
        return(FALSE)
    # Make sure this specific function has not been disabled
    if (!is.null(.ddg.get("ddg.annotate.off")) & func.name %in% .ddg.get("ddg.annotate.off"))
        return(FALSE)
    # Not annotating functions in general check if this specific function should be
    # annotated
    if (!is.null(.ddg.get("ddg.annotate.on")) & func.name %in% .ddg.get("ddg.annotate.on"))
        return(TRUE)
    # If we do not know anything specific about this function, follow the general
    # rule
    return(.ddg.get("ddg.annotate.inside"))
}

# ddg.eval evaluates a statement and creates data flow edges from variable and
# function return nodes that are used in the statement. If the statement is an
# assignment statement, it also creates a data node for the variable assigned and
# a corresponding data flow edge. If ddg.eval is called from inside a function,
# cmd.func is a function that returns the corresponding DDGStatement object.  If
# ddg.eval is called from inside a control block, cmd.func is an integer that
# points to the corresponding DDGStatement object stored in the list
# .ddg.statements.

# statement - the statement to evaluate.

ddg.eval <- function(statement, cmd.func = NULL) {
    # Statement at top level.
    if (is.null(cmd.func)) {
        parsed.statement <- parse(text = statement)
        cmd <- NULL
    } else if (is.numeric(cmd.func)) {
        # Statement inside control block.
        num <- cmd.func
        statements <- .ddg.get("ddg.statements")
        cmd <- statements[[num]]
        parsed.statement <- cmd@parsed
        # Statement inside function.
    } else {
        cmd <- cmd.func()
        parsed.statement <- cmd@parsed
    }
    if (.ddg.get("ddg.debug.lib"))
        print(paste("ddg.eval: statement =", statement))
    frame.num <- .ddg.get.frame.number(sys.calls())
    env <- sys.frame(frame.num)
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))) {
        return(eval(parsed.statement, env))
    }
    if (interactive() && .ddg.get(".ddg.enable.console") && !(.ddg.is.set("from.source") && .ddg.get("from.source"))) {
        .ddg.console.node()
    }

    # If break statement, create procedure node and close open start nodes.
    if (!is.null(cmd) && cmd@text == "break") {
        .ddg.break.statement()
    }

    # If next statement, create procedure node and close open start nodes.
    if (!is.null(cmd) && cmd@text == "next") {
        .ddg.next.statement()
    }
    ret.value <- .ddg.parse.commands(parsed.statement, environ = env, run.commands = TRUE,
        node.name = statement, called.from.ddg.eval = TRUE, cmds = list(cmd))

    if (.ddg.get(".ddg.func.depth")) {
        if (!is.null(cmd)) {
            .ddg.link.function.returns(cmd)
        }
    }
    return(ret.value)
}

# ddg.init intializes a new DDG.

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

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE,
    annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10,
    save.to.disk = TRUE) {
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

    if (save.to.disk) {
        .ddg.set("ddg.save.to.disk", TRUE)
        # Overwrite default is
        if (!overwrite) {
            no.overwrite.folder <- paste(ddg.path, "_timestamps", sep = "")
            if (!dir.exists(no.overwrite.folder)) {
                dir.create(no.overwrite.folder)
            }
            ddg.path <- paste(no.overwrite.folder, "/", basename(tools::file_path_sans_ext(r.script.path)),
                "_ddg_", .ddg.format.time(Sys.time()), sep = "")
        }
        .ddg.set("ddg.path", ddg.path)
        # Remove files from DDG directory
        .ddg.flush.ddg()
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
    if (interactive() && .ddg.get(".ddg.enable.console") && save.to.disk) {
        ddg.history.file <- paste(paste(.ddg.get("ddg.path"), "/data", sep = ""), "/.ddghistory", sep = "")
        .ddg.set(".ddg.history.file", ddg.history.file)
        # Empty file if it already exists, do the same with tmp file.
        file.create(ddg.history.file, showWarnings = FALSE)
        # One timestamp keeps track of last .ddg.save (the default).
        .ddg.write.timestamp.to.history()
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
    .ddg.set(".ddg.proc.start.time", .ddg.elapsed.time())
    # Store time when script begins execution.
    .ddg.set("ddg.start.time", .ddg.format.time(Sys.time()))
    invisible()
}

# .ddg.save inserts attribute information and the number of procedure steps at the
# top of the DDG. It writes the DDG and the procedure nodes, data nodes, and
# function return tables to the DDG directory.

# r.script.path (optional) - Path to the R script.  save.debug (optional) - If
# TRUE, save debug files to debug directory.  Used in console mode.  quit
# (optional) - If TRUE, remove all DDG files from memory.  Unlike ddg.run, this
# is set to false as default since it will generally be called internally and by
# tests, as opposed to by the user.

.ddg.save <- function(r.script.path = NULL, save.debug = FALSE, quit = FALSE) {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    if (interactive() && .ddg.get(".ddg.enable.console")) {
        # Get the final commands
        .ddg.console.node()
    }
    # If there is a display device open, grab what is on the display
    if (length(dev.list()) >= 1) {
        tryCatch(.ddg.capture.graphics(basename(.ddg.get("ddg.r.script.path")), called.from.save = TRUE),
            error = function(e) print(e))
    }
    # Save ddg.json to file.
    .ddg.json.write()
    if (interactive())
        print(paste("Saving ddg.json in ", .ddg.get("ddg.path"), sep = ""))
    # Save sourced scripts (if any). First row is main script.
    ddg.sourced.scripts <- .ddg.get(".ddg.sourced.scripts")
    if (!is.null(ddg.sourced.scripts)) {
        if (nrow(ddg.sourced.scripts) > 1) {
            for (i in 1:nrow(ddg.sourced.scripts)) {
                sname <- ddg.sourced.scripts[i, "sname"]
                if (.ddg.get("ddg.save.to.disk")) {
                  file.copy(sname, paste(paste(.ddg.get("ddg.path"), "/scripts", sep = ""), basename(sname), sep = "/"))
                }
            }
        }
    }
    # Save debug files to debug directory.
    if (save.debug | .ddg.get("ddg.save.debug")) {
        .ddg.save.debug.files()
    }
    # Clear DDGStatements from ddg environment.
    .ddg.set("ddg.statement.num", 0)
    .ddg.set("ddg.statements", list())
    # Clear loop information from ddg environment.
    .ddg.set("ddg.loop.num", 0)
    .ddg.set("ddg.loops", list())
    # By convention, this is the final call to ddg.save.
    if (quit) {
        # Restore history settings.
        if (.ddg.is.set("ddg.original.hist.size"))
            Sys.setenv(R_HISTSIZE = .ddg.get("ddg.original.hist.size"))
        # Delete temporary files.
        .ddg.delete.temp()
        # Capture current graphics device.
        .ddg.auto.graphic.node(dev.to.capture = dev.cur)
        # Shut down the DDG.
        .ddg.clear()
    }
    invisible()
}

# ddg.source reads in an R script and executes it in the provided enviroment.
# ddg.source essentially mimics the behaviour of the R source command, having
# similar input parameters and results, but with additional parameters
# ignore.ddg.calls and ignore.init.

# file - the name of the R script file to source.  ddgdir (optional) - the
# directory where the DDG will be saved.  If not provided, the DDG will be saved
# in a directory called 'ddg' in the current working directory.  local (optional)
# - the environment in which to evaluate parsed expressions. If TRUE, the
# environment from which ddg.source is called. If FALSE, the user's workspace
# (global environment).  echo (optional) - print each expression after parsing.
# print.eval (optional) - print result of each evaluation.  verbose (optional) -
# print extra diagnostics.  max.deparse.length (optional) - maximum number of
# characters output for deparse of a single expression.  chdir (optional) -
# change R working directory temporarily to the directory containing the file to
# be sourced.  encoding (optional) - encoding to be assumed when file is a
# character string.  ignore.ddg.calls (optional) - if TRUE, ignore DDG function
# calls.  ignore.init (optional) - if TRUE, ignore ddg.init and ddg.run.
# force.console (optional) - if TRUE, turn console mode on.

ddg.source <- function(file, ddgdir = NULL, local = FALSE, echo = verbose, print.eval = echo,
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console = ignore.init) {
    # Store script number & name.
    snum <- .ddg.get(".ddg.next.script.num")
    sname <- basename(file)
    if (snum == 0) {
        df <- data.frame(snum, sname, stringsAsFactors = FALSE)
    } else {
        df <- rbind(.ddg.get(".ddg.sourced.scripts"), c(snum, sname))
    }
    .ddg.set(".ddg.sourced.scripts", df)
    # Increment script number.
    .ddg.inc(".ddg.next.script.num")

    ### CODE IN THIS SECTION IS BASICALLY REPLICATION OF source FUNCTION ###

    # Get the environment under which the script should be executed.
    envir <- if (isTRUE(local)) {
        parent.frame()
    } else if (identical(local, FALSE)) {
        .GlobalEnv
    } else if (is.environment(local)) {
        local
    } else stop("'local' must be TRUE, FALSE or an environment")
    # Parse encoding information.
    have_encoding <- !missing(encoding) && encoding != "unknown"
    if (!missing(echo)) {
        if (!is.logical(echo))
            stop("'echo' must be logical")
        if (!echo && verbose) {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'\n")
            echo <- TRUE
        }
    }
    # Print extra information about environment.
    if (verbose) {
        cat("'envir' chosen:")
        print(envir)
    }
    # Parse input file and figure out encoding.
    ofile <- file
    from_file <- FALSE
    srcfile <- NULL
    if (is.character(file)) {
        if (identical(encoding, "unknown")) {
            enc <- utils::localeToCharset()
            encoding <- enc[length(enc)]
        } else enc <- encoding
        if (length(enc) > 1L) {
            encoding <- NA
            owarn <- options("warn")
            options(warn = 2)
            for (e in enc) {
                if (is.na(e))
                  next
                zz <- file(file, encoding = e)
                res <- tryCatch(readLines(zz, warn = FALSE), error = identity)
                close(zz)
                if (!inherits(res, "error")) {
                  encoding <- e
                  break
                }
            }
            options(owarn)
        }
        if (is.na(encoding))
            stop("unable to find a plausible encoding")
        if (verbose)
            cat(gettextf("encoding = \"%s\" chosen", encoding), "\n", sep = "")
        if (file == "") {
            filename <- "stdin"
            file <- stdin()
            srcfile <- "<stdin>"
        } else {
            filename <- file
            file <- file(filename, "r", encoding = encoding)
            on.exit(close(file))
            lines <- readLines(file, warn = FALSE)

            on.exit()
            close(file)
            srcfile <- srcfilecopy(filename, lines, file.info(filename)[1, "mtime"],
                isFile = TRUE)
        }
        loc <- utils::localeToCharset()[1L]
        encoding <- if (have_encoding)
            switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", "unknown") else "unknown"
    } else {
        filename <- "Connection"
        lines <- readLines(file, warn = FALSE)

        srcfile <- srcfilecopy(deparse(substitute(file)), lines)
    }
    # Parse the expressions from the file.
    exprs <- if (!from_file) {
        if (length(lines)) {
            parse(stdin(), n = -1, lines, "?", srcfile, encoding, keep.source = TRUE)
        } else expression()
    } else {
        parse(file, n = -1, NULL, "?", srcfile, encoding, keep.source = TRUE)
    }
    on.exit()
    # Set the working directory for the current script and expressions.
    if (from_file)
        close(file)
    if (verbose)
        cat("--> parsed", "expressions; now eval(.)ing them:\n")
    if (chdir) {
        if (is.character(ofile)) {
            isURL <- length(grep("^(ftp|http|file)://", ofile)) > 0L
            if (isURL)
                warning("'chdir = TRUE' makes no sense for a URL")
            if (!isURL && (path <- dirname(ofile)) != ".") {
                owd <- getwd()
                if (is.null(owd)) {
                  stop("cannot 'chdir' as current directory is unknown")
                  on.exit(setwd(owd), add = TRUE)
                  setwd(path)
                }
            }
        } else {
            warning("'chdir = TRUE' makes no sense for a connection")
        }
    }
    ### END OF MODIFIED source CODE SECTION ###

    # Calculate the regular expressions for what should be ignored and what
    # shouldn't.
    if (ignore.ddg.calls && !ignore.init) {
        if (verbose)
            warning("'ignore.ddg.calls' is TRUE, 'ignore.int' not; ... coercion 'ignore.init <- TRUE'\n")
        ignore.init <- TRUE
    }
    # Ignore calculation of certain execution steps.
    ignores <- c("^library[(]provR[)]$", if (ignore.ddg.calls) "^ddg." else if (ignore.init) c("^ddg.init",
        "^prov.capture") else "a^")
    # Now we can parse the commands as we normally would for a DDG.
    if (length(exprs) > 0) {
        # Turn on the console if forced to, keep track of previous setting, parse
        # previous commands if necessary.
        prev.on <- (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && .ddg.get(".ddg.enable.console")
        if (prev.on && interactive())
            .ddg.console.node()
        if (force.console)
            .ddg.console.on()
        # Let library know that we are sourcing a file.
        prev.source <- (.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")) && (.ddg.is.set("from.source") && .ddg.get("from.source"))
        # Initialize the tables for ddg.capture.
        .ddg.set("from.source", TRUE)
        # Parse the commands into a console node.
        .ddg.parse.commands(exprs, sname, snum, environ = envir, ignore.patterns = ignores,
            node.name = sname, echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
            run.commands = TRUE)
        # Save the DDG among other things, but don't return any values, TODO - should we
        # do this?  ddg.save()
        .ddg.set("from.source", prev.source)
        # Turn return console to previous state.
        if (!prev.on)
            .ddg.console.off() else .ddg.console.on()
    }
    invisible()
}

# .ddg.console.off turns off the console mode of DDG construction.

.ddg.console.off <- function() {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # Capture history if console was on up to this point.
    if (interactive() && .ddg.get(".ddg.enable.console")) {
        .ddg.console.node()
    }
    # Set the console to off.
    .ddg.set(".ddg.enable.console", FALSE)
}

# .ddg.console.on turns on the console mode of DDG construction.

.ddg.console.on <- function() {
    if (!(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized")))
        return(invisible())
    # Write a new timestamp if we're turning on the console so we only capture
    # history from this point forward.
    if (!.ddg.get(".ddg.enable.console"))
        .ddg.write.timestamp.to.history()
    .ddg.set(".ddg.enable.console", TRUE)
}

# ddg.annotate.on enables annotation for the specified functions. Functions not
# on this list are not annotated.  If fnames is NULL, all functions will be
# annotated

# fnames - a list of one or more function names passed in as strings.

ddg.annotate.on <- function(fnames = NULL) {
    if (is.null(fnames)) {
        .ddg.set("ddg.annotate.off", vector())
        .ddg.set("ddg.annotate.inside", TRUE)
        return()
    }
    # Add to the on list
    on.list <- .ddg.get("ddg.annotate.on")
    on.list <- union(on.list, fnames)
    .ddg.set("ddg.annotate.on", on.list)
    # Remove from the off list
    off.list <- .ddg.get("ddg.annotate.off")
    off.list <- Filter(function(off) !(off %in% fnames), off.list)
    .ddg.set("ddg.annotate.off", off.list)
}

# ddg.annotate.off disables annotation for the specified functions.  Functions
# not on this list are annotated.  If fnames is NULL, no functions will be
# annotated fnames - a list of one or more function names passed in as strings.

ddg.annotate.off <- function(fnames = NULL) {
    if (is.null(fnames)) {
        .ddg.set("ddg.annotate.on", vector())
        .ddg.set("ddg.annotate.inside", FALSE)
        return()
    }
    # Add to the off list
    off.list <- .ddg.get("ddg.annotate.off")
    off.list <- union(off.list, fnames)
    .ddg.set("ddg.annotate.off", off.list)
    # Remove from the on list
    on.list <- .ddg.get("ddg.annotate.on")
    on.list <- Filter(function(on) !(on %in% fnames), on.list)
    .ddg.set("ddg.annotate.on", on.list)
}

# .ddg.flush.ddg removes all files from the DDG directories unless the the DDG
# directory is the working directory. If no DDG directory is specified, the
# current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

.ddg.flush.ddg <- function(ddg.path = NULL) {
    # Use current DDG directories if no directory is specified.
    if (is.null(ddg.path)) {
        ddg.path <- .ddg.get("ddg.path")
        ddg.path.data <- paste(.ddg.get("ddg.path"), "/data", sep = "")
        ddg.path.debug <- paste(.ddg.get("ddg.path"), "/debug", sep = "")
        ddg.path.scripts <- paste(.ddg.get("ddg.path"), "/scripts", sep = "")
    }
    # Remove files unless the DDG directory is the working directory.
    if (ddg.path != getwd()) {
        unlink(paste(ddg.path, "*.*", sep = "/"))
        unlink(paste(ddg.path.data, "*.*", sep = "/"))
        unlink(paste(ddg.path.data, ".ddghistory", sep = "/"))
        unlink(paste(ddg.path.debug, "*.*", sep = "/"))
        unlink(paste(ddg.path.scripts, "*.*", sep = "/"))
    }
    invisible()
}
