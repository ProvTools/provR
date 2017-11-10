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

# prov.capture executes a script (r.script.path).  r.script.path - the full path
# to the R script.  If provided, a copy of the script will be saved with the DDG.
# If only r.script.path is provided, the script is sourced using ddg.source and a
# DDG is created for the script.  annotate.inside.functions (optional) - if TRUE,
# functions are annotated.  first.loop (optional) - the first loop to annotate in
# a for, while, or repeat statement.  max.loops (optional) - the maximum number
# of loops to annotate in a for, while, or repeat statement. If max.loops = -1
# there is no limit.  If max.loops = 0, no loops are annotated.  If non-zero,
# if-statements are also annotated.  save (optional) - if TRUE information is
# saved.
#' @export
prov.capture <- function(r.script.path = NULL,
    enable.console = TRUE, annotate.inside.functions = FALSE, first.loop = 1, max.loops = 1,
    max.snapshot.size = 10, debug = FALSE) {
    # Initiate ddg.
    prov.init(r.script.path, NULL, FALSE, enable.console, annotate.inside.functions,
        first.loop, max.loops, max.snapshot.size)
    # Set .ddg.is.sourced to TRUE if script provided.
    if (!is.null(r.script.path))
        .global.set(".ddg.is.sourced", TRUE)
    # If an R error is generated, get the error message and close the DDG.
    tryCatch(
      if (!is.null(r.script.path))
        prov.source(.global.get("ddg.r.script.path"), ddgdir = NULL, ignore.ddg.calls = FALSE, ignore.init = TRUE, force.console = FALSE)
    )
    invisible()
}

# prov.json return thr provenance json data corresponding to latest captured
# script.
#' @export
prov.json <- function() {
    json <- .json.current()
    return(json)
}

# ddg.annotate.on enables annotation for the specified functions. Functions not
# on this list are not annotated.  If fnames is NULL, all functions will be
# annotated
# fnames - a list of one or more function names passed in as strings.
ddg.annotate.on <- function(fnames = NULL) {
    if (is.null(fnames)) {
        .global.set("ddg.annotate.off", vector())
        .global.set("ddg.annotate.inside", TRUE)
        return()
    }
    # Add to the on list
    on.list <- .global.get("ddg.annotate.on")
    on.list <- union(on.list, fnames)
    .global.set("ddg.annotate.on", on.list)
    # Remove from the off list
    off.list <- .global.get("ddg.annotate.off")
    off.list <- Filter(function(off) !(off %in% fnames), off.list)
    .global.set("ddg.annotate.off", off.list)
}

# ddg.annotate.off disables annotation for the specified functions.  Functions
# not on this list are annotated.  If fnames is NULL, no functions will be
# annotated fnames - a list of one or more function names passed in as strings.
ddg.annotate.off <- function(fnames = NULL) {
    if (is.null(fnames)) {
        .global.set("ddg.annotate.on", vector())
        .global.set("ddg.annotate.inside", FALSE)
        return()
    }
    # Add to the off list
    off.list <- .global.get("ddg.annotate.off")
    off.list <- union(off.list, fnames)
    .global.set("ddg.annotate.off", off.list)
    # Remove from the on list
    on.list <- .global.get("ddg.annotate.on")
    on.list <- Filter(function(on) !(on %in% fnames), on.list)
    .global.set("ddg.annotate.on", on.list)
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
prov.source <- function(file, ddgdir = NULL, local = FALSE, echo = verbose, print.eval = echo,
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console = ignore.init) {
    # Store script number & name.
    snum <- .global.get(".ddg.next.script.num")
    sname <- basename(file)
    if (snum == 0) {
        df <- data.frame(snum, sname, stringsAsFactors = FALSE)
    } else {
        df <- rbind(.global.get(".ddg.sourced.scripts"), c(snum, sname))
    }
    .global.set(".ddg.sourced.scripts", df)
    # Increment script number.
    .global.inc(".ddg.next.script.num")

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
        prev.on <- (.global.is.set(".ddg.initialized") && .global.get(".ddg.initialized")) && .global.get(".ddg.enable.console")
        if (prev.on && interactive())
            .ddg.console.node()
        if (force.console)
            .ddg.console.on()
        # Let library know that we are sourcing a file.
        prev.source <- (.global.is.set(".ddg.initialized") && .global.get(".ddg.initialized")) && (.global.is.set("from.source") && .global.get("from.source"))
        # Initialize the tables for ddg.capture.
        .global.set("from.source", TRUE)
        # Parse the commands into a console node.
        .ddg.parse.commands(exprs, sname, snum, environ = envir, ignore.patterns = ignores,
            node.name = sname, echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
            run.commands = TRUE)
        # Save the DDG among other things, but don't return any values, TODO - should we
        # do this?  ddg.save()
        .global.set("from.source", prev.source)
        # Turn return console to previous state.
        if (!prev.on)
            .ddg.console.off() else .ddg.console.on()
    }
    invisible()
}
