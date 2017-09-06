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

# .ddg.lookup.function.name gets the name of the calling function and returns it
# as a string. pname may be passed as a string or a name. If NULL, pname is
# obtained from the calling environment.  Note that it is important that these be
# macros, not functions, due to the use of the substitute function in the body.

# pname - name of procedure node.

# If pname is not provided, get from function call.  Look up function call.  If
# the call uses a closure rather than a function name, we will call the name FUN.

.ddg.lookup.function.name <- gtools::defmacro(pname, expr = if (is.null(pname)) {
    call <- sys.call(-4)
    if (typeof(call[[1]]) == "closure") {
        pname <- "FUN"
    } else {
        pname <- as.character(call[[1]])
    }
} else if (!is.character(pname)) {
    # Convert pname to a string if necessary.
    pname <- deparse(substitute(pname))
})

# .ddg.lookup.value is used to determine what value to use when creating data
# nodes.  Note that it is important that these be macros, not functions, due to
# the use of the substitute function in the body.

# expr - the expression to be evaluted. This can be a string or a name.  value -
# the value that was passed in to the calling function.  If value already exists,
# nothing happens. If value is NULL, the expression is evaluated to determine the
# value.  env - the environment in which the evaluation is done.  procname - the
# name of the calling procedure, used to produce an error message if necessary.
# warn (optional) - if TRUE, warns user that the expression could not be
# evaluated.

.ddg.lookup.value <- gtools::defmacro(expr, value, env, procname, warn = TRUE, expr = if (is.null(value)) {
    arg <- substitute(expr)
    if (is.character(arg)) {
        tryCatch(arg <- parse(text = expr), error = function(e) {
        })
    } else expr <- deparse(arg)
    value <- tryCatch(eval(arg, env), error = function(e) {
        if (warn) {
            error.msg <- paste("Unable to evaluate", expr, "in call to", procname)
            .ddg.insert.error.message(error.msg)
        }
        return("")
    })
})
