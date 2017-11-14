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
