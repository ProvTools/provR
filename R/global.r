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

##### Mutators for specific common actions
.global.inc <- function(var) {
    value <- .global.get(var)
    .global.set(var, value + 1)
}

.global.dec <- function(var) {
    value <- .global.get(var)
    .global.set(var, value - 1)
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
