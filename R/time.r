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

# .format.time reformats time string. Input format is yyyy-mm-dd hh:mm:ss.
# Output format is (yyyy-mm-ddThh.mm.ss).

# time - input time string.

.format.time <- function(time) {
    formatted.time <- strftime(time, format = "%Y-%m-%dT%H.%M.%S", usetz = TRUE)
    # The strftime call leaves a space between time and time zone. We remove that
    # here.
    return(sub(" ", "", formatted.time))
}

.elapsed.time <- function() {
    time <- proc.time()
    if (.ddg.is.set(".ddg.proc.start.time"))
      start <- .ddg.get(".ddg.proc.start.time")
    else
      start <- 0
    elapsed <- time[1] + time[2] - start
    return(elapsed)
}

# .write.timestamp.to.history writes the current timestamp to the R history.
# The timestamp function does not work properly in Windows from within RStudio
# (the arguments are ignored).  In this case we create our own timestamp value
# and hope that the time does not change between when we set
# .ddg.history.timestamp and when the timestamp function inserts the timestamp in
# the history.

# var - the variable name under which the timestamp is saved.

.write.timestamp.to.history <- function(var = ".ddg.history.timestamp") {
    if (Sys.getenv("RSTUDIO") != "" && Sys.info()["sysname"] == "Windows") {
        .ddg.set(var, paste("##------", date(), "------##"))
        timestamp(quiet = TRUE)
    } else {
        .ddg.set(var, timestamp(prefix = "##-ddg-- ", quiet = TRUE))
    }
}
