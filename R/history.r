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

# .ddg.load.history takes in the name of a history file, opens it, scans it for
# the last occurrence of the string specified by timestamp, and returns the lines
# from that point forward.
# hist.file - name of history file.  timestamp - timestamp string.
.ddg.load.history <- function(hist.file, timestamp) {
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

# .ddg.save.history saves the current and unsaved R command history to the
# specified file if that file matches the DDG history file.  Note: the commented
# section of code appends the information to this file.

# savehistory is not supported on all R platforms.  If it is not supported, this
# will fail silently.

# hist.file - name of history file.

.ddg.save.history <- function(hist.file) {
    # USED TO STORE ENTIRE HISTORY IN SEP. FILE.  Write history out to temporary file
    if (.ddg.is.set(".ddg.history.file") && is.character(.ddg.get(".ddg.history.file")) &&
        .ddg.get(".ddg.history.file") == hist.file) {
        savehistory(hist.file)
    }
}
