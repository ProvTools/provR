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
prov.capture <- function(r.script.path, annotate.inside.functions = FALSE, first.loop = 1, 
    max.loops = 1, save = FALSE) {
    # Initiate ddg.
    ddg.init(r.script.path, NULL, FALSE, TRUE, annotate.inside.functions, first.loop, 
        max.loops, 0, save.to.disk = save)
    
    # Set .ddg.is.sourced to TRUE if script provided.
    if (!is.null(r.script.path)) 
        .ddg.set(".ddg.is.sourced", TRUE)
    
    # Save debug files to debug directory.
    .ddg.set("ddg.save.debug", FALSE)
    
    # If an R error is generated, get the error message and close the DDG.
    tryCatch(if (!is.null(r.script.path)) 
        ddg.source(.ddg.get("ddg.r.script.path"), ddgdir = NULL, ignore.ddg.calls = FALSE, 
            ignore.init = TRUE, force.console = FALSE) else stop("r.script.path cannot be NULL"), finally = {
        if (save) 
            ddg.save(r.script.path, FALSE)
    })
    
    invisible()
}

# prov.json return thr provenance json data corresponding to latest captured
# script.
#' @export
prov.json <- function() {
    ddg.json <- .ddg.json.current()
    return(ddg.json)
}
