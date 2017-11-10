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

.json.installedpackages <- function() {
    installed <- .ddg.installedpackages()
    output <- "\"rdt:installedPackages\" : [\n\t"
    packagearray <- paste("{\"package\" : \"", installed[, 1], "\", \"version\" : \"",
        installed[, 2], "\"}", sep = "", collapse = ",\n\t")
    output <- paste(output, packagearray, "]", sep = "")
    return(output)
}

# .json.nv returns a name-value pair for the ddg.json string.

.json.nv <- function(name, value) {
    jstr <- paste("\"", name, "\" : \"", value, "\",\n", sep = "")
    return(jstr)
}

.json.prefix <- function() {
    # add json prefix
    prefix <- "\"prefix\" : {\n\"prov\" : \"http://www.w3.org/ns/prov#\",\n\"rdt\" : \"http://rdatatracker.org/\"\n},\n"
    return(prefix)
}

# .json.environ returns prefix and environment information for the ddg.json
# string.

.json.environ <- function() {
    # add environment entities
    environ <- ""
    environ <- paste(environ, "\n\"environment\" : {\n", sep = "")
    environ <- paste(environ, .json.nv("rdt:name", "environment"), sep = "")
    environ <- paste(environ, .json.nv("rdt:architecture", R.Version()$arch), sep = "")
    environ <- paste(environ, .json.nv("rdt:operatingSystem", .Platform$OS.type), sep = "")
    environ <- paste(environ, .json.nv("rdt:language", "R"), sep = "")
    environ <- paste(environ, .json.nv("rdt:rVersion", R.Version()$version), sep = "")
    ddg.r.script.path <- .global.get("ddg.r.script.path")
    if (!is.null(ddg.r.script.path)) {
        script <- ddg.r.script.path
        sourced.scripts <- .ddg.sourced.script.names.json()
        script.timestamp <- .format.time(file.info(ddg.r.script.path)$mtime)
    } else {
        script <- ""
        sourced.scripts <- ""
        script.timestamp <- ""
        sourced.scripts.timestamps <- ""
    }
    environ <- paste(environ, .json.nv("rdt:script", ddg.r.script.path), sep = "")
    environ <- paste(environ, "\"rdt:sourcedScripts\" : ", sourced.scripts, ",\n", sep = "")
    environ <- paste(environ, .json.nv("rdt:scriptTimeStamp", script.timestamp), sep = "")
    environ <- paste(environ, .json.nv("rdt:workingDirectory", getwd()), sep = "")
    environ <- paste(environ, .json.nv("rdt:ddgDirectory", .global.get("ddg.path")), sep = "")
    environ <- paste(environ, .json.nv("rdt:ddgTimeStamp", .global.get("ddg.start.time")), sep = "")
    environ <- paste(environ, .json.nv("rdt:provRVersion", packageVersion("provR")), sep = "")
    environ <- paste(environ, .json.installedpackages(), sep = "")
    environ <- paste(environ, "\n}", sep = "")
    return(environ)
}

# .json.procedure.node adds a procedure node to the ddg.json string.

.json.procedure.node <- function(id, pname, ptype, ptime, snum, pos) {
    if (is.object(pos)) {
        jstr <- paste("\n\"p", id, "\" : {\n\"rdt:name\" : \"", pname, "\",\n\"rdt:type\" : \"",
            ptype, "\",\n\"rdt:elapsedTime\" : \"", ptime, "\",\n\"rdt:scriptNum\" : \"",
            snum, "\",\n\"rdt:startLine\" : \"", pos@startLine, "\"", ",\n\"rdt:startCol\" : \"",
            pos@startCol, "\"", ",\n\"rdt:endLine\" : \"", pos@endLine, "\"", ",\n\"rdt:endCol\" : \"",
            pos@endCol, "\"", "\n}", sep = "")
    } else {
        jstr <- paste("\n\"p", id, "\" : {\n\"rdt:name\" : \"", pname, "\",\n\"rdt:type\" : \"",
            ptype, "\",\n\"rdt:elapsedTime\" : \"", ptime, "\",\n\"rdt:scriptNum\" : \"",
            snum, "\",\n\"rdt:startLine\" : \"NA\"", ",\n\"rdt:startCol\" : \"NA\"",
            ",\n\"rdt:endLine\" : \"NA\"", ",\n\"rdt:endCol\" : \"NA\"", "\n}", sep = "")
    }
    .ddg.append.activity(jstr)
}

# .json.data.node adds a data node to the ddg.json string.

.json.data.node <- function(id, dname, dvalue, val.type, dtype, dscope, from.env,
    dhash, dtime, dloc) {
    jstr <- paste("\n\"d", id, "\" : {\n\"rdt:name\" : \"", dname, "\",\n\"rdt:value\" : \"",
        dvalue, "\",\n\"rdt:valType\" : ", val.type, ",\n\"rdt:type\" : \"", dtype,
        "\",\n\"rdt:scope\" : \"", dscope, "\",\n\"rdt:fromEnv\" : \"", from.env,
        "\",\n\"rdt:MD5hash\" : \"", dhash, "\",\n\"rdt:timestamp\" : \"", dtime,
        "\",\n\"rdt:location\" : \"", dloc, "\"\n}", sep = "")
    .ddg.append.entity(jstr)
}

# .json.control.edge adds a control flow edge to the ddg.json string.

.json.control.edge <- function(id, node1, node2) {
    jstr <- paste("\n\"e", id, "\" : {\n\"prov:informant\" : \"", node1, "\",\n\"prov:informed\" : \"",
        node2, "\"\n}", sep = "")
    .ddg.append.wasInformedBy(jstr)
}

# .json.data.out.edge adds an output data flow edge to the ddg.json string.

.json.data.out.edge <- function(id, node1, node2) {
    jstr <- paste("\n\"e", id, "\" : {\n\"prov:entity\" : \"", node2, "\",\n\"prov:activity\" : \"",
        node1, "\"\n}", sep = "")
    .ddg.append.wasGeneratedBy(jstr)
}

# .json.data.in.edge adds an input data flow edge to the ddg.json string.

.json.data.in.edge <- function(id, node1, node2) {
    jstr <- paste("\n\"e", id, "\" : {\n\"prov:activity\" : \"", node2, "\",\n\"prov:entity\" : \"",
        node1, "\"\n}", sep = "")
    .ddg.append.used(jstr)
}

# .ddg.sourced.script.names.json returns sourced script names, numbers and
# timestamps for the JSON file.

.ddg.sourced.script.names.json <- function() {
    ss <- .global.get(".ddg.sourced.scripts")
    # First row is main script.
    if (nrow(ss) == 1) {
        output <- "\"\"\n"
    } else {
        ss <- ss[ss$snum > 0, ]
        stimes <- file.info(ss$sname)$mtime
        stimes <- .format.time(stimes)

        scriptarray <- paste("\t{\"number\" : \"", ss[, 1], "\",
                             \"name\" : \"",
            ss[, 2], "\",
                             \"timestamp\" : \"", stimes,
            "\"}", sep = "", collapse = ",\n")
        output <- paste("[\n", scriptarray, " ]", sep = "")
    }
    return(output)
}

# .json.current returns the current ddg.json string.

.json.current <- function() {
    prefix <- .json.prefix()
    environ <- .json.environ()
    .ddg.append.activity(environ)
    activity <- .global.get("ddg.activity")
    entity <- .global.get("ddg.entity")
    wasInformedBy <- .global.get("ddg.wasInformedBy")
    wasGeneratedBy <- .global.get("ddg.wasGeneratedBy")
    used <- .global.get("ddg.used")
    ddg.json <- paste("{\n\n", prefix, "\"activity\":{\n", activity, "},\n", "\"entity\":{\n",
        entity, "},\n", "\"wasInformedBy\":{\n", wasInformedBy, "},\n", "\"wasGeneratedBy\":{\n",
        wasGeneratedBy, "},\n", "\"used\":{\n", used, "}\n", "}", sep = "")
    return(ddg.json)
}
