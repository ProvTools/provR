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

.ddg.installedpackages.json <- function() {
    installed <- .ddg.installedpackages()
    output <- "\"rdt:installedPackages\" : [\n\t"
    packagearray <- paste("{\"package\" : \"", installed[, 1], "\", \"version\" : \"",
        installed[, 2], "\"}", sep = "", collapse = ",\n\t")
    output <- paste(output, packagearray, "]", sep = "")
    return(output)
}

# .ddg.json.nv returns a name-value pair for the ddg.json string.

.ddg.json.nv <- function(name, value) {
    jstr <- paste("\"", name, "\" : \"", value, "\",\n", sep = "")
    return(jstr)
}

.ddg.json.prefix <- function() {
    # add json prefix
    prefix <- "\"prefix\" : {\n\"prov\" : \"http://www.w3.org/ns/prov#\",\n\"rdt\" : \"http://rdatatracker.org/\"\n},\n"
    return(prefix)
}

# .ddg.json.environ returns prefix and environment information for the ddg.json
# string.

.ddg.json.environ <- function() {
    # add environment entities
    environ <- ""
    environ <- paste(environ, "\n\"environment\" : {\n", sep = "")

    environ <- paste(environ, .ddg.json.nv("rdt:name", "environment"), sep = "")

    architecture <- R.Version()$arch
    environ <- paste(environ, .ddg.json.nv("rdt:architecture", architecture), sep = "")

    operating.system <- .Platform$OS.type
    environ <- paste(environ, .ddg.json.nv("rdt:operatingSystem", operating.system),
        sep = "")

    language = "R"
    environ <- paste(environ, .ddg.json.nv("rdt:language", language), sep = "")

    r.version <- R.Version()$version
    environ <- paste(environ, .ddg.json.nv("rdt:rVersion", r.version), sep = "")

    ddg.r.script.path <- .ddg.get("ddg.r.script.path")
    if (!is.null(ddg.r.script.path)) {
        script <- ddg.r.script.path
        sourced.scripts <- .ddg.sourced.script.names.json()
        script.timestamp <- .ddg.format.time(file.info(ddg.r.script.path)$mtime)
    } else {
        script <- ""
        sourced.scripts <- ""
        script.timestamp <- ""
        sourced.scripts.timestamps <- ""
    }

    environ <- paste(environ, .ddg.json.nv("rdt:script", ddg.r.script.path), sep = "")

    environ <- paste(environ, "\"rdt:sourcedScripts\" : ", sourced.scripts, ",\n",
        sep = "")

    environ <- paste(environ, .ddg.json.nv("rdt:scriptTimeStamp", script.timestamp),
        sep = "")

    working.directory = getwd()
    environ <- paste(environ, .ddg.json.nv("rdt:workingDirectory", working.directory),
        sep = "")

    ddg.directory = .ddg.get("ddg.path")
    environ <- paste(environ, .ddg.json.nv("rdt:ddgDirectory", ddg.directory), sep = "")

    ddg.timestamp <- .ddg.get("ddg.start.time")
    environ <- paste(environ, .ddg.json.nv("rdt:ddgTimeStamp", ddg.timestamp), sep = "")

    lib.version <- packageVersion("provR")
    environ <- paste(environ, .ddg.json.nv("rdt:provRVersion", lib.version), sep = "")

    environ <- paste(environ, .ddg.installedpackages.json(), sep = "")
    environ <- paste(environ, "\n}", sep = "")

    return(environ)
}

# .ddg.json.procedure.node adds a procedure node to the ddg.json string.

.ddg.json.procedure.node <- function(id, pname, ptype, ptime, snum, pos) {

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

# .ddg.json.data.node adds a data node to the ddg.json string.

.ddg.json.data.node <- function(id, dname, dvalue, val.type, dtype, dscope, from.env,
    dhash, dtime, dloc) {

    jstr <- paste("\n\"d", id, "\" : {\n\"rdt:name\" : \"", dname, "\",\n\"rdt:value\" : \"",
        dvalue, "\",\n\"rdt:valType\" : ", val.type, ",\n\"rdt:type\" : \"", dtype,
        "\",\n\"rdt:scope\" : \"", dscope, "\",\n\"rdt:fromEnv\" : \"", from.env,
        "\",\n\"rdt:MD5hash\" : \"", dhash, "\",\n\"rdt:timestamp\" : \"", dtime,
        "\",\n\"rdt:location\" : \"", dloc, "\"\n}", sep = "")

    .ddg.append.entity(jstr)
}

# .ddg.json.control.edge adds a control flow edge to the ddg.json string.

.ddg.json.control.edge <- function(id, node1, node2) {

    jstr <- paste("\n\"e", id, "\" : {\n\"prov:informant\" : \"", node1, "\",\n\"prov:informed\" : \"",
        node2, "\"\n}", sep = "")

    .ddg.append.wasInformedBy(jstr)
}

# .ddg.json.data.out.edge adds an output data flow edge to the ddg.json string.

.ddg.json.data.out.edge <- function(id, node1, node2) {

    jstr <- paste("\n\"e", id, "\" : {\n\"prov:entity\" : \"", node2, "\",\n\"prov:activity\" : \"",
        node1, "\"\n}", sep = "")

    .ddg.append.wasGeneratedBy(jstr)
}

# .ddg.json.data.in.edge adds an input data flow edge to the ddg.json string.

.ddg.json.data.in.edge <- function(id, node1, node2) {

    jstr <- paste("\n\"e", id, "\" : {\n\"prov:activity\" : \"", node2, "\",\n\"prov:entity\" : \"",
        node1, "\"\n}", sep = "")

    .ddg.append.used(jstr)
}

# .ddg.json.current returns the current ddg.json string.

.ddg.json.current <- function() {
    prefix <- .ddg.json.prefix()
    environ <- .ddg.json.environ()
    .ddg.append.activity(environ)
    activity <- .ddg.get("ddg.activity")
    entity <- .ddg.get("ddg.entity")
    wasInformedBy <- .ddg.get("ddg.wasInformedBy")
    wasGeneratedBy <- .ddg.get("ddg.wasGeneratedBy")
    used <- .ddg.get("ddg.used")
    ddg.json <- paste("{\n\n", prefix, "\"activity\":{\n", activity, "},\n", "\"entity\":{\n",
        entity, "},\n", "\"wasInformedBy\":{\n", wasInformedBy, "},\n", "\"wasGeneratedBy\":{\n",
        wasGeneratedBy, "},\n", "\"used\":{\n", used, "}\n", "}", sep = "")
    return(ddg.json)
}

# .ddg.json.write writes the current ddg.json string to the file ddg.json on the
# ddg directory.

.ddg.json.write <- function() {
    fileout <- paste(.ddg.get("ddg.path"), "/ddg.json", sep = "")
    # if (interactive()) print(paste('Saving DDG in ', fileout))
    ddg.json <- .ddg.json.current()
    write(ddg.json, fileout)
}
