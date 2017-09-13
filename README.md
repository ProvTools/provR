provR: Capture data provenance in R
===================================

Data provenance is a formalized, automatic way to record essential
meta-data about a computation as it's being executed. This meta-data
can provide key information for data science transparency,
reproducibility and benefaction. This package is the workhorse that
provides data provenance for users to analyze with other packages,
like those found in the [ProvTools](https://github.com/ProvTools)
ecosystem. 


Build Status
============

 | Branch      |Status                                                                                                                                                                                  |
 |-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
 | master      | [![Master Build Status](https://api.travis-ci.org/provtools/provr.svg?branch=master)](https://travis-ci.org/provtools/provr/branches)            |
 | development | [![Development Build Status](https://api.travis-ci.org/provtools/provr.svg?branch=dev)](https://travis-ci.org/provtools/provr/branches)  |


Install
=======

Install the most up-to-date version using the 
[devtools](https://github.com/hadley/devtools) package:

```R
library(devtools)
install_github("ProvTools/provR")
```

Capture Provenance
==================

Once loaded, the **provR** package provides a simple API to collect
provenance from scripts:

```R
library(provR)
prov.capture("example.R") ### NOT RUN
```

This runs the script "example.R" and records data provenance as the
script is running. The provenance is stored in memory as a
[W3C PROV-JSON](https://www.w3.org/Submission/2013/SUBM-prov-json-20130424)
formatted string, which can be accessed with:

```R
prov.json()
```

If you would like to write the provenance to disk, just run:

```R
prov.capture("example.R", save = TRUE)
```



**provR** is a lightweight fork of [RDataTracker](https://github.com/End-to-end-provenance/RDataTracker).



