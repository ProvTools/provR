Data Provenance Capture in R
============================

Collect meta-data from scripts written in the R programming language.



Build Status
============

 | Branch      |Status                                                                                                                                                                                  |
 |-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
 | master      | [![Master Build Status](https://api.travis-ci.org/provtools/provr.svg?branch=master)](https://travis-ci.org/provtools/provr/branches)            |
 | development | [![Development Build Status](https://api.travis-ci.org/provtools/provr.svg?branch=dev)](https://travis-ci.org/provtools/provr/branches)  |





To Install
==========

Install directly from github using [devtools](https://github.com/hadley/devtools):

```R
library(devtools)
install_github("ProvTools/ProvR")
```


Collect Provenance
==================

Once installed a libraried, you can now easily collect provenance from
any script:

```R
library(ProvR)
prov.capture("example.R")
```

The provenance will be stored in memory as a JSON formatted string
following the
[W3C PROV-JSON](https://www.w3.org/Submission/2013/SUBM-prov-json-20130424)
standard format , which can be accessed like so:


```R
prov.json()
```

You can write the provenance to your hard drive by doing the
following:

```R
prov.capture("example.R", save = TRUE)
```




**ProvR** is a simplified fork of the [RDataTracker](https://github.com/End-to-end-provenance/RDataTracker)
