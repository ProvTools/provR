#!/bin/bash

Rscript prepare.r
ant -file tests.xml test-travis
