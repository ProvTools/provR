#!/bin/bash

R CMD build .
R CMD check *tar.gz
R CMD INSTALL *tar.gz
