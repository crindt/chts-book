#!/bin/bash
PATH=/home/crindt/.cabal/bin:$PATH R -e "library(knitr); rmarkdown::render('chts_book.Rmd')"
