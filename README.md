
<!-- README.md is generated from README.Rmd. Please edit that file -->
INTEREST
========

INTEREST, acronym for **IN**teractive **T**ool for **E**xploring **RE**sults from **S**imulation s**T**udies, is an interactive web app developed using R and [`shiny`](https://shiny.rstudio.com/). It allows exploring results from simulation studies interactively.

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ellessenne/interest?branch=master&svg=true)](https://ci.appveyor.com/project/ellessenne/interest) [![Travis-CI Build Status](https://travis-ci.org/ellessenne/interest.svg?branch=master)](https://travis-ci.org/ellessenne/interest) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/interest)](https://cran.r-project.org/package=interest) [![CRAN\_Logs\_Badge](http://cranlogs.r-pkg.org/badges/interest)](https://cran.r-project.org/package=interest) [![CRAN\_Logs\_Badge\_Total](http://cranlogs.r-pkg.org/badges/grand-total/interest)](https://cran.r-project.org/package=interest)

Installation
============

INTEREST is shipped as a standalone R package, hosted only on GitHub at the moment.

It can be installed by typing the following R commands in your R console:

``` r
# install.packages("devtools")
devtools::install_github("ellessenne/interest")
```

Launch application
==================

To launch INTEREST first load the `interest` R package:

``` r
library(interest)
```

Then, use the `interest()` function:

``` r
interest()
```

The app should now launch in your browser.

Workflow
========

![](README_files/sirex-workflow.png)

The workflow of INTERACT is simple:

1.  load your data and define variables representing point estimates, standard errors, methods, and so on;
2.  summary statistics are computed automatically;
3.  tables and plots are produced automatically, and can be customised and explored interactively;
4.  tables and plots can be exported for later use.

Additional material on INTEREST
===============================

-   Slides to present SiReX (former name of INTEREST) at the Students’ Day, 38<sup>th</sup> Annual Conference of the International Society for Clinical Biostatistics [\[here\]](README_files/iscb38_slides.pdf)
-   Poster at the 2017 Annual Conference of the Department of Health Sciences, University of Leicester [\[here\]](README_files/hs_poster.pdf)
