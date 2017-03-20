[![CRAN version](http://www.r-pkg.org/badges/version/ukds)](https://cran.r-project.org/package=icpsrdata) ![](http://cranlogs.r-pkg.org/badges/grand-total/ukds) [![Travis-CI Build Status](https://travis-ci.org/fsolt/ukds.svg?branch=master)](https://travis-ci.org/fsolt/ukds)
------------------------------------------------------------------------

ukds
=========

`ukds` is an R package that provides reproducible, programmatic access to datasets stored in the [UK Data Service](https://www.ukdataservice.ac.uk) for [registered users](http://esds.ac.uk/newRegistration/newLogin.asp).


To install:

* the latest released version: `install.packages("ukds")`
* the latest development version:

```R
if (!require(ghit)) install.packages("ghit")
ghit::install_github("fsolt/ukds")
```

For more details, check out [the vignette](https://cran.r-project.org/package=ukds/vignettes/ukds-vignette.html).

* Note that, on Windows systems, `ukds` requires that [RTools](https://cran.r-project.org/bin/windows/Rtools/index.html) is installed.