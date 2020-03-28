---
title: " ukghg: spatio-temporal modelling of anthropogenic and biogenic fluxes of greenhouse gases in the UK"
author: "Peter Levy"
date: "2020-03-28"
# output: rmarkdown::html_vignette
output:
  html_document:
    keep_md: yes
vignette: >
  %\VignetteIndexEntry{use ukghg}
  %\VignetteEngine{knitr::rmarkdown}
  %\usepackage[utf8]{inputenc}
---



## Introduction 
This document describes the `ukghg` R package for producing spatio-temporal predictions of UK GHG emissions for the period 1990 to near-present.  Most users will only use a single function `calcFlux`, which returns a time-series of maps, given a date-time stamp as input (i.e. a 3D data cube).
The package uses the data structures provided by the `raster` package (https://CRAN.R-project.org/package=raster), but output can be written to generic netCDF files for use in other programs.

## Installation
The package can be installed directly from a bundled .tar.gz file.
Alternatively, and to keep versions up to date more easily, the package can be installed directly from GitHub, using `install_github` from devtools.
However, this is slightly more complicated than usual because there are some large files to install, and `install_github` does not work with the standard GitHub approach to this (Git LFS).
Instead we use the `piggyback` package to retrieve the large files.  This is achieved in the code chunk below.


```r
# If not already installed:
# install.packages(c("devtools", "piggyback"))
library(devtools)  # for install_github
library(piggyback) # for binary files from github
Sys.setenv(GITHUB_TOKEN="cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")

# install ukghg from github using an OAuth token
install_github("NERC-CEH/ukghg", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
# find where this has been installed:
pkgpath <- find.package("ukghg")
# and download the large binary files into here from github:
pb_download(repo = "NERC-CEH/ukghg",
            tag = "v0.6.0", dest = pkgpath)
```

## Using the package
Now we can load the package.

```r
library(ukghg)
```

One way to use it is via a web interface (a Shiny app), which opens as a web page in your browser.  You start this by typing:

`runShinyApp()`

at the R command line. Alternatively, you can call the `ukghg` functions directly from the R command line, as described below. The basic input is a sequence of time points, in POSIXct format.  You can specify these any way you want, but a simple sequence with regular intervals between two points is created with the following lines.


```r
# create a sequence of 2-hourly time points over one day
startDate <- as.POSIXct(strptime("01/07/2016", "%d/%m/%Y"), tz = "UTC")
endDate   <- as.POSIXct(strptime("02/07/2016", "%d/%m/%Y"), tz = "UTC")
nTimes <- 13  # number of time points
datect <- seq(startDate, endDate, length = nTimes)
datect
```

```
##  [1] "2016-07-01 00:00:00 UTC" "2016-07-01 02:00:00 UTC"
##  [3] "2016-07-01 04:00:00 UTC" "2016-07-01 06:00:00 UTC"
##  [5] "2016-07-01 08:00:00 UTC" "2016-07-01 10:00:00 UTC"
##  [7] "2016-07-01 12:00:00 UTC" "2016-07-01 14:00:00 UTC"
##  [9] "2016-07-01 16:00:00 UTC" "2016-07-01 18:00:00 UTC"
## [11] "2016-07-01 20:00:00 UTC" "2016-07-01 22:00:00 UTC"
## [13] "2016-07-02 00:00:00 UTC"
```

Now we can use the `calcFlux` function to output flux maps for these times.  At a minumum, you need to specify:

* `ghgName` - the greenhouse (or tracer) gas name: one of "ch4", "co2", "n2o", "c2h6" or "voc"
* `datect` - the vector of timestamps in POSIXct format
* `res`  - the resolution for the gridded data incharacter format, either "1", "20" or "100" km.  Defaults to 1 km, but it obviously quicker on a 100-km grid.
* `unitType` - detemines the type of unit used for map output, either molar ("mol") or mass-based ("g"). Defaults to "mol".
* `unitSIprefix` - allows you to request map output in different units by specifying any standard SI prefix (four/five characters, lower case e.g. "mega", "peta", "pico").

For example, to predict diurnal carbon dioxide flux on 1st July 2016 on a 20-km grid in $\mu$mol CO$_2$ m^-2^ s^-1^, we would use:

```r
flux_co2 <- calcFlux("co2", datect, res = "20", unitType = "mol", unitSIprefix = "micro")
```

To predict methane on a 100-km grid in nmol CH$_4$ m^-2^ s^-1^, we would use:

```r
flux_ch4 <- calcFlux("ch4", datect, res = "100", unitType = "mol", unitSIprefix = "nano")
```

To predict monthly ethane emission over four years on a 100-km grid in pg C$_2$H$_6$ m^-2^ s^-1^, ignoring day-of-the-week and diurnal variation, we would use:

```r
# create a sequence of monthly time points over four years
startDate <- as.POSIXct(strptime("01/01/2013", "%d/%m/%Y"), tz = "UTC")
endDate   <- as.POSIXct(strptime("01/12/2016", "%d/%m/%Y"), tz = "UTC")
nTimes <- 12*4  # number of time points
datect <- seq(startDate, endDate, length = nTimes)

system.time(
  flux_c2h6 <- calcFlux("c2h6", datect, res = "100", unitType = "g", unitSIprefix = "pico",
    timeScales = c(TRUE, TRUE, FALSE, FALSE))
)
```

```
##    user  system elapsed 
##   13.11    2.96   16.82
```

We can plot the resulting time series and raster maps in several ways, most simply with base R graphics e.g.


```r
plot(datect, flux_c2h6$total, ylab = "Ethane flux (Tg/y)", type = "b")
```

![](use_ukghg_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
names(flux_co2$ls_ghgByTimeBySector[[4]]) <- c(sectorLongName, "Natural")
plot(flux_co2$s_ghgTotal[[7]], 
main = "CO2 flux at 2016-07-01 12:00, umol/m2/s")
```

![](use_ukghg_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

## Other options
Further options allow you to specify the geographic projection of output rasters, the option to write to netCDF files, and which sectors to include.
Variation on four different temporal scales is included by default, but this can be restricted with the `timeScales` parameter.

* `proj` Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB.
* `writeNetCDF` Write NetCDF output files. Defaults to FALSE.
* `sectorList` A vector of SNAP sector numbers for which values should be returned, e.g. c(1,3,7). Defaults to all. Unlikely to be used, and probably will be removed in future.
* `includeBio` A logical for whether biogenic fluxes should be calculated as well as anthropogenic sectors 1-10. Defaults to TRUE.
* `timeScales` A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.

## Output
The object returned by the `calcFlux` function provides several aggregations of the data:

* `total` A vector of total flux
* `s_ghgTotal` A RasterStack of total flux
* `ls_ghgByTimeBySector` A list of RasterStacks of ghg fluxes where the z dimension corresponds to sector, one per timestep
* `ls_ghgBySectorByTime` A list of RasterStacks of ghg fluxes where the z dimension corresponds to timestep, one per sector


