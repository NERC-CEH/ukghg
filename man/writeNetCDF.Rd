% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukghg.R
\name{writeNetCDF}
\alias{writeNetCDF}
\title{A writeNetCDF Function}
\usage{
writeNetCDF(ghgName, datect, proj, res, flux)
}
\arguments{
\item{ghgName}{Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".}

\item{datect}{A vector of timestamps in POSIXct format.}

\item{proj}{Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB, LonLat not implemented yet.}

\item{res}{Resolution for the gridded data, either 1, 20 or 100 km.  Defaults to "1km". Not yet implemented for LonLat.}

\item{flux}{a ukghg flux object}
}
\description{
This function writes netCDF output files
}
\examples{
startDate <- as.POSIXct(strptime("01/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
endDate   <- as.POSIXct(strptime("02/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
# create a sequence of dates
nTimes <- 2
datect <- seq(startDate, endDate, length = nTimes)
# calculate fluxes for these times
myFlux <- calcFlux("ch4", datect, proj = "OSGB", res = 20, "mol", "nano")
rf          <- writeNetCDF(myFlux)
}
\keyword{flux}
\keyword{internal}