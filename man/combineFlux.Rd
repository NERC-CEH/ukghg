% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukghg.R
\name{combineFlux}
\alias{combineFlux}
\title{A function to combine map sequences of biogenic and anthropogenic fluxes}
\usage{
combineFlux(flux_anthro, flux_bio)
}
\arguments{
\item{flux_anthro}{anthropogenic greenhouse gas fluxes}

\item{flux_bio}{biogenic greenhouse gas fluxes}
}
\description{
This function combines biogenic and anthropogenic greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
}
\examples{
startDate <- as.POSIXct(strptime("01/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
endDate   <- as.POSIXct(strptime("02/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
# create a sequence of dates
nTimes <- 2
datect <- seq(startDate, endDate, length = nTimes)
flux_anthro <- calcFlux_anthro("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")
flux_bio    <- calcFlux_bio   ("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")
flux_all    <- combineFlux(flux_anthro, flux_bio)
}
\seealso{
\code{\link{calcFlux}}, the higher-level function which calls this.
}
\keyword{flux}
\keyword{internal}
