% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukghg.R
\name{calcFlux_bio}
\alias{calcFlux_bio}
\title{A low-level function for calculating a sequence of maps of biogenic GHG flux}
\usage{
calcFlux_bio(
  ghgName = c("ch4", "co2", "n2o", "c2h6", "voc"),
  datect = datect,
  proj = c("OSGB", "LonLat"),
  res = c("1", "20", "100"),
  unitType = c("mol", "g"),
  unitSIprefix = c("peta", "tera", "giga", "mega", "kilo", "none", "milli", "micro",
    "nano", "pico"),
  timeScales = c(TRUE, TRUE, TRUE, TRUE)
)
}
\arguments{
\item{ghgName}{Greenhouse gas: one of "ch4", "co2", "n2o", "c2h6" or "voc". Defaults to "ch4".}

\item{datect}{A vector of timestamps in POSIXct format.}

\item{proj}{Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB.}

\item{res}{Resolution for the gridded data, either 1, 20 or 100 km or 0.01 degrees for LonLat.  Defaults to "1km".}

\item{unitType}{Either molar ("mol") or mass-based ("g").}

\item{unitSIprefix}{Any standard SI prefix for the output units, from "peta" to "pico".}

\item{timeScales}{A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.}
}
\description{
This function calculates biopogenic greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
}
\examples{
startDate <- as.POSIXct(strptime("01/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
endDate   <- as.POSIXct(strptime("02/06/2006", "\%d/\%m/\%Y"), tz = "UTC")
# create a sequence of dates
nTimes <- 2
datect <- seq(startDate, endDate, length = nTimes)
myFlux <- calcFlux_bio("co2", datect, proj = "OSGB", res = "100", "mol", "micro")
plot(datect, myFlux$total)
}
\seealso{
\code{\link{calcFlux}}, the higher-level function which calls this.
}
\keyword{flux}
\keyword{internal}
