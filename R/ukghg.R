## ----ukghg_pkg, eval=TRUE------------------------------------------------
#' Generate maps of GHG fluxes for the UK.
#'
#' ukghg allows you to produce maps of GHG fluxes for the UK
#' and write these to netCDF files.
#'
#' The only function you're likely to need from \pkg{ukghg} is
#' \code{\link{calcFlux}}. Refer to the vignettes for details
#' of how to use it - use \code{vignette()}.
"_PACKAGE"
#> [1] "_PACKAGE"

## ----calc_alpha, eval=TRUE, results='asis', echo=TRUE, warning=TRUE, message=TRUE----
#' Calculate values of alpha, the coefficient of variation in time 
#'
#' This function calculates values of alpha, the time coefficient.
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param datect A vector of timestamps in POSIXct format.
#' @param sectorList A vector of sector numbers for which alpha values should be returned, e.g. c(1,3,7). Defaults to all.
#' @param timeScales A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.
#' @param beta_df A data frame of beta coefficients, which act as multipliers on each sector. Defaults to 1 for all, but This allows for parameter estimation by optimisation or MCMC.
#' @keywords internal alpha
#' @importFrom mgcv predict.gam
#' @export
#' @seealso \code{\link{calcFlux}} for the higher-level function which calls this.
#' @examples
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' # create a sequence of dates
#' nTimes <- 2
#' datect <- seq(startDate, endDate, length = nTimes)
#' alpha_df <- calcAlpha("ch4", datect)

calcAlpha <- function(ghgName = c("ch4", "co2", "n2o"), 
                datect, sectorList = 1:10, 
                             # year  yday  wday  hour 
                timeScales = c(TRUE, TRUE, TRUE, TRUE),
                beta_df = data.frame(sector = 1:10, # add default dataframe with beta = 1 for all
                    beta_year = rep(1, 10), 
                    beta_yday = rep(1, 10), 
                    beta_wday = rep(1, 10), 
                    beta_hour = rep(1, 10))){
  ghgName <- match.arg(ghgName)
  nTimes <- length(datect)
  iTime <- seq(1, length = nTimes)
  df <- data.frame(iTime, datect)
  # extract useful bits of timestamp
  df$datelt <- as.POSIXlt(df$datect, tz = "UTC")
  df$year <- as.numeric(df$datelt$year)+1900
  df$yday <- as.numeric(df$datelt$yday)
  df$wday <- as.numeric(df$datelt$wday)+1
  df$hour <- as.numeric(df$datelt$hour)
  # subset by ghgName
  alpha_year_df <- subset(alpha_year_byGHG_df, ghgName.ighg. == ghgName)
  df <- merge(df, alpha_year_df, by = c("year"))
  df <- merge(df, alpha_wday_df, by = c("wday", "sector"))

  df$alpha_yday  <- mgcv::predict.gam(mod.yday, df)
  df$alpha_hour  <- mgcv::predict.gam(mod.hour, df)
  # add beta values matching sector indices
  df$beta_year <- beta_df$beta_year[match(df$sector, beta_df$sector)]
  df$beta_yday <- beta_df$beta_yday[match(df$sector, beta_df$sector)]
  df$beta_wday <- beta_df$beta_wday[match(df$sector, beta_df$sector)]
  df$beta_hour <- beta_df$beta_hour[match(df$sector, beta_df$sector)]
  
  
  # if any time scales not being used set both alpha and beta to 1
  # assumes column order year  yday  wday  hour in beta_df
  if (any(!timeScales)) beta_df[,c(FALSE, !timeScales)] <- 1
  if (!timeScales[1]) df$alpha_year <- 1
  if (!timeScales[2]) df$alpha_yday <- 1
  if (!timeScales[3]) df$alpha_wday <- 1
  if (!timeScales[4]) df$alpha_hour <- 1
  
  # multiply alphas for each time scale with beta 
  df <- within(df, alpha <- alpha_year * beta_year *
                            alpha_yday * beta_yday *
                            alpha_wday * beta_wday *
                            alpha_hour * beta_hour)
  df$sectorName <- df$sectorName.x
  df <- with(df, data.frame(iTime, datect, sector, sectorName, alpha))
  # sort df by sector then date
  df <- df[with(df, order(sector, datect)), ]  
  # subset to just the requested sectors
  df <- subset(df, sector %in% sectorList)
  return(df)
}

## ----unit_conversions, eval=TRUE-----------------------------------------
#' A unit_conversion Function
#'
#' This function converts from Tg km-2 y-1 to a standard SI unit.
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param unitType Either molar ("mol") or mass-based ("g").
#' @param unitSIprefix Any standard SI prefix for the output units, from "kilo" to "pico".
#' @keywords internal units
#' @export
#' @seealso \code{\link{calcFlux}}, the higher-level function which calls this.
#' @examples
#' unit_conversion("ch4", "mol", "nano")
#' unit_conversion("co2", "mol", "micro")
#' unit_conversion("n2o", "mol", "nano")
#' unit_conversion("ch4", "g", "nano")

unit_conversion <- function(ghgName = c("ch4", "co2", "n2o"), 
                           unitType = c("mol", "g"),
                           unitSIprefix = c("kilo", "none", "milli", "micro", "nano", "pico")){
  ghgName <- match.arg(ghgName)
  unitType <- match.arg(unitType)
  unitSIprefix <- match.arg(unitSIprefix)
  
  secsPerYear <- 365*24*60*60
  km2_to_m2 <- 1e6
  Tg_to_g <- 1e12

  # molecular weight of gases
  if (ghgName == "ch4") {
      molWt <- 16
  } else if (ghgName == "co2") {
      molWt <- 44
  } else if (ghgName == "n2o") {
      molWt <- 44  
  }
  # ugly, but numerically correct
  if (unitType == "g") {
      molWt <- 1
  }

  # unit conversions - this could be outwith the function - stored as data
  #unitSIprefix <- "nano"
  SIprefix <- c("kilo", "none", "milli", "micro", "nano", "pico")
  SI_multiplier <- c(1e-3, 1, 1e3, 1e6, 1e9, 1e12)
  # match prefix with multiplier
  i <- match(unitSIprefix, SIprefix)
  mult <- SI_multiplier[i]
  
  # all gases in Tg
  value <- Tg_to_g *mult /molWt /km2_to_m2 /secsPerYear

  # return name as well
  name <- paste(SIprefix[i], unitType, ghgName, "m^-2_s^-1", sep="_")
  return(list(value=value, # unit conversion multiplier
               name=name))  # unit conversion name  
}

## ----calc_flux_predictions, eval=TRUE, results='asis', echo=TRUE, warning=TRUE, message=TRUE----
#' A high-level function for calculating flux maps
#'
#' This function calculates greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param datect A vector of timestamps in POSIXct format.
#' @param proj Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB.
#' @param res Resolution for the gridded data, either 1, 20 or 100 km.  Defaults to "1km". Not yet implemented for LonLat.
#' @param unitType Either molar ("mol") or mass-based ("g").
#' @param unitSIprefix Any standard SI prefix for the output units, from "kilo" to "pico".
#' @param writeNetCDF Write NetCDF output files. Defaults to FALSE.
#' @param sectorList A vector of sector numbers for which alpha values should be returned, e.g. c(1,3,7). Defaults to all.
#' @param includeBio A logical for whether biogenic fluxes should be calculated as well as anthropogenic sectors 1-10. Defaults to TRUE.
#' @param timeScales A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.
#' @param beta_df   A data frame of beta parameters, used in calibration of the model. Defaults to a dataframe with beta = 1 for all parameters.
#' @return total A vector of total flux
#' @return s_ghgTotal A RasterStack of total flux
#' @return ls_ghgByTimeBySector A list of RasterStacks of ghg fluxes where the z dimension corresponds to sector, one per timestep
#' @return ls_ghgBySectorByTime A list of RasterStacks of ghg fluxes where the z dimension corresponds to timestep, one per sector
#' @keywords units
#' @import raster
#' @export
#' @examples
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' nTimes <- 2
#' # create a sequence of timestamps
#' datect <- seq(startDate, endDate, length = nTimes)
#' # calculate fluxes for these times
#' myFlux <- calcFlux("ch4", datect, proj = "OSGB", res = "100" , "mol", "nano")

calcFlux <- function(ghgName = c("ch4", "co2", "n2o"), 
                     datect = datect, 
                     proj = c("OSGB", "LonLat"), 
                     res = c("1", "20", "100"), 
                     unitType = c("mol", "g"),
                     unitSIprefix = c("kilo", "none", "milli", "micro", "nano", "pico"),
                     writeNetCDF = FALSE,
                     sectorList = 1:10,
                     includeBio = TRUE,
                                  # year  yday  wday  hour 
                     timeScales = c(TRUE, TRUE, TRUE, TRUE),
                     beta_df = data.frame(sector = 1:10, # add default dataframe with beta = 1 for all
                        beta_year = rep(1, 10),
                        beta_yday = rep(1, 10),
                        beta_wday = rep(1, 10),
                        beta_hour = rep(1, 10))){
  ghgName <- match.arg(ghgName)
  proj <- match.arg(proj)
  res  <- match.arg(res)
  unitType <- match.arg(unitType)
  unitSIprefix <- match.arg(unitSIprefix) 
  
  flux <- calcFlux_anthro(ghgName, datect, proj, res, unitType, unitSIprefix, sectorList, timeScales, beta_df = beta_df)
  if (includeBio){
    flux_bio    <- calcFlux_bio   (ghgName, datect, proj, res, unitType, unitSIprefix, timeScales)
    flux    <- combineFlux(flux, flux_bio)
  }
  if (writeNetCDF == TRUE){writeNetCDF(ghgName, datect, proj, res, flux)}
  return(flux)
}

## ----calc_flux_anthro, eval=TRUE, results='asis', echo=TRUE, warning=TRUE, message=TRUE----
#' A calcFlux_anthro Function
#'
#' This function calculates anthropogenic greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param datect A vector of timestamps in POSIXct format.
#' @param proj Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB, LonLat not implemented yet.
#' @param res Resolution for the gridded data, either 1, 20 or 100 km.  Defaults to 1. Not yet implemented for LonLat.
#' @param unitType Either molar ("mol") or mass-based ("g").
#' @param unitSIprefix Any standard SI prefix for the output units, from "kilo" to "pico".
#' @param sectorList A vector of sector numbers for which alpha values should be returned, e.g. c(1,3,7). Defaults to all.
#' @param timeScales A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.
#' @param beta_df A data frame of beta coefficients, which act as multipliers on each sector. Defaults to 1 for all, but This allows for parameter estimation by optimisation or MCMC.
#' @keywords internal flux
#' @export
#' @seealso \code{\link{calcFlux}}, the higher-level function which calls this.
#' @examples
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' # create a sequence of dates
#' nTimes <- 2
#' datect <- seq(startDate, endDate, length = nTimes)
#' myFlux <- calcFlux_anthro("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")

calcFlux_anthro <- function(ghgName = c("ch4", "co2", "n2o"), 
                     datect = datect, 
                     proj = c("OSGB", "LonLat"), 
                     res = c("1", "20", "100"),                  
                     unitType = c("mol", "g"),
                     unitSIprefix = c("kilo", "none", "milli", "micro", "nano", "pico"), 
                     sectorList = 1:10,
                                  # year  yday  wday  hour 
                     timeScales = c(TRUE, TRUE, TRUE, TRUE),
                     beta_df = data.frame(sector = 1:10, # add default dataframe with beta = 1 for all
                         beta_year = rep(1, 10), 
                         beta_yday = rep(1, 10), 
                         beta_wday = rep(1, 10), 
                         beta_hour = rep(1, 10))){
  ghgName <- match.arg(ghgName)
  proj <- match.arg(proj)
  res  <- match.arg(res)
  res  <- as.numeric(res)
  unitType <- match.arg(unitType)
  unitSIprefix <- match.arg(unitSIprefix)
  
  # get the data frame of alpha values
  alpha_df <- calcAlpha(ghgName, datect, sectorList, timeScales, beta_df = beta_df)
  # declare an array for the total emission across sectors for each time
  nTimes <- length(datect)
  total <- array(dim = nTimes) 
  
  # depending which gas we want, read the appropriate data into stack
  # test version using local inst\extdata *not* installed package inst\extdata files
  ## comment out for package release version
  #path = "ukghg/inst/extdata/"
  #fname <- paste(path, ghgName, "BySector_", proj, "_", res, "km.grd", sep="")
  #ghgBySector <- stack(fname)

  fname <- paste(ghgName, "BySector_", proj, "_", res, "km.grd", sep="")
  ghgfile <- system.file("extdata", fname, package="ukghg")
  ghgBySector <- stack(ghgfile)
  
  unitConv <- unit_conversion(ghgName, unitType, unitSIprefix)
  
  r <- ghgBySector[[1]]
  # this works with stack, but is lost with brick
  r@data@unit <- unitConv$name
  
  # create a stack with nSectors layers
  s_ghgBySector <- brick(r, values=FALSE, nl=nSectors)
  s_ghgBySector  <- setValues(s_ghgBySector, 0)
  # create a stack with nTimes layers
  s_ghgTotal  <- brick(r, values=FALSE, nl=nTimes) 
  s_ghgTotal  <- setValues(s_ghgTotal, 0)
  # create a list of sector stacks, one for each time
  # this structure is used to sum over sectors at each time
  ls_ghgByTimeBySector <- replicate(nTimes, s_ghgBySector)
  # Create a list of time stacks, one for each sector
  # This structure is used to plot each sector at each time
  # This is needed because the syntax ls[[1:10]][[iSector]] doesn't work.
  # s_ghgTotal could be named s_ghgByTime, but is also used to store the totalling over sectors at each time
  ls_ghgBySectorByTime <- replicate(nSectors, s_ghgTotal)

  for (iRow in 1:(dim(alpha_df)[1])){
    #iRow <- 4
    iTime   <- alpha_df$iTime[iRow]
    iSector <- as.numeric(alpha_df$sector[iRow])
    ls_ghgByTimeBySector[[iTime]][[iSector]] <- 
      ghgBySector[[alpha_df$sector[iRow]]] * alpha_df$alpha[iRow]

    # put the same values in the other data structure
    ls_ghgBySectorByTime[[iSector]][[iTime]] <- 
    ls_ghgByTimeBySector[[iTime]][[iSector]]
  }

  for (iTime in 1:(nTimes)){
    # return a RasterLayer
    s_ghgTotal[[iTime]] <- sum(ls_ghgByTimeBySector[[iTime]], na.rm = TRUE)
    # return the sum
    total[iTime] <- cellStats(s_ghgTotal[[iTime]], "sum") * res^2  # so account for cell area in km2
    print          (cellStats(s_ghgTotal[[iTime]], "sum"))
  }
  
  # apply unit conversions
  s_ghgTotal <- s_ghgTotal * unitConv$value
  ls_ghgByTimeBySector <- lapply(ls_ghgByTimeBySector, function(x){x * unitConv$value})
  ls_ghgBySectorByTime <- lapply(ls_ghgBySectorByTime, function(x){x * unitConv$value})

  # set "units" attribute to returned objects
  attr(total, "units") <- "Tg y-1"
  attr(s_ghgTotal, "units") <- unitConv$name
  attr(ls_ghgByTimeBySector, "units") <- unitConv$name
  attr(ls_ghgBySectorByTime, "units") <- unitConv$name
  
  return(list(total=total, # vector of total emissions
    s_ghgTotal=s_ghgTotal, # stack of total emissions
    ls_ghgByTimeBySector=ls_ghgByTimeBySector,  # list of sector stacks of emissions, one per time
    ls_ghgBySectorByTime=ls_ghgBySectorByTime)) # list of time stacks of emissions, one per sector
}

## ----calc_flux_bio-------------------------------------------------------
#' A calcFlux_bio Function
#'
#' This function calculates biopogenic greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param datect A vector of timestamps in POSIXct format.
#' @param proj Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB, LonLat not implemented yet.
#' @param res Resolution for the gridded data, either 1, 20 or 100 km.  Defaults to "1km". Not yet implemented for LonLat.
#' @param unitType Either molar ("mol") or mass-based ("g").
#' @param unitSIprefix Any standard SI prefix for the output units, from "kilo" to "pico".
#' @param timeScales A vector of logicals for including variation at inter-annual, seasonal, intra-weekly, and diurnal time scales (i.e. the POSIXlt variables year, yday, wday, and hour. Defaults to TRUE for all four.
#' @keywords internal flux
#' @export
#' @seealso \code{\link{calcFlux}}, the higher-level function which calls this.
#' @examples
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' # create a sequence of dates
#' nTimes <- 2
#' datect <- seq(startDate, endDate, length = nTimes)
#' myFlux <- calcFlux_bio("co2", datect, proj = "OSGB", res = "100", "mol", "micro")
#' plot(datect, myFlux$total)

calcFlux_bio <- function(ghgName = c("ch4", "co2", "n2o"), 
                    datect = datect,
                    proj = c("OSGB", "LonLat"),
                    res = c("1", "20", "100"), 
                    unitType = c("mol", "g"),
                    unitSIprefix = c("kilo", "none", "milli", "micro", "nano", "pico"),
                                 # year  yday  wday  hour 
                    timeScales = c(TRUE, TRUE, TRUE, TRUE)){
  ghgName <- match.arg(ghgName)
  proj <- match.arg(proj)
  res  <- match.arg(res)
  res  <- as.numeric(res)
  unitType <- match.arg(unitType)
  unitSIprefix <- match.arg(unitSIprefix)
  
  nTimes <- length(datect)
  iTime <- seq(1, length = nTimes)
  df <- data.frame(iTime, datect)
  # declare an array (?vector) for the total biogenic flux for each time
  total <- array(dim = nTimes) 
  # extract useful bits of timestamp
  df$datelt <- as.POSIXlt(df$datect, tz = "UTC")
  df$yday <- as.numeric(df$datelt$yday)
  df$hour <- as.numeric(df$datelt$hour)
  
  # create stacks with nTimes layers
  # amplitude of diurnal cycle in NEE CO2
  fname <- paste("lai_", proj, "_", res, "km.grd", sep="")
  lai_file <- system.file("extdata", fname, package="ukghg")
  lai <- raster(lai_file)
  diurnalAmpli_yday  <- brick(lai, values=FALSE, nl=nTimes) 
  dailyMean_yday  <- diurnalAmpli_yday
  # CH4 is constant in time just now
  fname <- paste("Fch4_mean_Tgkm2y_", proj, "_", res, "km.grd", sep="")  
  ch4_bio_file <- system.file("extdata", fname, package="ukghg")
  Fch4_mean_Tgkm2y <- raster(ch4_bio_file)
  flux_ch4  <- brick(Fch4_mean_Tgkm2y, values=FALSE, nl=nTimes) 
  flux_ch4  <- setValues(flux_ch4, getValues(Fch4_mean_Tgkm2y))
  # N2O is just zero from natural land
  flux_n2o  <- setValues(flux_ch4, 0)
   
  # lai and LUE and ampli_min could by land class specific
  # need a table for Corine classes
  # LAI in chess ancil data seems static  
  LUE <- 1 # 4 # NEE/LAI, umol CO2 m-2 s-1 / m2 m-2
  ampli_min <- 0 # 1.2 # umol CO2 m-2 s-1
  offset <- 6 # hours
  for (i in 1:nTimes){
    #i <- 6
    # sinusoidal seasonal var in daily mean flux and diurnal amplitude
    diurnalAmpli_yday[[i]]   <-     lai*LUE*2 * sin(1*pi*(df$yday[i])/365) + ampli_min
    dailyMean_yday[[i]]  <-   -0.53*lai*LUE   * sin(2*pi*(df$yday[i]-68)/365)
    # gross NPP over summer 6 months = 300 g C m-2 from EB
    # convert to umol CO2 m-2 s-1
    #300 *1e6 /12 / (365*24*60*60/2)
    # gives mean uptake rate of 1.6 umol m-2 s-1
    # thf max amplitude of 3.2 over summer
    }
  # sinusoidal diurnal var in flux according to seasonally-variable amplitude, i.e.
  # sinusoidal seasonal plus diurnal var in flux 
  # variation at either time scale can be removed if timeScales 2 or 4 == FALSE
  # although yday variation in amplitude remains, even if daily mean == 0 constant
  flux_co2  <- dailyMean_yday * as.numeric(timeScales[2]) + 
     as.numeric(timeScales[4]) * diurnalAmpli_yday * sin(2*pi*(df$hour + offset)/24)
  
  ## need to sort out where to convert units
  ## add totalisier loop to flux_bio, 
  ## so outputs of both functions are the same and can be added?
  # co2 calculated in umol - need to convert to Tg km2 y for totalling
  unitConv <- unit_conversion("co2", "mol", "micro")
  flux_co2 <- flux_co2 / unitConv$value
  
  if (ghgName == "ch4") {
      s_ghg <- flux_ch4
  } else if (ghgName == "co2") {
      s_ghg <- flux_co2
  } else if (ghgName == "n2o") {
      s_ghg <- flux_n2o
  }  

  for (iTime in 1:(nTimes)){
    # return the sum
    total[iTime] <- cellStats(s_ghg[[iTime]], "sum") * res^2  # so account for cell area in km2
    print( cellStats(s_ghg[[iTime]], "sum") )
  }
  
  # apply unit conversion
  unitConv <- unit_conversion(ghgName, unitType, unitSIprefix)
  s_ghg <- s_ghg * unitConv$value
  s_ghg[is.na(s_ghg)] <- 0 # can't sum with missing values

  # set "units" attribute to returned objects
  attr(total, "units") <- "Tg y-1"
  attr(s_ghg, "units") <- unitConv$name
  s_ghg[is.na(s_ghg)] <- 0 # can't sum with missing values

  return(list(total=total, # vector of total emissions
    s_ghg=s_ghg)) # stack of total emissions
}

## ----combine-------------------------------------------------------------
#' A combineFlux Function
#'
#' This function combines biogenic and anthropogenic greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
#' @param flux_anthro anthropogenic greenhouse gas fluxes
#' @param flux_bio biogenic greenhouse gas fluxes
#' @keywords internal flux
#' @export
#' @seealso \code{\link{calcFlux}}, the higher-level function which calls this.
#' @examples
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' # create a sequence of dates
#' nTimes <- 2
#' datect <- seq(startDate, endDate, length = nTimes)
#' flux_anthro <- calcFlux_anthro("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")
#' flux_bio    <- calcFlux_bio   ("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")
#' flux_all    <- combineFlux(flux_anthro, flux_bio)

combineFlux <- function(flux_anthro, flux_bio){
  total <- flux_anthro$total + flux_bio$total
  # use the flux_anthro lists, and add bio flux to these
  s_ghgTotal           <- flux_anthro$s_ghgTotal
  ls_ghgByTimeBySector <- flux_anthro$ls_ghgByTimeBySector
  ls_ghgBySectorByTime <- flux_anthro$ls_ghgBySectorByTime
  
  nSectors <- length(ls_ghgBySectorByTime)
  nTimes   <- length(ls_ghgByTimeBySector)
  # add the bio flux stack to the list of sectors
  ls_ghgBySectorByTime[[nSectors+1]] <- flux_bio$s_ghg
  
  for (iTime in 1:(nTimes)){
    # add a bio flux raster to the stack of sectors
    ls_ghgByTimeBySector[[iTime]] <- 
    addLayer(ls_ghgByTimeBySector[[iTime]], flux_bio$s_ghg[[iTime]])
    # return a RasterLayer for the sum across sectors
    #s_ghgTotal[[iTime]] <- sum(ls_ghgByTimeBySector[[iTime]])
    s_ghgTotal[[iTime]] <- flux_anthro$s_ghgTotal[[iTime]] + flux_bio$s_ghg[[iTime]]
  }
  
  return(list(total=total, # vector of total emissions
  s_ghgTotal=s_ghgTotal, # stack of total emissions
  ls_ghgByTimeBySector=ls_ghgByTimeBySector,  # list of sector stacks of emissions, one per time
  ls_ghgBySectorByTime=ls_ghgBySectorByTime)) # list of time stacks of emissions, one per sector
}

## ----writeNetCDF---------------------------------------------------------
#' A writeNetCDF Function
#'
#' This function writes netCDF output files
#' @param ghgName Greenhouse gas: one of "ch4", "co2", or "n2o". Defaults to "ch4".
#' @param datect A vector of timestamps in POSIXct format.
#' @param proj Geographic projection for the gridded data, either "OSGB" or "LonLat".  Defaults to OSGB, LonLat not implemented yet.
#' @param res Resolution for the gridded data, either 1, 20 or 100 km.  Defaults to "1km". Not yet implemented for LonLat.
#' @param flux a ukghg flux object
#' @keywords internal flux
#' @export
#' @examples
#' \dontrun{
#' startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
#' endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
#' # create a sequence of dates
#' nTimes <- 2
#' datect <- seq(startDate, endDate, length = nTimes)
#' # calculate fluxes for these times
#' myFlux <- calcFlux("ch4", datect, proj = "OSGB", res = "100", "mol", "nano")
#' rf <- writeNetCDF("ch4", datect, proj = "OSGB", res = "100", myFlux)
#' }

writeNetCDF <- function(ghgName, datect, proj, res, flux){
  fname <- paste("uk_flux_total_", ghgName, "_", proj, "_", res, "km.nc", sep="")  
  vname <- paste(ghgName, "_flux", sep="")  
  lvname <- paste("Total", ghgName, "flux across sectors", sep=" ")  
  timename <- paste("Time starting", datect[1])
  timeunit <- difftime(datect[2], datect[1], units="days") # assumes regular intervals
  timeunit <- paste(timeunit, "days")
  
  # write total to file
  rf <-  writeRaster(flux$s_ghgTotal, 
    filename = fname, format="CDF", 
    varname = vname, 
    varunit = attr(flux$s_ghgTotal, "units"), 
    longname = lvname,
    zname = timename, 
    zunit = timeunit,
    overwrite=TRUE)   
    
  #nc <- nc_open(filename=fname)

  nSectors <- length(flux$ls_ghgBySectorByTime)
  if (nSectors == 11) sectorName <- c(sectorName, "natural")

  # write output to file by sector
  for (iSector in 1:nSectors){
    fname <- paste("uk_flux_", sectorName[iSector], "_", ghgName, "_", proj, "_", res, "km.nc", sep="")  
    lvname <- paste(ghgName, "flux from", sectorName[iSector], sep=" ")  
    # write total to file
    rf <-  writeRaster(flux$ls_ghgBySectorByTime[[iSector]], 
    filename = fname, format="CDF", 
    varname = vname, 
    varunit = attr(flux$ls_ghgBySectorByTime, "units"), 
    longname = lvname,
    zname = timename, 
    zunit = timeunit,
    overwrite=TRUE)       
  }
  return(print("NetCDF output files written"))
}

## ----runShinyApp----
#' Launches the shiny app for the ukghg package
#'
#' This function provides a web browser interface to calculate greenhouse gas fluxes from the UK, based on a spatio-temporal model and the national GHG inventory data.
#' @keywords shiny app
#' @return shiny application object
#' @export
#' @example \dontrun {runShinyApp()}
#' @import shiny

runShinyApp <- function() {
  appDir <- system.file("shinyApp", "ukghg-app", package = "ukghg")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `ukghg`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}