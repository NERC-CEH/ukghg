## R script to build the ukghg package
##
## Peter Levy, CEH Edinburgh
## Bush Estate, Penicuik, EH26 0QB, U.K.
## plevy@ceh.ac.uk
## Tel: 0131 445 8556

rm(list=ls(all=TRUE))
library(knitr)
library("devtools")  # alternative is devtools::install_github("klutometis/roxygen")
library(roxygen2)
getwd()
setwd("..")
setwd("./ukghg")
#devtools::use_data(nSectors, sectorName, sectorLongName, alpha_year_byGHG_df, mod.yday, alpha_wday_df, mod.hour, overwrite = TRUE)
#devtools::use_data(ch4BySector, co2BySector, n2oBySector, internal = TRUE)

check_man()
document()
clean_vignettes()
build_vignettes()

# build the manual
#Sys.getenv("PKG_CONFIG_PATH")
#Sys.getenv(c("R_TEXI2DVICMD", "R_PAPERSIZE", "RD2PDF_INPUTENC"))
#Sys.setenv(RD2PDF_INPUTENC = "inputenx ")
pack <- "ukghg"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
#C:/PROGRA~1/R/R-32~1.4RE/bin/x64/R R CMD Rd2pdf --no-clean N:/0Peter/prop/UKinverseFlux/GHG_TAP/DelD/anthEmis/ukghg

#check()
build()
build(manual = FALSE, vignettes = FALSE)
build(binary = TRUE)

setwd("..")
install("ukghg")
install.packages("./ukghg_0.5.tar.gz", repos = NULL, type="source")
library(ukghg)
vignette("use_ukghg")
?ukghg
?calcFlux
?runShinyApp
runShinyApp()
?calcFlux_anthro
?calcFlux_bio
?combineFlux
?writeNetCDF
?writeGIF
startDate <- as.POSIXct(strptime("01/01/2014 12:00", "%d/%m/%Y %H:%M"), tz = "UTC")
endDate   <- as.POSIXct(strptime("01/12/2014 12:00", "%d/%m/%Y %H:%M"), tz = "UTC")
# create a sequence of dates
nTimes <- 12
datect <- seq(startDate, endDate, length = nTimes)
flux_100 <- calcFlux("ch4", datect, proj = "OSGB", res = "100", "mol", "nano", writeNetCDF = FALSE, writeGIF = FALSE, beta_df = beta_df)
system.time(flux_100 <- calcFlux("ch4", datect, proj = "OSGB", res = "100", "mol", "nano", writeNetCDF = FALSE, writeGIF = FALSE))
plot(datect, flux_100$total, col = "red", type = "b", ylim = c(0, 7))
flux_1   <- calcFlux("ch4", datect, proj = "OSGB", res = "1",   "mol", "nano", writeNetCDF = FALSE, writeGIF = FALSE)
flux_20  <- calcFlux("ch4", datect, proj = "OSGB", res = "20",  "mol", "nano", writeNetCDF = FALSE, writeGIF = FALSE)
myFlux <- calcFlux("ch4", datect, "OSGB", "mol", "nano", beta_df = beta_df, writeGIF = FALSE)
myFlux <- calcFlux("ch4", datect, "LonLat", "mol", "nano", writeNetCDF = TRUE, writeGIF = TRUE)
ls.str(flux_1)
ls.str(flux_20)
ls.str(flux_100)
str(myFlux, max.level = 1)
points(datect, flux_20$total, col = "blue", type = "b")
points(datect,  flux_1$total, col = "black", type = "b")
waste <- cellStats(flux_100$ls_ghgBySectorByTime[[9]], "sum")
plot(datect, waste)
plot(flux_1$total, flux_20$total)
plot(flux_20$s_ghgTotal)
plot(flux_100$ls_ghgByTimeBySector[[4]], zlim=c(0, 70))
sectorLongName <- c(sectorLongName, "Natural")
names(flux_20$ls_ghgByTimeBySector) <- sectorLongName
plot(flux_20$ls_ghgByTimeBySector[[4]])
names(flux_20$ls_ghgBySectorByTime[[10]]) <- datect
plot(flux_20$ls_ghgBySectorByTime[[10]])
plot(flux_100$ls_ghgByTimeBySector[[4]], zlim=c(0, 70))
