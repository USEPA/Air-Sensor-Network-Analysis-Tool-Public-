
repository <- "http://cran.us.r-project.org"

if (!require(Rcpp)) install.packages("Rcpp", repos = repository)
if (!require(httr)) install.packages("httr", repos = repository)
if (!require(shiny)) install.packages("shiny", repos = repository)
if (!require(shinyBS)) install.packages("shinyBS", repos = repository)
if (!require(shinyjs)) install.packages("shinyjs", repos = repository)
if (!require(DT)) install.packages("DT", repos = repository)
if (!require(jsonlite)) install.packages("jsonlite", repos = repository)
if (!require(leaflet)) install.packages("leaflet", repos = repository)
if (!require(mapview)) install.packages("mapview", repos = repository)
if (!require(MazamaSpatialUtils)) install.packages("MazamaSpatialUtils", repos = repository)
if (!require(plotly)) install.packages("plotly", repos = repository)
if (!require(reshape2)) install.packages("reshape2", repos = repository)
if (!require(webshot)) install.packages("webshot", repos = repository)
if (!require(zip)) install.packages("zip", repos = repository)

if (!require(leaflet.providers)) install.packages("leaflet.providers", repos = repository)
if (!require(dplyr)) install.packages("dplyr", repos = repository)
if (!require(ggExtra)) install.packages("ggExtra", repos = repository)
if (!require(ggTimeSeries)) install.packages("ggTimeSeries", repos = repository)
if (!require(ggTimeSeries)) install.packages("ggTimeSeries", repos = repository)
if (!require(ggplot2)) install.packages("ggplot2", repos = repository)
if (!require(seismicRoll)) install.packages("seismicRoll", repos = repository)
if (!require(leaflet.providers)) install.packages("leaflet.providers", repos = repository)
if (!require(lubridate)) install.packages("lubridate", repos = repository)
if (!require(mgcv)) install.packages("mgcv", repos = repository)
if (!require(openair)) install.packages("openair", repos = repository)
if (!require(scales)) install.packages("scales", repos = repository)
if (!require(sf)) install.packages("sf", repos = repository)
if (!require(shinybusy)) install.packages("shinybusy", repos = repository)
if (!require(stringr)) install.packages("stringr", repos = repository)
if (!require(tidyr)) install.packages("tidyr", repos = repository)
if (!require(zoo)) install.packages("zoo", repos = repository)

cat("\n\nTesting that the Rtools C++ compiler works:\n")

Rcpp::sourceCpp(code = "
#include <Rcpp.h>
// [[Rcpp::export]]
bool test_cpp_works() {return true;}")

stopifnot(test_cpp_works())

# Load data frames: USCensusStates, USCensusCounties, USIndianLands. EXPENSIVE.
# NOTE: Per email on 2025-07-01 from jonathan.s.callahan@gmail.com
# MazamaSpatialUtils version 0.8 does not contain USIndianLands (not a priority)
# and the structure of these spatial datasets is different from the previous
# version 0.7 which contained USIndianLands.

cat("\n\nTesting that the MazamaSpatialUtils datasets can be loaded:\n\n")

if (dir.exists("data/tmp")) {
  unlink("data/tmp/*.rda")
} else {
  dir.create("data/tmp")
}

MazamaSpatialUtils::setSpatialDataDir("data/tmp")
MazamaSpatialUtils::installSpatialData("USCensusStates")
stopifnot(file.exists("data/tmp/USCensusStates.rda"))
stopifnot(file.exists("data/tmp/USCensusStates_01.rda"))
MazamaSpatialUtils::installSpatialData("USCensusCounties")
stopifnot(file.exists("data/tmp/USCensusCounties.rda"))
stopifnot(file.exists("data/tmp/USCensusCounties_01.rda"))
#MazamaSpatialUtils::installSpatialData("USIndianLands")
#stopifnot(!file.exists("data/tmp/USIndianLands.rda"))
#stopifnot(!file.exists("data/tmp/USIndianLands_01.rda"))
MazamaSpatialUtils::loadSpatialData("USCensusStates")
stopifnot(exists("USCensusStates"))
stopifnot(exists("USCensusStates_01"))
MazamaSpatialUtils::loadSpatialData("USCensusCounties")
stopifnot(exists("USCensusCounties"))
stopifnot(exists("USCensusCounties_01"))
#MazamaSpatialUtils::loadSpatialData("USIndianLands")
#stopifnot(exists(USIndianLands))
#stopifnot(exists(USIndianLands_01))

cat("\n\nDone.\nYou can now run ASNAT\n\n")

