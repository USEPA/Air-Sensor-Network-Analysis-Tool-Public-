
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
if (!require(webshot)) install.packages("webshot", repos = repository)
if (!require(plotly)) install.packages("plotly", repos = repository)
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
if (!require(openair)) install.packages("openair", repos = repository)
if (!require(scales)) install.packages("scales", repos = repository)
if (!require(sf)) install.packages("sf", repos = repository)
if (!require(shinybusy)) install.packages("shinybusy", repos = repository)
if (!require(stringr)) install.packages("stringr", repos = repository)
if (!require(tidyr)) install.packages("tidyr", repos = repository)
if (!require(zoo)) install.packages("zoo", repos = repository)

cat("\n\nTesting that the Rtools C++ compiler works:\n")

Rcpp::sourceCpp(code = '
#include <Rcpp.h>
// [[Rcpp::export]]
bool test_cpp_works() {return true;}')

stopifnot(test_cpp_works())

cat("\n\nDone.\nYou can now run ASNAT\n")
