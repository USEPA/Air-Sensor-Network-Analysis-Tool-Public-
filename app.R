###############################################################################
# PURPOSE: app.R - ASNAT Controller/View.
#
# NOTES:  To run: ASNAT
#
#         Uses the Model-View/Controller (MVC) Design Pattern.
#         https://en.wikipedia.org/wiki/Model-view-controller
#         The Model is ASNAT_Model.R
#           (which has-a ASNAT_Dataset_Manager which has-a ASNAT_Dataset).
#         This file, app.R contains both
#           the Controller (ui function)
#             which defines the GUI that updates the Model state and
#           the View (server function) which displays the Model state/data.
#         Uses RShiny framework to implement the View and Controller.
#
#         The phantomjs, pandoc, ffmpeg and curl platform-specific
#         (Mac, Windows, Linux) executables will be deployed with the ASNAT
#         script.
#         These external programs are invoked to generate output image and
#         movie files of the map view. The curl program is used to retrieve
#         data from webservices.
#         Using the curl program is more robust than using httr::GET() -
#         which can cause the process to terminate upon failure.
#
# HISTORY: 2022-10-30 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

###############################################################################
# Load required source files:
###############################################################################

source("ASNAT_Utilities.R")
source("ASNAT_Dataset.R")
source("ASNAT_DatasetManager.R")
source("ASNAT_Model.R")

###############################################################################
# Load required libraries:
###############################################################################

if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(shinybusy)
  library(DT)
  library(leaflet)
  library(mapview)
  library(webshot)
  library(plotly)
  library(reshape2)
  library(zip)
  library(zoo)
  library(dplyr)
  library(seismicRoll)
  library(ggplot2)
  library(leaflet.providers)
  library(lubridate)
  library(openair)
  library(ggTimeSeries)
  library(ggExtra)
  library(sf)
  library(stringr)
  library(tidyr)
  library(scales)
} else {

  # URL to download any required R packages from:

  repository <- "http://cran.us.r-project.org"

  if (!require(shiny)) install.packages("shiny", repos = repository)
  if (!require(shinyBS)) install.packages("shinyBS", repos = repository)
  # https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R
  if (!require(shinyjs)) install.packages("shinyjs", repos = repository)
  if (!require(shinybusy)) install.packages("shinybusy", repos = repository)
  if (!require(DT)) install.packages("DT", repos = repository)
  if (!require(leaflet)) install.packages("leaflet", repos = repository)
  if (!require(mapview)) install.packages("mapview", repos = repository)
  if (!require(webshot)) install.packages("webshot", repos = repository)
  if (!require(plotly)) install.packages("plotly", repos = repository)
  if (!require(reshape2)) install.packages("reshape2", repos = repository)
  if (!require(zip)) install.packages("zip", repos = repository)
  if (!require(ggplot2)) install.packages("ggplot2", repos = repository)
  if (!require(dplyr)) install.packages("dplyr", repos = repository)
  if (!require(seismicRoll)) install.packages("seismicRoll", repos = repository)
  if (!require(zoo)) install.packages("zoo", repos = repository)
  if (!require(leaflet.providers)) install.packages("leaflet.providers", repos = repository)
  if (!require(lubridate)) install.packages("lubridate", repos = repository)
  if (!require(openair)) install.packages("openair", repos = repository)
  if (!require(ggTimeSeries)) install.packages("ggTimeSeries", repos = repository)
  if (!require(ggExtra)) install.packages("ggExtra", repos = repository)
  if (!require(sf)) install.packages("sf", repos = repository)
  if (!require(stringr)) install.packages("stringr", repos = repository)
  if (!require(tidyr)) install.packages("tidyr", repos = repository)
  if (!require(scales)) install.packages("scales", repos = repository)
  # webshot::install_phantomjs()
}


###############################################################################
# Initialize global constants:
###############################################################################

user_manual_url <-
  paste0(rsigserver_url, "asnat/download/ASNAT_User_Manual.pdf")

purple_air_attribution_url <- "https://www2.purpleair.com/pages/attribution"

aqs_parameter_codes_url <-
  "https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html"

all_purple_air_sites <- "0 All sensors in view"
all_dataset_x_neighbor_sites <- "0 All Sites"

# FIX: Sizes other than 992 x 744 make the saved image unmatched!
the_map_width <- "992px"
the_map_height <- "744px"

the_site_neighbor_map_width <- "496px"
the_site_neighbor_map_height <- "372px"

# Plot width/height are in inches:
the_pdf_width <- 8L
the_pdf_height <- 6L
the_pdf_point_size <- 9L

# List of variables|units|description available from rsigserver webservice:

webservice_variable_metadata <- list(
  "AirNow.pm25|ug/m3|UTC hourly mean surface measured particulate matter (aerosols) 2.5 microns or smaller in diameter in micrograms per cubic meter.",
  "AirNow.pm10|ug/m3|UTC hourly mean surface measured particulate matter (aerosols) 10 microns or smaller in diameter in micrograms per cubic meter.",
  "AirNow.rh|%|UTC hourly mean surface measured relative humidity percentage.",
  "AirNow.temperature|C|UTC hourly mean surface measured air temperature (C)",
  "AirNow.pressure|C|UTC hourly mean surface measured atmospheric pressure (hPa)",
  "AirNow.ozone|ppb|UTC hourly mean surface measured ozone (ppb).",
  "AirNow.no2|ppb|UTC hourly mean surface measured nitrogen dioxide (ppb).",
  "AirNow.co|ppm|UTC hourly mean surface measured carbon monoxide (ppm).",
  "AirNow.so2|ppb|UTC hourly mean surface measured sulfer dioxide (ppb).",
  "AQS.pm25|ug/m3|UTC hourly mean surface measured particulate matter (aerosols) 2.5 microns or smaller in diameter in micrograms per cubic meter.",
  "AQS.pm10|ug/m3|UTC hourly mean surface measured particulate matter (aerosols) 10 microns or smaller in diameter in micrograms per cubic meter.",
  "AQS.rh|%|UTC hourly mean surface measured relative humidity in percent.",
  "AQS.temperature|C|UTC hourly mean surface measured air temperature (C)",
  "AQS.pressure|C|UTC hourly mean surface measured atmospheric pressure (hPa)",
  "AQS.ozone|ppb|UTC hourly mean surface measured ozone (ppb).",
  "AQS.no2|ppb|UTC hourly mean surface measured nitrogen dioxide (ppb).",
  "AQS.co|ppm|UTC hourly mean surface measured carbon monoxide (ppm).",
  "AQS.so2|ppb|UTC hourly mean surface measured sulfer dioxide (ppb).",
  "METAR.relativeHumidity|%|Relative humidity (computed using Magnus approximation formula from temperature and dewPoint).",
  "METAR.temperature|C|Air temperature in degrees C.",
  "METAR.seaLevelPress|hPa|Atmospheric pressure equivalent at sea level in hPa.",
  "PurpleAir.pm25_corrected|ug/m3|Scaled humidity-corrected particulate matter not more than 2.5 microns in diameter.",
  "PurpleAir.humidity|%|Sensor measured relative humidity inside the sensor housing (on average 4% lower than ambient conditions).",
  "PurpleAir.temperature|C|Temperature inside of the sensor housing. On average, this is 4C higher than ambient conditions.",
  "=== Slow Variables Below ===|-|Retrieving the following variables will be much slower.",
  "PurpleAir.ozone1|ppb|Sensor measured ozone concentration.",
  "PurpleAir.pm1|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, average for channel A and B but excluding downgraded channels and using CF=1 variant for indoor, ATM variant for outdoor devices.",
  "PurpleAir.pm1_a|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, channel A CF=1 variant for indoor, ATM variant for outdoor devices.'",
  "PurpleAir.pm1_b|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, channel B CF=1 variant for indoor, ATM variant for outdoor devices.'",
  "PurpleAir.pm1_atm|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, ATM variant average for channel A and B but excluding downgraded channels.'",
  "PurpleAir.pm1_atm_a|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, channel A ATM variant.'",
  "PurpleAir.pm1_atm_b|ug/m3|Sensor measured particulate matter not more than 1 micron in diameter, channel B ATM variant.'",
  "PurpleAir.pm25|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, average for channel A and B but excluding downgraded channels and using CF=1 variant for indoor, ATM variant for outdoor devices.",
  "PurpleAir.pm25_a|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel A CF=1 variant for indoor, ATM variant for outdoor devices.'",
  "PurpleAir.pm25_b|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel B CF=1 variant for indoor, ATM variant for outdoor devices.'",
  "PurpleAir.pm25_atm|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, ATM variant average for channel A and B but excluding downgraded channels.'",
  "PurpleAir.pm25_atm_a|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel A ATM variant.'",
  "PurpleAir.pm25_atm_b|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel B ATM variant.'",
  "PurpleAir.pm25_cf_1|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, CF=1 variant average for channel A and B but excluding downgraded channels.'",
  "PurpleAir.pm25_cf_1_a|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel A CF=1 variant.'",
  "PurpleAir.pm25_cf_1_b|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, channel B CF=1 variant.'",
  "PurpleAir.pm25_10minute|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 10 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, average for channel A and B but excluding downgraded channels.",
  "PurpleAir.pm25_10minute_a|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 10 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, channel A running average.",
  "PurpleAir.pm25_10minute_b|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 10 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, channel B running average.",
  "PurpleAir.pm25_60minute|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 60 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, average for channel A and B but excluding downgraded channels.",
  "PurpleAir.pm25_60minute_a|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 60 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, channel A running average.",
  "PurpleAir.pm25_60minute_b|ug/m3|Sensor measured particulate matter not more than 2.5 microns in diameter, 60 minute pseudo average using CF=1 data variant for indoor and ATM variant for outdoor, channel B running average.",
  "PurpleAir.pm10|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, average for channel A and B but excluding downgraded channels and using CF=1 variant for indoor, ATM variant for outdoor devices.",
  "PurpleAir.pm10_a|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel A CF=1 variant for indoor, ATM variant for outdoor devices.",
  "PurpleAir.pm10_b|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel B CF=1 variant for indoor, ATM variant for outdoor devices.",
  "PurpleAir.pm10_atm|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, ATM variant average for channel A and B but excluding downgraded channels.",
  "PurpleAir.pm10_atm_a|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel A ATM variant.",
  "PurpleAir.pm10_atm_b|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel B ATM variant.",
  "PurpleAir.pm10_cf_1|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, CF=1 variant average for channel A and B but excluding downgraded channels.'",
  "PurpleAir.pm10_cf_1_a|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel A CF=1 variant.'",
  "PurpleAir.pm10_cf_1_b|ug/m3|Sensor measured particulate matter not more than 10 microns in diameter, channel B CF=1 variant.'",
  "PurpleAir.0_3_um_count|particles/100ml|Count concentration of all particles greater than 0.3 microns in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.0_3_um_count_a|particles/100ml|Count concentration of all particles greater than 0.3 microns in diameter, channel A.",
  "PurpleAir.0_3_um_count_b|particles/100ml|Count concentration of all particles greater than 0.3 microns in diameter, channel B.",
  "PurpleAir.0_5_um_count|particles/100ml|Count concentration of all particles greater than 0.5 microns in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.0_5_um_count_a|particles/100ml|Count concentration of all particles greater than 0.5 microns in diameter, channel A.",
  "PurpleAir.0_5_um_count_b|particles/100ml|Count concentration of all particles greater than 0.5 microns in diameter, channel B.",
  "PurpleAir.1_um_count|particles/100ml|Count concentration of all particles greater than 1 micron in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.1_um_count_a|particles/100ml|Count concentration of all particles greater than 1 micron in diameter, channel A.",
  "PurpleAir.1_um_count_b|particles/100ml|Count concentration of all particles greater than 1 micron in diameter, channel B.",
  "PurpleAir.2_5_um_count|particles/100ml|Count concentration of all particles greater than 2.5 microns in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.2_5_um_count_a|particles/100ml|Count concentration of all particles greater than 2.5 microns in diameter, channel A.",
  "PurpleAir.2_5_um_count_b|particles/100ml|Count concentration of all particles greater than 2.5 microns in diameter, channel B.",
  "PurpleAir.5_um_count|particles/100ml|Count concentration of all particles greater than 5 microns in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.5_um_count_a|particles/100ml|Count concentration of all particles greater than 5 microns in diameter, channel A.",
  "PurpleAir.5_um_count_b|particles/100ml|Count concentration of all particles greater than 5 microns in diameter, channel B.",
  "PurpleAir.10_um_count|particles/100ml|Count concentration of all particles greater than 10 microns in diameter, average particle count for channel A and B but excluding downgraded channels.",
  "PurpleAir.10_um_count_a|particles/100ml|Count concentration of all particles greater than 10 microns in diameter, channel A.",
  "PurpleAir.10_um_count_b|particles/100ml|Count concentration of all particles greater than 10 microns in diameter, channel B.",
  "PurpleAir.pressure|hPa|Sensor measured atmospheric pressure.",
  "PurpleAir.voc|IAQ|Sensor measured volatile organic compounds in Bosch static IAQ units as per BME680 spec sheet (EXPERIMENTAL).",
  "" # End of list.
)

# Construct lists used for menus and tooltips:

webservice_variables <- list()
webservice_units <- list()
webservice_descriptions <- list()

initialize_variable_lists <- function() {
  count <- 0L

  for (entry in webservice_variable_metadata) {

    if (entry != "") {
      parts <- unlist(strsplit(entry, "|", fixed = TRUE))
      stopifnot(length(parts) == 3L)
      count <- count + 1L
      webservice_variables[count] <<- parts[[1L]]
      webservice_units[count] <<- parts[[2L]]
      webservice_descriptions[count] <<- parts[[3L]]
    }
  }
}

initialize_variable_lists()



# Declare default MVC Model Singleton.
#
# Use case 1 - running the app locally, either by using ASNAT shell program or
# in RStudio on the user's laptop:
# 1. This 'default_model' object will be initialized from reading the user's
#    .asnat file (if it exists).
#    For example, the map would be zoomed to the same viewing area as last time
#    the user ran this app (e.g., Boston) rather than the default viewing area
#    (NYC).
# 2. Inside server() the 'model' object will refer to (alias) 'default_model'.
#
# Use case 2 - running remote-hosted, e.g., rstudio-connect as client-server:
# 1. There is no .asnat file so this 'default_model' object will be initialized
#    to 'standard' state (NYC, etc.).
# 2. Inside server() the 'model' object will be independent of 'default_model'
#    so that each client user will have their own 'session' and their own
#    'model'. Additionally, to avoid having multiple concurrent users overwrite
#    each others temporary files, the unique session$token will be used in the
#    created temporary data directory used to hold data streamed from
#    webservices and for temporary computed results such as tables.

default_model <- ASNAT_Model()



################ Globals and functions for map legend colormaps ###############


# Helper function to make a color array used for the map legend.
# my_colormap <- make_colormap(c(68, 72), c(13, 21), c(84, 104))

make_colormap <- function(reds, greens, blues) {
  stopifnot(length(reds) > 0L)
  stopifnot(length(greens) == length(reds))
  stopifnot(length(blues) == length(reds))
  result <- rep(0L, length(reds))

  for (index in seq_along(reds)) {
    result[index] <-
      rgb(reds[index], greens[index], blues[index], maxColorValue = 255L)
  }

  return(result)
}



# Legend colormaps are ordered from lowest value to highest value.
# They have only about a half dozen discernable hues.
# Workaround R/Leaflet BUG: "green" was replaced with "#00FF00" otherwise
# "green" appears as "green4" (dark green) in the legend.

the_default_colormap <-
  c("black", "blue", "cyan", "#00FF00", "yellow", "red", "magenta")

# The viridis colormap is supposedly discernable by color-impaired viewers.
# The viridis rgb values are from RSIG3D file RGBColormap.cpp.

the_viridis_reds <-
##c(68,  72,  72,  69,  63,  57,  50,  45,  39,  35,  31,  32,  41,  60,  86, 116, 148, 184, 220, 253)
  c(0L, 72L, 50L, 31L, 86L, 220L)
the_viridis_greens <-
##c(13,  21,  38,  55,  71,  85, 100, 112, 125, 138, 150, 163, 175, 188, 198, 208, 216, 222, 227, 231)
  c(0L, 38L, 100L, 150L, 198L, 227L)
the_viridis_blues <-
##c(84, 104, 119, 129, 136, 140, 142, 142, 142, 141, 139, 134, 127, 117, 103,  85,  64,  41,  23,  37)
  c(0L, 119L, 142L, 139L, 103L,  23L)

the_viridis_colormap <-
  make_colormap(the_viridis_reds, the_viridis_greens, the_viridis_blues)

# https://www.codeofcolors.com/web-safe-colors.html
# retigo/stable/images/conc_symbols_colorsafe_inverted/conc_*.png

the_colorsafe_reds <-
##c(127, 255, 255, 249, 240, 230, 220, 210, 164, 142, 117, 107,  95,  81,  64,  39,  17 )
  c(230L, 255L, 240L, 210L, 142L, 107L,  64L)

the_colorsafe_greens <-
##c(127, 254, 252, 252, 241, 231, 220, 210, 150, 124,  99,  86,  71,  57,  41,  22,   9 )
  c(230L, 252L, 241L, 210L, 124L,  86L,  41L)

the_colorsafe_blues <-
##c(127, 203, 152, 101, 104, 108, 110, 113, 184, 192, 199, 201, 202, 204, 206, 207, 130 )
  c(230L, 152L, 104L, 113L, 192L, 201L, 206L)

the_ramp_down <- seq(253L, 0L, by = -40L)
#  { 255, 255, 255, 255, 255, 255, 255, 255, 255, 244, 232, 221, 209, 197, 186, 174, 162, 151, 139, 128  }  // blue
the_ramp_half_down <- c(255L, 255L, 255L, 232L, 186L, 139L, 128L)

the_gray_colormap <- make_colormap(the_ramp_down, the_ramp_down, the_ramp_down)
the_blue_colormap <-
  make_colormap(the_ramp_down, the_ramp_down, the_ramp_half_down)
the_colorsafe_colormap <-
  make_colormap(the_colorsafe_reds, the_colorsafe_greens, the_colorsafe_blues)



################# Globals and functions for colored map glyphs ################


# Constants:

glyph_size <- 40L
glyph_scale <- 2L
glyph_border_color <- "black"


# glyph pch codes: https://r-lang.com/pch-in-r/

the_glyphs <- list(filled_triangle_pointing_down = 25L,
                   filled_diamond = 23L,
                   filled_square = 22L,
                   filled_triangle_pointing_up = 24L,
                   filled_circle = 21L)


# Define function that returns the integer pch (point character) map glyph
# for a given coverage (source.variable).
#
# Note the choice of shape is not arbitrary but rather based on stability.
# A square is a stable shape so it is used to depict the most stable data: AQS.
# A diamond is unstable but when it falls over it becomes a stable square
# which is what becomes of AirNow data - it becomes AQS data (after extensive
# QA revisions).
# A triangle pointing up is stable and is like an airplane so it depicts METAR
# (mostly airport) data.
# A circle is unstable as is questionable PurpleAir measurements.
# A downward-pointing triangle is unstable but might become stable so it
# depicts other data read from files.

map_glyph <- function(coverage) {
  parts <- unlist(strsplit(coverage, ".", fixed = TRUE))
  data_source <- parts[[1L]]
  result <- the_glyphs$filled_triangle_pointing_down

  if (data_source == "AirNow") {
    result <- the_glyphs$filled_diamond
  } else if (data_source == "AQS") {
    result <- the_glyphs$filled_square
  } else if (data_source == "METAR") {
    result <- the_glyphs$filled_triangle_pointing_up
  } else if (data_source == "PurpleAir") {
    result <- the_glyphs$filled_circle
  }

  return(result)
}



# Define a function that generates a glyph png file name:

glyph_file_name <- function(glyph, color) {
  directory <- "data/glyphs"

  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  result <- paste0(directory, "/", glyph, "_", color, ".png")
  return(result)
}



# Function that generates glyph png files used to display point data on the map:
# https://www.appsloveworld.com/r/100/333/r-shiny-leaflet-add-crosses-for-points

create_glyph_files <- function(glyphs, colormap) {
  result <- c()
  index <- 1L

  for (glyph in glyphs) {

    for (color in colormap) {
      file_name <- glyph_file_name(glyph, color)

      if (!file.exists(file_name)) {
        png(file_name, width = glyph_size, height = glyph_size,
            bg = "transparent")
        par(mar = c(0L, 0L, 0L, 0L))
        plot.new()

        # Must use bg = color so down-pointing triangle is filled:

        points(0.5, 0.5, pch = glyph, col = glyph_border_color,
               cex = glyph_scale, bg = color)
        dev.off() # Close the plot and (magically) write png file.
      }

      result[index] <- file_name
      index <- index + 1L
    }
  }

  return(result)
}


the_default_glyph_files <- create_glyph_files(the_glyphs, the_default_colormap)
the_aqi_glyph_files <- create_glyph_files(the_glyphs, ASNAT_aqi_colormap)
the_viridis_glyph_files <- create_glyph_files(the_glyphs, the_viridis_colormap)
the_gray_glyph_files <- create_glyph_files(the_glyphs, the_gray_colormap)
the_blue_glyph_files <- create_glyph_files(the_glyphs, the_blue_colormap)
the_colorsafe_glyph_files <-
  create_glyph_files(the_glyphs, the_colorsafe_colormap)



# Define options for all tooltips:
# https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip#47478586

tooltip_options <- list("delay': 1000, 'hack" = "hack")



############################ Network Summary ##################################

column_data <- reactiveVal(NULL)
column_data1 <- reactiveVal(NULL)
column_data2 <- reactiveVal(NULL)
dataset_dropdown_l <- reactiveVal(NULL)

dataset_dropdown_txt <- reactiveVal(NULL)
dataset_dropdown1_txt <- reactiveVal(NULL)
dataset_dropdown2_txt <- reactiveVal(NULL)

col_value_txt <- reactiveVal(NULL)
col_value1_txt <- reactiveVal(NULL)
col_value2_txt <- reactiveVal(NULL)

mean_max_df <- reactiveVal(NULL)
df_id <- reactiveVal(NULL)
ns_mean_max_sel <- reactive(NULL)
reactive_polygon <- reactiveVal(NULL)

calendar_val <- reactiveVal(NULL)
reactive_ns_polygon <- reactiveVal(NULL)

reactive_leaflet_map <- reactiveVal(NULL)
reactive_time_series_plot <- reactiveVal(NULL)
reactive_calendar_series_plot <- reactiveVal(NULL)
reactive_calendar <- reactiveVal(NULL)
reactive_barchart <- reactiveVal(NULL)
reactive_barchart_aqi <- reactiveVal(NULL)
reactive_time_variation <- reactiveVal(NULL)
reactive_scatterplot <- reactiveVal(NULL)

ns_plot_list <- reactiveVal(NULL)



###############################################################################
# Declare ui object that defines the GUI Controller.
# Note this is a single function call to fluidPage() with a very long
# comma-delimited sequence of expressions.
###############################################################################

ui <- fluidPage(

  # When run outside RStudio, close the browser tab upon exit.
  # https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R

  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.closeWindow = function() {window.close();}",
                         functions = c("closeWindow")),

  shinybusy::add_busy_bar(color = "steelblue"),

  # Placeholder for Network Summary:
  tags$head(includeCSS("./layout.css")),

  # 1. The tags magic is needed to make scrolling work with tags$div() used for
  #    the list of variables.
  # 2. Make message area scrolling.
  # 3. Make popup messages appear at the top of the page.
  #    https://github.com/rstudio/shiny/issues/2988
  # 4. Try to reduce the amount of whitespace of map timestamp and legends.
  #    https://github.com/rstudio/leaflet/issues/488

  tags$head(
    tags$style(type = "text/css",
               ".scrolling .shiny-bound-input{overflow-y: scroll;}"),
    tags$style(type = "text/css",
               "#message_area {width: 100%; height: 80px; overflow: scroll;}"),
    tags$style(type = "text/css",
               "#shiny-notification-panel {
                 top: 0; bottom: unset; left: 0; right: 0;
                 margin-left: auto; margin-right: auto;
                 width: 100%;
                 max-width: 450px;
              }"),
    tags$style(type = "text/css",
      ".data-load-btn:disabled {
        opacity: 0.5 !important;
        cursor: not-allowed !important;
      }"),
    tags$style(HTML("
      .inline-elements {
        display: flex;
        align-items: center;
      }
      .info-icon {
        font-size: 16px;
        color: #007BFF; /* Bootstrap primary color */
        cursor: pointer;
      }
    ")),
  tags$style(HTML("
    #table3_interactive table.dataTable thead th {
      background-color: #f7f7f7;
      border-bottom: 2px solid #ddd; 
      padding: 4px 25px 4px 4px; /* top, right, bottom, left */
      text-align: center; 
      font-weight: bold; 
    }
  "))


    #tags$style(type="text/css",
    #          ".leaflet .legend {line-height: 10px; font-size: 10px;}",
    #          ".leaflet .legend i{height: 10px; width: 10px;}")
  ),

  titlePanel("ASNAT"),

  h5("asnat@epa.gov"),
  tags$a(href = user_manual_url, target = "_blank", user_manual_url),

  verbatimTextOutput("message_area", placeholder = TRUE),

  fluidRow(
    column(4L,
      h5("1. Pan/zoom map to region of interest."),
      h5("2. Start Date,Days to retrieve,Aggregate:"),

      fluidRow(
        column(5L,
               dateInput("start_date", label = "Start Date:",
                         #width = "100px",
                         value = start_date(default_model),
                         min = "1970-01-01", max = Sys.Date() + 1L),

        shinyBS::bsTooltip("start_date",
                           "Starting date of data to load.",
                           options = tooltip_options)),
        column(3L,
               numericInput("days", label = "Days:",
                            #width = "100px",
                            value = days(default_model),
                            min = 1L,
                            max = 366L,
                            step = 1L),

               shinyBS::bsTooltip("days",
                                 "Number of days of data to load.",
                                 options = tooltip_options)),

        column(4L,
               selectInput("timestep_size",
                           label = "Timestep:  ",
                           #width = "120px",
                           choices = list("hours", "days"),
                           selected = timestep_size(default_model)),

               shinyBS::bsTooltip("timestep_size",
                                  "Timestep size and data aggregation.",
                                  placement = "top",
                                  options = tooltip_options))
      ),

      actionButton("delete_data", label = "Delete Loaded Data"),
      # actionButton("debug_test", label = "debug test"),

      shinyBS::bsTooltip("delete_data",
                         paste0("Delete any data in memory and ",
                                "previously retrieved disk cache."),
                         options = tooltip_options),

      # The tags$div() is needed to make selectInput() scolling.

      tags$div(id = "scrolling1", class = "scrolling",
               selectInput("coverage_menu",
                           label = "Load Web (control/command-click two):",
                           multiple = TRUE, selectize = FALSE,
                           choices = webservice_variables,
                           size = 24L)),

      shinyBS::bsTooltip("coverage_menu",
                         paste0("Select one or two variables to retrieve ",
                                "from a webservice."),
                         options = tooltip_options),

      tags$a(href = purple_air_attribution_url, target = "_blank",
             purple_air_attribution_url),

      # JavaScript handler for purple air terms:
      tags$script("
        Shiny.addCustomMessageHandler('trackFocus', function(message) {
          var input = document.getElementById(message.id);
          if (input) {
            input.addEventListener('focus', function() {
              Shiny.setInputValue('purple_air_key_focused', true, {priority: 'event'});
            }, { once: true });
          }
        });
      "),

      textInput("purple_air_key", "PurpleAir Key:",
                value = purple_air_key(default_model)),

      shinyBS::bsTooltip("purple_air_key",
                         "Request a read key from contact@purpleair.com.",
                         placement = "top",
                         options = tooltip_options),

      shinyBS::bsModal("invalidKeyModal",
                       "Invalid Purple Air Key",
                       trigger = NULL,
                       size = "small",
                       p("Your Purple Air key appears to be invalid."),
                       p("Please ensure you have:"),
                       tags$ul(
                         tags$li("Requested an API key from ",
                                 tags$a(href = "mailto:contact@purpleair.com",
                                        "contact@purpleair.com")),
                        tags$li("Copied the key correctly")
                       )
      ),

      shinyBS::bsModal("invalid_retrieve_data_Modal",
                       "Data Retrieval Failed",
                       trigger = NULL,
                       size = "small",
                       p("Please make sure:"),
                       p("1. Rsig server is running."),
                       p("2. The data is available in the Rsig server."),
                       p("3. You have a valid Purple Air key."),
                       tags$ul(
                         tags$li("Requested an API key from ",
                                tags$a(href = "mailto:contact@purpleair.com",
                                       "contact@purpleair.com"))
                       )
      ),

      tags$div(id = "scrolling2", class = "scrolling",
               selectizeInput("purple_air_sites_menu",
                              label = "Purple Air Sites:",
                              options = list(maxItems = 1L),
                              choices = NULL)),

      shinyBS::bsTooltip("purple_air_sites_menu",
                         paste0("Optionally select a single PurpleAir sensor ",
                                "within the viewing area to retrieve ",
                                "(or 0 for all sensors)."),
                         placement = "top",
                         options = tooltip_options),

      tags$a(href = aqs_parameter_codes_url, target = "_blank",
             "AQS Parameter Codes Table"),

      tags$div(id = "scrolling2", class = "scrolling",
               selectizeInput("aqs_pm25_codes_menu",
                              label = "AirNow/AQS PM25 parameter codes:",
                              options = list(maxItems = 1L),
                              choices = ASNAT_aqs_pm25_codes)),

      shinyBS::bsTooltip("aqs_pm25_codes_menu",
                         paste0("Optionally select a subset of AirNow/AQS pm25",
                                " parameter codes to retrieve."),
                         placement = "right",
                         options = tooltip_options),

      actionButton("retrieve_data", label = "Retrieve Data",
                   class = "data-loading-btn"),

      shinyBS::bsTooltip("retrieve_data",
                         "Retrieve data from a webservice.",
                         options = tooltip_options),

      h1(),

      div(style = "position: relative;width: 100%;",

          fileInput("load_data_from_standard_file",
                    label = "Load data from a standard-format file:",
                    multiple = FALSE,
                    accept = list(".csv", ".tsv", ".txt", "text/plain")),
          tags$i(class = "fas fa-info-circle info-icon",
                id = "info_icon_standard_file",
                style = "position: absolute; top: 0px; left: 260px; cursor: pointer;"),
          actionButton("format_manual", "Format Manual", class = "btn-info",
                       style = "position: absolute; top: 83px;")
      ),

      shinyBS::bsPopover("info_icon_standard_file",
                        title = NULL,
                        content = paste0(
                          "Generated using importer tool (link). <br>",
                          "format (Please refer to the format manual below).",
                          " or see ASNAT-saved data files for exemplars."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container = "body")),

      # FIX: Why doesn't the following tooltip appear for the fileInput widget?

      shinyBS::bsTooltip("load_data_from_standard_file",
                         paste0("See ASNAT User Guide for Standard File Format",
                               " or see ASNAT-saved data files for exemplars."),
                         options = tooltip_options),

      br(),
      div(style = "position: relative; display: inline-block;",

          actionButton("show_import_fileset",
                       label = "Load data from sensor instrument files"),

          # Info icon positioned absolutely relative to button
          tags$i(class = "fas fa-info-circle info-icon",
                 id = "info_icon_import_fileset",
                 style = "position: absolute; top: -20px; right: -10px;")
      ),

      shinyBS::bsPopover("info_icon_import_fileset",
                      title = NULL,
                      content = "Currently only accepts offline (SD card) PurpleAir data",
                      placement = "top",
                      trigger = "hover",
                      options = list(container = "body")),

      shinyBS::bsTooltip("show_import_fileset",
                        "Show dialog for loading sensor data from files.",
                        options = tooltip_options),

      h1(),

      selectInput("dataset_x_menu",
                  label = "Select Dataset X:",
                  choices = list()),

      selectInput("dataset_x_variable_menu",
                  label = "Select Variable Of Dataset X:",
                  choices = list()),

      selectInput("dataset_y_menu",
                  label = "Select Comparison Dataset Y (optional):",
                  choices = list()),

      selectInput("dataset_y_variable_menu",
                  label = "Select Variable Of Dataset Y:",
                  choices = list()),

      selectInput("dataset_z_menu",
                  label = "Select Coloring Dataset Z (optional):",
                  choices = list()),

      selectInput("dataset_z_variable_menu",
                  label = "Select Variable Of Dataset Z:",
                  choices = list()),

      shinyBS::bsTooltip("dataset_z_menu",
                         paste0("This dataset will be used to color ",
                                "scatterplot points. It must match the source ",
                                "(e.g., AQS.rh, for AQS.pm25) of either ",
                                "Dataset X or Dataset Y."),
                         options = tooltip_options),

      # The tag magic is needed to force label on the same line as widget:

      div(style = "position: relative; display: inline-block;",

        tags$div(style = "display: inline-block;vertical-align:top;",
                 h5("Neighbor distance(m):")),

        # Info icon positioned absolutely relative to button
        tags$i(class = "fas fa-info-circle info-icon",
              id = "info_icon_distance",
              style = "position: absolute; top: 10px; right: -18px;")
      ),

      shinyBS::bsPopover("info_icon_distance",
                         title = NULL,
                         content = paste0(
                           "This is the maximum distance where pairs of X and Y data will be found.<br>",
                           "Larger distances may have more differences in air pollution between X and Y sites."
                         ),
                         placement = "top",
                         trigger = "hover",
                         options = list(container = "body")),

      # tags$div(style = "display: inline-block;vertical-align:top;",
      #          h5("Neighbor distance(m):")),
      tags$div(style = "display: inline-block;vertical-align:top; width: 45%; margin-left: 20px;",
               numericInput("maximum_neighbor_distance",
                            #label = "Max neighbor distance(m):",
                            label = NULL,
                            width = "80px",
                            value = maximum_neighbor_distance(default_model),
                            min = 0.0, max = 10000.0, step = 100.0)),

      shinyBS::bsTooltip("maximum_neighbor_distance",
                         paste0("Maximum distance in meters between ",
                                "time-matched X and Y neighbor points."),
                         options = tooltip_options),

      if (!ASNAT_is_remote_hosted) textInput("output_directory",
                                             label = "Save to Directory:",
                                             value =
                                               output_directory(default_model)),

      if (!ASNAT_is_remote_hosted) shinyBS::bsTooltip("output_directory",
                                     paste0("Select directory/folder to use ",
                                            "when saving loaded data files."),
                                                    options = tooltip_options),

      # The tag magic is needed to force label on the same line as widget:

      tags$div(style = "display: inline-block;vertical-align:top;",
               h5("Output File Format:")),
      tags$div(style = "display: inline-block;vertical-align:top; width: 80px;",
               selectInput("output_format_menu",
                           label = NULL,
                           choices = list("csv", "tsv", "json"),
                  selected = output_format(default_model))),
      br(),

      shinyBS::bsTooltip("output_format_menu",
                         "Select format of files when saving.",
                         placement = "top",
                         options = tooltip_options),

      if (!ASNAT_is_remote_hosted) actionButton("save_data", label = "Save Data") else
      downloadButton(outputId = "download_data", label = "Save Data"),

      shinyBS::bsTooltip("save_data",
                         "Save all loaded datasets and tables.",
                         options = tooltip_options),

      shinyBS::bsTooltip("download_data",
                         "Save all loaded datasets and tables as a zip file.",
                         options = tooltip_options),

      actionButton("quit", label = "Quit"),

      shinyBS::bsTooltip("quit",
                         "Save state and quit application.",
                         options = tooltip_options)

    ),

    # The following GUI elements appear as tabs on the right side of the page:

    column(8L,
      tabsetPanel(id = "tabs",

        tabPanel("Map", value = "map",

                 # The map width/height is set to match the default dimensions
                 # (somehow/automatically) set when saving images.
                 # This avoids surprises by ensuring that the saved map image
                 # appears the same as when viewed in this program.

                 leaflet::leafletOutput("map",
                               width = the_map_width,
                               height = the_map_height),

                 div(style = "margin: 10px 0;",
                     actionButton("reset_map", label = "Reset map")),

                 shinyBS::bsTooltip("reset_map",
                                    "Reset map to default state.",
                                    options = tooltip_options),

                 selectInput("legend_colormap_menu",
                             label = "Legend Colormap",
                             choices = list("default", "AQI", "gray", "blue",
                                            "colorsafe", "viridis"),
                             selected = legend_colormap(default_model)),

                 shinyBS::bsTooltip("legend_colormap_menu",
                                    paste0("Choose a legend colormap for data ",
                                           "displayed on the map. ",
                                           "(AQI note: PM hourly uses daily ",
                                           "breakpoints, ",
                                           "PM1 uses PM2.5 breakpoints and ",
                                           "ozone daily uses 8-hour ",
                                           "breakpoints)."),
                                    options = tooltip_options),

                 checkboxInput("use_fancy_labels",
                                label = "Use fancy names/units",
                                value = use_fancy_labels(default_model)),

                 shinyBS::bsTooltip("use_fancy_labels",
                                    paste0("Use sub/superscripts and symbols ",
                                    "for some variable names and units."),
                                    options = tooltip_options),

                 checkboxInput("show_mean_values",
                                label = "Show mean values over all timesteps",
                                value = show_mean_values_on_map(default_model)),

                 shinyBS::bsTooltip("show_mean_values",
                                    "Show mean values over all timesteps.",
                                    options = tooltip_options),

                 sliderInput("timestep_slider", label = "Timestep:",
                             value = timestep(default_model),
                             min = 0, max = timesteps(default_model) - 1L,
                             step = 1L),

                 shinyBS::bsTooltip("timestep_slider",
                                    "Set timestep to display.",
                                    options = tooltip_options),

                 actionButton("zoom_to_data", label = "Zoom To Data"),

                 shinyBS::bsTooltip("zoom_to_data",
                                    "Zoom map to location of loaded data.",
                                    options = tooltip_options),

                 if (!ASNAT_is_remote_hosted) actionButton("save_map_image", label = "Save Map Image") else
                 downloadButton(outputId = "download_map_image", label = "Save Map Image"),

                 shinyBS::bsTooltip("save_map_image",
                                    "Save current map image as a png file.",
                                    options = tooltip_options),

                 shinyBS::bsTooltip("download_map_image",
                                    "Save current map image as a png file.",
                                    options = tooltip_options),

                 if (!ASNAT_is_remote_hosted) actionButton("save_map_movie", label = "Save Map Movie") else
                 downloadButton(outputId = "download_map_movie", label = "Save Map Movie"),

                 shinyBS::bsTooltip("save_map_movie",
                                    "Save all map images as a movie file.",
                                    options = tooltip_options),

                 shinyBS::bsTooltip("download_map_movie",
                                    "Save all map images as a movie file.",
                                    options = tooltip_options)
        ),

        tabPanel("Flagging", value = "flagging",

                 selectInput("flag_dataset_menu", label = "Dataset to Flag:  ",
                             choices = list("Dataset X", "Dataset Y"),
                             selected = "Dataset X"),

                 shinyBS::bsTooltip("flag_dataset_menu",
                                    "Dataset to flag/reflag.",
                                    options = tooltip_options),

                 # Must use DT::dataTableOutput() to show data frames too large
                 # to fit on a single page.
                 # Also, implements multi-row selection for manual flagging.

                 DT::dataTableOutput("dataset_content_to_flag"),

                 shinyBS::bsTooltip("dataset_content_to_flag",
                                    "Optionally select data rows to flag 99",
                                    placement = "top",
                                    options = tooltip_options),

                 br(),

                 actionButton("exclude_99_flagged",
                              label = "Exclude 99 from flagged column"),

                 shinyBS::bsTooltip("exclude_99_flagged",
                                    "Exclude 99 from selected rows flagged column.",
                                    options = tooltip_options),

                 actionButton("include_99_flagged",
                              label = "Include 99 in flagged column"),

                 shinyBS::bsTooltip("include_99_flagged",
                                    "Include 99 in selected rows flagged column.",
                                    options = tooltip_options),

                 br(),

                 fileInput("load_flag_conditions",
                           label = "Load flag conditions from a file:",
                           multiple = FALSE,
                           accept = list(".txt", "text/plain")),

                 shinyBS::bsTooltip("load_flag_conditions",
                                    "Load flag conditions from a file.",
                                    options = tooltip_options),

                 textAreaInput("flag_conditions", "Flag Conditions:",
                               width = "800px", rows = 10L),

                 shinyBS::bsTooltip("flag_conditions",
                                    paste0("Enter flag conditions here - ",
                                           "one per line.\n",
                                           "Flag conditions are boolean ",
                                           "expressions comparing the ",
                                           "dataset's column values to ",
                                           "numbers using the column name ",
                                           "and operators 'or', 'and', ",
                                           "<, <=, >, >=, =\n",
                                           "E.g., pm25 < 0"),
                                    options = tooltip_options),

                 br(),

                 actionButton("apply_flagging",
                              label = "Apply Flagging Conditions Above"),

                 shinyBS::bsTooltip("apply_flagging",
                                    paste0("Apply all of the above flagging ",
                                    "to Dataset Y ",
                                    "(or Dataset X if Dataset Y is none)."),
                                    options = tooltip_options),

                 actionButton("clear_flagging", label = "Clear Flagging"),

                 shinyBS::bsTooltip("clear_flagging",
                                    paste0("Clear all current flaggings."),
                                    options = tooltip_options),

                 h1(),

                 if (!ASNAT_is_remote_hosted) actionButton("save_flag_conditions", label = "Save Flag Conditions") else
                 downloadButton(outputId = "download_flag_conditions",
                                label = "Save Flag Conditions"),

                 shinyBS::bsTooltip("save_flag_conditions",
                                    paste0("Save flag conditions to file ",
                                           "<save-directory>/flags.txt."),
                                    options = tooltip_options),

                 actionButton("refresh_flagging", label = "Refresh Table Using Settings Below"),
                 shinyBS::bsTooltip("refresh_flagging",
                                    paste0("Refresh the flagging table to reflect ",
                                         "changes from the preset flag check boxes below."),
                                    options = tooltip_options),
                 br(),
                 br(),

                 tabsetPanel(
                  tabPanel("Dataset Y Neighbor Flagging",

                    br(),

                    fluidRow(column(5L,
                                    checkboxInput("apply_difference",
                                                  label = "Apply flag 80 if difference > ",
                                                  value = apply_maximum_neighbor_value_difference(default_model))),
                              column(4L,
                                    numericInput("maximum_neighbor_value_difference",
                                                  label = NULL,
                                                  width = "80px",
                                                  value = maximum_neighbor_value_difference(default_model),
                                                  min = 0.0, max = 100.0, step = 1.0))), #fluidRow

                    shinyBS::bsTooltip("maximum_neighbor_value_difference",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 80 if the absolute difference",
                                              " of the time-matched comparison",
                                              " variable with Dataset X is",
                                              " greater than value."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_percent_difference",
                                                  label = "Apply flag 81 if percent difference > ",
                                                  value = apply_maximum_neighbor_value_percent_difference(default_model))),
                            column(4L,
                                   numericInput("maximum_neighbor_value_percent_difference",
                                                label = NULL,
                                                width = "80px",
                                                value = maximum_neighbor_value_percent_difference(default_model),
                                                min = 0.0, max = 100.0, step = 1.0))), # fluidRow

                    shinyBS::bsTooltip("maximum_neighbor_value_percent_difference",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 81 if the percent difference",
                                              " of the time-matched comparison",
                                              " variable with Dataset X is",
                                              " greater than value."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_r_squared",
                                                  label = "Apply flag 82 if R-squared < ",
                                                  value = apply_minimum_neighbor_value_r_squared(default_model))),
                             column(4L,
                                    numericInput("minimum_neighbor_value_r_squared",
                                                  label = NULL,
                                                  width = "80px",
                                                  value = minimum_neighbor_value_r_squared(default_model),
                                                  min = 0.0, max = 1.0, step = 0.1))), # fluidRow

                    shinyBS::bsTooltip("minimum_neighbor_value_r_squared",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 82 if the R-squared",
                                              " (per-site over all timesteps,",
                                              " excluding already flagged values)",
                                              " of the comparison",
                                              " variable with Dataset X is",
                                              " less than value."),
                                        placement = "top",
                                        options = tooltip_options),
                  ), # end tabPanel

                  tabPanel("Additional Flags",
                    br(),
                    fluidRow(column(5L,
                                    checkboxInput("apply_constant_value",
                                                  label = "Apply flag 83 if a constant value persists for three or more consecutive measurements.",
                                                  value = apply_constant_value_flag(default_model))),
                              column(4L,
                                     numericInput("constant_value_threshold",
                                                   label = NULL,
                                                   width = "80px",
                                                   value = NULL,
                                                   min = 0L, max = 100L, step = 1L)),
                    ),

                    shinyBS::bsTooltip("constant_value_threshold",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 83 if the value remains constant",
                                              " for more than the X",
                                              " of consecutive timesteps."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_long_missing",
                                                  label = "Apply flag 84 if missing data for more than x consecutive timesteps",
                                                  value = apply_long_missing_flag(default_model))),
                            column(4L,
                                   numericInput("long_missing_threshold",
                                                label = NULL,
                                                width = "80px",
                                                value = long_missing_threshold(default_model),
                                                min = 0L, max = 100L, step = 1L))),

                    shinyBS::bsTooltip("long_missing_threshold",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 84 if missing data for more than",
                                              " the value of consecutive timesteps."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_outlier_stat",
                                                  label = "Apply flag 85 if data point is a statistical outlier.",
                                                  value = apply_outlier_stat_flag(default_model))),
                             column(4L,
                                    numericInput("outlier_threshold",
                                                 label = NULL,
                                                 width = "80px",
                                                 value = outlier_threshold(default_model),
                                                 min = 0L, max = 100L, step = 1L))),

                    shinyBS::bsTooltip("outlier_threshold",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 85 if the data point is more than",
                                              " X standard deviations away from the mean."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(12L,
                                    checkboxInput("use_time_window",
                                                  label = "Apply outlier detection within a specified time window",
                                                  value = FALSE)),
                    conditionalPanel(
                      condition = "input.use_time_window == true",
                    fluidRow(column(6L,
                                    textInput("outlier_start_timestamps",
                                              label = "Start Time (YYYY-MM-DDTHH:MM:SS-0000)",
                                              value = NULL,
                                              placeholder = "e.g. 2022-06-01T00:00:00-0000")),
                             column(6L,
                                    textInput("outlier_end_timestamps",
                                              label = "End Time (YYYY-MM-DDTHH:MM:SS-0000)",
                                              value = NULL,
                                              placeholder = "e.g. 2022-06-01T23:59:59-0000"))))
                    ), #fluidRow
                    fluidRow(column(5L,
                                    checkboxInput("apply_hampel_filter",
                                                  label = "Apply flag 86 if data point is an outlier by hampel filter.",
                                                  value = apply_hampel_filter_flag(default_model))),
                             column(3L,
                                    numericInput("hampel_filter_window",
                                                  label = "Window",
                                                  width = "80px",
                                                  value = hampel_filter_window(default_model),
                                                  min = 0L, max = 100L, step = 1L)),
                             column(3L,
                                    numericInput("hampel_filter_threshold",
                                                  label = "Threshold",
                                                  width = "80px",
                                                  value = hampel_filter_threshold(default_model),
                                                  min = 0L, max = 100L, step = 1L))),

                    shinyBS::bsTooltip("hampel_filter_window",
                                        paste0("Set the window size for the Hampel filter."),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("hampel_filter_threshold",
                                        paste0("Set the threshold for the Hampel filter. "),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("apply_hampel_filter",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 86 if the data point is an outlier within the given window and threshold. ",
                                              " The outlier is flagged based on hampel filter."),
                                        placement = "top",
                                        options = tooltip_options),

                    conditionalPanel(
                      condition = "input.apply_hampel_filter == true",
                      fluidRow(
                        column(12L,
                          div(class = "well well-sm",
                            style = "margin-top: 10px; background-color: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px;",
                            fluidRow(
                              column(6L,
                                h5(icon("info-circle"), "Average Records per Device:",
                                  style = "color: #31708f; margin-bottom: 5px;")),
                              column(6L,
                                textOutput("avg_records_display") %>%
                                  tags$span(style = "font-size: 16px; font-weight: bold; color: #31708f; display: inline-block; margin-top: 5px;")
                              )
                            )
                          )
                        )
                      )
                    )

                  ), #tabPanel("Additional Flags"

                  tabPanel("Invalid Values Flags",
                    br(),
                    fluidRow(column(5L,
                                    checkboxInput("apply_invalid_negtive_value",
                                                  label = "Apply flag 60 for invalid negative values in pollutant measurements (O3, NO2, CO, etc).",
                                                  value = apply_negtive_value_flag(default_model))),
                            ),


                    shinyBS::bsTooltip("apply_invalid_negtive_value",
                                        paste0("Mark Dataset flagged column with",
                                               " 60 when pollutants have invalid negative values."),
                                        placement = "top",
                                        options = tooltip_options),



                  ), #Invalid Values Flags

                  # tabPanel("Invalid relationships Flags"
                  #   br(),

                  # ), # Invalid relationships Flags
                  tabPanel("Cross-field validation flags",
                    br(),
                    fluidRow(column(5L,
                                    checkboxInput("date_validation",
                                                  label = "Apply flag 65 for temporal inconsistencies (e.g., end date before start date).",
                                                  # value = apply_check_temporal_alignment(default_model))),
                                                  value = apply_date_validation_flag(default_model)))

                              ),

                    shinyBS::bsTooltip("date_validation",
                                      paste0("Marks Dataset Y flagged column with 65 when temporal fields are inconsistent ",
                                            "(e.g., end timestamps before start timestamps."),
                                      placement = "top",
                                      options = tooltip_options),

                  ), # Invalid relationships Flags

                  tabPanel("Suspicious changes flags",
                   br(),
                    fluidRow(column(5L,
                                    checkboxInput("apply_sudden_spike",
                                                  label = "Apply flag 70 for sudden spikes in pollutant levels",
                                                  value = apply_sudden_spike_flag(default_model))),
                             column(3L,
                                    numericInput("spike_time_window",
                                                  label = "Window",
                                                  width = "80px",
                                                  value = spike_time_window(default_model),
                                                  min = 0L, max = 100L, step = 1L)),
                             column(3L,
                                    numericInput("spike_threshold",
                                                  label = "Threshold",
                                                  width = "80px",
                                                  value = spike_threshold(default_model),
                                                  min = 0L, max = 100L, step = 1L))),


                    shinyBS::bsTooltip("apply_sudden_spike",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 70 if pollutant levels suddenly increase by more than",
                                              " X times within given timesteps."),                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("spike_time_window",
                                        paste0("How many timesteps to look back to check for a spike."),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("spike_threshold",
                                        paste0("Defines the percentage increase from the previous timestep that constitutes a spike."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_sudden_drop",
                                                  label = "Apply flag 71 for sudden drops in pollutant levels",
                                                  value = apply_sudden_drop_flag(default_model))),
                             column(3L,
                                    numericInput("drop_time_window",
                                                  label = "Window",
                                                  width = "80px",
                                                  value = drop_time_window(default_model),
                                                  min = 0L, max = 100L, step = 1L)),
                             column(3L,
                                    numericInput("drop_threshold",
                                                  label = "Threshold",
                                                  width = "80px",
                                                  value = drop_threshold(default_model),
                                                  min = 0L, max = 100L, step = 1L))),

                    shinyBS::bsTooltip("apply_sudden_drop",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 71 if pollutant levels suddenly decrease by more than",
                                              " X times within the given timesteps."),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("drop_time_window",
                                        paste0("How many timesteps to look back to check for a drop."),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("drop_threshold",
                                        paste0("Defines the percentage decrease from the previous timestep that constitutes a drop."),
                                        placement = "top",
                                        options = tooltip_options),

                    fluidRow(column(5L,
                                    checkboxInput("apply_daily_pattern_o3",
                                                  label = "Apply flag 72 for ozone inconsistent daily patterns",
                                                  value = apply_daily_pattern_o3(default_model)))),

                    fluidRow(column(5L,
                                    checkboxInput("apply_daily_pattern_pm",
                                                  label = "Apply flag 73 for PM inconsistent daily patterns",
                                                  value = apply_daily_pattern_pm(default_model)))),

                    shinyBS::bsTooltip("apply_daily_pattern_o3",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 72 if Ozone deviate from typical",
                                              " daily patterns."),
                                        placement = "top",
                                        options = tooltip_options),

                    shinyBS::bsTooltip("apply_daily_pattern_pm",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 73 if pm deviate from typical",
                                              " daily patterns."),
                                        placement = "top",
                                        options = tooltip_options),

                  ), #Suspicious changes flags

                  tabPanel("Redundancy check",
                   br(),
                    fluidRow(column(5L,
                                    checkboxInput("apply_redundancy_check",
                                                  label = "Apply flag 90 for duplicate timestamps at same location",
                                                  value = apply_redundancy_check_flag(default_model)))),

                    shinyBS::bsTooltip("redundancy_threshold",
                                        paste0("Mark Dataset Y flagged column with",
                                              " 90 if multiple measurements exist",
                                              " at the same location and timestamp."),
                                        placement = "top",
                                        options = tooltip_options),

                  ), # Redundancy check

                  tabPanel("Invalid formatting and units",
                    br(),
                    fluidRow(column(5L,
                                    checkboxInput("apply_format_check",
                                                  label = "Apply flag 95 for incorrect date formats",
                                                  value = apply_format_check_flag(default_model)))),

                    shinyBS::bsTooltip("apply_format_check",
                                        paste0("Mark Dataset flagged column with",
                                               " 95 if timestamps do not conform to",
                                               " the expected ISO 8601 format",
                                               " (YYYY-MM-DDTHH:MM:SS-ZZZZ)."),
                                        placement = "top",
                                        options = tooltip_options),

                    h5("Check for inconsistent units across variables and flag problematic variables."),

                    actionButton("check_units_consistency",
                                 label = "Check Units Consistency",
                                 icon = icon("check-circle"),
                                 class = "btn-primary"
                    ),

                    br(),

                    fluidRow(
                      column(6,
                        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; height: 100%;",
                            h4("All Variables"),
                            tableOutput("all_units_table")
                        )
                      ),
                      column(6,
                        div(style = "background-color: #fff3f3; padding: 15px; border-radius: 4px; height: 100%;",
                            h4("Inconsistent Variables"),
                            tableOutput("flagged_units_table")
                        )
                      )
                    )
                  )

                 ) #tabsetPanel(

        ), #flagging tab


        tabPanel("Tables", value = "tables",

                 actionButton("summarize",
                              label = "Summarize and Compare X and Y"),

                 shinyBS::bsTooltip("summarize",
                                    paste0("Summarize and compare selected ",
                                           "X and optional Y datasets."),
                                    options = tooltip_options),
                 actionButton("summary_statistics", 
                              label = "summary statistics for dataset X"),
                
                 shinyBS::bsTooltip("summary_statistics",
                                    "Generate validation statistics for the selected datasets.",
                                    options = tooltip_options),    
                 tableOutput("table1"),
                 tableOutput("table2"),
                 tableOutput("table3"),
                 tableOutput("table4"),
                 tableOutput("table5"),
                 DT::dataTableOutput("table3_interactive")
        ),

        tabPanel("Plots", value = "plots",

                 h5("Neighbors Map - Shows Neighbors of (Single) Selected Dataset X Site"),

                 leaflet::leafletOutput("site_neighbor_map",
                               width = the_site_neighbor_map_width,
                               height = the_site_neighbor_map_height),

                 checkboxInput("show_site_labels",
                                label = "Show site labels",
                                value = show_site_labels(default_model)),

                 shinyBS::bsTooltip("show_site_labels",
                                    paste0("Show site id labels."),
                                    options = tooltip_options),

                 tags$div(id = "scrolling3", class = "scrolling",
                          selectizeInput("neighbors_menu",
                                         label = "Select Dataset X Site With Neighbors:",
                                         options = list(maxItems = 1L),
                                         choices = NULL)),

                 shinyBS::bsTooltip("neighbors_menu",
                                    paste0("Select a single Dataset X site ",
                                           "(that has Dataset Y neighbors) ",
                                           "(or 0 for all)"),
                                    placement = "top",
                                    options = tooltip_options),

                 checkboxInput("use_interactive_plots",
                                label = "Use interactive plots",
                                value = use_interactive_plots(default_model)),

                 shinyBS::bsTooltip("use_interactive_plots",
                                    "Use interactive/color plots.",
                                    options = tooltip_options),

                 actionButton("make_plots",
                              label = "Make Plots of Datasets X, Y"),

                 shinyBS::bsTooltip("make_plots",
                                    "Make plots of selected X and Y datasets.",
                                    options = tooltip_options),

                 if (!ASNAT_is_remote_hosted) actionButton("save_plots", label = "Save Plots") else
                 downloadButton(outputId = "download_plots",
                                label = "Save Plots"),

                 shinyBS::bsTooltip("save_plots",
                                    "Save pdf files of plots and map.",
                                    options = tooltip_options),

                 shinyBS::bsTooltip("download_plots",
                                    "Save pdf files of plots and map.",
                                    options = tooltip_options),

                 br(),
                 br(),
                 plotOutput("boxplot_x"),
                 plotlyOutput("interactive_boxplot_x"),
                 br(),
                 br(),
                 br(),
                 plotOutput("aqi_plot_x"),
                 plotlyOutput("interactive_aqi_plot_x"),
                 br(),
                 br(),
                 br(),
                 plotOutput("timeseriesplot_x"),
                 plotlyOutput("interactive_timeseriesplot_x"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("boxplot_y"),
                 plotlyOutput("interactive_boxplot_y"),
                 br(),
                 br(),
                 br(),
                 plotOutput("aqi_plot_y"),
                 plotlyOutput("interactive_aqi_plot_y"),
                 br(),
                 br(),
                 br(),
                 plotOutput("timeseriesplot_y"),
                 plotlyOutput("interactive_timeseriesplot_y"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("scatterplot_xy"),
                 plotlyOutput("interactive_scatterplot_xy"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("scatterplot_xy_unflagged"),
                 plotlyOutput("interactive_scatterplot_xy_unflagged"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("aqi_plot"),
                 plotlyOutput("interactive_aqi_plot"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("aqi_plot_unflagged"),
                 plotlyOutput("interactive_aqi_plot_unflagged"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("neighbor_statistics_boxplots"),
                 plotlyOutput("interactive_neighbor_statistics_boxplots"),
                 br(),
                 hr(),
                 br(),
                 plotOutput("neighbor_statistics_boxplots_unflagged"),
                 plotlyOutput("interactive_neighbor_statistics_boxplots_unflagged"),
                 br(),
                 hr(),
                 br()
        ),

        tabPanel("Corrections", value = "corrections",

                 actionButton("correction",
                              label = "Regression Correction"), # Add Data Correction button
                 actionButton("move_average",
                              label = "Moving Averages"), # Add Data Correction button


                 shinyBS::bsTooltip("correction",
                                    paste0("Display the details for data correction"),
                                    options = tooltip_options),
                 shinyBS::bsTooltip("select_all", "Select all rows in the table"),
                 shinyBS::bsTooltip("unselect_all", "Unselect all rows in the table"),

                  # New data correction layout
                  div(id = "data_correction_regression_layout", style = "display: none;",
                    # Display a custom message at the top
                    h3("Regression Correction Panel"),
                    h5("(Please ensure that dataset X is the reference data and dataset Y is the target data.)"),
                    h4("Neighboring Points"),
                    DT::dataTableOutput("selectable_correction_table"),
                    # Adding select all and unselect all buttons
                    div(class = "btn-group", style = "margin-bottom:10px",
                      actionButton("select_all", "Select All", icon = icon("check-circle"), class = "btn btn-primary", style = "margin-right : 10px;"),
                      actionButton("unselect_all", "Unselect All", icon = icon("times-circle"), class = "btn btn-primary"),
                    ),

                    br(),

                    h4("Selected IDs:"),
                    verbatimTextOutput("selected_ids_display"),

                    br(),

                    h4("Correction Models"),
                    tabsetPanel(
                      tabPanel("Apply Correction",

                        br(),

                        selectInput("regression_type", "Select Regression Type",
                                    choices = c("Linear")),
                        # selectInput("regression_type", "Select Regression Type",
                        #             choices = c("Linear", "Polynomial", "Exponential", "Logarithmic", "Power")),
                        actionButton("generate_coefficients", "Generate Coefficients", class = "btn btn-primary"),

                        br(),

                        h5("Equations:"),
                        DT::dataTableOutput("equations_list"),
                        div(class = "btn-group", style = "margin-bottom:10px",
                            actionButton("select_all_equations", "Select All", icon = icon("check-circle"), class = "btn btn-primary", style = "margin-right : 10px;"),
                            actionButton("unselect_all_equations", "Unselect All", icon = icon("times-circle"), class = "btn btn-primary"),
                        ),

                        br(),

                        actionButton("apply_regression", "Apply Regression", class = "btn btn-danger"),
                        actionButton("output_to_csv", "Export to CSV", class = "btn btn-success"),

                        br(),
                        br(),

                        div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                            h5("Datapoint Size Selection: ", style = "margin-bottom: 10px;"),
                            radioButtons("datapoint_size_select",
                                         label = NULL,
                                         choices = list("Normal (2px)" = 2L,
                                                        "Large (4px)" = 4L,
                                                        "Extra Large (6px)" = 6L),
                                         selected = 2,
                                         inline = TRUE)
                        ),

                        helpText("Please click the 'Apply Regression' button to apply the changes.",
                                 style = "color: #6c757d; font-style: italic; font-size: 0.9em;"),

                        br(),

                        h4("Corrected Data"),
                        plotOutput("corrected_scatter_plot_temp"),
                        h4("Original Data"),
                        plotOutput("original_scatter_plot_temp"),

                        br(),

                        # actionButton("add_to_equations", "Add to Equations", class = "btn btn-success"),
                        h4("Device Details"),
                        selectInput("selected_device_id", "Select Device ID:",
                                    choices = NULL,
                                    selected = NULL),
                        shinyBS::bsTooltip("selected_device_id",
                                          "Select a device ID to display the correction plot",
                                          options = tooltip_options),
                        h4("Interactive Plot"),
                        plotlyOutput("interactive_correction_scatter_plot"),

                        br(),

                        plotlyOutput("interactive_original_scatter_plot"),
                        tableOutput("table6"),
                      ) # end tabpanel

                    # comment out custom equation for now
                    # tabPanel("Custom Equation",
                    #          DT::dataTableOutput("regression_models_table"),
                    #          textInput("custom_model_name", "Model Name", placeholder = "Enter model name"),
                    #          uiOutput("dependent_variable_selector"),
                    #          uiOutput("independent_variables_selector"),
                    #          textInput("custom_model", "Equation", placeholder = "y = ax + b"),
                    #          div(class = "btn-group", role = "group",
                    #              actionButton("add_model", label = tagList(icon("plus"), "Add Model"), class = "btn btn-success"),
                    #              actionButton("edit_model", label = tagList(icon("edit"), "Edit Model"), class = "btn btn-warning"),
                    #              actionButton("remove_model", label = tagList(icon("minus"), "Remove Model"), class = "btn btn-danger"),
                    #              actionButton("clear_model_inputs", label = tagList(icon("eraser"), "Clear Inputs"), class = "btn btn-info")
                    #          )
                    # )
                    ), # End tabsetPanel

                    br(),

                  ), # end div

                  div(id = "data_moving_average_layout", style = "display: none;",
                    # Title and note
                    h3("Moving Average Panel"),
                    div(class = "alert alert-info",
                      icon("info-circle"),
                      HTML("Please ensure you have applied the constant value flag (flag 83) before proceeding with moving averages.<br>
                            Records with constant values will be displayed below.")
                    ), # end div

                    # Current flagged data
                    h4("Records with Constant Values (Flag 83)"),
                    DT::dataTableOutput("flagged_records_table"),

                    br(),

                    div(class = "btn-group", style = "margin-bottom:10px",
                      actionButton("select_all_flagged", "Select All", icon = icon("check-circle"), class = "btn btn-primary", style = "margin-right : 10px;"),
                      actionButton("unselect_all_flagged", "Unselect All", icon = icon("times-circle"), class = "btn btn-primary")
                    ),

                    br(),

                    h4("Selected Records:"),
                    verbatimTextOutput("selected_flagged_display"),

                    br(),

                    # Moving average controls
                    div(class = "well",
                      fluidRow(
                        column(4,
                          div(style = "display: inline-block; vertical-align: top;",
                              numericInput("window_size",
                                           "Window Size (hours):",
                                           value = 3,
                                           min = 1,
                                           max = 24,
                                           width = "150px")  # Control input width
                          )
                        ),
                        column(4,
                          div(style = "display: inline-block; vertical-align: top; margin-top: 25px;",  # Add top margin to align with input
                              actionButton("apply_moving_average",
                                           "Calculate Moving Average",
                                           class = "btn btn-primary",
                                           icon = icon("calculator"))
                          )
                        )
                      )
                    ),

                    # Results section
                    div(id = "moving_average_results",

                      # Dropdown menu for ID selection
                      div(style = "margin-bottom: 20px;",
                        selectInput("selected_ma_device_id",
                                    "Select Device ID:",
                                    choices = NULL,
                                    selected = NULL,
                                    width = "200px")
                      ),

                      h4("Results"),
                      plotlyOutput("ma_correction_plot", height = "500px"),
                      tabsetPanel(
                        tabPanel("After Correction",
                          tableOutput("ma_correction_table")
                        ),
                        tabPanel("Original Data",
                          tableOutput("ma_original_table")
                        )
                      )
                    )
                  )
        ),

        tabPanel("Network Summary", value = "networksum",
                 p(),
                 fluidRow(
                   column(12, div("Step 1: Retrieve the data! Please click on the red button.", style = "color:blue"))
                 ),
                 p(),
                 fluidRow(
                   column(4, actionButton("search_dataset_l", "Retrieve Dataset", style = "color: #FFFFFF; background-color: #F80003; border-color: #F80003"))
                 ),
                 p(),
                 fluidRow(
                   column(4, varSelectInput("dataset_dropdown", "Select Dataset", NULL))
                 ),
                 fluidRow(
                   column(12, div("Step 2: Retrieve the data for the selected variable! Please click on the 'Retrieve' button.", style = "color:blue"))
                 ),
                 p(),
                 fluidRow(
                   column(4, actionButton("retrieve_var", "Retrieve Variables"))
                 ),
                 p(),
                 fluidRow(
                   column(4, varSelectInput("col_value", "Value", NULL))
                 ),
                 fluidRow(
                   column(12, div("Step 3: Update your variables! Please click on the 'Variables' button.", style = "color:blue"))
                 ),
                 p(),
                 fluidRow(
                   column(12, actionButton("variables", "Update Variables", style = "color: #FFFFFF; background-color: #F80003; border-color: #F80003"))
                 ),
                 p(),
                 fluidRow(
                   column(3, span(textOutput("dataset_dropdown_txt"), style = "color:red")),
                   column(3, span(textOutput("col_value_txt"), style = "color:red"))
                 ),
                 p(),
                 fluidRow(
                   column(12, div("Optional: Upload your own GeoJSON. An easy way to create one is under the following link:", style = "color:blue"))
                 ),
                 fluidRow(
                   column(12, div(tags$a(href = "https://geojson.io", "geojson.io", target = "_blank"), style = "color:blue"))
                   ),
                 p(),
                 fluidRow(
                   column(6,
                          fileInput("GeoJSON", "Upload GeoJSON File", multiple = FALSE, accept = c(".geojson")))
                 ),
                 fluidRow(
                   column(4, radioButtons("ns_polygon", label = "Calculate Statistics only for GeoJSON?",
                                          choices = c("Yes", "No"), selected = "No"))
                 ),
                 fluidRow(
                   column(4, radioButtons("ns_mean_max_sel", label = "Select Mean, Max or Diff Mean",
                                          choices = c("Mean", "Max", "Diff_Mean"), selected = "Mean") #Diff_Max
                   ),
                   column(4, actionButton("get_networksum_map", label = "Show Map"))
                 ),
                 fluidRow(
                   column(12, h4("Map with Monitoring Sites"))
                 ),
                 fluidRow(
                   column(12,
                          # Loading message
                          div(id = "loading-networksum_map", h4("Map")),
                          # The main app code goes here
                          hidden(div(id = "app-content")),
                          leafletOutput("networksum_map"))
                 ),
                 fluidRow(
                   column(12,
                          # Loading message
                          div(id = "loading-barchart_ns_map", h4("Plot")),
                          # The main app code goes here
                          hidden(div(id = "app-content")),
                          plotOutput("barchart_ns_map"))
                 ),
                 #fluidRow(
                   #column(12, h4("Air Quality by the Number of Days (only for PM2.5 (daily), PM10 (daily), Ozone (hourly))"))
                   #column(12, h4("Air Quality by the Number of Days (only for PM2.5 (daily), PM10 (daily))"))
                 #),
                 fluidRow(
                   column(12,
                          # Loading message
                          div(id = "loading-barchart_ns_map_nod", h4("This plot will only be displayed if PM2.5 or PM10  daily data was selected!")),
                          # The main app code goes here
                          hidden(div(id = "app-content")),
                          plotOutput("barchart_ns_map_nod"))
                 ),
                 fluidRow(
                   column(12, h4("Plot daily mean Statistics"))
                 ),
                 fluidRow(
                   column(4, actionButton("time_series_plot", label = "Create Time Series")),
                   column(4, actionButton("calendar_series_plot", label = "Create Calendar Series")),
                   column(4, actionButton("calendar_plot", label = "Create Calendar Plot"))
                 ),
                 p(),
                 fluidRow(
                   column(4),
                   column(4),
                   column(4, radioButtons("calendar_sel", label = "What values do you want to see in the plot?",
                                          choices = c("value", "date"), selected = "value"
                                          )
                   )
                 ),
                 p(),
                 fluidRow(
                   column(12,
                          # Loading message
                          div(id = "loading-orig_time_series_plot", h4("Plot")),
                          # The main app code goes here
                          hidden(div(id = "app-content")),
                          plotOutput("orig_time_series_plot"))
                 ),
                fluidRow(
                  column(12,
                         # Loading message
                         div(id = "loading-orig_calendar_series_plot", h4("Plot")),
                         # The main app code goes here
                         hidden(div(id = "app-content")),
                         plotOutput("orig_calendar_series_plot"))
                ),
                fluidRow(
                  column(12,
                         # Loading message
                         div(id = "loading-orig_calendar_plot", h4("Plot")),
                         # The main app code goes here
                         hidden(div(id = "app-content")),
                         plotOutput("orig_calendar_plot"))
                ),
                fluidRow(
                  column(12, h4("Compare two selected Datasets with each other."))
                ),
                p(),
                fluidRow(
                  column(12, div("For the Time Variation Plot it makes sense to compare dataset with the same unit!", style = "color:blue"))
                ),
                p(),
                fluidRow(
                  column(4, varSelectInput("dataset_dropdown1", "DF1", NULL)),
                  column(4, varSelectInput("dataset_dropdown2", "DF2", NULL))
                ),
                fluidRow(
                  column(4, actionButton("retrieve_var1", "Retrieve DF1 Variables")),
                  column(4, actionButton("retrieve_var2", "Retrieve DF2 Variables"))
                ),
                p(),
                fluidRow(
                  column(4, varSelectInput("sel_column", "Variable DF1", NULL)),
                  column(4, varSelectInput("sel_column2", "Variable DF2", NULL))
                ),
                p(),
                fluidRow(
                  column(12, actionButton("variables1", "Update Variables", style = "color: #FFFFFF; background-color: #F80003; border-color: #F80003"))
                ),
                p(),
                fluidRow(
                  column(3, span(textOutput("dataset_dropdown1_txt"), style = "color:red")),
                  column(3, span(textOutput("col_value1_txt"), style = "color:red"))
                ),
                fluidRow(
                  column(3, span(textOutput("dataset_dropdown2_txt"), style = "color:red")),
                  column(3, span(textOutput("col_value2_txt"), style = "color:red"))
                ),
                p(),
                fluidRow(
                  column(4, actionButton("timeVariation_plot", label = "Create Time Variation Plot")),
                  column(4, actionButton("scatter_plot", label = "Create Scatter Plot"))
                ),
                p(),
                fluidRow(
                  column(12,
                         # Loading message
                         div(id = "loading-orig_timevar_plot", h4("Plot")),
                         # The main app code goes here
                         hidden(div(id = "app-content")),
                         plotOutput("orig_timevar_plot"))
                ),
                p(),
                fluidRow(
                  column(12,
                         # Loading message
                         div(id = "loading-orig_scatter_plot", h4("Plot")),
                         # The main app code goes here
                         hidden(div(id = "app-content")),
                         plotOutput("orig_scatter_plot"))
                ),
                p(),
                fluidRow(
                  column(4, downloadButton("download_plots_ns", label = "Download all current plots!")),
                  column(4, downloadButton("download_df_ns", label = "Download all current data frames!"))
                ),
                p()
                )
      )
    )
  )
)



###############################################################################
# Define function, server, that handles GUI events (View).
#
# Note this is a definition of a function whose very large body contains
# delarations of some objects, function calls and many inner function
# definitions - either helper functions of the form
#   function_name <- function(args) {body}
# or GUI callback registration calls of the form
#   observeEvent(input$widget, {body})
# The input object contains the widgets (buttons, menus, slider, etc.),
# the output object contains the rendered objects (e.g., map, tables, plots)
# the session object is used to distinguish concurrent server-side processes
# from different users when running in a hosted environment (rstudio-connect)
# and also to update client-side widgets.
###############################################################################

server <- function(input, output, session) {
  ASNAT_dprint("server called.\n")

  #https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user#18037912
  options(shiny.maxRequestSize = 500L * 1024L * 1024L)

  rsigserver_host <- unlist(strsplit(rsigserver_url, "/"))[[3L]]
  output$message_area <-
    renderPrint({cat("Welcome. ",
                     "R-version: ", paste0(R.version["major"], ".",
                                           R.version["minor"]),
                     " platform: ", ASNAT_platform(),
                     " host: ", Sys.info()[[4L]],
                     " wd: ", getwd(),
                     " pid: ", Sys.getpid(),
                     " session$token: ", session$token,
                     " rsigserver-host: ", rsigserver_host,
                     "\n(Network and file I/O messages will appear here).",
                     sep = "")})

  # Declare client-specific (non-shared) model object.
  # If running locally then just reference the global default_model
  # else running remote-hosted (e.g., rstudio-connect) so create a model object
  # that is session-specific - i.e., not shared but rather unique to each client
  # and also create and use a unique data sub-directory for temporary files so
  # that if clients are connected to the same server process (host+pid) and
  # have the same current directory then they will write to different
  # sub-directories to avoid interfering with each other:

  model <- NULL

  if (!ASNAT_is_remote_hosted) {
    model <- default_model

    # Check if there is a newer version of this program available for download:

    if (is_newer_version_available(model)) {
      url <- download_url(model)
      message <- sprintf("A newer version of ASNAT is available:\n%s\n", url)
      showNotification(message, duration = NULL, closeButton = TRUE,
                       type = "message")
    }
  } else {
    model <- ASNAT_Model(session$token)
  }

  stopifnot(!is.null(model))

  # Define and install function to be called upon app exit:

  on_quit <- function() {
    ASNAT_dprint("on_quit() called before exiting app.\n")

    if (!ASNAT_is_remote_hosted) {
      ASNAT_dprint("Saving model state.\n")
      model <<- save_state(model)
    } else { # Remove the temporary output directory on the server:
      directory <- output_directory(model)
      ASNAT_dprint("Removing output directory on the server %s.\n", directory)
      unlink(directory, recursive = TRUE, force = TRUE, expand = FALSE)
    }
  }



  if (!ASNAT_is_remote_hosted) {

    # https://github.com/daattali/advanced-shiny/blob/master/auto-kill-app/app.R

    session$onSessionEnded(stopApp)
    onStop(on_quit)
  }



  # Define and install callback function to be called for ASNAT warnings:

  show_warning <- function(failure) {
    ASNAT_dprint("show_warning() called.\n")
    output$message_area <- renderPrint({cat(failure, sep = "\n")})
    showNotification(failure, duration = NULL, closeButton = TRUE,
                     type = "warning")
  }

  ASNAT_set_warning_callback(show_warning)



  if (!rsigserver_is_reachable) {
    ASNAT_warning(paste0("Warning: ", rsigserver_host, " is unreachable.\n",
                         "No web-based data retrievals are possible.\n"))
  }



  # the_map (leaflet) is another client-specific (non-shared) variable:

  the_map <- NULL

  # Create the map:

  create_map <- function() {
    west <- west_bound(model)
    east <- east_bound(model)
    south <- south_bound(model)
    north <- north_bound(model)

    ASNAT_dprint("create_map: [%f, %f] [%f, %f]\n", west, east, south, north)

    # Note: minZoom = 2 prevents zooming out to 'replicated longitude' view -
    # continents are only shown once - as setMaxBounds() does not prevent this!
    # https://gis.stackexchange.com/questions/224383/
    # leaflet-maxbounds-doesnt-prevent-zooming-out

    result <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                options = list(minZoom = 2)) %>%
      leaflet::addTiles(group = "Roads", options = list(minZoom = 2)) %>%
      leaflet::addLayersControl(position = "topleft",
                                baseGroups = c("Roads", "Satellite")) %>%
      # setView(lng = -74.05, lat = 40.72, zoom = 11) %>%
      leaflet::fitBounds(west, south, east, north) %>%
      leaflet::setMaxBounds(-180.0, -90.0, 180.0, 90.0) %>%
      leaflet::addScaleBar(position = "topleft",
                           options = list(imperial = FALSE))
    return(result)
  }

  the_map <- create_map()
  output$map <- leaflet::renderLeaflet(the_map)


  # There is also a small site_neighbor_map that shows only the Dataset X
  # single selected site and its Dataset Y neighbors:

  the_site_neighbor_map <- create_map()
  output$site_neighbor_map <- leaflet::renderLeaflet(the_site_neighbor_map)


  # Define function to get colors of values:

  data_colors <- function(values, minimum, maximum, colormap,
                          na_color = "black") {
    count <- length(values)
    result <- rep(na_color, count)
    colormap_size <- length(colormap)
    colormap_size_2 <- colormap_size - 2L

    if (maximum > minimum) {
      the_range <- maximum - minimum

      for (index in seq_along(values)) {
        value <- values[[index]]

        if (!is.na(value)) {

          if (value > maximum) {
            result[[index]] <- colormap[[colormap_size]]
          } else if (value >= minimum) {
            normalized_value <- (value - minimum) / the_range
            normalized_index <-
              as.integer(trunc(colormap_size_2 * normalized_value)) + 2L

            if (normalized_index > colormap_size) {
              normalized_index <- colormap_size
            }

            result[[index]] <- colormap[[normalized_index]]
          }
        }
      }
    }

    ASNAT_dprint("data_color(minimum = %f, maximum = %f):\n", minimum, maximum)
    ASNAT_dprint("values:")
    ASNAT_debug(str, values)
    ASNAT_dprint("colormap:")
    ASNAT_debug(str, colormap)
    ASNAT_dprint("data_color(): result:")
    ASNAT_debug(str, result)
    return(result)
  }



  # Define function to draw 'glyph = source' legend on the map:
  # https://stackoverflow.com/questions/41512908/leaflet-legend-in-r-based-on-color-and-shape

  draw_glyph_source_legend_on_map <- function(a_map, sources) {
    ASNAT_dprint("In draw_glyph_source_legend_on_map(sources)\n")
    sources_html <- c()

    for (source in sources) {
      glyph <- map_glyph(source)
      file_name <- glyph_file_name(glyph, "black")
      source_label <- paste0("<img src='data:image/png;base64,",
                             base64enc::base64encode(file_name),
                             "' width='30'; height='30';> ",
                             source,
                             "<br/>")
      sources_html <- paste(sources_html, source_label, collapse = " ")
    }

    a_map <- addControl(map = a_map, html = sources_html,
                           position = "bottomright",
                           layerId = "source_legend")

    return(a_map)
  }



  # Define function to draw dataset-colored glyphs and legends on the map:

  draw_data_on_map <- function() {
    ASNAT_dprint("In draw_data_on_map()\n")
    timer <- ASNAT_start_timer()

    # Remove any existing data point glyphs and legends:

    # FIX: does not work! the_map <<- clearGroup(the_map, "glyphs_and_legends")
    # work-around:

    the_map <<- leaflet::clearMarkers(the_map)
    the_map <<- leaflet::clearControls(the_map)
    the_site_neighbor_map <<- leaflet::clearMarkers(the_site_neighbor_map)
    the_site_neighbor_map <<- leaflet::clearControls(the_site_neighbor_map)

    if (dataset_count(model) > 0L) {

      # Get the ranges for each units across all datasets:

      units <- dataset_units(model)
      count <- length(units)
      stopifnot(count == dataset_count(model))

      # Override actual data range with particular units ranges:

      minimums <- dataset_minimums(model)
      maximums <- dataset_maximums(model)

      if (count == 1L) {
        unit <- units[[1L]]

        if (unit == "%") {
          minimums <- c(0.0)
          maximums <- c(100.0)
        } else if (unit == "C") {
          minimums <- c(0.0)
          maximums <- c(35.0)
        } else if (unit != "hPa") {
          minimums <- c(0.0)
        }

      } else {
        stopifnot(length(minimums) == length(units))
        stopifnot(length(maximums) == length(units))

        for (index in 1L:count) {
          unit <- units[[index]]

          if (unit == "%") {
            minimum <- 0.0
            maximum <- 100.0
          } else if (unit == "C") {
            minimum <- 0.0
            maximum <- 35.0
          } else {

            if (unit != "hPa") {
              minimum <- 0.0
              minimums[[index]] <- minimum
            }

            minimum <- minimums[[index]]
            maximum <- maximums[[index]]

            for (index2 in index:count) {
              unit2 <- units[[index2]]

              if (unit2 == unit) {
                minimum2 <- minimums[[index2]]
                maximum2 <- maximums[[index2]]

                if (minimum2 < minimum) {
                  minimum <- minimum2
                }

                if (maximum2 > maximum) {
                  maximum <- maximum2
                }
              }
            }
          }

          for (index2 in 1:count) {
            unit2 <- units[[index2]]

            if (unit2 == unit) {
              minimums[[index2]] <- minimum
              maximums[[index2]] <- maximum
            }
          }
        }
      }

      show_mean_values <- show_mean_values_on_map(model)
      match_timestamp <- timestamp(model)
      model_timestep_size <- timestep_size(model)

      if (model_timestep_size == "hours") {
        match_timestamp <- substr(match_timestamp, 1L, 13L)
      } else {
        match_timestamp <- substr(match_timestamp, 1L, 10L)
      }

      ASNAT_dprint("Filtering data by model_timestamp = %s\n", match_timestamp)
      the_unit <- NULL
      sources <- c()

      for (index in 1L:count) {
        ASNAT_dprint("dataset #%d:\n", index)
        the_dataset <- dataset(model, index)
        the_coverage <- coverage(the_dataset)
        parts <- unlist(strsplit(the_coverage, ".", fixed = TRUE))
        source <- parts[[1L]]
        variable <- variable_name(the_dataset)
        source_variable <- source_variable(the_dataset)
        unit <- units[[index]]
        the_data_frame <- data_frame(the_dataset)

        timestep_data_frame <- the_data_frame

        if (!show_mean_values) {
          the_data_frame_timestamps <- the_data_frame[[1L]]
          matched_rows <- startsWith(the_data_frame_timestamps, match_timestamp)
          timestep_data_frame <- the_data_frame[matched_rows, ]
        }

        ASNAT_dprint("addMarkers() for %s(%s):\n", source_variable, unit)
        longitudes <- timestep_data_frame[[2L]]
        latitudes <- timestep_data_frame[[3L]]
        measure_column <- variable_column(the_dataset)
        values <- timestep_data_frame[[measure_column]]
        column_names <- colnames(timestep_data_frame)
        id_column <- ASNAT_site_column_index(column_names)
        ids <- timestep_data_frame[[id_column]]
        note_column <- length(column_names)
        notes <- timestep_data_frame[[note_column]]

        if (show_mean_values) {
          unique_site_ids <- unique(sort.int(ids))
          site_count <- length(unique_site_ids)
          site_longitudes <- rep(0, site_count)
          site_latitudes <- rep(0, site_count)
          site_values <- rep(0, site_count)
          site_notes <- rep("", site_count)

          for (site_index in seq_along(unique_site_ids)) {
            id <- unique_site_ids[[site_index]]
            site_rows <- which(ids == id)
            value <- mean(values[site_rows], na.rm = TRUE)
            first_site_index <- site_rows[[1L]]
            longitude <- longitudes[[first_site_index]]
            latitude <- latitudes[[first_site_index]]
            note <- notes[[first_site_index]]
            site_longitudes[[site_index]] <- longitude
            site_latitudes[[site_index]] <- latitude
            site_values[[site_index]] <- value
            site_notes[[site_index]] <- note
          }

          ids <- unique_site_ids
          longitudes <- site_longitudes
          latitudes <- site_latitudes
          values <- site_values
          notes <- site_notes
        }

        labels <-
          paste0(source_variable, " = ", values, " id: ", ids, " ", notes)
        minimum <- minimums[[index]]
        maximum <- maximums[[index]]
        selected_colormap_name <- legend_colormap(model)
        selected_colormap <- NULL
        use_aqi <- FALSE
        value_colors <- NULL

        if (selected_colormap_name == "AQI") {
          variable_units <- paste0(variable, "(", unit, ")")

          if (ASNAT_is_aqi_variable(variable_units)) {
            is_hourly <- timestep_size(model) == "hours"
            value_colors <- ASNAT_aqi_colors(variable_units, is_hourly, values)
          }
        }

        if (!is.null(value_colors)) {
          use_aqi <- TRUE
          selected_colormap <- ASNAT_aqi_colormap
        } else {

          if (selected_colormap_name == "gray") {
            selected_colormap <- the_gray_colormap
          } else if (selected_colormap_name == "blue") {
            selected_colormap <- the_blue_colormap
          } else if (selected_colormap_name == "colorsafe") {
            selected_colormap <- the_colorsafe_colormap
          } else if (selected_colormap_name == "viridis") {
            selected_colormap <- the_viridis_colormap
          } else {
            selected_colormap <- the_default_colormap
          }

          value_colors <-
            data_colors(values, minimum, maximum, selected_colormap)
        }

        glyph <- map_glyph(the_coverage)
        glyph_file_template <- glyph_file_name(glyph, "black")
        point_glyph_files <- c()

        for (color in value_colors) {
          this_glyph_file <-
            gsub(fixed = TRUE, "black", color, glyph_file_template)
          point_glyph_files <- append(point_glyph_files, this_glyph_file)
        }

        enable_hover <- FALSE # Too intrusive/cluttered.
        enable_popup <- TRUE

        the_map <<- addMarkers(map = the_map,
                               lng = longitudes,
                               lat = latitudes,
                               group = "glyphs_and_legends",
                               data = values,
                               label = if (enable_hover) labels else NULL,
                               popup = if (enable_popup) labels else NULL,
                               icon = makeIcon(iconUrl = point_glyph_files,
                                               iconWidth = glyph_size,
                                               iconHeight = glyph_size,
                                               popupAnchorX = 0L,
                                               popupAnchorY = 0L))

        unmatched_units <- is.null(the_unit) || the_unit != unit
        ASNAT_dprint("index = %d, unmatched_units = %d\n",
                     index, as.integer(unmatched_units))

        if (unmatched_units) {
          the_unit <- unit
          variable_and_units <-
            paste0(fancy_legend_label(variable),
                   "(", fancy_legend_label(unit), ")")
          tick_values <- NULL
          legend_colors <- NULL

          if (use_aqi) {
            tick_values <- rev(ASNAT_aqi_names)
            legend_colors <- rev(ASNAT_aqi_colormap)
          } else {
            the_minimum <- minimums[[index]]
            the_maximum <- maximums[[index]]
            the_range <- the_maximum - the_minimum
            the_increment <- the_range / (length(selected_colormap) - 2L)
            tick_values <- double(0)
            tick_values <- append(tick_values, -1.0)
            the_tick_value <- the_minimum

            for (index in 2L:length(selected_colormap) - 1L) {
              tick_values <- append(tick_values, the_tick_value)
              the_tick_value <- the_tick_value + the_increment
            }

            tick_values <- round(tick_values, digits = 2L)
            tick_values <- as.character(tick_values)
            tick_values[1L] <- paste0("<", round(the_minimum, digits = 2L))
            the_length <- length(tick_values)
            tick_values[the_length] <- paste0(">", round(the_maximum, digits = 2L))

            # Legend colors must be ordered top-down (high-low) so reverse them:

            tick_values <- rev(tick_values)
            legend_colors <- rev(selected_colormap)
          }

          ASNAT_dprint("calling addLegend()\n")
          the_map <<-
            addLegend(map = the_map, position = "bottomright",
                      group = "glyphs_and_legends",
                      values = NULL,
                      colors = legend_colors,
                      labels = tick_values,
                      title = variable_and_units,
                      opacity = 1.0)
        }

        sources <- append(sources, source)
      }

      # Add legend showing glyph = source:

      unique_sources <- unique(sources)
      the_map <<- draw_glyph_source_legend_on_map(the_map, unique_sources)
    }

    ASNAT_elapsed_timer("draw_data_on_map", timer)
    return(the_map)
  }



  # Define function to draw timestamp on the map:

  draw_timestamp_on_map <- function() {
    ASNAT_dprint("In draw_timestamp_on_map()\n")

    the_label <- NULL
    show_mean_values <- show_mean_values_on_map(model)

    if (show_mean_values) { # Draw the timestamp as YYYY-MM-DD - YYYY-MM-DD:
      first_timestamp <- first_timestamp(model)
      yyyy_mm_dd <- substr(first_timestamp, 1L, 10L)
      the_label <- yyyy_mm_dd
      last_timestamp <- last_timestamp(model)
      yyyy_mm_dd <- substr(last_timestamp, 1L, 10L)

      if (yyyy_mm_dd != the_label) {
        the_label <- paste(the_label, "-", yyyy_mm_dd)
      }

    } else { # Draw the timestamp as YYYY-MM-DD HH:00 UTC:
      the_timestamp <- timestamp(model)
      yyyy_mm_dd <- substr(the_timestamp, 1L, 10L)
      hh_mm <- substr(the_timestamp, 12L, 16L)
      the_label <- yyyy_mm_dd

      if (timestep_size(model) == "hours") {
        the_label <- paste(the_label, hh_mm)
      }
    }

    the_label <- paste(the_label, "UTC")

    timestamp_label <- paste0("<label>", the_label, "</label>")
    timestamp_html <- tags$div(HTML(timestamp_label))
    the_map <<- addControl(map = the_map, html = timestamp_html,
                           position = "bottomleft",
                           layerId = "timestamp")
    return(the_map)
  }



  # Update Purple Air Sites Menu for changed viewing area:

  update_purple_air_sites_menu <- function() {
    selection <- all_purple_air_sites

    is_valid_input <-
      shiny::isTruthy(input$purple_air_sites_menu) &&
      !is.na(input$purple_air_sites_menu) &&
      nchar(input$purple_air_sites_menu) > 0L

    if (is_valid_input) {
      selection <- input$purple_air_sites_menu
    }

    sites <- append(all_purple_air_sites, purple_air_sites(model))
    found_index <- grep(fixed = TRUE, selection, sites)

    if (length(found_index) == 1L) {
      selection <- sites[[found_index]]
    } else {
      selection <- all_purple_air_sites
    }

    ASNAT_dprint("updating selectizeInput() in server() with %d items.\n",
                 length(sites))

    freezeReactiveValue(input, "purple_air_sites_menu")
    updateSelectizeInput(session, "purple_air_sites_menu",
                         choices = sites, server = TRUE)
  }



  # Redraw/zoom the site_neighbors_map based on selected Dataset X site:

  update_site_neighbors_map <- function(selected_dataset_x_site) {

    the_site_neighbor_map <<- leaflet::clearMarkers(the_site_neighbor_map) # Glyphs.
    the_site_neighbor_map <<- leaflet::clearControls(the_site_neighbor_map) #  Legend.

    if (selected_dataset_x_site == 0L) {
      shinyjs::hide(id = "site_neighbor_map")
      shinyjs::hide(id = "show_site_labels")
    } else {
      shinyjs::show(id = "site_neighbor_map")
      shinyjs::show(id = "show_site_labels")

      # If a single site is selected then get sources and zoom to neighbors:

      model_comparison_data_frame <- comparison_data_frame(model)
      model_summary_y_data_frame <- summary_y_data_frame(model)

      if (!is.null(model_comparison_data_frame) &&
          !is.null(model_summary_y_data_frame) &&
          ASNAT_units_match(input$dataset_x_variable_menu[1L],
                            input$dataset_y_variable_menu[1L])) {

        dataset_x_name <- input$dataset_x_menu[1L]
        dataset_x <- find_dataset(model, dataset_x_name)
        source_x <- coverage_source(dataset_x)

        dataset_y_name <- input$dataset_y_menu[1L]
        dataset_y <- find_dataset(model, dataset_y_name)
        source_y <- coverage_source(dataset_y)

        sources <- source_x

        if (source_y != source_x) {
          sources <- c(source_x, source_y)
        }

        # Get Dataset Y neighbor sites:
        # model_comparison_data_frame columns are:
        # 1 = time, 2 = id_x, 3 = measure_x, 4 = id_y, 5 = measure_y, 6 = flag_y

        matched_rows <-
          which(model_comparison_data_frame[[2L]] == selected_dataset_x_site)
        subset_data_frame <- model_comparison_data_frame[matched_rows, ]
        y_neighbor_ids <- subset_data_frame[[4L]]
        y_neighbor_ids <- unique(sort.int(y_neighbor_ids))

        # Get location and note of selected_dataset_x_site:

        data_frame_x <- data_frame(dataset_x)
        column_names <- colnames(data_frame_x)
        site_column <- ASNAT_site_column_index(column_names)
        matched_rows <-
          which(data_frame_x[[site_column]] == selected_dataset_x_site)
        subset_data_frame_x <- data_frame_x[matched_rows, ]
        longitudes <- subset_data_frame_x[[2L]]
        latitudes <- subset_data_frame_x[[3L]]
        longitude_x <- longitudes[[1L]]
        latitude_x <- latitudes[[1L]]
        last_column <- ncol(subset_data_frame_x)
        notes_x <- subset_data_frame_x[[last_column]]
        note_x <- notes_x[[1L]]
        west <- longitude_x
        east <- longitude_x
        south <- latitude_x
        north <- latitude_x

        # Get location and note of neighbor sites:

        data_frame_y <- data_frame(dataset_y)
        column_names <- colnames(data_frame_y)
        site_column <- ASNAT_site_column_index(column_names)
        last_column <- ncol(data_frame_y)
        neighbor_sites <- c()
        neighbor_notes <- c()
        neighbor_longitudes <- c()
        neighbor_latitudes <- c()

        for (y_neighbor_id in y_neighbor_ids) {
          matched_rows <- which(data_frame_y[[site_column]] == y_neighbor_id)
          subset_data_frame_y <- data_frame_y[matched_rows, ]
          notes <- subset_data_frame_y[[last_column]]
          longitudes <- subset_data_frame_y[[2L]]
          latitudes <- subset_data_frame_y[[3L]]
          note <- notes[[1L]]
          longitude <- longitudes[[1L]]
          latitude <- latitudes[[1L]]
          neighbor_sites <- append(neighbor_sites, y_neighbor_id)
          neighbor_notes <- append(neighbor_notes, note)
          neighbor_longitudes <- append(neighbor_longitudes, longitude)
          neighbor_latitudes <- append(neighbor_latitudes, latitude)
          west <- min(west, longitude)
          east <- max(east, longitude)
          south <- min(south, latitude)
          north <- max(north, latitude)
        }

        # Zoom map to selected site and neighbors:

        delta <- 0.002
        west <- west - delta
        east <- east + delta
        south <- south - delta
        north <- north + delta

        the_site_neighbor_map <<-
          fitBounds(the_site_neighbor_map, west, south, east, north)

        # Draw selected site and on the map:

        coverage_x <- coverage(dataset_x)
        glyph <- map_glyph(coverage_x)
        glyph_file_template <- glyph_file_name(glyph, "black")
        glyph_icon <- makeIcon(iconUrl = glyph_file_template,
                               iconWidth = glyph_size,
                               iconHeight = glyph_size,
                               popupAnchorX = 0L,
                               popupAnchorY = 0L)

        model_show_site_labels <- show_site_labels(model)

        if (model_show_site_labels) {
          label_options <-
            labelOptions(noHide = TRUE, direction = "bottom",
                         style = list("font-size" = "9px"))
          the_site_neighbor_map <<-
            addMarkers(map = the_site_neighbor_map,
                         lng = longitude_x,
                         lat = latitude_x,
                         group = "glyphs_and_legends",
                         data = selected_dataset_x_site,
                         label = selected_dataset_x_site,
                         labelOptions = label_options,
                         icon = glyph_icon)
        }

        label_x <-
          paste0(source_x, " = id: ", selected_dataset_x_site, " ", note_x)
        enable_hover <- TRUE
        enable_popup <- TRUE
        label_options <- labelOptions(noHide = FALSE, direction = "bottom")

        the_site_neighbor_map <<-
          addMarkers(map = the_site_neighbor_map,
                     lng = longitude_x,
                     lat = latitude_x,
                     group = "glyphs_and_legends",
                     data = selected_dataset_x_site,
                     label = if (enable_hover) label_x else NULL,
                     popup = if (enable_popup) label_x else NULL,
                     labelOptions = label_options,
                     icon = glyph_icon)

        # Draw neighbor sites on the map:

        coverage_y <- coverage(dataset_y)
        glyph <- map_glyph(coverage_y)
        # Gray markers are harder to see in the map. Just use black and rely on
        # shape and optional id to discern single X site from Y neighbors.
        # neighbor_color <- the_gray_colormap[[length(the_gray_colormap) - 3L]]
        neighbor_color <- "black"
        glyph_file_template <- glyph_file_name(glyph, neighbor_color)
        glyph_icon <- makeIcon(iconUrl = glyph_file_template,
                               iconWidth = glyph_size,
                               iconHeight = glyph_size,
                               popupAnchorX = 0L,
                               popupAnchorY = 0L)
        id_labels <- neighbor_sites
        labels <-
          paste0(source_y, " = id: ", neighbor_sites, " ", neighbor_notes)

        if (model_show_site_labels) {
          label_options <-
            labelOptions(noHide = TRUE, direction = "bottom",
                         style = list("font-size" = "9px"))
          the_site_neighbor_map <<-
            addMarkers(map = the_site_neighbor_map,
                       lng = neighbor_longitudes,
                       lat = neighbor_latitudes,
                       group = "glyphs_and_legends",
                       data = neighbor_sites,
                       label = id_labels,
                       labelOptions = label_options,
                       icon = glyph_icon)
        }

        label_options <- labelOptions(noHide = FALSE, direction = "bottom")
        the_site_neighbor_map <<-
          addMarkers(map = the_site_neighbor_map,
                     lng = neighbor_longitudes,
                     lat = neighbor_latitudes,
                     group = "glyphs_and_legends",
                     data = neighbor_sites,
                     label = if (enable_hover) labels else NULL,
                     popup = if (enable_popup) labels else NULL,
                     labelOptions = label_options,
                     icon = glyph_icon)


        # Add legend showing glyph = source of comparison Dataset X, Dataset Y:

        the_site_neighbor_map <<-
          draw_glyph_source_legend_on_map(the_site_neighbor_map, sources)
      }
    }

    output$site_neighbor_map <- leaflet::renderLeaflet(the_site_neighbor_map)
  }



  # Update Neighbors Menu for changed neighbors:

  update_neighbors_menu <- function() {
    selection <- all_dataset_x_neighbor_sites
    sites <- selection

    is_valid_input <-
      shiny::isTruthy(input$neighbors_menu) &&
      !is.na(input$neighbors_menu) &&
      nchar(input$neighbors_menu) > 0L

    if (is_valid_input) {
      selection <- input$neighbors_menu
    }

    dataset_x_name <- input$dataset_x_menu[1L]

    if (!is.null(dataset_x_name)) {
      neighbor_ids_notes <- dataset_x_neighbors_id_note(model, dataset_x_name)

      if (!is.null(neighbor_ids_notes)) {
        sites <- append(all_dataset_x_neighbor_sites, neighbor_ids_notes)
      }
    }

    found_index <- grep(fixed = TRUE, selection, sites)
    selected_site <- 0L

    if (length(found_index) == 1L) {
      selection <- sites[[found_index]]
      parts <- unlist(strsplit(selection, " "))
      selected_site <- as.integer(parts[[1L]])
    } else {
      selection <- all_dataset_x_neighbor_sites
    }

    update_site_neighbors_map(selected_site)

    ASNAT_dprint("updating selectizeInput() in server() with %d items.\n",
                 length(sites))
    freezeReactiveValue(input, "neighbors_menu")
    updateSelectizeInput(session, "neighbors_menu",
                         choices = sites, server = TRUE)
  }



  observeEvent(input$noDataInfo, {
    shinyjs::runjs("swal.close();")
  })


  # Callback for show site labels checkbox:

  observeEvent(input$show_site_labels, {

    if (shiny::isTruthy(input$show_site_labels)) {
      show_site_labels(model) <<- TRUE
    } else {
      show_site_labels(model) <<- FALSE
    }

    is_valid_input <-
      shiny::isTruthy(input$neighbors_menu) &&
      !is.na(input$neighbors_menu) &&
      nchar(input$neighbors_menu) > 0L
    selected_site <- 0L

    if (is_valid_input) {
      parts <- unlist(strsplit(input$neighbors_menu, " "))
      first_part <- parts[[1L]]
      selected_site <- as.integer(first_part)
    }

    if (selected_site > 0L) {
      update_site_neighbors_map(selected_site)
    }
  })



  # Callback for purple_air_sites_menu:

  observeEvent(input$neighbors_menu, {
    ASNAT_dprint("In neighbors_menu callback.\n")
    is_valid_input <-
      shiny::isTruthy(input$neighbors_menu) &&
      !is.na(input$neighbors_menu) &&
      nchar(input$neighbors_menu) > 0L
    selected_site <- 0L

    if (is_valid_input) {
      parts <- unlist(strsplit(input$neighbors_menu, " "))
      first_part <- parts[[1L]]
      selected_site <- as.integer(first_part)
    }

    update_site_neighbors_map(selected_site)
  })



  # Callback for Quit button:

  observeEvent(input$quit, {
    on_quit()
    # https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R
    js$closeWindow()
    ASNAT_dprint("Calling stopApp().\n")
    stopApp()
  })



  # Callback for map pan/zoom events:

  observeEvent(input$map_bounds, {
    ASNAT_dprint("In callback for map_bounds:\n")
    bounds_list <- input$map_bounds
    west <- bounds_list$west
    east <- bounds_list$east
    south <- bounds_list$south
    north <- bounds_list$north
    ASNAT_dprint("map: [%f, %f] [%f, %f]\n", west, east, south, north)

    # Check and adjust map bounds to stay within 'non-replicated view'
    # (that does not cross the +/-180 line) then update the model bounds:

    adjust <- FALSE
    delta <- 0.001

    if (west == east) {
      east <- west + 1.0
      adjust <- TRUE
    }

    if (south == north) {
      north <- south + 1.0
      adjust <- TRUE
    }

    if (west <= -180.0) {
      west <- -179.0 + delta
      adjust <- TRUE
    }

    if (east >= 180.0) {
      east <- 179.0 - delta
      adjust <- TRUE
    }

    if (south <= -90.0) {
      south <- -89.0 + delta
      adjust <- TRUE
    }

    if (north >= 90.0) {
      north <- 89.0 - delta
      adjust <- TRUE
    }

    #if (adjust || (east - west) < delta || (north - south) < delta) {
      west <- west - delta
      east <- east + delta
      south <- south - delta
      north <- north + delta
    #}

    the_map <<- fitBounds(the_map, west, south, east, north)
    #the_map <<- flyToBounds(the_map, west, south, east, north)

    ASNAT_dprint("adjust = %d, setting to [%f, %f] [%f, %f]\n",
                 as.integer(adjust), west, east, south, north)
    west_bound(model) <<- west
    east_bound(model) <<- east
    south_bound(model) <<- south
    north_bound(model) <<- north
    ASNAT_dprint("set model: [%f, %f] [%f, %f]\n",
                 west_bound(model), east_bound(model),
                 south_bound(model), north_bound(model))
    update_purple_air_sites_menu()
  })



  # Callback for reset map bounds:

  observeEvent(input$reset_map, {
    ASNAT_dprint("In reset_map callback.\n")

    # Zoom to New York City - Long Island:

    west <- -74.3
    east <- -72.7
    south <- 40.5
    north <- 41.4
    longitude_center <- (west + east) * 0.5
    latitude_center <- (south + north) * 0.5
    ASNAT_dprint("map: [%f, %f] [%f, %f]\n", west, east, south, north)
    the_map <<- leaflet::fitBounds(the_map, west, south, east, north)
    the_map <<- leaflet::flyToBounds(the_map, west, south, east, north)
    the_map <<-
      leaflet::setView(the_map, longitude_center, latitude_center, zoom = 9L)
    the_site_neighbor_map <<-
      leaflet::fitBounds(the_site_neighbor_map, west, south, east, north)
    the_site_neighbor_map <<-
      leaflet::flyToBounds(the_site_neighbor_map, west, south, east, north)
    the_site_neighbor_map <<-
      leaflet::setView(the_site_neighbor_map,
                       longitude_center, latitude_center, zoom = 9L)
    timer <- ASNAT_start_timer()
    output$map <- leaflet::renderLeaflet(the_map)
    output$site_neighbor_map <- leaflet::renderLeaflet(the_site_neighbor_map)
    ASNAT_elapsed_timer("render map:", timer)
    west_bound(model) <<- west
    east_bound(model) <<- east
    south_bound(model) <<- south
    north_bound(model) <<- north
    ASNAT_dprint("model: [%f, %f] [%f, %f]\n",
                 west_bound(model), east_bound(model),
                 south_bound(model), north_bound(model))
    update_purple_air_sites_menu()
  })



  # Helper for map click events:

  print_map_click_info <- function(click_list, bounds_list) {

    longitude <- click_list$lng
    latitude <- click_list$lat
    west <- bounds_list$west
    east <- bounds_list$east
    south <- bounds_list$south
    north <- bounds_list$north
    the_width_height <- ASNAT_width_height(west, east, south, north)
    width_meters <- the_width_height$width
    height_meters <- the_width_height$height
    message_text <-
      sprintf("map %0.0fm x %0.0fm [%f, %f] [%f, %f] (%f, %f)\n",
              width_meters, height_meters,
              west, east, south, north, longitude, latitude)
    output$message_area <- renderPrint({cat(message_text, sep = "\n")})
  }



  # Callback for map click events:

  observeEvent(input$map_click, {
    ASNAT_dprint("In callback for map_click:\n")
    print_map_click_info(input$map_click, input$map_bounds)
  })


  # Callback for site_neighbor_map click events:

  observeEvent(input$site_neighbor_map_click, {
    ASNAT_dprint("In callback for site_neighbor_map_click:\n")
    print_map_click_info(input$site_neighbor_map_click,
                         input$site_neighbor_map_bounds)
  })



  # Callback for Start Date:

  observeEvent(input$start_date, {
    ASNAT_dprint("In start_date callback.\n")

    is_valid_input <-
      shiny::isTruthy(input$start_date) &&
      ASNAT_is_valid_date(input$start_date)

    model_start_date <- start_date(model)

    if (!is_valid_input) {
      tomorrow <- Sys.Date() + 1L
      freezeReactiveValue(input, "start_date")
      updateDateInput(session = session, inputId = "start_date",
                      value = model_start_date,
                      min = "1970-01-01", max = tomorrow)
    } else {
      start_date(model) <<- input$start_date

      # Draw data for timestep if there is any:

      if (dataset_count(model) > 0L) {
        the_map <<- draw_data_on_map()
      }

      the_map <<- draw_timestamp_on_map()
      timer <- ASNAT_start_timer()
      output$map <- leaflet::renderLeaflet(the_map)
      ASNAT_elapsed_timer("render map:", timer)
    }

    ASNAT_dprint("start_date(model) = %s\n", as.character(start_date(model)))
  })



  # Callback for Days:

  observeEvent(input$days, {
    ASNAT_dprint("In days callback.\n")
    ASNAT_debug(str, list(input$days))
    # BUG in numericInput(): min, max are not enforced!
    # https://github.com/rstudio/shiny/issues/3689
    # HACK work-around:

    is_valid_input <-
      shiny::isTruthy(input$days) &&
      !is.na(input$days) &&
      input$days >= 1L &&
      input$days <= 366L

    ASNAT_dprint("is_valid_input = %d\n", as.integer(is_valid_input))

    if (!is_valid_input) {
      model_days <- days(model)
      ASNAT_dprint("Reseting invalid input to model_days = %d\n", model_days)
      freezeReactiveValue(input, "days")
      updateNumericInput(session = session, inputId = "days",
                         value = model_days)
    } else {
      ASNAT_dprint("Setting days(model) = %d\n", input$days)
      days(model) <<- input$days
      ASNAT_dprint("Set days(model) = %d\n", days(model))

      last_model_timestep <- timesteps(model) - 1L

      is_valid_input <-
        shiny::isTruthy(input$timestep_slider) &&
        !is.na(input$timestep_slider) &&
        input$timestep_slider >= 0L &&
        input$timestep_slider <= last_model_timestep

      new_timestep_value <- last_model_timestep

      if (is_valid_input) {
        new_timestep_value <- input$timestep_slider
      }

      ASNAT_dprint("Reseting timestep_slider to new_timestep_value = %d\n",
                   new_timestep_value)
      freezeReactiveValue(input, "timestep_slider")
      updateSliderInput(session = session, inputId = "timestep_slider",
                        value = new_timestep_value,
                        min = 0L, max = last_model_timestep, step = 1L)
    }

    ASNAT_dprint("returning with:\n")
    ASNAT_dprint("timestep_size(model) = %s\n", timestep_size(model))
    ASNAT_dprint("days(model) = %d\n", days(model))
    ASNAT_dprint("timesteps(model) = %d\n", timesteps(model))
    ASNAT_dprint("timestep(model) = %d\n", timestep(model))
  })



  # Callback for Timestep Size:

  observeEvent(input$timestep_size, {
    ASNAT_dprint("In timestep_size callback.\n")
    ASNAT_debug(str, list(input$timestep_size))

    if (shiny::isTruthy(input$timestep_size) &&
        input$timestep_size == "hours" ||
        input$timestep_size == "days") {
      timestep_size(model) <<- input$timestep_size
      ASNAT_dprint("Set timestep_size(model) = %s\n", timestep_size(model))
      model_timestep <- timestep(model)
      last_model_timestep <- timesteps(model) - 1L
      ASNAT_dprint("Reseting timestep_slider to %d, last_model_timestep = %d\n",
                   model_timestep, last_model_timestep)
      freezeReactiveValue(input, "timestep_slider")
      updateSliderInput(session = session, inputId = "timestep_slider",
                        value = model_timestep,
                        min = 0L, max = last_model_timestep, step = 1L)
    }

    ASNAT_dprint("returning with:\n")
    ASNAT_dprint("timestep_size(model) = %s\n", timestep_size(model))
    ASNAT_dprint("days(model) = %d\n", days(model))
    ASNAT_dprint("timesteps(model) = %d\n", timesteps(model))
    ASNAT_dprint("timestep(model) = %d\n", timestep(model))
  })



  # Helper: clear tables and plots:

  clear_tables_and_plots <- function() {
    output$table1 <- renderTable({NULL})
    output$table2 <- renderTable({NULL})
    output$table3 <- renderTable({NULL})
    output$table4 <- renderTable({NULL})
    output$table5 <- renderTable({NULL})
    output$table3_interactive <- DT::renderDT({NULL})
    output$dataset_content_to_flag <- DT::renderDT({NULL})

    output$boxplot_x <- renderPlot({NULL})
    output$aqi_plot_x <- renderPlot({NULL})
    output$timeseriesplot_x <- renderPlot({NULL})
    output$boxplot_y <- renderPlot({NULL})
    output$aqi_plot_y <- renderPlot({NULL})
    output$timeseriesplot_y <- renderPlot({NULL})
    output$scatterplot_xy <- renderPlot({NULL})
    output$scatterplot_xy_unflagged <- renderPlot({NULL})
    output$aqi_plot <- renderPlot({NULL})
    output$aqi_plot_unflagged <- renderPlot({NULL})
    output$neighbor_statistics_boxplots <- renderPlot({NULL})
    output$neighbor_statistics_boxplots_unflagged <- renderPlot({NULL})

    output$interactive_boxplot_x <- renderPlot({NULL})
    output$interactive_aqi_plot_x <- renderPlot({NULL})
    output$interactive_timeseriesplot_x <- renderPlot({NULL})
    output$interactive_boxplot_y <- renderPlot({NULL})
    output$interactive_aqi_plot_y <- renderPlot({NULL})
    output$interactive_timeseriesplot_y <- renderPlot({NULL})
    output$interactive_scatterplot_xy <- renderPlot({NULL})
    output$interactive_scatterplot_xy_unflagged <- renderPlot({NULL})
    output$interactive_aqi_plot <- renderPlot({NULL})
    output$interactive_aqi_plot_unflagged <- renderPlot({NULL})
    output$interactive_neighbor_statistics_boxplots <- renderPlot({NULL})
    output$interactive_neighbor_statistics_boxplots_unflagged <-
      renderPlot({NULL})
  }



  # Callback for Delete All Loaded Data button:

  observeEvent(input$delete_data, {
    ASNAT_dprint("In delete_data callback.\n")
    model <<- delete_datasets(model)
    ASNAT_dprint("model dataset count = %d\n", dataset_count(model))

    # Clear the Dataset X/Y selections menu:

    freezeReactiveValue(input, "dataset_x_menu")
    freezeReactiveValue(input, "dataset_x_variable_menu")
    freezeReactiveValue(input, "dataset_y_menu")
    freezeReactiveValue(input, "dataset_y_variable_menu")
    freezeReactiveValue(input, "dataset_z_menu")
    freezeReactiveValue(input, "dataset_z_variable_menu")
    updateSelectInput(session = session, inputId = "dataset_x_menu",
                      choices = character(0), selected = character(0))
    updateSelectInput(session = session, inputId = "dataset_x_variable_menu",
                      choices = character(0), selected = character(0))
    updateSelectInput(session = session, inputId = "dataset_y_menu",
                      choices = character(0), selected = character(0))
    updateSelectInput(session = session, inputId = "dataset_y_variable_menu",
                      choices = character(0), selected = character(0))
    updateSelectInput(session = session, inputId = "dataset_z_menu",
                      choices = character(0), selected = character(0))
    updateSelectInput(session = session, inputId = "dataset_z_variable_menu",
                      choices = character(0), selected = character(0))

    # Clear the map data glyphs and legends:

    # FIX: the_map <<- leaflet::clearGroup(the_map, "glyphs_and_legends")
    the_map <<- leaflet::clearMarkers(the_map) # Clear all glyphs.
    the_map <<- leaflet::clearControls(the_map) # This clears legends and the timestamp!
    the_map <<- draw_timestamp_on_map() # So add timestamp back.

    # Also clear the_site_nieghbor_map

    the_site_neighbor_map <<- leaflet::clearMarkers(the_site_neighbor_map) # Glyphs.
    the_site_neighbor_map <<- leaflet::clearControls(the_site_neighbor_map) #  Legend.

    output$map <- leaflet::renderLeaflet(the_map)
    output$site_neighbor_map <- leaflet::renderLeaflet(the_site_neighbor_map)

    # FIX: Map sometimes zooms out and sometimes won't display or zoom
    # and bounds collapse to a point. User must click Reset Map button.

    clear_tables_and_plots()
  })


 input_focused <- reactiveVal(FALSE)


  # JavaScript to detect first focus on the input:

  observe({
    session$sendCustomMessage(type = "trackFocus",
                              message = list(id = "purple_air_key"))
  })


  # ObserveEvent to detect user foucs on the api key input field:

  observeEvent(input$purple_air_key_focused, {

    if (!input_focused()) {
      showModal(modalDialog(
        title = "PurpleAir Terms of Use",
          tagList(
            p("Please note the following terms when using PurpleAir data:"),
            tags$ul(
              tags$li("This data is available through an agreement with PurpleAir."),
              tags$li("Misuse or abuse could lead to issues with continued access."),
              tags$li("It is meant only for small chunks of data and must not be redistributed."),
              tags$li("Attribution is required.")
            ),
            p(strong("Key points from PurpleAir's Terms of Service:")),
            tags$ul(
              tags$li("End Users shall have no right to distribute the Licensee Product or any portion thereof, or otherwise share or distribute any results obtained therefrom."),
              tags$li("For the avoidance of doubt, and notwithstanding anything to the contrary herein, Licensee shall not distribute or otherwise make available the PurpleAir Data or the PurpleAir API to any third-party except as expressly authorized herein; and Licensee shall not distribute or otherwise make available the PurpleAir Data or Data Derivatives to any third-party except as incorporated in the Licensee Products. "),
            )
          ),
        footer = tagList(
          modalButton("I Understand")
        )
      ))
      input_focused(TRUE)
    }
  })



  # Callback for purple_air_key:

  observeEvent(input$purple_air_key, {
    ASNAT_dprint("In purple_air_key callback.\n")
    key <- input$purple_air_key

    if (ASNAT_is_conforming_purple_air_key(key)) {
      purple_air_key(model) <<- key
    } else {
      warning_text <- "Non-conforming PurpleAir Key."
      warning(warning_text, call. = FALSE, immediate. = TRUE, noBreaks. = FALSE)
      # toggleModal(session, "invalidKeyModal", toggle = "toggle")
    }
  })



  # Callback for purple_air_sites_menu:

  observeEvent(input$purple_air_sites_menu, {
    ASNAT_dprint("In purple_air_sites_menu callback.\n")
    is_valid_input <-
      shiny::isTruthy(input$purple_air_sites_menu) &&
      !is.na(input$purple_air_sites_menu) &&
      nchar(input$purple_air_sites_menu) > 0L

    current_sensor_id <- purple_air_sensor(model)
    new_sensor_id <- -1L

    if (is_valid_input) {
      parts <- unlist(strsplit(input$purple_air_sites_menu, " "))
      first_part <- parts[[1L]]
      new_sensor_id <- as.integer(first_part)
    }

    if (new_sensor_id >= 0L && new_sensor_id != current_sensor_id) {
      purple_air_sensor(model) <<- new_sensor_id
    }
  })



  # Callback for aqs_pm25_codes_menu:

  observeEvent(input$aqs_pm25_codes_menu, {
    ASNAT_dprint("In aqs_pm25_codes_menu callback.\n")
    is_valid_input <-
      shiny::isTruthy(input$aqs_pm25_codes_menu) &&
      !is.na(input$aqs_pm25_codes_menu) &&
      nchar(input$aqs_pm25_codes_menu) > 0L &&
      ASNAT_is_valid_aqs_pm25_codes(input$aqs_pm25_codes_menu)

    if (is_valid_input) {
      aqs_pm25_codes(model) <<- input$aqs_pm25_codes_menu
    }
  })



  # Callback for Maximum neighbor distance:

  observeEvent(input$maximum_neighbor_distance, {
    ASNAT_dprint("In maximum_neighbor_distance callback.\n")
    ASNAT_debug(str, list(input$maximum_neighbor_distance))

    is_valid_input <-
      shiny::isTruthy(input$maximum_neighbor_distance) &&
      !is.na(input$maximum_neighbor_distance) &&
      input$maximum_neighbor_distance >= 0.0 &&
      input$maximum_neighbor_distance <= 10000.0

    ASNAT_dprint("is_valid_input = %d\n", as.integer(is_valid_input))

    if (!is_valid_input) {
      model_maximum_neighbor_distance <- maximum_neighbor_distance(model)
      ASNAT_dprint("Reseting invalid input to maximum_neighbor_distance = %f\n",
                   model_maximum_neighbor_distance)
      freezeReactiveValue(input, "maximum_neighbor_distance")
      updateNumericInput(session = session,
                         inputId = "maximum_neighbor_distance",
                         value = model_maximum_neighbor_distance)
    } else {
      ASNAT_dprint("Setting maximum_neighbor_distance(model) = %f\n",
                   input$maximum_neighbor_distance)
      maximum_neighbor_distance(model) <<- input$maximum_neighbor_distance
      ASNAT_dprint("Set maximum_neighbor_distance(model) = %f\n",
                   maximum_neighbor_distance(model))
    }

    ASNAT_dprint("returning with:\n")
    ASNAT_dprint("maximum_neighbor_distance(model) = %f\n",
                 maximum_neighbor_distance(model))
  })



  # Helper function to update flag dataset menu and table:

  update_flag_dataset_menu_and_table <- function() {

    if (!is.null(input$dataset_y_menu[1L]) &&
        nchar(input$dataset_y_menu[1L]) > 0L &&
        input$dataset_y_menu[1L] != "none") {

      # Automatically choose Dataset Y to flag and show it:

      updateSelectInput(session = session,
                        inputId = "flag_dataset_menu",
                        selected = "Dataset Y")
    } else if (!is.null(input$dataset_x_menu[1L]) &&
               nchar(input$dataset_x_menu[1L]) > 0L) {
      updateSelectInput(session = session,
                        inputId = "flag_dataset_menu",
                        selected = "Dataset X")
    }

    show_dataset_content_to_flag()
  }



  # Helper function to update X/Y selection menus when datasets have changed:

  update_dataset_x_y_selections <- function() {
    ASNAT_dprint("update_dataset_x_y_selections called.\n")

    count <- dataset_count(model)
    ASNAT_dprint("  dataset_count(model)) = %d\n", count)

    # If no datasets are loaded then clear the X/Y selection menus:

    if (count == 0L) {
      freezeReactiveValue(input, "dataset_x_menu")
      freezeReactiveValue(input, "dataset_x_variable_menu")
      freezeReactiveValue(input, "dataset_y_menu")
      freezeReactiveValue(input, "dataset_y_variable_menu")
      freezeReactiveValue(input, "dataset_z_menu")
      freezeReactiveValue(input, "dataset_z_variable_menu")
      updateSelectInput(session = session, inputId = "dataset_x_menu",
                        choices = character(0), selected = character(0))
      updateSelectInput(session = session, inputId = "dataset_x_variable_menu",
                        choices = character(0), selected = character(0))
      updateSelectInput(session = session, inputId = "dataset_y_menu",
                        choices = character(0), selected = character(0))
      updateSelectInput(session = session, inputId = "dataset_y_variable_menu",
                        choices = character(0), selected = character(0))
      updateSelectInput(session = session, inputId = "dataset_z_menu",
                        choices = character(0), selected = character(0))
      updateSelectInput(session = session, inputId = "dataset_z_variable_menu",
                        choices = character(0), selected = character(0))
      output$dataset_content_to_flag <- DT::renderDT({NULL})
      ASNAT_dprint("cleared dataset_x/y_menus.\n")
    } else {

      # List possibly different datasets in the X/Y selection menus:

      loaded_coverages <- dataset_coverages(model)
      available_datasets_x <- loaded_coverages
      available_datasets_y <- c("none", loaded_coverages)
      available_datasets_z <- c("none", loaded_coverages)

      reset_x <-
        is.null(input$dataset_x_menu[1L]) ||
        nchar(input$dataset_x_menu[1L]) == 0L ||
        !(input$dataset_x_menu[1L] %in% available_datasets_x)

      ASNAT_dprint("reset_x = %d.\n", reset_x)

      if (reset_x) {

        # Reset dataset x to the 1st dataset:

        selected_dataset_x <- available_datasets_x[[1L]]
        dataset_x <- dataset(model, 1L)
        data_frame_x <- data_frame(dataset_x)
        column_names_x <- colnames(data_frame_x)
        available_variables_x <- ASNAT_variable_column_names(column_names_x)
        selected_column_x <- variable_column(dataset_x)
        selected_variable_x <- column_names_x[[selected_column_x]]

        # Must call freezeReactiveValue() for this update to work!
        # https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs
        # HACK: but if freezeReactiveValue() is called this routine loops!
        #freezeReactiveValue(input, "dataset_x_menu")
        #freezeReactiveValue(input, "dataset_x_variable_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_x_menu",
                          choices = available_datasets_x,
                          selected = selected_dataset_x)
        updateSelectInput(session = session,
                          inputId = "dataset_x_variable_menu",
                          choices = available_variables_x,
                          selected = selected_variable_x)

      } else if (length(available_datasets_x) > length(input$dataset_x_menu)) {

        # No change to selection, but expand choices to include new datasets:

        #freezeReactiveValue(input, "dataset_x_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_x_menu",
                          choices = available_datasets_x,
                          selected = input$dataset_x_menu)
      }

      reset_y <-
        is.null(input$dataset_y_menu[1L]) ||
        nchar(input$dataset_y_menu[1L]) == 0L ||
        (input$dataset_y_menu[1L] != "none" &&
        !(input$dataset_y_menu[1L] %in% available_datasets_y))

      ASNAT_dprint("reset_y = %d.\n", reset_y)

      if (reset_y) {

        # Reset dataset y to none:

        selected_dataset_y <- "none"

        #freezeReactiveValue(input, "dataset_y_menu")
        #freezeReactiveValue(input, "dataset_y_variable_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_y_menu",
                          choices = available_datasets_y,
                          selected = selected_dataset_y)
        updateSelectInput(session = session,
                          inputId = "dataset_y_variable_menu",
                          choices = character(0),
                          selected = character(0))

      } else if (input$dataset_y_menu[1L] != "none" &&
                 length(input$dataset_y_variable_menu) == 0L) {

        # User choose a valid dataset and variable menu needs updating:

        selected_dataset_y <- input$dataset_y_menu[1L]

        dataset_y_index <- which(loaded_coverages == selected_dataset_y)
        dataset_y <- dataset(model, dataset_y_index)
        data_frame_y <- data_frame(dataset_y)
        column_names_y <- colnames(data_frame_y)
        available_variables_y <- ASNAT_variable_column_names(column_names_y)
        selected_column_y <- variable_column(dataset_y)
        selected_variable_y <- column_names_y[[selected_column_y]]

        #freezeReactiveValue(input, "dataset_y_variable_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_y_variable_menu",
                          choices = available_variables_y,
                          selected = selected_variable_y)

      } else if (length(available_datasets_y) > length(input$dataset_y_menu)) {

        # No change to selection, but expand choices to include new datasets:

        #freezeReactiveValue(input, "dataset_y_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_y_menu",
                          choices = available_datasets_y,
                          selected = input$dataset_y_menu)
      }

      reset_z <-
        is.null(input$dataset_z_menu[1L]) ||
        nchar(input$dataset_z_menu[1L]) == 0L ||
        (input$dataset_z_menu[1L] != "none" &&
        !(input$dataset_z_menu[1L] %in% available_datasets_z))

      ASNAT_dprint("reset_z = %d.\n", reset_z)

      if (reset_z) {

        # Reset dataset z to none:

        selected_dataset_z <- "none"

        #freezeReactiveValue(input, "dataset_z_menu")
        #freezeReactiveValue(input, "dataset_z_variable_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_z_menu",
                          choices = available_datasets_z,
                          selected = selected_dataset_z)
        updateSelectInput(session = session,
                          inputId = "dataset_z_variable_menu",
                          choices = character(0),
                          selected = character(0))

      } else if (input$dataset_z_menu[1L] != "none" &&
                 length(input$dataset_z_variable_menu) == 0L) {

        # User choose a valid dataset and variable menu needs updating:

        selected_dataset_z <- input$dataset_z_menu[1L]

        dataset_z_index <- which(loaded_coverages == selected_dataset_z)
        dataset_z <- dataset(model, dataset_z_index)
        data_frame_z <- data_frame(dataset_z)
        column_names_z <- colnames(data_frame_z)
        available_variables_z <- ASNAT_variable_column_names(column_names_z)
        selected_column_z <- variable_column(dataset_z)
        selected_variable_z <- column_names_z[[selected_column_z]]

        #freezeReactiveValue(input, "dataset_z_variable_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_z_variable_menu",
                          choices = available_variables_z,
                          selected = selected_variable_z)

      } else if (length(available_datasets_z) > length(input$dataset_z_menu)) {

        # No change to selection, but expand choices to include new datasets:

        #freezeReactiveValue(input, "dataset_z_menu")
        updateSelectInput(session = session,
                          inputId = "dataset_z_menu",
                          choices = available_datasets_z,
                          selected = input$dataset_z_menu)
      }
    }

    update_flag_dataset_menu_and_table()

    ASNAT_dprint("update_dataset_x_y_selections returning.\n")
  }



  # Helper function for dataset_y/x_menu selections:

  handle_dataset_menu_selections <-
  function(dataset_menu, dataset_variable_menu, dataset_variable_menu_name) {
    ASNAT_dprint("In handle_dataset_menu_selections().\n")
    stopifnot(nchar(dataset_variable_menu_name) > 0L)

    is_valid_input <-
      shiny::isTruthy(dataset_menu) &&
      !is.na(dataset_menu) &&
      nchar(dataset_menu[1L]) >= 3L &&
      dataset_menu[1L] != "none" &&
      shiny::isTruthy(dataset_variable_menu) &&
      !is.na(dataset_variable_menu) &&
      nchar(dataset_variable_menu[1L]) > 0L &&
      dataset_count(model) > 0L

    ASNAT_dprint("is_valid_input = %d\n", as.integer(is_valid_input))

    if (!is_valid_input) {
      update_dataset_x_y_selections()
    } else {
      loaded_coverages <- dataset_coverages(model)
      dataset_name <- dataset_menu[[1L]]
      dataset_index <- which(loaded_coverages == dataset_name)

      if (dataset_index == 0L) {
        update_dataset_x_y_selections()
      } else {
        update_flag_dataset_menu_and_table()
        selected_dataset <- dataset(model, dataset_index)
        selected_column <- variable_column(selected_dataset)
        data_frame <- data_frame(selected_dataset)
        column_names <- colnames(data_frame)
        selected_variable <- column_names[[selected_column]]
        new_selected_variable <- dataset_variable_menu[[1L]]

        if (new_selected_variable != selected_variable) {
          available_variables <- ASNAT_variable_column_names(column_names)
          is_valid_input <- new_selected_variable %in% available_variables

          if (!is_valid_input) {
            freezeReactiveValue(input, dataset_variable_menu_name)
            updateSelectInput(session = session,
                              inputId = dataset_variable_menu_name,
                              choices = available_variables,
                              selected = selected_variable)
          } else {
            new_variable_column <-
              which(column_names == new_selected_variable)
            model <<-
              set_variable_column(model, dataset_index, new_variable_column)
            ASNAT_dprint("Set variable_column(selected_dataset) = %d\n",
                         variable_column(selected_dataset))

            # Update the map with new selected variable:

            the_map <<- draw_data_on_map()
            the_map <<- draw_timestamp_on_map()
            output$map <- leaflet::renderLeaflet(the_map)

            clear_tables_and_plots()
          }
        }
      }
    }

    ASNAT_dprint("handle_dataset_menu_selections() returning.\n")
  }



  # Callback for dataset_x_menu:

  observeEvent(input$dataset_x_menu, {
    ASNAT_dprint("In dataset_x_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_x_menu,
                                   input$dataset_x_variable_menu,
                                   "dataset_x_variable_menu")
  })



  # Callback for dataset_x_variable_menu:

  observeEvent(input$dataset_x_variable_menu, {
    ASNAT_dprint("In dataset_x_variable_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_x_menu,
                                   input$dataset_x_variable_menu,
                                   "dataset_x_variable_menu")
  })



  # Callback for dataset_y_menu:

  observeEvent(input$dataset_y_menu, {
    ASNAT_dprint("In dataset_y_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_y_menu,
                                   input$dataset_y_variable_menu,
                                   "dataset_y_variable_menu")
  })



  # Callback for dataset_y_variable_menu:

  observeEvent(input$dataset_y_variable_menu, {
    ASNAT_dprint("In dataset_y_variable_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_y_menu,
                                   input$dataset_y_variable_menu,
                                   "dataset_y_variable_menu")
  })



  # Callback for dataset_z_menu:

  observeEvent(input$dataset_z_menu, {
    ASNAT_dprint("In dataset_z_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_z_menu,
                                   input$dataset_z_variable_menu,
                                   "dataset_z_variable_menu")
  })



  # Callback for dataset_z_variable_menu:

  observeEvent(input$dataset_z_variable_menu, {
    ASNAT_dprint("In dataset_z_variable_menu callback.\n")
    handle_dataset_menu_selections(input$dataset_z_menu,
                                   input$dataset_z_variable_menu,
                                   "dataset_z_variable_menu")
  })



  # Callback function (called by model) to show progress during web retrievals:

  retrieving_url_callback <- function(url, coverage, percent_done) {
    stopifnot(!is.null(url))
    stopifnot(class(url) == "character")
    stopifnot(nchar(url) > 0L)
    stopifnot(!is.null(coverage))
    stopifnot(class(coverage) == "character")
    stopifnot(nchar(coverage) > 0L)
    stopifnot(!is.null(percent_done))
    stopifnot(class(percent_done) == "numeric")

    retrieving_message <-
      sprintf("Retrieving %s data from webservice (%d%% done). Please wait...",
              coverage, trunc(percent_done))

    showNotification(retrieving_message,
                     duration = NULL, closeButton = FALSE,
                     id = "message_popup", type = "message")
  }



  # Callback for Retrieve data button:

  observeEvent(input$retrieve_data, {
    ASNAT_dprint("In retrieve_data callback.\n")

    set_retrieving_url_callback(model) <<- retrieving_url_callback

    showNotification("Retrieving data - please wait...",
                     duration = NULL, closeButton = FALSE,
                     id = "message_popup", type = "message")

    # Have the model retrieve and store each selected coverage:

    coverages <- input$coverage_menu
    urls <- character(0)

    timer <- ASNAT_start_timer()

    for (coverage in coverages) {
      ASNAT_dprint("coverage = %s\n", coverage)


      # If user selected menu separator line then skip it:

      if (grepl("[^A-Za-z0-9_.]", coverage)) {
        next
      }

      if (length(grep(fixed = TRUE, "PurpleAir", coverage)) > 0L &&
          !ASNAT_is_conforming_purple_air_key(input$purple_air_key)) {
        message_text <-
          "Skipping PurpleAir retrieval due to non-conforming PurpleAir Key."
        urls <- append(urls, message_text)
        toggleModal(session, "invalidKeyModal", toggle = "toggle")
        next
      }

      model <<- retrieve_data(model, coverage)

      # If no data retrieved, show error modal and skip:

      if (!ok(model)) {
        urls <- append(urls, paste0("Failed to retrieve data for ", coverage))
        toggleModal(session, "invalid_retrieve_data_Modal", toggle = "toggle")
        next
      }

      urls <- append(urls, retrieved_urls(model))
      data_loaded(TRUE)
    }

    removeNotification("message_popup")

    count <- dataset_count(model)
    ASNAT_dprint("  dataset_count(model)) = %d\n", count)
    output$message_area <- renderPrint({cat(urls, sep = "\n")})

    # Draw data glyphs and legends and timestamp on the map:

    the_map <<- draw_data_on_map()
    the_map <<- draw_timestamp_on_map()
    ASNAT_dprint("calling leaflet::renderLeaflet.\n")
    timer <- ASNAT_start_timer()
    output$map <- leaflet::renderLeaflet(the_map)
    ASNAT_elapsed_timer("render map:", timer)

    update_dataset_x_y_selections()
    ASNAT_dprint("after update_dataset_x_y_selections()\n")
    ASNAT_elapsed_timer("retrieve_data callback:", timer)
    ASNAT_dprint("retrieve_data callback returning.\n")
  })



  # Callback for load_data_from_standard_file:

  observeEvent(input$load_data_from_standard_file, {
    ASNAT_dprint("In load_data_from_standard_file callback.\n")
    ok <- FALSE
    selected_file_data_frame <- input$load_data_from_standard_file

    if (length(selected_file_data_frame$datapath) == 1L) {
      selected_file_name <- selected_file_data_frame$datapath[[1L]]

      if (file.exists(selected_file_name)) {
        data_frame <- ASNAT_read_standard_file(selected_file_name)

        if (!is.null(data_frame)) {

          # Get short name of data_frame from short file name:

          short_file_name <- selected_file_data_frame$name[[1L]]
          len <- nchar(short_file_name)
          name <- ""

          for (index in 1L:len) {
            ch <- substr(short_file_name, index, index)

            if (ch %in% LETTERS || ch %in% letters) {
              name <- paste0(name, ch)
            } else {
              break
            }
          }

          if (!ASNAT_is_valid_dataset_name(name)) {
            name <- "fileset"
          }

          # Append data_frame and draw data:

          model <<- append_data_frame(model, data_frame, name)
          ok <- TRUE
          the_map <<- draw_data_on_map()
          the_map <<- draw_timestamp_on_map()
          output$map <- leaflet::renderLeaflet(the_map)
          update_dataset_x_y_selections()
        }
      }
    }

    if (!ok) {
      showNotification("Failed to load standard file.",
                       duration = NULL, closeButton = TRUE, type = "message")
    }
  })



  # Callback for show_import_fileset:

  observeEvent(input$show_import_fileset, {
    ASNAT_dprint("In show_import_fileset callback.\n")
    showModal(
      modalDialog(
        fileInput("from_files", label = "Load Files:", multiple = TRUE,
                  accept = list(".csv")),

        shinyBS::bsTooltip("from_files",
                           "Load data from sensor instrument files.",
                           options = tooltip_options),

        numericInput("sensor_longitude", label = "Sensor longitude:",
                     width = "100px",
                     value = 0.0,
                     min = -180.0,
                     max = 180.0),

        shinyBS::bsTooltip("sensor_longitude",
                           paste0("Enter longitude of sensor (e.g., -74.05)."),
                           options = tooltip_options),

        numericInput("sensor_latitude", label = "Sensor latitude:",
                     width = "100px",
                     value = 0.0,
                     min = -90.0,
                     max = 90.0),

        shinyBS::bsTooltip("sensor_latitude",
                           paste0("Enter latitude of sensor (e.g., 40.72)."),
                           options = tooltip_options),

        textInput("fileset_name", "Dataset Name:", value = "MySensor"),

        shinyBS::bsTooltip("fileset_name",
                           paste0("Short simple name for dataset ",
                                  "(letters only, e.g., HomeRubber)."),
                           placement = "top",
                           options = tooltip_options),

        title = "Load Data From Files",
        size = "xl",
        fade = FALSE,
        footer = tagList(modalButton("Cancel"),
                         actionButton("load_files",
                                    label = "Process and Load Selected Files"))
      ))
  })



  # Files from last selection:

  selected_files_data_frame <- NULL

  # Callback for from_files:

  observeEvent(input$from_files, {
    ASNAT_dprint("In from_file callback.\n")
    selected_files_data_frame <<- input$from_files

    if (length(selected_files_data_frame$datapath) > 0L) {

      for (file in selected_files_data_frame$datapath) {
        ASNAT_filter_text_file(file)
      }

      first_file <- selected_files_data_frame$datapath[[1L]]
      first_line <- try(silent = TRUE, readLines(first_file, 1L))
      known_format <- FALSE

      if (startsWith(first_line, "UTCDateTime,mac_address,firmware_ver,")) {
        known_format <- TRUE # PurpleAir
      } else if (startsWith(first_line,
              "sensor:model,sensor:package,sensor:capability,sensor:units")) {
        #known_format <- TRUE # AirBeam
        known_format <- FALSE # AirBeam UNIMPLEMENTED.
        hide(id = "sensor_longitude")
        hide(id = "sensor_latitude")
      }

      if (!known_format) {
        showNotification("Unknown sensor file type.",
                         duration = NULL, closeButton = TRUE, type = "message")
        removeModal()
      }
    }
  })



  # Callback for load_files:

  observeEvent(input$load_files, {
    ASNAT_dprint("In load_files callback.\n")

    if (!is.null(selected_files_data_frame)) {
      aggregate <- "none"

      if (timestep_size(model) == "hours") {
        aggregate <- "hourly"
      } else {
        aggregate <- "daily"
      }

      showNotification("Processing files - please wait...",
                       duration = NULL, closeButton = FALSE,
                       id = "message_popup", type = "message")

      longitude <- input$sensor_longitude
      latitude <- input$sensor_latitude

      if (!(longitude >= -180.0 && longitude <= 180.0)) {
        longitude <- -74.05
      }

      if (!(latitude >= -90.0 && longitude <= 90.0)) {
        longitude <- 40.72
      }

      data_frame <-
        ASNAT_import(selected_files_data_frame, aggregate, longitude, latitude)

      removeNotification("message_popup")

      if (!is.null(data_frame)) {
        name <- input$fileset_name

        if (!ASNAT_is_valid_dataset_name(name)) {
          name <- "MySensor"
        }

        model <<- append_data_frame(model, data_frame, name)
        the_map <<- draw_data_on_map()
        the_map <<- draw_timestamp_on_map()
        output$map <- leaflet::renderLeaflet(the_map)
        update_dataset_x_y_selections()
      } else {
        showNotification("Failed to load files.",
                         duration = NULL, closeButton = TRUE, type = "message")
      }

      selected_files_data_frame <- NULL
    }

    removeModal()
    ASNAT_dprint("load_files returning.\n")
  })



  update_map_timestep <- function(timestep) {
    stopifnot(isTruthy(timestep))
    stopifnot(timestep >= 0L)
    stopifnot(timestep < timesteps(model))
    timestep(model) <<- timestep

    # Draw data for timestep if there is any:

    if (dataset_count(model) > 0L) {
      the_map <<- draw_data_on_map()
    }

    the_map <<- draw_timestamp_on_map()
    timer <- ASNAT_start_timer()
    output$map <- leaflet::renderLeaflet(the_map)
    ASNAT_elapsed_timer("render map:", timer)
  }



  # Callback for Legend Colormap menu:

  observeEvent(input$legend_colormap_menu, {
    is_valid_input <-
      shiny::isTruthy(input$legend_colormap_menu) &&
      !is.na(input$legend_colormap_menu) &&
      nchar(input$legend_colormap_menu[1L]) > 0L &&
      (input$legend_colormap_menu[1L] == "default" ||
      input$legend_colormap_menu[1L] == "AQI" ||
      input$legend_colormap_menu[1L] == "gray" ||
      input$legend_colormap_menu[1L] == "blue" ||
      input$legend_colormap_menu[1L] == "colorsafe" ||
      input$legend_colormap_menu[1L] == "viridis")

    if (is_valid_input) {
      legend_colormap(model) <<- input$legend_colormap_menu[1L]
    }

    # If there is data then redraw it on map:

    if (dataset_count(model) > 0L) {
      model_timestep <- timestep(model)
      update_map_timestep(model_timestep)
    }
  })



  # Callback for fancy labels checkbox:

  observeEvent(input$use_fancy_labels, {

    if (shiny::isTruthy(input$use_fancy_labels)) {
      use_fancy_labels(model) <<- TRUE
    } else {
      use_fancy_labels(model) <<- FALSE
    }

    # If there is data then redraw it on map with appropriate legend label:

    if (dataset_count(model) > 0L) {
      model_timestep <- timestep(model)
      update_map_timestep(model_timestep)
    }
  })



  # Callback for show mean values checkbox:

  observeEvent(input$show_mean_values, {

    if (shiny::isTruthy(input$show_mean_values)) {
      show_mean_values_on_map(model) <<- TRUE
      shinyjs::hide(id = "timestep_slider")
    } else {
      show_mean_values_on_map(model) <<- FALSE
      shinyjs::show(id = "timestep_slider")
    }

    # If there is data then redraw it on map with appropriate timestamp label:

    if (dataset_count(model) > 0L) {
      model_timestep <- timestep(model)
      update_map_timestep(model_timestep)
    }
  })



  # Callback for Timestep Slider:

  observeEvent(input$timestep_slider, {
    ASNAT_dprint("In timestep_slider callback with timestep = %d.\n",
                 as.integer(input$timestep_slider))

    if (isTruthy(input$timestep_slider)) {
      the_timestep <- as.integer(input$timestep_slider)

      if (the_timestep >= 0L && the_timestep < timesteps(model)) {
        update_map_timestep(the_timestep)
      }
    }
  })



  # Zoom map to location of loaded datasets and set start_date, days to
  # date range of loaded datasets:

  zoom_map_to_data <- function() {
    count <- dataset_count(model)

    if (count > 0L) {
      extent <- data_extent(model)
      west <- extent$west
      east <- extent$east
      south <- extent$south
      north <- extent$north
      the_start_date <- extent$start_date
      the_end_date <- extent$end_date

      total_days <- 1L + as.integer(the_end_date) - as.integer(the_start_date)

      if (total_days > 366L) {
        total_days <- 366L
        the_end_date <- the_start_date + 365L
      }

      # Set start date and days to the date range of loaded data:

      start_date(model) <<- the_start_date
      days(model) <<- total_days
      tomorrow <- Sys.Date() + 1L
      freezeReactiveValue(input, "start_date")
      updateDateInput(session = session, inputId = "start_date",
                      value = start_date(model),
                      min = "1970-01-01", max = tomorrow)
      freezeReactiveValue(input, "days")
      updateNumericInput(session = session, inputId = "days",
                         value = days(model))

      delta_degrees <- 0.001
      west <- west - delta_degrees
      east <- east + delta_degrees
      south <- south - delta_degrees
      north <- north + delta_degrees
      west_bound(model) <<- west
      east_bound(model) <<- east
      south_bound(model) <<- south
      north_bound(model) <<- north
      the_map <<- leaflet::fitBounds(the_map, west, south, east, north)
      the_map <<- leaflet::flyToBounds(the_map, west, south, east, north)
      longitude_center <- (west + east) * 0.5
      latitude_center <- (south + north) * 0.5

      # Compute zoom level from width (this should be done by fitBounds()!):

      width <- east - west
      # Start with zoom level 15 which makes the window width almost 5km.
      zoom_width <- 0.045
      zoom_level <- 15L

      while (width > zoom_width && zoom_level > 2L) {
        zoom_width <- zoom_width + zoom_width
        zoom_level <- zoom_level - 1L
      }

      the_map <<-
        leaflet::setView(the_map, longitude_center, latitude_center,
                         zoom = zoom_level)
      output$map <- leaflet::renderLeaflet(the_map)
      update_purple_air_sites_menu()
    }
  }



  # Callback for Zoom To Data button:

  observeEvent(input$zoom_to_data, {
    ASNAT_dprint("In zoom_to_data callback\n")
    zoom_map_to_data()
  })



  # Helper function to create map image file:

  create_map_image_file <- function(map, width, height, file_name) {
    ASNAT_dprint("In create_map_image_file(%s)\n", file_name)
    timer <- ASNAT_start_timer()

    message <- paste0("Saving map image ", file_name, " - please wait...")
    showNotification(message,
                     duration = NULL, closeButton = FALSE,
                     id = "message_popup", type = "message")

    # FIX: How to make the saved image match that displayed here?
    # If no width/height arguments are passed to mapshot() then
    # the saved png image will always be 992 x 744.
    # If the main map is sized 992 x 744 then the png file will match the
    # leaflet view.
    # The 496 x 372 site_nighbor_map will be saved at 992 x 744 and match zoom
    # level but the detail of the png will be higher.
    # https://github.com/r-spatial/mapview/issues/487

    mapshot_result <-
      try(silent = TRUE, mapview::mapshot(map, file = file_name))

    ASNAT_debug(str, mapshot_result)

    #str(width)
    #str(height)
    # try(silent = TRUE,
    #     mapview::mapshot(map, file = file_name,
    #     vwidth = width, wheight = height))

    output$message_area <- renderPrint({cat(file_name, sep = "\n")})
    removeNotification("message_popup")
    ASNAT_elapsed_timer("create_map_image_file:", timer)
    return(file_name)
  }



  # Callback for Save Map Image button - when not remote-hosted:

  observeEvent(input$save_map_image, {
    check_set_reset_output_directory()
    model_output_directory <- output_directory(model)
    file_name <- NULL

    if (show_mean_values_on_map(model)) {

      # Use file name with date range _YYYY-MM-DD_YYYY-MM-DD.png

      first_timestamp <- first_timestamp(model)
      yyyy_mm_dd1 <- substr(first_timestamp, 1L, 10L)
      last_timestamp <- last_timestamp(model)
      yyyy_mm_dd2 <- substr(last_timestamp, 1L, 10L)
      timestamp <- yyyy_mm_dd1

      if (yyyy_mm_dd1 != yyyy_mm_dd2) {
        timestamp <- paste0(timestamp, "_", yyyy_mm_dd2)
      }

      file_name <-
        paste0(model_output_directory, "/map_image_", timestamp, ".png")

    } else { # Use file name with timestep _ttt.png
      model_timestep <- timestep(model)
      file_name <-
        paste0(model_output_directory,
               sprintf("/map_image_%04d.png", model_timestep))
    }

    create_map_image_file(the_map, the_map_width, the_map_height, file_name)
  })



  # Callback for Download Image button - when remote-hosted:

  output$download_map_image <- downloadHandler(
    filename = function() { sprintf("map_image_%04d.png", timestep(model)) },
    content = function(file) {
      check_set_reset_output_directory()
      model_output_directory <- output_directory(model)
      file_name <- NULL

      if (show_mean_values_on_map(model)) {

        # Use file name with date range _YYYY-MM-DD_YYYY-MM-DD.png

        first_timestamp <- first_timestamp(model)
        yyyy_mm_dd1 <- substr(first_timestamp, 1L, 10L)
        last_timestamp <- last_timestamp(model)
        yyyy_mm_dd2 <- substr(last_timestamp, 1L, 10L)
        timestamp <- yyyy_mm_dd1

        if (yyyy_mm_dd1 != yyyy_mm_dd2) {
          timestamp <- paste0(timestamp, "_", yyyy_mm_dd2)
        }

        file_name <-
          paste0(model_output_directory, "/map_image_", timestamp, ".png")

      } else { # Use file name with timestep _ttt.png
        model_timestep <- timestep(model)
        file_name <-
          paste0(model_output_directory,
                 sprintf("/map_image_%04d.png", model_timestep))
      }

      image_file_name <-
        create_map_image_file(the_map, the_map_width, the_map_height, file_name)
      file.copy(from = image_file_name, to = file, overwrite = TRUE)
    },
    contentType = "image/png"
  )



  # Helper function to create movie file:

  create_movie_file <- function() {
    ASNAT_dprint("In create_movie_file().\n")
    check_set_reset_output_directory()
    timer <- ASNAT_start_timer()
    model_timesteps <-
      ifelse(show_mean_values_on_map(model), 1L, timesteps(model))
    model_timesteps_1 <- model_timesteps - 1L
    model_timestep <- timestep(model)
    model_output_directory <- output_directory(model)
    model_data_directory <- data_directory(model)
    model_app_directory <- app_directory(model)
    image_file_names <- vector(mode = "integer", length = model_timesteps)
    total_steps <- model_timesteps + 1L

    for (the_timestep in 0L:model_timesteps_1) {
      message <-
        sprintf("Saving map images (%0.0f%% done) - please wait...",
                100.0 * the_timestep / total_steps)
      showNotification(message, duration = NULL, closeButton = FALSE,
                       id = "message_popup", type = "message")
      update_map_timestep(the_timestep)
      output_image_file_name <-
        paste0(model_data_directory,
               sprintf("/map_image_%04d.png", the_timestep))
      output_image_file_name <-
        create_map_image_file(the_map, the_map_width, the_map_height,
                              output_image_file_name)
      image_file_names[[the_timestep + 1L]] <- output_image_file_name
    }

    timestep(model) <<- model_timestep

    # Create movie file from sequence of png images:

    output_movie_file_name <- paste0(model_output_directory, "/map_movie.mp4")

    if (file.exists(output_movie_file_name)) {
      # unlink(output_movie_file_name)
      output_movie_file_name <- ASNAT_unique_file_name(output_movie_file_name)
    }

    command_quote <- "'"

    if (.Platform$OS.type == "windows") {
      command_quote <- '"'
    }

    platform <- ASNAT_platform()
    command <-
      sprintf(paste0("%s/%s/bin/ffmpeg -y -f image2 -r 3 -i ",
                     "%s%s/map_image_%%04d.png%s ",
                     "-vcodec mpeg4 -pix_fmt yuv420p -qscale:v 1 %s%s%s"),
                     model_app_directory, platform,
                     command_quote, model_data_directory, command_quote,
                     command_quote, output_movie_file_name, command_quote)
    cat(command, "\n")
    system(command)
    unlink(image_file_names)
    output$message_area <-
      renderPrint({cat(command, "\n", output_movie_file_name)})
    removeNotification("message_popup")
    ASNAT_elapsed_timer("create_movie_file:", timer)
    return(output_movie_file_name)
  }



  # Callback for Save Map Movie button:

  observeEvent(input$save_map_movie, {
    ASNAT_dprint("In save_map_movie callback.\n")
    create_movie_file()
  })



  # Callback for Download Movie button - when remote-hosted:

  output$download_map_movie <- downloadHandler(
    filename = "map_movie.mp4",
    content = function(file) {
      movie_file_name <- create_movie_file()
      output$message_area <-
        renderPrint({cat(movie_file_name, file, sep = "\n")})
      file.copy(from = movie_file_name, to = file, overwrite = TRUE)
    },
    contentType = "video/mp4"
  )



  # Check/set/reset output_directory:
  # Check if input$output_directory is valid:
  # if not valid then reset it
  # else if it does not exist then create it and update model
  # else it exists and update model.

  check_set_reset_output_directory <- function() {

    if (!ASNAT_is_remote_hosted) {

      # Check that the directory name is valid and does not contain spaces:

      is_valid_input <-
        shiny::isTruthy(input$output_directory) &&
        length(grep("[ ]", input$output_directory)) == 0L

      # If valid and non-existent then try to create it:

      if (is_valid_input) {

        if (!dir.exists(input$output_directory)) {
          dir.create(input$output_directory)
        }

        if (!dir.exists(input$output_directory)) {
          is_valid_input <- FALSE
        }
      }

      if (is_valid_input) {
        output_directory(model) <<- input$output_directory
      } else {

        # Reset GUI to model state value:

        model_output_directory <- output_directory(model)
        freezeReactiveValue(input, "output_directory")
        updateTextInput(session = session, inputId = "output_directory",
                          value = model_output_directory)
      }
    }

    ASNAT_dprint("output_directory(model) = %s\n", output_directory(model))
  }



  # Callback for output_format:

  observeEvent(input$output_format_menu, {
    is_valid_input <-
      shiny::isTruthy(input$output_format_menu) &&
      shiny::isTruthy(input$output_format_menu[1L]) &&
      (input$output_format_menu[1L] == "csv" ||
         input$output_format_menu[1L] == "tsv" ||
         input$output_format_menu[1L] == "json")

    if (!is_valid_input) {
      model_output_format <- output_format(model)
      freezeReactiveValue(input, "output_format")
      updateTextInput(session = session, inputId = "output_format",
                      value = model_output_format)
    } else {
      output_format(model) <<- input$output_format_menu[1L]
    }

    ASNAT_dprint("output_format(model) = %s\n", output_format(model))
  })



  # Callback for Save Data button - when not remote hosted:

  observeEvent(input$save_data, {
    check_set_reset_output_directory()

    if (dataset_count(model) > 0L) {
      model <<- save_datasets(model)
    }

    output$message_area <- renderPrint({cat(saved_files(model), sep = "\n")})
  })



  # Callback for Download Data button - when remote-hosted:

  output$download_data <- downloadHandler(
    filename = "ASNAT_data.zip",
    content = function(file) {

      if (dataset_count(model) > 0L) {
        check_set_reset_output_directory()
        model <<- save_datasets(model)
        model_saved_files <- saved_files(model)
        output$message_area <-
          renderPrint({cat(model_saved_files, file, sep = "\n")})
        #utils::zip(file, files = model_saved_files, flags = "-j")
        zip::zip(file, files = model_saved_files,
                 recurse = FALSE, include_directories = FALSE,
                 mode = "cherry-pick")
      }
    },
    contentType = "application/zip"
  )



  # Helper function: do we have selected datasets?

  have_datasets <- function() {
    have_x <-
      shiny::isTruthy(input$dataset_x_menu) &&
      shiny::isTruthy(input$dataset_x_menu[1L]) &&
      nchar(input$dataset_x_menu[1L]) > 0L &&
      shiny::isTruthy(input$dataset_x_variable_menu) &&
      shiny::isTruthy(input$dataset_x_variable_menu[1L]) &&
      nchar(input$dataset_x_variable_menu[1L]) > 0L

    have_y <-
      have_x &&
      shiny::isTruthy(input$dataset_y_menu) &&
      shiny::isTruthy(input$dataset_y_menu[1L]) &&
      nchar(input$dataset_y_menu[1L]) > 0L &&
      input$dataset_y_menu[1L] != "none" &&
      shiny::isTruthy(input$dataset_y_variable_menu) &&
      shiny::isTruthy(input$dataset_y_variable_menu[1L]) &&
      nchar(input$dataset_y_variable_menu[1L]) > 0L

    have_z <-
      have_x &&
      shiny::isTruthy(input$dataset_z_menu) &&
      shiny::isTruthy(input$dataset_z_menu[1L]) &&
      nchar(input$dataset_z_menu[1L]) > 0L &&
      input$dataset_z_menu[1L] != "none" &&
      shiny::isTruthy(input$dataset_z_variable_menu) &&
      shiny::isTruthy(input$dataset_z_variable_menu[1L]) &&
      nchar(input$dataset_z_variable_menu[1L]) > 0L

    return(list(x = have_x, y = have_y, z = have_z))
  }



  # Possibly change variable column in a (shallow) copy of a returned dataset:

  set_dataset_variable <- function(dataset_name, dataset_variable) {
    coverages <- dataset_coverages(model)
    dataset_index <- which(coverages == dataset_name)
    dataset <- NULL

    if (dataset_index > 0L) {
      dataset <- dataset(model, dataset_index)
      data_frame <- data_frame(dataset)
      column_names <- colnames(data_frame)
      variable_column <- which(column_names == dataset_variable)

      if (variable_column(dataset) != variable_column) {
        variable_column(dataset) <- variable_column
      }
    }

    return(dataset)
  }



  # Helper function to get selected variable from the selected
  # flag database.

  get_variable_by_flag_dataset <- function() {
    result <- ""
    is_valid_input <-
      shiny::isTruthy(input$flag_dataset_menu) &&
      !is.na(input$flag_dataset_menu) &&
      nchar(input$flag_dataset_menu) > 0L

    if (is_valid_input) {
      selection <- input$flag_dataset_menu[1L]
      have_datasets_result <- have_datasets()
      have_x <- have_datasets_result$x
      have_y <- have_datasets_result$y

      if (selection == "Dataset X") {

        if (have_x) {
          result <- input$dataset_x_variable_menu[1L]
        }
      } else if (have_y) {
        result <- input$dataset_y_variable_menu[1L]
      }

    }

    return(result)
  }



  # Helper function to get index of flag dataset or 0 if none:

  get_flag_dataset_index <- function() {
    result <- 0L
    is_valid_input <-
      shiny::isTruthy(input$flag_dataset_menu) &&
      !is.na(input$flag_dataset_menu) &&
      nchar(input$flag_dataset_menu) > 0L

    if (is_valid_input) {
      selection <- input$flag_dataset_menu[1L]
      have_datasets_result <- have_datasets()
      have_x <- have_datasets_result$x
      have_y <- have_datasets_result$y
      flag_dataset_name <- NULL

      if (selection == "Dataset X") {

        if (have_x) {
          flag_dataset_name <- input$dataset_x_menu[1L]
        }
      } else if (have_y) {
        flag_dataset_name <- input$dataset_y_menu[1L]
      }

      if (!is.null(flag_dataset_name)) {
        coverages <- dataset_coverages(model)
        result <- which(coverages == flag_dataset_name)
      }
    }

    stopifnot(length(result) == 1L)
    return(result)
  }



  # Helper function to show dataset content to flag:

  show_dataset_content_to_flag <- function() {
    dataset_index <- get_flag_dataset_index()

    if (dataset_index > 0L) {
      the_dataset <- dataset(model, dataset_index)
      data_frame <- data_frame(the_dataset)

      # BUG in DT::renderDT() - displayed row numbers are wrong (not 1-N)
      # after deleting and re-loading data!
      # HACK workaround: removing the row names from this (shallow copy) of
      # data_frame somehow makes the table show correct (1-N) row numbers.

      rownames(data_frame) <- NULL

      # options_list =
      #   list( scrollY = "500px",     # Enable vertical scroll w/ 500px height.
      #         scrollX = TRUE,        # Enable horizontal scrolling if needed.
      #         scroller = TRUE,       # Enable scroller extension.
      #         deferRender = TRUE,    # Improve performance.
      #         scrollCollapse = TRUE, # Collapse empty scroll space.
      #         pageLength = -1,       # Show all rows.
      #         dom = "frti" # Filter, processing, table, info (no pagination).
      #   )

      options_list = 
      list(
        scrollY = "500px",    
        scrollX = TRUE,       
        deferRender = TRUE,    
        scrollCollapse = TRUE, 
        pageLength = 10,  
        lengthMenu = list(c(10, 25, 50, 100, 200, 500, 1000, 2500, 5000, 10000, -1), c(10, 25, 50, 100, 200, 500, 1000, 2500, 5000, 10000, "All")),  
        dom = "lfrtip" # Filter, processing, table, info (no pagination).
      )




      output$dataset_content_to_flag <-
        DT::renderDT(data_frame, options = options_list)

    } else {
      output$dataset_content_to_flag <- DT::renderDT({NULL})
    }
  }



  # Callback for flag_dataset_menu:

  observeEvent(input$flag_dataset_menu, {
    ASNAT_dprint("In flag_dataset_menu callback.\n")
    show_dataset_content_to_flag()
  })



  # Helper function to set or clear 99 value of flagged column of selected rows:

  set_or_clear_99_flag_of_selected_rows <- function(flag) {
    stopifnot(flag == 0L || flag == 99L)
    selected_rows <- sort.int(input$dataset_content_to_flag_rows_selected)

    if (length(selected_rows) > 0L) {
      dataset_index <- get_flag_dataset_index()
      stopifnot(dataset_index > 0L)
      the_dataset <- dataset(model, dataset_index)
      data_frame <- data_frame(the_dataset)
      column_names <- colnames(data_frame)
      flagged_column <- ASNAT_flagged_column_index(column_names)
      stopifnot(flagged_column > 0L)
      flagged <- data_frame[selected_rows, flagged_column]

      if (flag == 99L) {
        flagged <-
          vapply(flagged, ASNAT_include_flag, c(""), 99L, USE.NAMES = FALSE)
      } else {
        flagged <-
          vapply(flagged, ASNAT_exclude_flag, c(""), 99L, USE.NAMES = FALSE)
      }

      data_frame[selected_rows, flagged_column] <- flagged
      flagged <- data_frame[[flagged_column]]
      model <<- replace_flagged_column(model, dataset_index, flagged)
      show_dataset_content_to_flag()
    }
  }



  # Callback for exclude_99_flagged:

  observeEvent(input$exclude_99_flagged, {
    ASNAT_dprint("In exclude_99_flagged callback.\n")
    set_or_clear_99_flag_of_selected_rows(0L)
  })



  # Callback for include_99_flagged:

  observeEvent(input$include_99_flagged, {
    ASNAT_dprint("In include_99_flagged callback.\n")
    set_or_clear_99_flag_of_selected_rows(99L)
  })



  # Callback for load_flag_conditions:

  observeEvent(input$load_flag_conditions, {
    ASNAT_dprint("In load_flag_conditions callback.\n")
    ok <- FALSE
    selected_file_data_frame <- input$load_flag_conditions

    if (length(selected_file_data_frame$datapath) == 1L) {
      selected_file_name <- selected_file_data_frame$datapath[[1L]]

      if (file.exists(selected_file_name)) {
        flag_conditions <- try(silent = TRUE, readLines(selected_file_name))

        if (class(flag_conditions) == "character" &&
            length(flag_conditions) > 0L) {
          updateTextAreaInput(session = session, inputId = "flag_conditions",
                              value = paste(collapse = "\n", flag_conditions))
          ok <- TRUE
        }
      }
    }

    if (!ok) {
      showNotification("Failed to load flags file.",
                       duration = NULL, closeButton = TRUE, type = "message")
    }
  })



  # Callback for Refresh Flagging button:

  observeEvent(input$refresh_flagging, {
    summarize_and_compare_datasets_x_y()

    # Clear tables 1-5
    output$table1 <- renderTable(NULL)
    output$table2 <- renderTable(NULL)
    output$table3 <- renderTable(NULL)
    output$table4 <- renderTable(NULL)
    output$table5 <- renderTable(NULL)
  })



  # Callback for Apply Flagging button:

  observeEvent(input$apply_flagging, {

    if (shiny::isTruthy(input$flag_dataset_menu) &&
        nchar(input$flag_dataset_menu) > 0L &&
        shiny::isTruthy(input$flag_conditions) &&
        nchar(input$flag_conditions) > 0L) {
      have_datasets_result <- have_datasets()
      have_x <- have_datasets_result$x
      have_y <- have_datasets_result$y
      flag_dataset_name <- NULL

      if (input$flag_dataset_menu == "Dataset X") {

        if (have_x) {
          flag_dataset_name <- input$dataset_x_menu[1L]
        }
      } else if (have_y) {
        flag_dataset_name <- input$dataset_y_menu[1L]
      }

      if (!is.null(flag_dataset_name)) {
        coverages <- dataset_coverages(model)
        dataset_index <- which(coverages == flag_dataset_name)

        if (dataset_index > 0L) {
          showNotification("Applying flag conditions...",
                            duration = NULL, closeButton = FALSE,
                            id = "message_popup", type = "message")
          flag_conditions <- unlist(strsplit(input$flag_conditions, "\n"))
          model <<-
            apply_flag_conditions(model, dataset_index, flag_conditions)
          show_dataset_content_to_flag()
          removeNotification("message_popup")
        }
      }
    }
  })



  # Callback for Clear Flagging button:
  observeEvent(input$clear_flagging, {

    # Show notification that clearing is in progress
    showNotification("Clearing all flags...",
                    duration = NULL, closeButton = FALSE,
                    id = "clear_flags_popup", type = "message")

    # Get the dataset index
    dataset_index <- get_flag_dataset_index()

    if (dataset_index > 0L) {
      # Get the dataset
      the_dataset <- dataset(model, dataset_index)
      data_frame <- data_frame(the_dataset)

      # Clear the flagged column (keep_99 = FALSE to clear all flags including 99)
      cleared_data_frame <- ASNAT_clear_flagged_column(data_frame, keep_99 = FALSE)

      # Update the model with cleared flags
      flagged <- cleared_data_frame[["flagged(-)"]]
      model <<- replace_flagged_column(model, dataset_index, flagged)

      # Refresh the display
      show_dataset_content_to_flag()

      # Show success notification
      showNotification("All flags have been cleared.",
                      duration = 3, closeButton = TRUE,
                      type = "message")
    } else {
      # Show warning if no dataset is selected
      showNotification("Please select a dataset to clear flags.",
                      duration = 3, closeButton = TRUE,
                      type = "warning")
    }

    # Remove the "in progress" notification
    removeNotification("clear_flags_popup")
  })



  # Callback for Save Flag Conditions button - when not remote hosted:

  observeEvent(input$save_flag_conditions, {
    have_flag_conditions <-
      shiny::isTruthy(input$flag_conditions) &&
      nchar(input$flag_conditions) > 0L

    if (have_flag_conditions) {
      check_set_reset_output_directory()
      flags_file_name <- paste0(output_directory(model), "/flags.txt")
      cat(append = TRUE, sep = "\n", file = flags_file_name,
          input$flag_conditions)
      output$message_area <- renderPrint({flags_file_name})
    }
  })



  # Callback for Download Flag Conditions button - when remote-hosted:

  output$download_flag_conditions <- downloadHandler(
    filename = "flags.txt",
    content = function(file) {
      have_flag_conditions <-
        shiny::isTruthy(input$flag_conditions) &&
        nchar(input$flag_conditions) > 0L

      if (have_flag_conditions) {
        check_set_reset_output_directory()
        flags_file_name <- paste0(output_directory(model), "/flags.txt")
        cat(append = TRUE, sep = "\n", file = flags_file_name,
            input$flag_conditions)
        output$message_area <- renderPrint({flags_file_name})
        file.copy(from = flags_file_name, to = file, overwrite = TRUE)
      }
    },
    contentType = "text/plain"
  )



  # Callback for Apply Difference button:

  observeEvent(input$apply_difference, {
    apply_max_neighbor_value_diff(model) <<- input$apply_difference
  })



  # Callback for Maximum Neighbor Value Difference:

  observeEvent(input$maximum_neighbor_value_difference, {
    is_valid_input <-
      shiny::isTruthy(input$maximum_neighbor_value_difference) &&
      !is.na(input$maximum_neighbor_value_difference) &&
      input$maximum_neighbor_value_difference >= 0.0 &&
      input$maximum_neighbor_value_difference <= 100.0

    if (!is_valid_input) {
      model_value <- maximum_neighbor_value_difference(model)
      freezeReactiveValue(input, "maximum_neighbor_value_difference")
      updateNumericInput(session = session,
                         inputId = "maximum_neighbor_value_difference",
                         value = model_value)
    } else {
      new_value <- input$maximum_neighbor_value_difference
      max_neighbor_value_diff(model) <<- new_value
    }
  })



  # Callback for Apply Percent Difference button:

  observeEvent(input$apply_percent_difference, {
    apply_max_neighbor_val_pdiff(model) <<- input$apply_percent_difference
  })



  # Callback for Maximum Neighbor Value Percent Difference:

  observeEvent(input$maximum_neighbor_value_percent_difference, {
    is_valid_input <-
      shiny::isTruthy(input$maximum_neighbor_value_percent_difference) &&
      !is.na(input$maximum_neighbor_value_percent_difference) &&
      input$maximum_neighbor_value_percent_difference >= 0.0 &&
      input$maximum_neighbor_value_percent_difference <= 100.0

    if (!is_valid_input) {
      model_value <- maximum_neighbor_value_percent_difference(model)
      freezeReactiveValue(input, "maximum_neighbor_value_percent_difference")
      updateNumericInput(session = session,
                         inputId = "maximum_neighbor_value_percent_difference",
                         value = model_value)
    } else {
      new_value <- input$maximum_neighbor_value_percent_difference
      max_neighbor_value_pdiff(model) <<- new_value
    }
  })



  # Callback for Apply R-Squared button:

  observeEvent(input$apply_r_squared, {
    apply_min_neighbor_value_r2(model) <<- input$apply_r_squared
  })



  # Callback for Minimum Neighbor Value R-Squared:

  observeEvent(input$minimum_neighbor_value_r_squared, {
    is_valid_input <-
      shiny::isTruthy(input$minimum_neighbor_value_r_squared) &&
      !is.na(input$minimum_neighbor_value_r_squared) &&
      input$minimum_neighbor_value_r_squared >= 0.0 &&
      input$minimum_neighbor_value_r_squared <= 1.0

    if (!is_valid_input) {
      model_value <- minimum_neighbor_value_r_squared(model)
      freezeReactiveValue(input, "minimum_neighbor_value_r_squared")
      updateNumericInput(session = session,
                         inputId = "minimum_neighbor_value_r_squared",
                         value = model_value)
    } else {
      new_value <- input$minimum_neighbor_value_r_squared
      min_neighbor_value_r2(model) <<- new_value
    }
  })



  # Callback for Apply Constant Value checkbox:
  observeEvent(input$apply_constant_value, {
    apply_constant_value_flag(model) <<- input$apply_constant_value
  })



  # Callback for Apply Long Missing checkbox:
  observeEvent(input$apply_long_missing, {
    apply_long_missing_flag(model) <<- input$apply_long_missing
  })



  # Callback for Apply Hampel Filter checkbox:
  observeEvent(input$apply_hampel_filter, {
    apply_hampel_filter_flag(model) <<- input$apply_hampel_filter
  })




  # Callback for Apply Outlier Stat checkbox:
  observeEvent(input$apply_outlier_stat, {
    apply_outlier_stat_flag(model) <<- input$apply_outlier_stat
  })



  # Callback for Apply Invalid Negtive Value checkbox:
  observeEvent(input$apply_invalid_negtive_value, {
    apply_negtive_value_flag(model) <<- input$apply_invalid_negtive_value
  })



  # Callback for Apply Redundancy Check checkbox:
  observeEvent(input$apply_redundancy_check, {
    apply_redundancy_check_flag(model) <<- input$apply_redundancy_check
  })



  # Callback for Apply Format Check checkbox:
  observeEvent(input$apply_format_check, {
    apply_format_check_flag(model) <<- input$apply_format_check
  })



  # Callback for Apply Date Validation checkbox:
  observeEvent(input$date_validation, {
    apply_date_validation_flag(model) <<- input$date_validation
  })



  # Callback for Apply Sudden Spike checkbox:
  observeEvent(input$apply_sudden_spike, {
    apply_sudden_spike_flag(model) <<- input$apply_sudden_spike
  })



  # Callback for Apply Sudden Drop checkbox:
  observeEvent(input$apply_sudden_drop, {
    apply_sudden_drop_flag(model) <<- input$apply_sudden_drop
  })



  # Callback for Apply Daily Pattern O3 checkbox:
  observeEvent(input$apply_daily_pattern_o3, {
    apply_daily_pattern_o3(model) <<- input$apply_daily_pattern_o3
  })



  # Callback for Apply Daily Pattern PM checkbox:
  observeEvent(input$apply_daily_pattern_pm, {
    apply_daily_pattern_pm(model) <<- input$apply_daily_pattern_pm
  })



  # Function to validate the timestamp format
  validate_timestamp <- function(timestamp) {
    pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}-\\d{4}$"
    return(grepl(pattern, timestamp))
  }



  # Observer for Use Time Window for outlier
  observeEvent(input$use_time_window, {
    outlier_time_window(model) <<- input$use_time_window
  })



  # Observer for Outlier Start Timestamps
  observeEvent(input$outlier_start_timestamps, {
    start_time <- input$outlier_start_timestamps

    if (!is.null(start_time) && nchar(start_time) > 0) {

      if (!validate_timestamp(start_time)) {
        showNotification("Invalid start time format. Please use YYYY-MM-DDTHH:MM:SS-0000", type = "error")
      }
    }

    outlier_start_timestamps(model) <<- input$outlier_start_timestamps
  })



  # Observer for Outlier End Timestamps
  observeEvent(input$outlier_end_timestamps, {
    end_time <- input$outlier_end_timestamps

    if (!is.null(end_time) && nchar(end_time) > 0) {

      if (!validate_timestamp(end_time)) {
        showNotification("Invalid end time format. Please use YYYY-MM-DDTHH:MM:SS-0000", type = "error")
      } else {
        outlier_end_timestamps(model) <<- input$outlier_end_timestamps
      }
    }
  })



  # Observer for Outlier Threshold
  observeEvent(input$outlier_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$outlier_threshold) &&
      !is.na(input$outlier_threshold) &&
      input$outlier_threshold >= 0L &&
      input$outlier_threshold <= 100L

    if (!is_valid_input) {
      model_value <- outlier_threshold(model)
      freezeReactiveValue(input, "outlier_threshold")
      updateNumericInput(session = session,
                          inputId = "outlier_threshold",
                          value = model_value)
    } else {
      new_value <- input$outlier_threshold
      outlier_threshold(model) <<- new_value
    }
  })



  # Observer for Hampel Filter Window
  observeEvent(input$hampel_filter_window, {
    is_valid_input <-
      shiny::isTruthy(input$hampel_filter_window) &&
      !is.na(input$hampel_filter_window) &&
      input$hampel_filter_window >= 0L &&
      input$hampel_filter_window <= 100L

    if (!is_valid_input) {
      model_value <- hampel_filter_window(model)
      freezeReactiveValue(input, "hampel_filter_window")
      updateNumericInput(session = session,
                          inputId = "hampel_filter_window",
                          value = model_value)
    } else {
      new_value <- input$hampel_filter_window
      hampel_filter_window(model) <<- new_value
    }
  })



  # Observer for Drop Threshold
  observeEvent(input$drop_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$drop_threshold) &&
      !is.na(input$drop_threshold) &&
      input$drop_threshold >= 0L &&
      input$drop_threshold <= 100L

    if (!is_valid_input) {
      model_value <- drop_threshold(model)
      freezeReactiveValue(input, "drop_threshold")
      updateNumericInput(session = session,
                          inputId = "drop_threshold",
                          value = model_value)
    } else {
      new_value <- input$drop_threshold
      drop_threshold(model) <<- new_value
    }
  })



  # Observer for Drop Time Window
  observeEvent(input$drop_time_window, {
    is_valid_input <-
      shiny::isTruthy(input$drop_time_window) &&
      !is.na(input$drop_time_window) &&
      input$drop_time_window >= 0L &&
      input$drop_time_window <= 100L

    if (!is_valid_input) {
      model_value <- drop_time_window(model)
      freezeReactiveValue(input, "drop_time_window")
      updateNumericInput(session = session,
                          inputId = "drop_time_window",
                          value = model_value)
    } else {
      new_value <- input$drop_time_window
      drop_time_window(model) <<- new_value
    }
  })



  # Observer for Spike Time Window
  observeEvent(input$spike_time_window, {
    is_valid_input <-
      shiny::isTruthy(input$spike_time_window) &&
      !is.na(input$spike_time_window) &&
      input$spike_time_window >= 0L &&
      input$spike_time_window <= 100L

    if (!is_valid_input) {
      model_value <- spike_time_window(model)
      freezeReactiveValue(input, "spike_time_window")
      updateNumericInput(session = session,
                          inputId = "spike_time_window",
                          value = model_value)
    } else {
      new_value <- input$spike_time_window
      spike_time_window(model) <<- new_value
    }
  })



  # Observer for Spike Threshold
  observeEvent(input$spike_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$spike_threshold) &&
      !is.na(input$spike_threshold) &&
      input$spike_threshold >= 0L &&
      input$spike_threshold <= 1000L

    if (!is_valid_input) {
      model_value <- spike_threshold(model)
      freezeReactiveValue(input, "spike_threshold")
      updateNumericInput(session = session,
                          inputId = "spike_threshold",
                          value = model_value)
    } else {
      new_value <- input$spike_threshold
      spike_threshold(model) <<- new_value
    }
  })



  # Observer for Hampel Filter Threshold
  observeEvent(input$hampel_filter_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$hampel_filter_threshold) &&
      !is.na(input$hampel_filter_threshold) &&
      input$hampel_filter_threshold >= 0L &&
      input$hampel_filter_threshold <= 100L

    if (!is_valid_input) {
      model_value <- hampel_filter_threshold(model)
      freezeReactiveValue(input, "hampel_filter_threshold")
      updateNumericInput(session = session,
                          inputId = "hampel_filter_threshold",
                          value = model_value)
    } else {
      new_value <- input$hampel_filter_threshold
      hampel_filter_threshold(model) <<- new_value
    }
  })



  # Observer for Long Missing Threshold
  observeEvent(input$long_missing_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$long_missing_threshold) &&
      !is.na(input$long_missing_threshold) &&
      input$long_missing_threshold >= 0L &&
      input$long_missing_threshold <= 100L

    if (!is_valid_input) {
      model_value <- long_missing_threshold(model)
      freezeReactiveValue(input, "long_missing_threshold")
      updateNumericInput(session = session,
                         inputId = "long_missing_threshold",
                         value = model_value)
    } else {
      new_value <- input$long_missing_threshold
      long_missing_threshold(model) <<- new_value
    }
  })



  # Observer for Constant Value Threshold
  observeEvent(input$constant_value_threshold, {
    is_valid_input <-
      shiny::isTruthy(input$constant_value_threshold) &&
      !is.na(input$constant_value_threshold) &&
      input$constant_value_threshold >= 0L &&
      input$constant_value_threshold <= 100L

    if (!is_valid_input) {
      model_value <- constant_value_threshold(model)
      freezeReactiveValue(input, "constant_value_threshold")
      updateNumericInput(session = session,
                         inputId = "constant_value_threshold",
                         value = model_value)
    } else {
      new_value <- input$constant_value_threshold
      constant_value_threshold(model) <<- new_value
    }
  })



  # Observer for the check units consistency button
  observeEvent(input$check_units_consistency, {
    dataset_flag_index <- get_flag_dataset_index()
    the_dataset <- dataset(model, dataset_flag_index)
    data_frame <- data_frame(the_dataset)
    variables <- colnames(data_frame)

    units_df <- data.frame(Variable = character(),
                           Unit = character(),
                           stringsAsFactors = FALSE)

    # Parse units from variable names
    for (var in variables) {

      # Skip special columns
      if (!grepl("\\(|\\)", var)) {
        next
      }

      # Extract unit from variable name (format "name(unit)")
      unit <- gsub(".*\\((.+)\\).*", "\\1", var)
      var_name <- gsub("\\(.+\\)", "", var)

      units_df <- rbind(units_df,
                        data.frame(Variable = var_name, Unit = unit,
                                   stringsAsFactors = FALSE))
    }

    # Render the units table
    output$all_units_table <-
      renderTable({units_df}, caption = "Variables and Their Units")


    flagged_units <- units_df[
        (grepl("^timestamp", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "UTC") |
        (grepl("longitude", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "deg") |
        (grepl("latitude", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "deg") |
        (grepl("pm", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "ug/m3") |
        (grepl("temperature", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "C") |
        (grepl("^pressure", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "hPa") |
        (grepl("ozone|o3", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "ppb") |
        (grepl("(^rh|humidity)", units_df$Variable, ignore.case = TRUE) & units_df$Unit != "%"), ]

    # Render the flagged units table
    output$flagged_units_table <- renderTable({

      if (nrow(flagged_units) > 0) {
        flagged_units
      } else {
        "No unit inconsistencies found"
      }
    }, caption = "Inconsistent Units")
  })



  summarizing <- FALSE

  # Summarize and compare Datasets X and Y and show computed tables:

  summarize_and_compare_datasets_x_y <- function() {
    ASNAT_dprint("summarize_and_compare_datasets_x_y called.\n")
    timer <- ASNAT_start_timer()

    ASNAT_dprint("In handle_summarize(), summarizing = %d\n",
                 as.integer(summarizing))

    ASNAT_debug(str, input$dataset_x_menu)
    ASNAT_debug(str, input$dataset_x_menu[1L])

    have <- have_datasets()
    have_x <- have$x
    have_y <- have$y
    have_z <- have$z

    if (summarizing) {
      return(NULL)
    }

    model <<- delete_dataset_summaries(model)
    clear_tables_and_plots()

    if (!have_x) {
      return(NULL)
    }

    ASNAT_dprint("  have_x = %d, have_y = %d\n",
                 as.integer(have_x), as.integer(have_y))
    summarizing <<- TRUE
    ASNAT_dprint("  set summarizing = %d\n", as.integer(summarizing))

    dataset_x_name <- input$dataset_x_menu[1L]
    dataset_x_variable <- input$dataset_x_variable_menu[1L]
    dataset_y_name <- NULL
    dataset_y_variable <- NULL

    if (have_y) {
      dataset_y_name <- input$dataset_y_menu[1L]
      dataset_y_variable <- input$dataset_y_variable_menu[1L]
    }

    dataset_z_name <- NULL
    dataset_z_variable <- NULL

    if (have_z) {
      dataset_z_name <- input$dataset_z_menu[1L]
      dataset_z_variable <- input$dataset_z_variable_menu[1L]
    }

    showNotification("Summarizing and comparing data...",
                     duration = NULL, closeButton = FALSE,
                     id = "message_popup", type = "message")

    model <<- summarize_datasets(model,
                                 dataset_x_name, dataset_x_variable,
                                 dataset_y_name, dataset_y_variable)

    if (ok(model)) {
      summary_data_frame_x <- summary_x_data_frame(model)
      summary_data_frame_y <- NULL

      dataset_flag_index <- get_flag_dataset_index()
      selected_flag_variable <- get_variable_by_flag_dataset()
      model <<-
        process_flaggings(model, dataset_flag_index, selected_flag_variable)

      if (have_y) {
        summary_data_frame_y <- summary_y_data_frame(model)
      }

      ASNAT_dprint("summary_data_frame_x:\n")
      ASNAT_debug(str, summary_data_frame_x)
      title_x <- summary_x_title(model)
      subtitle_x <- summary_x_subtitle(model)
      caption_x <-
        as.character(tags$div(tags$h4(style = "color: black", title_x),
                              tags$h5(style = "color: black", subtitle_x)))
      caption_x <- fancy_label(caption_x)
      column_names <- colnames(summary_data_frame_x)
      fancy_column_names <-
        vapply(column_names, fancy_label, c(""), USE.NAMES = FALSE)
      colnames(summary_data_frame_x) <- fancy_column_names

      output$table1 <- renderTable(summary_data_frame_x,
                                   caption = caption_x,
                                   caption.placement = "top")

      if (!is.null(summary_data_frame_y)) {
        ASNAT_dprint("summary_data_frame_y:\n")
        ASNAT_debug(str, summary_data_frame_y)
        title_y <- summary_y_title(model)
        subtitle_y <- summary_y_subtitle(model)
        caption_y <-
          as.character(tags$div(tags$h4(style = "color: black", title_y),
                                tags$h5(style = "color: black", subtitle_y)))
        caption_y <- fancy_label(caption_y)
        column_names <- colnames(summary_data_frame_y)
        fancy_column_names <-
          vapply(column_names, fancy_label, c(""), USE.NAMES = FALSE)
        colnames(summary_data_frame_y) <- fancy_column_names

        output$table2 <- renderTable(summary_data_frame_y,
                                     caption = caption_y,
                                     caption.placement = "top")

        model <<- compare_datasets(model,
                                   dataset_x_name, dataset_x_variable,
                                   dataset_y_name, dataset_y_variable,
                                   dataset_z_name, dataset_z_variable)

        the_comparison_data_frame <- comparison_data_frame(model)

        if (nrow(the_comparison_data_frame) > 0L) {
          ASNAT_dprint("the_comparison_data_frame:\n")
          ASNAT_debug(str, the_comparison_data_frame)
          title <- comparison_title(model)
          subtitle <- comparison_subtitle(model)
          caption <-
            as.character(tags$div(tags$h4(style = "color: black", title),
                                  tags$h5(style = "color: black", subtitle)))
          caption <- fancy_label(caption)
          column_names <- colnames(the_comparison_data_frame)
          fancy_column_names <-
            vapply(column_names, fancy_label, c(""), USE.NAMES = FALSE)
          colnames(the_comparison_data_frame) <- fancy_column_names
          output$table3 <-
            renderTable(the_comparison_data_frame,
                        caption = caption, caption.placement = "top")

          the_comparison_r2_data_frame <- comparison_r2_data_frame(model)

          if (nrow(the_comparison_r2_data_frame) > 0L) {
            ASNAT_dprint("the_comparison_r2_data_frame:\n")
            ASNAT_debug(str, the_comparison_r2_data_frame)
            title <- comparison_r2_title(model)
            subtitle <- comparison_r2_subtitle(model)
            caption <-
              as.character(tags$div(tags$h4(style = "color: black", title),
                                    tags$h5(style = "color: black", subtitle)))
            caption <- fancy_label(caption)
            column_names <- colnames(the_comparison_r2_data_frame)
            fancy_column_names <-
              vapply(column_names, fancy_label, c(""), USE.NAMES = FALSE)
            colnames(the_comparison_r2_data_frame) <- fancy_column_names

            output$table4 <-
              renderTable(the_comparison_r2_data_frame, digits = 4L,
                          caption = caption, caption.placement = "top")
          }

          the_aqi_statistics_data_frame <- aqi_statistics_data_frame(model)

          if (nrow(the_aqi_statistics_data_frame) > 0L) {
            ASNAT_dprint("the_aqi_statistics_data_frame:\n")
            ASNAT_debug(str, the_aqi_statistics_data_frame)
            title <- aqi_statistics_title(model)
            subtitle <- aqi_statistics_subtitle(model)
            caption <-
              as.character(tags$div(tags$h4(style = "color: black", title),
                                    tags$h5(style = "color: black", subtitle)))
            caption <- fancy_label(caption)
            column_names <- colnames(the_aqi_statistics_data_frame)
            fancy_column_names <-
              vapply(column_names, fancy_label, c(""), USE.NAMES = FALSE)
            colnames(the_aqi_statistics_data_frame) <- fancy_column_names

            output$table5 <-
              renderTable(the_aqi_statistics_data_frame, digits = 4L,
                          caption = caption, caption.placement = "top")
          }
        }
      }
    }

    show_dataset_content_to_flag()
    removeNotification("message_popup")

    if (nrow(summary_x_data_frame(model)) == 0L) {
      message_text <-
        paste0("There are no neighboring points within ",
               maximum_neighbor_distance(model), " meters.\n")
      showNotification(message_text,
                       duration = 5, closeButton = TRUE,
                       id = "message_popup2", type = "warning")
    }

    summarizing <<- FALSE
    ASNAT_dprint("  reset summarizing = %d\n", as.integer(summarizing))
    ASNAT_elapsed_timer("summarize_and_compare_datasets_x_y:", timer)
  }

  get_summary_statistics <- function() {
    ASNAT_dprint("get_summary_statistics called.\n")
    timer <- ASNAT_start_timer()

    ASNAT_dprint("In handle_summarize(), summarizing = %d\n",
                as.integer(summarizing))

    ASNAT_debug(str, input$dataset_x_menu)
    ASNAT_debug(str, input$dataset_x_menu[1L])

    have <- have_datasets()
    have_x <- have$x

    model <<- delete_dataset_summaries(model)
    clear_tables_and_plots()

    if (!have_x) {
      return(NULL)
    }

    ASNAT_dprint("  have_x = %d, have_y = %d\n",
                as.integer(have_x), as.integer(have_y))
    ASNAT_dprint("  set summarizing = %d\n", as.integer(summarizing))

    dataset_x_name <- input$dataset_x_menu[1L]
    dataset_x_variable <- input$dataset_x_variable_menu[1L]

    coverages <- dataset_coverages(model)
    result <- which(coverages == dataset_x_name)

    dataset_index <- get_flag_dataset_index()

    if (dataset_index > 0L) {
      the_dataset <- dataset(model, dataset_index)
      data_frame <- data_frame(the_dataset)
    }
    if (is.null(data_frame)) {
      showNotification("No data available for summary statistics", type = "error")
      return(NULL)
    }
    
    # Calculate flag statistics
    flag_stats <- calculate_flag_statistics(data_frame)
    
    # Create a simple data frame for main summary display
    summary_df <- data.frame(
      Metric = c("Total Records", "Valid Records", "Valid Records (%)", 
                "Flagged Records", "Flagged Records (%)"),
      Value = c(flag_stats$total_records, flag_stats$valid_records, 
              sprintf("%.2f%%", flag_stats$valid_percent),
              flag_stats$flagged_records, 
              sprintf("%.2f%%", flag_stats$flagged_percent))
    )
    
    # Display the main summary table
    output$table1 <- renderTable(
      summary_df,
      caption = paste("Summary Statistics for", dataset_x_name),
      caption.placement = "top"
    )
    
    # Create a data frame for flag type percentages
    flag_types <- list(
      "65" = "Temporal inconsistencies",
      "70" = "Sudden spike",
      "71" = "Sudden drop",
      "80" = "Maximum neighbor value difference",
      "81" = "Maximum neighbor value percent difference",
      "82" = "Minimum neighbor value R-squared",
      "83" = "Constant value",
      "84" = "Long missing",
      "85" = "Statistical outliers",
      "86" = "Hampel filter outliers",
      "90" = "Redundancy check (duplicate)",
      "95" = "Timestamps format check"
    )
    
    # Create a data frame for flag type counts and percentages
    flag_df <- data.frame(
      Flag = character(),
      Description = character(),
      Count = integer(),
      Percentage = character(),
      stringsAsFactors = FALSE
    )
    
    # Add each flag type to the data frame
    for (flag in names(flag_types)) {
      if (!is.null(flag_stats$flag_counts[[flag]])) {
        count <- flag_stats$flag_counts[[flag]]
        percent <- flag_stats$flag_counts[[paste0(flag, "_percent")]]
        flag_df <- rbind(flag_df, data.frame(
          Flag = flag,
          Description = flag_types[[flag]],
          Count = count,
          Percentage = sprintf("%.2f%%", percent),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Sort flag types by count (descending)
    if (nrow(flag_df) > 0) {
      flag_df <- flag_df[order(-flag_df$Count), ]
    }
    
    # Display the flag type percentages table
    output$table2 <- renderTable(
      flag_df,
      caption = paste("Flag Distribution for", dataset_x_name),
      caption.placement = "top"
    )
    
    # If device-level statistics are available, display them in table3
    if ("id(-)" %in% colnames(data_frame)) {
      device_stats <- calculate_device_statistics(data_frame)
      
      if (nrow(device_stats) > 0) {
        # Create a copy of device_stats for display
        display_stats <- device_stats[, c("device_id", "total_records", "valid_records", 
                                        "valid_percent", "flagged_records", 
                                        "flagged_percent")]
        
        # Format percentages for display
        display_stats$valid_percent <- sprintf("%.2f%%", display_stats$valid_percent)
        display_stats$flagged_percent <- sprintf("%.2f%%", display_stats$flagged_percent)
        
        # Get flag percentage columns
        flag_percent_cols <- grep("^flag_\\d+_percent$", colnames(device_stats), value = TRUE)
        
        # Add flag percentage columns to display_stats
        if (length(flag_percent_cols) > 0) {
          for (col in flag_percent_cols) {
            flag_code <- gsub("flag_|_percent", "", col)
            flag_name <- flag_types[[flag_code]]
            # Format percentage and add to display_stats
            display_stats[[flag_name]] <- sprintf("%.2f%%", device_stats[[col]])
          }
        }
        
        # Rename columns for display
        colnames(display_stats)[1:6] <- c("Device ID", "Total Records", "Valid Records", 
                                        "Valid Records (%)", "Flagged Records", 
                                        "Flagged Records (%)")
        
        # Render the device statistics table with flag percentages
        output$table3_interactive <- DT::renderDataTable({
          DT::datatable(display_stats,
                        caption = paste("Device-Level Statistics for", dataset_x_name),
                        options = list(paging = FALSE),
                        class = 'row-border stripe hover nowrap')
        })
      }
    }
    
    ASNAT_elapsed_timer("get_summary_statistics:", timer)
    return(TRUE)
  }


  # Function to calculate flag-based statistics for a dataset
  calculate_flag_statistics <- function(data_frame) {
    # Total number of records
    total_records <- nrow(data_frame)
    
    # Check if flagged column exists
    if (!"flagged(-)" %in% colnames(data_frame)) {
      return(list(
        total_records = total_records,
        valid_records = total_records,
        valid_percent = 100,
        flagged_records = 0,
        flagged_percent = 0,
        flag_counts = list()
      ))
    }
    
    # Count records with any flags
    flagged_records <- sum(data_frame[["flagged(-)"]] != "0" & !is.na(data_frame[["flagged(-)"]]))
    
    # Calculate percentage of flagged records
    percent_flagged <- (flagged_records / total_records) * 100
    
    # Count records by specific flag types
    flag_counts <- list()
    
    # Define flag types and their descriptions
    flag_types <- list(
      "65" = "Date validation",
      "70" = "Sudden spike",
      "71" = "Sudden drop",
      "80" = "Maximum neighbor value difference",
      "81" = "Maximum neighbor value percent difference",
      "82" = "Minimum neighbor value R-squared",
      "83" = "Constant value",
      "84" = "Long missing",
      "85" = "Statistical outlier",
      "86" = "Hampel filter",
      "90" = "Redundancy check",
      "95" = "Format check"
    )
    
    # Count occurrences of each flag type
    for (flag in names(flag_types)) {
      flag_counts[[flag]] <- sum(grepl(flag, data_frame[["flagged(-)"]]))
      flag_counts[[paste0(flag, "_percent")]] <- (flag_counts[[flag]] / total_records) * 100
    }
    
    # Return statistics
    return(list(
      total_records = total_records,
      valid_records = total_records - flagged_records,
      valid_percent = 100 - percent_flagged,
      flagged_records = flagged_records,
      flagged_percent = percent_flagged,
      flag_counts = flag_counts
    ))
  }

  # # Function to calculate per-device statistics with flag type percentages
  calculate_device_statistics <- function(data_frame) {
    # Get unique device IDs
    device_ids <- unique(data_frame[["id(-)"]])
    
    # Define flag types
    flag_types <- list(
      "65" = "Date validation",
      "70" = "Sudden spike",
      "71" = "Sudden drop",
      "80" = "Maximum neighbor value difference",
      "81" = "Maximum neighbor value percent difference",
      "82" = "Minimum neighbor value R-squared",
      "83" = "Constant value",
      "84" = "Long missing",
      "85" = "Statistical outlier",
      "86" = "Hampel filter",
      "90" = "Redundancy check",
      "95" = "Format check"
    )
    
    # Initialize results data frame with basic columns
    device_stats <- data.frame(
      device_id = character(),
      total_records = integer(),
      valid_records = integer(),
      valid_percent = numeric(),
      flagged_records = integer(),
      flagged_percent = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Add columns for each flag type percentage
    for (flag in names(flag_types)) {
      device_stats[[paste0("flag_", flag, "_percent")]] <- numeric()
    }
    
    # Calculate statistics for each device
    for (device_id in device_ids) {
      device_data <- data_frame[data_frame[["id(-)"]] == device_id, ]
      total_records <- nrow(device_data)
      
      # Initialize row with basic stats
      device_row <- data.frame(
        device_id = device_id,
        total_records = total_records,
        valid_records = total_records,
        valid_percent = 100,
        flagged_records = 0,
        flagged_percent = 0,
        stringsAsFactors = FALSE
      )
      
      # Add columns for each flag type percentage with default value 0
      for (flag in names(flag_types)) {
        device_row[[paste0("flag_", flag, "_percent")]] <- 0
      }
      
      # Calculate flag statistics if flagged column exists
      if ("flagged(-)" %in% colnames(device_data)) {
        flagged_records <- sum(device_data[["flagged(-)"]] != "0" & !is.na(device_data[["flagged(-)"]]))
        valid_records <- total_records - flagged_records
        valid_percent <- (valid_records / total_records) * 100
        flagged_percent <- (flagged_records / total_records) * 100
        
        device_row$valid_records <- valid_records
        device_row$valid_percent <- valid_percent
        device_row$flagged_records <- flagged_records
        device_row$flagged_percent <- flagged_percent
        
        # Calculate percentage for each flag type
        for (flag in names(flag_types)) {
          flag_count <- sum(grepl(flag, device_data[["flagged(-)"]]))
          flag_percent <- (flag_count / total_records) * 100
          device_row[[paste0("flag_", flag, "_percent")]] <- flag_percent
        }
      }
      
      # Add this device's row to the results
      device_stats <- rbind(device_stats, device_row)
    }
    
    return(device_stats)
  }

  # Callback for Interactive (color) Plots checkbox
  # (also uses fancy labels in interactive plots):

  observeEvent(input$use_interactive_plots, {

    if (shiny::isTruthy(input$use_interactive_plots)) {
      use_interactive_plots(model) <<- TRUE
    } else {
      use_interactive_plots(model) <<- FALSE
    }

    # If there is data then redraw it on map with appropriate legend label:

    if (dataset_count(model) > 0L) {
      model_timestep <- timestep(model)
      update_map_timestep(model_timestep)
    }
  })



  # Helper for fancy legend labels:

  fancy_legend_label <- function(label) {
    stopifnot(nchar(label) > 0L)
    result <- label

    if (use_fancy_labels(model)) {
      result <- ASNAT_fancy_label(result)
    }

    return(result)
  }



  # Helper for fancy labels:

  fancy_label <- function(label) {
    stopifnot(nchar(label) > 0L)
    result <- label

    if (use_interactive_plots(model)) {
      result <- ASNAT_fancy_label(result)
    }

    return(result)
  }



  # Callback for Summarize and Compare X & Y button:

  observeEvent(input$summarize, {

    summarize_and_compare_datasets_x_y()
    update_neighbors_menu()
  })

 # callback for summary_statistics button
   observeEvent(input$summary_statistics, {
    get_summary_statistics()
    # # Clear tables 1-5
    # output$table1 <- renderTable(NULL)
    # output$table2 <- renderTable(NULL)
    # output$table3 <- renderTable(NULL)
    # output$table4 <- renderTable(NULL)
    # output$table5 <- renderTable(NULL)

    # summarize_and_compare_datasets_x_y()
    # update_neighbors_menu()
  })


  ############################# Boxplot functions #############################

  flagged_point_color <- "gray"
  saved_plot_files <- vector()


  # Helper to draw boxplot of a data.frame:
  # https://stackoverflow.com/questions/27276994/outputting-shiny-non-ggplot-plot-to-pdf#27381557

  draw_boxplot <-
  function(data_frame, main_title, x_label, y_label, interactive, file_name) {
    stopifnot(!is.null(data_frame))
    stopifnot(class(data_frame) == "data.frame")
    stopifnot(ncol(data_frame) >= 2L)
    stopifnot(nrow(data_frame) > 0L)
    stopifnot(!is.null(colnames(data_frame)))
    stopifnot(nchar(main_title) > 0L)
    stopifnot(nchar(x_label) > 0L)
    stopifnot(nchar(y_label) > 0L)
    stopifnot(interactive == TRUE || interactive == FALSE)
    ASNAT_dprint("In draw_boxplot(%s)\n", main_title)
    result <- NULL

    if (!is.null(file_name)) {
      ASNAT_dprint("In draw_boxplot() calling pdf(%s)\n", file_name)
      pdf(file_name, width = the_pdf_width, height = the_pdf_height,
          pointsize = the_pdf_point_size)
      par(mar = c(7, 4, 4, 2))
      boxplot(measure ~ site_id, data_frame,
              main = main_title,
              xlab = "",
              ylab = y_label,
              las = 2L)
      mtext(x_label, side = 1, line = -1, outer = TRUE)
      dev.off()
    } else {

      if (!interactive) {
        result <- renderPlot({
          par(mar = c(7, 4, 4, 2))
          boxplot(measure ~ site_id, data_frame,
                  main = main_title,
                  xlab = "",
                  ylab = y_label,
                  las = 2L)
          mtext(x_label, side = 1L, line = -1, outer = TRUE)
        })
      } else {
        result <- renderPlotly({
          plotly::plot_ly(data_frame,
                          x = ~as.character(site_id), y = ~measure,
                          type = "box",
                          # FIX: Set color to black or gray.
                          colors = "gray",
                          marker = list(color = "gray"),
                          line = list(color = "gray")) %>%
          plotly::layout(title = main_title,
                         margin = list(t = 100, pad = 1),
                         xaxis = list(title = x_label),
                         yaxis = list(title = y_label))
        })
      }
    }

    return(result)
  }



  # Draw dataset summary box plot of selected dataset (possibly filtered ids)
  # or, if a single site_id is specified with neighbor ids then plot single site
  # and its neighbors:

  draw_dataset_boxplot <-
  function(dataset_name, dataset_variable, filter_ids,
           neighbor_dataset_name, neighbor_dataset_variable,
           neighbor_ids, neighbor_distance, save_to_file) {

    ASNAT_dprint("In draw_dataset_boxplot(%s)\n", dataset_name)

    # Don't save interactive plots as PDF files.

    interactive <- use_interactive_plots(model) && !save_to_file

    # Get the data frame subsetted by filter_ids:

    the_dataset <- set_dataset_variable(dataset_name, dataset_variable)
    the_data_frame <- data_frame(the_dataset)
    subset_data_frame <- the_data_frame
    column_names <- colnames(the_data_frame)
    site_column <- ASNAT_site_column_index(column_names)
    is_single_site <- length(filter_ids) == 1L
    single_site_id <- 0L

    # Filter the data frame by filter_ids:

    if (!is.null(filter_ids)) {
      sites <- filter_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      is_single_site <- length(sites) == 1L
      matched_rows <- which(the_data_frame[[site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)

      if (is_single_site) {
        single_site_id <- sites[[1L]]
      }

      subset_data_frame <- the_data_frame[matched_rows, ]
    }

    if (nrow(subset_data_frame) < 1L) return(NULL)

    # Filter the neighbor data frame by neighbor_ids:

    neighbor_dataset <- NULL
    neighbor_data_frame <- NULL
    subset_neighbor_data_frame <- NULL
    neighbor_column_names <- NULL
    neighbor_site_column <- 0L

    if (is_single_site &&
        !is.null(neighbor_dataset_name) &&
        !is.null(neighbor_dataset_variable) &&
        !is.null(neighbor_ids)) {
      neighbor_dataset <-
        set_dataset_variable(neighbor_dataset_name, neighbor_dataset_variable)
      neighbor_data_frame <- data_frame(neighbor_dataset)
      neighbor_column_names <- colnames(neighbor_data_frame)
      neighbor_site_column <- ASNAT_site_column_index(neighbor_column_names)
      sites <- neighbor_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      matched_rows <-
        which(neighbor_data_frame[[neighbor_site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)
      subset_neighbor_data_frame <- neighbor_data_frame[matched_rows, ]
    }

    # Create main title with date range, bbox, etc.
    # and y-axis label with measure and units.

    units <- variable_units(the_dataset)
    source <- coverage_source(the_dataset)
    source_variable <- source_variable(the_dataset)
    fancy_source_variable <- source_variable
    the_variable <- variable_name(the_dataset)
    fancy_variable <- the_variable

    if (interactive) {
      fancy_variable <- fancy_label(the_variable)
      units <- fancy_label(units)
      fancy_source_variable <-
        paste0(coverage_source(the_dataset), ".",
               fancy_label(variable_name(the_dataset)))
    }

    # Get date-time range:

    first_model_timestamp <- first_timestamp(model)
    last_model_timestamp <- last_timestamp(model)
    model_timestep_size <- timestep_size(model)
    date_range <- substr(first_model_timestamp, 1L, 10L)

    if (days(model) > 1L) {
      date_range <-
        paste0(date_range, " - ", substr(last_model_timestamp, 1L, 10L))
    }

    averaging <- "Hourly"
    timestamp_length <- 13L

    if (model_timestep_size == "days") {
      averaging <- "Daily"
      timestamp_length <- 10L
    }

    first_model_timestamp <-
      substr(first_model_timestamp, 1L, timestamp_length)

    # Get bounds of viewing area:

    west <- west_bound(the_dataset)
    east <- east_bound(the_dataset)
    south <- south_bound(the_dataset)
    north <- north_bound(the_dataset)

    main_title <-
      sprintf("Dataset Summary: %s %s\n%s (%0.4f, %0.4f) - (%0.4f, %0.4f)",
              averaging,
              ifelse(interactive, fancy_source_variable, source_variable),
              date_range, west, east, south, north)

    y_label <-
      paste0(ifelse(interactive, fancy_variable, the_variable),
             "(", units, ")")

    # If plotting a single site with its neighbors then recreate main title:

    neighbor_source_variable <- NULL

    if (!is.null(subset_neighbor_data_frame)) {
      longitudes <- subset_data_frame[[2L]]
      latitudes <- subset_data_frame[[3L]]
      longitude <- longitudes[[1L]]
      latitude <- latitudes[[1L]]
      neighbor_source_variable <- source_variable(neighbor_dataset)
      neighbor_variable <- variable_name(neighbor_dataset)
      fancy_neighbor_source_variable <- neighbor_source_variable

      if (interactive) {
        fancy_neighbor_source_variable <-
          paste0(coverage_source(neighbor_dataset), ".",
                 fancy_label(neighbor_variable))
      }

      main_title <-
        sprintf("%s Site %d Neighbors (<= %dm) Summary:\n%s %s vs %s\n%s (%0.4f, %0.4f)",
                source, single_site_id, neighbor_distance, averaging,
                ifelse(interactive, fancy_source_variable, source_variable),
                ifelse(interactive, fancy_neighbor_source_variable, neighbor_source_variable),
                date_range, longitude, latitude)
    }

    # Plotted data frame will have just two columns: site and measure:

    measure_column <- variable_column(the_dataset)
    subset_data_frame <- subset_data_frame[, c(site_column, measure_column)]
    colnames(subset_data_frame) <- c("site_id", "measure")

    if (is.null(neighbor_dataset)) {

      # Add first/left boxplot as a summary of all (filtered) site measures,
      # prepend rows with id = 0 and all (filtered) measures:

      filtered_row_count <- nrow(subset_data_frame)
      ids0 <- rep(0L, filtered_row_count)
      filtered_measures <- subset_data_frame[[2L]]
      extra_data_frame <- data.frame(site_id = ids0, measure = filtered_measures)
      subset_data_frame <- rbind(extra_data_frame, subset_data_frame)
    } else {

      # Append rows with neighbor site_ids and measures:

      neighbor_measure_column <- variable_column(neighbor_dataset)
      subset_neighbor_data_frame <-
        subset_neighbor_data_frame[, c(neighbor_site_column, neighbor_measure_column)]
      neighbor_measures <- subset_neighbor_data_frame[[2L]]
      neighbor_sites <- subset_neighbor_data_frame[[1L]]
      neighbors_data_frame <-
        data.frame(site_id = neighbor_sites, measure = neighbor_measures)
      subset_data_frame <- rbind(subset_data_frame, neighbors_data_frame)
    }

    ASNAT_dprint("Rendering boxplot with %s\n", source_variable)
    ASNAT_debug(str, subset_data_frame)

    x_label <- ifelse(is_single_site, "Site_Id", "Site_Id (0 = all)")
    pdf_file <- NULL

    if (save_to_file) {
      directory <- output_directory(model)
      site_string <- ""

      if (is_single_site && !is.null(neighbor_source_variable)) {
        site_string <-
          paste0("_site_", single_site_id, "_neighbors_",
                 neighbor_source_variable)
      }

      pdf_file <-
        paste0(directory, "/",
               gsub(fixed = TRUE, ".", "_", source_variable),
               site_string,
               "_boxplot.pdf")
      saved_plot_files <<- append(saved_plot_files, pdf_file)
    }

    result <-
      draw_boxplot(subset_data_frame, main_title, x_label, y_label,
                   interactive, pdf_file)

    return(result)
  }



  ########################## Timeseries Plot functions ########################

  # Helper to make basic timeseries plot:
  # The first call (with result NULL) creates the plot with the first
  # set of points. Subsequent calls add points and lines to the plot result.
  # https://www.statology.org/how-to-plot-multiple-lines-data-series-in-one-chart-in-r/

  make_basic_timeseries_plot <-
  function(x_values, y_values,
           x_minimum, x_maximum, y_minimum, y_maximum,
           symbol, main_title, x_label, y_label, point_colors,
           has_flagged_points, line_label, result) {

    if (is.null(result)) {
      # HACK: Unlike interactive plots, assigning to result from plot(),
      # points(), lines() makes the plot not display right!
      #result <-
      result <- TRUE
        plot(x = x_values, y = y_values,
             xlim = c(x_minimum, x_maximum), ylim = c(y_minimum, y_maximum),
             type = "o", pch = symbol, lty = 3L,
             main = main_title, xlab = x_label, ylab = y_label, las = 1L,
             # xaxp = c(x_minimum, x_maximum, 24L), # BUG: no effect.
             col = point_colors)

      # If the plot includes flagged data points then include a text legend
      # that explains that gray points indicate flagged data points.

      if (has_flagged_points) {
        flagged_label <- "o = flagged"
        flagged_point_color <- "gray"
        flagged_label_x <- x_minimum
        flagged_label_y <- y_minimum + (y_maximum - y_minimum) * 0.9
        #result <- result %>%
          text(labels = flagged_label,
               x = flagged_label_x, y = flagged_label_y, adj = c(0, 0),
               col = flagged_point_color)
      }
    } else {
      #result <- result %>%
        points(x = x_values, y = y_values, pch = symbol, col = point_colors)

      #result <- result %>%
        lines(x = x_values, y = y_values, lty = 3L)
    }

    return(result)
  }



  # Helper to make interactive timeseries plot:
  # The first call (with result NULL) creates the boxplot with the first
  # set of points. Subsequent calls add points and lines to the boxplot result.

  make_interactive_timeseries_plot <-
  function(x_values, y_values,
           x_minimum, x_maximum, y_minimum, y_maximum,
           symbol, main_title, x_label, y_label, point_colors,
           has_flagged_points, line_label, result) {

    if (is.null(result)) {

      # FIX: How can the point/line style of flagged points be shown?

      annotations_list <- NULL

      if (has_flagged_points) {
        flagged_label <- "o = flagged"
        flagged_point_color <- "gray"
        flagged_label_x <- x_minimum
        flagged_label_y <- y_minimum + (y_maximum - y_minimum) * 0.9
        annotations_list <- list(showarrow = FALSE,
                                 text = flagged_label,
                                 x = flagged_label_x,
                                 y = flagged_label_y,
                                 color = flagged_point_color)
      }

      result <-
        plotly::plot_ly(x = x_values, y = y_values,
                        type = "scatter", mode = "lines+markers",
                        line = list(width = 1L, linetype = "dash"),
                        marker = list(size = 4L,
                                      symbols = symbol,
                                      fillcolor = "none",
                                      line = list(color = point_colors,
                                                  width = 1L)),
                      name = line_label) %>%
        plotly::layout(title = main_title,
                       margin = list(t = 100, pad = 1),
                       yaxis = list(title = y_label),
                       xaxis = list(title = x_label),
                       annotations = annotations_list)
    } else {
      result <- result %>%
        plotly::add_trace(x = x_values, y = y_values,
                         type = "scatter", mode = "lines+markers",
                         line = list(width = 1L, linetype = "dash"),
                         marker = list(size = 4L,
                                       symbols = symbol,
                                       color = "none",
                                       line = list(color = point_colors,
                                                   width = 1L)),
                         name = line_label)
    }

    return(result)
  }



  # Helper for looping through each site's measures for timeseries plot.

  make_timeseries_plot_helper <-
  function(data_frame,
           timestamp_column, site_column, measure_column, flagged_column,
           timestamp_format,
           time_0, time_last, minimum, maximum,
           interactive, main_title, x_label, y_label,
           point_colors, has_flagged_points,
           plotter, result) {

    stopifnot(!is.null(data_frame))
    stopifnot(class(data_frame) == "data.frame")
    stopifnot(ncol(data_frame) >= 4L)
    stopifnot(nrow(data_frame) > 0L)
    stopifnot(!is.null(colnames(data_frame)))
    stopifnot(timestamp_column >= 1L && timestamp_column <= ncol(data_frame))
    stopifnot(site_column >= 1L && site_column <= ncol(data_frame))
    stopifnot(measure_column >= 1L && measure_column <= ncol(data_frame))
    stopifnot(flagged_column >= 1L && flagged_column <= ncol(data_frame))
    stopifnot(nchar(timestamp_format) > 0L)
    stopifnot(time_0 <= time_last)
    stopifnot(minimum <= maximum)
    stopifnot(interactive == TRUE || interactive == FALSE)
    stopifnot(nchar(main_title) > 0L)
    stopifnot(nchar(x_label) > 0L)
    stopifnot(nchar(y_label) > 0L)
    stopifnot(!is.null(point_colors))
    stopifnot(has_flagged_points == TRUE || has_flagged_points == FALSE)
    stopifnot(!is.null(plotter))

    sites <- data_frame[[site_column]]
    sites <- sort.int(sites)
    sites <- unique(sites)

    for (site in sites) {
      matched_rows <- which(data_frame[[site_column]] == site)
      subset_data_frame <- data_frame[matched_rows, ]
      measures <- subset_data_frame[[measure_column]]
      flagged <- subset_data_frame[[flagged_column]]
      point_colors <- ifelse(flagged == "0", "black", flagged_point_color)
      timestamps <- subset_data_frame[[timestamp_column]]
      timestamp_labels <-
        as.POSIXct(timestamps, timestamp_format, tz = "UTC")

      # http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

      symbol <- NULL

      if (!interactive) {
        symbol <- ifelse(is.null(result), 0L, 1L)
      } else {
        symbol <- ifelse(is.null(result), "circle-open", "o")
      }

      result <-
        plotter(timestamp_labels, measures,
                time_0, time_last, minimum, maximum,
                symbol, main_title, x_label, y_label,
                point_colors, has_flagged_points, site, result)
    }

    return(result)
  }



  # Helper to draw timeseries plot of data_frame and neighbor_data_frame.

  draw_timeseries_plot <-
  function(data_frame, timestamp_column, site_column, measure_column,
           flagged_column,
           neighbor_data_frame, neighbor_timestamp_column,
           neighbor_site_column, neighbor_measure_column,
           neighbor_flagged_column,
           timestamp_format, time_0, time_last, minimum, maximum,
           interactive, main_title, x_label, y_label, file_name) {

    stopifnot(!is.null(data_frame))
    stopifnot(class(data_frame) == "data.frame")
    stopifnot(ncol(data_frame) >= 4L)
    stopifnot(nrow(data_frame) > 0L)
    stopifnot(!is.null(colnames(data_frame)))
    stopifnot(timestamp_column >= 1L && timestamp_column <= ncol(data_frame))
    stopifnot(site_column >= 1L && site_column <= ncol(data_frame))
    stopifnot(measure_column >= 1L && measure_column <= ncol(data_frame))
    stopifnot(flagged_column >= 1L && flagged_column <= ncol(data_frame))
    stopifnot(is.null(neighbor_data_frame) ||
              class(neighbor_data_frame) == "data.frame")
    stopifnot(is.null(neighbor_data_frame) || ncol(neighbor_data_frame) >= 4L)
    stopifnot(is.null(neighbor_data_frame) || nrow(neighbor_data_frame) > 0L)
    stopifnot(is.null(neighbor_data_frame) ||
              !is.null(colnames(neighbor_data_frame)))
    stopifnot(is.null(neighbor_data_frame) ||
              (neighbor_timestamp_column >= 1L &&
               neighbor_timestamp_column <= ncol(neighbor_data_frame)))
    stopifnot(is.null(neighbor_data_frame) ||
              (neighbor_site_column >= 1L &&
               neighbor_site_column <= ncol(neighbor_data_frame)))
    stopifnot(is.null(neighbor_data_frame) ||
              (neighbor_measure_column >= 1L &&
               neighbor_measure_column <= ncol(neighbor_data_frame)))
    stopifnot(is.null(neighbor_data_frame) ||
              (neighbor_flagged_column >= 1L &&
               neighbor_flagged_column <= ncol(neighbor_data_frame)))
    stopifnot(nchar(timestamp_format) > 0L)
    stopifnot(time_0 <= time_last)
    stopifnot(minimum <= maximum)
    stopifnot(interactive == TRUE || interactive == FALSE)
    stopifnot(nchar(main_title) > 0L)
    stopifnot(nchar(x_label) > 0L)
    stopifnot(nchar(y_label) > 0L)

    ASNAT_dprint("In draw_timeseries_plot(%s)\n", main_title)

    flagged_point_color <- "gray"

    flagged <- data_frame[[flagged_column]]
    point_colors <- ifelse(flagged == "0", "black", flagged_point_color)
    has_flagged_points <-
      length(which(data_frame[[flagged_column]] != "0")) > 0L

    neighbor_point_colors <- NULL

    if (!is.null(neighbor_data_frame)) {
      neighbor_point_colors <-
        ifelse(flagged == "0", "black", flagged_point_color)
      has_flagged_points <- has_flagged_points ||
        (length(which(neighbor_data_frame[[neighbor_flagged_column]] != "0"))
         > 0L)
    }

    result <- NULL

    if (!is.null(file_name)) {

      # Don't output pdf of interactive plots.
      # Instead use built-in interactive plot feature 'Download plot as a png'.

      interactive <- FALSE
      ASNAT_dprint("In draw_timeseries_plot() calling pdf(%s)\n", file_name)
      pdf(file_name, width = the_pdf_width, height = the_pdf_height,
          pointsize = the_pdf_point_size)
      plot_result <-
        make_timeseries_plot_helper(data_frame,
                                    timestamp_column, site_column,
                                    measure_column, flagged_column,
                                    timestamp_format,
                                    time_0, time_last, minimum, maximum,
                                    interactive, main_title, x_label, y_label,
                                    point_colors, has_flagged_points,
                                    make_basic_timeseries_plot, NULL)

      if (!is.null(neighbor_data_frame)) {
        plot_result <-
          make_timeseries_plot_helper(neighbor_data_frame,
                                      neighbor_timestamp_column,
                                      neighbor_site_column,
                                      neighbor_measure_column,
                                      neighbor_flagged_column,
                                      timestamp_format,
                                      time_0, time_last, minimum, maximum,
                                      interactive, main_title, x_label, y_label,
                                      neighbor_point_colors,
                                      has_flagged_points,
                                      make_basic_timeseries_plot, plot_result)
      }

      result <- plot_result
      dev.off()
    } else if (!interactive) {
      result <- renderPlot({
        plot_result <-
          make_timeseries_plot_helper(data_frame,
                                      timestamp_column, site_column,
                                      measure_column, flagged_column,
                                      timestamp_format,
                                      time_0, time_last, minimum, maximum,
                                      interactive, main_title, x_label, y_label,
                                      point_colors, has_flagged_points,
                                      make_basic_timeseries_plot, NULL)

        if (!is.null(neighbor_data_frame)) {
          plot_result <-
            make_timeseries_plot_helper(neighbor_data_frame,
                                        neighbor_timestamp_column,
                                        neighbor_site_column,
                                        neighbor_measure_column,
                                        neighbor_flagged_column,
                                        timestamp_format,
                                        time_0, time_last, minimum, maximum,
                                        interactive,
                                        main_title, x_label, y_label,
                                        neighbor_point_colors,
                                        has_flagged_points,
                                        make_basic_timeseries_plot, plot_result)
        }

        return(plot_result)
      })
    } else {
      result <- renderPlotly({
        plot_result <-
          make_timeseries_plot_helper(data_frame,
                                      timestamp_column, site_column,
                                      measure_column, flagged_column,
                                      timestamp_format,
                                      time_0, time_last, minimum, maximum,
                                      interactive, main_title, x_label, y_label,
                                      point_colors, has_flagged_points,
                                      make_interactive_timeseries_plot, NULL)

        if (!is.null(neighbor_data_frame)) {
          plot_result <-
            make_timeseries_plot_helper(neighbor_data_frame,
                                        neighbor_timestamp_column,
                                        neighbor_site_column,
                                        neighbor_measure_column,
                                        neighbor_flagged_column,
                                        timestamp_format,
                                        time_0, time_last, minimum, maximum,
                                        interactive,
                                        main_title, x_label, y_label,
                                        neighbor_point_colors,
                                        has_flagged_points,
                                        make_interactive_timeseries_plot,
                                        plot_result)
        }

        return(plot_result)
      })
    }


    return(result)
  }



  # Draw dataset time-series plot of selected dataset (possibly filtered ids)
  # or, if a single site_id is specified with neighbor ids then plot single site
  # and its neighbors:

  draw_dataset_timeseries_plot <-
  function(dataset_name, dataset_variable, filter_ids,
           neighbor_dataset_name, neighbor_dataset_variable, neighbor_ids,
           neighbor_distance, save_to_file) {

    ASNAT_dprint("In draw_dataset_timeseries_plot(%s %s)\n",
                 dataset_name, dataset_variable)

    interactive <- use_interactive_plots(model) && !save_to_file

    # Get the data frame subsetted by filter_ids and store unique site ids:

    the_dataset <- set_dataset_variable(dataset_name, dataset_variable)
    the_data_frame <- data_frame(the_dataset)
    subset_data_frame <- the_data_frame
    column_names <- colnames(the_data_frame)
    site_column <- ASNAT_site_column_index(column_names)
    is_single_site <- FALSE
    single_site_id <- 0L

    # Filter the data frame by filter_ids:

    if (!is.null(filter_ids)) {
      sites <- filter_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      matched_rows <- which(the_data_frame[[site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)
      is_single_site <- length(sites) == 1L

      if (is_single_site) {
        single_site_id <- sites[[1L]]
      }

      subset_data_frame <- the_data_frame[matched_rows, ]
    }

    if (nrow(subset_data_frame) < 1L) return(NULL)

    # Filter the neighbor data frame by neighbor_ids:

    neighbor_dataset <- NULL
    neighbor_data_frame <- NULL
    subset_neighbor_data_frame <- NULL
    neighbor_column_names <- NULL
    neighbor_site_column <- -1L

    if (is_single_site &&
        !is.null(neighbor_dataset_name) &&
        !is.null(neighbor_dataset_variable) &&
        !is.null(neighbor_ids)) {
      neighbor_dataset <-
        set_dataset_variable(neighbor_dataset_name, neighbor_dataset_variable)
      neighbor_data_frame <- data_frame(neighbor_dataset)
      neighbor_column_names <- colnames(neighbor_data_frame)
      neighbor_site_column <- ASNAT_site_column_index(neighbor_column_names)
      sites <- neighbor_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      matched_rows <-
        which(neighbor_data_frame[[neighbor_site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)
      subset_neighbor_data_frame <- neighbor_data_frame[matched_rows, ]
    }

    # Create main title with date range, bbox, etc.
    # and y-axis label with measure and units
    # and x-axis with timestamps:

    units <- variable_units(the_dataset)
    source <- coverage_source(the_dataset)
    source_variable <- source_variable(the_dataset)
    fancy_source_variable <- source_variable
    the_variable <- variable_name(the_dataset)
    fancy_variable <- the_variable

    if (interactive) {
      fancy_variable <- fancy_label(the_variable)
      units <- fancy_label(units)
      fancy_source_variable <-
        paste0(coverage_source(the_dataset), ".",
               fancy_label(variable_name(the_dataset)))
    }

    # Get date-time range:

    first_model_timestamp <- first_timestamp(model)
    last_model_timestamp <- last_timestamp(model)
    model_timestep_size <- timestep_size(model)
    date_range <- substr(first_model_timestamp, 1L, 10L)

    if (days(model) > 1L) {
      date_range <-
        paste0(date_range, " - ", substr(last_model_timestamp, 1L, 10L))
    }

    averaging <- "Hourly"
    timestamp_length <- 13L
    timestamp_format <- "%Y-%m-%dT%H"

    if (model_timestep_size == "days") {
      averaging <- "Daily"
      timestamp_length <- 10L
      timestamp_format <- "%Y-%m-%d"
    }

    first_model_timestamp <- substr(first_model_timestamp, 1L, timestamp_length)
    last_model_timestamp <- substr(last_model_timestamp, 1L, timestamp_length)
    time_0 <- as.POSIXct(first_model_timestamp, timestamp_format, tz = "UTC")
    time_last <- as.POSIXct(last_model_timestamp, timestamp_format, tz = "UTC")

    # Get bounds of viewing area:

    west <- west_bound(the_dataset)
    east <- east_bound(the_dataset)
    south <- south_bound(the_dataset)
    north <- north_bound(the_dataset)

    main_title <-
      sprintf("Dataset %s Time-series: %s\n%s (%0.4f, %0.4f) - (%0.4f, %0.4f)",
              averaging,
              ifelse(interactive, fancy_source_variable, source_variable),
              date_range, west, east, south, north)

    y_label <-
      paste0(ifelse(interactive, fancy_variable, the_variable),
             "(", units, ")")

    x_label <-
      paste0(if (model_timestep_size == "hours") "Hours" else "Days",
             " from ", first_model_timestamp, " to ", last_model_timestamp)

    # If plotting a single site with its neighbors then recreate main title:

    neighbor_source_variable <- NULL

    if (is_single_site &&
        !is.null(neighbor_dataset_name) &&
        !is.null(neighbor_dataset_variable) &&
        !is.null(neighbor_ids)) {
      longitudes <- subset_data_frame[[2L]]
      latitudes <- subset_data_frame[[3L]]
      longitude <- longitudes[[1L]]
      latitude <- latitudes[[1L]]
      neighbor_source_variable <- source_variable(neighbor_dataset)
      neighbor_variable <- variable_name(neighbor_dataset)
      fancy_neighbor_source_variable <- neighbor_source_variable

      if (interactive) {
        fancy_neighbor_source_variable <-
          paste0(coverage_source(neighbor_dataset), ".",
                 fancy_label(neighbor_variable))
      }

      main_title <-
        sprintf("%s Site %d Neighbors (<= %dm) %s Time-Series:\n%s vs %s\n%s (%0.4f, %0.4f)",
                source, single_site_id, neighbor_distance, averaging,
                ifelse(interactive, fancy_source_variable, source_variable),
                ifelse(interactive, fancy_neighbor_source_variable, neighbor_source_variable),
                date_range, longitude, latitude)
    }

    ASNAT_dprint("Rendering timeseriesplot with %s\n", source_variable)

    pdf_file <- NULL

    if (save_to_file) {
      directory <- output_directory(model)
      site_string <- ""

      if (is_single_site && !is.null(neighbor_source_variable)) {
        site_string <-
          paste0("_site_", single_site_id, "_neighbors_",
                 neighbor_source_variable)
      }

      pdf_file <-
        paste0(directory, "/",
               gsub(fixed = TRUE, ".", "_", source_variable),
               site_string,
               "_timeseries.pdf")
      saved_plot_files <<- append(saved_plot_files, pdf_file)
    }

    measure_column <- variable_column(the_dataset)
    measures <- subset_data_frame[[measure_column]]
    minimum <- min(measures, na.rm = TRUE)
    maximum <- max(measures, na.rm = TRUE)
    flagged_column <- ASNAT_flagged_column_index(column_names)

    # Compute neighbor column indices and update min/max:

    neighbor_measure_column <- -1L
    neighbor_flagged_column <- -1L

    if (!is.null(subset_neighbor_data_frame)) {
      neighbor_measure_column <- variable_column(neighbor_dataset)
      neighbor_flagged_column <-
        ASNAT_flagged_column_index(neighbor_column_names)
      neighbor_measures <- subset_neighbor_data_frame[[neighbor_measure_column]]
      neighbor_minimum <- min(neighbor_measures, na.rm = TRUE)
      neighbor_maximum <- max(neighbor_measures, na.rm = TRUE)
      minimum <- min(minimum, neighbor_minimum, na.rm = TRUE)
      maximum <- max(maximum, neighbor_maximum, na.rm = TRUE)
    }

    timestamp_column <- 1L

    result <-
      draw_timeseries_plot(subset_data_frame,
                           timestamp_column, site_column, measure_column,
                           flagged_column,
                           subset_neighbor_data_frame,
                           timestamp_column, neighbor_site_column,
                           neighbor_measure_column,
                           neighbor_flagged_column,
                           timestamp_format, time_0, time_last,
                           minimum, maximum,
                           interactive, main_title, x_label, y_label, pdf_file)
    return(result)
  }



  ########################### Scatterplot functions ##########################


  # Helper to make stats label for scatter plot:

  compute_scatterplot_stats <- function(x_values, y_values, interactive) {

    n <- length(x_values)
    correlation <- if (n > 1L) cor(x_values, y_values) else 0.0
    r_squared <- correlation * correlation
    err <- x_values - y_values
    se <- err * err
    mse <- mean(se, na.rm = TRUE)
    rmse <- sqrt(mse)
    mean_x_values <- mean(x_values, na.rm = TRUE)
    nrmse <- rmse / mean_x_values * 100.0
    abs_x_values <- abs(x_values)
    nmbe <- mean(err / abs_x_values, na.rm = TRUE) * 100.0
    slope <- cov(x_values, y_values) / var(x_values, na.rm = TRUE)
    y_intercept <-
      mean(y_values, na.rm = TRUE) - mean(x_values, na.rm = TRUE) * slope
    label <-
      sprintf(paste0("N: %d\n",
                     "Slope: %0.2f\n",
                     "Y-Intercept: %0.2f\n",
                     "R2: %0.2f\n",
                     "RMSE: %0.2f\n",
                     "NRMSE: %0.2f%%\n",
                     "NMBE: %0.2f%%"),
             n, slope, y_intercept, r_squared, rmse, nrmse, nmbe)

    # FIX: Replace R2 in stats_label with R-superscript-2:
    # https://www.statology.org/superscript-subscript-in-r/
    # https://coderclub.w.uib.no/2015/05/07/expressions-in-r/

    FANCY_LABEL_HACK <- FALSE

    if (FANCY_LABEL_HACK) {
      stats_label_before_R2 <-
        sprintf(paste0("N: %d\n",
                       "Slope: %0.2f\n",
                       "Y-Intercept: %0.2f\n"),
                n, slope, y_intercept)
      stats_label_after_R2 <-
        sprintf(paste0(": %0.2f\n",
                       "RMSE: %0.2f\n",
                       "NRMSE: %0.2f%%\n",
                       "NMBE: %0.2f%%"),
                r_squared, rmse, nrmse, nmbe)

      if (!interactive) {
        label <- paste0(label, "\n_____ is regression line")
      }

      label <-
        bquote(.(stats_label_before_R2)~R^2~.(stats_label_after_R2))
    }

    result <- list(label = label,
                   n = n, slope = slope, y_intercept = y_intercept,
                   r_squared = r_squared, rmse = rmse, nrmse = nrmse,
                   nmbe = nmbe)
    return(result)
  }



  # Helper to draw basic scatter plot:
  # https://r-coder.com/scatter-plot-r/
  # https://r-charts.com/base-r/title/

  make_basic_scatterplot <-
  function(x_values, y_values, main_title, x_label, y_label,
           has_flagged_points, point_colors,
           legend_label = NULL, legend_categories = NULL,
           legend_colormap = NULL) {

    x_minimum <- min(x_values, na.rm = TRUE)
    x_maximum <- max(x_values, na.rm = TRUE)
    y_minimum <- min(y_values, na.rm = TRUE)
    y_maximum <- max(y_values, na.rm = TRUE)
    y_range <- y_maximum - y_minimum
    text_line_height <- y_range * 0.05
    stats_label_x <- x_minimum
    stats_label_y <- y_minimum + y_range * 0.6
    yx_line_label_x <- stats_label_x
    yx_line_label_y <- stats_label_y - text_line_height
    yx_line_label <- "_____ is line y = x"
    stats <- compute_scatterplot_stats(x_values, y_values, FALSE)
    stats_label <- stats$label
    slope <- stats$slope
    y_intercept <- stats$y_intercept

    plot(x = x_values, y = y_values,
         xlim = c(x_minimum, x_maximum),
         ylim = c(y_minimum, y_maximum),
         main = main_title,
         xlab = x_label, ylab = y_label,
         las = 1L,
         col = point_colors)
    abline(a = 0.0, b = 1.0, col = "lightgray")

    if (!is.na(slope) && !is.na(y_intercept)) {
      abline(a = y_intercept, b = slope)
    }

    text(stats_label, x = stats_label_x, y = stats_label_y, adj = c(0, 0))
    text(labels = yx_line_label,
         x = yx_line_label_x, y = yx_line_label_y, adj = c(0, 0),
         col = "lightgray")

    if (has_flagged_points) {
      flagged_label <- "o = flagged"
      flagged_point_color <- "gray"
      flagged_label_x <- stats_label_x
      flagged_label_y <- yx_line_label_y - text_line_height
      text(labels = flagged_label,
           x = flagged_label_x, y = flagged_label_y, adj = c(0, 0),
           col = flagged_point_color)
    }

    if (!is.null(legend_label) && !is.null(legend_colormap)) {
      legend(x = "bottomright",
             bty = "o",
             legend = rev(legend_categories),
             fill = rev(legend_colormap),
             title = legend_label,
             cex = 0.85)
    }
  }



  # Helper to draw interactive scatter plot:

  make_interactive_scatterplot <-
  function(x_values, y_values, main_title, x_label, y_label,
           has_flagged_points, point_colors,
           point_size = 4L,
           legend_label = NULL, labeled_legend_colormap = NULL,
           data_legend_categories = NULL) {

    x_minimum <- min(x_values, na.rm = TRUE)
    x_maximum <- max(x_values, na.rm = TRUE)
    y_minimum <- min(y_values, na.rm = TRUE)
    y_maximum <- max(y_values, na.rm = TRUE)
    x_range <- x_maximum - x_minimum
    y_range <- y_maximum - y_minimum
    stats_label_x <- x_minimum + x_range * 0.12
    stats_label_y <- y_minimum + y_range * 0.6
    stats <- compute_scatterplot_stats(x_values, y_values, TRUE)
    stats_label <- stats$label
    slope <- stats$slope
    y_intercept <- stats$y_intercept

    result <- NULL
    the_legend <- NULL

    if (is.null(legend_label)) {
      result <-
        plotly::plot_ly(x = x_values, y = y_values,
                        type = "scatter", mode = "markers",
                        symbols = "o",
                        marker = list(size = point_size,
                                      symbols = "circle-open",
                                      color = point_colors),
                        name = "Neighbor pair measurements")
    } else {
      the_legend <- list(title = list(text = legend_label))
      data_frame <- data.frame(x_values = x_values, y_values = y_values,
                               categories = as.factor(data_legend_categories))
      result <-
        plotly::plot_ly(data_frame) %>%
        add_markers(x = ~x_values,
                    y = ~y_values,
                    color = ~categories,
                    colors = labeled_legend_colormap,
                    type = "scatter", mode = "markers",
                    symbols = "o",
                    marker = list(size = point_size,
                                  symbols = "circle-open"))
    }

    result <- result %>%
      plotly::layout(title = main_title,
                     legend = the_legend,
                     margin = list(t = 100, pad = 1),
                     xaxis = list(title = x_label),
                     yaxis = list(title = y_label)) %>%
      plotly::add_trace(x = c(x_minimum, x_maximum),
                        y = c(x_minimum, x_maximum),
                        type = "scatter", mode = "lines",
                        marker = NULL,
                        line = list(color = "lightgray",
                                    dash = "dotted",
                                    width = 1L),
                        name = "line y = x") %>%
      plotly::add_trace(x = c(x_minimum, x_maximum),
                        y = c(slope * x_minimum + y_intercept,
                              slope * x_maximum + y_intercept),
                        type = "scatter", mode = "lines",
                        marker = NULL,
                        line = list(color = "black",
                                    dash = "dashed",
                                    width = 1L),
                        name = "regression line") %>%
      plotly::layout(annotations = list(text = stats_label,
                                        textposition = "top left",
                                        align = "left",
                                        x = stats_label_x,
                                        y = stats_label_y))

    if (has_flagged_points) {
      flagged_label <- "o = flagged"
      flagged_point_color <- "gray"

      result <- result %>%
        plotly::layout(annotations = list(showarrow = FALSE,
                                          text = flagged_label,
                                          textposition = "top left",
                                          x = stats_label_x,
                                          y = y_maximum,
                                          color = flagged_point_color))
    }

    return(result)
  }



  # Helper method to make the plot with ggplot (used on Corrections tab):

  make_static_scatterplot <- function(plot_data, main_title, x_label, y_label,
                                      has_flagged_points, point_colors,
                                      point_size = 2L) {

    # Convert point_size to numeric
    point_size <- as.numeric(point_size)
    # Compute scatterplot stats per device_id
    stats_df <- plot_data %>%
      group_by(device_id) %>%

      do({
        x_values <- .$x
        y_values <- .$y
        stats <- compute_scatterplot_stats(x_values, y_values)
        data.frame(device_id = unique(.$device_id),
                   slope = stats$slope,
                   y_intercept = stats$y_intercept,
                   label = stats$label)
      })

    # Create a data frame for lines per device
    lines_data <- stats_df %>%
      mutate(Type = "Regression Line") %>%
      select(device_id, slope, y_intercept, Type)

    # Add y = x line
    lines_data_yx <- data.frame(device_id = unique(plot_data$device_id),
                                slope = 1,
                                y_intercept = 0,
                                Type = "y = x")

    lines_data <- bind_rows(lines_data, lines_data_yx)

    # Base ggplot scatter plot
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(color = "blue", shape = 20, size = point_size) +
      labs(title = main_title, x = x_label, y = y_label) +
      geom_abline(data = lines_data,
                  aes(slope = slope, intercept = y_intercept, linetype = Type),
                  color = "black",
                  size = 1) +
      scale_linetype_manual(values = c("y = x" = "dotted", "Regression Line" = "solid")) +
      facet_wrap(~ device_id) +
      theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top",
      # Remove default panel borders
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      # Customize strip text for better visibility
      strip.background = element_rect(fill = "lightgrey", color = "black", size = 1),
      strip.text = element_text(face = "bold")
    )


    # Add stats labels to each facet
    p <- p + geom_text(
      data = stats_df,
      aes(x = 0.05, y = 0.95, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 4,
      color = "black",
      # Use relative coordinates within each facet
      position = position_nudge(x = 0, y = 0),
      check_overlap = TRUE
    )

    return(p)
  }



  # Draw scatter plot of selected X/Y datasets:
  # https://r-coder.com/scatter-plot-r/
  # https://r-charts.com/base-r/title/

  draw_scatterplot <- function(dataset_name_x, dataset_x_variable,
                               dataset_name_y, dataset_y_variable,
                               single_site,
                               only_unflagged,
                               save_to_file) {

    ASNAT_dprint("In draw_scatterplot(%s (%s), %s (%s))\n",
                 dataset_name_x, dataset_x_variable,
                 dataset_name_y, dataset_y_variable)

    result <- NULL
    data_frame <- comparison_data_frame(model)

    # data_frame columns are:
    # 1 = timestamp, 2 = id_x, 3 = measure_x, 4 = id_y, 5 = measure_y, 6 = flagged_y

    if (nrow(data_frame) > 0L) {

      # If a single dataset_x site is specified then subset to only the
      # rows of that site.

      if (single_site > 0L) {
        matched_rows <- which(data_frame[[2L]] == single_site)
        stopifnot(length(matched_rows) > 0L)
        data_frame <- data_frame[matched_rows, ]
      }

      unflagged_rows <- which(data_frame[[6L]] == "0")

      if (only_unflagged == FALSE || length(unflagged_rows) > 0L) {
        dataset_x <- set_dataset_variable(dataset_name_x, dataset_x_variable)
        dataset_y <- set_dataset_variable(dataset_name_y, dataset_y_variable)
        source_variable_x <- source_variable(dataset_x)
        source_variable_y <- source_variable(dataset_y)
        source <- coverage_source(dataset_x)
        subtitle <- comparison_subtitle(model)
        main_title <- comparison_title(model)
        main_title <- gsub(fixed = TRUE, ": ", ":\nx = ", main_title)
        main_title <- gsub(fixed = TRUE, " vs ", " y = ", main_title)

        if (single_site > 0L) {
          main_title <- paste0(source, " Site ", single_site, " ", main_title)

          # Change subtitle lon-lat bounds to single site location:

          dataset_x <- find_dataset(model, dataset_name_x)
          data_frame_x <- data_frame(dataset_x)
          column_names <- colnames(data_frame_x)
          site_column <- ASNAT_site_column_index(column_names)
          site_rows <- which(data_frame_x[[site_column]] == single_site)
          first_site_row <- site_rows[[1L]]
          longitude <- data_frame_x[first_site_row, 2L]
          latitude <- data_frame_x[first_site_row, 3L]
          date_range <- unlist(strsplit(subtitle, "(", fixed = TRUE))[1L]
          subtitle <-
            sprintf("%s (%0.5f, %0.5f)", date_range, longitude, latitude)
        }

        main_title <- paste0(main_title, "\n", subtitle)

        x_y_column_names <- colnames(data_frame)
        x_label <- x_y_column_names[[3L]]
        y_label <- x_y_column_names[[5L]]

        using_fancy_plots <- !save_to_file && use_interactive_plots(model)

        measures_x <- data_frame[[3L]]
        measures_y <- data_frame[[5L]]
        has_flagged_points <- FALSE
        point_colors <- NULL
        legend_label <- NULL
        legend_categories <- NULL
        legend_colormap <- NULL
        labeled_legend_colormap <- NULL
        data_legend_categories <- NULL

        # Color points by measures_z (humidity) if it is available:

        if (ncol(data_frame) == 7L &&
            grepl("(%)", x_y_column_names[[7L]], fixed = TRUE)) {
          legend_label <- x_y_column_names[[7L]]
          measures_z <- data_frame[[7L]]

          if (only_unflagged) {
            measures_z <- measures_z[unflagged_rows]
          }

          minimum_z <- 0.0
          maximum_z <- 100.0
          legend_colormap <- the_blue_colormap
          legend_colormap <- legend_colormap[-length(legend_colormap)]
          na_color <- "red"
          legend_colormap[[1L]] <- na_color
          legend_categories <- c("NA", "0", "20", "40", "60", "80+")
          stopifnot(length(legend_colormap) == length(legend_categories))
          point_colors <-
            data_colors(measures_z, minimum_z, maximum_z, legend_colormap,
                        na_color)

          if (using_fancy_plots) {
            labeled_legend_colormap <- legend_colormap
            names(labeled_legend_colormap) <- legend_categories
            count <- length(measures_z)
            data_legend_categories <- rep("0", count)

            for (index in 1L:count) {
              z_value <- measures_z[[index]]

              if (is.na(z_value)) {
                data_legend_categories[[index]] <- "NA"
              } else if (z_value >= 80.0) {
                data_legend_categories[[index]] <- "80+"
              } else if (z_value >= 60.0) {
                data_legend_categories[[index]] <- "60"
              } else if (z_value >= 40.0) {
                data_legend_categories[[index]] <- "40"
              } else if (z_value >= 20.0) {
                data_legend_categories[[index]] <- "20"
              }
            }
          }
        }

        if (only_unflagged) {
          main_title <-
            gsub(fixed = TRUE,
                 "Neighboring", "Unflagged Neighboring", main_title)
          measures_x <- measures_x[unflagged_rows]
          measures_y <- measures_y[unflagged_rows]

          if (is.null(point_colors)) {
            point_colors <- rep("black", length(measures_x))
          }
        } else {

          if (is.null(point_colors)) {
            point_colors <-
              ifelse(data_frame[[6L]] == "0", "black", flagged_point_color)
          }

          has_flagged_points <- length(which(data_frame[[6L]] != "0")) > 0L
        }

        ASNAT_dprint("measures_x:\n")
        ASNAT_debug(str, measures_x)
        ASNAT_dprint("measures_y:\n")
        ASNAT_debug(str, measures_y)
        ASNAT_dprint("Rendering scatterplot\n")
        result <- NULL

        if (save_to_file) {
          directory <- output_directory(model)
          first_part <- gsub(fixed = TRUE, ".", "_", source_variable_x)

          if (single_site > 0L) {
            first_part <-
              paste0(first_part, "_site_", single_site, "_neighbors")
          }

          file_name <-
            paste0(directory, "/",
                   first_part,
                   "_vs_",
                   gsub(fixed = TRUE, ".", "_", source_variable_y),
                   "_scatterplot",
                   if (only_unflagged) "_unflagged.pdf" else ".pdf")
          saved_plot_files <<- append(saved_plot_files, file_name)
          ASNAT_dprint("In draw_scatterplot() calling pdf(%s)\n", file_name)
          pdf(file_name, width = the_pdf_width, height = the_pdf_height,
              pointsize = the_pdf_point_size)
          make_basic_scatterplot(measures_x, measures_y,
                                 main_title, x_label, y_label,
                                 has_flagged_points, point_colors,
                                 legend_label, legend_categories,
                                 legend_colormap)
          dev.off()
        } else {

          if (!using_fancy_plots) {
            result <- renderPlot({
              make_basic_scatterplot(measures_x, measures_y,
                                     main_title, x_label, y_label,
                                     has_flagged_points, point_colors,
                                     legend_label, legend_categories,
                                     legend_colormap)
            })
          } else {
            fancy_main_title <- fancy_label(main_title)
            fancy_x_label <- fancy_label(x_label)
            fancy_y_label <- fancy_label(y_label)

            result <- renderPlotly({
              make_interactive_scatterplot(measures_x, measures_y,
                                           fancy_main_title,
                                           fancy_x_label, fancy_y_label,
                                           has_flagged_points,
                                           point_colors,
                                           point_size = 4L,
                                           legend_label,
                                           labeled_legend_colormap,
                                           data_legend_categories)
            })
          }
        }
      }
    }

    return(result)
  }



  ######################### AQI summary plot functions ########################



  # Helper to draw basic aqi summary plot:

  make_basic_aqi_summary_plot <-
  function(aqi_data_frame, main_title, x_label, y_label, legend_inset = -0.2) {
    par(mar = c(7, 4, 4, 10), xpd = TRUE)
    barplot(height = t(as.matrix(aqi_data_frame[, -1L])),
            col = ASNAT_aqi_colormap,
            legend = ASNAT_aqi_names,
            args.legend = list(x = "topright", inset = c(legend_inset, 0.0)),
            main = main_title,
            axisnames = TRUE, names.arg = aqi_data_frame[[1L]],
            ylab = y_label,
            ylim = c(0.0, 100.0),
            las = 2L)
    mtext(x_label, side = 1, line = -1, outer = TRUE)
  }



  # Helper to draw interactive aqi summary plot:
  # https://plotly.com/r/bar-charts/

  make_interactive_aqi_summary_plot <-
  function(aqi_data_frame, main_title, x_label, y_label) {

    aqi_named_colors <- setNames(ASNAT_aqi_colormap, ASNAT_aqi_names)
    melted_data_frame <- reshape2::melt(aqi_data_frame, id.vars = "site_id")
    # Keep the site id at its original order:
    melted_data_frame$site_id <-
      factor(melted_data_frame$site_id, levels = unique(aqi_data_frame$site_id))
    # Remove underbar from AQI category:
    melted_data_frame$variable <-
      gsub(fixed = TRUE, "_", " ", melted_data_frame$variable)
    # Restore original order:
    melted_data_frame$variable <-
      factor(melted_data_frame$variable, levels = ASNAT_aqi_names)
      gsub(fixed = TRUE, "_", " ", melted_data_frame$variable)
    result <- plotly::plot_ly(data = melted_data_frame,
                              x = ~site_id,
                              y = ~value,
                              type = "bar",
                              color = ~variable,
                              colors = aqi_named_colors) %>%
      plotly::layout(title = main_title,
                     margin = list(t = 100, pad = 1),
                     xaxis = list(title = x_label, tickangle = 270),
                     yaxis = list(title = y_label, range = c(0, 100)),
                     barmode = "stack")
    return(result)
  }



  # Draw either basic or interactive AQI summary plot of a dataset:

  draw_aqi_summary_plot <-
  function(dataset_name, dataset_variable, filter_ids,
           neighbor_dataset_name, neighbor_dataset_variable,
           neighbor_ids, neighbor_distance, save_to_file) {

    ASNAT_dprint("In draw_aqi_summary_plot(%s)\n", dataset_name)

    # Don't save interactive plots as PDF files.

    interactive <- use_interactive_plots(model) && !save_to_file

    # Get the data frame subsetted by filter_ids:

    the_dataset <- set_dataset_variable(dataset_name, dataset_variable)
    the_data_frame <- data_frame(the_dataset)
    subset_data_frame <- the_data_frame
    column_names <- colnames(the_data_frame)
    site_column <- ASNAT_site_column_index(column_names)
    is_single_site <- length(filter_ids) == 1L
    single_site_id <- 0L

    # Filter the data frame by filter_ids:

    if (!is.null(filter_ids)) {
      sites <- filter_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      is_single_site <- length(sites) == 1L
      matched_rows <- which(the_data_frame[[site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)

      if (is_single_site) {
        single_site_id <- sites[[1L]]
      }

      subset_data_frame <- the_data_frame[matched_rows, ]
    }

    if (nrow(subset_data_frame) < 1L) return(NULL)

    # Filter the neighbor data frame by neighbor_ids:

    neighbor_dataset <- NULL
    neighbor_data_frame <- NULL
    subset_neighbor_data_frame <- NULL
    neighbor_column_names <- NULL
    neighbor_site_column <- 0L

    if (is_single_site &&
        !is.null(neighbor_dataset_name) &&
        !is.null(neighbor_dataset_variable) &&
        !is.null(neighbor_ids)) {
      neighbor_dataset <-
        set_dataset_variable(neighbor_dataset_name, neighbor_dataset_variable)
      neighbor_data_frame <- data_frame(neighbor_dataset)
      neighbor_column_names <- colnames(neighbor_data_frame)
      neighbor_site_column <- ASNAT_site_column_index(neighbor_column_names)
      sites <- neighbor_ids
      sites <- sort.int(sites)
      sites <- unique(sites)
      matched_rows <-
        which(neighbor_data_frame[[neighbor_site_column]] %in% sites)
      stopifnot(length(matched_rows) > 0L)
      subset_neighbor_data_frame <- neighbor_data_frame[matched_rows, ]
    }

    # Create main title with date range, bbox, etc.
    # and y-axis label with measure and units.

    source <- coverage_source(the_dataset)
    source_variable <- source_variable(the_dataset)
    fancy_source_variable <- source_variable

    if (interactive) {
      fancy_source_variable <-
        paste0(coverage_source(the_dataset), ".",
               fancy_label(variable_name(the_dataset)))
    }

    # Get date-time range:

    first_model_timestamp <- first_timestamp(model)
    last_model_timestamp <- last_timestamp(model)
    model_timestep_size <- timestep_size(model)
    date_range <- substr(first_model_timestamp, 1L, 10L)

    if (days(model) > 1L) {
      date_range <-
        paste0(date_range, " - ", substr(last_model_timestamp, 1L, 10L))
    }

    averaging <- "Hourly"
    timestamp_length <- 13L

    if (model_timestep_size == "days") {
      averaging <- "Daily"
      timestamp_length <- 10L
    }

    first_model_timestamp <-
      substr(first_model_timestamp, 1L, timestamp_length)

    # Get bounds of viewing area:

    west <- west_bound(the_dataset)
    east <- east_bound(the_dataset)
    south <- south_bound(the_dataset)
    north <- north_bound(the_dataset)

    main_title <-
      sprintf("Dataset AQI Summary: %s %s\n%s (%0.4f, %0.4f) - (%0.4f, %0.4f)",
              averaging,
              ifelse(interactive, fancy_source_variable, source_variable),
              date_range, west, east, south, north)

    # If plotting a single site with its neighbors then recreate main title:

    if (!is.null(subset_neighbor_data_frame)) {
      longitudes <- subset_data_frame[[2L]]
      latitudes <- subset_data_frame[[3L]]
      longitude <- longitudes[[1L]]
      latitude <- latitudes[[1L]]
      neighbor_source_variable <- source_variable(neighbor_dataset)
      neighbor_variable <- variable_name(neighbor_dataset)
      fancy_neighbor_source_variable <- neighbor_source_variable

      if (interactive) {
        fancy_neighbor_source_variable <-
          paste0(coverage_source(neighbor_dataset), ".",
                 fancy_label(neighbor_variable))
      }

      main_title <-
        sprintf("%s Site %d Neighbors (<= %dm) AQI Summary:\n%s %s vs %s\n%s (%0.4f, %0.4f)",
                source, single_site_id, neighbor_distance, averaging,
                ifelse(interactive, fancy_source_variable, source_variable),
                ifelse(interactive, fancy_neighbor_source_variable, neighbor_source_variable),
                date_range, longitude, latitude)
    }

    # Plotted data frame will have just two columns: site and measure:

    measure_column <- variable_column(the_dataset)
    subset_data_frame <- subset_data_frame[, c(site_column, measure_column)]
    colnames(subset_data_frame) <- c("site_id", "measure")

    if (is.null(neighbor_dataset)) {

      # Add first/left boxplot as a summary of all (filtered) site measures,
      # prepend rows with id = 0 and all (filtered) measures:

      filtered_row_count <- nrow(subset_data_frame)
      ids0 <- rep(0L, filtered_row_count)
      filtered_measures <- subset_data_frame[[2L]]
      extra_data_frame <- data.frame(site_id = ids0, measure = filtered_measures)
      subset_data_frame <- rbind(extra_data_frame, subset_data_frame)
    } else {

      # Append rows with neighbor site_ids and measures:

      neighbor_measure_column <- variable_column(neighbor_dataset)
      subset_neighbor_data_frame <-
        subset_neighbor_data_frame[, c(neighbor_site_column, neighbor_measure_column)]
      neighbor_measures <- subset_neighbor_data_frame[[2L]]
      neighbor_sites <- subset_neighbor_data_frame[[1L]]
      neighbors_data_frame <-
        data.frame(site_id = neighbor_sites, measure = neighbor_measures)
      subset_data_frame <- rbind(subset_data_frame, neighbors_data_frame)
    }

    # Compute percent of each site's measures in AQI categories:

    is_hourly <- timestep_size(model) == "hours"
    sites <- subset_data_frame[[1L]]
    sites <- sort.int(sites)
    sites <- unique(sites)
    aqi_categories <- length(ASNAT_aqi_names)
    aqi_data_frame <- data.frame(site_id = sites)

    for (index in seq_along(sites)) {
      site <- sites[[index]]
      site_rows <- which(subset_data_frame[[1L]] == site)
      site_measures <- subset_data_frame[site_rows, 2L]
      site_aqi <- ASNAT_aqi_indices(dataset_variable, is_hourly, site_measures)
      count <- length(site_aqi)

      for (aqi_category in 1L:aqi_categories) {
        site_aqi_catagory_percent <-
          100.0 * length(which(site_aqi == aqi_category)) / count
        aqi_data_frame[index, 1L + aqi_category] <- site_aqi_catagory_percent
      }
    }

    colnames(aqi_data_frame) <-
      c("site_id", gsub(fixed = TRUE, " ", "_", ASNAT_aqi_names))


    x_label <- ifelse(is_single_site, "Site_Id", "Site_Id (0 = all)")
    y_label <- "Percentage of each site's measures in AQI categories"

    ASNAT_dprint("Rendering aqi_summary_plot with %s\n", source_variable)
    ASNAT_debug(str, subset_data_frame)

    result <- NULL

    if (save_to_file) {
      directory <- output_directory(model)
      source_variable_x <- source_variable(the_dataset)
      first_part <- gsub(fixed = TRUE, ".", "_", source_variable_x)

      if (single_site_id > 0L) {
        first_part <-
          paste0(first_part, "_site_", single_site_id, "_neighbors")
      }

      file_name <-
        paste0(directory, "/",
               first_part,
               "_aqi_summary_plot.pdf")
      saved_plot_files <<- append(saved_plot_files, file_name)
      ASNAT_dprint("In draw_aqi_summary_plot() calling pdf(%s)\n", file_name)
      pdf(file_name, width = the_pdf_width, height = the_pdf_height,
          pointsize = the_pdf_point_size)
      make_basic_aqi_summary_plot(aqi_data_frame,
                                  main_title, x_label, y_label,
                                  legend_inset = -0.3)
      dev.off()
    } else {

      if (!interactive) {
        result <- renderPlot({
          make_basic_aqi_summary_plot(aqi_data_frame,
                                      main_title, x_label, y_label)
        })
      } else {
        fancy_main_title <- fancy_label(main_title)

        result <- renderPlotly({
          make_interactive_aqi_summary_plot(aqi_data_frame,
                                            fancy_main_title, x_label, y_label)
        })
      }
    }

    return(result)
  }



  ######################### AQI neighbor plot functions #######################



  # Helper to draw basic aqi plot:

  make_basic_aqi_plot <-
  function(x_values, y_values, main_title, x_label, y_label) {
    count <- length(x_values)
    bar_spaces <- rep(0.2, count)
    aqi_categories <- length(ASNAT_aqi_names)

    for (index in seq_along(x_values)) {

      if ((index - 1L) %% aqi_categories == 0L) {
        bar_spaces[[index]] <- 1.0
      }
    }

    title_size <- 1.25

    if (length(grep(fixed = TRUE, " Site ", main_title)) > 0L) {
      title_size <- 1.0
    }

    par(mar = c(7, 4, 4, 10), xpd = TRUE)
    barplot(y_values ~ x_values,
            space = bar_spaces,
            col = ASNAT_aqi_colormap,
            legend = ASNAT_aqi_names,
            args.legend = list(x = "topright", inset = c(-0.2, 0.0)),
            ylim = c(0.0, 100.0),
            main = main_title, cex.main = title_size,
            xlab = x_label, ylab = "",
            las = 2L,
            mgp = c(4, 1, 0))
    title(ylab = y_label, line = 3)
  }



  # Helper to draw interactive aqi plot:
  # https://plotly.com/r/bar-charts/

  make_interactive_aqi_plot <- function(y_values, main_title, y_label) {
    data_frame <- data.frame(ASNAT_aqi_names)
    aqi_categories <- length(ASNAT_aqi_names)
    first <- 1L

    for (index in 1L:aqi_categories) {
      aqi_name <- ASNAT_aqi_names[[index]]
      aqi_name_underscore <- gsub(fixed = TRUE, " ", "_", aqi_name)
      name <- paste0("x=", aqi_name_underscore)
      last <- first + aqi_categories - 1L
      data_frame <- cbind(data_frame, y_values[first:last])
      names(data_frame)[[index + 1L]] <- name
      first <- first + aqi_categories
    }

    melted_data_frame <- reshape2::melt(data_frame, id.vars = "ASNAT_aqi_names")

    # Set the order of 'ASNAT_aqi_names' in the legend:

    melted_data_frame$ASNAT_aqi_names <-
      factor(melted_data_frame$ASNAT_aqi_names, levels = ASNAT_aqi_names)

    aqi_named_colors <- setNames(ASNAT_aqi_colormap, ASNAT_aqi_names)

    result <- plotly::plot_ly(data = melted_data_frame,
                              x = ~variable, y = ~value,
                              type = "bar",
                              color = ~ASNAT_aqi_names,
                              colors = aqi_named_colors)

    result <- result %>% plotly::layout(title = main_title,
                                        margin = list(t = 100, pad = 1),
                                        yaxis = list(title = y_label,
                                                     range = c(0, 100)),
                                        xaxis = list(title = ASNAT_aqi_names))

    return(result)
  }



  # Draw aqi plot of selected X/Y datasets:

  draw_aqi_plot <- function(dataset_name_x, dataset_x_variable,
                            dataset_name_y, dataset_y_variable,
                            single_site,
                            only_unflagged,
                            save_to_file) {

    ASNAT_dprint("In draw_aqi_plot(%s (%s), %s (%s))\n",
                 dataset_name_x, dataset_x_variable,
                 dataset_name_y, dataset_y_variable)

    stopifnot(ASNAT_is_aqi_compatible(dataset_x_variable, dataset_y_variable))
    result <- NULL
    data_frame <- comparison_data_frame(model)

    # data_frame columns are:
    # 1 = timestamp, 2 = id_x, 3 = measure_x, 4 = id_y, 5 = measure_y, 6 = flagged_y

    if (nrow(data_frame) > 0L) {

      # If a single dataset_x site is specified then subset to only the
      # rows of that site.

      if (single_site > 0L) {
        matched_rows <- which(data_frame[[2L]] == single_site)
        stopifnot(length(matched_rows) > 0L)
        data_frame <- data_frame[matched_rows, ]
      }

      unflagged_rows <- which(data_frame[[6L]] == "0")

      if (only_unflagged == FALSE || length(unflagged_rows) > 0L) {
        dataset_x <- set_dataset_variable(dataset_name_x, dataset_x_variable)
        dataset_y <- set_dataset_variable(dataset_name_y, dataset_y_variable)
        source_variable_x <- source_variable(dataset_x)
        source_variable_y <- source_variable(dataset_y)
        source <- coverage_source(dataset_x)
        subtitle <- comparison_subtitle(model)
        main_title <- comparison_title(model)
        main_title <- gsub(fixed = TRUE, ": ", ":\nx = ", main_title)
        main_title <- gsub(fixed = TRUE, " vs ", " y = ", main_title)
        main_title <-
          gsub(fixed = TRUE, "Comparison", "x:y AQI Comparison", main_title)

        if (single_site > 0L) {
          main_title <-
            paste0(source, " Site ", single_site, " has ", main_title)

          # Change subtitle lon-lat bounds to single site location:

          dataset_x <- find_dataset(model, dataset_name_x)
          data_frame_x <- data_frame(dataset_x)
          column_names <- colnames(data_frame_x)
          site_column <- ASNAT_site_column_index(column_names)
          site_rows <- which(data_frame_x[[site_column]] == single_site)
          first_site_row <- site_rows[[1L]]
          longitude <- data_frame_x[first_site_row, 2L]
          latitude <- data_frame_x[first_site_row, 3L]
          date_range <- unlist(strsplit(subtitle, "(", fixed = TRUE))[1L]
          subtitle <-
            sprintf("%s (%0.5f, %0.5f)", date_range, longitude, latitude)
        }

        main_title <- paste0(main_title, "\n", subtitle)

        aqi_categories <- length(ASNAT_aqi_names)
        x_label <-
          paste(collapse = ", ", 1L:aqi_categories, "=", ASNAT_aqi_names)
        y_label <- "Percentage of y measures in AQI category"

        using_fancy_plots <- !save_to_file && use_interactive_plots(model)

        measures_x <- data_frame[[3L]]
        measures_y <- data_frame[[5L]]

        if (only_unflagged) {
          measures_x <- measures_x[unflagged_rows]
          measures_y <- measures_y[unflagged_rows]
        }

        is_hourly <- timestep_size(model) == "hours"
        aqi_indices_x <-
          ASNAT_aqi_indices(dataset_x_variable, is_hourly, measures_x)
        aqi_indices_y <-
          ASNAT_aqi_indices(dataset_y_variable, is_hourly, measures_y)
        values_y <- ASNAT_aqi_comparison(aqi_indices_x, aqi_indices_y)
        values_x <- rep("", aqi_categories * aqi_categories)
        index <- 1L

        for (x in 1L:aqi_categories) {

          for (y in 1L:aqi_categories) {
            values_x[[index]] <- sprintf("x=%d:y=%d", x, y)
            index <- index + 1L
          }
        }

        point_count <- length(aqi_indices_x)

        if (only_unflagged) {
          main_title <- gsub(fixed = TRUE, "Neighboring",
                               sprintf("%d Unflagged Neighboring",
                                       point_count),
                               main_title)
        } else {
          main_title <- gsub(fixed = TRUE, "Neighboring",
                             sprintf("%d Neighboring", point_count),
                             main_title)
        }

        ASNAT_dprint("values_x:\n")
        ASNAT_debug(str, values_x)
        ASNAT_dprint("values_y:\n")
        ASNAT_debug(str, values_y)
        ASNAT_dprint("Rendering aqi plot\n")
        result <- NULL

        if (save_to_file) {
          directory <- output_directory(model)
          first_part <- gsub(fixed = TRUE, ".", "_", source_variable_x)

          if (single_site > 0L) {
            first_part <-
              paste0(first_part, "_site_", single_site, "_neighbors")
          }

          file_name <-
            paste0(directory, "/",
                   first_part,
                   "_vs_",
                   gsub(fixed = TRUE, ".", "_", source_variable_y),
                   "_aqi_plot",
                   if (only_unflagged) "_unflagged.pdf" else ".pdf")
          saved_plot_files <<- append(saved_plot_files, file_name)
          ASNAT_dprint("In draw_aqi_plot() calling pdf(%s)\n", file_name)
          pdf(file_name, width = the_pdf_width, height = the_pdf_height,
              pointsize = the_pdf_point_size)
          make_basic_aqi_plot(values_x, values_y, main_title, x_label, y_label)
          dev.off()
        } else {

          if (!using_fancy_plots) {
            result <- renderPlot({
              make_basic_aqi_plot(values_x, values_y,
                                  main_title, x_label, y_label)
            })
          } else {
            fancy_main_title <- fancy_label(main_title)

            result <- renderPlotly({
              make_interactive_aqi_plot(values_y, fancy_main_title, y_label)
            })
          }
        }
      }
    }

    return(result)
  }



  ##################### Neighbor Statistics Boxplot functions #################

  draw_statistics_boxplots <- function(neighbor_statistics_data_frame,
                                       aqi_statistic_target_ranges,
                                       main_title, interactive, file_name) {

    stopifnot(!is.null(neighbor_statistics_data_frame))
    stopifnot(class(neighbor_statistics_data_frame) == "data.frame")
    stopifnot(ncol(neighbor_statistics_data_frame) >= 2L)
    stopifnot(nrow(neighbor_statistics_data_frame) > 0L)
    stopifnot(!is.null(colnames(neighbor_statistics_data_frame)))
    stopifnot(is.null(aqi_statistic_target_ranges) ||
              class(aqi_statistic_target_ranges) == "list")
    stopifnot(nchar(main_title) > 0L)
    stopifnot(interactive == TRUE || interactive == FALSE)

    ASNAT_dprint("In draw_statistics_boxplots(%s)\n", main_title)

    columns <- ncol(neighbor_statistics_data_frame)
    result <- NULL

    if (!is.null(file_name)) {
      ASNAT_dprint("In draw_statistics_boxplots() calling pdf(%s)\n", file_name)
      pdf(file_name, width = the_pdf_width, height = the_pdf_height,
          pointsize = 12L)
      par(mfrow = c(1L, columns), mar = c(3.1, 4.1, 5.1, 2.1))
      invisible(lapply(2L:columns,
        function(column) {
          y_minimum <-
            min(neighbor_statistics_data_frame[, column], na.rm = TRUE)
          y_maximum <-
            max(neighbor_statistics_data_frame[, column], na.rm = TRUE)
          lower <- NULL
          upper <- NULL

          if (!is.null(aqi_statistic_target_ranges)) {
            y_range <- aqi_statistic_target_ranges[[column - 1L]]
            lower <- y_range[[1L]]
            upper <- y_range[[2L]]

            if (!is.na(lower)) {
              y_minimum <- min(y_minimum, lower, na.rm = TRUE)
              y_maximum <- max(y_maximum, upper, na.rm = TRUE)
            }
          }

          y_limits <- NULL

          if (!is.na(y_minimum) && !is.na(y_maximum) &&
              !is.nan(y_minimum) && !is.nan(y_maximum) &&
              is.finite(y_minimum) && is.finite(y_maximum)) {
            y_limits <- c(y_minimum, y_maximum)
          }

          boxplot(neighbor_statistics_data_frame[, column],
                  ylab = colnames(neighbor_statistics_data_frame)[[column]],
                  ylim = y_limits,
                  las = 2L)

          if (!is.null(aqi_statistic_target_ranges)) {

            # Draw target lines:

            x0 <- 0.6
            x1 <- 1.4
            segments(x0 = x0, y0 = lower,
                     x1 = x1, y1 = lower,
                     col = "gray3", lwd = 2L, lty = 1L)
            segments(x0 = x0, y0 = upper,
                     x1 = x1, y1 = upper,
                     col = "gray3", lwd = 2L, lty = 1L)
          }
      }))

      mtext(main_title, side = 3L, line = -5L, outer = TRUE, font = 2L)

      dev.off()
    } else {

      if (!interactive) {
        result <- renderPlot({

          # https://r-coder.com/boxplot-r/#Multiple_boxplots

          #par(mfrow = c(1L, columns), cex = 1.0)
          par(mfrow = c(1L, columns), cex = 1.0, mar = c(3.1, 4.1, 5.1, 2.1))
          invisible(lapply(2L:columns,
            function(column) {
              y_minimum <-
                min(neighbor_statistics_data_frame[, column], na.rm = TRUE)
              y_maximum <-
                max(neighbor_statistics_data_frame[, column], na.rm = TRUE)
              lower <- NULL
              upper <- NULL

              if (!is.null(aqi_statistic_target_ranges)) {
                y_range <- aqi_statistic_target_ranges[[column - 1L]]
                lower <- y_range[[1L]]
                upper <- y_range[[2L]]

                if (!is.na(lower)) {
                  y_minimum <- min(y_minimum, lower, na.rm = TRUE)
                  y_maximum <- max(y_maximum, upper, na.rm = TRUE)
                }
              }

              y_limits <- NULL

              if (!is.na(y_minimum) && !is.na(y_maximum) &&
                  !is.nan(y_minimum) && !is.nan(y_maximum) &&
                  is.finite(y_minimum) && is.finite(y_maximum)) {
                y_limits <- c(y_minimum, y_maximum)
              }

              boxplot(neighbor_statistics_data_frame[, column],
                      ylab = colnames(neighbor_statistics_data_frame)[[column]],
                      ylim = y_limits,
                      las = 2L)

              if (!is.null(aqi_statistic_target_ranges)) {

                # Draw target lines:

                x0 <- 0.6
                x1 <- 1.4
                segments(x0 = x0, y0 = lower,
                         x1 = x1, y1 = lower,
                         col = "gray3", lwd = 2L, lty = 1L)
                segments(x0 = x0, y0 = upper,
                         x1 = x1, y1 = upper,
                         col = "gray3", lwd = 2L, lty = 1L)
              }
            }))

         mtext(main_title, side = 3L, line = -5L, outer = TRUE,
               font = 2L, cex = 1.25)
        })
      } else {
        result <- renderPlotly({
          column_names <- colnames(neighbor_statistics_data_frame)
          subplots <- rep(NULL, columns - 1L)

          # Nested helper function to draw a horizontal line:
          # https://plotly.com/r/horizontal-vertical-shapes/

          hline <- function(y = 0, color = "gray3") {
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = y,
              y1 = y,
              line = list(color = color)
            )
          }

          invisible(lapply(2L:columns,
            function(column) {
              y_minimum <-
                min(neighbor_statistics_data_frame[, column], na.rm = TRUE)
              y_maximum <-
                max(neighbor_statistics_data_frame[, column], na.rm = TRUE)
              lower <- NA
              upper <- NA

              if (!is.null(aqi_statistic_target_ranges)) {
                y_range <- aqi_statistic_target_ranges[[column - 1L]]
                lower <- y_range[[1L]]
                upper <- y_range[[2L]]

                if (!is.na(lower)) {
                  y_minimum <- min(y_minimum, lower, na.rm = TRUE)
                  y_maximum <- max(y_maximum, upper, na.rm = TRUE)
                }
              }

              y_limits <- NULL

              if (!is.na(y_minimum) && !is.na(y_maximum) &&
                  !is.nan(y_minimum) && !is.nan(y_maximum) &&
                  is.finite(y_minimum) && is.finite(y_maximum)) {
                y_limits <- c(y_minimum, y_maximum)
              }

              shape_list <- NULL

              if (!is.na(lower)) {
                shape_list <- list(hline(lower), hline(upper))
              }

              plot <-
                plotly::plot_ly(neighbor_statistics_data_frame) %>%
                plotly::add_trace(y = neighbor_statistics_data_frame[[column]],
                                  type = "box",
                                  name = column_names[[column]]) %>%
                plotly::layout(yaxis = list(side = "left",
                                            zeroline = column != 4L,
                                            range = y_limits),
                               shapes = shape_list)

                subplots[[column - 1L]] <<- plot
            }))

          main_title_font <- list(family = "Arial", size = 18L, color = "black")

          sub_plot <-
            plotly::subplot(subplots,
                            nrows = 1L, margin = 0.05,
                            shareX = FALSE, shareY = FALSE) %>%
            plotly::layout(showlegend = FALSE,
                           title = list(text = main_title,
                                        font = main_title_font),
                           margin = list(t = 150, pad = 1))
          return(sub_plot)
        })
      }
    }

    return(result)
  }



  draw_neighbor_statistics_boxplots <-
  function(dataset_name, dataset_variable,
           neighbor_dataset_name, neighbor_dataset_variable,
           selected_dataset_x_site,
           neighbor_distance,
           neighbor_statistics_data_frame,
           only_unflagged,
           save_to_file) {

    ASNAT_dprint("In draw_neighbor_statistics_boxplots(%s)\n", dataset_name)

    # Don't save interactive plots as PDF files.

    interactive <- use_interactive_plots(model) && !save_to_file

    # Get the datasets:

    the_dataset <- set_dataset_variable(dataset_name, dataset_variable)
    neighbor_dataset <-
      set_dataset_variable(neighbor_dataset_name, neighbor_dataset_variable)

    # Create main title with date range, bbox, etc.

    source <- coverage_source(the_dataset)
    source_variable <- source_variable(the_dataset)
    fancy_source_variable <- source_variable

    if (interactive) {
      fancy_source_variable <-
        paste0(coverage_source(the_dataset), ".",
               fancy_label(variable_name(the_dataset)))
    }

    neighbor_source_variable <- source_variable(neighbor_dataset)
    neighbor_variable <- variable_name(neighbor_dataset)
    fancy_neighbor_source_variable <- neighbor_source_variable

    if (interactive) {
      fancy_neighbor_source_variable <-
        paste0(coverage_source(neighbor_dataset), ".",
               fancy_label(neighbor_variable))
    }

    # Get date-time range:

    first_model_timestamp <- first_timestamp(model)
    date_range <- substr(first_model_timestamp, 1L, 10L)

    if (days(model) > 1L) {
      last_model_timestamp <- last_timestamp(model)
      date_range <-
        paste0(date_range, " - ", substr(last_model_timestamp, 1L, 10L))
    }

    model_timestep_size <- timestep_size(model)
    averaging <- "Hourly"

    if (model_timestep_size == "days") {
      averaging <- "Daily"
    }

    # Get bounds of viewing area:

    west <- west_bound(the_dataset)
    east <- east_bound(the_dataset)
    south <- south_bound(the_dataset)
    north <- north_bound(the_dataset)
    count <- nrow(neighbor_statistics_data_frame)
    unflagged_string <- ifelse(only_unflagged, "Unflagged ", "")

    main_title <-
      sprintf("%d %sNeighbors (<= %dm) Statistics:\n%s %s vs %s\n%s (%0.4f, %0.4f) - (%0.4f, %0.4f)",
              count, unflagged_string, neighbor_distance, averaging,
              ifelse(interactive, fancy_source_variable, source_variable),
              ifelse(interactive, fancy_neighbor_source_variable, neighbor_source_variable),
              date_range, west, east, south, north)

    # If plotting a single site with its neighbors then recreate main title:

    if (selected_dataset_x_site > 0L) {
      the_data_frame <- data_frame(the_dataset)
      longitudes <- the_data_frame[[2L]]
      latitudes <- the_data_frame[[3L]]
      longitude <- longitudes[[1L]]
      latitude <- latitudes[[1L]]

      main_title <-
        sprintf("%s Site %d %sNeighbors (<= %dm) Statistics:\n%s %s vs %s\n%s (%0.4f, %0.4f)",
                source, selected_dataset_x_site, unflagged_string,
                neighbor_distance, averaging,
                ifelse(interactive, fancy_source_variable, source_variable),
                ifelse(interactive, fancy_neighbor_source_variable, neighbor_source_variable),
                date_range, longitude, latitude)
    }

    ASNAT_dprint("Rendering neighbor statistics boxplots\n")

    file_name <- NULL

    if (save_to_file) {
      directory <- output_directory(model)
      first_part <- gsub(fixed = TRUE, ".", "_", source_variable)

      if (selected_dataset_x_site > 0L) {
        first_part <-
          paste0(first_part, "_site_", selected_dataset_x_site, "_neighbors")
      }

      file_name <-
        paste0(directory, "/",
               first_part,
               "_vs_",
               gsub(fixed = TRUE, ".", "_", neighbor_source_variable),
               "_statistics_boxplots",
               ifelse(only_unflagged, "_unflagged.pdf", ".pdf"))

      saved_plot_files <<- append(saved_plot_files, file_name)
    }

    aqi_statistic_target_ranges <-
      ASNAT_aqi_statistic_target_ranges(dataset_variable)

    result <-
      draw_statistics_boxplots(neighbor_statistics_data_frame,
                               aqi_statistic_target_ranges,
                               main_title, interactive, file_name)

    return(result)
  }



  ############################### Draw all plots ##############################



  # Helper to show or hide a plot render output:

  show_plot <- function(render_result, using_interactive_plots,
                        basic_id, interactive_id) {

    if (is.null(render_result)) {
      shinyjs::hide(id = basic_id)
      shinyjs::hide(id = interactive_id)
    } else {

      if (using_interactive_plots) {
        shinyjs::hide(id = basic_id)
        shinyjs::show(id = interactive_id)
      } else {
        shinyjs::hide(id = interactive_id)
        shinyjs::show(id = basic_id)
      }
    }
  }



  plotting <- FALSE

  # Helper function to draw all of the plots:

  draw_plots <- function(save_to_file) {
    ASNAT_dprint("draw_plots(%d) called.\n", as.integer(save_to_file))
    timer <- ASNAT_start_timer()

    ASNAT_dprint("plotting = %d\n", as.integer(plotting))

    have <- have_datasets()
    have_x <- have$x
    have_y <- have$y

    if (plotting || !have_x) {
      return(NULL)
    }

    plotting <<- TRUE
    ASNAT_dprint("  set plotting = %d\n", as.integer(plotting))
    showNotification("Plotting data...",
                     duration = NULL, closeButton = FALSE,
                     id = "message_popup", type = "message")

    using_interactive_plots <- use_interactive_plots(model)

    draw_result1 <- NULL
    draw_result2 <- NULL
    draw_result3 <- NULL
    draw_result4 <- NULL
    draw_result5 <- NULL
    draw_result6 <- NULL
    draw_result7 <- NULL
    draw_result8 <- NULL
    draw_result9 <- NULL
    draw_result10 <- NULL
    draw_result11 <- NULL
    draw_result12 <- NULL

    the_maximum_neighbor_distance <- maximum_neighbor_distance(model)
    model_summary_x_data_frame_ids <- NULL
    model_summary_y_data_frame_ids <- NULL
    model_summary_x_data_frame <- summary_x_data_frame(model)
    model_summary_y_data_frame <- summary_y_data_frame(model)
    model_comparison_data_frame <- comparison_data_frame(model)
    subset_model_comparison_data_frame <- model_comparison_data_frame
    selected_dataset_x_site <- 0L

    if (nrow(model_summary_x_data_frame) > 0L) {

      if (have_y && nchar(input$neighbors_menu[1L]) > 0L) {
        selected_dataset_x_site <-
          as.integer(unlist(strsplit(input$neighbors_menu[1L], " "))[1L])
      }

      if (selected_dataset_x_site > 0L) {
        model_summary_x_data_frame_ids <- selected_dataset_x_site

        if (nrow(model_comparison_data_frame) > 0L &&
            nrow(model_summary_y_data_frame) > 0L &&
            ASNAT_units_match(input$dataset_x_variable_menu[1L],
                              input$dataset_y_variable_menu[1L])) {
          matched_rows <-
            which(model_comparison_data_frame[[2L]] == selected_dataset_x_site)
          subset_model_comparison_data_frame <-
            model_comparison_data_frame[matched_rows, ]
          model_summary_y_data_frame_ids <- subset_model_comparison_data_frame[[4L]]
        }
      } else {
        model_summary_x_data_frame_ids <- model_summary_x_data_frame[[1L]]
      }

      draw_result1 <-
        draw_dataset_boxplot(input$dataset_x_menu[1L],
                             input$dataset_x_variable_menu[1L],
                             model_summary_x_data_frame_ids,
                             input$dataset_y_menu[1L],
                             input$dataset_y_variable_menu[1L],
                             model_summary_y_data_frame_ids,
                             the_maximum_neighbor_distance,
                             save_to_file)

      draw_result2 <-
        draw_dataset_timeseries_plot(input$dataset_x_menu[1L],
                                     input$dataset_x_variable_menu[1L],
                                     model_summary_x_data_frame_ids,
                                     input$dataset_y_menu[1L],
                                     input$dataset_y_variable_menu[1L],
                                     model_summary_y_data_frame_ids,
                                     the_maximum_neighbor_distance,
                                     save_to_file)

      is_aqi <- ASNAT_is_aqi_variable(input$dataset_x_variable_menu[1L])

      if (is_aqi) {
        draw_result11 <-
          draw_aqi_summary_plot(input$dataset_x_menu[1L],
                                input$dataset_x_variable_menu[1L],
                                model_summary_x_data_frame_ids,
                                input$dataset_y_menu[1L],
                                input$dataset_y_variable_menu[1L],
                                model_summary_y_data_frame_ids,
                                the_maximum_neighbor_distance,
                                save_to_file)
      }
    }

    if (have_y) {

      if (selected_dataset_x_site == 0L) {

        if (nrow(model_summary_y_data_frame) > 0L) {
          model_summary_y_data_frame_ids <- model_summary_y_data_frame[[1L]]
        }

        draw_result3 <-
          draw_dataset_boxplot(input$dataset_y_menu[1L],
                               input$dataset_y_variable_menu[1L],
                               model_summary_y_data_frame_ids,
                               NULL, NULL, NULL, 0.0,
                               save_to_file)

        draw_result4 <-
          draw_dataset_timeseries_plot(input$dataset_y_menu[1L],
                                       input$dataset_y_variable_menu[1L],
                                       model_summary_y_data_frame_ids,
                                       NULL, NULL, NULL, 0.0,
                                       save_to_file)

        if (ASNAT_is_aqi_variable(input$dataset_y_variable_menu[1L])) {
          draw_result12 <-
            draw_aqi_summary_plot(input$dataset_y_menu[1L],
                                  input$dataset_y_variable_menu[1L],
                                  model_summary_y_data_frame_ids,
                                  NULL,
                                  NULL,
                                  NULL,
                                  the_maximum_neighbor_distance,
                                  save_to_file)
        }
      }

      # If there is a Dataset Y then draw the scatter plots -
      # either for all pairs of neighboring points or
      # (if selected_data_x_site > 0) for only those Y neigbors of the single
      # selected_data_x_site.

      draw_result5 <-
        draw_scatterplot(input$dataset_x_menu[1L],
                         input$dataset_x_variable_menu[1L],
                         input$dataset_y_menu[1L],
                         input$dataset_y_variable_menu[1L],
                         selected_dataset_x_site,
                         FALSE,
                         save_to_file)

      draw_result6 <-
        draw_scatterplot(input$dataset_x_menu[1L],
                         input$dataset_x_variable_menu[1L],
                         input$dataset_y_menu[1L],
                         input$dataset_y_variable_menu[1L],
                         selected_dataset_x_site,
                         TRUE,
                         save_to_file)

      if (ASNAT_is_aqi_compatible(input$dataset_x_variable_menu[1L],
                                  input$dataset_y_variable_menu[1L])) {
        draw_result7 <-
          draw_aqi_plot(input$dataset_x_menu[1L],
                        input$dataset_x_variable_menu[1L],
                        input$dataset_y_menu[1L],
                        input$dataset_y_variable_menu[1L],
                        selected_dataset_x_site,
                        FALSE,
                        save_to_file)

        draw_result8 <-
          draw_aqi_plot(input$dataset_x_menu[1L],
                        input$dataset_x_variable_menu[1L],
                        input$dataset_y_menu[1L],
                        input$dataset_y_variable_menu[1L],
                        selected_dataset_x_site,
                        TRUE,
                        save_to_file)
      }

      if (nrow(subset_model_comparison_data_frame) > 0L) {
        neighbor_statistics_data_frame <-
          ASNAT_neighbor_statistics(subset_model_comparison_data_frame, FALSE)

        if (!is.null(neighbor_statistics_data_frame) &&
            nrow(neighbor_statistics_data_frame) > 0L) {
          draw_result9 <-
            draw_neighbor_statistics_boxplots(
              input$dataset_x_menu[1L],
              input$dataset_x_variable_menu[1L],
              input$dataset_y_menu[1L],
              input$dataset_y_variable_menu[1L],
              selected_dataset_x_site,
              the_maximum_neighbor_distance,
              neighbor_statistics_data_frame,
              FALSE,
              save_to_file)
        }

        neighbor_statistics_data_frame <-
          ASNAT_neighbor_statistics(subset_model_comparison_data_frame, TRUE)

        if (!is.null(neighbor_statistics_data_frame) &&
            nrow(neighbor_statistics_data_frame) > 0L) {
          draw_result10 <-
            draw_neighbor_statistics_boxplots(
              input$dataset_x_menu[1L],
              input$dataset_x_variable_menu[1L],
              input$dataset_y_menu[1L],
              input$dataset_y_variable_menu[1L],
              selected_dataset_x_site,
              the_maximum_neighbor_distance,
              neighbor_statistics_data_frame,
              TRUE,
              save_to_file)
        }
      }
    }

    if (!save_to_file) {

      # Show or hide each available plot then assign to appropriate output:
      # UGLY: no way to loop/index over outputs?

      show_plot(draw_result1, using_interactive_plots,
                "boxplot_x",
                "interactive_boxplot_x")

      if (!is.null(draw_result1)) {

        if (using_interactive_plots) {
          output$interactive_boxplot_x <- draw_result1
        } else {
          output$boxplot_x <- draw_result1
        }
      }

      show_plot(draw_result2, using_interactive_plots,
                "timeseriesplot_x",
                "interactive_timeseriesplot_x")

      if (!is.null(draw_result2)) {

        if (using_interactive_plots) {
          output$interactive_timeseriesplot_x <- draw_result2
        } else {
          output$timeseriesplot_x <- draw_result2
        }
      }

      show_plot(draw_result3, using_interactive_plots,
                "boxplot_y",
                "interactive_boxplot_y")

      if (!is.null(draw_result3)) {

        if (using_interactive_plots) {
          output$interactive_boxplot_y <- draw_result3
        } else {
          output$boxplot_y <- draw_result3
        }
      }

      show_plot(draw_result4, using_interactive_plots,
                "timeseriesplot_y",
                "interactive_timeseriesplot_y")

      if (!is.null(draw_result4)) {

        if (using_interactive_plots) {
          output$interactive_timeseriesplot_y <- draw_result4
        } else {
          output$timeseriesplot_y <- draw_result4
        }
      }

      show_plot(draw_result5, using_interactive_plots,
                "scatterplot_xy",
                "interactive_scatterplot_xy")

      if (!is.null(draw_result5)) {

        if (using_interactive_plots) {
          output$interactive_scatterplot_xy <- draw_result5
        } else {
          output$scatterplot_xy <- draw_result5
        }
      }

      show_plot(draw_result6, using_interactive_plots,
                "scatterplot_xy_unflagged",
                "interactive_scatterplot_xy_unflagged")

      if (!is.null(draw_result6)) {

        if (using_interactive_plots) {
          output$interactive_scatterplot_xy_unflagged <- draw_result6
        } else {
          output$scatterplot_xy_unflagged <- draw_result6
        }
      }


      show_plot(draw_result7, using_interactive_plots,
                "aqi_plot",
                "interactive_aqi_plot")

      if (!is.null(draw_result7)) {

        if (using_interactive_plots) {
          output$interactive_aqi_plot <- draw_result7
        } else {
          output$aqi_plot <- draw_result7
        }
      }

      show_plot(draw_result8, using_interactive_plots,
                "aqi_plot_unflagged",
                "interactive_aqi_plot_unflagged")

      if (!is.null(draw_result8)) {

        if (using_interactive_plots) {
          output$interactive_aqi_plot_unflagged <- draw_result8
        } else {
          output$aqi_plot_unflagged <- draw_result8
        }
      }

      show_plot(draw_result9, using_interactive_plots,
                "neighbor_statistics_boxplots",
                "interactive_neighbor_statistics_boxplots")

      if (!is.null(draw_result9)) {

        if (using_interactive_plots) {
          output$interactive_neighbor_statistics_boxplots <- draw_result9
        } else {
          output$neighbor_statistics_boxplots <- draw_result9
        }
      }

      show_plot(draw_result10, using_interactive_plots,
                "neighbor_statistics_boxplots_unflagged",
                "interactive_neighbor_statistics_boxplots_unflagged")

      if (!is.null(draw_result10)) {

        if (using_interactive_plots) {
          output$interactive_neighbor_statistics_boxplots_unflagged <-
            draw_result10
        } else {
          output$neighbor_statistics_boxplots_unflagged <- draw_result10
        }
      }

      show_plot(draw_result11, using_interactive_plots,
                "aqi_plot_x",
                "interactive_aqi_plot_x")

      if (!is.null(draw_result11)) {

        if (using_interactive_plots) {
          output$interactive_aqi_plot_x <- draw_result11
        } else {
          output$aqi_plot_x <- draw_result11
        }
      }

      show_plot(draw_result12, using_interactive_plots,
                "aqi_plot_y",
                "interactive_aqi_plot_y")

      if (!is.null(draw_result12)) {

        if (using_interactive_plots) {
          output$interactive_aqi_plot_y <- draw_result12
        } else {
          output$aqi_plot_y <- draw_result12
        }
      }
    }

    removeNotification("message_popup")
    plotting <<- FALSE
    ASNAT_dprint("  reset plotting = %d\n", as.integer(plotting))
    ASNAT_elapsed_timer("draw_plots:", timer)
  }



  # Callback for make_plots button:

  observeEvent(input$make_plots, {

    # Ensure that summary and comparison data is available:

    the_summary_x_data_frame <- summary_x_data_frame(model)
    the_summary_y_data_frame <- summary_y_data_frame(model)

    recompute <-
      nrow(the_summary_x_data_frame) == 0L ||
      (input$dataset_y_menu[[1L]] != "none" &&
       nrow(the_summary_y_data_frame) == 0L) ||
      (input$dataset_y_menu[[1L]] == "none" &&
       nrow(the_summary_y_data_frame) > 0L)

    if (recompute) {
      summarize_and_compare_datasets_x_y()
    }

    draw_plots(FALSE)
  })



 # Get selected Dataset X site number (or 0):

  get_selected_dataset_x_site <- function() {
    result <- 0L

    is_valid_input <-
      shiny::isTruthy(input$neighbors_menu) &&
      !is.na(input$neighbors_menu) &&
      nchar(input$neighbors_menu) > 0L

    if (is_valid_input) {
      selection <- input$neighbors_menu
      parts <- unlist(strsplit(selection, " "))
      result <- as.integer(parts[[1L]])
    }

    return(result)
  }



  # Save all plots as pdf files and site neighbor map as a png file.
  # Returns a vector of names of saved files.

  save_plot_files <- function() {
    ASNAT_dprint("In save_plot_files()\n")
    timer <- ASNAT_start_timer()

    saved_plot_files <<- vector()

    if (dataset_count(model) > 0L) {
      check_set_reset_output_directory()
      ASNAT_dprint("save_plot_files() calling draw_plots(TRUE)\n")
      draw_plots(TRUE)
      ASNAT_dprint("done\n")

      # Also save site_neighbor_map if plotting site neighbors:

      selected_site <- get_selected_dataset_x_site()

      if (selected_site > 0L) {
        map_file_name <-
          paste0(output_directory(model),
                 sprintf("/map_image_site_%d.png", selected_site))
        map_file_name <-
          create_map_image_file(the_site_neighbor_map,
                                the_site_neighbor_map_width,
                                the_site_neighbor_map_height,
                                map_file_name)

        if (file.exists(map_file_name)) {
          saved_plot_files <<- append(saved_plot_files, map_file_name)
        }
      }

      output$message_area <- renderPrint({cat(saved_plot_files, sep = "\n")})
    }

    ASNAT_elapsed_timer("save_plot_files:", timer)
    ASNAT_dprint("save_plot_files() returning\n")
    return(saved_plot_files)
  }



  # Callback for Save Plots button - when not remote-hosted:

  observeEvent(input$save_plots, {
    save_plot_files()
  })



  # Callback for Download Plots button - when remote-hosted:

  output$download_plots <- downloadHandler(
    filename = "ASNAT_plots.zip",
    content = function(file) {
      #cat("In download_plots downloadHandler() content function\n")
      file_names <- save_plot_files()
      #cat("In downloadHandler content function, file_names:"); str(file_names)

      if (length(file_names) > 0L) {
        # cat(sep = "", "file_names[[1L]] = '", file_names[[1L]], "' exists = ",
        #    file.exists(file_names[[1L]]), "\n")
        #utils::zip(file, files = file_names, flags = "-j")
        zip::zip(file, files = file_names,
                 recurse = FALSE, include_directories = FALSE,
                 mode = "cherry-pick")
      }
    },
    contentType = "application/zip"
  )



  #############################################################################

  # A reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)



  # Reactive value to store regression models
  regression_models <- reactiveVal(data.frame(
    Name = c("Linear Model", "Polynomial Model", "Exponential Model"),
    Equation = c("y=ax+b", "y=ax^2+bx+c", "y=ae^(bx)")
  ))



  # Reactive value to store the currently selected model for editing
  selected_model <- reactiveVal(NULL)

  # Reactive values to store selected variables
  selected_vars <- reactiveValues(
    dependent = NULL,
    independent = c()
  )



  # Reactive expression for the selected device's data
  selected_device_data <- reactive({
    req(input$selected_device_id)
    req(correction_dictionary(model)[[input$selected_device_id]])
    correction_dictionary(model)[[input$selected_device_id]]
  })



  # Reactive value for output
  equations_display_df <- reactiveVal(NULL)

  # Reactive value to store the filtered comparison_r2_df
  filtered_comparison_df <- reactiveVal(NULL)



  # Reactive value to track the selected device(id) for generate plots
  selected_device <- reactive({
    input$selected_device_id
  })



  # Reactive value to track the selected device for moving average
  selected_ma_device <- reactive({
    input$selected_ma_device_id
  })



  # last_clicked <- reactiveVal(NULL)

  # Reactive value that stores the selected rows for the text display in data correction
  selected_pairs <- reactive({
    selected_rows <- input$selectable_correction_table_rows_selected
    num_selected <- length(selected_rows)  # Count the number of selected rows

    if (length(selected_rows) > 0) {
      comparison_r2_df <- filtered_comparison_df()
      x_col_name <- names(comparison_r2_df)[1]
      y_col_name <- names(comparison_r2_df)[6]
      selected_pairs <- paste(
        x_col_name, ":", comparison_r2_df[selected_rows, 1],
        "| ", y_col_name, ":", comparison_r2_df[selected_rows, 6],
        "| R-squared:", round(comparison_r2_df[selected_rows, "R2(-)"], 3)
      )

      # Add the number of selected rows to the display
      selected_pairs <- c(
        paste("Number of points selected:", num_selected),
        selected_pairs
      )
    } else {
      selected_pairs <- "No points selected"
    }

    return(paste(selected_pairs, collapse = "\n"))
  })



  # Observer for the delete_data button
  observeEvent(input$delete_data, {
    data_loaded(FALSE)
  })



  # Observer to control/grey out retrieve_data button
  observe({

    if (data_loaded()) {
      shinyjs::disable("retrieve_data")
      # shinyjs::disable("load_data_from_standard_file")
      # shinyjs::disable("show_import_fileset")
    } else {
      shinyjs::enable("retrieve_data")
      # shinyjs::enable("load_data_from_standard_file")
      # shinyjs::enable("show_import_fileset")
    }
  })



  # Display area for the selected devices in data correction
  output$selected_ids_display <- renderText({
    selected_pairs()
  })



  # Display area for hampel filter
  # display the average records per device
  output$avg_records_display <- renderText({

    have <- have_datasets()
    have_x <- have$x
    have_y <- have$y
    avg_records <- "N/A"

    if (!have_x && !have_y) {
      return(avg_records)
    }

    dataset_flag_index <- get_flag_dataset_index()
    the_dataset <- dataset(model, dataset_flag_index)


    if (dataset_flag_index > 0) {
      data_frame <- data_frame(the_dataset)
      total_records <- nrow(data_frame)
      unique_ids <- length(unique(data_frame[["id(-)"]]))
      avg_records <- round(total_records / unique_ids, 2)

    }

    return(paste(avg_records, "records"))
  })



  # Function to validate custom equation input
  validate_equation <- function(equation) {
    # Trim whitespace from the equation
    equation <- trimws(equation)

    # 1. Check if the equation is not empty
    if (nchar(equation) == 0) {
      return(FALSE)
    }

    # 2. Check if there is exactly one "=" in the equation
    if (sum(strsplit(equation, "")[[1]] == "=") != 1) {
      return(FALSE)
    }

    # 3. Check if there are non-empty parts on both sides of "="
    parts <- strsplit(equation, "=")[[1]]

    if (length(parts) != 2 || nchar(trimws(parts[1])) == 0 || nchar(trimws(parts[2])) == 0) {
      return(FALSE)
    }

    return(TRUE)
  }



  # Helper function to get all loaded datasets
  get_all_datasets <- function(model) {
    dataset_count <- dataset_count(model)
    all_datasets <- list()

    for (i in 1:dataset_count) {
      dataset <- dataset(model, i)
      all_datasets[[coverage(dataset)]] <- dataset
    }

    return(all_datasets)
  }



  parse_equation <- function(dependent, independent) {
    dependent <- trimws(dependent)
    independent <- trimws(independent)

    if (nchar(dependent) == 0 || nchar(independent) == 0) {
      return(list(error = "Both dependent and independent variables must be specified"))
    }

    dependent_vars <- unlist(strsplit(dependent, "\\s+"))
    independent_vars <- unlist(strsplit(independent, "\\s+"))

    if (length(dependent_vars) > 1) {
      return(list(error = "Only one dependent variable is allowed"))
    }

    return(list(
      dependent = dependent_vars[1],
      predictors = independent_vars,
      num_variables = length(independent_vars)
    ))
  }



  ####################### moving average #####################################


  # Reactive value to store moving average results
  moving_average_results <- reactiveVal(NULL)

  # Get records with flag 83 (constant value flag)
  get_flagged_records <- function() {
    have <- have_datasets()

    dataset <- NULL
    dataset_variable <- NULL
    dataset_name <- NULL

    if (have$y) {
      dataset_variable <- input$dataset_y_variable_menu[1L]
      dataset_name <- input$dataset_y_menu[1L]
      dataset <- find_dataset(model, dataset_name)
    } else if (have$x) {
      dataset_variable <- input$dataset_x_variable_menu[1L]
      dataset_name <- input$dataset_x_menu[1L]
      dataset <- find_dataset(model, dataset_name)
    }

    if (is.null(dataset) || is.null(dataset_variable)) {
      return(NULL)
    }

    df <- data_frame(dataset)
    # Get rows where flagged column contains 83
    flagged_rows <- grepl("83", df$flagged)
    flagged_data <- list()
    # dataset name
    flagged_data$dataset_name <- dataset_name
    # flagged dataframe
    flagged_data$dataframe <- df[flagged_rows, ]
    # unique flagged ids
    flagged_data$flagged_ids <- c(unique(flagged_data$dataframe[["id(-)"]]))
    # full dataset
    flagged_data$df <- df
    # selected variable
    flagged_data$dataset_variable <- dataset_variable

    if (any(flagged_rows)) {
      return(flagged_data)
    } else {
      return(NULL)
    }
  }



  # Callback for move_average
  observeEvent(input$move_average, {

    # Get flagged data
    flagged_data <- get_flagged_records()

    if (!is.null(flagged_data)) {

      shinyjs::hide("data_correction_regression_layout")
      shinyjs::show("data_moving_average_layout")
      # Proxy for the flagged records table
      flagged_proxy <- DT::dataTableProxy("flagged_records_table")

      output$flagged_records_table <- DT::renderDataTable({
        flagged_data <- get_flagged_records()

        if (is.null(flagged_data)) {
          showNotification("No data flagged with constant value (83)",
                          type = "warning",
                          duration = 5)

        }

        DT::datatable(
          flagged_data$dataframe,
          caption = "Records Flagged with Constant Value (83)",
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          selection = "multiple"
        )
      })

      # Observers for select/unselect buttons
      observeEvent(input$select_all_flagged, {
        flagged_data <- get_flagged_records()

        if (!is.null(flagged_data)) {
          DT::selectRows(flagged_proxy, seq_len(nrow(flagged_data$dataframe)))
        }
      })

      # Observer for unselect all entries
      observeEvent(input$unselect_all_flagged, {
        DT::selectRows(flagged_proxy, NULL)
      })

      # Output for selected records display
      output$selected_flagged_display <- renderText({
        selected_rows <- input$flagged_records_table_rows_selected
        flagged_data <- get_flagged_records()

        if (length(selected_rows) > 0 && !is.null(flagged_data)) {
          selected_data <- flagged_data$dataframe[selected_rows, ]
          num_selected <- length(selected_rows)

          # Display text
          display_text <- paste0("Number of records selected: ", num_selected, "\n\n")
          display_text <- paste0(display_text, "Selected Devices:\n")
          unique_ids <- unique(selected_data[["id(-)"]])

          for (id in unique_ids) {
            record_count <- nrow(selected_data[selected_data[["id(-)"]] == id, ])
            display_text <- paste0(display_text,
                                   sprintf("Device: %s (%d records)\n", id, record_count))
          }

          return(display_text)
        } else {
          return("No records selected")
        }
      })



      # Modify the apply moving average observer to only process selected rows
      observeEvent(input$apply_moving_average, {
        selected_rows <- input$flagged_records_table_rows_selected

        if (length(selected_rows) == 0) {
          showNotification("Please select records to apply moving average.",
                          type = "warning")
          return()
        }

        flagged_data <- get_flagged_records()
        selected_data <- flagged_data$dataframe[selected_rows, ]
        selected_variable <- flagged_data$dataset_variable
        dataset_name <- flagged_data$dataset_name
        selected_ids <- unique(selected_data[["id(-)"]])
        # Get window size in hours
        window_size <- input$window_size
        # Get the original dataset
        original_df <- flagged_data$df
        # Create a new dataframe for corrected values
        corrected_df <- original_df
        # Initialize list to store results for each device
        results <- list()

        for (device_id in selected_ids) {
          # Get all data for this device
          device_data <- original_df[original_df[["id(-)"]] == device_id, ]

          if (length(device_data) == 0) {
            showNotification(paste("No data found for Device ID:", device_id), type = "error")
            next
          }

          window_size <- min(window_size, nrow(device_data))
          ma_values <- rollmean(device_data[[selected_variable]],
                      k = window_size,
                      fill = "extend",
                      align = "center")
          # # Find rows with flag 83 for this device
          # flagged_rows <- grep("83", device_data[["flagged(-)"]])

          names(ma_values) <- rownames(device_data)


          selected_device_data <- selected_data[selected_data[["id(-)"]] == device_id, ]
          rows_to_update <- as.numeric(rownames(selected_device_data))
          ma_values_to_use <- ma_values[as.character(rows_to_update)]

          # Update only the selected flagged rows in corrected_df
          # Loop through the named vector ma_values_to_use
          for (row_name in names(ma_values_to_use)) {
            row_index <- as.numeric(row_name)  # Convert name to numeric index
            value <- ma_values_to_use[row_name]  # Get the value
            corrected_df[row_index, selected_variable] <- value
          }

          # corrected_df[rows_to_update, selected_variable] <- ma_values_to_use


          results[[device_id]] <- list(
            original_values = device_data,
            corrected_values = corrected_df[corrected_df[["id(-)"]] == device_id, ]
          )
        }

        # Store the results in the reactive value
        moving_average_results(results)

        # Update the device ID dropdown choices
        updateSelectInput(session, "selected_ma_device_id",
                          choices = selected_ids,
                          selected = selected_ids[1])


      })
    } else {
      showNotification("No data flagged with constant value (83)",
                       type = "warning",
                       duration = 5)
    }
  })




  # Render the interactive scatter plot for moving average correction
  output$ma_correction_plot <- renderPlotly({

    # Ensure a device is selected before proceeding
    req(selected_ma_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_ma_device())

    table <- moving_average_results()

    corrected <- table[[device_id]]$corrected_values
    original <- table[[device_id]]$original_values
    flagged_data <- get_flagged_records()
    selected_variable <- flagged_data$dataset_variable
    has_flag_83 <- grepl("83", corrected[["flagged(-)"]])

    # Data frame for plotting
    df <- data.frame(
      timestamp = as.POSIXct(original[["timestamp(UTC)"]],
                            format = "%Y-%m-%dT%H:%M:%S%z",
                            tz = "UTC"),
      original = original[[selected_variable]],
      corrected = corrected[[selected_variable]],
      is_flagged = has_flag_83
    )

    # Create the interactive plot
    plot <- plot_ly(df, x = ~timestamp) %>%
      # Add original values line
      add_trace(
        y = ~original,
        name = "Original Values",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "blue", width = 1),
        marker = list(size = 4)
      ) %>%
      # Add corrected values line
      add_trace(
        y = ~corrected,
        name = "Corrected Values",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "red", width = 1),
        marker = list(size = 4)
      ) %>%
      # Add flagged points as circles
      add_trace(
        data = df[df$is_flagged, ],
        x = ~timestamp,
        y = ~original,
        name = "Flagged Points (83)",
        type = "scatter",
        mode = "markers",
        marker = list(
          symbol = "circle-open",
          size = 8,
          color = "orange",
          line = list(color = "black", width = 1)
        ),
        showlegend = TRUE
      ) %>%
      layout(
        title = paste("Original vs Corrected Values\n",
              "Dataset:", flagged_data$dataset_name, "\n",
              "Variable:", selected_variable, "\n",
              "Device ID:", device_id),
        xaxis = list(title = "Timestamp"),
        yaxis = list(title = selected_variable),
        hovermode = "compare",
        showlegend = TRUE,
        legend = list(x = 0.1, y = 0.9)
      )

    # Add range selector and buttons
    plot %>% layout(
      xaxis = list(
        rangeselector = list(
          buttons = list(
            list(count = 1, label = "1d", step = "day", stepmode = "backward"),
            list(count = 7, label = "1w", step = "day", stepmode = "backward"),
            list(count = 1, label = "1m", step = "month", stepmode = "backward"),
            list(step = "all")
          )
        ),
        rangeslider = list(type = "date")
      )
    )
  })



  # Update table with corrected data
  output$ma_correction_table <- renderTable({
    # Ensure a device is selected before proceeding
    req(selected_ma_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_ma_device())

    table <- moving_average_results()

    return(table[[device_id]]$corrected_values)
  })



  # Update table with corrected data
  output$ma_original_table <- renderTable({
    # Ensure a device is selected before proceeding
    req(selected_ma_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_ma_device())

    table <- moving_average_results()

    return(table[[device_id]]$original_values)
  })



  ####################### data correction and regression ######################

  # Callback for Data Correction button:
  observeEvent(input$correction, {

    shinyjs::show("data_correction_regression_layout")
    shinyjs::hide("data_moving_average_layout")
    have <- have_datasets()
    have_x <- have$x
    have_y <- have$y

    if (!have_x || !have_y) {
      showNotification("Please select X and Y variables to apply.", type = "warning")
      return(NULL)
    }

    # Summarize and compare datasets
    summarize_and_compare_datasets_x_y()

    max_distance <- maximum_neighbor_distance(model)
    # Get the comparison_r2_data_frame from the model
    comparison_r2_df <- comparison_r2_data_frame(model)
    summary_y <- summary_y_data_frame(model)

    # Hide existing tables and plots
    if (nrow(comparison_r2_df) == 0) {
      shinyjs::hide("data_correction_regression_layout")
      return(NULL)
    } else {
      # Hide existing tables and plots
      shinyjs::show("data_correction_regression_layout")
    }

    # Get the station pairs from summary_y
    station_pairs <- summary_y[, c("Site_Id(-)", "Nearest_Y_Site(-)")]

    # Create a logical vector for matching pairs
    matched_pairs <- sapply(1:nrow(comparison_r2_df), function(i) {
      x_id <- comparison_r2_df[i, 1]  # Using column index 4 instead of name
      y_id <- comparison_r2_df[i, 4]  # Using column index 1 instead of name

      # Check if this pair exists in either direction in summary_y
      any((station_pairs[["Site_Id(-)"]] == y_id &
          station_pairs[["Nearest_Y_Site(-)"]] == x_id))
    })

    # Filter comparison_r2_df to keep only matched pairs
    comparison_r2_df <- comparison_r2_df[matched_pairs, ]

    # After filtering matched pairs, check if the filtered dataframe is empty
    if (nrow(comparison_r2_df) == 0) {
      showNotification("No matching pairs found within the distance.", type = "warning")
    }

    # Reorder columns to swap AQS and PurpleAir related columns
    comparison_r2_df <- comparison_r2_df[, c(
      names(comparison_r2_df)[4],
      names(comparison_r2_df)[5],
      names(comparison_r2_df)[6],
      names(comparison_r2_df)[8],
      names(comparison_r2_df)[7],
      names(comparison_r2_df)[1],
      names(comparison_r2_df)[2],
      names(comparison_r2_df)[3]
    )]
    filtered_comparison_df(comparison_r2_df)

    # Render the selectable correction table
    output$selectable_correction_table <- DT::renderDataTable({
      DT::datatable(comparison_r2_df,
                    caption = paste("Neighboring Points ( <=", max_distance, "m)"),
                    options = list(pageLength = 10),
                    selection = "multiple")
    })

    # Render the regression models table
    # This table will dynamically update when models are added or removed
    output$regression_models_table <- DT::renderDataTable({
      DT::datatable(regression_models(),
                    caption = "Correction Models",
                    options = list(pageLength = 5),
                    selection = "single") %>%
        DT::formatStyle(columns = c("Name", "Equation"),
                        cursor = "pointer")
    })

  })



  # Callback for Apply Regression button:
  # Apply the selected regression to correct the data
  observeEvent(input$apply_regression, {
    selected_rows <- 0

    # Check if the euqation is selected
    if (!is.null(input$equations_list_rows_selected)) {
      selected_rows <- input$equations_list_rows_selected
    } else {
      showNotification("No equation selection is selected. ", type = "error")
      return()
    }

    if (length(selected_rows) == 0) {
      showNotification("Please select at least one equation to apply.", type = "warning")
      return()
    }

    eq_df <- equations_display_df()
    #  Example of eq_df
    #  Device_ID   Equation               R_Squared
    #  107228      y = 10.0837 + 0.1538x   1

    # 3. Extract selected equations
    selected_equations <- eq_df[selected_rows, ]

    # 4. Extract Device_IDs from the selected rows
    device_ids <- selected_equations$Device_ID

    # 6. Notify the user that regression has been applied
    showNotification("Regression applied to selected equations successfully!", type = "message")

    # 7. Prepare data for the scatter plot
    main_title <- paste0(comparison_r2_title(model), "\n", comparison_r2_subtitle(model))
    has_flagged_points <- FALSE
    point_colors <- "blue"

    # 8. Initialize vectors to accumulate plot data
    corrected_data <- correction_dictionary(model)
      # example of corrected_data ( by ids )
      # corrected_data <- list(
      #   filtered_data_y = filtered_data_y,
      #   # calculated_values = round(coefficients[1] + coefficients[2] * filtered_data_y[[independent_vars]], 4),
      #   calculated_values = round((filtered_data_y[[dependent_var]] - coefficients[1]) / coefficients[2], 4),
      #   dependent_var = dependent_var,
      #   independent_vars = independent_vars
      # )

    if (length(corrected_data) == 0) {
      cat("Correction dictionary is empty.\n")
    }

    # Initialize data frames to store plot data for each device_id
    plot_data_corrected <- data.frame()
    plot_data_original <- data.frame()

    # 9. Loop through each selected device_id to extract data
    for (device_id in device_ids) {

      # Retrieve corrected_data for the device_id
      corrected_entry <- corrected_data[device_id][[1]]

      # example of filtered_data_y
      # timestamp(UTC)
      # AQS.id(-)
      # AQS.pm25(ug/m3)
      # PurpleAir.id(-)
      # PurpleAir.pm25_corrected_hourly(ug/m3)
      # PurpleAir.flagged(-)
      # 814
      # 2022-06-01T03:00:00-0000
      # 420030002
      # 17
      # 107228
      # 12.69877
      # 0

      if (is.null(corrected_entry)) {
        showNotification(paste("No corrected data found for Device ID:", device_id), type = "error")
        next
      }

      # Extract filtered_data_y and calculated_values
      filtered_data_y <- corrected_entry$filtered_data_y
      calculated_values <- corrected_entry$calculated_values

      dependent_var_name <- corrected_entry$dependent_var
      independent_var_names <- corrected_entry$independent_vars

      if (length(independent_var_names) != 1) {
        showNotification(paste("Only one independent variable is supported. Device ID:", device_id, "Independent Variables:", corrected_entry$independent_vars), type = "error")
        next
      }

      independent_var_name <- independent_var_names[1]

      # Check if columns exist
      if (!dependent_var_name %in% names(filtered_data_y)) {
        showNotification(paste("Dependent variable", dependent_var_name, "not found for Device ID:", device_id), type = "error")
        next
      }

      # x for AQS, and y for PurpleAir
      # Extract y_values and x_values
      y_values <- calculated_values
      x_values <- filtered_data_y[[independent_var_name]]
      y_original <- filtered_data_y[[dependent_var_name]]

      # Check if there is data to plot
      if (length(y_values) == 0 || length(x_values) == 0) {
        showNotification("No data available to plot.", type = "error")
        return()
      }

      # Validate that lengths match
      if (length(x_values) != length(y_values)) {
        showNotification(paste("Mismatch in lengths of x and y values for Device ID:", device_id), type = "error")
        next
      }

      # Accumulate data for plotting
      plot_data_corrected <- rbind(plot_data_corrected, data.frame(
        device_id = device_id,
        x = x_values,
        y = y_values
      ))

      plot_data_original <- rbind(plot_data_original, data.frame(
        device_id = device_id,
        x = x_values,
        y = y_original
      ))

    }

    if (length(plot_data_corrected) == 0 || length(plot_data_original) == 0) {
      showNotification("No valid data available to plot.", type = "warning")
      return()
    }

    x_label <- independent_var_name
    y_label <- dependent_var_name

    # 10. Generate the interactive scatter plot
    scatter_plot_corrected_temp <- make_static_scatterplot(
        plot_data = plot_data_corrected,
        main_title = main_title,
        x_label = x_label,
        y_label = y_label,
        has_flagged_points = has_flagged_points,
        point_colors = point_colors,
        point_size = input$datapoint_size_select
    )

    scatter_plot_original_temp <- make_static_scatterplot(
      plot_data = plot_data_original,
      main_title = main_title,
      x_label = x_label,
      y_label = y_label,
      has_flagged_points = has_flagged_points,
      point_colors = point_colors,
      point_size = input$datapoint_size_select
    )

    # 11. Render the plot in original_scatter_plot
    output$corrected_scatter_plot_temp <- renderPlot({
        scatter_plot_corrected_temp
    })
    output$original_scatter_plot_temp <- renderPlot({
        scatter_plot_original_temp
    })


    # Update the device ID dropdown choices
    updateSelectInput(session, "selected_device_id",
                      choices = device_ids,
                      selected = device_ids[1])

  })

  # Render the interactive scatter plot for corrected data
  output$interactive_correction_scatter_plot <- renderPlotly({
    # Ensure a device is selected before proceeding
    req(selected_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_device())

    # Retrieve the data for the selected device from the correction dictionary
    device_data <- correction_dictionary(model)[[device_id]]


    # Extract x values (original data) and y values (corrected data)
    x_values <- device_data$filtered_data_y[[device_data$independent_vars]]
    y_values <- device_data$calculated_values

    lm_model <- lm(y_values ~ x_values)
    coefficients <- coef(lm_model)

    # Generate equation string
    equation <- sprintf("y = %.8f + %.8fx", coefficients[1], coefficients[2])


    # Generate and return the interactive scatter plot
    make_interactive_scatterplot(
      x_values = x_values,
      y_values = y_values,
      main_title = paste("Corrected Data for Device", device_id, "\n", equation),
      x_label = device_data$independent_vars,
      y_label = device_data$dependent_var,
      has_flagged_points = FALSE,
      point_colors = "blue",
      point_size = as.integer(input$datapoint_size_select) + 2L
    )
  })



  # Render the interactive scatter plot for original data
  output$interactive_original_scatter_plot <- renderPlotly({
    # Ensure a device is selected before proceeding
    req(selected_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_device())

    # Retrieve the data for the selected device from the correction dictionary
    device_data <- correction_dictionary(model)[[device_id]]

    # Extract x and y values from the device data
    x_values <- device_data$filtered_data_y[[device_data$independent_vars]]
    y_values <- device_data$filtered_data_y[[device_data$dependent_var]]

    lm_model <- lm(y_values ~ x_values)
    coefficients <- coef(lm_model)

    # Generate equation string
    equation <- sprintf("y = %.4f + %.4fx", coefficients[1], coefficients[2])


    # Generate and return the interactive scatter plot
    make_interactive_scatterplot(
      x_values = x_values,
      y_values = y_values,
      main_title = paste("Original Data for Device", device_id, "\n", equation),
      x_label = device_data$independent_vars,
      y_label = device_data$dependent_var,
      has_flagged_points = FALSE,
      point_colors = "blue",
      point_size = as.integer(input$datapoint_size_select) + 2L
    )
  })



  # Update table6 with corrected data
  output$table6 <- renderTable({
    # Ensure a device is selected before proceeding
    req(selected_device())

    # Convert the selected device ID to an integer
    device_id <- as.integer(selected_device())

    # Retrieve the data for the selected device from the correction dictionary
    device_data <- correction_dictionary(model)[[device_id]]
    corrected_column_name <- paste0(device_data$dependent_var, "_corrected")

    corrected_df <- device_data$filtered_data_y
    corrected_df[[corrected_column_name]] <- device_data$calculated_values
    # Extract x values (original data) and y values (corrected data)
    corrected_df
  })



  proxy <- DT::dataTableProxy("selectable_correction_table")
  equations_proxy <- DT::dataTableProxy("equations_list")



  # Observe the "Select All" button
  # Select all/unselect all functionality
  observeEvent(input$select_all, {
    # Select all rows
    DT::selectRows(proxy, seq_len(nrow(comparison_r2_data_frame(model))))
  })



  # Observe the "Unselect All" button
  observeEvent(input$unselect_all, {
    # Unselect all rows
    DT::selectRows(proxy, NULL)
  })



  # Observe the "Select All" button for equations_list
  observeEvent(input$select_all_equations, {
    # Retrieve the current equations data frame
    equations_df <- equations_display_df()

    # Check if equations_df is not NULL and has rows
    if (!is.null(equations_df) && nrow(equations_df) > 0) {
      # Select all rows
      DT::selectRows(equations_proxy, seq_len(nrow(equations_df)))
    } else {
      # Optionally, you can show a notification if there's nothing to select
      showNotification("No equations available to select.", type = "warning")
    }
  })



  # Observe the "Unselect All" button for equations_list
  observeEvent(input$unselect_all_equations, {
    # Unselect all rows
    DT::selectRows(equations_proxy, NULL)
  })



  observeEvent(input$clear_model_inputs, {
    updateTextInput(session, "custom_model_name", value = "")
    updateTextInput(session, "custom_model", value = "")
    selected_vars$dependent <- NULL
    selected_vars$independent <- c()
  })


  # -----------------------------------------------
  # Functions for equation creation
  # -----------------------------------------------

  # commented out for future implementation

  # # Modify the loaded_variables UI render function
  # output$loaded_variables <- renderUI({
  #   all_datasets <- get_all_datasets(model)

  #   tagList(
  #     div(
  #       style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;",
  #       lapply(names(all_datasets), function(dataset_name) {
  #         variable_names <- colnames(data_frame(all_datasets[[dataset_name]]))
  #         variable_names <- variable_names[!grepl("id\\(-\\)|flagged\\(-\\)|note\\(-\\)|count\\(-\\)", variable_names)]

  #         tagList(
  #           h5(dataset_name, style = "color: #2c3e50; margin-top: 10px;"),
  #           div(
  #             style = "display: flex; flex-wrap: wrap;",
  #             lapply(variable_names, function(var) {
  #               actionButton(
  #                 inputId = paste0("var_", make.names(paste(dataset_name, var, sep = "_"))),
  #                 label = var,
  #                 style = "margin: 5px; padding: 5px 10px; background-color: #3498db; color: white; border-radius: 5px; cursor: pointer;",
  #                 onclick = sprintf("Shiny.setInputValue('loaded_variables_buttons', '%s.%s', {priority: 'event'});", dataset_name, var)
  #               )
  #             })
  #           )
  #         )
  #       })
  #     )
  #   )
  # })



  # output$dependent_variable_selector <- renderUI({
  #   tagList(
  #     h4("Dependent Variable"),
  #     tags$div(
  #       style = "border: 1px solid #ddd; padding: 5px; margin-bottom: 10px;",
  #       if (!is.null(selected_vars$dependent)) {
  #         actionButton(
  #           inputId = "remove_dependent",
  #           label = selected_vars$dependent,
  #           style = "background-color: #3498db; color: white; padding: 2px 5px; margin: 2px; border: none; border-radius: 3px;"
  #         )
  #       } else {
  #         "No variable selected"
  #       }
  #     )
  #   )
  # })

  # output$independent_variables_selector <- renderUI({
  #   tagList(
  #     h4("Independent Variables"),
  #     tags$div(
  #       style = "border: 1px solid #ddd; padding: 5px; margin-bottom: 10px;",
  #       if (length(selected_vars$independent) > 0) {
  #         lapply(selected_vars$independent, function(var) {
  #           actionButton(
  #             inputId = paste0("remove_independent_", make.names(var)),
  #             label = var,
  #             style = "background-color: #3498db; color: white; padding: 2px 5px; margin: 2px; border: none; border-radius: 3px;"
  #           )
  #         })
  #       } else {
  #         "No variables selected"
  #       }
  #     )
  #   )
  # })

  # # Additional input field for custom regression model
  # output$custom_model_input <- renderUI({
  #   tagList(
  #     # Using a div to wrap the label and the icon together
  #     div(
  #       style = "display: flex; align-items: center;",  # Align items horizontally and vertically
  #       span("Enter Custom Equation (optional):", style = "margin-right: 5px;"),  # Label with some spacing
  #       actionLink("info_custom_model", icon("info-circle"))  # Info icon next to the label
  #     ),
  #     textInput("custom_model",
  #               NULL,  # Set the label to NULL because we're handling the label manually
  #               placeholder = "y = ax + b"),
  #     bsPopover(id = "info_custom_model", title = "Custom Equation Format",
  #               content = "Please follow the format: y = ax + b, where y is the dependent variable and x is the independent variable.",
  #               placement = "right", trigger = "hover")
  #   )
  # })



  # # Modify the observer for variable selection
  # observeEvent(input$loaded_variables_buttons, {
  #   clicked_button <- input$loaded_variables_buttons
  #   selected_type <- input$variable_type_switch

  #   if (selected_type == "dependent") {
  #     selected_vars$dependent <- clicked_button
  #   } else {
  #     selected_vars$independent <- unique(c(selected_vars$independent, clicked_button))
  #   }

  #   # Update the last clicked button
  #   last_clicked(clicked_button)
  # })

  # observeEvent(input$remove_dependent, {
  #   selected_vars$dependent <- NULL
  # })


  # observe({
  #   # Get all input names
  #   input_names <- names(input)

  #   # Filter for remove_independent buttons
  #   remove_buttons <- input_names[grepl("^remove_independent_", input_names)]

  #   # For each remove button
  #   for (button in remove_buttons) {
  #     observeEvent(input[[button]], {
  #       var_to_remove <- sub("remove_independent_", "", button)

  #       # Find the matching variable name (ignoring special characters)
  #       var_to_remove_pattern <- gsub("[^[:alnum:]]", ".", var_to_remove)
  #       index_to_remove <- grep(var_to_remove_pattern, selected_vars$independent)

  #       if (length(index_to_remove) > 0) {
  #         selected_vars$independent <- selected_vars$independent[-index_to_remove]
  #       }
  #     })
  #   }
  # })


  # # Observer for table row selection
  # observeEvent(input$regression_models_table_rows_selected, {
  #   selected_row <- input$regression_models_table_rows_selected
  #   if (length(selected_row) > 0) {
  #     model <- regression_models()[selected_row, ]
  #     selected_model(selected_row)
  #     updateTextInput(session, "custom_model_name", value = model$Name)
  #     updateTextInput(session, "custom_model", value = model$Equation)
  #   }
  # })

  # # Observer for the "Add Model" button
  # observeEvent(input$add_model, {
  #   name <- input$custom_model_name
  #   equation <- input$custom_model

  #   if (nchar(name) > 0 && nchar(equation) > 0) {
  #     if (validate_equation(equation)) {
  #       new_model <- data.frame(
  #         Name = name,
  #         Equation = equation
  #       )
  #       regression_models(rbind(regression_models(), new_model))
  #       updateTextInput(session, "custom_model_name", value = "")
  #       updateTextInput(session, "custom_model", value = "")
  #       selected_model(NULL)
  #     } else {
  #       showNotification("Invalid equation format. It should start with 'y=' and contain 'x'.", type = "error")
  #     }
  #   } else {
  #     showNotification("Please enter both a name and an equation.", type = "warning")
  #   }
  # })

  # # Observer for the "Edit Model" button
  # observeEvent(input$edit_model, {
  #   selected <- selected_model()
  #   if (!is.null(selected)) {
  #     name <- input$custom_model_name
  #     equation <- input$custom_model

  #     if (nchar(name) > 0 && nchar(equation) > 0) {
  #       if (validate_equation(equation)) {
  #         current_models <- regression_models()
  #         current_models[selected, ] <- c(name, equation)
  #         regression_models(current_models)
  #         updateTextInput(session, "custom_model_name", value = "")
  #         updateTextInput(session, "custom_model", value = "")
  #         selected_model(NULL)
  #       } else {
  #         showNotification("Invalid equation format. It should start with 'y=' and contain 'x'.", type = "error")
  #       }
  #     } else {
  #       showNotification("Please enter both a name and an equation.", type = "warning")
  #     }
  #   } else {
  #     showNotification("Please select a model to edit.", type = "warning")
  #   }
  # })


  # observeEvent(input$remove_model, {
  #   selected <- selected_model()
  #   if (!is.null(selected)) {
  #     showModal(modalDialog(
  #       title = "Confirm Deletion",
  #       "Are you sure you want to remove this model?",
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("confirmRemove", "Remove")
  #       )
  #     ))
  #   } else {
  #     showNotification("Please select a model to remove.", type = "warning")
  #   }
  # })


  # # Observer for the "Remove Model" button
  # observeEvent(input$confirmRemove, {
  #   selected <- selected_model()
  #   if (!is.null(selected)) {
  #     current_models <- regression_models()
  #     updated_models <- current_models[-selected, ]
  #     regression_models(updated_models)
  #     updateTextInput(session, "custom_model_name", value = "")
  #     updateTextInput(session, "custom_model", value = "")
  #     selected_model(NULL)
  #     removeModal()
  #     showNotification("Model removed successfully.", type = "message")
  #   }
  # })



  # -----------------------------------------------
  # Functions for equation creation
  # -----------------------------------------------

  find_id_column <- function(data_frame, prefix = NULL) {

    for (col_name in colnames(data_frame)) {

      # Check for exact matches first, else grep for patterns
      if (col_name %in% c("id(-)", "PurpleAir.id(-)", "AQS.id(-)")) {

        if (is.null(prefix) || grepl(paste0("^", prefix), col_name)) {
          return(col_name)
        }
      } else if (grepl("id\\(-\\)$", col_name, ignore.case = TRUE) ||
                 grepl("\\.id$", col_name, ignore.case = TRUE) ||
                 grepl("^id\\.", col_name, ignore.case = TRUE)) {

        if (is.null(prefix) || grepl(paste0("^", prefix), col_name, ignore.case = TRUE)) {
          return(col_name)
        }
      }
    }

    return(NULL)
  }



  # Generate coefficients for the selected data entries
  observeEvent(input$generate_coefficients, {
    # 1. Check if dependent and independent variables are selected
    # if (is.null(selected_vars$dependent) || length(selected_vars$independent) == 0) {
    #   showNotification("Please select both dependent and independent variables.", type = "warning")
    #   return()
    # }

    # 2. Check if rows are selected in the Neighboring Points table
    selected_rows <- input$selectable_correction_table_rows_selected
    if (length(selected_rows) == 0) {
      showNotification("Please select rows from the Neighboring Points table.", type = "warning")
      return()
    }

    # print("selected_rows in coefficient")
    # print(selected_rows)

    # 3. Get the selected data and extract dataset names and IDs
    selected_data <- filtered_comparison_df()[selected_rows, ]

    # Extract dataset names from column names ( can potentially use dataset_coverages(model) to get names)
    column_names <- colnames(selected_data)
    dataset_names <- unique(sapply(strsplit(column_names[grepl("\\.", column_names)], "\\."), `[`, 1))

    if (length(dataset_names) != 2) {
      showNotification("Expected two distinct datasets, but found a different number.", type = "error")
      return()
    }

    dataset_x_name <- dataset_names[2]
    dataset_y_name <- dataset_names[1]


    # Find the ID columns dynamically
    id_column_x <- find_id_column(selected_data, dataset_x_name)
    id_column_y <- find_id_column(selected_data, dataset_y_name)

    if (is.null(id_column_x) || is.null(id_column_y)) {
      showNotification("Could not find ID columns in the selected data.", type = "error")
      return()
    }

    #selected_ids_x <- selected_data[[id_column_x]]
    selected_ids_y <- selected_data[[id_column_y]]

    # 4. Retrieve full datasets and filter based on selected IDs
    full_data <- comparison_data_frame(model)


    display_equation <- list()

    for (id in selected_ids_y) {
      filtered_data_y <- full_data[full_data[[id_column_y]] %in% id, ]

      # Ensure that the dependent variable is from dataset Y and independents from X
      dependent_var <- paste0(dataset_y_name, ".", input$dataset_y_variable_menu)
      independent_vars <- paste0(dataset_x_name, ".", input$dataset_x_variable_menu)

      # Perform regression based on the selected type
      tryCatch({
        if (input$regression_type == "Linear") {
          formula_str <- paste("`", dependent_var, "` ~ `", independent_vars, "`", sep = "")
          formula <- as.formula(formula_str)
          lm_model <- lm(formula, data = filtered_data_y)
          coefficients <- coef(lm_model) # 1-Intercept, 2-slope
          equation <- sprintf("y = %.4f + %.4fx", coefficients[1], coefficients[2])
        }
        # else if (regression_type == "Polynomial") {
        #   formula_str <- paste("`", dependent_var, "` ~ poly(`", independent_vars, "`, 2)", sep = "")
        #   formula <- as.formula(formula_str)
        #   model <- lm(formula, data = filtered_data_y)
        #   coefficients <- coef(model)
        #   equation <- sprintf("y = %.4f + %.4fx + %.4fx^2", coefficients[1], coefficients[2], coefficients[3])
        # } else if (regression_type == "Exponential") {
        #   formula_str <- paste("log(`", dependent_var, "`) ~ `", independent_vars, "`", sep = "")
        #   formula <- as.formula(formula_str)
        #   model <- lm(formula, data = filtered_data_y)
        #   coefficients <- exp(coef(model))
        #   equation <- sprintf("y = %.4f * exp(%.4fx)", coefficients[1], log(coefficients[2]))
        # }


        # Create a list to store all the information for this ID
        id_info <- list(
          equation = equation,
          coefficients = as.list(coefficients),
          Coefficients = list(
            dependent = dependent_var,
            independent = as.list(coefficients)
          ),
          model_summary = summary(lm_model),
          r_squared = summary(lm_model)$r.squared,
          adjusted_r_squared = summary(lm_model)$adj.r.squared,
          device_name = id_column_y
        )


        # Add the equation to the id_equations list in the model
        model <<- set_id_equation(model, id, id_info)


        # Add the equation and other information to display_equation
        display_equation[[length(display_equation) + 1]] <- list(
          id = id,
          equation = equation,
          coefficients = as.list(coefficients),
          r_squared = summary(lm_model)$r.squared
        )


        # Generate the corrected dataset
        corrected_data <- list(
          filtered_data_y = filtered_data_y,
          # calculated_values = round(coefficients[1] + coefficients[2] * filtered_data_y[[independent_vars]], 4),
          calculated_values = round((filtered_data_y[[dependent_var]] - coefficients[1]) / coefficients[2], 4),
          dependent_var = dependent_var,
          independent_vars = independent_vars
        )

        model <<- add_dataset_to_dictionary(model, id, corrected_data)

      }, error = function(e) {
        output$message_area <- renderPrint({
          cat("Error in regression:", e$message, sep = "\n")
        })
        showNotification(paste("Error in regression:", e$message), type = "error")
      })
    }

    # -----------------------------------------------
    # Convert display_equation to a Data Frame
    # -----------------------------------------------
    if (length(display_equation) > 0) {
      # Convert each list item to a data frame row
      equations_df <- do.call(rbind, lapply(display_equation, function(eq) {
        data.frame(
          Device_ID = eq$id,
          Equation = eq$equation,
          R_Squared = round(as.numeric(eq$r_squared), 4),
          stringsAsFactors = FALSE
        )
      }))

      # Store equations_df in the reactiveVal
      equations_display_df(equations_df)

      # -----------------------------------------------
      # Render the Equations Table in the UI
      # -----------------------------------------------
      output$equations_list <- DT::renderDataTable({
        DT::datatable(
          equations_df,
          options = list(
            pageLength = 10,
            autoWidth = TRUE
          ),
          caption = "Regression Equations and Statistics",
          rownames = FALSE,
          selection = "multiple"

        )
      })
      showNotification("Regression coefficients generated successfully!", type = "message")
    } else {
      showNotification("No equations were generated.", type = "warning")
    }

  })



  observeEvent(input$output_to_csv, {
    # Check if any equations are selected
    if (length(input$equations_list_rows_selected) == 0) {
      showNotification("Please select at least one equation to export.", type = "warning")
      return()
    }

    # Get the selected equations
    selected_equations <- equations_display_df()[input$equations_list_rows_selected, ]

    # Get the regression type
    regression_type <- input$regression_type

    # Create the data/equation folder if it doesn't exist
    dir.create("data/equation", recursive = TRUE, showWarnings = FALSE)
    corrected_data <- id_equations(model)

    # Generate a unique filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("data/equation/", regression_type, "_output_", timestamp, ".csv")

    # Initialize an empty list to store data for each device
    all_devices_data <- list()

    # Process each selected equation (device)
    for (i in 1:seq_len(nrow(selected_equations))) {
      device_id <- selected_equations$Device_ID[i]

      device_data <- corrected_data[[device_id]]

      if (!is.null(device_data)) {
        device_prefix <- strsplit(device_data$device_name, "\\.")[[1]][1]

        # Create a data frame for this device
        device_row <- data.frame(
          Device_ID = device_id,
          Device_Prefix = device_prefix,
          Equation_Form = regression_type,
          Equation = selected_equations$Equation[i],
          R2 = selected_equations$R_Squared[i],
          R2_adj = device_data$adjusted_r_squared
        )

        # Add coefficients
        coeffs <- device_data$coefficients

        for (coeff_name in names(coeffs)) {
          safe_coeff_name <- make.names(coeff_name)
          safe_coeff_name <- sub("^X\\.", "", safe_coeff_name)
          device_row[[safe_coeff_name]] <- coeffs[[coeff_name]]
        }

        # Add this device's data to the list
        all_devices_data[[i]] <- device_row
      }
    }

    # Combine all device data into a single data frame
    csv_data <- do.call(rbind, all_devices_data)

    # Write the data to the CSV file
    write.csv(csv_data, file = filename, row.names = FALSE)

    showNotification(paste("Data exported to", filename), type = "message")
  })



  ####################### Network Summary #####################################

  ## Reset the plots and reactive values for the Network Summary
  reset_plots <- function(reset_flag = 1) {

    # shinyjs::hide("app-content")
    # reset plots 1 - 6 ( geoJson )
    if (reset_flag == 1) {

      ## output features from ui
      output$networksum_map <- renderLeaflet({NULL})
      output$barchart_ns_map <- renderPlot({NULL})
      output$barchart_ns_map_nod <- renderPlot({NULL})
      output$orig_time_series_plot <- renderPlot({NULL})
      output$orig_calendar_series_plot <- renderPlot({NULL})
      output$orig_calendar_plot <- renderPlot({NULL})

      ## reactive input features from ui
      output$dataset_dropdown_txt <- renderText({NULL})
      dataset_dropdown_txt(NULL)
      output$col_value_txt <- renderText({NULL})
      col_value_txt(NULL)
      calendar_val(NULL)
      reactive_ns_polygon(NULL)

      ## store plots in reactive values for download
      reactive_leaflet_map <- reactiveVal(NULL)
      reactive_time_series_plot <- reactiveVal(NULL)
      reactive_calendar_series_plot <- reactiveVal(NULL)
      reactive_calendar <- reactiveVal(NULL)
      reactive_barchart <- reactiveVal(NULL)
      reactive_barchart_aqi <- reactiveVal(NULL)

      ## leaflet map; gray placeholders connected to layout.css
      shinyjs::show(id = "loading-networksum_map", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-networksum_map').style.display = 'block';")

      ## time series; gray placeholders connected to layout.css
      shinyjs::show(id = "loading-orig_time_series_plot", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-orig_time_series_plot').style.display = 'block';")

      ## number of days barchart for certain variables (pm25 and pm10 daily data)
      shinyjs::show(id = "loading-barchart_ns_map_nod", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-barchart_ns_map_nod').style.display = 'block';")

      ## simple barchart below leaflet map; gray placeholders connected to layout.css
      shinyjs::show(id = "loading-barchart_ns_map", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-barchart_ns_map').style.display = 'block';")

      ## calendar series plot; gray placeholders connected to layout.css
      shinyjs::show("loading-orig_calendar_series_plot")
      shinyjs::runjs("document.getElementById('loading-orig_calendar_series_plot').style.display = 'block';")

      ## simple calendar plot; gray placeholders connected to layout.css
      shinyjs::show("loading-orig_calendar_plot")
      shinyjs::runjs("document.getElementById('loading-orig_calendar_plot').style.display = 'block';")

    # reset plots 7 - 8 ( timevar_plot, scatter_plot )
    } else if (reset_flag == 2) {
      output$orig_timevar_plot <- renderPlot({NULL})
      output$orig_scatter_plot <- renderPlot({NULL})

      output$dataset_dropdown1_txt <- renderText({NULL})
      dataset_dropdown1_txt(NULL)

      ## reactive values for download
      reactive_time_variation <- reactiveVal(NULL)
      reactive_scatterplot <- reactiveVal(NULL)

      output$col_value1_txt <- renderText({NULL})
      col_value1_txt(NULL)

      shinyjs::show(id = "loading-orig_timevar_plot", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-orig_timevar_plot').style.display = 'block';")

      shinyjs::show(id = "loading-orig_scatter_plot", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-orig_scatter_plot').style.display = 'block';")

    # reset plots 7 - 8 ( timevar_plot, scatter_plot )
    } else if (reset_flag == 3) {
      output$orig_timevar_plot <- renderPlot({NULL})
      output$orig_scatter_plot <- renderPlot({NULL})

      output$dataset_dropdown2_txt <- renderText({NULL})
      dataset_dropdown2_txt(NULL)

      output$col_value2_txt <- renderText({NULL})
      col_value2_txt(NULL)

      ## reactive values for download
      reactive_time_variation <- reactiveVal(NULL)
      reactive_scatterplot <- reactiveVal(NULL)

      shinyjs::show(id = "loading-orig_timevar_plot", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-orig_timevar_plot').style.display = 'block';")

      shinyjs::show(id = "loading-orig_scatter_plot", anim = TRUE, animType = "fade")
      shinyjs::runjs("document.getElementById('loading-orig_scatter_plot').style.display = 'block';")
      }
  }



  ####################### Network Summary #####################################

  ## observeEvent for retrieving the selected datasets from the "Load Web" panel
  observeEvent(input$search_dataset_l, {

    if (dataset_count(model) < 1) {
      showNotification("No datasets available", type = "warning")
      return()
    }

    ## make sure all plots are cleared prior to new data
    reset_plots(1)
    reset_plots(2)

    data_l <- dataset_coverages(model)
    ## create empty data frame with n columns based on how many datasets got selected
    selected_datasets <- data.frame(matrix(ncol = length(data_l), nrow = 0))

    ## change the column names; col names will be used for the dropdown menu
    colnames(selected_datasets) <- data_l

    ## update the placeholder
    dataset_dropdown_l(selected_datasets)
  })



  ## observeEvent for retrieving the column names from the data frame of the
  ## selected variable in the dropdown menu (search_dataset_l)
  observeEvent(input$retrieve_var, {


    if (is.null(input$dataset_dropdown)) {
      showNotification("Please load a dataset first!", type = "error")

    } else {
      ## make sure plots 1 - 6 are cleared prior to new data
      reset_plots(1)
      dataset <- NULL

      showNotification(paste0("Show variables for: ", input$dataset_dropdown), type = "message")

      ## the identification of the index is important when multiple datasets are loaded
      ## the index (idx) is used to retrieve the specific data from dataset(model, idx)
      idx <- grep(input$dataset_dropdown, colnames(dataset_dropdown_l()))

      while (is.null(names(dataset))) {
        data <- dataset(model, idx)

        ## check if the input variable from the dropdown menu equals the data coverage name
        if (data@coverage == input$dataset_dropdown) {
          dataset <- data_frame(data)
        } else {
          output$message_area <- renderPrint({
            cat("Error in dataset selection!", sep = "\n")
          })
        }
      }
      column_data(dataset)
    }
  })



  ## this reactive feature makes sure that the data frames and plots etc. do not
  ## automatically update if the user selects a different input dataset from the
  ## dropdown menu; this feature is connected to the "update variables" action button
  updateVariables <- eventReactive(input$variables, {

    ## render the text below the "update variables" action button
    output$dataset_dropdown_txt <- renderText({
      as.character(input$dataset_dropdown)
    })
    ## store the selected variable from the dropdown menu in the reactive value
    dataset_dropdown_txt(input$dataset_dropdown)

    ## render the text below the "update variables" action button
    output$col_value_txt <- renderText({
      as.character(input$col_value)
    })
    ## store the selected variable from the dropdown menu in the reactive value
    col_value_txt(input$col_value)
  })



  ## the observe is necessary to display the updates in the ui when the "update
  ## variables" action button is clicked
  observe(updateVariables())



  ## this reactive feature does the same as "updateVariables" for the comparison
  ## of data frame 1 and data frame 2
  updateVariables1 <- eventReactive(input$variables1, {

    output$dataset_dropdown1_txt <- renderText({as.character(input$dataset_dropdown1)})
    output$dataset_dropdown2_txt <- renderText({as.character(input$dataset_dropdown2)})
    dataset_dropdown1_txt(input$dataset_dropdown1)
    dataset_dropdown2_txt(input$dataset_dropdown2)

    output$col_value1_txt <- renderText({as.character(input$sel_column)})
    output$col_value2_txt <- renderText({as.character(input$sel_column2)})
    col_value1_txt(input$sel_column)
    col_value2_txt(input$sel_column2)
  })


  observe(updateVariables1())



  ## observeEvent for the comparison of two variables
  ## retrieve the data frame for variable 1
  observeEvent(input$retrieve_var1, {

    if (is.null(input$dataset_dropdown1)) {
      showNotification("Please load a dataset first!", type = "error")
    } else {

      # make sure plots 7 - 8 are cleared prior to new data
      reset_plots(2)

      dataset <- NULL
      dataset_name <- input$dataset_dropdown1

      showNotification(paste0("Show variables for: ", dataset_name), type = "message")
      idx <- grep(dataset_name, colnames(dataset_dropdown_l()))

      while (is.null(names(dataset))) {
        data <- dataset(model, idx)

        if (data@coverage == dataset_name) {
          dataset <- data_frame(data)
        } else {
          output$message_area <- renderPrint({
            cat("Error in dataset selection!", sep = "\n")
          })
        }
      }
      column_data1(dataset)
    }
  })



  ## observeEvent for the comparison of two variables
  ## retrieve the data frame for variable 2
  observeEvent(input$retrieve_var2, {

    if (is.null(input$dataset_dropdown2)) {
      showNotification("Please load a dataset first!", type = "error")
    } else {

      # make sure plots 7 - 8 are cleared prior to new data
      reset_plots(3)

      dataset <- NULL
      dataset_name <- input$dataset_dropdown2

      showNotification(paste0("Show variables for: ", dataset_name), type = "message")
      idx <- grep(dataset_name, colnames(dataset_dropdown_l()))

      while (is.null(names(dataset))) {
        data <- dataset(model, idx)

        if (data@coverage == dataset_name) {
          dataset <- data_frame(data)
        } else {
          output$message_area <- renderPrint({
            cat("Error in dataset selection!", sep = "\n")
          })
          showNotification(paste0("It seems the selected variable does not fit to the dataset: ", dataset_name), type = "error")
        }
      }

      column_data2(dataset)
    }
  })



  ## update the dropdown menus by retrieving the column names of the selected data frame of a variable
  observeEvent(dataset_dropdown_l(), {
    updateVarSelectInput(session, "dataset_dropdown", data = dataset_dropdown_l())
    updateVarSelectInput(session, "dataset_dropdown1", data = dataset_dropdown_l())
    updateVarSelectInput(session, "dataset_dropdown2", data = dataset_dropdown_l())
  })



  ## update the dropdown menus by retrieving the column names of the selected data frame of a variable
  ## exclude certain options in the dropdown menu to avoid processing errors due
  ## to the wrong column selection
  observeEvent(column_data(), {
    df <- column_data()
    exclude_columns <- ASNAT_non_variable_column_names()
    dropdown_value <-
      as.data.frame(df[, -which(names(df) %in% exclude_columns), drop = FALSE])

    if (ncol(dropdown_value) > 0) {
      updateVarSelectInput(session, "col_value", data = dropdown_value)
    } else {
      updateVarSelectInput(session, "col_value", data = NULL)
    }
  })



  ## update the dropdown menus by retrieving the column names of the selected data frame of a variable
  ## exclude certain options in the dropdown menu to avoid processing errors due
  ## to the wrong column selection (comparison variable 1)
  observeEvent(column_data1(), {
    df <- column_data1()
    exclude_columns <- ASNAT_non_variable_column_names()
    dropdown_value <-
      as.data.frame(df[, -which(names(df) %in% exclude_columns), drop = FALSE])
    updateVarSelectInput(session, "sel_column", data = dropdown_value)
  })



  ## update the dropdown menus by retrieving the column names of the selected data frame of a variable
  ## exclude certain options in the dropdown menu to avoid processing errors due
  ## to the wrong column selection (comparison variable 2)
  observeEvent(column_data2(), {
    df <- column_data2()
    exclude_columns <- ASNAT_non_variable_column_names()
    dropdown_value <-
      as.data.frame(df[, -which(names(df) %in% exclude_columns), drop = FALSE])
    updateVarSelectInput(session, "sel_column2", data = dropdown_value)
  })



  ## trigger the creation of the leaflet map for the Network Summary
  observeEvent(input$get_networksum_map, {

    ## first check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")

    } else {
      showNotification("Create Map for Network Summary", type = "message")
      df <- column_data()

      if (col_value_txt() == "timestamp(UTC)" || !is.numeric(df[[col_value_txt()]])) {
        showNotification("Invalid measurement variable selected.", type = "error")
        return(NULL)
      }

      ## start the calculation of the mean and max values
      calculate_mean_max(df)

      ## create the leaflet map by using the reactive values from mean_max_df()
      ## mean_max_df() got updated in the "calculate_mean_max()" function
      leaflet_map <- create_ns_leaflet(mean_max_df(), input$ns_mean_max_sel)

      ## store the leaflet map in a reactive value; this is necessary to grab the
      ## leaflet map for the download
      reactive_leaflet_map(leaflet_map)

      ## show leaflet map in ui
      output$networksum_map <- renderLeaflet({leaflet_map})
      hide(id = "loading-networksum_map", anim = TRUE, animType = "fade")
      show("app-content")

      ## create the "Mean", "Max" or "Diff Mean" barchart plot
      plot1 <- create_ns_barchart(mean_max_df(), input$ns_mean_max_sel)

      ## store the barchart plot in a reactive value; this is necessary to grab the
      ## barchart plot for the download
      reactive_barchart(plot1)

      ## show first barchart with "Mean", "Max" or "Diff Mean" in ui
      output$barchart_ns_map <- renderPlot({plot1})
      hide(id = "loading-barchart_ns_map", anim = TRUE, animType = "fade")
      show("app-content")

      ## check if the selected data frame is based on daily PM2.5 or PM10 data
      ## if yes, create a barchart that counts the number of days (nob) with a
      ## certain air quality based on the provided breakpoints
      if (any(TRUE %in% stringr::str_detect(as.character(col_value_txt()), c("pm25", "pm10"))) & input$timestep_size == "days") {
        showNotification("Number of days/hours with AQI will be processed!", type = "message")

        ## create the nod barchart for PM2.5 and PM10
        plot1a <- create_ns_barchart_aqi(df_id())

        ## store the barchart in a reactive value for the download
        reactive_barchart_aqi(plot1a)

        ## show nod barchart in ui
        output$barchart_ns_map_nod <- renderPlot({plot1a})
        hide(id = "loading-barchart_ns_map_nod", anim = TRUE, animType = "fade")
        show("app-content")

      } else {
          output$message_area <- renderPrint({
            cat("Processing...", sep = "\n")
          })

      }

      ## Future features: add the nod for ozone!!
      #} else if (any(TRUE %in% stringr::str_detect(as.character(col_value_txt()), c("ozone"))) & input$timestep_size == "hours") {
      #  showNotification("Number of days/hours with AQI will be processed!", type = "message")
      #  plot1a <- create_ns_barchart_aqi(df_id())
      #  reactive_barchart_aqi(plot1a)
      #  output$barchart_ns_map_nod <- renderPlot({plot1a})
      #  hide(id = "loading-barchart_ns_map_nod", anim = TRUE, animType = "fade")
      #  show("app-content")
      #}
    }
  })



  ## observeEvent to create the time series plot if action button is clicked
  observeEvent(input$time_series_plot, {

    ## First check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")

    } else {
      showNotification("Time Series will be processed!", type = "message")

      ## create the time series plot
      plot2 <- draw_time_series(column_data())

      ## store time series in reactive value for download
      reactive_time_series_plot(plot2)

      ## show time series plot in ui
      output$orig_time_series_plot <- renderPlot({plot2})
      hide(id = "loading-orig_time_series_plot", anim = TRUE, animType = "fade")
      show("app-content")
    }
  })



  ## observeEvent to trigger the calendar series when action button is clicked
  observeEvent(input$calendar_series_plot, {

    ## First check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")

    } else {
      showNotification("Calendar Series will be processed!", type = "message")

      ## create the calendar series plot
      plot3 <- draw_calendar_series_plot(column_data())

      ## store calendar series plot in reactive value for download
      reactive_calendar_series_plot(plot3)

      ## show calendar series plot in ui
      output$orig_calendar_series_plot <- renderPlot({plot3})
      hide(id = "loading-orig_calendar_series_plot", anim = TRUE, animType = "fade")
      show("app-content")
    }
  })



  ## observeEvent for the normal calendar plot when action button is clicked
  observeEvent(input$calendar_plot, {

    ## First check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")

    } else {
      showNotification("Calendar will be processed!", type = "message")

      ## the number in the calendar will either display the value or the date based
      ## on the user selection
      calendar_val(input$calendar_sel)

      ## create the calendar plot using openair library (only 1 year will be displayed)
      plot4 <- draw_calendarPlot(column_data())

      ## store the calendar plot in a reactive value for the download
      reactive_calendar(plot4)

      ## show the calendar plot in ui
      output$orig_calendar_plot <- renderPlot({plot4})
      hide(id = "loading-orig_calendar_plot", anim = TRUE, animType = "fade")
      show("app-content")
    }
  })



  ## observeEvent to generate the time variation plot based on Carslaw et al. (xxxx) Fig. 4
  observeEvent(input$timeVariation_plot, {

    ## First check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown1_txt()) && is.null(dataset_dropdown2_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")
    } else if (input$sel_column == input$sel_column2 && input$dataset_dropdown1 == input$dataset_dropdown2) {
      # check if the selected variables are the same and from the same dataset
      showNotification("Please select two different variables from different datasets!", type = "error")
    } else {
      showNotification("Time variation plot will be processed!", type = "message")

      ## create the time variation plot based on df1 and df2
      plot5 <- draw_timeVariation(column_data1(), column_data2())

      ## store the time variation plot in reactive value for the download
      reactive_time_variation(plot5)

      ## show the time variation plot in ui
      output$orig_timevar_plot <- renderPlot({plot5})
      hide(id = "loading-orig_timevar_plot", anim = TRUE, animType = "fade")
      show("app-content")
    }
  })



  ## observeEvent to create the scatterplot when action button is clicked
  observeEvent(input$scatter_plot, {

    ## First check if variables got selected and reactiveVal is not NULL!
    if (is.null(dataset_dropdown1_txt()) && is.null(dataset_dropdown2_txt())) {
      showNotification("Please update the input variables! (Update Variables Button)", type = "error")

    } else {
      showNotification("Scatterplot will be processed!", type = "message")

      ## create the scatterplot based on two input data frames
      plot6 <- draw_scatterplot_ns(column_data1(), column_data2())

      ## store scatterplot in reactive value for download
      reactive_scatterplot(plot6)

      ## show plot in ui
      output$orig_scatter_plot <- renderPlot({plot6})
      hide(id = "loading-orig_scatter_plot", anim = TRUE, animType = "fade")
      show("app-content")
    }
  })



  ## download all visible plots
  output$download_plots_ns <- downloadHandler(

    # Time-stamped output file name (without spaces):
    filename = paste0("ASNAT_Network_Summary_Plots_",
                       format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),
                       ".zip"),

    content = function(file) {
      setwd(tempdir())

      output$message_area <- renderPrint({
        cat("temporary file generated in ", tempdir(), sep = "\n")
      })

      plot_list <- c()

      ## leaflet map
      if (!is.null(reactive_leaflet_map())) {
        mapshot(reactive_leaflet_map(), url = "leaflet.html")
        plot_list <- c(plot_list, "leaflet.html")
      }

      ## barchart (mean, max or diff mean)
      if (!is.null(reactive_barchart())) {
        ggsave("barchart.png", plot = reactive_barchart(),  width = 8, height = 6, units = "in", dpi = 300, device = "png")
        plot_list <- c(plot_list, "barchart.png")
      }

      ## barchart with air quality index (currently only for pm2.5 and pm10)
      if (!is.null(reactive_barchart_aqi())) {
        ggsave("barchart_aqi.png", plot = reactive_barchart_aqi(),  width = 8, height = 6, units = "in", dpi = 300, device = "png")
        plot_list <- c(plot_list, "barchart_aqi.png")
      }

      ## time series plot
      if (!is.null(reactive_time_series_plot())) {
        ggsave("time_series.png", plot = reactive_time_series_plot(), width = 8, height = 6, units = "in", dpi = 300, device = "png")
        plot_list <- c(plot_list, "time_series.png")
      }

      ## calendar series
      if (!is.null(reactive_calendar_series_plot())) {
        ggsave("calendar_series.png", plot = reactive_calendar_series_plot(),  width = 8, height = 6, units = "in", dpi = 300, device = "png")
        plot_list <- c(plot_list, "calendar_series.png")
      }

      ## calendar plot (12 months) using openair library
      if (!is.null(reactive_calendar())) {
        png(filename = "calendar.png", width = 8, height = 6, units = "in", res = 300)
        print(reactive_calendar())
        dev.off()
        plot_list <- c(plot_list, "calendar.png")
      }

      ## time variation plot using openair library see Carslaw et al. (xxxx) Fig. 4
      if (!is.null(reactive_time_variation())) {
        png(filename = "timevariation.png", width = 8, height = 6, units = "in", res = 300)
        print(reactive_time_variation())
        dev.off()
        plot_list <- c(plot_list, "timevariation.png")
      }

      ## scatterplot
      if (!is.null(reactive_scatterplot())) {
        ggsave("scatterplot.png", plot = reactive_scatterplot(), width = 8, height = 6, units = "in", device = "png")
        plot_list <- c(plot_list, "scatterplot.png")
      }

      ## check if plot_list is not empty
      if (is.null(plot_list)) {
        showNotification("No plots generated!", type = "error")

      } else {
        # add all available plots in zip file
        zip(file, plot_list)
      }
  })



  ## download all used data frames
  output$download_df_ns <- downloadHandler(

    # Time-stamped output file name (without spaces):
    filename = paste0("ASNAT_Network_Summary_DF_",
                       format(Sys.time(), "%Y-%m-%d_%H_%M_%S"),
                       ".zip"),

    content = function(file) {
      setwd(tempdir())
      output$message_area <- renderPrint({
        cat("temporary file generated in ", tempdir(), sep = "\n")
      })
      df_list <- c()

      ## create csv file for df
      if (!is.null(column_data())) {
        write.csv(column_data(), file = "df.csv")
        df_list <- c(df_list, "df.csv")
      }

      ## create csv file for df1
      if (!is.null(column_data1())) {
        write.csv(column_data1(), file = "df1.csv")
        df_list <- c(df_list, "df1.csv")
      }

      ## create csv file for df2
      if (!is.null(column_data2())) {
        write.csv(column_data2(), file = "df2.csv")
        df_list <- c(df_list, "df2.csv")
      }

      if (is.null(df_list)) {
        showNotification("No df generated!", type = "error")

      } else {
        # put all available data frames in zip file
        zip(file, df_list)
      }
    })



  ########################## Functions for Network Summary ######################

  ## This mean max calculation will be used to show the mean and maximum values
  ## in the leaflet map
  calculate_mean_max <- function(df) {

    ## calculate mean and max for unique lat/lon coordinates
    coord_l <- unique(paste0(df$`latitude(deg)`, "_", df$`longitude(deg)`))

    ## create empty data frames that will be filled with the newly calculated data
    mean_max_df <- data.frame()
    df_id <- data.frame()

    ## i is needed for the 'newid' column; Letters like A, B, C are limited to 36,
    ## therefore, numbers were used as ID
    i <- 1

    for (coord in coord_l) {
      # print(paste0("Process coordinates: ", coord, " / ", i, " / ", length(coord_l)))

      ## split the string to get lat/lon coordinates
      sub_lat <- strsplit(coord, split = "_")[[1]][1]
      sub_lon <- strsplit(coord, split = "_")[[1]][2]

      ## subset each station by lat/lon information
      sub_df <- subset(df, subset = `latitude(deg)` == as.numeric(sub_lat) & `longitude(deg)` == as.numeric(sub_lon))
      sub_df["newid"] <- i

      ## retrieve the unique lat/lon coordinates from the subsetted data frame
      #unique_lat <- unique(sub_df$`latitude(deg)`)
      #unique_lng <- unique(sub_df$`longitude(deg)`)

      # print(paste0("Lat: ", unique_lat, " | Lng: ", unique_lng))

      ## check if the unique lon coordinate is not bigger than one
      ## it should only be one value otherwise
      if (length(unique(sub_df$`longitude(deg)`)) > 1) {
        showNotification("Error longitude is bigger than 1! Please check calculate_mean_max function", type = "warning")
      }

      if (length(unique(sub_df$`latitude(deg)`)) > 1) {
        showNotification("Error latitude is bigger than 1! Please check calculate_mean_max function", type = "warning")
      }

      ## calculate Mean and Max values for each lat/lon for the selected time period
      new_df <- data.frame(
        lat = unique(sub_df$`latitude(deg)`),
        lon = unique(sub_df$`longitude(deg)`),
        Mean = mean(sub_df[[col_value_txt()]], na.rm = TRUE),
        Max = max(sub_df[[col_value_txt()]], na.rm = TRUE),
        newid = unique(sub_df$newid),
        origid = unique(sub_df$`id(-)`)
      )

      ## row bind the new data frame
      mean_max_df <- rbind(mean_max_df, new_df)
      df_id <- rbind(df_id, sub_df)

      ## update 'newid'
      i <- i + 1
    }

    ## how much does the mean of each point differ by the overall mean of all points
    mean_all_stations <- mean(mean_max_df$Mean, na.rm = TRUE)
    max_all_stations <- max(mean_max_df$Max, na.rm = TRUE)
    mean_max_df["Diff_Mean"] <- mean_max_df$Mean - mean_all_stations
    mean_max_df["Diff_Max"] <- mean_max_df$Max - max_all_stations

    ## update reactive values
    mean_max_df(mean_max_df)
    df_id(df_id)
  }



  df_geojson <- function(polygon_sf, df_complete) {

    output$message_area <- renderPrint({
      cat("Filter data frame by GeoJSON. ", sep = "\n")
    })

    ## convert the data frame into a spatial point feature using lat/lon coordinates
    points_sf <- sf::st_as_sf(df_complete, coords = c("lon", "lat"), crs = st_crs(polygon_sf), remove = FALSE)

    ## carry out a spatial join to keep only point withing a polygon feature
    df1 <- points_sf %>% mutate(intersection = as.integer(st_intersects(geometry, polygon_sf)))

    ## create a new data frame that only include column values with 1 at column "intersection"
    df0 <- subset(df1, subset = df1$intersection == 1)

    ## check if the new data frame is not empty
    if (dim(df0)[1] == 0) {
      showNotification("There are no measurement stations in the GeoJSON!", type = "warning")
      showNotification("Therefore GeoJSON will be ignored! All available measurements will be used!", type = "warning")

      ## if the dimension equals 0 go back to the original data frame for the further analysis
      ## this can happen if there are no observation sites in the polygon feature
      df <- df_complete

    } else {
      output$message_area <- renderPrint({
        cat("Looks good! Measurement stations are found withing the GeoJSON!", sep = "\n")
      })
      ## if dimension is not 0 go on with the new data frame and rename it to df
      df <- df0
      reactive_polygon(TRUE)
    }

    return(df)
  }



  create_ns_leaflet <- function(df_complete, sel_column) {

    ## update reactive value if a GeoJSON was uploaded
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

      ## add the uploaded GeoJSON (spatial polygon feature) to the map
      map <- leaflet(data = df) %>%
        addPolygons(data = polygon_sf)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## but no GeoJSON file was uploaded (NO POLYGON)
    } else if (reactive_ns_polygon() == "Yes" && is.null(input$GeoJSON)) {
      showNotification("Did you forget to upload a GeoJSON?", type = "warning")
      showNotification("All monitoring sites will be used instead.", type = "warning")

      df <- df_complete
      map <- leaflet(data = df)

    ## check if the user does not want to carry out the analysis withing the polygon (NO)
    ## and no GeoJSON was uploaded (POLYGON)
    } else if (reactive_ns_polygon() == "No" && !is.null(input$GeoJSON)) {
      showNotification("Plot GeoJSON without removing data", type = "message")

      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      df <- df_complete
      map <- leaflet(data = df) %>%
        addPolygons(data = polygon_sf)

    ## NO and NO POLYGON
    } else {
      df <- df_complete
      map <- leaflet(data = df)
    }

    ## define color palette from gray to red
    ## Future opportunities: Add a drop down with color schemes
    colpal <- colorNumeric(
      palette = c("gray", "#F80003", "#bf0000"),
      na.color = "blue",
      domain = df[[sel_column]],
      reverse = FALSE
    )

    ## create the leaflet map for network summary
    map <- map %>%

      ## add markers for monitoring sites based on lat/lon coordinates
      addCircleMarkers(
        color = colpal(df[[sel_column]]),
        lng = df$lon,
        lat = df$lat,
        opacity = 1,
        fillOpacity = 0.9,
        stroke = FALSE,
        popup = ~as.character(
          paste0(
            dataset_dropdown_txt(), "<br>",
            sel_column, ": ", round(df[[sel_column]], digits = 2), "<br>",
            "Original ID: ", df$origid, "<br>",
            "Barchart ID: ", df$newid
            )
          ),
        label = df$newid,
        labelOptions = labelOptions(noHide = TRUE, offset = c(-5, 0), textOnly = TRUE)
      ) %>%

      ## add default OpenStreetMap layer
      addTiles(
        group = "Default View",
        options = list(minZoom = 2)
      ) %>%

      ## add satellite imagery layer
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite",
        options = list(minZoom = 2)
      ) %>%

      ## add layer control
      addLayersControl(
        baseGroups = c("Default View", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%

      ## add map title depending on the selected column
      ## the title will change based on the selection after clicking on "Show Map"
      addControl(paste0(dataset_dropdown_txt(), " (", sel_column, ")"), position = "bottomleft", className = "map-title") %>%

      #setView(lng = -74.05, lat = 40.72, zoom = 11) %>%

      setMaxBounds(-180.0, -90.0, 180.0, 90.0) %>%

      ## zoom in and out feature
      addScaleBar(position = "topleft", options = list(imperial = FALSE)) %>%

      ## add map legend
      addLegend(
        "bottomright", pal = colpal,
        values = df[[sel_column]], #round(df[[sel_column]], digits=2),
        title = sel_column,
        opacity = 0.9
      )

    ## Check if it is a single point coordinate
    ## if so the bounding box needs to add +1 and -1 to the coordinates
    if (all(df$lon == min(df$lon) & df$lon == max(df$lon) & df$lat == min(df$lat) & df$lat == max(df$lat))) {
      map <- map %>%
        fitBounds(
          lng1 = df$lon - 1, lng2 = df$lon + 1,
          lat1 = df$lat - 1, lat2 = df$lat + 1
        )

    } else {
      map <- map %>%
        fitBounds(
          lng1 = min(df$lon), lng2 = max(df$lon),
          lat1 = min(df$lat), lat2 = max(df$lat)
        )
    }

    return(map)
  }



  create_ns_barchart <- function(df_complete, sel_column) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

    } else {
      df <- df_complete
    }

    ## retrieve if polygon is TRUE or FALSE
    polygon <- reactive_polygon()

    ## create plot
    plot <- ggplot(df, aes(x = newid, y = get(sel_column))) +
      geom_bar(stat = "identity", position = "identity") +
      {if (sel_column == "Mean") geom_hline(yintercept = mean(df$Mean, na.rm = TRUE), linetype = "dashed", color = "#F80003")} +
      {if (sel_column == "Max") geom_hline(yintercept = max(df$Max, na.rm = TRUE), linetype = "dashed", color = "#F80003")} +
      xlab("ID") + ylab(paste0(sel_column, " for ", col_value_txt())) +
      {if (polygon == TRUE) ggtitle(paste0(dataset_dropdown_txt(), " within GeoJSON"))} +
      {if (polygon == FALSE) ggtitle(paste0(dataset_dropdown_txt(), " all monitoring sites"))} +
      scale_x_continuous(breaks = seq(min(df$newid), max(df$newid), 1), guide = guide_axis(check.overlap = TRUE)) +
      theme_bw()

    return(plot)
  }



  ## calculate number of days with specific air quality
  create_ns_barchart_aqi <- function(df_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      ## rename the lat/lon columns
      df_complete["lat"] <- df_complete$`latitude(deg)`
      df_complete["lon"] <- df_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

    } else {
      output$message_area <- renderPrint({
        cat("No input GeoJSON!", sep = "\n")
      })

      df <- df_complete
    }

    ## retrieve if polygon is TRUE or FALSE
    polygon <- reactive_polygon()

    ## create a new column in df called "aqi"
    df["aqi"] <- NA

    ## create empty data frame to store values
    df_aqi_nod <- data.frame()

    ## check if pm25 in selected variable and
    ## check if timestep_size equals days
    if (grepl("pm25", col_value_txt()) && input$timestep_size == "days") {
      breakpoint <- ASNAT_pm25_daily_aqi_breakpoints

    ## check if pm10 in selected variable and
    ## check if timestep_size equals days
    } else if (grepl("pm10", col_value_txt()) && input$timestep_size == "days") {
      breakpoint <- ASNAT_pm10_daily_aqi_breakpoints

    ## Future option: add ozone
    #} else if (grepl("ozone", col_value_txt()) & input$timestep_size == "hours") {
      #breakpoint <- ASNAT_ozone_1_hour_aqi_breakpoints
      #breakpoint <- ASNAT_ozone_8_hour_aqi_breakpoints

    } else {
      output$message_area <- renderPrint({
        cat("No match!", sep = "\n")
      })
    }

    ## add information about the air quality based on the breakpoint to empty data frame
    if (dim(filter(df, df[[col_value_txt()]] < breakpoint[1]))[1] != 0) {
      ## Good
      df_A <- df %>% filter(get(col_value_txt()) < breakpoint[1]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[1]))
      df_aqi_nod <- rbind(df_aqi_nod, df_A)
    }

    if (dim(filter(df, df[[col_value_txt()]] >= breakpoint[1] & df[[col_value_txt()]] < breakpoint[2]))[1] != 0) {
      ## Moderate
      df_B <- df %>% filter(get(col_value_txt()) >= breakpoint[1] & get(col_value_txt()) < breakpoint[2]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[2]))
      df_aqi_nod <- rbind(df_aqi_nod, df_B)
    }

    if (dim(filter(df, df[[col_value_txt()]] >= breakpoint[2] & df[[col_value_txt()]] < breakpoint[3]))[1] != 0) {
      ## Unhealthy for some
      df_C <- df %>% filter(get(col_value_txt()) >= breakpoint[2] & get(col_value_txt()) < breakpoint[3]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[3]))
      df_aqi_nod <- rbind(df_aqi_nod, df_C)
    }

    if (dim(filter(df, df[[col_value_txt()]] >= breakpoint[3] & df[[col_value_txt()]] < breakpoint[4]))[1] != 0) {
      ## Unhealthy
      df_D <- df %>% filter(get(col_value_txt()) >= breakpoint[3] & get(col_value_txt()) < breakpoint[4]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[4]))
      df_aqi_nod <- rbind(df_aqi_nod, df_D)
    }

    if (dim(filter(df, df[[col_value_txt()]] >= breakpoint[4] & df[[col_value_txt()]] < breakpoint[5]))[1] != 0) {
      ## Very unhealthy
      df_E <- df %>% filter(get(col_value_txt()) >= breakpoint[4] & get(col_value_txt()) < breakpoint[5]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[5]))
      df_aqi_nod <- rbind(df_aqi_nod, df_E)
    }

    if (dim(filter(df, df[[col_value_txt()]] >= breakpoint[5] & df[[col_value_txt()]] < breakpoint[6]))[1] != 0) {
      ## Harzadous
      df_F <- df %>% filter(get(col_value_txt()) >= breakpoint[5]) %>% mutate(aqi = tidyr::replace_na(ASNAT_aqi_names[6]))
      df_aqi_nod <- rbind(df_aqi_nod, df_F)
    }

    ## rename the new data frame
    df <- df_aqi_nod

    ## create an unique list of id's
    id_l <- unique(df$newid)

    ## create empty data frame that counts the number of days with certain air quality
    df_count <- data.frame()

    for (id in id_l) {
      df_id <- subset(df, subset = df$newid == id)
      aqi_l <- unique(df_id$aqi)

      for (a in aqi_l) {
        df1 <- subset(df_id, subset = aqi == a & newid == id)
        new_df <- data.frame(
          count = nrow(df1),
          AQI = unique(df1$aqi),
          newid = unique(df1$newid)
        )
        df_count <- rbind(df_count, new_df)
      }
    }

    ## convert the AQI column into a factor for the plot
    df_count$AQI <- factor(df_count$AQI, levels = ASNAT_aqi_names)

    ## create plot
    plot <- ggplot(df_count, aes(x = newid, y = count, fill = AQI)) +
      geom_bar(stat = "identity", position = "stack") +
      # Add text labels showing the count on each bar segment
      geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      xlab("ID") + ylab(paste0("Number of ", input$timestep_size, " for ", col_value_txt())) +
      {if (polygon == TRUE) ggtitle(paste0(dataset_dropdown_txt(), " within GeoJSON"))} +
      {if (polygon == FALSE) ggtitle(paste0(dataset_dropdown_txt(), " all monitoring sites"))} +
      scale_x_continuous(breaks = seq(min(df_count$newid), max(df_count$newid), 1), guide = guide_axis(check.overlap = TRUE)) +
      scale_fill_manual(name = "AQI", values = ASNAT_aqi_colormap, labels = ASNAT_aqi_names) +
      theme_bw() +
      theme(legend.position = "bottom")

    return(plot)
  }

  draw_time_series <- function(df_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {

      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      ## rename lat/lon columns
      df_complete["lat"] <- df_complete$`latitude(deg)`
      df_complete["lon"] <- df_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

    } else {
      output$message_area <- renderPrint({
        cat("No input GeoJSON!", sep = "\n")
      })
      df <- df_complete
    }

    ## retrieve if polygon is TRUE or FALSE
    polygon <- reactive_polygon()

    ## retrieve variable from selection
    value <- col_value_txt()

    ## create new column called "original date" and copy the original date in the column
    df["orig_date"] <- as.POSIXct(df$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")

    ## convert date column to a date object and format the date object to "%Y-%m-%d"
    df$`timestamp(UTC)` <- as.POSIXct(df$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")
    df$`timestamp(UTC)` <- format(df$`timestamp(UTC)`, "%Y-%m-%d")

    ## create unique list based on POSIXct ("%Y-%m-%dT%H:%M:%S-%OS") that includes hourly information
    hourly_l <- unique(df$orig_date)

    ## create new data frame to store the new df
    ts_hourlymean_df <- data.frame()

    for (h in seq(1, length(hourly_l))) {
      # print(paste0("Process hourly mean for: ", hourly_l[h]))

      ## subset the data frame by date
      sub_df <- subset(df, df$orig_date == hourly_l[h])

      ## check if subsetted data frame is not empty
      if (dim(sub_df)[1] != 0) {
        ## Create a new data frame with the mean values for each day
        new_df <- data.frame(
          date = hourly_l[h],
          year = unique(lubridate::year(sub_df$orig_date)),
          mean = mean(sub_df[[value]], na.rm = TRUE)
        )
        ts_hourlymean_df <- rbind(ts_hourlymean_df, new_df)

      } else {
        output$message_area <- renderPrint({
            cat("Data frame empty! Process next day!", sep = "\n")
          })
      }
    }

    ## convert date column into POSIXct object
    ts_hourlymean_df$date <- as.POSIXct(ts_hourlymean_df$date)

    ## calculate mean value of daily data
    daily_l <- unique(df$`timestamp(UTC)`)

    ts_dailymean_df <- data.frame()

    for (d in seq(1, length(daily_l))) {
      # print(paste0("Process daily mean for: ", daily_l[d]))

      ## subset the data frame by date
      sub_df <- subset(df, df$`timestamp(UTC)` == daily_l[d])

      ## check if subsetted data frame is not empty
      if (dim(sub_df)[1] != 0) {
        ## create a new data frame with the mean values for each day
        new_df <- data.frame(
          date = daily_l[d],
          year = unique(lubridate::year(sub_df$`timestamp(UTC)`)),
          mean = mean(sub_df[[value]], na.rm = TRUE),
          max = max(sub_df[[value]], na.rm = TRUE)
        )
        ts_dailymean_df <- rbind(ts_dailymean_df, new_df)

      } else {
        output$message_area <- renderPrint({
            cat("Data frame empty! Process next day!", sep = "\n")
        })
      }
    }

    ## convert date column to POSIXct object
    ts_dailymean_df$date <- as.POSIXct(ts_dailymean_df$date)

    ## create time series plot
    timeseries <- ggplot(ts_dailymean_df, aes(x = date, y = mean)) +
      {if (input$timestep_size == "hours") geom_line(data = ts_hourlymean_df, mapping = aes(x = date, y = mean), color = "gray65")} +
      geom_point() +
      geom_line() +
      scale_x_datetime(labels = scales::date_format("%Y-%m-%d")) +
      xlab("") + ylab(value) +
      {if (polygon == TRUE) ggtitle(paste0(dataset_dropdown_txt(), " within GeoJSON"))} +
      {if (polygon == FALSE) ggtitle(paste0(dataset_dropdown_txt(), " all monitoring sites"))} +
      theme_bw()

    return(timeseries)
  }



  draw_calendarPlot <- function(df_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      ## rename lat/lon column
      df_complete["lat"] <- df_complete$`latitude(deg)`
      df_complete["lon"] <- df_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

    } else {
      output$message_area <- renderPrint({
        cat("No input GeoJSON!", sep = "\n")
      })
      df <- df_complete
    }

    ## retrieve if polygon is TRUE or FALSE
    polygon <- reactive_polygon()

    ## retrieve variable from selection
    value <- col_value_txt()

    ## convert date column to a date object
    df$`timestamp(UTC)` <- as.POSIXct(df$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")
    df$`timestamp(UTC)` <- format(df$`timestamp(UTC)`, "%Y-%m-%d")

    ## calculate mean value of daily data
    daily_l <- unique(df$`timestamp(UTC)`)

    ## create empty data frame to store the new data frame values
    calendar_dailymean_df <- data.frame()

    for (d in seq(1, length(daily_l))) {
      # print(paste0("Process daily mean for: ", daily_l[d]))

      ## subset the dataframe by date
      sub_df <- subset(df, df$`timestamp(UTC)` == daily_l[d])

      ## check if subsetted data frame is not empty
      if (dim(sub_df)[1] != 0) {
        ## create a new data frame with the mean values for each day
        new_df <- data.frame(
          date = daily_l[d],
          year = unique(lubridate::year(sub_df$`timestamp(UTC)`)),
          mean = mean(sub_df[[value]], na.rm = TRUE),
          max = max(sub_df[[value]], na.rm = TRUE)
        )
        calendar_dailymean_df <- rbind(calendar_dailymean_df, new_df)

      } else {
        output$message_area <- renderPrint({
          cat("Subset data frame empty! Process next day!", sep = "\n")
        })
      }
    }

    ## convert date column to date object
    calendar_dailymean_df$date <- as.Date(calendar_dailymean_df$date)

    if (polygon == TRUE) {
      names(calendar_dailymean_df)[names(calendar_dailymean_df) == "mean"] <- paste0("mean_geojson")
      pol_var <- "mean_geojson"
    } else {
      pol_var <- "mean"
    }

    ## create calendar plot
    calendarPlot <- openair::calendarPlot(
      calendar_dailymean_df,
      pollutant = pol_var,
      #year = y,
      annotate = calendar_val(),
      cols = c("white", "#F80003"),
      layout = c(4, 3),
      remove.empty = FALSE
      )

    return(calendarPlot)
  }



  ## create calendar heatmap series
  draw_calendar_series_plot <- function(df_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })
      ## rename lat/lon column
      df_complete["lat"] <- df_complete$`latitude(deg)`
      df_complete["lon"] <- df_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## create data frame that includes only monitoring sites within GeoJSON
      df <- df_geojson(polygon_sf, df_complete)

    } else {
      output$message_area <- renderPrint({
        cat("No input GeoJSON!", sep = "\n")
      })

      df <- df_complete
    }

    ## retrieve if polygon is TRUE or FALSE
    polygon <- reactive_polygon()

    ## retrieve variable from selection
    value <- col_value_txt()

    ## convert date column to a date object
    df$`timestamp(UTC)` <- as.POSIXct(df$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")
    df$`timestamp(UTC)` <- format(df$`timestamp(UTC)`, "%Y-%m-%d")

    ## calculate mean value of daily data
    daily_l <- unique(df$`timestamp(UTC)`)

    calendar_dailymean_df <- data.frame()

    for (d in seq(1, length(daily_l))) {
      # print(paste0("Process calendar series daily mean for: ", daily_l[d]))

      ## subset the dataframe by date
      sub_df <- subset(df, df$`timestamp(UTC)` == daily_l[d])

      if (dim(sub_df)[1] != 0) {
        ## Create a new data frame with the mean values for each day
        new_df <- data.frame(
          date = daily_l[d],
          year = unique(lubridate::year(sub_df$`timestamp(UTC)`)),
          mean = mean(sub_df[[value]], na.rm = TRUE),
          max = max(sub_df[[value]], na.rm = TRUE)
        )
        calendar_dailymean_df <- rbind(calendar_dailymean_df, new_df)
      } else {
        output$message_area <- renderPrint({
          cat("Subset data frame empty! Process next day!", sep = "\n")
        })

      }
    }

    ## convert date column into date object for plot
    calendar_dailymean_df$date <- as.Date(calendar_dailymean_df$date)

    ## add missing dates in data frame and fill with NA
    date_range <- seq.Date(from = as.Date(paste0(lubridate::year(calendar_dailymean_df$date[1]), "-01-01")), to = as.Date(paste0(lubridate::year(calendar_dailymean_df$date[length(calendar_dailymean_df$date)]), "-12-31")), by = "day")
    missing_date_l <- as.character(date_range[!date_range %in% calendar_dailymean_df$date])
    # print(missing_date_l)

    for (md in missing_date_l) {
      # print(paste0("Process missing date (daily df): ", md))
      missing_df <- data.frame(date = as.character(md), year = lubridate::year(md), mean = NA, max = NA)
      calendar_dailymean_df <- rbind(calendar_dailymean_df, missing_df)
    }

    ## order data frame by date
    calendar_dailymean_df <- calendar_dailymean_df[order(as.Date(calendar_dailymean_df$date, format = "%Y-%m-%d")), ]

    ## make sure date column is a date object
    calendar_dailymean_df$date <- as.Date(calendar_dailymean_df$date)

    ## create calendar plot
    plot <- ggTimeSeries::ggplot_calendar_heatmap(calendar_dailymean_df, "date", "mean") +
      scale_fill_continuous(low = "white", high = "#F80003") +
      xlab("") + ylab("") +
      {if (polygon == TRUE) ggtitle(paste0(dataset_dropdown_txt(), " within GeoJSON"))} +
      {if (polygon == FALSE) ggtitle(paste0(dataset_dropdown_txt(), " all monitoring sites"))} +
      facet_wrap(~Year, ncol = 1)

    return(plot)
  }



  draw_timeVariation <- function(df1_complete, df2_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      df1_complete["lat"] <- df1_complete$`latitude(deg)`
      df1_complete["lon"] <- df1_complete$`longitude(deg)`

      df2_complete["lat"] <- df2_complete$`latitude(deg)`
      df2_complete["lon"] <- df2_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## convert the data frames into spatial point features
      points1_sf <- sf::st_as_sf(df1_complete, coords = c("lon", "lat"), crs = st_crs(polygon_sf), remove = FALSE)
      points2_sf <- sf::st_as_sf(df2_complete, coords = c("lon", "lat"), crs = st_crs(polygon_sf), remove = FALSE)

      ## carry out a spatial join
      df_sub1 <- points1_sf %>% mutate(intersection = as.integer(st_intersects(geometry, polygon_sf)))
      df_sub2 <- points2_sf %>% mutate(intersection = as.integer(st_intersects(geometry, polygon_sf)))

      ## only keep values where intersection equals1 and store them in a new data frame
      df01 <- subset(df_sub1, subset = df_sub1$intersection == 1)
      df02 <- subset(df_sub2, subset = df_sub2$intersection == 1)

      ## remove variables
      rm(df_sub1, df_sub2, polygon_sf, points1_sf, points2_sf)

      ## check if both data frames are not empty
      if (dim(df01)[1] != 0 && dim(df02)[1] != 0) {
        output$message_area <- renderPrint({
          cat("Looks good! Measurement stations are found withing the GeoJSON!", sep = "\n")
        })

        df1 <- df01
        df2 <- df02
        polygon <- TRUE

      ## if one data frame turns out to be empty, use for both data frames "all monitoring sites!"
      } else {
        showNotification("One of the data frames has no entries within the GeoJSON!", type = "warning")
        showNotification("All available measurements will be used instead. GeoJSON will be ignored!", type = "warning")
        df1 <- df1_complete
        df2 <- df2_complete
      }

    } else {
      df1 <- df1_complete
      df2 <- df2_complete
    }

    ## add a string to the data frame name
    df_name1 <- paste0(dataset_dropdown1_txt(), "_df1")
    df_name2 <- paste0(dataset_dropdown2_txt(), "_df2")

    ## retrieve the selected variables
    var1 <- col_value1_txt()
    var2 <- col_value2_txt()

    ## convert date column to a date object
    df1$`timestamp(UTC)` <- as.POSIXct(df1$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")
    df2$`timestamp(UTC)` <- as.POSIXct(df2$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")

    ## create hourly mean data frame
    datetime_l1 <- unique(df1$`timestamp(UTC)`)
    datetime_l2 <- unique(df2$`timestamp(UTC)`)

    ## create empty data frames to store new variables
    hourly_mean_df1 <- data.frame()
    hourly_mean_df2 <- data.frame()

    output$message_area <- renderPrint({
      cat("Start generating hourly means!", sep = "\n")
    })

    ## create data frame for df1; this is necessary to carry out the merge of df1 and df2
    for (idx1 in seq(1, length(datetime_l1))) {

      dt <- datetime_l1[idx1]
      # print(paste0("Process datetime: ", dt))

      sub_df1 <- subset(df1, df1$`timestamp(UTC)` == dt)

      new_df1 <- data.frame(
        date = unique(sub_df1$`timestamp(UTC)`),
        year = unique(lubridate::year(sub_df1$`timestamp(UTC)`)),
        variable_1 = mean(sub_df1[[var1]], na.rm = TRUE)
      )
      # print(new_df1)
      hourly_mean_df1 <- rbind(hourly_mean_df1, new_df1)
    }

    ## create data frame for df2; this is necessary to carry out the merge of df1 and df2
    for (idx2 in seq(1, length(datetime_l2))) {

      dt <- datetime_l2[idx2]
      # print(paste0("Process datetime: ", dt))

      sub_df2 <- subset(df2, df2$`timestamp(UTC)` == dt)

      new_df2 <- data.frame(
        date = unique(sub_df2$`timestamp(UTC)`),
        year = unique(lubridate::year(sub_df2$`timestamp(UTC)`)),
        variable_2 = mean(sub_df2[[var2]], na.rm = TRUE)
      )
      # print(new_df2)
      hourly_mean_df2 <- rbind(hourly_mean_df2, new_df2)
    }

    ## merge data frames by date
    showNotification("Merge the selected dataframes", type = "message")
    merged_df <- merge(hourly_mean_df1, hourly_mean_df2, by = c("date", "year"))

    ## replace name by selected input column
    if (polygon == TRUE) {
      dfn1 <- paste0(as.character(df_name1), "_gj")
      dfn2 <- paste0(as.character(df_name2), "_gj")
      names(merged_df)[names(merged_df) == "variable_1"] <- dfn1
      names(merged_df)[names(merged_df) == "variable_2"] <- dfn2
    } else {
      dfn1 <- as.character(df_name1)
      dfn2 <- as.character(df_name2)
      names(merged_df)[names(merged_df) == "variable_1"] <- dfn1
      names(merged_df)[names(merged_df) == "variable_2"] <- dfn2
    }

    output$message_area <- renderPrint({
      cat("Process plot!", sep = "\n")
    })

    ## create time variation plot based on the merged data frame using the openair library
    plot <- openair::timeVariation(merged_df, pollutant = c(dfn1, dfn2), ylab = NULL)

    return(plot)
  }



  draw_scatterplot_ns <- function(df1_complete, df2_complete) {

    ## reset the polygon to FALSE
    polygon <- reactive_polygon(FALSE)

    ## update the reactive polygon feature if TRUE or FALSE
    reactive_ns_polygon(input$ns_polygon)

    ## check if user only wants to carry out the analysis only for features in the GeoJSON (YES)
    ## check if a GeoJSON file is available (POLYGON)
    if (reactive_ns_polygon() == "Yes" && !is.null(input$GeoJSON)) {
      output$message_area <- renderPrint({
        cat("Filter data frame by GeoJSON", sep = "\n")
      })

      df1_complete["lat"] <- df1_complete$`latitude(deg)`
      df1_complete["lon"] <- df1_complete$`longitude(deg)`

      df2_complete["lat"] <- df2_complete$`latitude(deg)`
      df2_complete["lon"] <- df2_complete$`longitude(deg)`

      ## convert the polygon into a spatial feature
      polygon_sf <- sf::read_sf(input$GeoJSON$datapath)

      ## convert the data frames into spatial point features
      points1_sf <- sf::st_as_sf(df1_complete, coords = c("lon", "lat"), crs = st_crs(polygon_sf), remove = FALSE)
      points2_sf <- sf::st_as_sf(df2_complete, coords = c("lon", "lat"), crs = st_crs(polygon_sf), remove = FALSE)

      ## carry out spatial join
      df_sub1 <- points1_sf %>% mutate(intersection = as.integer(st_intersects(geometry, polygon_sf)))
      df_sub2 <- points2_sf %>% mutate(intersection = as.integer(st_intersects(geometry, polygon_sf)))

      ## only keep values where intersection equals1 and store them in a new data frame
      df01 <- subset(df_sub1, subset = df_sub1$intersection == 1)
      df02 <- subset(df_sub2, subset = df_sub2$intersection == 1)

      ## remove variables
      rm(df_sub1, df_sub2, polygon_sf, points1_sf, points2_sf)

      ## check if both data frames are not empty
      if (dim(df01)[1] != 0 && dim(df02)[1] != 0) {
        output$message_area <- renderPrint({
          cat("Looks good! Measurement stations are found withing the GeoJSON!", sep = "\n")
        })

        df1 <- df01
        df2 <- df02
        polygon <- TRUE

      ## if one data frame turns out to be empty, use for both data frames "all monitoring sites!"
      } else {
        showNotification("One of the data frames has no entries within the GeoJSON!", type = "warning")
        showNotification("All available measurements will be used instead. GeoJSON will be ignored!", type = "warning")
        df1 <- df1_complete
        df2 <- df2_complete
      }

    } else {
      df1 <- df1_complete
      df2 <- df2_complete
    }

    ## add a string to the data frame name
    df_name1 <- paste0(dataset_dropdown1_txt(), "_df1")
    df_name2 <- paste0(dataset_dropdown2_txt(), "_df2")

    ## retrieve selected variables
    var1 <- col_value1_txt()
    var2 <- col_value2_txt()

    ## convert date column to a date object
    df1$`timestamp(UTC)` <- as.POSIXct(df1$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")
    df2$`timestamp(UTC)` <- as.POSIXct(df2$`timestamp(UTC)`, format = "%Y-%m-%dT%H:%M:%S-%OS", tz = "UTC")

    ## create hourly mean data frame
    datetime_l1 <- unique(df1$`timestamp(UTC)`)
    datetime_l2 <- unique(df2$`timestamp(UTC)`)

    ## create empty data frame to store new variables
    hourly_mean_df1 <- data.frame()
    hourly_mean_df2 <- data.frame()

    output$message_area <- renderPrint({
      cat("Start generating hourly means!", sep = "\n")
    })

    ## create data frame for df1; this is necessary to carry out the merge of df1 and df2
    for (idx1 in seq(1, length(datetime_l1))) {
      dt <- datetime_l1[idx1]
      # print(paste0("Process datetime: ", dt))

      ## subset data frame by available datetime list
      sub_df1 <- subset(df1, df1$`timestamp(UTC)` == dt)

      ## calculate the mean for sub_df
      new_df1 <- data.frame(
        date = unique(sub_df1$`timestamp(UTC)`),
        year = unique(lubridate::year(sub_df1$`timestamp(UTC)`)),
        variable_1 = mean(sub_df1[[var1]], na.rm = TRUE)
      )
      # print(new_df1)
      hourly_mean_df1 <- rbind(hourly_mean_df1, new_df1)
    }

    ## create data frame for df2; this is necessary to carry out the merge of df1 and df2
    for (idx2 in seq(1, length(datetime_l2))) {
      dt <- datetime_l2[idx2]
      # print(paste0("Process datetime: ", dt))

      ## subset data frame by available datetime list
      sub_df2 <- subset(df2, df2$`timestamp(UTC)` == dt)

      ## calculate the mean for sub_df
      new_df2 <- data.frame(
        date = unique(sub_df2$`timestamp(UTC)`),
        year = unique(lubridate::year(sub_df2$`timestamp(UTC)`)),
        variable_2 = mean(sub_df2[[var2]], na.rm = TRUE)
      )
      # print(new_df2)
      hourly_mean_df2 <- rbind(hourly_mean_df2, new_df2)
    }

    ## merge data frames by date
    showNotification("Merge the selected dataframes", type = "message")
    merged_df <- merge(hourly_mean_df1, hourly_mean_df2, by = c("date", "year"))

    ## replace name by selected input column
    names(merged_df)[names(merged_df) == "variable_1"] <- as.character(df_name1)
    names(merged_df)[names(merged_df) == "variable_2"] <- as.character(df_name2)

    # print("Process plot!")

    ## create plot based on merged data frame
    plot <- ggplot(data = merged_df, aes(x = get(as.character(df_name1)), y = get(as.character(df_name2)))) +
      geom_point() + #alpha=1/10
      xlab(as.character(df_name1)) + ylab(as.character(df_name2)) +
      {if (polygon == TRUE) ggtitle("Monitoring sites within GeoJSON")} +
      {if (polygon == FALSE) ggtitle("All monitoring sites")} +
      geom_smooth(method = "lm") +
      theme_bw() +
      theme(
        legend.position = "none",
        aspect.ratio = 1
        )

    ## add histogram to scatterplot
    plot1 <- ggExtra::ggMarginal(plot, type = "histogram")

    return(plot1)
  }

############################# End of Network Summary ########################




  # Functions for debug purpose only
  # observeEvent(input$debug_test, {
  #   test_function()
  # })



  # test_function<- function() {
  #   dataset_x_name <- input$dataset_x_menu[1L]
  #   dataset_y_name <- input$dataset_y_menu[1L]
  #   variable_x <- input$dataset_x_variable_menu[1L]
  #   variable_y <- input$dataset_y_variable_menu[1L]
  #   aggregate <- "none"
  #
  #   if (timestep_size(model) == "hours") {
  #     aggregate <- "hourly"
  #   } else {
  #     aggregate <- "daily"
  #   }
  #
  #   #if datasetX is avaliable
  #   if (!is.null(dataset_x_name)) {
  #     dataset_x <- find_dataset(model, dataset_x_name)
  #
  #     if (aggregate == "hourly") {
  #     }
  #   }
  #
  #
  #   # Nested helper function to categorize AQI
  #   categorize_daily_aqi <- function(values) {
  #     aqi_cats <- character(length(values))
  #
  #     for (i in seq_along(values)) {
  #       value <- values[i]
  #
  #       if (is.na(value)) {
  #         aqi_cats[i] <- NA
  #       } else {
  #         idx <- findInterval(value, ASNAT_pm25_daily_aqi_breakpoints) + 1
  #         aqi_cats[i] <- ASNAT_aqi_names[idx]
  #       }
  #     }
  #
  #     return(aqi_cats)
  #   }
  #
  #
  #   # Process dataset X
  #
  #   if (!is.null(dataset_x_name)) {
  #     dataset_x <- find_dataset(model, dataset_x_name)
  #
  #     if (!is.null(dataset_x)) {
  #       df_x <- data_frame(dataset_x)
  #       var_col_x <- variable_column(dataset_x)
  #
  #       # Get AQI categories for X
  #       aqi_cats_x <- categorize_daily_aqi(df_x[[var_col_x]])
  #       aqi_counts_x <- table(aqi_cats_x)
  #
  #       # Create plot for dataset X
  #       output$aqi_distribution_x <- renderPlotly({
  #          plot_ly() %>%
  #          add_bars(x = names(aqi_counts_x),
  #                   y = as.numeric(aqi_counts_x),
  #                   marker = list(color = ASNAT_aqi_colormap[1:length(aqi_counts_x)]),
  #                   name = coverage(dataset_x)) %>%
  #          layout(title = paste("AQI Distribution -", coverage(dataset_x)),
  #                 xaxis = list(title = "AQI Category", tickangle = 45),
  #                 yaxis = list(title = "Number of Days"),
  #                 showlegend = TRUE)
  #       })
  #     }
  #   }
  # }


  observeEvent(input$format_manual, {
    showModal(modalDialog(
      title = "Specification of Standard Format Sensor Files",
      tags$div(style = "overflow-y: auto; max-height: 400px; padding: 10px;",
        tags$p("ASNAT can load data from webservices, files in standard format, and Purple Air raw sensor files."),
        tags$p("Standard format files can be loaded using the 'Load data from a standard-format file' Browse button."),
        tags$p("This can be tested by saving retrieved data to files in TSV format (tab-separated is preferred but comma-separated also works) then later using the ASNAT browser to load them back into ASNAT."),
        tags$p("The standard-format files must conform to the following description:"),
        tags$ul(
          tags$li("The first line is a tab-delimited or comma-delimited one-line header:"),
          tags$code("timestamp(UTC) longitude(deg) latitude(deg) id(-) pm25(ug/m3) ... note(-)"),
          tags$li("The first 4 columns and the last column must exactly match the above."),
          tags$li("There are no spaces in the header line. If tab-delimited, there are no commas in the header line."),
          tags$li("Columns after id(-) are numeric variables with units (e.g., pm25(ug/m3) or ozone(ppm)).")
        ),
        tags$p("Example data line:"),
        tags$code("2022-06-01T00:00:00-0000 -74.05 40.72 202438586 35.43 ... 20220601.csv;mac_address=98:cd:ac:10:f7:ba"),
        tags$ul(
          tags$li("Data lines are sorted by timestamp and sensor id."),
          tags$li("Timestamps are in UTC using ISO-8601 format (yyyy-mm-ddThh:mm:ss-0000)."),
          tags$li("Invalid time formats include local time zones, t, Z, +00:00, etc.")
        ),
        tags$p("Additional optional columns:"),
        tags$ul(
          tags$li("An optional elevation(m) column after latitude(deg)."),
          tags$li("An optional count(-) column after id(-), representing aggregated sample counts.")
        ),
        tags$p("Variable column rules:"),
        tags$ul(
          tags$li("Variable names contain only letters, digits, and underscores."),
          tags$li("Each variable has parenthesized units (e.g., ug/m3). Units must be SI (e.g., temperature(C)).")
        ),
        tags$p("Contact ASNAT support for further questions at the email provided in the application.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })



}



###############################################################################
# Run the app:
###############################################################################
shinyApp(ui, server)

