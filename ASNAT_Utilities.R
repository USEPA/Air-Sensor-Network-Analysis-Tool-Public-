###############################################################################
# PURPOSE: ASNAT_Utilities.R - Utility routines used by ASNAT code.
# HISTORY: 2022-10-12 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

# Is ASNAT app deployed as a hosted remote server shiny app
# (e.g., on rstudio-connect or shinyapps.io, etc.) or
# as a local app on the user's computer?

ASNAT_is_remote_hosted <- Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect"

###############################################################################
# Load required libraries:
###############################################################################

# URL to download any required R packages from:

repository <- "http://cran.us.r-project.org"

library(methods)

if (ASNAT_is_remote_hosted) {
  library(seismicRoll) # For seismicRoll::findOutliers().
} else {
  if (!require(seismicRoll)) install.packages("seismicRoll", repos = repository)
}


############################# Tunable Parameters ##############################

# Use C++ versions of functions (defined in ASNAT_Utilities.cpp)
# for 100x speedup?

ASNAT_use_cpp_functions <- TRUE

if (ASNAT_use_cpp_functions) {

  if (ASNAT_is_remote_hosted) {
    library(Rcpp)
  } else {
    if (!require(Rcpp)) install.packages("Rcpp", repos = repository)
  }

  Rcpp::sourceCpp(code = '
  #include <Rcpp.h>
  // [[Rcpp::export]]
  bool RTools_is_installed_and_c_plus_plus_compiler_works() {return true;}')

  stopifnot(RTools_is_installed_and_c_plus_plus_compiler_works())

  Rcpp::sourceCpp("ASNAT_Utilities.cpp")
}

# Use curl program to get data from webservices?
# If TRUE then the platform/bin/curl program will be used
# if FALSE then httr::GET() will be used.
# Note: if running remote-hosted and the remote server is still mis-configured
# (SSL library mismatch problem) then using httr::GET() will fail and cause
# the app to terminate! Using the curl program will also fail to retrieve data
# but the app will not be terminated and local file loads are still possible.

ASNAT_use_curl_program <- TRUE

if (!ASNAT_use_curl_program) {

  if (ASNAT_is_remote_hosted) {
    library(httr)
  } else {
    if (!require(httr)) install.packages("httr", repos = repository)
  }
}


# When ASNAT reads data from webservices or files, these missing/problematic
# values will be converted to NA and kept in the resulting data.frame.
# This allows multi-variable/column data to be read in when some columns have
# missing values.
# However, functions such as min(), max(), mean(), var(), quantile(), etc.
# must then include argument na.rm = TRUE to avoid numeric/plot problems.

ASNAT_na_strings <-
  c("", "''", "NA", "NaN", "nan", "-9999", "-9999.0", "-Inf", "Inf")

# When ASNAT outputs data to a file, any NA values are converted to this:

ASNAT_output_missing_string <- "-9999.0"


# Print debug messages to stderr and perform extra checks?

ASNAT_debugging <- FALSE

if (ASNAT_debugging) {
  ASNAT_dprint <- function(...) cat(sep = "", file = stderr(), sprintf(...))
  ASNAT_check <- function(condition) stopifnot(condition)
  ASNAT_debug <- function(a_function, f_args) do.call(a_function, list(f_args))
} else {
  ASNAT_dprint <- function(...) NULL
  ASNAT_check <- function(...) NULL
  ASNAT_debug <- function(...) NULL
}

# Do performance profiling with timing messages printed to stderr?

ASNAT_timing <- FALSE

if (ASNAT_timing) {
  ASNAT_start_timer <- function() Sys.time()
  ASNAT_elapsed_timer <- function(message, timer) {
    cat(sep = "", file = stderr(),
        sprintf("%s %0.1f seconds.\n",
                message,
                as.numeric(difftime(Sys.time(), timer, units = "secs"))))
  }
} else {
  ASNAT_start_timer <- function(...) {0}
  ASNAT_elapsed_timer <- function(...) {}
}



# Provide a warning message callback so failure messages printed to the console
# can be also be displayed in a GUI message area or pop-up:

ASNAT_warning_callback <- NULL

ASNAT_set_warning_callback <- function(callback) {

  if (is.function(callback)) {
    ASNAT_warning_callback <<- callback
  } else {
    ASNAT_warning_callback <<- NULL
  }
}


ASNAT_warning <- function(failure) {
  warning(failure, call. = FALSE, immediate. = TRUE, noBreaks. = FALSE)

  if (is.function(ASNAT_warning_callback)) {
    ASNAT_warning_callback(failure)
  }
}



# Allow fancy labels so pm25(ug/m3) uses subscript 2.5, mu and superscript?

ASNAT_allow_fancy_labels <- TRUE

# Convert string containing ug/m3 to mu and cube and pm25 to subscript:

ASNAT_fancy_label <- function(label) {
  stopifnot(nchar(label) > 0L)
  result <- label

  if (ASNAT_allow_fancy_labels) {

    # Convert certain units to fancy versions:

    result <- gsub(fixed = TRUE, "ug/m3", "\u03BCg/m\u00B3", result)

    # Construct strings for subscripted PM variables:

    subs <- c("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085")

    pm25_subscripted <- as.character(2.5)
    lapply(0:5, function(z) {
      pm25_subscripted <<-
        gsub(pattern = z, replacement = subs[z + 1], x = pm25_subscripted)
    })
    pm25_subscripted <- paste0("PM", pm25_subscripted)

    pm10_subscripted <- as.character(10)
    lapply(0:5, function(z) {
      pm10_subscripted <<-
        gsub(pattern = z, replacement = subs[z + 1], x = pm10_subscripted)
    })
    pm10_subscripted <- paste0("PM", pm10_subscripted)

    pm1_subscripted <- as.character(1)
    lapply(0:5, function(z) {
      pm1_subscripted <<-
        gsub(pattern = z, replacement = subs[z + 1], x = pm1_subscripted)
    })
    pm1_subscripted <- paste0("PM", pm1_subscripted)

    # Replace all occurrences of such variables with fancy versions:

    result <- gsub(fixed = TRUE, "pm25", pm25_subscripted, result)
    result <- gsub(fixed = TRUE, "pm10", pm10_subscripted, result)
    result <- gsub(fixed = TRUE, "pm1", pm1_subscripted, result)
  }

  return(result)
}



# AQS/AirNow pm25 parameter codes:
# https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html

ASNAT_aqs_pm25_codes <-
  c("88101,88500,88501,88502",
    "88101,88500,88501",
    "88101,88500,88502",
    "88101,88501,88502",
    "88500,88501,88502",
    "88101,88500",
    "88101,88501",
    "88101,88502",
    "88500,88501",
    "88500,88502",
    "88501,88502",
    "88101",
    "88500",
    "88501",
    "88502")



# AQI
# https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.html

ASNAT_aqi_names <- c("Good", "Moderate", "Unhealthy for some", "Unhealthy",
                     "Very Unhealthy", "Hazardous")

# Workaround BUG: Use "#00FF00" for green else "green" appears as "green4".

ASNAT_aqi_colormap <-
  c("#00FF00", "yellow", "orange", "red", "purple", "maroon")

# pm25(ug/m3), ozone(ppm):

ASNAT_pm25_daily_aqi_breakpoints <- c(9.0, 35.4, 55.4, 125.4, 225.4, 1e6)
ASNAT_pm10_daily_aqi_breakpoints <- c(54.0, 154.0, 254.0, 354.0, 424.0, 1e6)
ASNAT_ozone_8_hour_aqi_breakpoints <- c(0.054, 0.07, 0.085, 0.105, 0.2, 1e3)
ASNAT_ozone_1_hour_aqi_breakpoints <- c(0.124, 0.124, 0.164, 0.204, 0.404, 1e3)
ASNAT_ozone_8_hour_aqi_breakpoints <- ASNAT_ozone_8_hour_aqi_breakpoints * 1000
ASNAT_ozone_1_hour_aqi_breakpoints <- ASNAT_ozone_1_hour_aqi_breakpoints * 1000

# Create more plot breakpoints/labels based on the above values:

ASNAT_pm25_daily_aqi_breakpoints_plots <- c(0, ASNAT_pm25_daily_aqi_breakpoints)
ASNAT_pm10_daily_aqi_breakpoints_plots <- c(0, ASNAT_pm10_daily_aqi_breakpoints)

ASNAT_pm25_daily_aqi_breakpoints_labels <-
  rep("", length(ASNAT_pm25_daily_aqi_breakpoints_plots) - 1L)

ASNAT_pm10_daily_aqi_breakpoints_labels <-
  rep("", length(ASNAT_pm10_daily_aqi_breakpoints_plots) - 1L)

for (.i in seq_along(ASNAT_pm25_daily_aqi_breakpoints_labels)) {
  ASNAT_pm25_daily_aqi_breakpoints_labels[.i] <-
    paste0(ASNAT_pm25_daily_aqi_breakpoints_plots[.i], " - ",
           ASNAT_pm25_daily_aqi_breakpoints_plots[.i + 1])
}

for (.i in seq_along(ASNAT_pm10_daily_aqi_breakpoints_labels)) {
  ASNAT_pm10_daily_aqi_breakpoints_labels[.i] <-
    paste0(ASNAT_pm10_daily_aqi_breakpoints_plots[.i], " - ",
           ASNAT_pm10_daily_aqi_breakpoints_plots[.i + 1])
}

ASNAT_pm25_daily_aqi_breakpoints_labels[[length(ASNAT_pm25_daily_aqi_breakpoints_labels)]] <-
  paste0("> ", as.character(ASNAT_pm25_daily_aqi_breakpoints[[length(ASNAT_pm25_daily_aqi_breakpoints) - 1L]]))

ASNAT_pm10_daily_aqi_breakpoints_labels[[length(ASNAT_pm10_daily_aqi_breakpoints_labels)]] <-
  paste0("> ", as.character(ASNAT_pm10_daily_aqi_breakpoints[[length(ASNAT_pm10_daily_aqi_breakpoints) - 1L]]))


# Get AQI statistics target ranges
# targets <- ASNAT_aqi_statistic_target_ranges("pm25_corrected_hourly(ug/m3)")

ASNAT_aqi_statistic_target_ranges <- function(variable_units) {
  ASNAT_dprint("ASNAT_aqi_statistic_target_ranges():")
  stopifnot(class(variable_units) == "character")
  stopifnot(nchar(variable_units) > 0L)
  ASNAT_dprint("  %s\n", variable_units)
  result <- NULL

  # https://www.epa.gov/air-sensor-toolbox/air-sensor-performance-targets-and-testing-protocols

  if ((startsWith(variable_units, "pm10") ||
       grepl(".pm10", variable_units, fixed = TRUE)) &&
       endsWith(variable_units, "(ug/m3)")) {
    result <- list(r2 = c(0.7, 1.0),
                   slope = c(0.65, 1.35),
                   intercept = c(-10.0, 10.0),
                   rmse = c(0.0, 14.0),
                   nrmse = c(0.0, 30.0))
  } else if ((startsWith(variable_units, "pm1") ||
             grepl(".pm1", variable_units, fixed = TRUE)) &&
             endsWith(variable_units, "(ug/m3)")) {

    # No standard for pm1 but use same values as for pm2.5:

    result <- list(r2 = c(0.7, 1.0),
                   slope = c(0.65, 1.35),
                   intercept = c(-5.0, 5.0),
                   rmse = c(0.0, 7.0),
                   nrmse = c(0.0, 30.0))
  } else if ((startsWith(variable_units, "pm25") ||
              grepl(".pm25", variable_units, fixed = TRUE)) &&
              endsWith(variable_units, "(ug/m3)")) {
    result <- list(r2 = c(0.7, 1.0),
                   slope = c(0.65, 1.35),
                   intercept = c(-5.0, 5.0),
                   rmse = c(0.0, 7.0),
                   nrmse = c(0.0, 30.0))
  } else if ((startsWith(variable_units, "ozone") ||
              grepl(".ozone", variable_units, fixed = TRUE)) &&
              endsWith(variable_units, "(ppb)")) {
    result <- list(r2 = c(0.8, 1.0),
                   slope = c(0.8, 1.2),
                   intercept = c(-5.0, 5.0),
                   rmse = c(0.0, 5.0),
                   nrmse = c(NA, NA))
  } else if ((startsWith(variable_units, "no2") ||
              grepl(".no2", variable_units, fixed = TRUE)) &&
              endsWith(variable_units, "(ppb)")) {
    result <- list(r2 = c(0.7, 1.0),
                   slope = c(0.65, 1.35),
                   intercept = c(-5.0, 5.0),
                   rmse = c(0.0, 15.0),
                   nrmse = c(NA, NA))
  } else if ((startsWith(variable_units, "so2") ||
              grepl(".so2", variable_units, fixed = TRUE)) &&
              endsWith(variable_units, "(ppb)")) {
    result <- list(r2 = c(0.7, 1.0),
                   slope = c(0.65, 1.35),
                   intercept = c(-5.0, 5.0),
                   rmse = c(0.0, 15.0),
                   nrmse = c(NA, NA))
  } else if ((startsWith(variable_units, "co") ||
              grepl(".co", variable_units, fixed = TRUE)) &&
              endsWith(variable_units, "(ppm)")) {
    result <- list(r2 = c(0.8, 1.0),
                   slope = c(0.8, 1.2),
                   intercept = c(-0.05, 0.05),
                   rmse = c(0.0, 0.15),
                   nrmse = c(NA, NA))
  }

  ASNAT_debug(str, result)
  return(result)
}



# Is variable AQI-compatible?
# is_aqi <- ASNAT_is_aqi_variable("pm25_corrected_hourly(ug/m3)")


ASNAT_is_aqi_variable <- function(variable_units) {
  ASNAT_dprint("ASNAT_is_aqi_variable():")
  stopifnot(class(variable_units) == "character")
  stopifnot(nchar(variable_units) > 0L)
  ASNAT_dprint("  %s\n", variable_units)
  result <- FALSE

  if (startsWith(variable_units, "pm10") ||
      grepl(".pm10", variable_units, fixed = TRUE)) {
    result <- endsWith(variable_units, "(ug/m3)")
  } else if (startsWith(variable_units, "pm1") ||
             grepl(".pm1", variable_units, fixed = TRUE)) {
    result <- endsWith(variable_units, "(ug/m3)")
  } else if (startsWith(variable_units, "pm25") ||
             grepl(".pm25", variable_units, fixed = TRUE)) {
    result <- endsWith(variable_units, "(ug/m3)")
  } else if (startsWith(variable_units, "ozone") ||
             grepl(".ozone", variable_units, fixed = TRUE)) {
    result <- endsWith(variable_units, "(ppb)")
  }

  ASNAT_dprint("  result = %d", as.integer(result))
  return(result)
}



# Are variables AQI-compatible?
# is_aqi <- ASNAT_is_aqi_compatible("pm25(ug/m3)",
#                                   "pm25_corrected_hourly(ug/m3)")


ASNAT_is_aqi_compatible <- function(variable_units1, variable_units2) {
  ASNAT_dprint("ASNAT_is_aqi_compatible():")
  stopifnot(class(variable_units1) == "character")
  stopifnot(class(variable_units2) == "character")
  stopifnot(nchar(variable_units1) > 0L)
  stopifnot(nchar(variable_units2) > 0L)
  ASNAT_dprint("  %s %s\n", variable_units1, variable_units2)
  result <- FALSE

  if (startsWith(variable_units1, "pm10") ||
      grepl(".pm10", variable_units1, fixed = TRUE)) {
    result <-
      (startsWith(variable_units2, "pm10") ||
       grepl(".pm10", variable_units2, fixed = TRUE)) &&
      endsWith(variable_units1, "(ug/m3)") &&
      endsWith(variable_units2, "(ug/m3)")
  } else if (startsWith(variable_units1, "pm1") ||
             grepl(".pm1", variable_units1, fixed = TRUE)) {
    result <-
      (startsWith(variable_units2, "pm1") ||
       grepl(".pm1", variable_units2, fixed = TRUE)) &&
      endsWith(variable_units1, "(ug/m3)") &&
      endsWith(variable_units2, "(ug/m3)")
  } else if (startsWith(variable_units1, "pm25") ||
             grepl(".pm25", variable_units1, fixed = TRUE)) {
    result <-
      (startsWith(variable_units2, "pm25") ||
       grepl(".pm25", variable_units1, fixed = TRUE)) &&
      endsWith(variable_units1, "(ug/m3)") &&
      endsWith(variable_units2, "(ug/m3)")
  } else if (startsWith(variable_units1, "ozone") ||
             grepl(".ozone", variable_units1, fixed = TRUE)) {
    result <-
      (startsWith(variable_units2, "ozone") ||
       grepl(".ozone", variable_units1, fixed = TRUE)) &&
      endsWith(variable_units1, "(ppb)") &&
      endsWith(variable_units2, "(ppb)")
  }

  ASNAT_dprint("  result = %d", as.integer(result))
  return(result)
}



# Define function to get AQI indices of values given breakpoints:

ASNAT_aqi_data_indices <- function(values, breakpoints) {
  ASNAT_dprint("ASNAT_aqi_data_indices():")
  stopifnot(class(values) == "numeric" || class(values) == "integer")
  stopifnot(length(values) >= 1L)
  stopifnot(length(breakpoints) > 0L)
  ASNAT_dprint("values:")
  ASNAT_debug(str, values)
  ASNAT_dprint("breakpoints:")
  ASNAT_debug(str, breakpoints)

  count <- length(values)
  result <- rep(0L, count)

  for (index in seq_along(values)) {
    value <- values[[index]]

    for (aqi_index in seq_along(breakpoints)) {
      aqi_breakpoint <- breakpoints[[aqi_index]]

      if (value <= aqi_breakpoint) {
        result[[index]] <- aqi_index
        break
      }
    }
  }

  ASNAT_dprint("ASNAT_aqi_data_indices(): result:")
  ASNAT_debug(str, result)
  return(result)
}



# Define function to get AQI colors of values given breakpoints:

ASNAT_aqi_data_colors <- function(values, breakpoints, colormap) {
  ASNAT_dprint("ASNAT_aqi_data_colors():")
  stopifnot(class(values) == "numeric" || class(values) == "integer")
  stopifnot(length(values) >= 1L)
  stopifnot(length(breakpoints) > 0L)
  stopifnot(length(colormap) == length(breakpoints))
  ASNAT_dprint("values:")
  ASNAT_debug(str, values)
  ASNAT_dprint("breakpoints:")
  ASNAT_debug(str, breakpoints)
  ASNAT_dprint("colormap:")
  ASNAT_debug(str, colormap)

  count <- length(values)
  result <- rep("black", count)

  for (index in seq_along(values)) {
    value <- values[[index]]

    # Skip NA values - assign a default color

    if (is.na(value)) {
      result[[index]] <- "#858585"
      next
    }

    for (color_index in seq_along(breakpoints)) {
      aqi_breakpoint <- breakpoints[[color_index]]
      aqi_color <- colormap[[color_index]]

      if (value <= aqi_breakpoint) {
        result[[index]] <- aqi_color
        break
      }
    }
  }

  ASNAT_dprint("ASNAT_aqi_data_color(): result:")
  ASNAT_debug(str, result)
  return(result)
}



# Return array of AQI indices for values or NULL if not applicable to variable:

ASNAT_aqi_indices <- function(variable, is_hourly, values) {
  ASNAT_dprint("ASNAT_aqi_value_indices():")
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  stopifnot(is_hourly == TRUE || is_hourly == FALSE)
  stopifnot(class(values) == "numeric" || class(values) == "integer")
  stopifnot(length(values) >= 1L)
  ASNAT_dprint("  variable = %s\n", variable)
  result <- NULL

  if ((startsWith(variable, "pm10") ||
       grepl(".pm10", variable, fixed = TRUE)) &&
       endsWith(variable, "(ug/m3)")) {
    result <- ASNAT_aqi_data_indices(values, ASNAT_pm10_daily_aqi_breakpoints)
  } else if (((startsWith(variable, "pm1") ||
               grepl(".pm1", variable, fixed = TRUE)) ||
              (startsWith(variable, "pm25") ||
               grepl(".pm25", variable, fixed = TRUE))) &&
             endsWith(variable, "(ug/m3)")) {
    result <- ASNAT_aqi_data_indices(values, ASNAT_pm25_daily_aqi_breakpoints)
  } else if ((startsWith(variable, "ozone") ||
              grepl(".ozone", variable, fixed = TRUE)) &&
              endsWith(variable, "(ppb)")) {

    if (is_hourly) {
      result <-
        ASNAT_aqi_data_indices(values, ASNAT_ozone_1_hour_aqi_breakpoints)
    } else {
      result <-
        ASNAT_aqi_data_indices(values, ASNAT_ozone_8_hour_aqi_breakpoints)
    }
  }

  return(result)
}



# Return array of AQI colors for values or NULL if not applicable to variable:

ASNAT_aqi_colors <- function(variable, is_hourly, values) {
  ASNAT_dprint("ASNAT_aqi_value_colors():")
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  stopifnot(is_hourly == TRUE || is_hourly == FALSE)
  stopifnot(class(values) == "numeric" || class(values) == "integer")
  stopifnot(length(values) >= 1L)
  ASNAT_dprint("  variable = %s\n", variable)
  result <- NULL

  if ((startsWith(variable, "pm10") ||
       grepl(".pm10", variable, fixed = TRUE)) &&
       endsWith(variable, "(ug/m3)")) {
    result <-
      ASNAT_aqi_data_colors(values,
                            ASNAT_pm10_daily_aqi_breakpoints,
                            ASNAT_aqi_colormap)
  } else if (((startsWith(variable, "pm1") ||
               grepl(".pm1", variable, fixed = TRUE)) ||
              (startsWith(variable, "pm25") ||
               grepl(".pm25", variable, fixed = TRUE))) &&
             endsWith(variable, "(ug/m3)")) {
    result <-
      ASNAT_aqi_data_colors(values,
                            ASNAT_pm25_daily_aqi_breakpoints,
                            ASNAT_aqi_colormap)
  } else if ((startsWith(variable, "ozone") ||
              grepl(".ozone", variable, fixed = TRUE)) &&
              endsWith(variable, "(ppb)")) {

    if (is_hourly) {
      result <-
        ASNAT_aqi_data_colors(values,
                              ASNAT_ozone_1_hour_aqi_breakpoints,
                              ASNAT_aqi_colormap)
    } else {
      result <-
        ASNAT_aqi_data_colors(values,
                              ASNAT_ozone_8_hour_aqi_breakpoints,
                              ASNAT_aqi_colormap)
    }
  }

  return(result)
}



# Return array of AQI category neighbor match percentages:

ASNAT_aqi_comparison <- function(aqi_indices_x, aqi_indices_y) {
  ASNAT_dprint("ASNAT_aqi_comparison():")
  stopifnot(class(aqi_indices_x) == "integer")
  stopifnot(class(aqi_indices_y) == "integer")
  stopifnot(length(aqi_indices_x) > 0L)
  stopifnot(length(aqi_indices_y) == length(aqi_indices_x))

  aqi_categories <- length(ASNAT_aqi_names)
  result <- rep(0L, aqi_categories * aqi_categories)
  index <- 1L

  for (aqi_category in 1L:aqi_categories) {
    indices_x <- which(aqi_indices_x == aqi_category)
    count_x <- length(indices_x)

    if (count_x > 0L) {
      neighbor_aqi_indices_y <- aqi_indices_y[indices_x]

      for (aqi_category2 in 1L:aqi_categories) {
        indices_y <- which(neighbor_aqi_indices_y == aqi_category2)
        count_y <- length(indices_y)
        result[index] <- count_y / count_x * 100.0
        index <- index + 1L
      }
    } else {
      index <- index + aqi_categories
    }
  }

  ASNAT_dprint("result:\n")
  ASNAT_debug(str, result)
  return(result)
}



# Return array of AQI breakpoints for variable or NULL if not applicable:

ASNAT_aqi_variable_breakpoints <- function(variable, is_hourly) {
  ASNAT_dprint("ASNAT_aqi_variable_breakpoints():")
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  stopifnot(is_hourly == TRUE || is_hourly == FALSE)

  ASNAT_dprint("  variable = %s\n", variable)
  result <- NULL

  if ((startsWith(variable, "pm10") ||
       grepl(".pm10", variable, fixed = TRUE)) &&
       endsWith(variable, "(ug/m3)")) {
    result <- ASNAT_pm10_daily_aqi_breakpoints
  } else if (((startsWith(variable, "pm1") ||
               grepl(".pm1", variable, fixed = TRUE)) ||
              (startsWith(variable, "pm25") ||
               grepl(".pm25", variable, fixed = TRUE))) &&
             endsWith(variable, "(ug/m3)")) {
    result <- ASNAT_pm25_daily_aqi_breakpoints
  } else if ((startsWith(variable, "ozone") ||
              grepl(".ozone", variable, fixed = TRUE)) &&
              endsWith(variable, "(ppb)")) {

    if (is_hourly) {
      result <- ASNAT_ozone_1_hour_aqi_breakpoints
    } else {
      result <- ASNAT_ozone_8_hour_aqi_breakpoints
    }
  }

  return(result)
}



# Return data.frame of neighbor AQI statistics:

ASNAT_neighbor_aqi_statistics <-
function(variable, is_hourly, measures_x, measures_y) {
  ASNAT_dprint("ASNAT_neighbor_aqi_statistics():")
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  stopifnot(is_hourly == TRUE || is_hourly == FALSE)
  stopifnot(class(measures_x) == "numeric" || class(measures_x) == "integer")
  stopifnot(class(measures_y) == "numeric" || class(measures_y) == "integer")
  stopifnot(length(measures_x) > 0L)
  stopifnot(length(measures_y) == length(measures_x))
  ASNAT_dprint("  variable = %s\n", variable)

  aqi_categories <- length(ASNAT_aqi_names)
  aqi_breakpoints <- ASNAT_aqi_variable_breakpoints(variable, is_hourly)

  result <- data.frame(AQI = ASNAT_aqi_names,
                       breakpoint = aqi_breakpoints,
                       N = rep(0, aqi_categories),
                       NMBE = rep(0, aqi_categories),
                       RMSE = rep(0, aqi_categories),
                       NRMSE = rep(0, aqi_categories))

  aqi_indices_x <- ASNAT_aqi_indices(variable, is_hourly, measures_x)

  for (aqi_category in 1L:aqi_categories) {
    aqi_indices <- which(aqi_indices_x == aqi_category)
    n <- length(aqi_indices)

    if (n > 0L) {
      aqi_measures_x <- measures_x[aqi_indices]
      aqi_measures_y <- measures_y[aqi_indices]
      err <- aqi_measures_x - aqi_measures_y
      se <- err * err
      mse <- mean(se, na.rm = TRUE)
      rmse <- sqrt(mse)
      mean_x_values <- mean(aqi_measures_x, na.rm = TRUE)
      nrmse <- rmse / mean_x_values * 100.0
      abs_x_values <- abs(aqi_measures_x)
      nmbe <- mean(err / abs_x_values, na.rm = TRUE) * 100.0
      aqi_name <- ASNAT_aqi_names[[aqi_category]]
      breakpoint <- aqi_breakpoints[[aqi_category]]
      result[aqi_category, ] <- c(aqi_name, breakpoint, n, nmbe, rmse, nrmse)
    }
  }

  parts <- unlist(strsplit(variable, "[()]"))
  units <- parts[[2L]]
  column2 <- sprintf("Upper_Limit(%s)", units)
  column5 <- sprintf("RMSE(%s)", units)
  colnames(result) <-
    c("AQI_Category(-)", column2, "Count(-)", "NMBE(%)", column5, "NRMSE(%)")

  # Ensure the non-string columns are integer or numeric:

  for (the_column in 2L:6L) {
    the_values <- result[[the_column]]

    if (the_column == 3L) {
      the_values <- as.integer(the_values)
    } else {
      the_values <- as.numeric(the_values)
    }

    result[the_column] <- the_values
  }

  ASNAT_dprint("result:\n")
  ASNAT_debug(print, result)
  return(result)
}



# Return data.frame of neighbor statistics for each pair of neighboring sites:

ASNAT_neighbor_statistics <- function(neighbors_data_frame, only_unflagged) {
  ASNAT_dprint("ASNAT_neighbor_statistics():")
  stopifnot(class(neighbors_data_frame) == "data.frame")
  stopifnot(ncol(neighbors_data_frame) >= 6L)
  stopifnot(nrow(neighbors_data_frame) > 0L)
  stopifnot(only_unflagged == TRUE || only_unflagged == FALSE)

  result <- NULL

  sites_x <- as.integer(neighbors_data_frame[[2L]])
  unique_sites_x <- unique(sort.int(sites_x))
  sites_y <- as.integer(neighbors_data_frame[[4L]])
  unique_sites_y <- unique(sort.int(sites_y))
  flagged_y <- neighbors_data_frame[[6L]]
  measures_x <- neighbors_data_frame[[3L]]
  measures_y <- neighbors_data_frame[[5L]]

  # Compute number of paired neighbor sites:

  count <- 0L

  for (site_x in unique_sites_x) {

    for (site_y in unique_sites_y) {

      if (only_unflagged) {
        paired_sites_rows <-
          which(sites_x == site_x & sites_y == site_y & flagged_y == "0")
      } else {
        paired_sites_rows <-
          which(sites_x == site_x & sites_y == site_y)
      }

      n <- length(paired_sites_rows)

      if (n > 0L) {
        count <- count + 1L
      }
    }
  }

  if (count > 0L) {

    # Allocate result data.frame:

    result <- data.frame(site = rep(0L, count),
                         R2 = rep(NA, count),
                         slope = rep(NA, count),
                         intercept = rep(NA, count),
                         RMSE = rep(NA, count),
                         NRMSE = rep(NA, count))

    index <- 0L

    for (site_x in unique_sites_x) {

      for (site_y in unique_sites_y) {

        if (only_unflagged) {
          paired_sites_rows <-
            which(sites_x == site_x & sites_y == site_y & flagged_y == "0")
        } else {
          paired_sites_rows <-
            which(sites_x == site_x & sites_y == site_y)
        }

        n <- length(paired_sites_rows)

        if (n > 0L) {

          # Compute statistics of neighbor pair time-matched measures:

          site_measures_x <- measures_x[paired_sites_rows]
          site_measures_y <- measures_y[paired_sites_rows]
          correlation <- 0.0

          if (n > 1L) {
            correlation <- cor(site_measures_x, site_measures_y)

            if (is.na(correlation)) {
              correlation <- 0.0
            }
          }

          r_squared <- correlation * correlation
          slope <- cov(site_measures_x, site_measures_y) /
                   var(site_measures_x, na.rm = TRUE)
          y_intercept <-
            mean(site_measures_y, na.rm = TRUE) -
            mean(site_measures_x, na.rm = TRUE) * slope
          err <- site_measures_x - site_measures_y
          se <- err * err
          mse <- mean(se, na.rm = TRUE)
          rmse <- sqrt(mse)
          mean_measures_x <- mean(site_measures_x, na.rm = TRUE)
          nrmse <- rmse / mean_measures_x * 100.0

          # Store row of statistics:

          index <- index + 1L
          result[index, ] <-
            c(site_x, r_squared, slope, y_intercept, rmse, nrmse)
        }
      }
    }

    column_names <- colnames(neighbors_data_frame)
    variable <- column_names[[3L]]
    parts <- unlist(strsplit(variable, "[()]"))
    units <- parts[[2L]]
    intercept_header <- sprintf("Intercept(%s)", units)
    rmse_header <- sprintf("RMSE(%s)", units)
    colnames(result) <-
      c("Site(-)", "R2(-)", "Slope(-)", intercept_header, rmse_header,
        "NRMSE(%)")
  }

  ASNAT_dprint("result:\n")
  ASNAT_debug(print, result)
  return(result)
}



###############################################################################

# Get name of platform this program is running on.

ASNAT_platform <- function() {
  result <- Sys.info()[["sysname"]]

  if (result != "Windows") {
    result <- paste0(result, ".", Sys.info()[["machine"]])
  }

  ASNAT_dprint("ASNAT_platform() returning result = %s\n", result)
  return(result)
}



# Use curl program to retrieve data from url to a file and return TRUE if
# successful else FALSE.

ASNAT_http_get_curl <- function(url, timeout_seconds, output_file_name) {
  stopifnot(nchar(url) > 0L)
  stopifnot(timeout_seconds >= 0L)
  stopifnot(nchar(output_file_name) > 0L)
  result <- FALSE
  single_quote <- "'"
  double_quote <- '"'
  command <- NULL
  args <-
    paste0(" -k --silent --retry 0 -L --tcp-nodelay --max-time ",
           timeout_seconds,
           " --output ", double_quote, output_file_name, double_quote, " ")
  current_directory <- getwd()
  platform <- ASNAT_platform()

  if (startsWith(platform, "Windows")) {
    command <-
      paste0(current_directory, "/Windows/bin/curl.exe ", args,
             double_quote, url, double_quote)
  } else if (startsWith(platform, "Darwin") && file.exists("/usr/bin/curl")) {
    # New MacOS disallows running curl from other directories!
    command <- paste0("/usr/bin/curl ", args, single_quote, url, single_quote)
  } else {
    command <-
      paste0(current_directory, "/", platform, "/bin/curl ", args,
             single_quote, url, single_quote)
  }

  ASNAT_dprint("%s\n", command)
  status <- system(command)
  ASNAT_dprint("status = %d\n", status)
  result <- status == 0L

  if (!result) {
    cat(sep = "", file = stderr(), "Failed to retrieve data from:\n", url, "\n")
  }

  ASNAT_dprint("ASNAT_http_get_curl() returning result = %d\n", result)
  return(result)
}



# Retrieve data from a url to a file and return TRUE if successful else FALSE.

ASNAT_http_get <- function(url, timeout_seconds, output_file_name) {
  stopifnot(nchar(url) > 0L)
  stopifnot(timeout_seconds >= 0L)
  stopifnot(nchar(output_file_name) > 0L)
  result <- FALSE

  if (ASNAT_use_curl_program) {
    result <- ASNAT_http_get_curl(url, timeout_seconds, output_file_name)
  } else {
    ASNAT_dprint("httr::GET(%s, timeout = %d)\n", url, timeout_seconds)
    response <- httr::GET(url, httr::timeout(timeout_seconds))
    status <- httr::status_code(response)
    ASNAT_dprint("status = %d\n", status)

    if (status == 200L) {
      the_content <- httr::content(response, as = "text")

      if (!is.null(the_content)) {
        output_file <- file(output_file_name, "wb") # wb prevents \r chars.
        try(silent = TRUE, writeLines(the_content, output_file))
        close(output_file)
        result <- file.size(output_file_name) > 0L
      }
    }
  }

  if (!result) {
    cat(sep = "", file = stderr(), "Failed to retrieve data from:\n", url, "\n")
  }

  ASNAT_dprint("ASNAT_http_get() returning result = %d\n", result)
  return(result)
}



# Declare S4 class method and ensure isGeneric():
# https://adv-r.hadley.nz/s4.html

ASNAT_declare_method <- function(class_name, method_name, method_body) {
  stopifnot(!is.null(class_name))
  stopifnot(!is.null(method_name))
  stopifnot(nchar(class_name) > 0L)
  stopifnot(nchar(method_name) > 0L)
  stopifnot(!is.null(method_body))

  if (!methods::isGeneric(method_name)) {
    suppressWarnings(methods::setGeneric(method_name, function(object, ...)
                                         standardGeneric(method_name)))
  }

  methods::setMethod(method_name, class_name, method_body)
  stopifnot(isGeneric(method_name))
}



# HACK work-around Windows EPA laptop configuration that hijacks ~:

ASNAT_home_path <- function() {
  result <- path.expand("~")

  if (nchar(result) > 3L &&
      substr(result, 2L, 2L) == ":" &&
      length(grep(fixed = TRUE, "OneDrive", result)) > 0L) {
    parts_list <- strsplit(result, "OneDrive", fixed = TRUE)
    first_part <- unlist(parts_list[[1L]])
    result <- first_part[[1L]]
  }

  return(result)
}



# Add $PWD/$PLATFORM/bin to $PATH so phantomjs and pandoc executables are found
# and also ensure the executables are executable.

ASNAT_augment_path <- function() {
  current_path <- Sys.getenv("PATH")
  current_directory <- getwd()
  platform <- ASNAT_platform()
  new_path <- NULL

  if (platform == "Windows") {
    new_path <- paste0(current_path, ";", current_directory, "\\Windows\\bin")
  } else {
    new_path <-
      paste0(current_path, ":", current_directory, "/", platform, "/bin")

    # Also ensure the executables are executable:

    chmod_command <- paste0("chmod -R +x ", platform)
    system(chmod_command)
  }

  Sys.setenv(PATH = new_path)
}



# Return unique numbered file name:
# result <- ASNAT_unique_file_name("/tmp/data/map_movie.mp4")
# result is next unique file name such as
#   /tmp/data/map_movie.mp4
#   /tmp/data/map_movie_2.mp4
#   /tmp/data/map_movie_3.mp4
#   ...

ASNAT_unique_file_name <- function(file_name) {
  stopifnot(nchar(file_name) > 0L)
  result <- file_name

  if (file.exists(result)) {
    first <- file_name
    extension <- ""
    parts <- unlist(strsplit(file_name, ".", fixed = TRUE))
    parts_length <- length(parts)

    if (parts_length > 1L) {
      extension <- paste0(".", parts[[parts_length]])
      last <- nchar(file_name) - nchar(extension)
      first <- substr(file_name, 1L, last)
    }

    i <- 2L

    repeat {
      result <- paste0(first, "_", as.character(i), extension)

      if (!file.exists(result)) {
        break
      }

      i <- i + 1L
    }
  }

  stopifnot(!file.exists(result))
  return(result)
}



# Does purple_air_key appear conforming?

ASNAT_is_conforming_purple_air_key <- function(purple_air_key) {
  return(nchar(purple_air_key) >= 3L && !grepl("[^A-Z0-9-]", purple_air_key))
}



# Is aqs_pm25_codes a valid subset of AQS pm25 parameter codes?

ASNAT_is_valid_aqs_pm25_codes <- function(aqs_pm25_codes) {
  result <- FALSE

  if (nchar(aqs_pm25_codes)) {
    result <- aqs_pm25_codes %in% ASNAT_aqs_pm25_codes
  }

  return(result)
}



# Is dataset name valid?

ASNAT_is_valid_dataset_name <- function(name) {
  return(nchar(name) >= 1L && !grepl("[^A-Za-z]", name))
}



# Do units match?

ASNAT_units_match <- function(variable1, variable2) {
  result <- FALSE

  if (nchar(variable1) >= 3L && nchar(variable2) >= 3L &&
      endsWith(variable1, ")") && endsWith(variable2, ")")) {
    parts1 <- unlist(strsplit(variable1, "(", fixed = TRUE))
    parts2 <- unlist(strsplit(variable2, "(", fixed = TRUE))

    if (length(parts1) == 2L && length(parts2) == 2L) {
      units <- paste0("(", parts1[[2L]])
      result <- endsWith(variable2, units)
    }
  }

  return(result)
}



# Remove control characters from a readable/writable text file:

ASNAT_filter_text_file <- function(file_name) {
  stopifnot(nchar(file_name) > 0L)
  stopifnot(file.exists(file_name))
  the_file <- file(file_name, "rb")

  if (inherits(the_file, "connection")) {
    lines <- try(silent = TRUE, readLines(the_file))
    close(the_file)

    if (class(lines) == "character") {
      the_file <- try(silent = TRUE, file(file_name, "wb"))

      if (inherits(the_file, "connection")) {

        for (line in lines) {
          line <- gsub("[[:cntrl:]]", "", line)
          try(silent = TRUE, writeLines(line, the_file))
        }
      }
    }
  }

  close(the_file)
}



# Get index of site column from data frame column names:

ASNAT_site_column_index <- function(column_names) {
  stopifnot(!is.null(column_names))
  stopifnot(length(column_names) > 0L)
  possible_site_column_names <-
    c("station(-)", "site(-)", "id(-)", "site_id(-)")
  result <- 0L

  for (possible_site_column_name in possible_site_column_names) {
    result <- which(column_names == possible_site_column_name)

    if (length(result) > 0L && result[[1L]] > 0L) {
      break
    }
  }

  stopifnot(result >= 0L && result <= length(column_names))
  return(result)
}



# Get index of flagged column from data frame column names or 0 if none:

ASNAT_flagged_column_index <- function(column_names) {
  stopifnot(!is.null(column_names))
  stopifnot(length(column_names) > 0L)
  indices <- which(column_names == "flagged(-)")
  result <- 0L

  if (length(indices) > 0L && indices[[1L]] > 0L) {
    result <- indices[[1L]]
  }

  stopifnot(result >= 0L && result <= length(column_names))
  return(result)
}



# Return a data_frame with a next-to-last column "flagged(-)"
# (initialized to "0" if not already present):
# data_frame <- ASNAT_ensure_flagged_column(data_frame)

ASNAT_ensure_flagged_column <- function(data_frame) {
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) > 4L)
  stopifnot(nrow(data_frame) > 0L)
  result <- data_frame
  column_names <- colnames(result)
  has_flagged_column <-
    length(grep(fixed = TRUE, "flagged(-)", column_names)) == 1L

  if (!has_flagged_column) {
    column_count <- length(column_names)
    last_column <- result[[column_count]]
    last_column_name <- column_names[[column_count]]
    result <- result[-column_count]
    row_count <- nrow(result)
    flagged_vector <- rep("0", row_count)
    extra_data_frame <- data.frame(f = flagged_vector, n = last_column)
    colnames(extra_data_frame) <- c("flagged(-)", last_column_name)
    result <- cbind(result, extra_data_frame)
  }

  return(result)
}



# Clear flagged(-) column to "0":

ASNAT_clear_flagged_column <- function(data_frame, keep_99) {
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) > 4L)
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(is.logical(keep_99))

  result <- data_frame
  column_names <- colnames(result)
  index_of_flagged_column <- which(column_names == "flagged(-)")

  if (index_of_flagged_column == 0L) {
    result <- ensure_flagged_column(data_frame)
  } else if (keep_99) {
    result[, index_of_flagged_column] <-
      ifelse(grepl(fixed = TRUE, "99", result[[index_of_flagged_column]]),
             "99", "0")
  } else {
    row_count <- nrow(result)
    result[index_of_flagged_column] <- rep("0", row_count)
  }

  return(result)
}



# Include flag in a sorted unique semi-colon-separated string of flag integers:

ASNAT_include_flag <- function(flagged, flag) {
  stopifnot(grepl("^[0-9]+$", flag))
  stopifnot(length(flag) == 1L)
  stopifnot(as.integer(flag) >= 1L)
  stopifnot(as.integer(flag) <= 99L)
  stopifnot(nchar(flagged) >= 1L)

  if (flagged == 0L) {
    flagged <- flag
  } else if (flagged != flag) {
    flagged_string <- as.character(flagged)
    semicolon_flag <- paste0(";", flag)

    if (length(grep(fixed = TRUE, semicolon_flag, flagged_string)) == 0L) {
      flagged_vector <- unlist(strsplit(flagged_string, ";"))
      flagged_vector <- unique(sort.int(append(flag, flagged_vector)))
      flagged <- paste(sep = ";", flagged_vector, collapse = ";")
    }
  }

  result <- as.character(flagged)
  return(result)
}



# Exclude flag in a sorted unique semi-colon-separated string of flag integers:

ASNAT_exclude_flag <- function(flagged, flag) {
  stopifnot(grepl("^[0-9]+$", flag))
  stopifnot(length(flag) == 1L)
  stopifnot(as.integer(flag) >= 1L)
  stopifnot(as.integer(flag) <= 99L)
  stopifnot(nchar(flagged) >= 1L)

  if (flagged != 0L) {
    flagged_string <- as.character(flagged)
    flagged_vector <- unlist(strsplit(flagged_string, ";"))
    non_flag_indices <- which(flagged_vector != flag)

    if (length(non_flag_indices) > 0L) {
      flagged_vector <- flagged_vector[non_flag_indices]
    } else {
      flagged_vector <- 0L
    }

    flagged <- paste(sep = ";", flagged_vector, collapse = ";")
  }

  result <- as.character(flagged)
  return(result)
}



# function to apply auto/preset flags to data frame
ASNAT_auto_flags <-
function(object, data_frame, selected_flag_variable,
          apply_negtive_value_flag,
          apply_date_validation_flag,
          apply_redundancy_check_flag,
          apply_format_check_flag,
          apply_sudden_spike_flag,
          apply_sudden_drop_flag,
          spike_threshold, spike_time_window,
          drop_threshold, drop_time_window,
          constant_value_threshold,
          apply_daily_pattern_o3,
          apply_daily_pattern_pm,
          hampel_filter_threshold,
          hampel_filter_window,
          long_missing_threshold,
          outlier_threshold,
          outlier_time_window,
          outlier_start_timestamps,
          outlier_end_timestamps) {
  stopifnot(apply_negtive_value_flag == FALSE ||
            apply_negtive_value_flag == TRUE)

  updated <- FALSE
  # Update the model with cleared flags
  flagged_y_full <- data_frame[["flagged(-)"]]
  flagged_y <- seq_len(nrow(data_frame)) # This will be the index

  if (constant_value_threshold > 0L) {
    indices <- integer(0)  # Initialize an empty integer vector
    min_consecutive <- constant_value_threshold

    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)
      rle_result <- rle(data_frame[station_rows, selected_flag_variable])
      consecutive_indices <- which(rle_result$lengths >= min_consecutive)

      if (length(consecutive_indices) > 0) {

        for (idx in consecutive_indices) {
          # Calculate indices within station_rows
          start_index <- if (idx == 1) 1 else sum(rle_result$lengths[1:(idx - 1)]) + 1
          end_index <- sum(rle_result$lengths[1:idx])

          # Map local indices to global dataset indices using station_rows
          global_indices <- station_rows[start_index:end_index]
          indices <- c(indices, global_indices)
        }
      }
    }

    # Update flagged_y values using ASNAT_include_flag
    flagged_y_full[indices] <-
      vapply(flagged_y_full[indices], ASNAT_include_flag,
             c(""), 83L, USE.NAMES = FALSE)
    updated <- TRUE
  }

  if (outlier_threshold > 0) {

    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)
      station_data <- data_frame[data_frame[["id(-)"]] == station, ]
      station_data$rows <- station_rows

      station_data[["timestamp(UTC)"]] <-
        as.POSIXct(station_data[["timestamp(UTC)"]],
                   format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")

      #check for outlier
      measurement_data <- station_data[[selected_flag_variable]]
      mean_value <- mean(measurement_data, na.rm = TRUE)

      if (length(measurement_data) > 0) {
        sd_value <- sd(measurement_data, na.rm = TRUE)
      } else {
        sd_value <- 0
      }

      outlier_indices <- which(abs(station_data[[selected_flag_variable]] - mean_value) > (outlier_threshold * sd_value))
      station_data <- station_data[outlier_indices, ]

      # Check if outlier_start_timestamps and outlier_end_timestamps are not empty
      if (!is.null(outlier_start_timestamps) && !is.null(outlier_end_timestamps) &&
          nchar(outlier_start_timestamps) > 0 && nchar(outlier_end_timestamps) > 0) {
        start_time <- as.POSIXct(outlier_start_timestamps, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
        end_time <- as.POSIXct(outlier_end_timestamps, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
        time_check <- start_time <= end_time

        # Apply time window filter if outlier_time_window is true
        if (outlier_time_window && time_check) {
          # station_data <- station_data[station_data["timestamp(UTC)"] >= start_time & station_data["timestamp(UTC)"]  <= end_time, ]
          station_data <- station_data[station_data[["timestamp(UTC)"]] >= start_time & station_data[["timestamp(UTC)"]] <= end_time, ]
        }
      }

      if (length(outlier_indices) > 0 && nrow(station_data) > 0) {
        flagged_y_full[station_data$rows] <-
          vapply(flagged_y_full[station_data$rows], ASNAT_include_flag,
                 c(""), 85L, USE.NAMES = FALSE)
        updated <- TRUE
      }
    }
  }

  # New condition for long missing data
  if (long_missing_threshold > 0L) {
    indices <- integer(0)  # Initialize an empty integer vector

    for (station in unique(data_frame[["id(-)"]])) {
      # long_missing_indices <- which(data_frame[["id(-)"]] == station & data_frame[[selected_flag_variable]] == 2.1157)
      long_missing_indices <- which(data_frame[["id(-)"]] == station & is.na(data_frame[[selected_flag_variable]]))
      indices <- c(indices, long_missing_indices)
    }

    # Update flagged_y values using ASNAT_include_flag
    flagged_y_full[indices] <-
      vapply(flagged_y_full[indices], ASNAT_include_flag,
             c(""), 84L, USE.NAMES = FALSE)
    updated <- TRUE
  }

  if (apply_negtive_value_flag) {
    # Get column names that start with specified prefixes
    col_names <- names(data_frame)
    target_cols <-
      grep("^(ozone|O3|NO2|CO|pm)", col_names, value = TRUE, ignore.case = TRUE)

    # Loop through each station in the flagged_df
    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)

      # Check each target column for negative values
      negative_indices <- integer(0)

      for (col in target_cols) {
        col_negative_indices <- station_rows[which(data_frame[station_rows, col] < 0)]
        # col_negative_indices <- station_rows[which(data_frame[station_rows, "id(-)"] == 131447)]

        negative_indices <- unique(c(negative_indices, col_negative_indices))
      }

      if (length(negative_indices) > 0) {
        # Get the corresponding indices in the full dataset
        full_indices <- flagged_y[negative_indices]
        # Update flagged_y_full with flag 60 for negative values
        flagged_y_full[full_indices] <- vapply(flagged_y_full[full_indices],
                                               ASNAT_include_flag,
                                               c(""), 60L, USE.NAMES = FALSE)
        updated <- TRUE
      }
    }
  }

  if (apply_date_validation_flag) {

    # Loop through each station in the data frame
    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)

      # Get timestamps for this station
      timestamps <- data_frame[station_rows, "timestamp(UTC)"]

      # Convert timestamps to POSIXct for proper comparison
      timestamps <-
        as.POSIXct(timestamps, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")

      # Check if first timestamp > last timestamp
      if (timestamps[1] > timestamps[length(timestamps)]) {
        # Flag the entire group
        full_indices <- flagged_y[station_rows]
        flagged_y_full[full_indices] <- vapply(flagged_y_full[full_indices],
                                               ASNAT_include_flag,
                                               c(""), 65L, USE.NAMES = FALSE)
        updated <- TRUE
      } else if (length(timestamps) > 1) {
        # Original check for out-of-order timestamps
        invalid_indices <- integer(0)

        for (i in 1:(length(timestamps) - 1)) {

          if (timestamps[i] > timestamps[i + 1]) {
            invalid_indices <-
              c(invalid_indices, station_rows[i], station_rows[i + 1])
          }
        }

        if (length(invalid_indices) > 0) {
          # Get the corresponding indices in the full dataset
          full_indices <- flagged_y[invalid_indices]
          # Update flagged_y_full with flag 61 for invalid timestamps
          flagged_y_full[full_indices] <- vapply(flagged_y_full[full_indices],
                                                 ASNAT_include_flag,
                                                 c(""), 65L, USE.NAMES = FALSE)
          updated <- TRUE
        }
      }
    }
  }

  if (apply_redundancy_check_flag) {
    # Create a data frame with location columns
    location_cols <- c("longitude(deg)", "latitude(deg)")

    if ("elevation(m)" %in% names(data_frame)) {
      location_cols <- c(location_cols, "elevation(m)")
    }

    # Create a composite key of timestamp and location for the entire dataset
    composite_key <- paste(
      data_frame[["timestamp(UTC)"]],
      apply(data_frame[, location_cols, drop = FALSE], 1, paste, collapse = "|"),
      sep = "|"
    )

    # Find duplicate entries
    duplicate_indices <-
      which(duplicated(composite_key) | duplicated(composite_key,
            fromLast = TRUE))

    if (length(duplicate_indices) > 0) {
      # Update flagged_y_full with flag 62 for redundant entries
      flagged_y_full[duplicate_indices] <-
        vapply(flagged_y_full[duplicate_indices],
               ASNAT_include_flag, c(""), 90L, USE.NAMES = FALSE)
      updated <- TRUE
    }
  }

  if (hampel_filter_threshold > 0L && length(selected_flag_variable) > 0) {

    for (station in unique(data_frame[["id(-)"]])) {
      # Filter rows for the current station
      station_rows <- which(data_frame[["id(-)"]] == station)
      station_data <-
        data_frame[station_rows, selected_flag_variable, drop = FALSE]

      # Ensure window size is smaller than the number of records for this station
      if (length(station_rows) >= hampel_filter_threshold) {
        # Apply the Hampel filter to the selected variable for this station
        outlier_indices <- c()  # Default value if there's an error
        outliers <-
          try(seismicRoll::findOutliers(station_data[[selected_flag_variable]],
                                        n = 5,
                                        thresholdMin = 1),
              silent = TRUE)

        # Check if there was an error
        if (!inherits(outliers, "try-error")) {
          # Map the outlier indices from the station subset back to the full dataset indices
          outlier_indices <- station_rows[outliers]
        }

        if (length(outlier_indices) > 0) {
          # Map outlier indices to the full data set
          full_indices <- flagged_y[outlier_indices]
          # Flag these indices with code 86 for outliers detected by the Hampel filter
          flagged_y_full[full_indices] <- vapply(flagged_y_full[full_indices],
                                                 ASNAT_include_flag,
                                                 c(""), 86L, USE.NAMES = FALSE)
          updated <- TRUE
        }
      }
    }
  }

  if (apply_format_check_flag) {
    timestamp_pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}-0000$"

    inconsistent_indices <-
      which(!grepl(timestamp_pattern, data_frame[["timestamp(UTC)"]]))

    if (length(inconsistent_indices) > 0) {
        # Update flagged_y_full with flag 95 for format inconsistencies
      flagged_y_full[inconsistent_indices] <-
        vapply(flagged_y_full[inconsistent_indices],
                 ASNAT_include_flag,  c(""), 95L, USE.NAMES = FALSE)
      updated <- TRUE
    }
  }

  if (spike_threshold > 0) {

    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)

      if (length(station_rows) > 1) { # Need at least 2 points to check for spikes
        # Get the measurement values for this station
        station_data <- data_frame[station_rows, selected_flag_variable]

        # Initialize vector to store indices that need flagging
        spike_indices <- integer(0)

        # Check all points, including those within the initial window
        for (i in 2:length(station_data)) {
          current_value <- station_data[i]

          # For points within initial window, use all available previous values
          start_idx <- max(1, i - spike_time_window)
          previous_values <- station_data[start_idx:(i - 1)]

          # Calculate the average of previous values
          prev_avg <- mean(previous_values, na.rm = TRUE)

          # Check if current value is significantly higher than previous average
          if (prev_avg > 0) {
            percent_increase <- (current_value - prev_avg) / prev_avg

            if (percent_increase > spike_threshold) {
              # Store the index for later flagging
              spike_indices <- c(spike_indices, station_rows[i])
            }
          }
        }
      }

      # Update flags in bulk if any spikes were found
      if (length(spike_indices) > 0) {
        flagged_y_full[spike_indices] <- vapply(flagged_y_full[spike_indices],
                                                ASNAT_include_flag,
                                                c(""), 70L,
                                                USE.NAMES = FALSE)
        updated <- TRUE
      }
    }
  }

  if (drop_threshold > 0) {

    for (station in unique(data_frame[["id(-)"]])) {
      station_rows <- which(data_frame[["id(-)"]] == station)

      if (length(station_rows) > 1) { # Need at least 2 points to check for spikes
        # Get the measurement values for this station
        station_data <- data_frame[station_rows, selected_flag_variable]

        # Initialize vector to store indices that need flagging
        drop_indices <- integer(0)

        # Check all points, including those within the initial window
        for (i in 2:length(station_data)) {
          current_value <- station_data[i]

          # For points within initial window, use all available previous values
          start_idx <- max(1, i - spike_time_window)
          previous_values <- station_data[start_idx:(i - 1)]

          # Calculate the average of previous values
          prev_avg <- mean(previous_values, na.rm = TRUE)

          if (prev_avg > 0) {
            percent_decrease <- (prev_avg - current_value) / prev_avg

            if (percent_decrease > drop_threshold) {
              # Store the index for later flagging
              drop_indices <- c(drop_indices, station_rows[i])
            }
          }
        }
      }

      # Update flags in bulk if any spikes were found
      if (length(drop_indices) > 0) {
        flagged_y_full[drop_indices] <- vapply(flagged_y_full[drop_indices],
                                               ASNAT_include_flag,
                                               c(""), 71L,
                                               USE.NAMES = FALSE)
        updated <- TRUE
      }
    }
  }#drop_threshold

  if (apply_daily_pattern_o3) {
    # Find ozone columns
    col_names <- names(data_frame)
    ozone_cols <- grep("^(ozone|o3)", col_names, value = TRUE, ignore.case = TRUE)

    # Only proceed if we found ozone columns
    if (length(ozone_cols) > 0) {
      # Initialize variables
      all_indices <- integer(0)


      # Define daytime and nighttime hours (in UTC)
      daytime_hours <- 6:18  # 6 AM to 6 PM
      nighttime_hours <- 0:5  # Midnight to 6 AM

      # Convert timestamps to POSIXct
      timestamps <- as.POSIXct(data_frame[["timestamp(UTC)"]],
                               format = "%Y-%m-%dT%H:%M:%S%z",
                               tz = "UTC")

      # Extract hours from timestamps
      hours <- as.numeric(format(timestamps, "%H"))

      # Calculate median daytime value
      daytime_indices <- which(hours %in% daytime_hours)

      for (ozone_col in ozone_cols) {

        # Only proceed if we have daytime measurements
        if (length(daytime_indices) > 0) {
          daytime_median <- median(data_frame[daytime_indices, ozone_col],
                                   na.rm = TRUE)
          daytime_sd <- sd(data_frame[daytime_indices, ozone_col],
                           na.rm = TRUE)

          # Find nighttime measurements
          nighttime_indices <- which(hours %in% nighttime_hours)

          if (length(nighttime_indices) > 0) {
            # Flag nighttime values that are significantly higher than daytime median
            # (using 1.5 times the daytime median as a threshold)
            unusual_night_indices <- nighttime_indices[
              which(data_frame[nighttime_indices, ozone_col] >
                    (daytime_median + 1.5 * daytime_sd))
            ]

            if (length(unusual_night_indices) > 0) {
              all_indices <- unique(c(all_indices, unusual_night_indices))
            }
          }
        }
      }
      # Update flags if any unusual patterns were found
      if (length(all_indices) > 0) {
        flagged_y_full[all_indices] <- vapply(flagged_y_full[all_indices],
                                              ASNAT_include_flag,
                                              c(""), 72L,
                                              USE.NAMES = FALSE)
        updated <- TRUE
      }

    }
  }

  if (apply_daily_pattern_pm) {
    # Find PM columns
    col_names <- names(data_frame)
    pm_cols <- grep("^(pm|PM)", col_names, value = TRUE, ignore.case = TRUE)

    # Only proceed if we found PM columns
    if (length(pm_cols) > 0) {
      # Initialize variables
      all_indices <- integer(0)

      # Define time periods (in UTC)
      morning_hours <- 7:10    # Morning peak
      night_hours <- 21:23     # Night peak
      afternoon_hours <- 15:17 # Afternoon low

      # Convert timestamps to POSIXct
      timestamps <- as.POSIXct(data_frame[["timestamp(UTC)"]],
                               format = "%Y-%m-%dT%H:%M:%S%z",
                               tz = "UTC")

      # Extract hours from timestamps
      hours <- as.numeric(format(timestamps, "%H"))

      for (pm_col in pm_cols) {
        # Calculate medians for each period
        morning_indices <- which(hours %in% morning_hours)
        night_indices <- which(hours %in% night_hours)
        afternoon_indices <- which(hours %in% afternoon_hours)

        # Check morning peak period
        if (length(morning_indices) > 0) {
          morning_median <-
            median(data_frame[morning_indices, pm_col], na.rm = TRUE)
          morning_sd <- sd(data_frame[morning_indices, pm_col], na.rm = TRUE)

          # Check afternoon measurements against morning peak
          if (length(afternoon_indices) > 0) {
            unusual_afternoon_vs_morning <- afternoon_indices[
              which(data_frame[afternoon_indices, pm_col] >
                    (morning_median + 1.5 * morning_sd))
            ]
            if (length(unusual_afternoon_vs_morning) > 0) {
              all_indices <-
                unique(c(all_indices, unusual_afternoon_vs_morning))
            }
          }
        }

        # Check night peak period
        if (length(night_indices) > 0) {
          night_median <-
            median(data_frame[night_indices, pm_col], na.rm = TRUE)
          night_sd <- sd(data_frame[night_indices, pm_col], na.rm = TRUE)

          # Check afternoon measurements against night peak
          if (length(afternoon_indices) > 0) {
            unusual_afternoon_vs_night <- afternoon_indices[
              which(data_frame[afternoon_indices, pm_col] >
                    (night_median + 1.5 * night_sd))
            ]
            if (length(unusual_afternoon_vs_night) > 0) {
              all_indices <- unique(c(all_indices, unusual_afternoon_vs_night))
            }
          }
        }
      }

      # Update flags if any unusual patterns were found
      if (length(all_indices) > 0) {
        flagged_y_full[all_indices] <-
          vapply(flagged_y_full[all_indices],
                 ASNAT_include_flag,
                 c(""), 73L, # Using flag 73 for PM pattern
                 USE.NAMES = FALSE)
        updated <- TRUE
      }
    }
  }

  result <- list(updated = updated, flagged_y_full = flagged_y_full)
  return(result)
}



# Compute updated flagged_y of neighbor points.
# result <-
#   ASNAT_flag_y_neighbors(timestamps, measures_x, measures_y, sites_y,
#                          flagged_y,
#                          maximum_difference,
#                          maximum_percent_difference,
#                          minimum_r_squared)
#
# if (result$updated) {
#   data_frame_y[indices_y, flagged_column_y] <- result$flagged_y
# }
#

ASNAT_flag_y_neighbors <-
function(timestamps, measures_x, measures_y, sites_y, flagged_y,
         maximum_difference,
         maximum_percent_difference,
         minimum_r_squared) {
  timer <- ASNAT_start_timer()
  stopifnot(length(timestamps) > 0L)
  stopifnot(length(measures_x) == length(timestamps))
  stopifnot(length(measures_y) == length(measures_x))
  stopifnot(length(sites_y) == length(measures_x))
  stopifnot(length(flagged_y) == length(measures_x))
  stopifnot(maximum_difference == -1.0 ||
            maximum_difference >= 0.0 && maximum_difference <= 100.0)
  stopifnot(maximum_percent_difference == -1.0 ||
            maximum_percent_difference >= 0.0 &&
            maximum_percent_difference <= 100.0)
  stopifnot(minimum_r_squared == -1.0 ||
            minimum_r_squared >= 0.0 && minimum_r_squared <= 1.0)

  # Update flagged_y based on maximum_difference
  # between pairs of neighbor values:

  updated <- FALSE
  absolute_difference <- NULL

  if (maximum_difference >= 0.0) {
    timer <- ASNAT_start_timer()

    if (is.null(absolute_difference)) {
      absolute_difference <- abs(measures_x - measures_y)
    }

    indices <- which(absolute_difference > maximum_difference)

    if (length(indices) > 0L) {
      flagged_y[indices] <-
         vapply(flagged_y[indices], ASNAT_include_flag,
                c(""), 80L, USE.NAMES = FALSE)
      updated <- TRUE
    }

    ASNAT_dprint("maximum_difference = %d, updated = %d\n",
                 maximum_difference, updated)
    ASNAT_debug(str, flagged_y)
    ASNAT_elapsed_timer("Flagged difference:", timer)
  }

  # Update flagged_y based on maximum_percent_difference
  # between pairs of neighbor values:

  if (maximum_percent_difference >= 0.0) {
    timer <- ASNAT_start_timer()

    if (is.null(absolute_difference)) {
      absolute_difference <- abs(measures_x - measures_y)
    }

    indices <-
      which(200.0 * absolute_difference / abs(measures_x + measures_y) >
            maximum_percent_difference)

    if (length(indices) > 0L) {
      flagged_y[indices] <-
         vapply(flagged_y[indices], ASNAT_include_flag,
                c(""), 81L, USE.NAMES = FALSE)
      updated <- TRUE
    }

    ASNAT_dprint("maximum_percent_difference = %f, updated = %d\n",
                 maximum_percent_difference, updated)
    ASNAT_debug(str, flagged_y)
    ASNAT_elapsed_timer("Flagged percent_difference:", timer)
  }

  # Update flagged_y based on minimum_r_squared
  # (aka 'coefficient of determination' =
  # square(sample Pearson correlation coefficient))
  # of set of per-site neighbor value pairs over all timesteps that are not
  # already flagged:

  if (minimum_r_squared >= 0.0) {
    timer <- ASNAT_start_timer()
    unique_sites_y <- unique(sort.int(sites_y))

    for (site in unique_sites_y) {
      site_rows <- which(sites_y == site & flagged_y == "0")
      site_measures_x <- measures_x[site_rows]
      site_measures_y <- measures_y[site_rows]
      r_squared <- 0.0
      sample_size <- length(site_measures_x)
      stopifnot(length(site_measures_y) == sample_size)

      if (sample_size > 1L) {
        correlation <- cor(site_measures_x, site_measures_y)

        if (!is.na(correlation)) {
          r_squared <- correlation * correlation
        }
      }

      if (!is.na(r_squared) && r_squared < minimum_r_squared) {
        flagged_y[site_rows] <-
          vapply(flagged_y[site_rows], ASNAT_include_flag,
                 c(""), 82L, USE.NAMES = FALSE)
        updated <- TRUE
      }
    }

    ASNAT_dprint("minimum_r_squared = %f, updated = %d\n",
                 minimum_r_squared, updated)
    ASNAT_debug(str, flagged_y)
    ASNAT_elapsed_timer("Flagged r_squared:", timer)
  }

  result <- list(updated = updated, flagged_y = flagged_y)
  return(result)
}



# Translate user-specified flag condition to R syntax.
# Example:
# r_condition <-
#   ASNAT_r_syntax_condition("temperature < 5 or humidity > 90",
#                            c("timestamp", temperature", "humidity"),
#                            "values")
# Example 1:
# temperature < 5 or humidity > 90
# becomes
# values[[2L]] < 5 || values[[3L]] > 90
# Example 2:
# timestamp <= 2022-06-01T14
# becomes
# substr(values[[1L]], 1L, nchar('2022-06-01T14')) <= '2022-06-01T14'

ASNAT_r_syntax_condition <-
  function(condition, condition_variables, function_arguments) {
  #stopifnot(nchar(condition) > 3L)
  stopifnot(length(condition_variables) > 0L)
  stopifnot(nchar(function_arguments) > 0L)
  relational_operators <- c("=", "<", "<=", ">", ">=")
  result <- NULL

  # Ensure spaces around relational operators (to separate from operands):

  # 1. Explode single character operators:

  condition <- gsub(fixed = TRUE, "=", " = ", condition)
  condition <- gsub(fixed = TRUE, "<", " < ", condition)
  condition <- gsub(fixed = TRUE, ">", " > ", condition)
  condition <- gsub(fixed = TRUE, "(", " ( ", condition)
  condition <- gsub(fixed = TRUE, ")", " ) ", condition)

  # 2. Glue back together two character operators:

  condition <- gsub(fixed = TRUE, "<  =", "<=", condition)
  condition <- gsub(fixed = TRUE, ">  =", ">=", condition)

  # 3. Remove double-spaces around operators:

  condition <- gsub(fixed = TRUE, "  =  ", " = ", condition)
  condition <- gsub(fixed = TRUE, "  <  ", " < ", condition)
  condition <- gsub(fixed = TRUE, "  >  ", " > ", condition)
  condition <- gsub(fixed = TRUE, "  <=  ", " <= ", condition)
  condition <- gsub(fixed = TRUE, "  >=  ", " >= ", condition)

  # Split condition into space-delimited words:

  words <- unlist(strsplit(fixed = TRUE, condition, " "))
  word_count <- length(words)

  if (word_count >= 3L) {
    skip <- 0L

    for (word_index in 1L:word_count) {
      word <- words[[word_index]]

      if (nchar(word) > 0L) {

        if (skip > 0L) {
          skip <- skip - 1L
          next
        }

        new_word <- word

        # Change condition variable names to indices then operators to R syntax:

        variable_index <- which(word == condition_variables)

        if (length(variable_index) == 1L) {
          new_word <- paste0(function_arguments, "[[", variable_index, "L]]")

          # Check for and truncate timestamp comparisons:

          if (variable_index == 1L) {
            # Form1: timestamp < 2022-06-01T12
            # Form2: 2022-06-01T12 < timestamp
            is_form2 <- FALSE
            operator <- NULL
            operand <- NULL
            new_expression <- NULL

            if (word_index + 2L <= word_count &&
                words[[word_index + 1L]] %in% relational_operators) {
              operator <- words[[word_index + 1L]]
              operand <- toupper(words[[word_index + 2L]])
              operand_length <- nchar(operand)
              skip <- 3L

              if (operator == "=") {
                operator <- "=="
              }

              new_expression <-
                paste0("substr(", new_word, ", 1L, ", operand_length, "L) ",
                       operator, " '", operand, "'")
            } else if (word_index - 2L >= 1L &&
                       words[[word_index - 1L]] %in% relational_operators) {
              operator <- words[[word_index - 1L]]
              operand <- toupper(words[[word_index - 2L]])
              operand_length <- nchar(operand)
              is_form2 <- TRUE

              if (operator == "=") {
                operator <- "=="
              }

              new_expression <-
                paste0("'", operand, "' ", operator,
                       " substr(", new_word, ", 1L, ", operand_length, "L)")
            }

            if (!is.null(operator)) {
             new_word <- new_expression

              # If form2 then remove previous 2 words from parsed result:

              if (is_form2) {
                result2 <- unlist(strsplit(fixed = TRUE, result, " "))
                result2_length <- length(result2)
                stopifnot(result2_length >= 2L)

                if (result2_length <= 2L) {
                  result <- ""
                } else {
                   result3 <-
                     paste0(result2[1L:(result2_length - 2L)], "",
                            collapse = " ")
                   result <- result3
                }
              }
            }
          }
        } else if (word == "or") {
          new_word <- "||"
        } else if (word == "and") {
          new_word <- "&&"
        } else if (word == "=") {
          new_word <- "=="
        }

        if (is.null(result)) {
          result <- new_word
        } else {
          result <- paste0(result, " ", new_word)
        }
      }
    }
  }

  return(result)
}



# Make a flag function from a boolean condition.
# Example:
# flag_function <-
#  ASNAT_make_flag_function("values",
#                           "values[[6L]] < 5 || values[[7L]] > 90", 6L)

ASNAT_make_flag_function <-
function(function_arguments, condition, return_result) {
  function_text <-
    paste0("flagger", return_result, " <- function(", function_arguments, ") {",
           " if (", condition, ") ", as.integer(return_result), " else 0L }")
  ASNAT_dprint("function_text = %s\n", function_text)
  parsed_result <- try(parse(text = function_text), silent = TRUE)
  result <- NULL

  if (!is.expression(parsed_result)) {
    warning_text <-
      paste0("Syntax error in condition number ", as.character(return_result),
             "\ngenerated function:\n", function_text, "\n")
    ASNAT_warning(warning_text)
  } else {
    result <- eval(parsed_result)
    stopifnot(is.function(result))
  }

  return(result)
}



# Return a vector of flag functions created from boolean conditions.
# Example:
# flag_functions <-
#   ASNAT_make_flag_functions(function_arguments, conditions)

ASNAT_make_flag_functions <- function(function_arguments, conditions) {
  stopifnot(nchar(function_arguments) > 0L)
  stopifnot(length(conditions) > 0L)
  condition_count <- length(conditions)
  result <- rep(NULL, condition_count)

  for (condition_index in 1L:condition_count) {
    condition <- conditions[[condition_index]]

    if (!is.null(condition)) {
      parts <- unlist(strsplit(fixed = TRUE, condition, " "))

      if (length(parts) >= 3L && nchar(condition) >= 3L) {
        flag_function <-
          ASNAT_make_flag_function(function_arguments, condition,
                                   condition_index)
        ASNAT_debug(str, flag_function)

        if (is.null(flag_function)) {
          break
        }

        result[[condition_index]] <- flag_function
      } else {
        warning_text <- paste0("Invalid condition: ", condition, "\n")
        ASNAT_warning(warning_text)
        break
      }
    }
  }

  return(result)
}



# Apply flag function to a vector of values and return updated flagged value.
# Example:
# flagged <-
#   ASNAT_apply_flag_function_and_update_flagged(flag_function, values, flagged)

ASNAT_apply_flag_function_and_update_flagged <-
  function(flag_function, values, flagged) {
  stopifnot(is.function(flag_function))
  stopifnot(ncol(values) > 0L)
  stopifnot(nchar(flagged) > 0L)
  result <- flagged
  flag_function_result <- try(flag_function(values), silent = TRUE)

  if (is.numeric(flag_function_result) && flag_function_result != 0) {
    result <- ASNAT_include_flag(result, flag_function_result)
    ASNAT_dprint("flag_function_result = %d, result = %s\n",
                 flag_function_result, result)
  }

  return(result)
}



# Apply flag_functions to a single row of column values and
# return updated flagged column value (keeps any existing "99" flag values).
# Example:
# flagged <-
#   ASNAT_apply_flag_functions_to_values(values,
#                                        flagged_column_index,
#                                        function_arguments,
#                                        flag_functions)


ASNAT_apply_flag_functions_to_values <-
  function(values, flagged_column_index, function_arguments, flag_functions) {
  stopifnot(ncol(values) > 0L)
  stopifnot(flagged_column_index > 0L)
  stopifnot(nchar(function_arguments) > 0L)
  stopifnot(length(flag_functions) > 0L)

  function_count <- length(flag_functions)
  flagged <- values[[flagged_column_index]]
  has_99 <- length(grep(fixed = TRUE, "99", flagged)) == 1L
  result <- "0"

  for (function_index in 1L:function_count) {
    flag_function <- flag_functions[[function_index]]
    result <-
      ASNAT_apply_flag_function_and_update_flagged(flag_function, values,
                                                   result)
  }

  if (has_99) {

    if (result == "0") {
      result <- "99"
    } else {
      result <- paste0(result, ";99")
    }
  }

  ASNAT_dprint("ASNAT_apply_flag_functions_to_values() result = '%s'\n", result)
  return(result)
}



# Apply flag_functions to data_frame (keeps any existing "99" flag values) and
# return the updated data_frame.
# Example:
# data_frame <-
#   ASNAT_apply_flag_functions_to_data_frame(data_frame, function_arguments,
#                                            flag_functions)

ASNAT_apply_flag_functions_to_data_frame <-
  function(data_frame, function_arguments, flag_functions) {
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) > 2L)
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(ASNAT_flagged_column_index(colnames(data_frame)) > 0L)
  stopifnot(nchar(function_arguments) > 0L)
  stopifnot(length(flag_functions) > 0L)

  flagged_column_index <- ASNAT_flagged_column_index(colnames(data_frame))
  row_count <- nrow(data_frame)

  for (row in 1L:row_count) {
    data_frame[[row, flagged_column_index]] <-
      ASNAT_apply_flag_functions_to_values(data_frame[row, ],
                                           flagged_column_index,
                                           function_arguments,
                                           flag_functions)
  }

  return(data_frame)
}



# Apply user-supplied flag conditions to a data frame.
# Example:
# data_frame <-
#  ASNAT_apply_flagging(c("temperature < 0", "humidity > 90"), data_frame)

ASNAT_apply_flagging <- function(flag_conditions, data_frame) {
  stopifnot(length(flag_conditions) > 0L)
  stopifnot(!is.null(data_frame))
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(ncol(data_frame) > 0L)

  function_arguments <- "values"

  # Remove blank and comment lines:

  uncommented_flag_conditions <-
    flag_conditions[which(grepl("^[ \t]*[$#]", flag_conditions) == FALSE)]
  uncommented_flag_conditions <-
    uncommented_flag_conditions[which(nchar(uncommented_flag_conditions) > 0L)]

  if (length(uncommented_flag_conditions) > 0L) {
    column_names <- colnames(data_frame)
    unitless_column_names <-
      unlist(strsplit(column_names, "(", fixed = TRUE))[c(TRUE, FALSE)]

    # Omit last column (note) from condition variables:

    condition_variables <-
      unitless_column_names[-length(unitless_column_names)]

    r_flag_conditions <-
      mapply(ASNAT_r_syntax_condition, uncommented_flag_conditions,
             MoreArgs = list(condition_variables, function_arguments))

    ASNAT_dprint("r_flag_conditions:\n")
    ASNAT_debug(str, r_flag_conditions)

    flag_functions <-
      ASNAT_make_flag_functions(function_arguments, r_flag_conditions)

    ASNAT_dprint("flag_functions:\n")
    ASNAT_debug(str, flag_functions)

    if (length(flag_functions) == length(r_flag_conditions)) {
      data_frame <-
        ASNAT_apply_flag_functions_to_data_frame(data_frame,
                                                 function_arguments,
                                                 flag_functions)
      ASNAT_dprint("flagged data_frame:\n")
      ASNAT_debug(str, data_frame)
    }
  }

  return(data_frame)
}



# Get vector of data frame column names that are not data variables:

ASNAT_non_variable_column_names <- function() {
  result <-
    c("timestamp(UTC)",
      "longitude(deg)", "latitude(deg)", "elevation(m)",
      "id(-)", "count(-)", "flagged(-)", "note(-)")
  return(result)
}



# Get vector of data frame column names that are data variables:

ASNAT_variable_column_names <- function(column_names) {
  stopifnot(!is.null(column_names))
  stopifnot(length(column_names) > 4L)
  column_count <- length(column_names)
  result <- c(0L)
  count <- 0L
  non_variable_column_names <- ASNAT_non_variable_column_names()

  for (index in 4L:column_count) {
    name <- column_names[[index]]
    lower_name <- tolower(name)

     if (!(lower_name %in% non_variable_column_names) &&
         !grepl("time", lower_name, fixed = TRUE)) {
      count <- count + 1L
      result[count] <- name
    }
  }

  stopifnot(length(result) > 0L)
  return(result)
}



# Get index of last variable column in data frame column names:

ASNAT_last_variable_index <- function(column_names) {
  stopifnot(!is.null(column_names))
  stopifnot(length(column_names) > 4L)
  variable_column_names <- ASNAT_variable_column_names(column_names)
  count <- length(variable_column_names)
  last_variable_column_name <- variable_column_names[count]
  result <- which(column_names == last_variable_column_name)
  stopifnot(result > 0L)
  stopifnot(result <= length(column_names))
  return(result)
}



# Is date valid?

ASNAT_is_valid_date <- function(date) {
  result <-
    class(date) == "Date" &&
    !is.null(date) &&
    !is.na(date) &&
    date >= 0L &&
    date <= Sys.Date() + 1L
  return(result)
}



# Return the file name of a summary:

ASNAT_summary_file_name <-
function(variable, variable2, start_date, end_date, directory, file_format) {
  stopifnot(!is.null(variable))
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  #stopifnot(is.null(variable2) ||
  #          (nchar(variable2) > 0L && variable2 != variable))
  stopifnot(is.null(variable2) || nchar(variable2) > 0L)
  stopifnot(class(start_date) == "Date")
  stopifnot(class(end_date) == "Date")
  stopifnot(start_date <= end_date)
  stopifnot(dir.exists(directory))
  stopifnot(!is.null(file_format))
  stopifnot(file_format == "csv" || file_format == "tsv")

  extra <- ""

  if (!is.null(variable2)) {
    extra <- paste0("_vs_", gsub(fixed = TRUE, ".", "_", variable2))
  }

  result <-
    paste0(directory, "/",
           gsub(fixed = TRUE, ".", "_", variable),
           extra,
           "_summary_",
           start_date, "_to_", end_date,
           ".", file_format)

  return(result)
}



# Return the file name of a comparison:

ASNAT_comparison_file_name <-
function(variable, variable2, start_date, end_date, directory, file_format) {
  stopifnot(!is.null(variable))
  stopifnot(class(variable) == "character")
  stopifnot(nchar(variable) > 0L)
  stopifnot(class(variable2) == "character")
  stopifnot(nchar(variable2) > 0L)
  stopifnot(class(start_date) == "Date")
  stopifnot(class(end_date) == "Date")
  stopifnot(start_date <= end_date)
  stopifnot(dir.exists(directory))
  stopifnot(!is.null(file_format))
  stopifnot(file_format == "csv" || file_format == "tsv")

  result <-
    paste0(directory, "/",
           gsub(fixed = TRUE, ".", "_", variable),
           "_vs_",
           gsub(fixed = TRUE, ".", "_", variable2),
           "_comparison_",
           start_date, "_to_", end_date,
           ".", file_format)

  return(result)
}


# Return a list of width and height in meters of lon-lat bounds:

ASNAT_width_height <- function(west, east, south, north) {
  stopifnot(west >= -180.0)
  stopifnot(west <= 180.0)
  stopifnot(east >= west)
  stopifnot(east <= 180.0)
  stopifnot(south >= -90.0)
  stopifnot(south <= 90.0)
  stopifnot(north >= south)
  stopifnot(north <= 90.0)

  delta_longitude <- east - west
  delta_latitude <- north - south

  # Compute width and height in meters. http://en.wikipedia.org/wiki/Lat-lon

  to_radians <- 0.017453292519943
  meters_per_degree_equator <- 111132.954
  mean_latitude_radians <- (south + north) * 0.5 * to_radians
  mean_latitude_radians_2 <- mean_latitude_radians + mean_latitude_radians
  mean_latitude_radians_4 <- mean_latitude_radians_2 + mean_latitude_radians_2

  meters_per_degree_longitude <-
    meters_per_degree_equator * cos(mean_latitude_radians)
  meters_per_degree_latitude <-
    meters_per_degree_equator - 559.822 * cos(mean_latitude_radians_2) +
    1.175 * cos(mean_latitude_radians_4)
  delta_longitude_meters <- delta_longitude * meters_per_degree_longitude
  delta_latitude_meters <- delta_latitude * meters_per_degree_latitude
  width_meters <- delta_longitude_meters
  height_meters <- delta_latitude_meters
  result <- list(width = width_meters, height = height_meters)
  stopifnot(result$width >= 0.0)
  stopifnot(result$height >= 0.0)
  return(result)
}



# Return a pair of the nearest site_id and meters distance to a given point:

ASNAT_nearest_site <-
function(longitude, latitude, longitudes, latitudes, site_ids) {
  stopifnot(longitude >= -180.0)
  stopifnot(longitude <= 180.0)
  stopifnot(latitude >= -90.0)
  stopifnot(latitude <= 90.0)
  stopifnot(length(longitudes) >= 1L)
  stopifnot(length(latitudes) == length(longitudes))
  stopifnot(length(site_ids) == length(longitudes))
  stopifnot(longitudes[[1L]] >= -180.0)
  stopifnot(longitudes[[1L]] <= 180.0)
  stopifnot(latitudes[[1L]] >= -90.0)
  stopifnot(latitudes[[1L]] <= 90.0)
  ASNAT_check(min(longitudes) >= -180.0 && max(longitudes) <= 180.0)
  ASNAT_check(min(latitudes) >= -90.0 && max(latitudes) <= 90.0)
  nearest_index <- 1L
  nearest_distance_squared <- 1e30

  for (index in seq_along(site_ids)) {
    site_longitude <- longitudes[[index]]
    longitude_distance <- longitude - site_longitude

    if (longitude_distance < 0.0) {
      longitude_distance <- -longitude_distance
    }

    if (longitude_distance < nearest_distance_squared) {
      site_latitude <- latitudes[[index]]
      latitude_distance <- latitude - site_latitude

      if (latitude_distance < 0.0) {
        latitude_distance <- -latitude_distance
      }

      if (latitude_distance < nearest_distance_squared) {
        distance_squared <-
          longitude_distance * longitude_distance +
          latitude_distance * latitude_distance

        if (distance_squared < nearest_distance_squared) {
          nearest_distance_squared <- distance_squared
          nearest_index <- index
        }
      }
    }
  }

  nearest_site_id <- site_ids[[nearest_index]]
  nearest_longitude <- longitudes[[nearest_index]]
  nearest_latitude <- latitudes[[nearest_index]]

  # Compute distance in meters. http://en.wikipedia.org/wiki/Lat-lon

  to_radians <- 0.017453292519943
  meters_per_degree_equator <- 111132.954
  mean_latitude_radians <- (latitude + nearest_latitude) * 0.5 * to_radians
  mean_latitude_radians_2 <- mean_latitude_radians + mean_latitude_radians
  mean_latitude_radians_4 <- mean_latitude_radians_2 + mean_latitude_radians_2

  meters_per_degree_longitude <-
    meters_per_degree_equator * cos(mean_latitude_radians)
  meters_per_degree_latitude <-
    meters_per_degree_equator - 559.822 * cos(mean_latitude_radians_2) +
    1.175 * cos(mean_latitude_radians_4)

  delta_longitude <- longitude - nearest_longitude
  delta_latitude <- latitude - nearest_latitude

  delta_longitude_meters <- delta_longitude * meters_per_degree_longitude
  delta_latitude_meters <- delta_latitude * meters_per_degree_latitude
  delta_longitude_meters_squared <-
    delta_longitude_meters * delta_longitude_meters
  delta_latitude_meters_squared <-
    delta_latitude_meters * delta_latitude_meters

  nearest_distance_meters <-
    sqrt(delta_longitude_meters_squared + delta_latitude_meters_squared)

  result <- list(id = nearest_site_id, distance = nearest_distance_meters)
  return(result)
}



# Distance in meters between (longitude1, latitude1) and (longitude2, latitude2)

ASNAT_distance_meters <-
function(longitude1, latitude1, longitude2, latitude2) {
  stopifnot(longitude1 >= -180.0)
  stopifnot(longitude1 <= 180.0)
  stopifnot(latitude1 >= -90.0)
  stopifnot(latitude1 <= 90.0)
  stopifnot(longitude2 >= -180.0)
  stopifnot(longitude2 <= 180.0)
  stopifnot(latitude2 >= -90.0)
  stopifnot(latitude2 <= 90.0)

  to_radians <- 0.017453292519943
  meters_per_degree_equator <- 111132.954
  result <- FALSE

  # Compute distance in meters.
  # http://en.wikipedia.org/wiki/Lat-lon

  mean_latitude_radians <- (latitude1 + latitude2) * 0.5 * to_radians

  meters_per_degree_longitude <-
    meters_per_degree_equator * cos(mean_latitude_radians)
  delta_longitude <- longitude1 - longitude2

  if (delta_longitude < 0.0) {
    delta_longitude <- -delta_longitude
  }

  delta_longitude_meters <- delta_longitude * meters_per_degree_longitude
  mean_latitude_radians_2 <- mean_latitude_radians + mean_latitude_radians
  mean_latitude_radians_4 <- mean_latitude_radians_2 + mean_latitude_radians_2
  meters_per_degree_latitude <-
    meters_per_degree_equator -
    559.822 * cos(mean_latitude_radians_2) +
    1.175 * cos(mean_latitude_radians_4)
  delta_latitude <- latitude1 - latitude2

  if (delta_latitude < 0.0) {
    delta_latitude <- -delta_latitude
  }

  delta_latitude_meters <- delta_latitude * meters_per_degree_latitude
  delta_longitude_meters_squared <-
    delta_longitude_meters * delta_longitude_meters
  delta_latitude_meters_squared <-
    delta_latitude_meters * delta_latitude_meters
  distance_meters_squared <-
    delta_longitude_meters_squared + delta_latitude_meters_squared
  result <- sqrt(distance_meters_squared)
  return(result)
}



# Is (longitude1, latitude1) within delta_meters of (longitude2, latitude2)?

ASNAT_is_nearby_point <-
function(delta_meters, longitude1, latitude1, longitude2, latitude2) {
  stopifnot(delta_meters >= 0.0)
  stopifnot(longitude1 >= -180.0)
  stopifnot(longitude1 <= 180.0)
  stopifnot(latitude1 >= -90.0)
  stopifnot(latitude1 <= 90.0)
  stopifnot(longitude2 >= -180.0)
  stopifnot(longitude2 <= 180.0)
  stopifnot(latitude2 >= -90.0)
  stopifnot(latitude2 <= 90.0)

  to_radians <- 0.017453292519943
  meters_per_degree_equator <- 111132.954
  delta_meters_squared <- delta_meters * delta_meters
  result <- FALSE

  # Compute distance in meters.
  # http://en.wikipedia.org/wiki/Lat-lon

  mean_latitude_radians <- (latitude1 + latitude2) * 0.5 * to_radians

  meters_per_degree_longitude <-
    meters_per_degree_equator * cos(mean_latitude_radians)
  delta_longitude <- longitude1 - longitude2

  if (delta_longitude < 0.0) {
    delta_longitude <- -delta_longitude
  }

  delta_longitude_meters <- delta_longitude * meters_per_degree_longitude

  if (delta_longitude_meters <= delta_meters) {
    mean_latitude_radians_2 <- mean_latitude_radians + mean_latitude_radians
    mean_latitude_radians_4 <- mean_latitude_radians_2 + mean_latitude_radians_2
    meters_per_degree_latitude <-
      meters_per_degree_equator -
      559.822 * cos(mean_latitude_radians_2) +
      1.175 * cos(mean_latitude_radians_4)
    delta_latitude <- latitude1 - latitude2

    if (delta_latitude < 0.0) {
      delta_latitude <- -delta_latitude
    }

    delta_latitude_meters <- delta_latitude * meters_per_degree_latitude

    if (delta_latitude_meters <= delta_meters) {
      delta_longitude_meters_squared <-
        delta_longitude_meters * delta_longitude_meters
      delta_latitude_meters_squared <-
        delta_latitude_meters * delta_latitude_meters
      distance_meters_squared <-
        delta_longitude_meters_squared + delta_latitude_meters_squared
      result <- distance_meters_squared <= delta_meters_squared
    }
  }

  return(result)
}



# Write a summary of a data_frame (and data_frame2 if not null) to output_file:

ASNAT_write_summary <-
function(data_frame, measure_column, data_frame2, timesteps, delta_meters,
         delimiter, output_file) {
  timer <- ASNAT_start_timer()
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) >= 6L)
  stopifnot(measure_column >= 4L)
  stopifnot(measure_column <= ncol(data_frame) - 1L)
  stopifnot(is.null(data_frame2) ||
            (class(data_frame2) == "data.frame" && ncol(data_frame2) >= 6L))
  stopifnot(timesteps >= 1L)
  stopifnot(delta_meters >= 0)
  stopifnot(delta_meters <= 10000)
  stopifnot(nchar(delimiter) == 1L)
  stopifnot(!is.null(output_file))

  compare <- !is.null(data_frame2)
  column_names <- colnames(data_frame)
  site_column <- ASNAT_site_column_index(column_names)
  site_column2 <- 0L

  if (compare) {
    column_names2 <- colnames(data_frame2)
    site_column2 <- ASNAT_site_column_index(column_names2)
  }

  # Create sorted vector of unique site_ids.
  # Note: a vector (unlike a data.frame) must be sorted for unique to work.

  site_ids <- data_frame[[site_column]]
  unique_site_ids <- unique(sort.int(site_ids))

  # If comparing to data_frame2 then create sorted vectors of unique
  # site_ids2, longitudes2, latitudes2 for data_frame2.
  # This shortcut works if sites do not move over time.

  site_ids2 <- NULL
  longitudes2 <- NULL
  latitudes2 <- NULL

  if (compare) {
    subset_data_frame2 <- data_frame2[, c(site_column2, 2L, 3L)]
    subset_data_frame2 <- unique(subset_data_frame2)
    site_ids2 <- subset_data_frame2[[1L]]
    longitudes2 <- subset_data_frame2[[2L]]
    latitudes2 <- subset_data_frame2[[3L]]
  }

  for (site_id in unique_site_ids) {

    # NOTE: PROFILE HOTSPOT:
    # The cummulative runtime of the calls to the built-in function 'which()'
    # on the line below is 90% of the total runtime of this function.

    site_rows <- which(site_ids == site_id)

    site_data_frame <- data_frame[site_rows, ]

    nearest_other_site_id <- 0L
    nearest_other_site_distance <- 0.0

    if (compare) {
      longitude <- (site_data_frame[[2L]])[[1L]]
      latitude <- (site_data_frame[[3L]])[[1L]]

      nearest_other_site <- NULL

      if (ASNAT_use_cpp_functions) {
        nearest_other_site <-
          ASNAT_nearest_site_cpp(longitude, latitude,
                                 longitudes2, latitudes2, site_ids2)
      } else {
        nearest_other_site <-
          ASNAT_nearest_site(longitude, latitude,
                             longitudes2, latitudes2, site_ids2)
      }

      nearest_other_site_id <- nearest_other_site$id
      nearest_other_site_distance <- nearest_other_site$distance
    }

    if (nearest_other_site_distance <= delta_meters) {
      timestamps <- site_data_frame[[1L]]
      reported_count <- length(timestamps)
      timestamp_first <- timestamps[[1L]]
      timestamp_last <- timestamps[[reported_count]]
      missing_percent <- (1.0 - reported_count / timesteps) * 100.0
      measures <- site_data_frame[[measure_column]]
      minimum <- min(measures, na.rm = TRUE)
      maximum <- max(measures, na.rm = TRUE)
      mean_value <- mean(measures, na.rm = TRUE)
      percentiles <-
        stats::quantile(measures, na.rm = TRUE, probs = c(.25, .5, .75))
      p25 <- percentiles[[1L]]
      median_value <- percentiles[[2L]]
      p75 <- percentiles[[3L]]

      cat(sep = "", file = output_file, append = TRUE,
          site_id, delimiter,
          timestamp_first, delimiter,
          timestamp_last, delimiter,
          reported_count, delimiter,
          round(missing_percent), delimiter,
          round(mean_value, 1L), delimiter,
          round(minimum, 1L), delimiter,
          round(p25, 1L), delimiter,
          round(median_value, 1L), delimiter,
          round(p75, 1L), delimiter,
          round(maximum, 1L))

      if (compare) {
        cat(sep = "", file = output_file, append = TRUE,
            delimiter, nearest_other_site_id,
            delimiter, round(nearest_other_site_distance))
      }

      cat(sep = "", file = output_file, append = TRUE, "\n")
    }
  }

  ASNAT_elapsed_timer("ASNAT_write_summary:", timer)
}



# Return a pair of arrays of indices into two data_frames of matched points
# i.e., points that are at the same time and within delta_meters apart:

ASNAT_compare_datasets <-
function(data_frame_x, data_frame_y, delta_meters, is_hourly) {
  timer <- ASNAT_start_timer()
  stopifnot(class(data_frame_x) == "data.frame")
  stopifnot(ncol(data_frame_x) >= 6L)
  stopifnot(nrow(data_frame_x) >= 1L)
  stopifnot(class(data_frame_y) == "data.frame")
  stopifnot(ncol(data_frame_y) >= 6L)
  stopifnot(nrow(data_frame_y) >= 1L)
  stopifnot(!is.null(delta_meters))
  stopifnot(delta_meters >= 0)
  stopifnot(delta_meters <= 10000)
  stopifnot(class(is_hourly) == "logical")

  result <- NULL

  if (ASNAT_use_cpp_functions) {
    timer_cpp <- ASNAT_start_timer()
    result <-
      ASNAT_compare_datasets_cpp(data_frame_x, data_frame_y,
                                 delta_meters, is_hourly)
    ASNAT_elapsed_timer("ASNAT_compare_datasets_cpp:", timer_cpp)
  } else {

    # Note: Type 'R list' is not a 'linked-list' but rather a kind of vector
    # (array) so appending runs out of space requiring copying - not efficient!

    result_x <- list()
    result_y <- list()
    result_count <- 0L

    rows_x <- nrow(data_frame_x)
    rows_y <- nrow(data_frame_y)
    row_start_y <- 1L

    timestamp_length <- if (is_hourly) 13L else 10L

    for (row_x in 1L:rows_x) {
      timestamp_x <- data_frame_x[[row_x, 1L]]
      timestamp_x <- substr(timestamp_x, 1L, timestamp_length)

      longitude_x <- data_frame_x[[row_x, 2L]]
      latitude_x <- data_frame_x[[row_x, 3L]]
      timestamp_matches <- 0L

      for (row_y in row_start_y:rows_y) {
        timestamp_y <- data_frame_y[[row_y, 1L]]
        timestamp_y <- substr(timestamp_y, 1L, timestamp_length)

        # Note: data_frame timestamps are (assumed to be) sorted
        # so the break logic below can be used to shorten this inner loop.

        if (timestamp_y > timestamp_x) {
          row_start_y <- row_y - timestamp_matches
          break
        }

        if (timestamp_y == timestamp_x) {
          longitude_y <- data_frame_y[[row_y, 2L]]
          latitude_y <- data_frame_y[[row_y, 3L]]
          is_neighbor <-
            ASNAT_is_nearby_point(delta_meters,
                                  longitude_x, latitude_x,
                                  longitude_y, latitude_y)

          timestamp_matches <- timestamp_matches + 1L

          if (is_neighbor) {
            result_count <- result_count + 1L
            result_x[[result_count]] <- row_x
            result_y[[result_count]] <- row_y
          }
        }
      }
    }

    result_x <- unlist(result_x)
    result_y <- unlist(result_y)
    stopifnot(length(result_x) == length(result_y))
    result <- list(x = result_x, y = result_y)
  }

  ASNAT_dprint("result:\n")
  ASNAT_debug(str, result)
  ASNAT_elapsed_timer("ASNAT_compare_datasets:", timer)
  return(result)
}



# Validate data frame read from a webservice or file valid and return it
# (with possible edits).
# If not valid then print a failure message to the console and return NULL.
# Validated input data frames match:
# timestamp(UTC)	longitude(deg)	latitude(deg)	id(-)	...	note(-)

ASNAT_validate_input_data_frame <- function(data_frame) {
  failure <- NULL

  if (class(data_frame) != "data.frame") {
    failure <- "failed to read data.\n"
  } else if (nrow(data_frame) < 1L) {
    failure <- "no data rows.\n"
  } else if (ncol(data_frame) < 6L) {
    failure <- "data does not have at least 6 columns.\n"
  } else {

    # Check/edit column names:

    column_names <- colnames(data_frame)
    last_column <- length(column_names)
    column_names[1L:4L] <- tolower(column_names[1L:4L])
    column_names[last_column] <- tolower(column_names[[last_column]])
    elevation_column <- 0L
    id_column <- 4L

    if (column_names[[1L]] == "timestamp(utc)") {
      column_names[1L] <- "timestamp(UTC)"
    } else {
      failure <- "column 1 is not 'timestamp(UTC)'.\n"
    }

    if (is.null(failure)) {

      if (column_names[[2L]] != "longitude(deg)") {
        failure <- "column 2 is not 'longitude(deg)'.\n"
      } else if (column_names[[3L]] != "latitude(deg)") {
        failure <- "column 3 is not 'latitude(deg)'.\n"
      } else if (column_names[[4L]] == "elevation(m)") {
        elevation_column <- 4L
        id_column <- 5L
      }

      if (is.null(failure)) {

        if (column_names[[id_column]] != "id(-)") {
          failure <- sprintf("data column %d is not 'id(-)'.\n", id_column)
        } else if (column_names[[last_column]] != "note(-)") {
          failure <- "last column is not 'note(-)'.\n"
        } else {

          # Ensure column names are of the form variable(units) without spaces:
          # E.g., "kg m^-2" becomes "kg.m^-2".

          column_names <- gsub(fixed = TRUE, " ", ".", column_names)

          for (column_name in column_names) {
            ok <-
              endsWith(column_name, ")") &&
              ! endsWith(column_name, "()") &&
              ! startsWith(column_name, "(")
              length(unlist(strsplit(column_name, "(", fixed = TRUE))) == 2L
              length(unlist(strsplit(column_name, ")", fixed = TRUE))) == 2L

            if (!ok) {
              failure <- "column names must end with non-empty '(units)'."
              break
            }
          }
        }

        if (is.null(failure)) {
          colnames(data_frame) <- column_names
          rownames(data_frame) <- NULL

          # Check that required metadata columns are the right type and
          # contain valid numeric values:

          if (anyNA(data_frame[[1L]]) || !is.character(data_frame[[1L]])) {
            failure <- "timestamp cannot have invalid values.\n"
          } else if (anyNA(data_frame[[2L]]) || !is.numeric(data_frame[[2L]])) {
            failure <- "longitude cannot have invalid values.\n"
          } else if (anyNA(data_frame[[3L]]) || !is.numeric(data_frame[[3L]])) {
            failure <- "latitude cannot have invalid values.\n"
          } else if (elevation_column > 0L &&
                     (anyNA(data_frame[[elevation_column]]) ||
                      !is.numeric(data_frame[[elevation_column]]))) {
            failure <- "elevation cannot have invalid values.\n"
          } else if (anyNA(data_frame[[id_column]]) ||
                     !is.numeric(data_frame[[id_column]])) {
            failure <- "'id' cannot have invalid values.\n"
          } else if (anyNA(data_frame[[last_column]])) {
            failure <- "'note' cannot have invalid values.\n"
          } else if (min(data_frame[[2L]]) < -180.0) {
            failure <- "longitude cannot be < -180.\n"
          } else if (max(data_frame[[2L]]) > 180.0) {
            failure <- "longitude cannot be > 180.\n"
          } else if (min(data_frame[[3L]]) < -90.0) {
            failure <- "latitude cannot be < -90.\n"
          } else if (max(data_frame[[3L]]) > 90.0) {
            failure <- "latitude cannot be > 90.\n"
          } else if (min(data_frame[[id_column]]) < 1) {
            failure <- "id cannot be < 1.\n"
          }

          if (is.null(failure)) {
            data_frame[id_column] <- trunc(data_frame[[id_column]])
            data_frame[id_column] <- as.integer(data_frame[[id_column]])

            # Check if coercion of id to integer introduced NA values:

            if (anyNA(data_frame[[id_column]])) {
              failure <- "id cannot be > 2147483647.\n"
            } else {

              # Check flagged:

              flagged_column <- which(column_names == "flagged(-)")

              if (length(flagged_column) > 0L &&
                  anyNA(data_frame[[flagged_column]])) {
                failure <- "flagged(-) cannot have invalid values.\n"
              } else {

                # Check timestamps are YYYY-MM-DDTHH:MM:SS-0000 ISO-8601 format
                # and sorted:

                timestamp_pattern <-
                  "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}-0000"
                timestamps <- data_frame[[1L]]
                previous_timestamp <- timestamps[[1L]]

                for (timestamp in timestamps) {
                  ok <- nchar(timestamp) == 24L

                  if (ok) {
                    ok <- length(grep(timestamp_pattern, timestamp)) == 1L

                    if (ok) {

                      # Check date of timestamp:

                      yyyy <- as.integer(substr(timestamp, 1L, 4L))
                      mm <- as.integer(substr(timestamp, 6L, 7L))
                      dd <- as.integer(substr(timestamp, 9L, 10L))

                      ok <- yyyy >= 1900L && yyyy < 3000L &&
                            mm >= 1L && mm <= 12L &&
                            dd >= 1L && dd <= 31L

                      if (ok) {

                        if (dd > 28L) {

                          if (mm == 9L || mm == 4L || mm == 6L || mm == 11L) {
                            ok <- dd <= 30L
                          } else if (mm == 2L) {
                            is_leap_year <-
                              yyyy %% 4L == 0L &&
                              ! (yyyy %% 100L == 0L && yyyy %% 400L != 0L)

                            if (is_leap_year) {
                              ok <- dd <= 29L
                            } else {
                              ok <- dd <= 28L
                            }
                          }
                        }

                        if (ok) {

                          # Check time of timestamp:

                          hh <- as.integer(substr(timestamp, 12L, 13L))
                          mm <- as.integer(substr(timestamp, 15L, 16L))
                          ss <- as.integer(substr(timestamp, 18L, 19L))

                          ok <- hh >= 0L && hh <= 23L &&
                                mm >= 0L && mm <= 59L &&
                                ss >= 0L && ss <= 59L

                          if (ok) {

                            # Check that timestamps are sorted:

                            ok <- timestamp >= previous_timestamp
                          }
                        }
                      }
                    }
                  }

                  if (!ok) {
                    failure <- "timestamps must be sorted ascending.\n"
                    break
                  }

                  previous_timestamp <- timestamp
                } # End loop on timestamps

                if (!ok) {
                  failure <-
                    paste0("timestamps must be valid YYYY-MM-DDTHH:MM:SS-0000",
                           " and in increasing order.\n")
                }
              }
            }
          }
        }
      }
    }
  }

  result <- NULL

  if (is.null(failure)) {
    result <- data_frame
  } else {
    failure <- paste0("Invalid input data. Problem: ", failure)
    ASNAT_warning(failure)
  }

  return(result)
}



# Read a standard-format file into a data_frame and return it or NULL if failed:
# data_frame <-
#  ASNAT_read_standard_file("AQS_pm25_2022-06-01_to_2022-06-01.tsv")

ASNAT_read_standard_file <- function(file_name) {
  #ASNAT_dprint("In ASNAT_load_standard_file().\n")
  stopifnot(!is.null(file_name))
  stopifnot(class(file_name) == "character")
  stopifnot(nchar(file_name) > 0L)
  stopifnot(file.exists(file_name))
  result <- NULL
  failure <- NULL

  # Read first two lines to determine column delimiter, column count and
  # non-empty data rows:

  first_two_lines <- try(silent = TRUE, readLines(file_name, 2L))

  if (class(first_two_lines) == "character" && length(first_two_lines) == 2L) {
    header_line <- first_two_lines[[1L]]
    column_delimiter <- NULL

    if (length(grep(fixed = TRUE, "\t", header_line)) > 0L) {
      column_delimiter <- "\t"
    } else if (length(grep(fixed = TRUE, ",", header_line)) > 0L) {
      column_delimiter <- ","
    } else {
      failure <-
        paste0("Invalid (non-tab, non-comma) column delimiters in input file '",
               file_name, "'\n.")
    }

    if (!is.null(column_delimiter)) {
      column_names <- unlist(strsplit(header_line, column_delimiter))
      column_count <- length(column_names)

      if (column_count < 6L) {
        failure <-
          paste0("Not enough columns in input file '", file_name, "'\n.")
      } else {
        result <-
          try(silent = TRUE,
              read.delim(sep = column_delimiter, check.names = FALSE,
                         strip.white = TRUE, file_name,
                         na.strings = ASNAT_na_strings,
                         stringsAsFactors = FALSE))

        result <- ASNAT_validate_input_data_frame(result)
      }
    }
  } else {
    failure <- paste0("Invalid input file '", file_name, "'\n.")
  }

  if (!is.null(failure)) {
    ASNAT_warning(failure)
  }

  return(result)
}



# PURPOSE: pm25_corrected_piecewise - Compute pm25_corrected from pm25_atm_a/b
#         channel measures and relative humidity with filtering by
#         absolute difference and ratio in channel measures.
# INPUTS: pm25_atm_a    Channel A PM2.5 measure (ug/m3).
#         pm25_atm_b    Channel B PM2.5 measure (ug/m3).
#         humidity      Relative humidity measure (%).
# RETURNS: pm25_corrected or returns NA if invalid values are
#         given or generated.
# NOTES: This version is used for data with channel A & B PM25 both available.
#        Formula inputs (atm) revised on 2022-01-11 per Karoline Barkjohn email.

ASNAT_pm25_corrected_piecewise <- function(pm25_atm_a, pm25_atm_b, humidity) {

  # If there is a missing value input then return NA:

  if (is.na(pm25_atm_a) || is.na(pm25_atm_b) || is.na(humidity)) {
    return(NA)
  }

  allow_negative_pm25_corrected <- TRUE

  #  Maximum acceptable absolute difference of channel measures (ug/m3):

  maximum_channel_difference <- 5.0

  # Maximum acceptable absolute ratio of channel measures [0.0, 1.0]:

  maximum_channel_ratio <- 0.7

  pm25_corrected <- NA

  # Formula constants:

  MINIMUM_VALID_HUMIDITY <- 0.0
  MAXIMUM_VALID_HUMIDITY <- 100.0

  MINIMUM_VALID_PM <- 0.0
  MAXIMUM_VALID_PM <- 1e6

  PM25_LIMIT1           <- 30.0
  PM25_LIMIT2           <- 50.0
  PM25_LIMIT3           <- 210.0
  PM25_LIMIT4           <- 260.0

  ADJUSTED_PM25_SCALE1  <-  0.05
  ADJUSTED_PM25_SCALE2  <-  0.02
  ADJUSTED_PM25_OFFSET1 <- -1.5
  ADJUSTED_PM25_OFFSET2 <- -4.2

  PM25_COEFFICIENT1     <- 0.524
  PM25_COEFFICIENT2     <- 0.786
  PM25_COEFFICIENT3     <- 0.69
  PM25_COEFFICIENT4     <- 0.000884

  HUMIDITY_COEFFICIENT  <- -0.0862

  OFFSET1               <- 5.75
  OFFSET2               <- 2.966

  # Check if humidity is usable:

  usable <-
    humidity >= MINIMUM_VALID_HUMIDITY && humidity <= MAXIMUM_VALID_HUMIDITY

  if (usable) {

    # Check if a and b channel measures are usable:

    usable <-
      pm25_atm_a >= MINIMUM_VALID_PM && pm25_atm_a <= MAXIMUM_VALID_PM &&
      pm25_atm_b >= MINIMUM_VALID_PM && pm25_atm_b <= MAXIMUM_VALID_PM

    if (usable) {
      the_sum <- pm25_atm_a + pm25_atm_b

      # Check if a and b channel measures differ by <= given ug/m3:

      absolute_difference <-
        if (pm25_atm_a > pm25_atm_b)
          pm25_atm_a - pm25_atm_b
        else pm25_atm_b - pm25_atm_a

      usable <- absolute_difference <= maximum_channel_difference

      if (!usable) {

        # Else check if a and b are <= given normalized percent difference:

        absolute_ratio <- (absolute_difference + absolute_difference) / the_sum
        usable <- absolute_ratio <= maximum_channel_ratio
      }

      if (usable) {

        # Compute pm25_corrected formula:

        mean_pm25 <- 0.5 * the_sum

        if (mean_pm25 < PM25_LIMIT1) {
          pm25_corrected <-
            PM25_COEFFICIENT1 * mean_pm25 +
            HUMIDITY_COEFFICIENT * humidity +
            OFFSET1
        } else if (mean_pm25 < PM25_LIMIT2) {
          adjusted_mean_pm25 <-
            mean_pm25 * ADJUSTED_PM25_SCALE1 + ADJUSTED_PM25_OFFSET1
          one_minus_adjusted_mean_pm25 <- 1.0 - adjusted_mean_pm25
          pm25_corrected <-
            (PM25_COEFFICIENT2 * adjusted_mean_pm25 +
             PM25_COEFFICIENT1 * one_minus_adjusted_mean_pm25) * mean_pm25 +
            HUMIDITY_COEFFICIENT * humidity +
            OFFSET1
        } else if (mean_pm25 < PM25_LIMIT3) {
          pm25_corrected <-
            PM25_COEFFICIENT2 * mean_pm25 +
            HUMIDITY_COEFFICIENT * humidity +
            OFFSET1
        } else if (mean_pm25 < PM25_LIMIT4) {
          adjusted_mean_pm25 <-
            mean_pm25 * ADJUSTED_PM25_SCALE2 + ADJUSTED_PM25_OFFSET2
          one_minus_adjusted_mean_pm25 <- 1.0 - adjusted_mean_pm25
          term1 <-
            (PM25_COEFFICIENT3 * adjusted_mean_pm25 +
             PM25_COEFFICIENT2 * one_minus_adjusted_mean_pm25) * mean_pm25
          term2 <-
            HUMIDITY_COEFFICIENT * humidity * one_minus_adjusted_mean_pm25
          term3 <- OFFSET2 * adjusted_mean_pm25
          term4 <- OFFSET1 * one_minus_adjusted_mean_pm25
          term5 <-
            PM25_COEFFICIENT4 * mean_pm25 * mean_pm25 * adjusted_mean_pm25
          pm25_corrected <- term1 + term2 + term3 + term4 + term5
        } else {
          pm25_corrected <-
            PM25_COEFFICIENT3 * mean_pm25 +
            PM25_COEFFICIENT4 * mean_pm25 * mean_pm25 +
            OFFSET2
        }

        # Check to filter invalid negative or NaN results:

        if (! (allow_negative_pm25_corrected || pm25_corrected >= 0.0)) {
          pm25_corrected <- NA
        }
      }
    }
  }

  return(pm25_corrected)
}



# Aggregate data_frame numeric columns:

ASNAT_aggregate <- function(data_frame, aggregate) {
  timer <- ASNAT_start_timer()
  stopifnot(!is.null(data_frame))
  stopifnot(ncol(data_frame) >= 6L)
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(aggregate == "hourly" || aggregate == "daily")

  timestamp_length <- 13L
  timestamp_suffix <- ":00:00-0000"

  if (aggregate == "daily") {
    timestamp_length <- 10L
    timestamp_suffix <- "T00:00:00-0000"
  }

  result <- NULL
  numeric_columns <- unlist(lapply(data_frame, is.numeric))

  ids <- unique(as.integer(data_frame[[4L]]))

  for (id in ids) {
    matched_id_rows <- which(data_frame[[4L]] == id)
    matched_id_data_frame <- data_frame[matched_id_rows, ]
    aggregate_row_values <- matched_id_data_frame[1L, ]
    timestamps <- matched_id_data_frame[[1L]]
    timestamps <- substr(timestamps, 1L, timestamp_length)
    timestamps <- unique(timestamps)

    for (timestamp in timestamps) {
      aggregate_row_values[1L, 1L] <- paste0(timestamp, timestamp_suffix)
      timestamp_rows <-
        which(startsWith(matched_id_data_frame[[1L]], timestamp))
      matched_timestamp_data_frame <- matched_id_data_frame[timestamp_rows, ]
      numeric_data_frame <- matched_timestamp_data_frame[, numeric_columns]

      # Must convert any NaN values to NA which are ignored by colMeans():

      numeric_data_frame[is.na(numeric_data_frame)] <- NA

      means <- colMeans(numeric_data_frame, na.rm = TRUE)
      #means[is.nan(means) | is.na(means)] <- NA

      # Get the number of non-NA values per column (includes header line):

      counts <- colSums(!is.na(numeric_data_frame))
      count <- as.integer(counts[length(counts)] - 1L)

      # Update aggregate_row_values with the means:

      aggregate_row_values[1L, numeric_columns] <- means

      # Insert count column after id(-):

      count_data_frame <- data.frame(count)
      colnames(count_data_frame) <- c("count(-)")

      aggregate_row_values_with_count <-
        cbind(aggregate_row_values[1L, 1L:4L, drop = FALSE],
              count_data_frame,
              aggregate_row_values[1L, 5L:ncol(aggregate_row_values),
                                   drop = FALSE])

      # Overwrite integer columns of aggregate_row_values:

      aggregate_row_values_with_count[1L, 4L] <- as.integer(id)

      # Append to result:

      if (is.null(result)) {
        result <- aggregate_row_values_with_count
      } else {
        result <- rbind(result, aggregate_row_values_with_count)
      }
    }
  }

  # Overwrite integer columns of result:

  result[, 4L] <- as.integer(result[[4L]])

  ASNAT_elapsed_timer("ASNAT_aggregate:", timer)
  return(result)
}



# Read data from a set of purple air sensor files:
# https://community.purpleair.com/t/sd-card-file-headers/279

ASNAT_import_purple_air <-
function(file_list_data_frame, longitude, latitude) {
  ASNAT_dprint("In ASNAT_import_purple_air()\n")
  timer <- ASNAT_start_timer()
  stopifnot(!is.null(file_list_data_frame))
  stopifnot(class(file_list_data_frame) == "data.frame")
  stopifnot(nrow(file_list_data_frame) > 0L)
  stopifnot(ncol(file_list_data_frame) > 0L)
  stopifnot(length(file_list_data_frame$datapath) > 0L)
  stopifnot(longitude >= -180.0)
  stopifnot(longitude <=  180.0)
  stopifnot(latitude >= -90.0)
  stopifnot(latitude <=  90.0)

  result <- NULL
  file_name <- file_list_data_frame$datapath[[1L]]
  header_line <- try(silent = TRUE, readLines(file_name, 1L))

  if (class(header_line) != "character" ||
      !startsWith(header_line,
                  "UTCDateTime,mac_address,firmware_ver,hardware,")) {
    return(NULL)
  }

  file_index <- 1L

  for (file_name in file_list_data_frame$datapath) {
    short_file_name <- file_list_data_frame$name[[file_index]]
    this_header_line <- try(silent = TRUE, readLines(file_name, 1L))

    if (this_header_line != header_line) {
      next # Skip file that does not match the first file header line.
    }

    data_frame <-
      try(silent = TRUE,
          read.delim(sep = ",", check.names = FALSE, strip.white = TRUE,
                     file_name,
                     na.strings = ASNAT_na_strings,
                     stringsAsFactors = FALSE))

    if (class(data_frame) != "data.frame" || nrow(data_frame) < 1L) {
      next # Skip file that does not yield a usable data frame.
    }

    # Remove uneeded/problematic columns:

    remove_columns <- c("firmware_ver", "hardware", "gas")
    data_frame <-
      subset(data_frame,
             select = !(colnames(data_frame) %in% remove_columns))

    # Convert names and add units to columns:

    column_names <- colnames(data_frame)

    # Convert timestamp strings (column 1) from "2022/06/01T00:00:35z"
    # to ISO-8601 UTC format "2022-06-01T00:00:35-0000":

    timestamp_strings <- data_frame[[1L]]
    substr(timestamp_strings, 5L, 5L) <- "-"
    substr(timestamp_strings, 8L, 8L) <- "-"
    substr(timestamp_strings, 11L, 11L) <- "T"
    substr(timestamp_strings, 20L, 20L) <- "-"
    timestamp_strings <- paste0(timestamp_strings, "0000")
    data_frame[, 1L] <- timestamp_strings

    column_names[1L] <- "timestamp(UTC)"

    # Convert unique mac_address (column 2 "98:cd:ac:10:f7:ba") to integer id.
    # Only use last 7 hex digits to avoid signed 32-bit integer overflow:

    mac_addresses <- data_frame[[2L]]
    mac_addresses2 <- gsub(fixed = TRUE, ":", "", mac_addresses)
    last <- nchar(mac_addresses2)
    first <- max(last - 6L, 1L)
    shortened_mac_addresses <- substr(mac_addresses2, first, last)
    ids <- strtoi(shortened_mac_addresses, base = 16L)
    data_frame[2L] <- ids
    column_names[2L] <- "id(-)"

    for (column in seq_along(column_names)) {
      column_name <- column_names[[column]]

      if (column_name == "current_temp_f") {
        column_names[[column]] <- "temperature(C)"
        data_frame[column] <- (data_frame[column] - 32.0) * (5.0 / 9.0)
      } else if (column_name == "current_dewpoint_f") {
        column_names[[column]] <- "dewpoint(C)"
        data_frame[column] <- (data_frame[column] - 32.0) * (5.0 / 9.0)
      } else if (column_name == "pressure") {
        column_names[[column]] <- "pressure(hPa)"
      } else if (column_name == "current_humidity") {
        column_names[[column]] <- "humidity(%)"
      } else if (column_name == "adc") {
        column_names[[column]] <- "adc(V)"
      } else if (column_name == "mem") {
        column_names[[column]] <- "memory(bytes)"
      } else if (column_name == "rssi") {
        column_names[[column]] <- "rssi(dBm)"
      } else if (column_name == "uptime") {
        column_names[[column]] <- "uptime(s)"
      } else if (startsWith(column_name, "pm2_5")) {
        parts <- unlist(strsplit(column_name, "2_5", fixed = TRUE))
        column_names[[column]] <- paste0("pm25", parts[[2L]], "(ug/m3)")
      } else if (startsWith(column_name, "pm2.5")) {
        parts <- unlist(strsplit(column_name, "2.5", fixed = TRUE))
        column_names[[column]] <- paste0("pm25", parts[[2L]], "(ug/m3)")
      } else if (startsWith(column_name, "pm")) {
        parts <- unlist(strsplit(column_name, "_0", fixed = TRUE))

        if (length(parts) > 0L) {
          column_name <- parts[[1L]]

          if (length(parts) > 1L) {
            column_name <- paste0(column_name, parts[[2L]])
          }

          column_names[[column]] <- paste0(column_name, "(ug/m3)")
        }
      } else if (startsWith(column_name, "p_2_5")) {
        parts <- unlist(strsplit(column_name, "2_5", fixed = TRUE))
        column_names[[column]] <-
          paste0("p_25", parts[[2L]], "(particles/100ml)")
      } else if (startsWith(column_name, "p_")) {
        parts <- unlist(strsplit(column_name, "_0_u", fixed = TRUE))

        if (length(parts) > 0L) {
          column_name <- parts[[1L]]

          if (length(parts) > 1L) {
            column_name <- paste0(column_name, "_u", parts[[2L]])
          }

          column_names[[column]] <- paste0(column_name, "(particles/100ml)")
        }
      }
    } # End loop on columns

    colnames(data_frame) <- column_names

    # Append note column containing short file name and mac_address:

    row_count <- nrow(data_frame)
    notes <- rep(short_file_name, row_count)
    notes <- paste0(notes, ";mac_address=", mac_addresses)
    notes_data_frame <- data.frame(NOTES = notes)
    data_frame <- cbind(data_frame, notes_data_frame)

    if (is.null(result)) {
      result <- data_frame
    } else {
      result <- rbind(result, data_frame)
    }

    file_index <- file_index + 1L
  } # End loop on files

  if (!is.null(result)) {

    # Create a temporary data.frame for timestamp, longitude, latitude:

    row_count <- nrow(result)
    longitudes <- rep(longitude, row_count)
    latitudes <- rep(latitude, row_count)
    extra_data_frame <-
      data.frame(t = result[1L], x = longitudes, y = latitudes)
    colnames(extra_data_frame) <-
      c("timestamp(UTC)", "longitude(deg)", "latitude(deg)")

    # Remove first column of result since it is in extra_data_frame:

    result <- result[-1L]

    # Create result by combining extra_data_frame as the first 3 columns
    # followed by the remaining columns of result:

    result <- cbind(extra_data_frame, result)

    # Compute pm25_corrected as next-to-last-column:

    pm25_atm_a <- NULL
    pm25_atm_b <- NULL
    humidity <- NULL

    column_names <- colnames(result)
    column <- which(column_names == "pm25_atm(ug/m3)")

    if (length(column) == 1L && column[[1L]] > 0L) {
      pm25_atm_a <- result[[column]]
    }

    column <- which(column_names == "pm25_atm_b(ug/m3)")

    if (length(column) == 1L && column[[1L]] > 0L) {
      pm25_atm_b <- result[[column]]
    }

    column <- which(column_names == "humidity(%)")

    if (length(column) == 1L && column[[1L]] > 0L) {
      humidity <- result[[column]]
    } else {

      # If humidity is missing, use value 50% per email from PI K. Barkjohn:

      humidity <- rep(50.0, nrow(result))
    }

    if (!is.null(pm25_atm_a) && !is.null(pm25_atm_b) && !is.null(humidity)) {
      pm25_corrected <-
        mapply(ASNAT_pm25_corrected_piecewise,
               pm25_atm_a, pm25_atm_b, humidity)

      last_column <- ncol(result)
      notes <- result[[last_column]]
      extra_data_frame <- data.frame(p = pm25_corrected, n = notes)
      colnames(extra_data_frame) <- c("pm25_corrected(ug/m3)", "note(-)")

      # Remove last column of result since it is in extra_data_frame:

      result <- result[-last_column]

      # Create result by appending extra_data_frame as the last 2 columns

      result <- cbind(result, extra_data_frame)
    }

    # Validate final result. If invalid then result will be NULL:

    result <- ASNAT_validate_input_data_frame(result)
  } # end if result is not null.

  ASNAT_elapsed_timer("ASNAT_import_purple_air:", timer)
  return(result)
}



# Read and aggregate data from a set of known-format sensor files:
# data_frame <- ASNAT_import(file_list_data_frame, TRUE, -74.05, 40.72)

ASNAT_import <- function(file_list_data_frame, aggregate, longitude, latitude) {
  ASNAT_dprint("In ASNAT_import()\n")
  timer <- ASNAT_start_timer()
  stopifnot(!is.null(file_list_data_frame))
  stopifnot(class(file_list_data_frame) == "data.frame")
  stopifnot(nrow(file_list_data_frame) > 0L)
  stopifnot(ncol(file_list_data_frame) > 0L)
  stopifnot(length(file_list_data_frame$datapath) > 0L)
  stopifnot(aggregate == "none" ||
            aggregate == "hourly" ||
            aggregate == "daily")
  stopifnot(longitude >= -180.0)
  stopifnot(longitude <=  180.0)
  stopifnot(latitude >= -90.0)
  stopifnot(latitude <=  90.0)

  result <- NULL
  file_name <- file_list_data_frame$datapath[[1L]]
  header_line <- try(silent = TRUE, readLines(file_name, 1L))

  is_purple_air <-
    startsWith(header_line,
               "UTCDateTime,mac_address,firmware_ver,hardware,current_temp_f,")

  if (is_purple_air) {
    result <- ASNAT_import_purple_air(file_list_data_frame, longitude, latitude)
  }

  if (!is.null(result)) {

    if (aggregate != "none") {

      # Sort data frame by site (column 4) then time (column 1):

      result <- result[order(result[[4L]], result[[1L]]), ]
      result <- ASNAT_aggregate(result, aggregate)
      #result <- result[complete.cases(result), ]
      rownames(result) <- NULL
    }

    # Sort data frame by time (column 1) then site (column 4):

    result <- result[order(result[[1L]], result[[4L]]), ]
  }

  ASNAT_elapsed_timer("ASNAT_import:", timer)
  return(result)
}
