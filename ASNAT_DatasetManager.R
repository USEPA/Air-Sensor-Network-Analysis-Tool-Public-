###############################################################################
# PURPOSE: ASNAT_DatasetManager.R - Manages ASNAT_Datasets retrieved from a
#          webservice or file.
#
# NOTES:  Requires our software to be installed in $HOME/ASNAT
#         and downloaded ASCII data files in $HOME/ASNAT/data/tmp/*.txt
#
# HISTORY: 2022-10-12 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

###############################################################################
# Load required source files:
###############################################################################

source("ASNAT_Utilities.R") # For function ASNAT_declare_method().
source("ASNAT_Dataset.R")   # For class ASNAT_Dataset.

###############################################################################
# Load required libraries:
###############################################################################

# URL to download any required R packages from:

repository <- "http://cran.us.r-project.org"

library(methods)

if (ASNAT_is_remote_hosted) {
  library(httr) # For httr::http_error().
  library(jsonlite) # For jsonlite::toJSON().
} else {
  if (!require(httr)) install.packages("httr", repos = repository)
  if (!require(jsonlite)) install.packages("jsonlite", repos = repository)
}

###############################################################################
# Global constants:
###############################################################################

# If maple is reachable then use it otherwise use ofmpub:

rsigserver_url <-
if (is.logical(try(httr::http_error("https://maple.hesc.epa.gov"), silent = TRUE))) "https://maple.hesc.epa.gov/rsig/rsigserver?" else
  "https://ofmpub.epa.gov/rsig/rsigserver?"

rsigserver_is_reachable <-
  is.logical(try(httr::http_error(rsigserver_url), silent = TRUE))

###############################################################################


# Define class ASNAT_DatasetManager (Singleton):

methods::setClass(
  "ASNAT_DatasetManager",
  slots = list(
    datasets = "vector", # Vector of loaded ASNAT_Dataset objects.
    ok = "logical", # Did last command succeed?
    timeout_seconds = "integer", # Seconds to wait to timeout retrievals.
    data_directory = "character", # Where streamed data files are written.
    retrieving_url_callback = "function", # Optional callback function.
    retrieved_urls = "vector", # Vector of URLs retrieved last.
    saved_files = "vector" # Vector of files last saved.
))


# Validator:

methods::setValidity("ASNAT_DatasetManager", function(object) {
  stopifnot(object@timeout_seconds >= 0L)
  #stopifnot(dir.exists(object@data_directory))

  for (dataset in object@datasets) {
    stopifnot(methods::validObject(dataset))
  }

  return(TRUE)
})


# Constructor:

ASNAT_DatasetManager <-
function(app_directory, data_subdirectory = NULL) {
  #stopifnot(dir.exists(app_directory))

  data_directory <- paste0(app_directory, "/data/tmp")

  if (!is.null(data_subdirectory)) {
    data_directory <- paste0(data_directory, "/", data_subdirectory)
  }

  if (!dir.exists(data_directory)) {
    dir.create(data_directory, recursive = TRUE)
  }

  object <- methods::new("ASNAT_DatasetManager",
                          datasets = vector(),
                          ok = TRUE,
                          timeout_seconds = 300L,
                          data_directory = data_directory,
                          retrieving_url_callback = function(...) {},
                          retrieved_urls = vector(),
                          saved_files = vector())

  return(object)
}


# Getters:

ASNAT_declare_method("ASNAT_DatasetManager", "ok", function(object) object@ok)

ASNAT_declare_method("ASNAT_DatasetManager", "timeout_seconds",
function(object) object@timeout_seconds)

ASNAT_declare_method("ASNAT_DatasetManager", "data_directory",
function(object) object@data_directory)

# Get URLs of retrieved in last call to retrieve_data:

ASNAT_declare_method("ASNAT_DatasetManager", "retrieved_urls",
function(object) object@retrieved_urls)

# Get file names of saved datasets in last call to save_datasets:

ASNAT_declare_method("ASNAT_DatasetManager", "saved_files",
function(object) object@saved_files)


# Queries:

# Get number of stored datasets:
# n <- count(dataset_manager)

ASNAT_declare_method("ASNAT_DatasetManager", "count",
function(object) length(object@datasets))

# Get dataset at specified index:
# dataset <- dataset(dataset_manager, index)

ASNAT_declare_method("ASNAT_DatasetManager", "dataset",
function(object, index) {
  ASNAT_check(methods::validObject(object))
  stopifnot(index >= 1L && index <= length(object@datasets))
  return(object@datasets[[index]])
})


# Get attribute of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "get_dataset_attribute",
function(object, dataset_function) {
  ASNAT_check(methods::validObject(object))
  stopifnot(length(object@datasets) > 0L)
  stopifnot(!class(dataset_function) == "function")
  count <- length(object@datasets)
  result <- rep(0, count)

  for (index in seq_along(object@datasets)) {
    result[index] <- dataset_function(object@datasets[[index]])
  }

  return(result)
})


# Get variable of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "dataset_coverages",
function(object) get_dataset_attribute(object, coverage))

# Get variable of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "dataset_variables",
function(object) get_dataset_attribute(object, variable_name))

# Get units of variable of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "dataset_units",
function(object) get_dataset_attribute(object, variable_units))

# Get minimum of variable of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "dataset_minimums",
function(object) get_dataset_attribute(object, minimum))

# Get miximum of variable of each stored dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "dataset_maximums",
function(object) get_dataset_attribute(object, maximum))

# Get download URL of this application:

ASNAT_declare_method("ASNAT_DatasetManager", "download_url",
function(object) {
  platform <- ASNAT_platform()
  result <- paste0(rsigserver_url, "asnat/download/", platform, "/ASNAT.zip")
  return(result)
})

# Get current version of this application as integer yyyymmdd:

ASNAT_declare_method("ASNAT_DatasetManager", "current_version",
function(object) {
  url <- paste0(rsigserver_url, "asnat/download/ASNAT_VERSION")
  quick_timeout_seconds <- 10L
  output_file_name <- paste0(object@data_directory, "/ASNAT_VERSION")
  result <- 0L
  ok <- ASNAT_http_get(url, quick_timeout_seconds, output_file_name)

  if (ok) {
    the_content <- try(silent = TRUE, readLines(output_file_name, 1L))
    unlink(output_file_name)

    if (class(the_content) == "character") {
      value <- as.integer(the_content)

      if (value >= 20221001L && value <= 29991231L) {
        result <- value
      }
    }
  }

  ASNAT_dprint("version = %d\n", result)
  return(result)
})


# Setters:

# Change timeout_seconds:
# Example change to 0 which waits indefinitely:
# timeout_seconds(dataset_manager) <<- 0L

ASNAT_declare_method("ASNAT_DatasetManager", "timeout_seconds<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  object@timeout_seconds <- as.integer(value)
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})


# Set optional callback function to be called before each webservice retrieval.
# Example:
#
# my_callback <- function(url, coverage, percent_done) {
#   cat("url =", url, "coverage =", coverage, percent_done, "% done\n")
# }
#
# set_retrieving_url_callback(dataset_manager) <<- my_callback

ASNAT_declare_method("ASNAT_DatasetManager", "set_retrieving_url_callback<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(is.null(value) || class(value) == "function")

  if (is.null(value)) {
    object@retrieving_url_callback <- function(...) {}
  } else {
    object@retrieving_url_callback <- value
  }

  ASNAT_check(methods::validObject(object))
  return(object)
})


# Commands:


# delete_all: delete any loaded datasets and retrieved files in data_directory:
#   dataset_manager <- delete_all(dataset_manager):

ASNAT_declare_method("ASNAT_DatasetManager", "delete_all",
function(object) {
  ASNAT_check(methods::validObject(object))
  object@datasets <- vector()
  object@retrieved_urls <- vector()
  object@saved_files <- vector()
  unlink(paste0(object@data_directory, "/*.txt"))
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})


# Change the variable column of a dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "set_variable_column",
function(object, dataset_index, variable_column) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= length(object@datasets))
  stopifnot(variable_column > 3L)
  stopifnot(variable_column <
            ncol(data_frame(object@datasets[[dataset_index]])))
  dataset <- object@datasets[[dataset_index]]
  variable_column(dataset) <- variable_column
  object@datasets[[dataset_index]] <- dataset
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Apply flag conditions to a dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "apply_flag_conditions",
function(object, dataset_index, flag_conditions) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= length(object@datasets))
  stopifnot(length(flag_conditions) > 0L)
  the_dataset <- object@datasets[[dataset_index]]
  the_dataset <- apply_flag_conditions(the_dataset, flag_conditions)
  object@datasets[[dataset_index]] <- the_dataset
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Replace the flagged column values of a dataset:

ASNAT_declare_method("ASNAT_DatasetManager", "replace_flagged_column_values",
function(object, dataset_index, flagged_values) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= length(object@datasets))
  stopifnot(length(flagged_values) ==
            nrow(data_frame(object@datasets[[dataset_index]])))
  dataset <- object@datasets[[dataset_index]]
  dataset <- flagged_column_values(dataset, flagged_values)
  object@datasets[[dataset_index]] <- dataset
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Retrieve data variable subset by time and lon-lat rectangle from rsigserver
# and write it to tab-delimited ASCII file in data_directory/ASNAT/data/tmp/
# then append it to vector object@datasets
# then return the object.
# Example:
# dataset_manager <-
#   retrieve_data(dataset_manager,
#                 "PurpleAir.pm25_corrected",
#                 as.Date("2022-07-01"), as.Date("2022-07-02"),
#                 "hourly",
#                 -75.0, -74.0, 40.0, 41.0,
#                 "my-valid-purple-air-api-read-key")
# if (ok(dataset_manager)) {
#   last <- count(dataset_manager)
#   dataset <- dataset_manager[[last]]
#   print(data_frame(dataset))
# }

ASNAT_declare_method("ASNAT_DatasetManager", "retrieve_data",
function(object,
         coverage,
         start_date, end_date,
         aggregate,
         west_bound, east_bound,
         south_bound, north_bound,
         purple_air_key = "",
         purple_air_sensor = 0L,
         aqs_pm25_codes = "") {

  ASNAT_dprint("In retrieve_data()\n")

  ASNAT_check(methods::validObject(object))
  stopifnot(nchar(coverage) >= 3L)
  stopifnot(!grepl("[^A-Za-z0-9_.]", coverage))
  stopifnot(length(grep(fixed = TRUE, ".", coverage)) > 0L)

  stopifnot(class(start_date) == "Date")
  stopifnot(class(end_date) == "Date")
  stopifnot(start_date <= end_date)

  stopifnot(aggregate == "hourly" || aggregate == "daily")

  stopifnot(west_bound >= -180.0)
  stopifnot(east_bound <= 180.0)
  stopifnot(west_bound <= east_bound)

  stopifnot(south_bound >= -90.0)
  stopifnot(north_bound <= 90.0)
  stopifnot(south_bound <= north_bound)

  stopifnot(length(grep(fixed = TRUE, "PurpleAir", coverage)) == 0L ||
            ASNAT_is_conforming_purple_air_key(purple_air_key))

  stopifnot(purple_air_sensor >= 0L)

  stopifnot(nchar(aqs_pm25_codes) == 0L ||
            ASNAT_is_valid_aqs_pm25_codes(aqs_pm25_codes))

  object@ok <- FALSE
  object@retrieved_urls <- vector()
  options <- ""
  sensor_tag <- ""

  if (length(grep(fixed = TRUE, "PurpleAir", coverage))) {
    aggregate_option <- paste0("&AGGREGATE=", aggregate)

    # Use pre-aggregated pseudo-variable if available:

    pre_aggregated_variables <-
      c("PurpleAir.pm25_corrected",
        "PurpleAir.humidity",
        "PurpleAir.temperature"
      )

    if (coverage %in% pre_aggregated_variables) {
      aggregate_option <- ""
      coverage <- paste0(coverage, "_", aggregate)
    }

    sensor_option <- ""

    if (purple_air_sensor > 0L) {
      sensor_option <- paste0("&SENSOR=", purple_air_sensor)
      sensor_tag <- paste0("_sensor_", purple_air_sensor)
    }

    options <- paste0(
      aggregate_option,
      "&KEY=",
      purple_air_key,
      sensor_option,
      "&OUT_IN_FLAG=0",
      "&MAXIMUM_DIFFERENCE=5",
      "&MAXIMUM_RATIO=0.7",
      "&MINIMUM_AGGREGATION_COUNT_PERCENTAGE=75")
  } else if (length(grep(fixed = TRUE, "AQS", coverage))) {
    options <- "&FILTER_MISSING=1"

    if (aggregate == "daily") {
      options <- paste0(options, "&AGGREGATE=daily_mean")
    }
  } else if (length(grep(fixed = TRUE, "AirNow", coverage)) ||
             length(grep(fixed = TRUE, "METAR", coverage))) {

    if (aggregate != "hourly") {
      options <- paste0("&AGGREGATE=", aggregate)
    }

    if (length(grep(fixed = TRUE, "AirNow", coverage))) {
      options <- paste0(options, "&FILTER_MISSING=1")
    }
  }

  if (nchar(aqs_pm25_codes) > 0 &&
      (length(grep(fixed = TRUE, "AirNow.pm25", coverage)) > 0L ||
       length(grep(fixed = TRUE, "AQS.pm25", coverage)) > 0L)) {
    options <- paste0(options, "&ONLY_CODES=", aqs_pm25_codes)
  }

  # To avoid http timeout while waiting for multi-day processing,
  # request one day at a time and concatenate ASCII results to output file
  # (except for all but the first header line):

  coverage_underscores <- gsub(fixed = TRUE, ".", "_", coverage)

  file_name <- paste0(
    object@data_directory, "/",
    coverage_underscores,
    sensor_tag,
    "_", format(start_date, "%Y-%m-%d"), "_to_", format(end_date, "%Y-%m-%d"),
    ".txt")

  # If file already exists in data_directory then don't call webservice.

  file_cached <- file.exists(file_name)
  wrote_header <- FALSE

  if (!file_cached) {
    output_file <- file(file_name, "wb") # wb prevents introducing \r chars.
  }

  request_date <- start_date
  done <- FALSE
  retrieved_days_count <- 0L
  requested_days_count <- 0L
  total_days <- 1L + as.integer(end_date) - as.integer(start_date)

  while (!done) {
    yyyy_mm_dd <- format(request_date, "%Y-%m-%d")
    time_range <- paste0(yyyy_mm_dd, "T00:00:00Z/", yyyy_mm_dd, "T23:59:59Z")

    url <- paste0(rsigserver_url,
                  "SERVICE=wcs&VERSION=1.0.0&REQUEST=GetCoverage",
                  "&FORMAT=ascii",
                  "&COVERAGE=",
                  coverage,
                  "&BBOX=",
                  west_bound, ",", south_bound, ",",
                  east_bound, ",", north_bound, ",",
                  "&TIME=",
                  time_range,
                  options)

    ok <- file_cached
    the_content <- NULL

    if (!file_cached) {
      len <- nchar(file_name)
      last <- len - 28L
      stopifnot(last > 0L)
      day_file_name <- paste0(substr(file_name, 1L, last), yyyy_mm_dd, ".txt")
      percent_done <- requested_days_count / total_days * 100.0
      ASNAT_dprint("requested_days_count = %d, total_days = %d, percent_done = %f\n",
                   requested_days_count, total_days, percent_done)
      object@retrieving_url_callback(url, coverage, percent_done)

      ok <- ASNAT_http_get(url, object@timeout_seconds, day_file_name)

      if (ok) {

        # File is tab-delimited and the first line is a header that looks like:
        # timestamp(UTC)  longitude(deg)	latitude(deg)	...

        the_content <- try(silent = TRUE, readLines(day_file_name))
        unlink(day_file_name)
        ok <- class(the_content) == "character" && length(the_content) > 1L &&
              nchar(the_content[[1L]]) > 40L
      }
    }

    if (ok) {

      if (!file_cached) {

        if (wrote_header) {
          the_content <- the_content[-1L]
        }

        try(silent = TRUE, writeLines(the_content, output_file))
        wrote_header <- TRUE
      }

      retrieved_days_count <- retrieved_days_count + 1L
      object@retrieved_urls[retrieved_days_count] <- url
    }

    requested_days_count <- requested_days_count + 1L
    request_date <- request_date + 1L
    done <- request_date > end_date

    ASNAT_dprint("request_date = %d, end_date = %d, done = %d\n",
                 request_date, end_date, as.integer(done))
  } # End loop on files.

  if (!file_cached) {
    close(output_file)
  }

  if (!file_cached && retrieved_days_count < 1L) {
    unlink(file_name)
  } else {

    # File is tab-delimited and the first line is a header that looks like:
    # Timestamp(UTC)  LONGITUDE(deg)  LATITUDE(deg)   STATION(-)      pm25(ug/m3)     SITE_NAME
    # or
    # Timestamp(UTC)	Longitude(deg)	Latitude(deg)	Elevation(m)	id(-)	count(-)	pm25_corrected_hourly(ug/m3)	Notes(-)

    header_line <- try(silent = TRUE, readLines(file_name, 1L))
    ok <- class(header_line) == "character" && length(header_line) == 1L &&
          nchar(header_line) > 40L

    if (ok) {
      column_delimiter <- ","

      if (length(grep(fixed = TRUE, "\t", header_line)) > 0L) {
        column_delimiter <- "\t"
      }

      data_frame <-
        try(silent = TRUE,
            read.delim(sep = column_delimiter, check.names = FALSE,
                       strip.white = TRUE, file_name,
                       na.strings = ASNAT_na_strings,
                       stringsAsFactors = FALSE))

      if (class(data_frame) == "data.frame" && ncol(data_frame) >= 6L) {

        # Edit the data_rame column names to standard format:
        # timestamp(UTC)	longitude(deg)	latitude(deg)	id(-)	...	note(-)

        column_count <- ncol(data_frame)
        column_names <- colnames(data_frame)
        column_names[[1L]] <- "timestamp(UTC)"
        column_names[[2L]] <- "longitude(deg)"
        column_names[[3L]] <- "latitude(deg)"

        if (tolower(column_names[[4L]]) == "elevation(m)") {
          column_names[[4L]] <- "elevation(m)"
          column_names[[5L]] <- "id(-)"
        } else {
          column_names[[4L]] <- "id(-)"
        }

        column_names[[column_count]] <- "note(-)"
        colnames(data_frame) <- column_names

        #data_frame <- data_frame[complete.cases(data_frame), ]

        if (nrow(data_frame) > 0L) {
          data_frame <- ASNAT_ensure_flagged_column(data_frame)

          # Validate the final data_frame before storing it:

          data_frame <- ASNAT_validate_input_data_frame(data_frame)

          if (!is.null(data_frame)) {

            # Overwrite temp cached file with contents of edited data_frame:

            output_file <- file(file_name, "wb") # wb prevents writing \r chars.
            try(silent = TRUE,
                write.table(data_frame, file = output_file,
                            sep = "\t", quote = FALSE, row.names = FALSE,
                            na = ASNAT_output_missing_string))
            close(output_file)

            # Replace or append dataset:

            data_column_index <- ASNAT_last_variable_index(column_names)

            dataset <-
              ASNAT_Dataset(coverage = coverage,
                            start_date = start_date,
                            end_date = end_date,
                            aggregate = aggregate,
                            url = object@retrieved_urls,
                            file_name = file_name,
                            note = "",
                            data_frame = data_frame,
                            variable_column = data_column_index)

            found <- FALSE

            for (index in seq_along(object@datasets)) {
              the_dataset <- object@datasets[[index]]

              if (url(the_dataset)[1L] != "" &&
                  coverage(the_dataset) == coverage) {
                found <- TRUE
                object@datasets[[index]] <- dataset
              }
            }

            if (!found) {
              object@datasets <- append(object@datasets, dataset)
            }

            object@ok <- TRUE
          }
        }
      }
    }
  }

  ASNAT_dprint("retrieve_data() returning\n")
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Retrieve purple air sites for date from rsigserver
# then return the data_frame.
# Example:
# data_frame <-
#   retrieve_purple_air_sites(dataset_manager,
#                             as.Date("2022-07-01")
#                             "my-valid-purple-air-api-read-key")

ASNAT_declare_method("ASNAT_DatasetManager", "retrieve_purple_air_sites",
function(object, start_date, key) {
  ASNAT_dprint("In retrieve_purple_air_sites()\n")
  ASNAT_check(methods::validObject(object))
  stopifnot(class(start_date) == "Date")
  stopifnot(class(key) == "character")

  result <- data.frame()

  if (ASNAT_is_conforming_purple_air_key(key)) {
    the_date <- start_date
    yesterday <- Sys.Date() - 1L

    if (the_date >= yesterday) {
      the_date <- yesterday - 1L
    }

    yyyy_mm_dd <- format(the_date, "%Y-%m-%d")
    time_option <- paste0(yyyy_mm_dd, "T00:00:00Z")

    url <- paste0(rsigserver_url,
                  "SERVICE=wcs&VERSION=1.0.0&REQUEST=GetCoverage",
                  "&FORMAT=ascii",
                  "&COVERAGE=PurpleAir.sites",
                  "&BBOX=-180,-90,180,90",
                  "&OUT_IN_FLAG=0",
                  "&KEY=",
                  key,
                  "&TIME=",
                  time_option)

    file_name <- paste0(object@data_directory, "/PurpleAirSites.txt")
    ok <- ASNAT_http_get(url, object@timeout_seconds, file_name)

    if (ok) {

      # File is tab-delimited and the first line is a header that looks like:
      # longitude(deg)	latitude(deg)	id(-)	note(-)

      result <-
        try(silent = TRUE,
            read.delim(sep = "\t", check.names = FALSE, strip.white = TRUE,
                       file_name))
      unlink(file_name)
      ok <- class(result) == "data.frame" && nrow(result) > 0L

      if (!ok) {
        result <- data.frame()
      }
    }
  }

  ASNAT_dprint("retrieve_purple_air_sites() returning\n")
  ASNAT_check(methods::validObject(object))
  return(result)
})



# Append a data_frame read from a set of files:

ASNAT_declare_method("ASNAT_DatasetManager", "append_data_frame",
function(object, data_frame, aggregate, name) {
  ASNAT_dprint("In append_data_frame()\n")
  ASNAT_check(methods::validObject(object))
  stopifnot(!is.null(data_frame))
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) > 4L)
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(aggregate == "hourly" || aggregate == "daily")
  stopifnot(ASNAT_is_valid_dataset_name(name))

  timestamps <- data_frame[[1L]]
  first_timestamp <- timestamps[[1L]]
  start_date <- as.Date(substr(first_timestamp, 1L, 10L), tz = "UTC")
  last_timestamp <- timestamps[[length(timestamps)]]
  end_date <- as.Date(substr(last_timestamp, 1L, 10L), tz = "UTC")

  # Ensure the dataset name is unique by appending a counter if needed:

  unique_name <- name

  if (length(object@datasets) > 0L) {
    the_coverages <- dataset_coverages(object)
    the_parts <- unlist(strsplit(the_coverages, ".", fixed = TRUE))
    the_names <- the_parts[seq_along(the_parts) %% 2L == 1L]

    name_i <- name
    matches <- which(the_names == name_i)
    i <- 1L

    while (length(matches) > 0L) {
      i <- i + 1L
      name_i <- paste0(name, "_", i)
      matches <- which(the_names == name_i)
    }

    unique_name <- name_i
  }

  coverage <- NULL

  if (aggregate == "hourly") {
    coverage <- paste0(unique_name, ".hourly_data")
  } else {
    coverage <- paste0(unique_name, ".daily_data")
  }

  short_file_name <- gsub(fixed = TRUE, ".", "_", coverage)

  file_name <- paste0(
    object@data_directory, "/",
    short_file_name,
    "_", format(start_date, "%Y-%m-%d"), "_to_", format(end_date, "%Y-%m-%d"),
    ".txt")

  data_frame <- ASNAT_ensure_flagged_column(data_frame)

  # Write data_frame to disk cache:

  try(silent = TRUE,
      write.table(data_frame, file = file_name,
                  sep = "\t", quote = FALSE, row.names = FALSE,
                  na = ASNAT_output_missing_string))

  column_names <- colnames(data_frame)
  data_column_index <- ASNAT_last_variable_index(column_names)
  dataset <-
    ASNAT_Dataset(coverage = coverage,
                  start_date = start_date,
                  end_date = end_date,
                  aggregate = aggregate,
                  url = "",
                  file_name = file_name,
                  note = "",
                  data_frame = data_frame,
                  variable_column = data_column_index)

  object@datasets <- append(object@datasets, dataset)
  object@ok <- TRUE

  ASNAT_dprint("append_data_frame() returning\n")
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Save datasets to specified output_directory:
# Example:
# dataset_manager <- save_datasets(dataset_manager, ASNAT_home_path(), ".tsv")
#
# if (ok(dataset_manager)) {
#   list.files(home_directory, pattern = "*.tsv")
# }

ASNAT_declare_method("ASNAT_DatasetManager", "save_datasets",
function(object, output_directory, output_format) {
  ASNAT_check(methods::validObject(object))
  #stopifnot(dir.exists(output_directory))
  stopifnot(output_format == "tsv" ||
            output_format == "csv" ||
            output_format == "json" ||
            output_format == "kml")

  object@ok <- FALSE
  object@saved_files <- vector()
  file_extension <- paste0(".", output_format)

  column_delimiter <- NULL

  if (output_format == "tsv") {
    column_delimiter <- "\t"
  } else if (output_format == "csv") {
    column_delimiter <- ","
  }

  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  count <- 0L

  for (the_dataset in object@datasets) {
    original_file_name <- file_name(the_dataset)
    parts <- unlist(strsplit(original_file_name, "/", fixed = TRUE))
    file_name <- parts[length(parts)]
    parts <- unlist(strsplit(file_name, ".", fixed = TRUE))
    file_name <- parts[length(parts) - 1L]
    output_file_name <- paste0(output_directory, "/", file_name, file_extension)
    ASNAT_dprint("Saving to file %s\n", output_file_name)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
    the_data_frame <- data_frame(the_dataset)

    if (!is.null(column_delimiter)) {
      try(silent = TRUE,
          write.table(the_data_frame, file = output_file_name,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
      object@ok <- TRUE
    } else if (output_format == "json") {
      json_object <-
        try(silent = TRUE, jsonlite::toJSON(the_data_frame, pretty = TRUE))

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file_name, json_object)
      }
    } else {
      ASNAT_dprint("UNIMPLEMENTED: output format\n")
    }
  }

  ASNAT_check(methods::validObject(object))
  return(object)
})



# Remove the temporary unique data directory and output directory
# if running remote-hosted.

ASNAT_declare_method("ASNAT_DatasetManager", "remove_temporary_directory",
function(object) {
  ASNAT_check(methods::validObject(object))

  if (ASNAT_is_remote_hosted) {
    ASNAT_dprint("Removing temporary unique data directory %s\n",
                 object@data_directory)
    unlink(object@data_directory)
  }

  return(object)
})


