###############################################################################
# PURPOSE: ASNAT_Dataset.R - Datasets of geospatial time-varying surface point
#          measured data.
#
# HISTORY: 2022-10-12 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

###############################################################################
# Load required source files:
###############################################################################

source("ASNAT_Utilities.R") # For function ASNAT_declare_method().

###############################################################################
# Load required libraries:
###############################################################################

library(methods) # For setClass().

###############################################################################

# Define class ASNAT_Dataset:

methods::setClass(
  "ASNAT_Dataset",
  slots = c(
    coverage = "character",      # "PurpleAir.pm25_corrected".
    start_date = "Date",         # Of retrieval: 2022-07-01.
    end_date = "Date",           # Of retrieval: 2022-07-07.
    aggregate = "character",     # "none", "all", "hourly", "daily".
    west_bound = "numeric",      # Of data: -124.0.
    east_bound = "numeric",      # Of data: -65.0.
    south_bound = "numeric",     # Of data: 24.0.
    north_bound = "numeric",     # Of data: 50.0.
    url = "character",           # Of webservice call used to retrieve data.
    file_name = "character",     # Of retrieved data: $HOME/ASNAT/data/tmp
    note = "character",          # "" or auxilliary info.
    data_frame = "data.frame",   # Of loaded data.
    variable_column = "integer"  # Column number in data_frame of variable.
))


# Validator:

methods::setValidity("ASNAT_Dataset", function(object) {

  # coverage does not contain problematic characters

  stopifnot(nchar(object@coverage) > 0L)
  stopifnot(length(grep("[ \"(),]", object@coverage)) == 0L)

  stopifnot(as.character(object@start_date) <= as.character(object@end_date))

  stopifnot(object@aggregate == "none" || object@aggregate == "all" ||
            object@aggregate == "hourly" || object@aggregate == "daily")

  stopifnot(object@west_bound >= -180.0)
  stopifnot(object@east_bound <= 180.0)
  stopifnot(object@west_bound <= object@east_bound)

  stopifnot(object@south_bound >= -90.0)
  stopifnot(object@north_bound <= 90.0)
  stopifnot(object@south_bound <= object@north_bound)

  #stopifnot(nchar(object@file_name) == 0L || file.exists(object@file_name))

  stopifnot(length(grep(fixed = TRUE, " ", object@url)) == 0L)

  stopifnot(nrow(object@data_frame) >= 1L)
  stopifnot(ncol(object@data_frame) >= 6L) # time, lon, lat, id, pm25, note.

  # variable_column is not time, lon, lat or note
  # and is not an empty column name in data_frame:

  stopifnot(object@variable_column >= 4L)
  stopifnot(object@variable_column <= ncol(object@data_frame) - 1L)
  stopifnot(nchar(colnames(object@data_frame)[[object@variable_column]]) > 0L)

  return(TRUE)
})


# Constructor:

ASNAT_Dataset <-
function(coverage,
         start_date,
         end_date,
         aggregate = "none",
         url = "",
         file_name = "",
         note = "",
         data_frame,
         variable_column) {

  longitudes <- data_frame[[2L]]
  latitudes <- data_frame[[3L]]
  west_bound <- min(longitudes)
  east_bound <- max(longitudes)
  south_bound <- min(latitudes)
  north_bound <- max(latitudes)

  object <- methods::new("ASNAT_Dataset",
                         coverage = coverage,
                         start_date = start_date,
                         end_date = end_date,
                         aggregate = aggregate,
                         west_bound = west_bound,
                         east_bound = east_bound,
                         south_bound = south_bound,
                         north_bound = north_bound,
                         url = url,
                         file_name = file_name,
                         note = note,
                         data_frame = data_frame,
                         variable_column = variable_column)

  return(object)
}


# Getters:

ASNAT_declare_method("ASNAT_Dataset", "coverage",
function(object) object@coverage)

ASNAT_declare_method("ASNAT_Dataset", "start_date",
function(object) object@start_date)

ASNAT_declare_method("ASNAT_Dataset", "end_date",
function(object) object@end_date)

ASNAT_declare_method("ASNAT_Dataset", "aggregation",
function(object) object@aggregate)

ASNAT_declare_method("ASNAT_Dataset", "west_bound",
function(object) object@west_bound)

ASNAT_declare_method("ASNAT_Dataset", "east_bound",
function(object) object@east_bound)

ASNAT_declare_method("ASNAT_Dataset", "south_bound",
function(object) object@south_bound)

ASNAT_declare_method("ASNAT_Dataset", "north_bound",
function(object) object@north_bound)

ASNAT_declare_method("ASNAT_Dataset", "url", function(object) object@url)

ASNAT_declare_method("ASNAT_Dataset", "file_name",
function(object) object@file_name)

ASNAT_declare_method("ASNAT_Dataset", "note", function(object) object@note)

ASNAT_declare_method("ASNAT_Dataset", "data_frame",
function(object) object@data_frame)

ASNAT_declare_method("ASNAT_Dataset", "variable_column",
function(object) object@variable_column)


# Setters:

ASNAT_declare_method("ASNAT_Dataset", "note<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  object@note <- value
  stopifnot(methods::validObject(object))
  return(object)
})


# Set index of variable column in data_frame.

ASNAT_declare_method("ASNAT_Dataset", "variable_column<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "integer" || class(value) == "numeric")
  stopifnot(value >= 4L && value <= ncol(object@data_frame) - 1L)

  ivalue <- as.integer(value)
  object@variable_column <- ivalue

  stopifnot(methods::validObject(object))
  return(object)
})



# Apply flag conditions to a dataset:

ASNAT_declare_method("ASNAT_Dataset", "apply_flag_conditions",
function(object, flag_conditions) {
  ASNAT_check(methods::validObject(object))
  stopifnot(length(flag_conditions) > 0L)
  flagged_data_frame <- ASNAT_clear_flagged_column(object@data_frame, TRUE)
  flagged_data_frame <- ASNAT_apply_flagging(flag_conditions, flagged_data_frame)
  object@data_frame <- flagged_data_frame
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Replace the flagged column values of a dataset:

ASNAT_declare_method("ASNAT_Dataset", "flagged_column_values",
function(object, flagged_values) {
  ASNAT_check(methods::validObject(object))
  stopifnot(length(flagged_values) == nrow(object@data_frame))
  the_data_frame <- object@data_frame
  the_column_names <- colnames(the_data_frame)
  flagged_column_index <- ASNAT_flagged_column_index(the_column_names)
  stopifnot(flagged_column_index > 0L)
  the_data_frame[flagged_column_index] <- flagged_values
  object@data_frame <- the_data_frame
  ASNAT_check(methods::validObject(object))
  return(object)
})


# Queries:

# Get variable name:

ASNAT_declare_method("ASNAT_Dataset", "variable_name",
function(object) {
  stopifnot(methods::validObject(object))
  the_data_frame <- object@data_frame
  the_column_names <- colnames(the_data_frame)
  name_units <- the_column_names[[object@variable_column]]
  parts <- unlist(strsplit(name_units, "[()]"))
  result <- parts[[1L]]
  return(result)
})


# Get variable units:

ASNAT_declare_method("ASNAT_Dataset", "variable_units",
function(object) {
  stopifnot(methods::validObject(object))
  the_data_frame <- object@data_frame
  the_column_names <- colnames(the_data_frame)
  name_units <- the_column_names[[object@variable_column]]
  parts <- unlist(strsplit(name_units, "[()]"))
  result <- parts[[2L]]
  return(result)
})


# Get "source" e.g., "AQS" or "fileset1":

ASNAT_declare_method("ASNAT_Dataset", "coverage_source",
function(object) {
  stopifnot(methods::validObject(object))
  parts <- unlist(strsplit(object@coverage, ".", fixed = TRUE))
  result <- parts[[1L]]
  return(result)
})


# Get "source.variable" e.g., "AQS.pm25" or "fileset1.pm25_corrected":

ASNAT_declare_method("ASNAT_Dataset", "source_variable",
function(object) {
  stopifnot(methods::validObject(object))
  parts <- unlist(strsplit(object@coverage, ".", fixed = TRUE))
  source <- parts[[1L]]
  result <- paste0(source, ".", variable_name(object))
  return(result)
})



# Get minimum data value:

ASNAT_declare_method("ASNAT_Dataset", "minimum",
function(object) {
  stopifnot(methods::validObject(object))
  the_data_frame <- object@data_frame
  the_variable_values <- the_data_frame[[object@variable_column]]
  result <- min(the_variable_values, na.rm = TRUE)
  return(result)
})


# Get maximum data value:

ASNAT_declare_method("ASNAT_Dataset", "maximum",
function(object) {
  stopifnot(methods::validObject(object))
  the_data_frame <- object@data_frame
  the_variable_values <- the_data_frame[[object@variable_column]]
  result <- max(the_variable_values, na.rm = TRUE)
  return(result)
})


# Commands:


# Create summary file and return a summary data.frame of dataset
# optionally compared to dataset2:

ASNAT_declare_method("ASNAT_Dataset", "summarize_dataset",
function(object, dataset2, total_timesteps, delta_meters, directory,
         file_format) {
  ASNAT_dprint("In summarize_dataset()\n")
  stopifnot(methods::validObject(object))
  stopifnot(is.null(dataset2) || class(dataset2) == "ASNAT_Dataset")
  stopifnot(delta_meters >= 0.0)
  stopifnot(delta_meters <= 10000.0)
  stopifnot(total_timesteps >= 1L)
  #stopifnot(dir.exists(directory))
  stopifnot(file_format == "csv" || file_format == "tsv")

  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  dataset <- object
  delimiter <- "\t"

  if (file_format == "csv") {
    delimiter <- ","
  }

  compare <- !is.null(dataset2)
  the_coverage <- coverage(dataset)
  parts <- unlist(strsplit(the_coverage, ".", fixed = TRUE))
  source <- parts[[1L]]
  variable <- paste0(source, ".", variable_name(dataset))
  variable2 <- NULL

  if (compare) {
    the_coverage <- coverage(dataset2)
    parts <- unlist(strsplit(the_coverage, ".", fixed = TRUE))
    source <- parts[[1L]]
    variable2 <- paste0(source, ".", variable_name(dataset2))
  }

  file_name <-
    ASNAT_summary_file_name(variable, variable2,
                            dataset@start_date, dataset@end_date,
                            directory, file_format)

  output_file <- file(file_name, "wb")

  if (file.exists(file_name)) {

    # One line before header:

    cat(sep = "", file = output_file, "Dataset Summary: ", variable)

    if (compare) {
      cat(sep = "", file = output_file, append = TRUE, " vs ", variable2)
    }

    cat(sep = "", file = output_file, append = TRUE, "\n")

    cat(sep = "", file = output_file, append = TRUE,
        variable, ": ",
        as.character(dataset@start_date), " to ",
        as.character(dataset@end_date),
        " (", dataset@west_bound, ", ", dataset@east_bound, ") (",
        dataset@south_bound, ", ", dataset@north_bound, ")\n")

    # One line header:

    units <- variable_units(dataset)

    cat(sep = "", file = output_file, append = TRUE,
        "Site_Id(-)", delimiter,
        "Timestamp_First_Measurement(UTC)", delimiter,
        "Timestamp_Last_Measurement(UTC)", delimiter,
        "Count(-)", delimiter,
        "Missing(%)", delimiter,
        "Mean_", variable, "(", units, ")", delimiter,
        "Min_", variable, "(", units, ")", delimiter,
        "P25_", variable, "(", units, ")", delimiter,
        "Median_", variable, "(", units, ")", delimiter,
        "P75_", variable, "(", units, ")", delimiter,
        "Max_", variable, "(", units, ")")

    if (compare) {
      cat(sep = "", file = output_file, append = TRUE,
          delimiter, "Nearest_Y_Site(-)",
          delimiter, "Nearest_Y_Distance(m)")
    }

    cat(sep = "", file = output_file, append = TRUE, "\n")
    data_frame <- data_frame(dataset)
    data_frame2 <- NULL

    if (compare) {
      data_frame2 <- data_frame(dataset2)
    }

    ASNAT_write_summary(data_frame, object@variable_column, data_frame2,
                        total_timesteps, delta_meters, delimiter, output_file)
    close(output_file)
  }

  # Read file into result data.frame.

  summary_data_frame <-
    try(silent = TRUE,
        read.delim(sep = delimiter, check.names = FALSE,
                   strip.white = TRUE, skip = 2L, file_name,
                   na.strings = ASNAT_na_strings,
                   stringsAsFactors = FALSE))

  if (class(summary_data_frame) != "data.frame") {
    summary_data_frame <- data.frame()
  }

  result <- summary_data_frame
  ASNAT_dprint("summarize_dataset() result:\n")
  ASNAT_debug(str, result)
  return(result)
})



# Compute and append _nowcast variable column of current variable and return
# the modified dataset:

ASNAT_declare_method("ASNAT_Dataset", "compute_hourly_nowcast",
function(object) {
  ASNAT_dprint("In ASNAT_Dataset compute_hourly_nowcast()\n")
  stopifnot(methods::validObject(object))

  the_data_frame <- object@data_frame
  column_names <- colnames(the_data_frame)
  the_variable_column <- object@variable_column
  name_units <- column_names[[the_variable_column]]
  nowcast_parameters <- ASNAT_nowcast_parameters(name_units)
  parts <- unlist(strsplit(name_units, "[()]"))
  nowcast_column_name <- paste0(parts[[1L]], "_nowcast(", parts[[2L]], ")")
  column_names <- colnames(the_data_frame)

  # Check if variable is nowcast-compatible and
  # the nowcast variable is not already present.

  if (!is.null(nowcast_parameters) &&
      !is.element(nowcast_column_name, column_names)) {

    # Insert column of nowcast measures (initialized to NA) before flagged
    # or lsast column:

    row_count <- nrow(the_data_frame)
    nowcast_vector <- rep(as.numeric(NA), row_count)
    flagged_column <- ASNAT_flagged_column_index(column_names)
    before <- ifelse(flagged_column > 0L, flagged_column - 1L, column_count - 1L)
    column_count <- length(column_names)
    rest <- before + 1L
    the_data_frame <- data.frame(the_data_frame[1L:before],
                                 nowcast_column_name = nowcast_vector,
                                 the_data_frame[rest:column_count],
                                 check.names = FALSE, stringsAsFactors = FALSE)
    the_nowcast_column <- rest
    column_names <- colnames(the_data_frame)
    column_names[[the_nowcast_column]] <- nowcast_column_name
    colnames(the_data_frame) <- column_names
    column_count <- column_count + 1L

    # Compute nowcast value for each site hourly measure
    # with NA used for missing site measure for that hour.
    # Note that the data frame must be sorted by time but may have dropouts
    # i.e., non-consecutive timestamps and also site may not have a measure
    # at every timestep.

    window_hours <- nowcast_parameters$window_hours
    minimum_weight_factor <- nowcast_parameters$minimum_weight_factor
    digits <- nowcast_parameters$digits
    timestamps <- the_data_frame[[1L]]
    timestamps <- substr(timestamps, 1L, 13L)
    first_timestamp <- timestamps[[1L]]
    last_timestamp <- timestamps[[length(timestamps)]]
    stopifnot(last_timestamp >= first_timestamp)
    first_time <- as.POSIXct(first_timestamp, format = "%Y-%m-%dT%H")
    last_time <- as.POSIXct(last_timestamp, format = "%Y-%m-%dT%H")
    timesteps <- 1L +
      as.integer(difftime(last_time, first_time, units = "hours"))
    stopifnot(timesteps >= 1L)
    site_column <- ASNAT_site_column_index(column_names)
    sites <- the_data_frame[[site_column]]
    unique_sites <- unique(sort.int(sites))
    measures_with_na <- rep(as.numeric(NA), timesteps)
    complete_timestamps <-
      format(seq(first_time, last_time, "hours"), format = "%Y-%m-%dT%H")

    for (site in unique_sites) {
      timer <- ASNAT_start_timer()
      measures_with_na[1L:timesteps] <- as.numeric(NA)
      ASNAT_elapsed_timer("assign NA:", timer)
      timer <- ASNAT_start_timer()

      if (ASNAT_use_cpp_functions) {
        ASNAT_copy_site_measures_cpp(site, site_column, the_variable_column,
                                     complete_timestamps, the_data_frame,
                                     measures_with_na)
      } else {

        for (timestep in 1L:timesteps) {
          timestamp <- complete_timestamps[[timestep]]
          matched_rows <- which(sites == site & timestamps == timestamp) # SLOW

          if (length(matched_rows) == 1L) {
            site_measure <- the_data_frame[matched_rows, the_variable_column]
            measures_with_na[[timestep]] <- site_measure
          }
        }
      }

      ASNAT_elapsed_timer("Copy site measures:", timer)
      timer <- ASNAT_start_timer()
      nowcast_measures <-
        ASNAT_compute_nowcast_callahan(measures_with_na, window_hours,
                                       minimum_weight_factor, digits)
      ASNAT_elapsed_timer("Copy site ASNAT_compute_nowcast_callahan:", timer)
      timer <- ASNAT_start_timer()

      # Overwrite nowcast column measures for this site in the_data_frame:

      if (ASNAT_use_cpp_functions) {
        ASNAT_copy_nowcast_measures_cpp(site, site_column, the_nowcast_column,
                                     complete_timestamps, nowcast_measures,
                                     the_data_frame)
      } else {

        for (timestep in 1L:timesteps) {
          timestamp <- complete_timestamps[[timestep]]
          matched_rows <- which(sites == site & timestamps == timestamp) # SLOW

          if (length(matched_rows) == 1L) {
            the_data_frame[matched_rows, the_nowcast_column] <-
              nowcast_measures[[timestep]]
          }
        }
      }

      ASNAT_elapsed_timer("Copy nowcast measures:", timer)
    }

    object@variable_column <- the_nowcast_column # Select new _nowcast variable.
    object@data_frame <- the_data_frame
  }

  return(object)
})



