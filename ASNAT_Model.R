###############################################################################
# PURPOSE: ASNAT_Model.R - Manages the state of ASNAT.
#
# NOTES:  Model-View-Controller Design Pattern.
#
# HISTORY: 2022-10-12 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

###############################################################################
# Load required source files:
###############################################################################

source("ASNAT_Utilities.R")      # For ASNAT_declare_method().
source("ASNAT_DatasetManager.R") # For class ASNAT_DatasetManager.

###############################################################################
# Load required libraries:
###############################################################################

# URL to download any required R packages from:

repository <- "http://cran.us.r-project.org"

library(methods)

if (ASNAT_is_remote_hosted) {
  library(jsonlite) # For jsonlite::toJSON().
} else {
  if (!require(jsonlite)) install.packages("jsonlite", repos = repository)
}

###############################################################################

# Define class ASNAT_Model:

methods::setClass(
"ASNAT_Model",
 slots = list(
   app_directory = "character", # Where ASNAT is installed. $HOME/ASNAT.
   output_directory = "character", # Where user saves data files. $HOME.
   output_format = "character", # tsv, csv, kml, json.
   start_date = "Date", # Of retrievals.
   days = "integer", # Of retrievals.
   timestep_size = "character",  # Of retrievals. hours or days.
   timestep = "integer", # Current timestep to display.
   west_bound = "numeric",  # Of retrievals.
   east_bound = "numeric",  # Of retrievals.
   south_bound = "numeric", # Of retrievals.
   north_bound = "numeric", # Of retrievals.
   purple_air_key = "character", # For retrievals of PurpleAir data.
   purple_air_sensor = "integer", # 0 for all sensors or > 0 for a specific one.
   purple_air_sites_data_frame = "data.frame", # PurpleAir sites.
   aqs_pm25_codes = "character", # For optional subset of AirNow/AQS pm25 codes.
   ok = "logical", # Did last command succeed?
   legend_colormap = "character", # Name of legend colormap to use.
   show_mean_values_on_map = "logical", # Show map points colored by mean?
   use_fancy_labels = "logical", # Use sub/superscripts and mu symbols?
   use_interactive_plots = "logical", # Use interactive plots or basic ones?
   show_site_labels = "logical", # Always show site id labels on neighbor map?
   maximum_neighbor_distance = "numeric", # Maximum neighbor distance in meters.
   apply_maximum_neighbor_value_difference = "logical", # Apply difference flag?
   apply_maximum_neighbor_value_percent_difference = "logical", # Apply flag?
   apply_minimum_neighbor_value_r_squared = "logical", # Apply flag?
   maximum_neighbor_value_difference = "numeric", # E.g., 5 ug/m3 else flag 80.
   maximum_neighbor_value_percent_difference = "numeric", # E.g., 70% else  81.
   minimum_neighbor_value_r_squared = "numeric", # E.g., 0.5 else flag 82.
   apply_constant_value_flag = "logical", # Apply flag?
   apply_negtive_value_flag = "logical", # Apply flag?
   apply_date_validation_flag = "logical", # Apply flag?
   apply_sudden_spike_flag = "logical", # Apply flag?
   apply_hampel_filter_flag = "logical", # Apply flag?
   apply_redundancy_check_flag = "logical", # Apply flag?
   apply_sudden_drop_flag = "logical", # Apply flag?
   apply_daily_pattern_o3 = "logical", # Apply flag?
   apply_daily_pattern_pm = "logical", # Apply flag?
   apply_format_check_flag = "logical", # Apply flag?
   apply_long_missing_flag = "logical", # Apply flag?
   long_missing_threshold = "integer", # E.g., 3 else flag 84.
   hampel_filter_threshold = "integer", # E.g., 3 else flag 86.
   hampel_filter_window = "integer", # E.g., 3 else flag 86.
   spike_time_window = "integer", # E.g., 3 else flag 70.
   spike_threshold = "numeric", # E.g., 3 else flag 70.
   drop_time_window = "integer", # E.g., 3 else flag 71.
   drop_threshold = "numeric", # E.g., 3 else flag 71.
   constant_value_threshold = "integer", # E.g., 3 else flag 83.
   apply_outlier_stat_flag = "logical", # Apply flag?
   outlier_threshold = "integer", # E.g., 3 else flag 85.
   outlier_time_window = "logical", # Apply time window flag?
   outlier_start_timestamps = "character", # Start timestamps for time window
   outlier_end_timestamps = "character", # End timestamps for time window
   dataset_manager = "ASNAT_DatasetManager", # Input datasets from web or files.
   summary_x_file_name = "character", # May be empty if not summarized.
   summary_x_title = "character", # May be empty if not summarized.
   summary_x_subtitle = "character", # May be empty if not summarized.
   summary_x_data_frame = "data.frame", # May be empty if not summarized.
   summary_y_file_name = "character", # May be empty if not summarized.
   summary_y_title = "character", # May be empty if not summarized.
   summary_y_subtitle = "character", # May be empty if not summarized.
   summary_y_data_frame = "data.frame", # May be empty if not summarized.
   comparison_file_name = "character", # May be empty if not summarized.
   comparison_title = "character", # May be empty if not summarized.
   comparison_subtitle = "character", # May be empty if not summarized.
   comparison_data_frame = "data.frame", # May be empty if not summarized.
   comparison_r2_file_name = "character", # May be empty if not summarized.
   comparison_r2_title = "character", # May be empty if not summarized.
   comparison_r2_subtitle = "character", # May be empty if not summarized.
   comparison_r2_data_frame = "data.frame", # May be empty if not summarized.
   comparison_r2_model_frame = "data.frame", # May be empty if not summarized.
   equation_list = "list", # May be empty if not summarized.
   aqi_statistics_file_name = "character", # May be empty if not summarized.
   aqi_statistics_title = "character", # May be empty if not summarized.
   aqi_statistics_subtitle = "character", # May be empty if not summarized.
   aqi_statistics_data_frame = "data.frame", # May be empty if not summarized.
   saved_files = "vector", # Vector of files last saved.
   id_equations = "list",  # New slot to store equations for each ID
   correction_dictionary = "list",  # New slot to store correction dataset
   correction_selection = "integer" # New slot to store correction selection 0 for none, 1 for linear, 2 for quadratic, 3 for cubic
))



# Validator:

methods::setValidity("ASNAT_Model", function(object) {
  #stopifnot(dir.exists(object@app_directory))
  #stopifnot(dir.exists(object@output_directory))
  stopifnot(object@output_format == "tsv" || object@output_format == "csv" ||
            object@output_format == "kml" || object@output_format == "json")
  stopifnot(class(object@start_date) == "Date")
  stopifnot(object@days >= 1L)
  stopifnot(object@days <= 366L)
  stopifnot(object@timestep_size == "hours" || object@timestep_size == "days")
  stopifnot(object@timestep >= 0L)
  timesteps <- object@days

  if (object@timestep_size == "hours") {
    timesteps <- timesteps * 24L
  }

  stopifnot(object@timestep < timesteps)
  stopifnot(object@west_bound >= -180.0)
  stopifnot(object@east_bound <= 180.0)
  stopifnot(object@west_bound <= object@east_bound)
  stopifnot(object@south_bound >= -90.0)
  stopifnot(object@north_bound <= 90.0)
  stopifnot(object@south_bound <= object@north_bound)
  #stopifnot(ASNAT_is_conforming_purple_air_key(object@purple_air_key))
  stopifnot(ASNAT_is_valid_aqs_pm25_codes(object@aqs_pm25_codes))
  stopifnot(object@purple_air_sensor >= 0L)
  stopifnot(ncol(object@purple_air_sites_data_frame) == 0L ||
            (ncol(object@purple_air_sites_data_frame) == 4L &&
             nrow(object@purple_air_sites_data_frame) > 0L))
  stopifnot(object@ok == FALSE || object@ok == TRUE)
  stopifnot(object@legend_colormap == "default" ||
            object@legend_colormap == "AQI" ||
            object@legend_colormap == "gray" ||
            object@legend_colormap == "blue" ||
            object@legend_colormap == "colorsafe" ||
            object@legend_colormap == "viridis")
  stopifnot(object@show_mean_values_on_map == FALSE ||
            object@show_mean_values_on_map == TRUE)
  stopifnot(object@use_fancy_labels == FALSE || object@use_fancy_labels == TRUE)
  stopifnot(object@use_interactive_plots == FALSE ||
            object@use_interactive_plots == TRUE)
  stopifnot(object@show_site_labels == FALSE || object@show_site_labels == TRUE)
  stopifnot(object@maximum_neighbor_distance >= 0.0)
  stopifnot(object@maximum_neighbor_distance <= 10000.0)
  stopifnot(object@apply_maximum_neighbor_value_difference == FALSE ||
            object@apply_maximum_neighbor_value_difference == TRUE)
  stopifnot(object@apply_maximum_neighbor_value_percent_difference == FALSE ||
            object@apply_maximum_neighbor_value_percent_difference == TRUE)
  stopifnot(object@apply_minimum_neighbor_value_r_squared == FALSE ||
            object@apply_minimum_neighbor_value_r_squared == TRUE)
  stopifnot(object@maximum_neighbor_value_difference >= 0.0)
  stopifnot(object@maximum_neighbor_value_difference <= 100.0)
  stopifnot(object@maximum_neighbor_value_percent_difference >= 0.0)
  stopifnot(object@maximum_neighbor_value_percent_difference <= 100.0)
  stopifnot(object@minimum_neighbor_value_r_squared >= 0.0)
  stopifnot(object@minimum_neighbor_value_r_squared <= 1.0)
  stopifnot(object@apply_constant_value_flag == FALSE ||
            object@apply_constant_value_flag == TRUE)
  stopifnot(object@apply_negtive_value_flag == FALSE ||
            object@apply_negtive_value_flag == TRUE)
  stopifnot(object@apply_date_validation_flag == FALSE ||
            object@apply_date_validation_flag == TRUE)
  stopifnot(object@apply_sudden_spike_flag == FALSE ||
            object@apply_sudden_spike_flag == TRUE)
  stopifnot(object@apply_hampel_filter_flag == FALSE ||
            object@apply_hampel_filter_flag == TRUE)
  stopifnot(object@apply_redundancy_check_flag == FALSE ||
            object@apply_redundancy_check_flag == TRUE)
  stopifnot(object@apply_sudden_drop_flag == FALSE ||
            object@apply_sudden_drop_flag == TRUE)
  stopifnot(object@apply_daily_pattern_o3 == FALSE ||
            object@apply_daily_pattern_o3 == TRUE)
  stopifnot(object@apply_daily_pattern_pm == FALSE ||
            object@apply_daily_pattern_pm == TRUE)
  stopifnot(object@apply_format_check_flag == FALSE ||
            object@apply_format_check_flag == TRUE)
  stopifnot(object@apply_long_missing_flag == FALSE ||
            object@apply_long_missing_flag == TRUE)
  stopifnot(object@apply_outlier_stat_flag == FALSE ||
            object@apply_outlier_stat_flag == TRUE)
  stopifnot(object@outlier_threshold >= 0L)
  stopifnot(object@outlier_threshold <= 100L)
  stopifnot(object@outlier_time_window == FALSE ||
            object@outlier_time_window == TRUE)
  stopifnot(object@long_missing_threshold >= 0L)
  stopifnot(object@long_missing_threshold <= 100L)
  stopifnot(object@hampel_filter_threshold >= 0L)
  stopifnot(object@hampel_filter_threshold <= 100L)
  stopifnot(object@spike_time_window >= 0L)
  stopifnot(object@spike_time_window <= 100L)
  stopifnot(object@spike_threshold >= 0.0)
  stopifnot(object@spike_threshold <= 1000.0)
  stopifnot(object@drop_time_window >= 0L)
  stopifnot(object@drop_time_window <= 100L)
  stopifnot(object@drop_threshold >= 0.0)
  stopifnot(object@drop_threshold <= 1000.0)
  stopifnot(object@hampel_filter_window >= 0L)
  stopifnot(object@hampel_filter_window <= 100L)
  stopifnot(object@constant_value_threshold >= 0L)
  stopifnot(object@constant_value_threshold <= 100L)
  stopifnot(methods::validObject(object@dataset_manager))
  stopifnot(nchar(object@summary_x_file_name) == 0L ||
            (#file.exists(object@summary_x_file_name) &&
            nchar(object@summary_x_title) > 0L &&
            nchar(object@summary_x_subtitle) > 0L &&
            ncol(object@summary_x_data_frame) >= 11L))
  stopifnot(nchar(object@summary_y_file_name) == 0L ||
            (#file.exists(object@summary_y_file_name) &&
            nchar(object@summary_y_title) > 0L &&
            nchar(object@summary_y_subtitle) > 0L &&
            ncol(object@summary_y_data_frame) >= 11L))
  stopifnot(nchar(object@comparison_file_name) == 0L ||
            (#file.exists(object@comparison_file_name) &&
            nchar(object@comparison_title) > 0L &&
            nchar(object@comparison_subtitle) > 0L &&
            (ncol(object@comparison_data_frame) == 6L ||
             ncol(object@comparison_data_frame) == 7L)))
  stopifnot(nchar(object@comparison_r2_file_name) == 0L ||
            (#file.exists(object@comparison_r2_file_name) &&
            nchar(object@comparison_r2_title) > 0L &&
            nchar(object@comparison_r2_subtitle) > 0L &&
            ncol(object@comparison_r2_data_frame) == 8L))
  stopifnot(nchar(object@aqi_statistics_file_name) == 0L ||
            (#file.exists(object@aqi_statistics_file_name) &&
            nchar(object@aqi_statistics_title) > 0L &&
            nchar(object@aqi_statistics_subtitle) > 0L &&
            ncol(object@aqi_statistics_data_frame) == 6L))
  return(TRUE)
})



# Constructor:

ASNAT_Model <-
function(data_subdirectory = NULL) {

  # Set app_directory and ensure that it exists:

  home_path <- ASNAT_home_path()
  output_directory <- home_path

  if (!is.null(data_subdirectory)) {
    output_directory <- paste0(output_directory, "/", data_subdirectory)
  }

  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  app_directory <- getwd()

  # Add $PWD/$platform/bin to $PATH so phantomjs and pandoc executables are
  # found:

  ASNAT_augment_path()

  # Remove log file, if it exists:

  the_log_file_name <- paste0(home_path, "/ASNAT_log.txt")

  if (file.exists(the_log_file_name)) {
    unlink(the_log_file_name)
  }

  # Default state:

  output_format <- "tsv"
  start_date <- Sys.Date() - 1 # Yesterday
  days <- 1L
  timestep_size <- "hours"
  timestep <- 0L

  # Default to New York City - Long Island:

  west_bound <- -74.3
  east_bound <- -72.7
  south_bound <- 40.5
  north_bound <- 41.4

  purple_air_key <- ""
  purple_air_sensor <- 0L
  aqs_pm25_codes <- ASNAT_aqs_pm25_codes[[1L]]
  legend_colormap <- "default"
  show_mean_values_on_map <- FALSE
  use_fancy_labels <- FALSE
  use_interactive_plots <- FALSE # Default to fast basic plots.
  show_site_labels <- FALSE
  maximum_neighbor_distance <- 1000.0
  apply_maximum_neighbor_value_difference <- FALSE
  apply_maximum_neighbor_value_percent_difference <- FALSE
  apply_minimum_neighbor_value_r_squared <- FALSE
  maximum_neighbor_value_difference <- 5.0
  maximum_neighbor_value_percent_difference <- 70.0
  minimum_neighbor_value_r_squared <- 0.5
  apply_long_missing_flag <- FALSE
  apply_outlier_stat_flag <- FALSE
  outlier_threshold <- 3L
  long_missing_threshold <- 3L
  constant_value_threshold <- 3L
  hampel_filter_threshold <- 3L
  hampel_filter_window <- 3L
  spike_time_window <- 3L
  spike_threshold <- 1.5
  drop_time_window <- 3L
  drop_threshold <- 1.5
  outlier_time_window <- FALSE
  outlier_start_timestamps <- ""
  outlier_end_timestamps <- ""
  apply_constant_value_flag <- FALSE
  apply_negtive_value_flag <- FALSE
  apply_date_validation_flag <- FALSE
  apply_sudden_spike_flag <- FALSE
  apply_hampel_filter_flag <- FALSE
  apply_redundancy_check_flag <- FALSE
  apply_sudden_drop_flag <- FALSE
  apply_daily_pattern_o3 <- FALSE
  apply_daily_pattern_pm <- FALSE
  apply_format_check_flag <- FALSE
  correction_selection <- 0L

  # Read state file:

  save_state_file <- paste0(home_path, "/.asnat")

  if (file.exists(save_state_file)) {
    lines <- try(silent = TRUE, readLines(save_state_file))

    if (class(lines) != "character") {
      lines <- NULL
    }

    for (line in lines) {
      parts <- unlist(strsplit(line, " ", fixed = TRUE))

      if (length(parts) == 2L) {
        tag <- parts[[1L]]
        value <- parts[[2L]]

        if (tag == "output_directory") {

          if (dir.exists(value)) {
            output_directory <- value
          }
        } else if (tag == "output_format") {

          if (value == "tsv" || value == "csv" || value == "kml" ||
              value == "json") {
            output_format <- value
          }

        } else if (tag == "start_date") {
          start_date <- as.Date(value)
        } else if (tag == "days") {
          ivalue <- as.integer(value)

          if (ivalue >= 1L && ivalue <= 366L) {
            days <- ivalue
          }

        } else if (tag == "timestep_size") {

          if (value == "hours" || value == "days") {
            timestep_size <- value
          }
        } else if (tag == "timestep") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L) {
            timesteps <- days

            if (timestep_size == "hours") {
              timesteps <- timesteps * 24L
            }

            if (ivalue < timesteps) {
              timestep <- ivalue
            }
          }
        } else if (tag == "west_bound") {
          fvalue <- as.numeric(value)

          if (fvalue >= -180.0 && fvalue <= 180.0) {
            west_bound <- fvalue

            if (east_bound < west_bound) {
              east_bound <- west_bound + 1.0
            }
          }
        } else if (tag == "east_bound") {
          fvalue <- as.numeric(value)

          if (fvalue >= -180.0 && fvalue <= 180.0) {
            east_bound <- fvalue

            if (west_bound > east_bound) {
              west_bound <- east_bound - 1.0
            }
          }
        } else if (tag == "south_bound") {
          fvalue <- as.numeric(value)

          if (fvalue >= -90.0 && fvalue <= 90.0) {
            south_bound <- fvalue

            if (north_bound < south_bound) {
              north_bound <- south_bound + 1.0
            }
          }
        } else if (tag == "north_bound") {
          fvalue <- as.numeric(value)

          if (fvalue >= -90.0 && fvalue <= 90.0) {
            north_bound <- fvalue

            if (south_bound > north_bound) {
              south_bound <- north_bound - 1.0
            }
          }
        } else if (tag == "legend_colormap") {

          if (value == "default" ||
              value == "AQI" ||
              value == "gray" ||
              value == "blue" ||
              value == "colorsafe" ||
              value == "viridis") {
            legend_colormap <- value
          }
        } else if (tag == "show_mean_values_on_map") {

          if (value == TRUE) {
            show_mean_values_on_map <- TRUE
          }
        } else if (tag == "use_fancy_labels") {

          if (value == TRUE) {
            use_fancy_labels <- TRUE
          }
        } else if (tag == "use_interactive_plots") {

          if (value == TRUE) {
            use_interactive_plots <- TRUE
          }
        } else if (tag == "show_site_labels") {

          if (value == TRUE) {
            show_site_labels <- TRUE
          }
        } else if (tag == "maximum_neighbor_distance") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 10000.0) {
            maximum_neighbor_distance <- fvalue
          }
        } else if (tag == "apply_maximum_neighbor_value_difference") {

          if (value == TRUE) {
            apply_maximum_neighbor_value_difference <- TRUE
          }
        } else if (tag == "apply_maximum_neighbor_value_percent_difference") {

          if (value == TRUE) {
            apply_maximum_neighbor_value_percent_difference <- TRUE
          }
        } else if (tag == "apply_minimum_neighbor_value_r_squared") {

          if (value == TRUE) {
            apply_minimum_neighbor_value_r_squared <- TRUE
          }
        } else if (tag == "maximum_neighbor_value_difference") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 100.0) {
            maximum_neighbor_value_difference <- fvalue
          }
        } else if (tag == "maximum_neighbor_value_percent_difference") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 100.0) {
            maximum_neighbor_value_percent_difference <- fvalue
          }
        } else if (tag == "minimum_neighbor_value_r_squared") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 100.0) {
            minimum_neighbor_value_r_squared <- fvalue
          }
        } else if (tag == "apply_constant_value_flag") {

          if (value == TRUE) {
            apply_constant_value_flag <- TRUE
          }
        } else if (tag == "apply_long_missing_flag") {

          if (value == TRUE) {
            apply_long_missing_flag <- TRUE
          }
        } else if (tag == "apply_outlier_stat_flag") {

          if (value == TRUE) {
            apply_outlier_stat_flag <- TRUE
          }
        } else if (tag == "outlier_threshold") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            outlier_threshold <- ivalue
          }
        } else if (tag == "outlier_time_window") {

          if (value == TRUE) {
            outlier_time_window <- TRUE
          }
        } else if (tag == "constant_value_threshold") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            constant_value_threshold <- ivalue
          }
        } else if (tag == "hampel_filter_threshold") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            hampel_filter_threshold <- ivalue
          }
        } else if (tag == "hampel_filter_window") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            hampel_filter_window <- ivalue
          }
        } else if (tag == "spike_threshold") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 1000.0) {
            spike_threshold <- fvalue
          }
        } else if (tag == "spike_time_window") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            spike_time_window <- ivalue
          }
        } else if (tag == "drop_time_window") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            drop_time_window <- ivalue
          }
        } else if (tag == "drop_threshold") {
          fvalue <- as.numeric(value)

          if (fvalue >= 0.0 && fvalue <= 1000.0) {
            drop_threshold <- fvalue
          }
        } else if (tag == "long_missing_threshold") {
          ivalue <- as.integer(value)

          if (ivalue >= 0L && ivalue <= 100L) {
            long_missing_threshold <- ivalue
          }
        } else if (tag == "purple_air_key") {
          purple_air_key <- value
        } else if (tag == "purple_air_sensor") {
          ivalue <- as.integer(value)

          if (ivalue >= 0) {
            purple_air_sensor <- ivalue
          }
        } else if (tag == "aqs_pm25_codes") {

          if (ASNAT_is_valid_aqs_pm25_codes(value)) {
            aqs_pm25_codes <- value
          }
        }
      }
    }
  }

  timesteps <- days

  if (timestep_size == "hours") {
    timesteps <- timesteps * 24L
  }

  if (timestep >= timesteps) {
    timestep <- timesteps - 1L
  }

  dataset_manager <-
    ASNAT_DatasetManager(app_directory, data_subdirectory)

  purple_air_sites_data_frame <-
    retrieve_purple_air_sites(dataset_manager, start_date, purple_air_key)

  object <- new("ASNAT_Model",
                app_directory = app_directory,
                output_directory = output_directory,
                output_format = output_format,
                start_date = start_date,
                days = days,
                timestep_size = timestep_size,
                timestep = timestep,
                west_bound = west_bound,
                east_bound = east_bound,
                south_bound = south_bound,
                north_bound = north_bound,
                purple_air_key = purple_air_key,
                purple_air_sensor = purple_air_sensor,
                purple_air_sites_data_frame = purple_air_sites_data_frame,
                aqs_pm25_codes = aqs_pm25_codes,
                ok = TRUE,
                legend_colormap = legend_colormap,
                show_mean_values_on_map = show_mean_values_on_map,
                use_fancy_labels = use_fancy_labels,
                use_interactive_plots = use_interactive_plots,
                show_site_labels = show_site_labels,
                maximum_neighbor_distance = maximum_neighbor_distance,
                apply_maximum_neighbor_value_difference =
                  apply_maximum_neighbor_value_difference,
                apply_maximum_neighbor_value_percent_difference =
                  apply_maximum_neighbor_value_percent_difference,
                apply_minimum_neighbor_value_r_squared =
                  apply_minimum_neighbor_value_r_squared,
                maximum_neighbor_value_difference =
                  maximum_neighbor_value_difference,
                maximum_neighbor_value_percent_difference =
                  maximum_neighbor_value_percent_difference,
                minimum_neighbor_value_r_squared =
                  minimum_neighbor_value_r_squared,
                apply_constant_value_flag = apply_constant_value_flag,
                apply_negtive_value_flag = apply_negtive_value_flag,
                apply_date_validation_flag = apply_date_validation_flag,
                apply_sudden_spike_flag = apply_sudden_spike_flag,
                apply_hampel_filter_flag = apply_hampel_filter_flag,
                apply_redundancy_check_flag = apply_redundancy_check_flag,
                apply_sudden_drop_flag = apply_sudden_drop_flag,
                apply_daily_pattern_o3 = apply_daily_pattern_o3,
                apply_daily_pattern_pm = apply_daily_pattern_pm,
                apply_format_check_flag = apply_format_check_flag,
                apply_long_missing_flag = apply_long_missing_flag,
                long_missing_threshold = long_missing_threshold,
                hampel_filter_threshold = hampel_filter_threshold,
                hampel_filter_window = hampel_filter_window,
                spike_threshold = spike_threshold,
                spike_time_window = spike_time_window,
                correction_selection = correction_selection,
                drop_threshold = drop_threshold,
                drop_time_window = drop_time_window,
                constant_value_threshold = constant_value_threshold,
                apply_outlier_stat_flag = apply_outlier_stat_flag,
                outlier_threshold = outlier_threshold,
                outlier_time_window = outlier_time_window,
                outlier_start_timestamps = outlier_start_timestamps,
                outlier_end_timestamps = outlier_end_timestamps,
                dataset_manager = dataset_manager,
                summary_x_file_name = "",
                summary_x_title = "",
                summary_x_subtitle = "",
                summary_x_data_frame = data.frame(),
                summary_y_file_name = "",
                summary_y_title = "",
                summary_y_subtitle = "",
                summary_y_data_frame = data.frame(),
                comparison_file_name = "",
                comparison_title = "",
                comparison_subtitle = "",
                comparison_data_frame = data.frame(),
                comparison_r2_file_name = "",
                comparison_r2_title = "",
                comparison_r2_subtitle = "",
                comparison_r2_data_frame = data.frame(),
                comparison_r2_model_frame = data.frame(),
                equation_list = list(),
                aqi_statistics_file_name = "",
                aqi_statistics_title = "",
                aqi_statistics_subtitle = "",
                aqi_statistics_data_frame = data.frame(),
                saved_files = vector(),
                correction_dictionary = list(),
                id_equations = list())
  return(object)
}



# Getters:

ASNAT_declare_method("ASNAT_Model", "ok", function(object) object@ok)

ASNAT_declare_method("ASNAT_Model", "legend_colormap",
function(object) object@legend_colormap)

ASNAT_declare_method("ASNAT_Model", "show_mean_values_on_map",
function(object) object@show_mean_values_on_map)

ASNAT_declare_method("ASNAT_Model", "use_fancy_labels",
function(object) object@use_fancy_labels)

ASNAT_declare_method("ASNAT_Model", "use_interactive_plots",
function(object) object@use_interactive_plots)

ASNAT_declare_method("ASNAT_Model", "show_site_labels",
function(object) object@show_site_labels)

ASNAT_declare_method("ASNAT_Model", "app_directory",
function(object) object@app_directory)

ASNAT_declare_method("ASNAT_Model", "data_directory",
function(object) data_directory(object@dataset_manager))

ASNAT_declare_method("ASNAT_Model", "output_directory",
function(object) object@output_directory)

ASNAT_declare_method("ASNAT_Model", "output_format",
function(object) object@output_format)

ASNAT_declare_method("ASNAT_Model", "start_date",
function(object) object@start_date)

ASNAT_declare_method("ASNAT_Model", "days", function(object) object@days)

ASNAT_declare_method("ASNAT_Model", "timestep_size",
function(object) object@timestep_size)

ASNAT_declare_method("ASNAT_Model", "timesteps",
function(object) {
  result <- object@days

  if (object@timestep_size == "hours") {
    result <- result * 24L
  }

  return(result)
})

ASNAT_declare_method("ASNAT_Model", "timestep",
function(object) object@timestep)

ASNAT_declare_method("ASNAT_Model", "west_bound",
function(object) object@west_bound)

ASNAT_declare_method("ASNAT_Model", "east_bound",
function(object) object@east_bound)

ASNAT_declare_method("ASNAT_Model", "south_bound",
function(object) object@south_bound)

ASNAT_declare_method("ASNAT_Model", "north_bound",
function(object) object@north_bound)

ASNAT_declare_method("ASNAT_Model", "purple_air_key",
function(object) object@purple_air_key)

ASNAT_declare_method("ASNAT_Model", "purple_air_sensor",
function(object) object@purple_air_sensor)

ASNAT_declare_method("ASNAT_Model", "purple_air_sites",
function(object) object@purple_air_sites_data_frame)

ASNAT_declare_method("ASNAT_Model", "aqs_pm25_codes",
function(object) object@aqs_pm25_codes)

ASNAT_declare_method("ASNAT_Model", "maximum_neighbor_distance",
function(object) object@maximum_neighbor_distance)

ASNAT_declare_method("ASNAT_Model", "apply_maximum_neighbor_value_difference",
function(object) object@apply_maximum_neighbor_value_difference)

ASNAT_declare_method("ASNAT_Model",
                     "apply_maximum_neighbor_value_percent_difference",
function(object) object@apply_maximum_neighbor_value_percent_difference)

ASNAT_declare_method("ASNAT_Model", "apply_minimum_neighbor_value_r_squared",
function(object) object@apply_minimum_neighbor_value_r_squared)

ASNAT_declare_method("ASNAT_Model", "maximum_neighbor_value_difference",
function(object) object@maximum_neighbor_value_difference)

ASNAT_declare_method("ASNAT_Model", "maximum_neighbor_value_percent_difference",
function(object) object@maximum_neighbor_value_percent_difference)

ASNAT_declare_method("ASNAT_Model", "minimum_neighbor_value_r_squared",
function(object) object@minimum_neighbor_value_r_squared)

ASNAT_declare_method("ASNAT_Model", "apply_constant_value_flag",
function(object) object@apply_constant_value_flag)

ASNAT_declare_method("ASNAT_Model", "apply_negtive_value_flag",
function(object) object@apply_negtive_value_flag)

ASNAT_declare_method("ASNAT_Model", "apply_date_validation_flag",
function(object) object@apply_date_validation_flag)

ASNAT_declare_method("ASNAT_Model", "apply_sudden_spike_flag",
function(object) object@apply_sudden_spike_flag)

ASNAT_declare_method("ASNAT_Model", "apply_hampel_filter_flag",
function(object) object@apply_hampel_filter_flag)

ASNAT_declare_method("ASNAT_Model", "apply_redundancy_check_flag",
function(object) object@apply_redundancy_check_flag)

ASNAT_declare_method("ASNAT_Model", "apply_sudden_drop_flag",
function(object) object@apply_sudden_drop_flag)

ASNAT_declare_method("ASNAT_Model", "apply_daily_pattern_o3",
function(object) object@apply_daily_pattern_o3)

ASNAT_declare_method("ASNAT_Model", "apply_daily_pattern_pm",
function(object) object@apply_daily_pattern_pm)

ASNAT_declare_method("ASNAT_Model", "apply_format_check_flag",
function(object) object@apply_format_check_flag)

ASNAT_declare_method("ASNAT_Model", "apply_long_missing_flag",
function(object) object@apply_long_missing_flag)

ASNAT_declare_method("ASNAT_Model", "long_missing_threshold",
function(object) object@long_missing_threshold)

ASNAT_declare_method("ASNAT_Model", "hampel_filter_threshold",
function(object) object@hampel_filter_threshold)

ASNAT_declare_method("ASNAT_Model", "hampel_filter_window",
function(object) object@hampel_filter_window)

ASNAT_declare_method("ASNAT_Model", "spike_threshold",
function(object) object@spike_threshold)

ASNAT_declare_method("ASNAT_Model", "spike_time_window",
function(object) object@spike_time_window)

ASNAT_declare_method("ASNAT_Model", "drop_threshold",
function(object) object@drop_threshold)

ASNAT_declare_method("ASNAT_Model", "drop_time_window",
function(object) object@drop_time_window)

ASNAT_declare_method("ASNAT_Model", "constant_value_threshold",
function(object) object@constant_value_threshold)

ASNAT_declare_method("ASNAT_Model", "apply_outlier_stat_flag",
function(object) object@apply_outlier_stat_flag)

ASNAT_declare_method("ASNAT_Model", "outlier_threshold",
function(object) object@outlier_threshold)

ASNAT_declare_method("ASNAT_Model", "outlier_time_window",
function(object) object@outlier_time_window)

ASNAT_declare_method("ASNAT_Model", "outlier_start_timestamps",
function(object) object@outlier_start_timestamps)

ASNAT_declare_method("ASNAT_Model", "outlier_end_timestamps",
function(object) object@outlier_end_timestamps)

# Get id_equations:
# Example: equations <- id_equations(asnat_model)
ASNAT_declare_method("ASNAT_Model", "id_equations",
function(object) object@id_equations)

# Getter for dataset_dictionary:
ASNAT_declare_method("ASNAT_Model", "correction_dictionary",
function(object) object@correction_dictionary)

ASNAT_declare_method("ASNAT_Model", "correction_selection",
function(object) object@correction_selection)


# Queries:

# Get string timestamp of current timestep:
# timestamp <- timestamp(model)
# E.g., "2021-07-01T12:00:00-0000"

ASNAT_declare_method("ASNAT_Model", "timestamp",
function(object) {
  result <- ""
  timestep_date <- object@start_date

  if (object@timestep_size == "days") {
    timestep_date <- timestep_date + object@timestep
    result <- format(timestep_date, "%Y-%m-%d")
  } else if (object@timestep_size == "hours") {
    timestep_days <- object@timestep %/% 24L
    timestep_date <- timestep_date + timestep_days
    hh <- object@timestep %% 24L
    result <- format(timestep_date, "%Y-%m-%d")
    result <- sprintf("%sT%02d:00:00-0000", result, hh)
  }

  return(result)
})


# Get string timestamp of first timestep:
# timestamp <- first_timestamp(model)
# E.g., "2021-07-01T00:00:00-0000"

ASNAT_declare_method("ASNAT_Model", "first_timestamp",
function(object) {
  current_timestep <- object@timestep
  object@timestep <- 0L
  result <- timestamp(object)
  object@timestep <- current_timestep
  return(result)
})



# Get string timestamp of last timestep:
# timestamp <- last_timestamp(model)
# E.g., "2021-07-01T23:00:00-0000"

ASNAT_declare_method("ASNAT_Model", "last_timestamp",
function(object) {
  current_timestep <- object@timestep
  object@timestep <- timesteps(object) - 1L
  result <- timestamp(object)
  object@timestep <- current_timestep
  return(result)
})


# Get number of stored datasets:
# n <- dataset_count(model)

ASNAT_declare_method("ASNAT_Model", "dataset_count",
function(object) count(object@dataset_manager))


# Get indexed stored dataset:
# the_dataset <- dataset(model, index)

ASNAT_declare_method("ASNAT_Model", "dataset",
function(object, index) dataset(object@dataset_manager, index))

# Get named dataset (or NULL if not found):
# the_dataset <- find_dataset(model, dataset_name)

ASNAT_declare_method("ASNAT_Model", "find_dataset",
function(object, dataset_name) {
  result <- NULL

  if (dataset_count(object) > 0L) {
    coverages <- dataset_coverages(object)
    index <- which(coverages == dataset_name)

    if (index > 0L) {
      result <- dataset(object, index)
    }
  }

  return(result)
})


# Get coverage of each stored dataset:

ASNAT_declare_method("ASNAT_Model", "dataset_coverages",
function(object) dataset_coverages(object@dataset_manager))


# Get name of variable of each stored datasets:

ASNAT_declare_method("ASNAT_Model", "dataset_variables",
function(object) dataset_variables(object@dataset_manager))


# Get units of variable of each stored datasets:

ASNAT_declare_method("ASNAT_Model", "dataset_units",
function(object) dataset_units(object@dataset_manager))


# Get minimum of variable of each stored datasets:

ASNAT_declare_method("ASNAT_Model", "dataset_minimums",
function(object) dataset_minimums(object@dataset_manager))


# Get maximum of variable of each stored datasets:

ASNAT_declare_method("ASNAT_Model", "dataset_maximums",
function(object) dataset_maximums(object@dataset_manager))


# Get spatial and temporal extent of all stored datasets:
#   Returns list result$west,east,south,north,start_date,end_date

ASNAT_declare_method("ASNAT_Model", "data_extent",
function(object) {
  result <- list(west = 0.0, east = 0.0, south = 0.0, north = 0.0,
                 start_date = NULL, end_date = NULL)
  count <- dataset_count(object)

  if (count > 0L) {
    na <- as.numeric(NA)
    west <- na
    east <- na
    south <- na
    north <- na
    date_minimum <- NULL
    date_maximum <- NULL

    for (index in 1L:count) {
      the_dataset <- dataset(object, index)
      west <- min(west_bound(the_dataset), west, na.rm = TRUE)
      east <- max(east_bound(the_dataset), east, na.rm = TRUE)
      south <- min(south_bound(the_dataset), south, na.rm = TRUE)
      north <- max(north_bound(the_dataset), north, na.rm = TRUE)
      the_start_date <- start_date(the_dataset)
      the_end_date <- end_date(the_dataset)

      if (is.null(date_minimum)) {
        date_minimum <- the_start_date
        date_maximum <- the_end_date
      } else {

        if (the_start_date < date_minimum) {
          date_minimum <- the_start_date
        }

        if (the_end_date > date_maximum) {
          date_maximum <- the_end_date
        }
      }
    }

    result$west <- west
    result$east <- east
    result$south <- south
    result$north <- north
    result$start_date <- the_start_date
    result$end_date <- the_end_date
  }

  return(result)
})


# Get URLs of retrieved datasets in last call to retrieve_data:

ASNAT_declare_method("ASNAT_Model", "retrieved_urls",
function(object) retrieved_urls(object@dataset_manager))


# Get file names of saved datasets in last call to save_datasets:

ASNAT_declare_method("ASNAT_Model", "saved_files",
function(object) {
  return(append(saved_files(object@dataset_manager), object@saved_files))
})

# Get file name of summary_x if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_x_file_name",
function(object) object@summary_x_file_name)

# Get title of summary_x if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_x_title",
function(object) object@summary_x_title)

# Get subtitle of summary_x if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_x_subtitle",
function(object) object@summary_x_subtitle)

# Get data_frame of summary_x if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_x_data_frame",
function(object) object@summary_x_data_frame)

# Get file name of summary_y if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_y_file_name",
function(object) object@summary_y_file_name)

# Get title of summary_y if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_y_title",
function(object) object@summary_y_title)

# Get subtitle of summary_y if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_y_subtitle",
function(object) object@summary_y_subtitle)

# Get data_frame of summary_y if summarized:

ASNAT_declare_method("ASNAT_Model", "summary_y_data_frame",
function(object) object@summary_y_data_frame)

# Get file name of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_file_name",
function(object) object@comparison_file_name)

# Get title of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_title",
function(object) object@comparison_title)

# Get subtitle of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_subtitle",
function(object) object@comparison_subtitle)

# Get data_frame of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_data_frame",
function(object) object@comparison_data_frame)

# Get file name of comparison_r2 if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_r2_file_name",
function(object) object@comparison_r2_file_name)

# Get title of comparison_r2 if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_r2_title",
function(object) object@comparison_r2_title)

# Get subtitle of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_r2_subtitle",
function(object) object@comparison_r2_subtitle)

# Get data_frame of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "comparison_r2_data_frame",
function(object) object@comparison_r2_data_frame)

# Get data_frame of model if compared:
ASNAT_declare_method("ASNAT_Model", "comparison_r2_model_frame",
function(object) object@comparison_r2_model_frame)

# Get list of equations if compared:
ASNAT_declare_method("ASNAT_Model", "equation_list",
function(object) object@equation_list)

# Get file name of aqi_statistics if compared:

ASNAT_declare_method("ASNAT_Model", "aqi_statistics_file_name",
function(object) object@aqi_statistics_file_name)

# Get title of aqi_statistics if compared:

ASNAT_declare_method("ASNAT_Model", "aqi_statistics_title",
function(object) object@aqi_statistics_title)

# Get subtitle of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "aqi_statistics_subtitle",
function(object) object@aqi_statistics_subtitle)

# Get data_frame of comparison if compared:

ASNAT_declare_method("ASNAT_Model", "aqi_statistics_data_frame",
function(object) object@aqi_statistics_data_frame)

# Get download URL of this application:

ASNAT_declare_method("ASNAT_Model", "download_url",
function(object) download_url(object@dataset_manager))


# What version of ASNAT are we running?

ASNAT_declare_method("ASNAT_Model", "version",
function(object) {
  program_name <- paste0(object@app_directory, "/app.R")
  file_timestamp <- file.mtime(program_name)
  result <- as.integer(strftime(file_timestamp, format = "%Y%m%d", tz = "UTC"))
  return(result)
})


# What version of ASNAT is available on rsigserver?

ASNAT_declare_method("ASNAT_Model", "public_version",
function(object) current_version(object@dataset_manager))


# Is there a newer version of ASNAT available for download?

ASNAT_declare_method("ASNAT_Model", "is_newer_version_available",
function(object) {

  # If ASNAT is deployed as a remote hosted client-server app then
  # the user is accessing the current version.
  # Otherwise if ASNAT is deployed as a local app then compare the version
  # vs the public version.

  if (ASNAT_is_remote_hosted) {
    result <- FALSE
  } else {
    this_version <- version(object)
    the_public_version <- public_version(object)
    result <- the_public_version > this_version
  }

  return(result)
})



# Setters:

# Change output directory:
# Example change to subdirectory tmp:
# output_directory(asnat_model) <<-
#   paste0(output_directory(asnat_model), "/tmp")

ASNAT_declare_method("ASNAT_Model", "output_directory<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dir.exists(value))
  object@output_directory <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change output format:
# Example:
# output_format(asnat_model) <<- "csv"

ASNAT_declare_method("ASNAT_Model", "output_format<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == "tsv" || value == "csv" || value == "kml" ||
            value == "json")
  object@output_format <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change start_date:
# Example change start_date to today: start_date(asnat_model) <<- Sys.Date())

ASNAT_declare_method("ASNAT_Model", "start_date<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "Date")
  object@start_date <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change days:
#   days(asnat_model) <<- 7L

ASNAT_declare_method("ASNAT_Model", "days<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "integer" || class(value) == "numeric")
  ivalue <- as.integer(value)
  stopifnot(ivalue >= 1L && ivalue <= 366L)
  object@days <- ivalue

  last_timestep <- timesteps(object) - 1L

  if (object@timestep > last_timestep) {
    object@timestep <- last_timestep
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change timestep_size:
# Example timestep_size(asnat_model) <<- "days"
# Also resets timestep to 0 to ensure it is within valid range.

ASNAT_declare_method("ASNAT_Model", "timestep_size<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  stopifnot(value == "hours" || value == "days")
  object@timestep_size <- value
  object@timestep <- 0L
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change timestep:
# Example timestep(asnat_model) <<- 2L

ASNAT_declare_method("ASNAT_Model", "timestep<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "numeric" || class(value) == "integer")
  ivalue <- as.integer(value)
  stopifnot(ivalue >= 0L)
  object_timesteps <- timesteps(object)
  stopifnot(ivalue < object_timesteps)
  object@timestep <- ivalue
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change west_bound:
# Example: west_bound(asnat_model) <<- -74.1

ASNAT_declare_method("ASNAT_Model", "west_bound<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= -180.0)
  stopifnot(value <= 180.0)
  object@west_bound <- value

  if (object@west_bound > object@east_bound) {
    object@east_bound <- object@west_bound + 1.0
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change east_bound:
# Example: east_bound(asnat_model) <<- -73.9

ASNAT_declare_method("ASNAT_Model", "east_bound<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= -180.0)
  stopifnot(value <= 180.0)
  object@east_bound <- value

  if (object@east_bound < object@west_bound) {
    object@west_bound <- object@east_bound - 1.0
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change south_bound:
# Example: south_bound(asnat_model) <<- 40.7

ASNAT_declare_method("ASNAT_Model", "south_bound<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= -90.0)
  stopifnot(value <= 90.0)
  object@south_bound <- value

  if (object@south_bound > object@north_bound) {
    object@north_bound <- object@south_bound + 1.0
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change north_bound:
# Example: north_bound(asnat_model) <<- 40.8

ASNAT_declare_method("ASNAT_Model", "north_bound<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= -90.0)
  stopifnot(value <= 90.0)
  object@north_bound <- value

  if (object@south_bound > object@north_bound) {
    object@south_bound <- object@north_bound - 1.0
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change purple_air_key:
# Example: purple_air_key(asnat_model) <<- my_purple_air_key

ASNAT_declare_method("ASNAT_Model", "purple_air_key<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  stopifnot(length(grep("[ ]", value)) == 0L)
  object@ok <- FALSE

  if (ASNAT_is_conforming_purple_air_key(value)) {
    object@purple_air_key <- value

    object@purple_air_sites_data_frame <-
      retrieve_purple_air_sites(object@dataset_manager, object@start_date,
                                object@purple_air_key)

    object@ok <- nrow(object@purple_air_sites_data_frame) > 0L
  }

  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change purple_air_sensor:
# Example: purple_air_sensor(asnat_model) <<- my_purple_air_sensor

ASNAT_declare_method("ASNAT_Model", "purple_air_sensor<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "numeric" || class(value) == "integer")
  stopifnot(as.integer(value) >= 0L)
  object@purple_air_sensor <- as.integer(value)
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change aqs_pm25_codes:
# Example: aqs_pm25_codes(asnat_model) <<- aqs_pm25_codes

ASNAT_declare_method("ASNAT_Model", "aqs_pm25_codes<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  stopifnot(ASNAT_is_valid_aqs_pm25_codes(value))
  object@aqs_pm25_codes <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change legend_colormap:
# Example: legend_colormap(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "legend_colormap<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == "default" ||
            value == "AQI" ||
            value == "gray" ||
            value == "blue" ||
            value == "colorsafe" ||
            value == "viridis")
  object@legend_colormap <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change show_mean_values_on_map:
# Example: show_mean_values_on_map(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "show_mean_values_on_map<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@show_mean_values_on_map <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change use_fancy_labels:
# Example: use_fancy_labels(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "use_fancy_labels<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@use_fancy_labels <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change use_interactive_plots:
# Example: use_interactive_plots(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "use_interactive_plots<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@use_interactive_plots <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change show_site_labels:
# Example: show_site_labels(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "show_site_labels<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@show_site_labels <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change maximum_neighbor_distance:
# Example: maximum_neighbor_distance(asnat_model) <<- 500.0

ASNAT_declare_method("ASNAT_Model", "maximum_neighbor_distance<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 10000.0)
  object@maximum_neighbor_distance <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change apply_max_neighbor_value_diff:
# Example: apply_max_neighbor_value_diff(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_max_neighbor_value_diff<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_maximum_neighbor_value_difference <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change apply_max_neighbor_val_pdiff:
# Example: apply_max_neighbor_val_pdiff(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_max_neighbor_val_pdiff<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_maximum_neighbor_value_percent_difference <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change apply_min_neighbor_value_r2:
# Example: apply_min_neighbor_value_r2(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_min_neighbor_value_r2<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_minimum_neighbor_value_r_squared <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change max_neighbor_value_diff:
# Example: max_neighbor_value_diff(asnat_model) <<- 5.0

ASNAT_declare_method("ASNAT_Model", "max_neighbor_value_diff<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 100.0)
  object@maximum_neighbor_value_difference <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change max_neighbor_value_percent_diff:
# Example: max_neighbor_value_percent_diff(asnat_model) <<- 70.0

ASNAT_declare_method("ASNAT_Model", "max_neighbor_value_pdiff<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 100.0)
  object@maximum_neighbor_value_percent_difference <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change min_neighbor_value_r2:
# Example: min_neighbor_value_r2(asnat_model) <<- 5.0

ASNAT_declare_method("ASNAT_Model", "min_neighbor_value_r2<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 1.0)
  object@minimum_neighbor_value_r_squared <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Apply flag conditions to a dataset:

ASNAT_declare_method("ASNAT_Model", "apply_flag_conditions",
function(object, dataset_index, flag_conditions) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= count(object@dataset_manager))
  stopifnot(length(flag_conditions) > 0L)
  object@dataset_manager <-
    apply_flag_conditions(object@dataset_manager, dataset_index,
                          flag_conditions)
  object@ok <- ok(object@dataset_manager)
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Replace flagged column of a dataset:

ASNAT_declare_method("ASNAT_Model", "replace_flagged_column",
function(object, dataset_index, flagged) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= count(object@dataset_manager))
  stopifnot(length(flagged) > 0L)
  object@dataset_manager <-
    replace_flagged_column_values(object@dataset_manager,
                                  dataset_index, flagged)
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_constant_value_flag:
# Example: apply_constant_value_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_constant_value_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_constant_value_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_negtive_value_flag:
# Example: apply_negtive_value_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_negtive_value_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_negtive_value_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_daily_pattern_pm:
# Example: apply_daily_pattern_pm(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_daily_pattern_pm<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_daily_pattern_pm <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_daily_pattern_o3:
# Example: apply_daily_pattern_o3(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_daily_pattern_o3<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_daily_pattern_o3 <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_sudden_drop_flag:
# Example: apply_sudden_drop_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_sudden_drop_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_sudden_drop_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_redundancy_check_flag:
# Example: apply_redundancy_check_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_redundancy_check_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_redundancy_check_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_format_check_flag:
# Example: apply_format_check_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_format_check_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_format_check_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_sudden_spike_flag:
# Example: apply_sudden_spike_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_sudden_spike_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_sudden_spike_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_date_validation_flag:
# Example: apply_date_validation_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_date_validation_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_date_validation_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set apply_hampel_filter_flag:
# Example: apply_hampel_filter_flag(asnat_model) <<- TRUE

ASNAT_declare_method("ASNAT_Model", "apply_hampel_filter_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_hampel_filter_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "apply_long_missing_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_long_missing_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "hampel_filter_window<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@hampel_filter_window <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "hampel_filter_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@hampel_filter_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "spike_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 1000.0)
  object@spike_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "spike_time_window<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@spike_time_window <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "drop_time_window<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@drop_time_window <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "drop_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0.0)
  stopifnot(value <= 1000.0)
  object@drop_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "long_missing_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@long_missing_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "constant_value_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@constant_value_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "apply_outlier_stat_flag<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@apply_outlier_stat_flag <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "outlier_threshold<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value >= 0L)
  stopifnot(value <= 100L)
  object@outlier_threshold <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "outlier_time_window<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(value == TRUE || value == FALSE)
  object@outlier_time_window <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "outlier_start_timestamps<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  # stopifnot(length(grep("[ ]", value)) == 0L)
  object@outlier_start_timestamps <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "outlier_end_timestamps<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "character")
  # stopifnot(length(grep("[ ]", value)) == 0L)
  object@outlier_end_timestamps <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "correction_selection<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(class(value) == "numeric" || class(value) == "integer")
  stopifnot(as.integer(value) >= 0L)
  stopifnot(as.integer(value) <= 3L)
  object@correction_selection <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})


# Set optional callback function to be called before each webservice retrieval.
# Example:
#
# my_callback <- function(url, coverage, percent_done) {
#   cat("url =", url, "coverage = ", coverage, percent_done, "% done\n")
# }
#
# set_retrieving_url_callback(asnat_model) <<- my_callback

ASNAT_declare_method("ASNAT_Model", "set_retrieving_url_callback<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))
  stopifnot(is.null(value) || class(value) == "function")
  set_retrieving_url_callback(object@dataset_manager) <- value
  ASNAT_check(methods::validObject(object))
  return(object)
})


# Commands:


# delete_datasets: delete any loaded datasets:
# Example asnat_model <<- delete_datasets(asnat_model):

ASNAT_declare_method("ASNAT_Model", "delete_datasets",
function(object) {
  ASNAT_check(methods::validObject(object))
  object@dataset_manager <- delete_all(object@dataset_manager)

  # Also delete any summary and comparison data and files:

  the_data_directory <- data_directory(object@dataset_manager)
  unlink(paste0(the_data_directory, "/*.tsv"))

  object@summary_x_file_name <- ""
  object@summary_x_title <- ""
  object@summary_x_subtitle <- ""
  object@summary_x_data_frame <- data.frame()

  object@summary_y_file_name <- ""
  object@summary_y_title <- ""
  object@summary_y_subtitle <- ""
  object@summary_y_data_frame <- data.frame()

  object@comparison_file_name <- ""
  object@comparison_title <- ""
  object@comparison_subtitle <- ""
  object@comparison_data_frame <- data.frame()

  object@comparison_r2_file_name <- ""
  object@comparison_r2_title <- ""
  object@comparison_r2_subtitle <- ""
  object@comparison_r2_data_frame <- data.frame()
  object@comparison_r2_model_frame <- data.frame()
  object@equation_list <- list()

  object@aqi_statistics_file_name <- ""
  object@aqi_statistics_title <- ""
  object@aqi_statistics_subtitle <- ""
  object@aqi_statistics_data_frame <- data.frame()

  object@saved_files <- vector()

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Change the variable column of a dataset:

ASNAT_declare_method("ASNAT_Model", "set_variable_column",
function(object, dataset_index, variable_column) {
  ASNAT_check(methods::validObject(object))
  stopifnot(dataset_index > 0L)
  stopifnot(dataset_index <= count(object@dataset_manager))
  stopifnot(variable_column > 3L)
  stopifnot(variable_column < ncol(data_frame(dataset(object@dataset_manager, dataset_index))))
  object@dataset_manager <-
    set_variable_column(object@dataset_manager, dataset_index, variable_column)
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Set equation for a specific ID
# Example: set_id_equation(asnat_model, id, equation_info)

ASNAT_declare_method("ASNAT_Model", "set_id_equation",
function(object, id, equation_info) {
  ASNAT_check(methods::validObject(object))
  stopifnot(is.numeric(id))
  stopifnot(!is.null(equation_info))
  stopifnot(is.list(equation_info))

  object@id_equations[[id]] <- equation_info

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



ASNAT_declare_method("ASNAT_Model", "correction_dictionary<-",
function(object, value) {
  ASNAT_check(methods::validObject(object))

  # Validate input
  stopifnot(is.list(value))
  key_names <- names(value)
  stopifnot(!is.null(key_names), all(grepl("^\\d+$", key_names)))

  for (key in key_names) {
    stopifnot(is.list(value[[key]])) # Each entry should be a list of datasets
    # Additional checks can be implemented here
  }

  object@correction_dictionary <- value
  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Add a dataset to the correction_dictionary:

ASNAT_declare_method("ASNAT_Model", "add_dataset_to_dictionary",
function(object, key, dataset) {
  ASNAT_check(methods::validObject(object))

  # Ensure key is numeric
  stopifnot(is.numeric(key))

  # key_str <- as.character(key)

  # # Initialize list for the key if it doesn't exist
  # if (!key_str %in% names(object@correction_dictionary)) {
  #   object@correction_dictionary[[key]] <- list()
  # }

  # Append the new dataset
  object@correction_dictionary[[key]] <- dataset

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# Remove a dataset from the correction_dictionary:

ASNAT_declare_method("ASNAT_Model", "remove_dataset_from_dictionary",
function(object, key, dataset_index) {
  ASNAT_check(methods::validObject(object))

  # Ensure key is numeric
  stopifnot(is.numeric(key))

  key_str <- as.character(key)

  # Check if key exists
  stopifnot(key_str %in% names(object@correction_dictionary))

  # Check dataset_index is valid
  stopifnot(dataset_index > 0 && dataset_index <= length(object@correction_dictionary[[key_str]]))

  # Remove the dataset
  object@correction_dictionary[[key_str]] <- object@correction_dictionary[[key_str]][-dataset_index]

  # If no datasets remain for the key, remove the key
  if (length(object@correction_dictionary[[key_str]]) == 0) {
    object@correction_dictionary[[key_str]] <- NULL
  }

  object@ok <- TRUE
  ASNAT_check(methods::validObject(object))
  return(object)
})



# process_flaggings:
# function to process auto/preset flags to data frame
# dataset_flag_index - the index of the dataset to be processed
# selected_flag_variable - the variable to be processed
# Example
#  asnat_model <<-
#    process_flaggings(asnat_model, dataset_flag_index, selected_flag_variable):

ASNAT_declare_method("ASNAT_Model", "process_flaggings",
function(object, dataset_flag_index, selected_flag_variable) {
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  apply_negtive_value_flag <- object@apply_negtive_value_flag
  apply_date_validation_flag <- object@apply_date_validation_flag
  apply_redundancy_check_flag <- object@apply_redundancy_check_flag
  apply_format_check_flag <- object@apply_format_check_flag

  spike_threshold <-
    if (object@apply_sudden_spike_flag) object@spike_threshold else -1L
  spike_time_window <- object@spike_time_window

  drop_threshold <-
    if (object@apply_sudden_drop_flag) object@drop_threshold else -1L
  drop_time_window <- object@drop_time_window


  apply_daily_pattern_o3 <- object@apply_daily_pattern_o3
  apply_daily_pattern_pm <- object@apply_daily_pattern_pm

  hampel_filter_threshold <-
    if (object@apply_hampel_filter_flag) object@hampel_filter_threshold else -1L

  hampel_filter_window <-
    if (object@apply_hampel_filter_flag) object@hampel_filter_window else -1L
  constant_value_threshold <-
    if (object@apply_constant_value_flag) object@constant_value_threshold else
    -1L
  the_dataset <- dataset(object, dataset_flag_index)
  data_frame <- data_frame(the_dataset)

  long_missing_threshold <-
    if (object@apply_long_missing_flag) object@long_missing_threshold else -1L

  constant_value_threshold <-
    if (object@apply_constant_value_flag) object@constant_value_threshold else
    -1L

  apply_date_validation_flag <- object@apply_date_validation_flag
  outlier_threshold <-
    if (object@apply_outlier_stat_flag) object@outlier_threshold else -1L

  outlier_time_window <- object@outlier_time_window
  outlier_start_timestamps <- object@outlier_start_timestamps
  outlier_end_timestamps <- object@outlier_end_timestamps

  flagged_result <-
    ASNAT_auto_flags(object, data_frame, selected_flag_variable,
                    apply_negtive_value_flag,
                    apply_date_validation_flag, apply_redundancy_check_flag,
                    apply_format_check_flag,
                    apply_sudden_spike_flag, apply_sudden_drop_flag,
                    spike_threshold, spike_time_window,
                    drop_threshold, drop_time_window,
                    constant_value_threshold,
                    apply_daily_pattern_o3, apply_daily_pattern_pm,
                    hampel_filter_threshold, hampel_filter_window,
                    long_missing_threshold,
                    outlier_threshold,
                    outlier_time_window,
                    outlier_start_timestamps,
                    outlier_end_timestamps)

  # apply the actual flag changes to the dataset
  if (flagged_result$updated) {
    object@dataset_manager <-
      replace_flagged_column_values(object@dataset_manager,
                                    dataset_flag_index,
                                    flagged_result$flagged_y_full)
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("process_flaggings:", timer)
  return(object)
})



# delete_dataset_summaries:
# Example
# asnat_model <<- delete_dataset_summaries(asnat_model):

ASNAT_declare_method("ASNAT_Model", "delete_dataset_summaries",
function(object) {
  ASNAT_check(methods::validObject(object))

  if (file.exists(object@summary_x_file_name)) {
    unlink(object@summary_x_file_name)
  }

  if (file.exists(object@summary_y_file_name)) {
    unlink(object@summary_y_file_name)
  }

  if (file.exists(object@comparison_file_name)) {
    unlink(object@comparison_file_name)
  }

  if (file.exists(object@comparison_r2_file_name)) {
    unlink(object@comparison_r2_file_name)
  }

  if (file.exists(object@aqi_statistics_file_name)) {
    unlink(object@aqi_statistics_file_name)
  }

  object@summary_x_file_name <- ""
  object@summary_x_title <- ""
  object@summary_x_subtitle <- ""
  object@summary_x_data_frame <- data.frame()
  object@summary_y_file_name <- ""
  object@summary_y_title <- ""
  object@summary_y_subtitle <- ""
  object@summary_y_data_frame <- data.frame()
  object@comparison_file_name <- ""
  object@comparison_title <- ""
  object@comparison_subtitle <- ""
  object@comparison_data_frame <- data.frame()
  object@comparison_r2_file_name <- ""
  object@comparison_r2_title <- ""
  object@comparison_r2_subtitle <- ""
  object@comparison_r2_data_frame <- data.frame()
  object@comparison_r2_model_frame <- data.frame()
  object@equation_list <- list()
  object@aqi_statistics_file_name <- ""
  object@aqi_statistics_title <- ""
  object@aqi_statistics_subtitle <- ""
  object@aqi_statistics_data_frame <- data.frame()

  ASNAT_check(methods::validObject(object))
  return(object)
})



# summarize_datasets:
# Example
#  asnat_model <<-
#    summarize_datasets(asnat_model, dataset_x_name, "pm25(ug/m3)",
#                       dataset_y_name, "pm25_corrected(ug/m3)"):

ASNAT_declare_method("ASNAT_Model", "summarize_datasets",
function(object, dataset_x_name, dataset_x_variable,
         dataset_y_name, dataset_y_variable) {
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  stopifnot(!is.null(dataset_x_name))
  stopifnot(class(dataset_x_name) == "character")
  stopifnot(nchar(dataset_x_name) > 0L)
  stopifnot(is.null(dataset_y_name) ||
            (class(dataset_y_name) == "character" &&
            nchar(dataset_y_name) > 0L &&
            class(dataset_y_variable) == "character" &&
            nchar(dataset_y_variable) > 0L))

  object@ok <- FALSE
  object@summary_x_file_name <- ""
  object@summary_x_title <- ""
  object@summary_x_subtitle <- ""
  object@summary_x_data_frame <- data.frame()
  object@summary_y_file_name <- ""
  object@summary_y_title <- ""
  object@summary_y_subtitle <- ""
  object@summary_y_data_frame <- data.frame()

  coverages <- dataset_coverages(object)
  index <- which(coverages == dataset_x_name)

  if (index > 0L) {
    dataset_x <- dataset(object, index)
    dataset_y <- NULL
    have_y <- FALSE

    if (!is.null(dataset_y_name)) {
      index <- which(coverages == dataset_y_name)

      if (index > 0L) {
        dataset_y <- dataset(object, index)
        have_y <- TRUE
      }
    }

    total_timesteps <- timesteps(object)
    directory <- data_directory(object@dataset_manager)
    file_format <- "tsv"

    # Possibly change variable column in (shallow) copy of dataset_x/y:

    data_frame_x <- data_frame(dataset_x)
    column_names_x <- colnames(data_frame_x)
    variable_column_x <- which(column_names_x == dataset_x_variable)

    if (variable_column(dataset_x) != variable_column_x) {
      variable_column(dataset_x) <- variable_column_x
    }

    if (have_y) {
      data_frame_y <- data_frame(dataset_y)
      column_names_y <- colnames(data_frame_y)
      variable_column_y <- which(column_names_y == dataset_y_variable)

      if (variable_column(dataset_y) != variable_column_y) {
        variable_column(dataset_y) <- variable_column_y
      }
    }

    data_frame_x <-
      summarize_dataset(dataset_x, dataset_y,
                        total_timesteps, object@maximum_neighbor_distance,
                        directory, file_format)

    if (nrow(data_frame_x) > 0L) {
      source_variable_x <- source_variable(dataset_x)
      source_variable_y <- if (have_y) source_variable(dataset_y) else NULL

      file_name <-
        ASNAT_summary_file_name(source_variable_x, source_variable_y,
                                start_date(dataset_x), end_date(dataset_x),
                                directory, file_format)
      lines <- try(silent = TRUE, readLines(file_name, 2L))

      if (class(lines) == "character" && length(lines) == 2L) {
        object@summary_x_title <- lines[1L]
        object@summary_x_subtitle <- lines[2L]
        object@summary_x_file_name <- file_name
        object@summary_x_data_frame <- data_frame_x
        object@ok <- !have_y

        if (have_y) {
          data_frame_y <-
            summarize_dataset(dataset_y, dataset_x,
                              total_timesteps, object@maximum_neighbor_distance,
                              directory, file_format)

          if (nrow(data_frame_y) > 0L) {
            file_name <-
              ASNAT_summary_file_name(source_variable_y, source_variable_x,
                                      start_date(dataset_y), end_date(dataset_y),
                                      directory, file_format)
            lines <- try(silent = TRUE, readLines(file_name, 2L))

            if (class(lines) == "character" && length(lines) == 2L) {
              object@summary_y_title <- lines[1L]
              object@summary_y_subtitle <- lines[2L]
              object@summary_y_file_name <- file_name
              object@summary_y_data_frame <- data_frame_y
              object@ok <- TRUE
            }
          }
        }
      }
    }
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("summarize_datasets:", timer)
  return(object)
})



# compare_datasets:
# Example asnat_model <<-
#    compare_datasets(asnat_model, dataset_x_name, "pm25(ug/m3)",
#                     dataset_y_name, "pm25_corrected(ug/m3)"):
# Note: optional dataset_z source (e.g., AQS, PurpleAir) must match
# either dataset_x or dataset_y.

ASNAT_declare_method("ASNAT_Model", "compare_datasets",
function(object, dataset_x_name, dataset_x_variable,
         dataset_y_name, dataset_y_variable,
         dataset_z_name = NULL, dataset_z_variable = NULL,
         regression_method = NULL ) {

  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  stopifnot(!is.null(dataset_x_name))
  stopifnot(class(dataset_x_name) == "character")
  stopifnot(nchar(dataset_x_name) > 0L)
  stopifnot(!is.null(dataset_x_variable))
  stopifnot(class(dataset_x_variable) == "character")
  stopifnot(nchar(dataset_x_variable) > 0L)
  stopifnot(!is.null(dataset_y_name))
  stopifnot(class(dataset_y_name) == "character")
  stopifnot(nchar(dataset_y_name) > 0L)
  stopifnot(!is.null(dataset_y_variable))
  stopifnot(class(dataset_y_variable) == "character")
  stopifnot(nchar(dataset_y_variable) > 0L)
  stopifnot((is.null(dataset_z_name) && is.null(dataset_z_variable)) ||
            (!is.null(dataset_z_name) && !is.null(dataset_z_variable)))

  object@ok <- FALSE
  object@comparison_title <- ""
  object@comparison_subtitle <- ""
  object@comparison_file_name <- ""
  object@comparison_data_frame <- data.frame()
  object@comparison_r2_title <- ""
  object@comparison_r2_subtitle <- ""
  object@comparison_r2_file_name <- ""
  object@comparison_r2_data_frame <- data.frame()
  object@comparison_r2_model_frame <- data.frame()
  object@equation_list <- list()
  object@aqi_statistics_title <- ""
  object@aqi_statistics_subtitle <- ""
  object@aqi_statistics_file_name <- ""
  object@aqi_statistics_data_frame <- data.frame()

  coverages <- dataset_coverages(object)
  dataset_index_x <- which(coverages == dataset_x_name)
  dataset_index_y <- which(coverages == dataset_y_name)

  if (dataset_index_x > 0L && dataset_index_y > 0L) {
    dataset_x <- dataset(object, dataset_index_x)
    dataset_y <- dataset(object, dataset_index_y)

    # Possibly change variable column in (shallow) copy of dataset_x/y:

    data_frame_x <- data_frame(dataset_x)
    column_names_x <- colnames(data_frame_x)
    variable_column_x <- which(column_names_x == dataset_x_variable)

    if (variable_column(dataset_x) != variable_column_x) {
      variable_column(dataset_x) <- variable_column_x
    }

    data_frame_y <- data_frame(dataset_y)
    column_names_y <- colnames(data_frame_y)
    variable_column_y <- which(column_names_y == dataset_y_variable)

    if (variable_column(dataset_y) != variable_column_y) {
      variable_column(dataset_y) <- variable_column_y
    }

    source_variable_x <- source_variable(dataset_x)
    units_x <- variable_units(dataset_x)
    measure_column_x <- variable_column(dataset_x)

    source_variable_y <- source_variable(dataset_y)
    units_y <- variable_units(dataset_y)
    columns_y <- ncol(data_frame_y)
    measure_column_y <- variable_column(dataset_y)

    model_timesteps <- timesteps(object)
    first_model_timestamp <- first_timestamp(object)
    last_model_timestamp <- last_timestamp(object)
    averaging <- "Hourly"
    model_timestep_size <- object@timestep_size

    if (model_timestep_size == "days") {
      averaging <- "Daily"
    }

    date_range <- substr(first_model_timestamp, 1L, 10L)

    if (object@days > 1L) {
      date_range <-
        paste0(date_range, " - ", substr(last_model_timestamp, 1L, 10L))
    }

    object@comparison_title <-
      sprintf("Neighboring %s Point Measures (<= %0.0fm) Comparison: %s vs %s",
              averaging, object@maximum_neighbor_distance,
              source_variable_x, source_variable_y)

    object@comparison_r2_title <-
      gsub(fixed = TRUE, " Comparison: ", " Comparision R2: ",
           paste0("Unflagged ", object@comparison_title))

    object@aqi_statistics_title <-
      gsub(fixed = TRUE, " Comparison: ", " AQI Category Statistics: ",
           paste0("Unflagged ", object@comparison_title))

    west <- west_bound(dataset_x)
    east <- east_bound(dataset_x)
    south <- south_bound(dataset_x)
    north <- north_bound(dataset_x)
    object@comparison_subtitle <-
      sprintf("%s (%0.4f, %0.4f) - (%0.4f, %0.4f)",
              date_range, west, east, south, north)
    object@comparison_r2_subtitle <- object@comparison_subtitle
    object@aqi_statistics_subtitle <- object@comparison_subtitle

    x_label <- paste0(source_variable_x, "(", units_x, ")")
    y_label <- paste0(source_variable_y, "(", units_y, ")")


    # Before (re)computing neighbor flags, clear dataset_y of neighbor flags
    # that may have been previously computed by ASNAT_flag_y_neighbors().
    # This is necessary to make this function idempotent.

    flagged_column_y <- ASNAT_flagged_column_index(column_names_y)
    flagged_y <- data_frame_y[[flagged_column_y]]
    flagged_y <- vapply(flagged_y, ASNAT_exclude_flag,
                        c(""), 80L, USE.NAMES = FALSE)
    flagged_y <- vapply(flagged_y, ASNAT_exclude_flag,
                        c(""), 81L, USE.NAMES = FALSE)
    flagged_y <- vapply(flagged_y, ASNAT_exclude_flag,
                        c(""), 82L, USE.NAMES = FALSE)
    data_frame_y[flagged_column_y] <- flagged_y

    # Update stored dataset_y with modified data_frame_y:

    object@dataset_manager <-
      replace_flagged_column_values(object@dataset_manager,
                                    dataset_index_y,
                                    data_frame_y[[flagged_column_y]])

    # Compute neighbor indices (expensive):

    is_hourly <- object@timestep_size == "hours"

    x_y_indices <-
      ASNAT_compare_datasets(data_frame_x, data_frame_y,
                             object@maximum_neighbor_distance, is_hourly)

    indices_x <- x_y_indices$x
    indices_y <- x_y_indices$y

    if (length(indices_x) > 0L) {
      timestamps_x <- data_frame_x[indices_x, 1L]
      timestamps_y <- data_frame_y[indices_y, 1L]
      measures_x <- data_frame_x[indices_x, measure_column_x]
      measures_y <- data_frame_y[indices_y, measure_column_y]
      site_column_x <- ASNAT_site_column_index(column_names_x)
      site_column_y <- ASNAT_site_column_index(column_names_y)
      sites_x <- as.integer(data_frame_x[indices_x, site_column_x])
      sites_y <- as.integer(data_frame_y[indices_y, site_column_y])
      longitudes_x <- data_frame_x[indices_x, 2L]
      latitudes_x <- data_frame_x[indices_x, 3L]
      longitudes_y <- data_frame_y[indices_y, 2L]
      latitudes_y <- data_frame_y[indices_y, 3L]
      coverage_source_dataset_x <- coverage_source(dataset_x)
      coverage_source_dataset_y <- coverage_source(dataset_y)
      x_id_label <- paste0(coverage_source_dataset_x, ".id(-)")
      y_id_label <- paste0(coverage_source_dataset_y, ".id(-)")
      y_flagged_label <- paste0(coverage_source_dataset_y, ".flagged(-)")
      flagged_y <- data_frame_y[indices_y, flagged_column_y]

      maximum_difference <-
        if (object@apply_maximum_neighbor_value_difference) object@maximum_neighbor_value_difference else
        -1.0
      maximum_percent_difference <-
        if (object@apply_maximum_neighbor_value_percent_difference) object@maximum_neighbor_value_percent_difference else
        -1.0
      minimum_r_squared <-
        if (object@apply_minimum_neighbor_value_r_squared) object@minimum_neighbor_value_r_squared else
        -1.0

      flagged_result <-
        ASNAT_flag_y_neighbors(timestamps_x, measures_x, measures_y, sites_y,
                               flagged_y,
                               maximum_difference,
                               maximum_percent_difference,
                               minimum_r_squared)

      flagged_y <- flagged_result$flagged_y

      if (flagged_result$updated) {

        # Note a dataset_y measure can have multiple dataset_x neighbor measures
        # and the dataset_y measure can have multiple flags
        # (because multiple dataset_x neighbor measures have different values)
        # so update dataset_y flagged column with all of the (non-0) flags.
        # E.g., if 2 AQS points have values 1.0 and 2.0 and a single PurpleAir
        # neighbor measure with value 7.0 and we're flagging 80 if > 5 (ug/m3)
        # then the PurpleAir neighbor would have flag 80 from the first
        # AQS measure but flag 0 from the second AQS measure.
        # So which flag to store?
        # Answer: 80 because we want to indicate that the PurpeAir measure
        # differs by more than 5 (ug/m3) from at least one neighbor AQS measure.

        for (flag_index in seq_along(indices_y)) {
          flags_y_string <- flagged_y[[flag_index]]

          if (flags_y_string != "0") {
            flags_y_vector <- unlist(strsplit(flags_y_string, ";"))

            for (flag in flags_y_vector) {
              neighbor_flag_index <- indices_y[[flag_index]]
              current_flags <-
                data_frame_y[neighbor_flag_index, flagged_column_y]
              new_flags <- ASNAT_include_flag(current_flags, flag)
              data_frame_y[neighbor_flag_index, flagged_column_y] <- new_flags
            }
          }
        }

        # Update stored dataset_y with modified data_frame_y:

        object@dataset_manager <-
          replace_flagged_column_values(object@dataset_manager,
                                        dataset_index_y,
                                        data_frame_y[[flagged_column_y]])
      }

      # Optional: get timestamp-site matched measures_z:

      matched_measures_z <- NULL
      z_label <- NULL

      if (!is.null(dataset_z_name)) {
        dataset_index_z <- which(coverages == dataset_z_name)
        dataset_z <- dataset(object, dataset_index_z)

        source_x <- coverage_source(dataset_x)
        source_y <- coverage_source(dataset_y)
        source_z <- coverage_source(dataset_z)

        match_sites <- NULL
        timestamp_length <- 13L

        if (object@timestep_size == "days") {
          timestamp_length <- 10L
        }

        if (source_z == source_x) {
          match_sites <- sites_x
          match_timestamps <- substr(timestamps_x, 1L, timestamp_length)
        } else if (source_z == source_y) {
          match_sites <- sites_y
          match_timestamps <- substr(timestamps_y, 1L, timestamp_length)
        }

        if (!is.null(match_sites)) {
          data_frame_z <- data_frame(dataset_z)
          column_names_z <- colnames(data_frame_z)
          variable_column_z <- which(column_names_z == dataset_z_variable)

          if (variable_column(dataset_z) != variable_column_z) {
            variable_column(dataset_z) <- variable_column_z
          }

          source_variable_z <- source_variable(dataset_z)
          units_z <- variable_units(dataset_z)
          z_label <- paste0(source_variable_z, "(", units_z, ")")
          measure_column_z <- variable_column(dataset_z)
          site_column_z <- ASNAT_site_column_index(column_names_z)
          timestamps_z <- data_frame_z[, 1L]

          timestamps_z <- substr(timestamps_z, 1L, timestamp_length)
          
          sites_z <- as.integer(data_frame_z[, site_column_z])
          measures_z <- data_frame_z[, measure_column_z]
          count <- length(match_sites)
          matched_measures_z <- rep(NA, count)
          sites_z_matched <- rep(NA, count)
          longitudes_z <- data_frame_z[, 2L]
          latitudes_z <- data_frame_z[, 3L]

          for (row in 1L:count) {
            timestamp <- match_timestamps[[row]]
            site <- match_sites[[row]]
            matched_rows <-
              which(timestamps_z == timestamp & sites_z == site)

            if (length(matched_rows) > 0L) {
              row_index <- matched_rows[[1L]]
              matched_measures_z[[row]] <- measures_z[[row_index]]
              sites_z_matched[[row]] <- sites_z[[row_index]]
            }
          }
        }
      }

      if (!is.null(matched_measures_z)) {
        object@comparison_data_frame <-
          data.frame(timestamps_x, sites_x, measures_x, sites_y, measures_y,
                     flagged_y, matched_measures_z)
        colnames(object@comparison_data_frame) <-
          c("timestamp(UTC)", x_id_label, x_label, y_id_label, y_label,
            y_flagged_label, z_label)
      } else {
        object@comparison_data_frame <-
          data.frame(timestamps_x, sites_x, measures_x, sites_y, measures_y,
                     flagged_y)
        colnames(object@comparison_data_frame) <-
          c("timestamp(UTC)", x_id_label, x_label, y_id_label, y_label,
            y_flagged_label)
      }

      directory <- data_directory(object@dataset_manager)
      file_format <- "tsv"
      object@comparison_file_name <-
        ASNAT_comparison_file_name(source_variable_x, source_variable_y,
                                   start_date(dataset_x), end_date(dataset_x),
                                   directory, file_format)

      output_file <- file(object@comparison_file_name, "wb")

      if (inherits(output_file, "connection") &&
          file.exists(object@comparison_file_name)) {

        # Write 2 lines before header:

        cat(sep = "", file = output_file,
            object@comparison_title, "\n",
            object@comparison_subtitle, "\n")

        # Write header line followed by data rows:

        try(silent = TRUE,
            write.table(object@comparison_data_frame, file = output_file,
                        sep = "\t", quote = FALSE, row.names = FALSE,
                        na = ASNAT_output_missing_string))
        close(output_file)
        object@ok <- TRUE
      }

      # Also, compute and write comparison_r2 table/file of unflagged measures:

      if (object@ok) {
        object@ok <- FALSE
        unique_sites_x <- unique(sort.int(sites_x))
        unique_sites_y <- unique(sort.int(sites_y))

        # Compute number of paired sites:

        count <- 0L

        for (site_x in unique_sites_x) {

          for (site_y in unique_sites_y) {
            paired_sites_rows <-
                which(sites_x == site_x & sites_y == site_y & flagged_y == "0")

            n <- length(paired_sites_rows)

            if (n > 0L) {
              count <- count + 1L
            }
          }
        }

        # Allocate vector outputs:

        x_ids <- rep(0L, count)
        x_longitudes <- rep(NA, count)
        x_latitudes <- rep(NA, count)
        y_ids <- rep(0L, count)
        y_longitudes <- rep(NA, count)
        y_latitudes <- rep(NA, count)
        distances <- rep(NA, count)
        r2s <- rep(NA, count)
        equations <- rep(NULL, count)

        if (!is.null(matched_measures_z)) {
          z_ids <- rep(0L, count)
          z_longitudes <- rep(NA, count)
          z_latitudes <- rep(NA, count)
          z_distances <- rep(NA, count)
        }

        quadratic_regression <- FALSE
        cubic_regression <- FALSE

        if (!is.null(regression_method)) {

          if (regression_method == "Quadratic") {
            quadratic_regression <- TRUE
          } else if (regression_method == "Cubic") {
            cubic_regression <- TRUE
          }
        }

        index <- 0L

        # Data rows will be sorted by X sites:

        for (site_x in unique_sites_x) {

          for (site_y in unique_sites_y) {

            # if single variable regression
            if (object@correction_selection == 0L) {
              site_rows <-
                which(sites_x == site_x & sites_y == site_y & flagged_y == "0")

              if (length(site_rows) > 0L) {
                site_measures_x <- measures_x[site_rows]
                site_measures_y <- measures_y[site_rows]
                r_squared <- 0.0
                lm_model <- list()
                sample_size <- length(site_measures_x)

                if (sample_size > 1L) {

                  if (quadratic_regression) {
                    lm_model <- lm(site_measures_y ~ site_measures_x + I(site_measures_x^2))
                    r_squared <- summary(lm_model)$r.squared
                  } else if (cubic_regression) {
                    # For cubic R-squared
                    lm_model <- lm(site_measures_y ~ site_measures_x + I(site_measures_x^2) + I(site_measures_x^3))
                    r_squared <- summary(lm_model)$r.squared
                  } else {
                    lm_model <- lm(site_measures_y ~ site_measures_x)
                    r_squared <- summary(lm_model)$r.squared
                  }
                }

                # Compute column values:

                site_longitudes_x <- longitudes_x[site_rows]
                site_latitudes_x <- latitudes_x[site_rows]
                site_longitudes_y <- longitudes_y[site_rows]
                site_latitudes_y <- latitudes_y[site_rows]
                longitude_x <- site_longitudes_x[[1L]]
                latitude_x <- site_latitudes_x[[1L]]
                longitude_y <- site_longitudes_y[[1L]]
                latitude_y <- site_latitudes_y[[1L]]
                distance <-
                  ASNAT_distance_meters(longitude_x, latitude_x,
                                        longitude_y, latitude_y)

                # Store column values:

                index <- index + 1L
                x_ids[[index]] <- as.integer(site_x)
                x_longitudes[[index]] <- longitude_x
                x_latitudes[[index]] <- latitude_x
                y_ids[[index]] <- as.integer(site_y)
                y_longitudes[[index]] <- longitude_y
                y_latitudes[[index]] <- latitude_y
                distances[[index]] <- distance
                r2s[[index]] <- r_squared
                equations[[index]] <- lm_model
              }

            } else if (!is.null(matched_measures_z)) {
              # multiple variable regression and matched_measures_z exists
              coverage_source_dataset_z <- coverage_source(dataset_z)
              z_id_label <- paste0(coverage_source_dataset_z, ".id(-)_z")
              # object@correction_selection == 1 or 2
              # object@correction_selection == 1 is additive
              # object@correction_selection == 2 is interactive

              site_rows <-
                which(sites_x == site_x & sites_y == site_y & flagged_y == "0")

              if (length(site_rows) > 0L &&
                  sum(!is.na(measures_x[site_rows])) /
                  length(measures_x[site_rows]) >= 0.5 &&
                  sum(!is.na(measures_y[site_rows])) /
                  length(measures_y[site_rows]) >= 0.5 &&
                  sum(!is.na(matched_measures_z[site_rows])) /
                  length(matched_measures_z[site_rows]) >= 0.5) {

                site_measures_x <- measures_x[site_rows]
                site_measures_y <- measures_y[site_rows]
                site_measures_z <- matched_measures_z[site_rows]
                r_squared <- 0.0
                lm_model <- NULL
                sample_size <- length(site_measures_x)

                if (sample_size > 15L && !all(is.na(site_measures_z))) {
                  
                  if (object@correction_selection == 1L) {
                    # Additive

                    if (quadratic_regression) {
                      lm_model <- lm(site_measures_y ~ site_measures_x + I(site_measures_x^2) + site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    } else if (cubic_regression) {
                      # For cubic R-squared
                      lm_model <- lm(site_measures_y ~ site_measures_x + I(site_measures_x^2) + I(site_measures_x^3) + site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    } else {
                      lm_model <- lm(site_measures_y ~ site_measures_x + site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    }
                  } else if (object@correction_selection == 2L) {
                    # interactive
                  
                    if (quadratic_regression) {
                      lm_model <- lm(site_measures_y ~ (site_measures_x + I(site_measures_x^2)) * site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    } else if (cubic_regression) {
                      # For cubic R-squared
                      lm_model <- lm(site_measures_y ~ (site_measures_x + I(site_measures_x^2) + I(site_measures_x^3)) * site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    } else {
                      lm_model <- lm(site_measures_y ~ site_measures_x * site_measures_z)
                      # coefficients <- coef(lm_model)
                      r_squared <- summary(lm_model)$r.squared
                    }
                  }

                  # Compute column values:

                  site_longitudes_x <- longitudes_x[site_rows]
                  site_latitudes_x <- latitudes_x[site_rows]
                  site_longitudes_y <- longitudes_y[site_rows]
                  site_latitudes_y <- latitudes_y[site_rows]
                  site_z <- sites_z_matched[site_rows]
                  site_z <- unique(na.omit(site_z))
                  longitude_x <- site_longitudes_x[[1L]]
                  latitude_x <- site_latitudes_x[[1L]]
                  longitude_y <- site_longitudes_y[[1L]]
                  latitude_y <- site_latitudes_y[[1L]]
                  distance <-
                    ASNAT_distance_meters(longitude_x, latitude_x,
                                          longitude_y, latitude_y)

                  # Store column values:

                  index <- index + 1L
                  x_ids[[index]] <- as.integer(site_x)
                  x_longitudes[[index]] <- longitude_x
                  x_latitudes[[index]] <- latitude_x
                  y_ids[[index]] <- as.integer(site_y)
                  y_longitudes[[index]] <- longitude_y
                  y_latitudes[[index]] <- latitude_y
                  distances[[index]] <- distance
                  r2s[[index]] <- r_squared
                  equations[[index]] <- lm_model
                  z_ids[[index]] <- as.integer(site_z)
                }
              }
            }
          }
        }

        if (length(x_ids) > 0L) {
          x_ids <- unlist(x_ids)
          x_longitudes <- unlist(x_longitudes)
          x_latitudes <- unlist(x_latitudes)
          y_ids <- unlist(y_ids)
          y_longitudes <- unlist(y_longitudes)
          y_latitudes <- unlist(y_latitudes)
          distances <- unlist(distances)
          r2s <- unlist(r2s)

          x_longitude_label <-
            paste0(coverage_source_dataset_x, ".longitude(deg)")
          x_latitude_label <-
            paste0(coverage_source_dataset_x, ".latitude(deg)")
          y_longitude_label <-
            paste0(coverage_source_dataset_y, ".longitude(deg)")
          y_latitude_label <-
            paste0(coverage_source_dataset_y, ".latitude(deg)")

          if (object@correction_selection == 1L ||
              object@correction_selection == 2L) {
          # multivariable regression
            z_ids <- unlist(z_ids)
            object@comparison_r2_data_frame <-
              data.frame(x_ids, x_longitudes, x_latitudes,
                        y_ids, y_longitudes, y_latitudes,
                        distances, r2s, z_ids)

            colnames(object@comparison_r2_data_frame) <-
              c(x_id_label, x_longitude_label, x_latitude_label,
                y_id_label, y_longitude_label, y_latitude_label,
                "distance(m)", "R2(-)", z_id_label)
            object@comparison_r2_model_frame <- data.frame(x_ids, y_ids, z_ids, distances, r2s)
          } else {
            # single variable regression

            object@comparison_r2_data_frame <-
              data.frame(x_ids, x_longitudes, x_latitudes,
                        y_ids, y_longitudes, y_latitudes,
                        distances, r2s)

            colnames(object@comparison_r2_data_frame) <-
              c(x_id_label, x_longitude_label, x_latitude_label,
                y_id_label, y_longitude_label, y_latitude_label,
                "distance(m)", "R2(-)")
            object@comparison_r2_model_frame <- data.frame(x_ids, y_ids, distances, r2s)
          }

          # object@equation_list <- list(equations, x_measurements, y_measurements)
          object@equation_list <- equations

          directory <- data_directory(object@dataset_manager)
          file_format <- "tsv"
          object@comparison_r2_file_name <-
            gsub(fixed = TRUE, "_comparison_", "_comparison_r2_",
                 object@comparison_file_name)

          output_file <- file(object@comparison_r2_file_name, "wb")

          if (inherits(output_file, "connection") &&
              file.exists(object@comparison_r2_file_name)) {

            # Write 2 lines before header:

            cat(sep = "", file = output_file,
                object@comparison_r2_title, "\n",
                object@comparison_r2_subtitle, "\n")

            # Write header line followed by data rows:

            try(silent = TRUE,
                write.table(object@comparison_r2_data_frame, file = output_file,
                            sep = "\t", quote = FALSE, row.names = FALSE,
                            na = ASNAT_output_missing_string))
            close(output_file)
            object@ok <- TRUE
          }
        }
      }

      # Also, compute and write aqi_statistics file:

      if (object@ok &&
          ASNAT_is_aqi_compatible(dataset_x_variable, dataset_y_variable)) {
        object@ok <- FALSE
        unflagged_rows <- which(flagged_y == "0")
        unflagged_measures_x <- measures_x[unflagged_rows]
        unflagged_measures_y <- measures_y[unflagged_rows]
        is_hourly <- object@timestep_size == "hours"
        object@aqi_statistics_data_frame <-
          ASNAT_neighbor_aqi_statistics(dataset_x_variable, is_hourly,
                                        unflagged_measures_x,
                                        unflagged_measures_y)

        directory <- data_directory(object@dataset_manager)
        file_format <- "tsv"
        object@aqi_statistics_file_name <-
          gsub(fixed = TRUE, "_comparison_", "_aqi_statistics_",
               object@comparison_file_name)

        output_file <- file(object@aqi_statistics_file_name, "wb")

        if (inherits(output_file, "connection") &&
            file.exists(object@aqi_statistics_file_name)) {

          # Write 2 lines before header:

          cat(sep = "", file = output_file,
              object@aqi_statistics_title, "\n",
              object@aqi_statistics_subtitle, "\n")

          # Write header line followed by data rows:

          try(silent = TRUE,
              write.table(object@aqi_statistics_data_frame, file = output_file,
                          sep = "\t", quote = FALSE, row.names = FALSE,
                          na = ASNAT_output_missing_string))
          close(output_file)
          object@ok <- TRUE
        }
      }
    }
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("compare_datasets:", timer)
  return(object)
})



# Retrieve data variable subset by time and lon-lat rectangle from rsigserver
# and write it to tab-delimited ASCII file in $HOME/ASNAT/data/tmp/
# then load the data then return the object.
# Example:
# asnat_model <<- retrieve_data(asnat_model, "PurpleAir.pm25_corrected")
#
# if (ok(asnat_model)) {
#   last <- dataset_count(asnat_model)
#   dataset <- dataset(asnat_model, last)
#   print(data_frame(dataset))
# }

ASNAT_declare_method("ASNAT_Model", "retrieve_data",
function(object, coverage) {
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  aggregate <- "hourly"

  if (object@timestep_size == "days") {
    aggregate <- "daily"
  }

  end_date <- object@start_date + (object@days - 1L)
  object@dataset_manager <-
    retrieve_data(object@dataset_manager,
                  coverage,
                  object@start_date, end_date,
                  aggregate,
                  object@west_bound, object@east_bound,
                  object@south_bound, object@north_bound,
                  object@purple_air_key,
                  object@purple_air_sensor,
                  object@aqs_pm25_codes)
  object@ok <- ok(object@dataset_manager)

  # Write retrieved URLs to log file:

  if (object@ok) {
    the_retrieved_urls <- retrieved_urls(object)
    the_log_file_name <- paste0(ASNAT_home_path(), "/ASNAT_log.txt")
    cat(append = TRUE, sep = "\n", file = the_log_file_name, the_retrieved_urls)
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("retrieve_data:", timer)
  return(object)
})



# Get vector of purple air site and note strings in bounding area.
# Example:
# sites <- purple_air_sites(asnat_model)
# print(sites)
#

ASNAT_declare_method("ASNAT_Model", "purple_air_sites",
function(object) {
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  ASNAT_dprint("nrow(object@purple_air_sites_data_frame) = %d\n",
               nrow(object@purple_air_sites_data_frame))
  result <- NULL

  if (nrow(object@purple_air_sites_data_frame) > 0L) {
    matched_rows <-
      which(object@purple_air_sites_data_frame[[1L]] >= object@west_bound &
            object@purple_air_sites_data_frame[[1L]] <= object@east_bound &
            object@purple_air_sites_data_frame[[2L]] >= object@south_bound &
            object@purple_air_sites_data_frame[[2L]] <= object@north_bound)

    row_count <- length(matched_rows)

    if (row_count > 0L) {
      data_frame <- object@purple_air_sites_data_frame[matched_rows, ]
      result <- paste(data_frame[[3L]], data_frame[[4L]])
    }
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("purple_air_sites:", timer)
  return(result)
})



# Get vector of dataset_x neighbor site and note strings.
# Example:
# sites <- dataset_x_neighbors_id_note(asnat_model, "AQS.pm25")
# print(sites)
#

ASNAT_declare_method("ASNAT_Model", "dataset_x_neighbors_id_note",
function(object, dataset_x_name) {
  stopifnot(nchar(dataset_x_name) > 0L)
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  ASNAT_dprint("nrow(object@comparison_data_frame) = %d, dataset_x_name = %s\n",
               nrow(object@comparison_data_frame), dataset_x_name)
  result <- NULL

  if (nrow(object@comparison_data_frame) > 0L) {
    neighbor_ids <- object@comparison_data_frame[[2L]]
    neighbor_ids <- unique(sort.int(neighbor_ids))
    dataset_x <- find_dataset(object, dataset_x_name)

    if (!is.null(dataset_x)) {
      data_frame_x <- data_frame(dataset_x)
      column_names <- colnames(data_frame_x)
      id_column <- ASNAT_site_column_index(column_names)
      notes_column <- ncol(data_frame_x)

      for (neighbor_id in neighbor_ids) {
        matched_rows <- which(data_frame_x[[id_column]] == neighbor_id)
        row_count <- length(matched_rows)

        if (row_count > 0L) {
          matched_data_frame_x <- data_frame_x[matched_rows, ]
          neighbor_notes <- matched_data_frame_x[[notes_column]]
          neighbor_note <- neighbor_notes[[1L]]
          id_note <- paste(neighbor_id, neighbor_note)
          result <- append(result, id_note)
        }
      }
    }
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("dataset_x_neighbors_id_note:", timer)
  return(result)
})



# Append a dataset read from a file.

ASNAT_declare_method("ASNAT_Model", "append_data_frame",
function(object, data_frame, name) {
  ASNAT_check(methods::validObject(object))
  stopifnot(!is.null(data_frame))
  stopifnot(class(data_frame) == "data.frame")
  stopifnot(ncol(data_frame) > 4L)
  stopifnot(nrow(data_frame) > 0L)
  stopifnot(ASNAT_is_valid_dataset_name(name))

  aggregate <- if (object@timestep_size == "hours") "hourly" else "daily"

  object@dataset_manager <-
    append_data_frame(object@dataset_manager, data_frame, aggregate, name)
  object@ok <- ok(object@dataset_manager)

  ASNAT_check(methods::validObject(object))
  return(object)
})



# Save datasets to specified output_directory:
# Example:
# asnat_model <<- save_datasets(asnat_model, "csv")
#
# if (ok(asnat_model)) {
#   list.files(output_directory(asnat_model), pattern = "*.txt")
# }

ASNAT_declare_method("ASNAT_Model", "save_datasets",
function(object) {
  timer <- ASNAT_start_timer()
  ASNAT_check(methods::validObject(object))
  object@dataset_manager <-
    save_datasets(object@dataset_manager, object@output_directory,
                  object@output_format)

  if (object@output_format == "kml") return(object) # UNIMPLEMENTED kml output

  # Also write summary files if computed:

  object@saved_files <- vector()
  count <- 0L

  column_delimiter <- NULL

  if (object@output_format == "csv") {
    column_delimiter <- ","
  } else if (object@output_format == "tsv") {
    column_delimiter <- "\t"
  }

  if (nrow(object@summary_x_data_frame) > 0L) {
    output_file_name <-
      paste0(object@output_directory, "/", basename(object@summary_x_file_name))
    output_file_name <-
      substr(output_file_name, 1L, nchar(output_file_name) - 3L)
    output_file_name <- paste0(output_file_name, object@output_format)
    ASNAT_dprint("Writing summary file %s.\n", output_file_name)
    output_file <- file(output_file_name, "wb")

    if (!is.null(column_delimiter)) {
      cat(sep = "", file = output_file,
          object@summary_x_title, "\n",
          object@summary_x_subtitle, "\n")
      try(silent = TRUE,
          write.table(object@summary_x_data_frame, file = output_file,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
    } else if (object@output_format == "json") {
      # UNIMPLEMENTED: add title and subtitle:
      json_object <-
        try(silent = TRUE,
            jsonlite::toJSON(object@summary_x_data_frame, pretty = TRUE))

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file, json_object)
      }
    }

    close(output_file)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
  }

  if (nrow(object@summary_y_data_frame) > 0L) {
    output_file_name <-
      paste0(object@output_directory, "/", basename(object@summary_y_file_name))
    output_file_name <-
      substr(output_file_name, 1L, nchar(output_file_name) - 3L)
    output_file_name <- paste0(output_file_name, object@output_format)
    ASNAT_dprint("Writing summary file %s.\n", output_file_name)
    output_file <- file(output_file_name, "wb")

    if (!is.null(column_delimiter)) {
      cat(sep = "", file = output_file,
          object@summary_y_title, "\n",
          object@summary_y_subtitle, "\n")
      try(silent = TRUE,
          write.table(object@summary_y_data_frame, file = output_file,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
    } else if (object@output_format == "json") {
      # UNIMPLEMENTED: add title and subtitle:
      json_object <-
        jsonlite::toJSON(object@summary_y_data_frame, pretty = TRUE)

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file, json_object)
      }
    }

    close(output_file)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
  }

  if (nrow(object@comparison_data_frame) > 0L) {
    output_file_name <-
      paste0(object@output_directory, "/",
             basename(object@comparison_file_name))
    output_file_name <-
      substr(output_file_name, 1L, nchar(output_file_name) - 3L)
    output_file_name <- paste0(output_file_name, object@output_format)
    ASNAT_dprint("Writing comparison file %s.\n", output_file_name)
    output_file <- file(output_file_name, "wb")

    if (!is.null(column_delimiter)) {
      cat(sep = "", file = output_file,
          object@comparison_title, "\n",
          object@comparison_subtitle, "\n")
      try(silent = TRUE,
          write.table(object@comparison_data_frame, file = output_file,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
    } else if (object@output_format == "json") {
      # UNIMPLEMENTED: add title and subtitle:
      json_object <-
        try(silent = TRUE,
            jsonlite::toJSON(object@comparison_data_frame, pretty = TRUE))

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file, json_object)
      }
    }

    close(output_file)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
  }

  if (nrow(object@comparison_r2_data_frame) > 0L) {
    output_file_name <-
      paste0(object@output_directory, "/",
             basename(object@comparison_r2_file_name))
    output_file_name <-
      substr(output_file_name, 1L, nchar(output_file_name) - 3L)
    output_file_name <- paste0(output_file_name, object@output_format)
    ASNAT_dprint("Writing comparison file %s.\n", output_file_name)
    output_file <- file(output_file_name, "wb")

    if (!is.null(column_delimiter)) {
      cat(sep = "", file = output_file,
          object@comparison_r2_title, "\n",
          object@comparison_r2_subtitle, "\n")
      try(silent = TRUE,
          write.table(object@comparison_r2_data_frame, file = output_file,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
    } else if (object@output_format == "json") {
      # UNIMPLEMENTED: add title and subtitle:
      json_object <-
        try(silent = TRUE,
            jsonlite::toJSON(object@comparison_r2_data_frame, pretty = TRUE))

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file, json_object)
      }
    }

    close(output_file)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
  }

  if (nrow(object@aqi_statistics_data_frame) > 0L) {
    output_file_name <-
      paste0(object@output_directory, "/",
             basename(object@aqi_statistics_file_name))
    output_file_name <-
      substr(output_file_name, 1L, nchar(output_file_name) - 3L)
    output_file_name <- paste0(output_file_name, object@output_format)
    ASNAT_dprint("Writing comparison file %s.\n", output_file_name)
    output_file <- file(output_file_name, "wb")

    if (!is.null(column_delimiter)) {
      cat(sep = "", file = output_file,
          object@aqi_statistics_title, "\n",
          object@aqi_statistics_subtitle, "\n")
      try(silent = TRUE,
          write.table(object@aqi_statistics_data_frame, file = output_file,
                      sep = column_delimiter, quote = FALSE, row.names = FALSE,
                      na = ASNAT_output_missing_string))
    } else if (object@output_format == "json") {
      # UNIMPLEMENTED: add title and subtitle:
      json_object <-
        try(silent = TRUE,
            jsonlite::toJSON(object@aqi_statistics_data_frame, pretty = TRUE))

      if (class(json_object) == "json") {
        cat(sep = "", file = output_file, json_object)
      }
    }

    close(output_file)
    count <- count + 1L
    object@saved_files[count] <- output_file_name
  }

  ASNAT_check(methods::validObject(object))
  ASNAT_elapsed_timer("save_datasets:", timer)
  return(object)
})



# Save state:
# Example:
#   asnat_model <<- save_state(asnat_model)
#   print(ok(asnat_model))

ASNAT_declare_method("ASNAT_Model", "save_state",
function(object) {
  ASNAT_check(methods::validObject(object))
  object@ok <- FALSE

  save_state_file <- paste0(ASNAT_home_path(), "/.asnat")

  if (!file.exists(save_state_file)) {
    file.create(save_state_file)
  }

  if (file.exists(save_state_file)) {
    ASNAT_dprint("Writing save state file = '%s'\n", save_state_file)
    cat(sep = "", file = save_state_file,
        "output_directory ", object@output_directory, "\n",
        "output_format ", object@output_format, "\n",
        "start_date ", format(object@start_date, "%Y-%m-%d"), "\n",
        "days ", object@days, "\n",
        "timestep_size ", object@timestep_size, "\n",
        "west_bound ", object@west_bound, "\n",
        "east_bound ", object@east_bound, "\n",
        "south_bound ", object@south_bound, "\n",
        "north_bound ", object@north_bound, "\n",
        "purple_air_key ", object@purple_air_key, "\n",
        "purple_air_sensor ", object@purple_air_sensor, "\n",
        "aqs_pm25_codes ", object@aqs_pm25_codes, "\n",
        "legend_colormap ", object@legend_colormap, "\n",
        "show_mean_values_on_map ", object@show_mean_values_on_map, "\n",
        "use_fancy_labels ", object@use_fancy_labels, "\n",
        "use_interactive_plots ", object@use_interactive_plots, "\n",
        "show_site_labels ", object@show_site_labels, "\n",
        "maximum_neighbor_distance ", object@maximum_neighbor_distance, "\n",
        "apply_maximum_neighbor_value_difference ", object@apply_maximum_neighbor_value_difference, "\n",
        "maximum_neighbor_value_difference ", object@maximum_neighbor_value_difference, "\n",
        "apply_maximum_neighbor_value_percent_difference ", object@apply_maximum_neighbor_value_percent_difference, "\n",
        "maximum_neighbor_value_percent_difference ", object@maximum_neighbor_value_percent_difference, "\n",
        "apply_minimum_neighbor_value_r_squared ", object@apply_minimum_neighbor_value_r_squared, "\n",
        "minimum_neighbor_value_r_squared ", object@minimum_neighbor_value_r_squared, "\n",
        "apply_constant_value_flag ", object@apply_constant_value_flag, "\n",
        "apply_long_missing_flag ", object@apply_long_missing_flag, "\n",
        "apply_outlier_stat_flag ", object@apply_outlier_stat_flag, "\n",
        "outlier_threshold ", object@outlier_threshold, "\n",
        "outlier_time_window ", object@outlier_time_window, "\n",
        "outlier_start_timestamps ", object@outlier_start_timestamps, "\n",
        "outlier_end_timestamps ", object@outlier_end_timestamps, "\n",
        "constant_value_threshold", object@constant_value_threshold, "\n",
        "long_missing_threshold ", object@long_missing_threshold, "\n"
      )
    object@ok <- TRUE
  }

  ASNAT_check(methods::validObject(object))
  return(object)
})



# Remove the temporary unique data directory when running remote-hosted.

ASNAT_declare_method("ASNAT_Model", "remove_temporary_directory",
function(object) {
  ASNAT_check(methods::validObject(object))

  if (ASNAT_is_remote_hosted) {

    if (object@output_directory != ASNAT_home_path()) {
      unlink(object@output_directory)
    }

    object@dataset_manager <- remove_temporary_directory(object@dataset_manager)
  }

  return(object)
})



