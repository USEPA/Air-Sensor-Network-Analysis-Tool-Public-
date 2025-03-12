###############################################################################
# PURPOSE: ASNAT_tester.R - Test ASNAT code.
# NOTES:   To run:
#  Rscript ASNAT_tester.R
# HISTORY: 2022-10-12 plessel.todd@epa.gov
# STATUS:  unreviewed tested
###############################################################################

.unused <- require(compiler, quietly = TRUE) && compiler::enableJIT(3)

###############################################################################
# Load required source files:
###############################################################################

source("../ASNAT_Utilities.R", chdir = TRUE) # For ASNAT_dprint().
source("../ASNAT_Model.R", chdir = TRUE)     # For class ASNAT_Model.

###############################################################################

# Tester:

ASNAT_tester <- function() {
  coverages <- c(
    "AirNow.pm25",
    "AQS.pm25",
    "PurpleAir.pm25_corrected",
    "AirNow.rh",
    "AQS.rh",
    "PurpleAir.humidity",
    "METAR.relativeHumidity"
  )

  ASNAT_dprint("ASNAT_home_path() = %s\n", ASNAT_home_path())
  ASNAT_dprint("Instantiating ASNAT_Model.\n")
  model <- ASNAT_Model()
  newer_available <- is_newer_version_available(model)
  cat("is_newer_version_available = ", newer_available, "\n")
  url <- download_url(model)
  cat("download_url = ", url, "\n")
  start_date(model) <- as.Date("2022-07-01")
  days(model) <- 1L
  timestep_size(model) <- "hours"
  total_timesteps <- timesteps(model)
  output_directory <- output_directory(model)
  output_format <- output_format(model)
  delta_meters <- maximum_neighbor_distance(model)

  west_bound(model) <- -74.3
  east_bound(model) <- -72.7
  south_bound(model) <- 40.5
  north_bound(model) <- 41.4
  purple_air_key(model) <- "EPA"
  delete_datasets(model)
  ok <- FALSE
  previous_dataset <- NULL

  for (coverage in coverages) {
    ASNAT_dprint("  Retrieving coverage = %s\n", coverage)
    model <-
      retrieve_data(model,
                    coverage)
    ASNAT_dprint("  after retrieval\n")
    ok <- ok(model)
    ASNAT_dprint("  ok = %d\n", as.integer(ok))

    if (ok) {
      count <- dataset_count(model)
      ASNAT_dprint("  count = %d\n", count)
      dataset <- dataset(model, count)

      if (is.null(dataset)) {
        break
      } else {
        cat("\ndataset:\n")
        print(dataset)
        cat("data_frame:\n")
        print(data_frame(dataset))
        cat("source_variable: ", source_variable(dataset), "\n")
        cat("variable: ", variable_name(dataset), "\n")
        cat("variable_column: ", variable_column(dataset), "\n")
        cat("units: ", variable_units(dataset), "\n")
        cat("start_date: ", as.character(start_date(dataset)), "\n")
        cat("end_date: ", as.character(end_date(dataset)), "\n")
        cat("aggregation: ", aggregation(dataset), "\n")
        cat("west_bound: ", west_bound(dataset), "\n")
        cat("east_bound: ", east_bound(dataset), "\n")
        cat("south_bound: ", south_bound(dataset), "\n")
        cat("north_bound: ", north_bound(dataset), "\n")
        cat("file_name: ", file_name(dataset), "\n")
        cat("url: ", url(dataset), "\n")
        cat("note: ", note(dataset), "\n")
        cat("minimum: ", minimum(dataset), "\n")
        cat("maximum: ", maximum(dataset), "\n")

        # Compute and save summary of dataset(s):
        dataset2 <- NULL

        if (!is.null(previous_dataset) &&
            variable_units(dataset) == variable_units(previous_dataset)) {
          dataset2 <- previous_dataset
        }

        summarize_dataset(dataset, dataset2,
                          total_timesteps, delta_meters,
                          output_directory, "tsv")
        previous_dataset <- dataset
      }
    } else {
      break
    }
  }

  if (ok) {
    cat("\nTesting model:\n")
    cat("ok: ", ok(model), "\n")
    cat("app_directory: ", app_directory(model), "\n")
    cat("output_directory: ", output_directory(model), "\n")
    cat("tab: ", tab(model), "\n")
    cat("start_date: ", start_date(model), "\n")
    cat("days: ", days(model), "\n")
    cat("timestep_size: ", timestep_size(model), "\n")
    cat("timesteps: ", timesteps(model), "\n")
    cat("timestep: ", output_directory(model), "\n")
    cat("west_bound: ", west_bound(model), "\n")
    cat("east_bound: ", east_bound(model), "\n")
    cat("south_bound: ", south_bound(model), "\n")
    cat("north_bound: ", north_bound(model), "\n")
    cat("purple_air_key: ", purple_air_key(model), "\n")
    cat("purple_air_sensor: ", purple_air_sensor(model), "\n")
    cat("maximum_neighbor_distance: ", maximum_neighbor_distance(model), "\n")
    cat("timestamp: ", timestamp(model), "\n")
    cat("dataset_count: ", dataset_count(model), "\n")
    dataset_1 <- dataset(model, 1L)
    str(dataset_1)
    cat("dataset_coverages: ", dataset_coverages(model), "\n")
    cat("dataset_variables: ", dataset_variables(model), "\n")
    cat("dataset_units: ", dataset_units(model), "\n")
    cat("dataset_minimums: ", dataset_minimums(model), "\n")
    cat("dataset_maximums: ", dataset_maximums(model), "\n")
    cat("retrieved_urls: ", retrieved_urls(model), "\n")
    cat("saved_files: ", saved_files(model), "\n")

    output_directory(model) <- output_directory(model)
    output_format(model) <- output_format(model)
    tab(model) <- tab(model)
    start_date(model) <- start_date(model)
    days(model) <- days(model)
    timestep_size(model) <- timestep_size(model)
    timestep(model) <- timestep(model)
    west_bound(model) <- west_bound(model)
    east_bound(model) <- east_bound(model)
    south_bound(model) <- south_bound(model)
    north_bound(model) <- north_bound(model)
    purple_air_key(model) <- purple_air_key(model)

    coverage_1 <- source_variable(dataset_1)
    variable_1 <-
      sprintf("%s(%s)", variable_name(dataset_1), variable_units(dataset_1))
    coverage_2 <- NULL
    dataset_2 <- NULL
    variable_2 <- NULL

    if (dataset_count(model) > 1L) {
      dataset_2 <- dataset(model, 2L)
      coverage_2 <- source_variable(dataset_2)
      variable_2 <-
        sprintf("%s(%s)", variable_name(dataset_2), variable_units(dataset_2))
      ASNAT_dprint("Summarizing datasets %s %s.\n", coverage_1, coverage_2)
    } else {
      ASNAT_dprint("Summarizing dataset %s.\n", coverage_1)
    }

    model <-
      summarize_datasets(model, coverage_1, variable_1, coverage_2, variable_2)
    ASNAT_dprint("Done summarizing.\n")
    ok <- ok(model)

    if (ok) {
      cat("summary_x_file_name: ", summary_x_file_name(model))
      cat("summary_x_title: ", summary_x_title(model))
      cat("summary_x_subtitle: ", summary_x_subtitle(model))
      cat("summary_x_data_frame:\n")
      print(summary_x_data_frame(model))

      if (ncol(summary_y_data_frame(model))) {
        cat("summary_y_file_name: ", summary_y_file_name(model))
        cat("summary_y_title: ", summary_y_title(model))
        cat("summary_y_subtitle: ", summary_y_subtitle(model))
        cat("summary_y_data_frame:\n")
        print(summary_y_data_frame(model))

        if (variable_units(dataset_1) == variable_units(dataset_2)) {
          ASNAT_dprint("\nComparing datasets %s %s.\n", coverage_1, coverage_2)
          model <- compare_datasets(model, coverage_1, variable_1,
                                    coverage_2, variable_2)
          ASNAT_dprint("Done comparing.\n")
          ok <- ok(model)

          if (ok) {
            cat("comparison_file_name: ", comparison_file_name(model))
            cat("comparison_title: ", comparison_title(model))
            cat("comparison_subtitle: ", comparison_subtitle(model))
            cat("comparison_data_frame:\n")
            print(comparison_data_frame(model))
          }
        }
      }
    }

    ASNAT_dprint("Saving datasets to %s.\n", output_directory)

    for (output_file_format in c("tsv", "csv")) {
      output_format(model) <- output_file_format
      model <- save_datasets(model)
      ok <- ok(model)
      cat("ok = ", ok, "\n")

      if (!ok) {
        break
      }
    }

    if (ok) {
      ASNAT_dprint("Saving model state.\n")
      model <- save_state(model)
      ok <- ok(model)
      ASNAT_dprint("Deleting all datasets.\n")
      model <- delete_dataset_summaries(model)
      model <- delete_datasets(model)
    }
  }

  cat("ok = ", ok, "\n")
  return(ok)
}

ok <- ASNAT_tester()
ASNAT_debug(summary, warnings())

if (ok) {
  cat("\nPASSED\n")
}


