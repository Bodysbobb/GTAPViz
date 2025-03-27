#' @title Determine Output Formats for GTAP Data (Internal)
#' @description Returns a character vector of valid output formats based on the provided input.
#'
#' @param formats Input that determines output formats. Can be:
#'        - NULL: Returns default format "csv"
#'        - Character vector: Valid formats include "csv", "stata", "rds" ("r"), "txt"
#'        - List: Named list with "yes"/"no" values for valid formats
#'
#' @return A character vector of valid output format names.
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.output_format <- function(formats = NULL) {
  valid_formats <- c("csv", "stata", "rds", "r", "txt")

  if (is.null(formats)) {
    return(character(0))
  }

  if (is.character(formats)) {
    if (length(formats) == 0) {
      return(character(0))
    }

    formats <- tolower(formats)
    formats[formats == "r"] <- "rds"
    formats <- formats[formats %in% valid_formats]

    return(unique(formats))
  } else if (is.list(formats)) {
    result <- character()

    for (name in names(formats)) {
      format_name <- tolower(name)
      if (format_name %in% valid_formats || format_name == "r") {
        format_value <- tolower(as.character(formats[[name]]))
        if (format_value == "yes") {
          if (format_name == "r") format_name <- "rds"
          result <- c(result, format_name)
        }
      }
    }

    return(unique(result))
  }

  return(character(0))
}


#' @title Validate GTAP Files (Internal)
#' @description Validates input files, mapping configurations, case names, and output formats for GTAP processing.
#'
#' @param input_dir Character. Path to folder containing GTAP input files.
#' @param output_dir Character. Path where output files will be saved.
#' @param experiment Character vector. Case names (experiment names) to validate.
#' @param mapping_info Character. Mapping mode: "GTAPv7", "Yes", "No", or "Mix".
#' @param sl4var Logical. Whether SL4 data should be processed.
#' @param harvar Logical. Whether HAR data should be processed.
#' @param sl4map Data frame, NULL, or FALSE. Mapping for SL4 variables.
#' @param harmap Data frame, NULL, or FALSE. Mapping for HAR variables.
#' @param output_formats Character vector. Export formats (e.g., c("csv", "stata")).
#' @param plot_data Logical. Whether to generate plots without exporting data.
#' @param sl4_file_suffix Character. Suffix for SL4 files (e.g., "" or "-custom").
#' @param har_file_suffix Character. Suffix for HAR files (e.g., "-WEL").
#'
#' @return A list with three elements:
#' \item{status}{Character indicating validation status ("ok", "error", or "warning").}
#' \item{messages}{Character vector of validation messages.}
#' \item{proceed}{Logical indicating whether processing should continue.}
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.validate_gtap_files <- function(input_dir, output_dir,
                                 experiment, mapping_info, sl4var, harvar,
                                 sl4map, harmap, output_formats, plot_data,
                                 sl4_file_suffix = ".sl4", har_file_suffix = "-WEL.har") {

  validation_results <- list(
    status = "ok",
    messages = character(),
    proceed = TRUE
  )

  # Directory Check
  check_directory <- function(dir, dir_name) {
    if (!dir.exists(dir)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       sprintf("%s folder does not exist. Please check the path: %s", dir_name, dir))
      validation_results$proceed <- FALSE
      return(FALSE)
    }
    return(TRUE)
  }

  if (!check_directory(input_dir, "Input")) {
    return(validation_results)
  }

  # Check output directory only if we're exporting data
  if (!is.null(output_formats) && length(output_formats) > 0) {
    if (!check_directory(output_dir, "Output")) {
      # If output directory doesn't exist, create it
      dir.create(output_dir, recursive = TRUE)
      validation_results$messages <- c(validation_results$messages,
                                       sprintf("Created output folder: %s", output_dir))
    }
  }

  # Output Format Check
  if (!plot_data && (is.null(output_formats) || length(output_formats) == 0)) {
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     "No outputs selected: both output_formats is empty and plot_data is FALSE.",
                                     "Please select at least one output option:",
                                     "  - Specify at least one output format (csv, stata, rds, txt)",
                                     "  - Set plot_data = TRUE to prepare data for plotting")
    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    proceed_without_output <- .ask_confirmation("Do you want to proceed without any output? (Y/N): ")

    if (!proceed_without_output) {
      validation_results$proceed <- FALSE
      return(validation_results)
    }
    validation_results$messages <- character()
  }

  # Only perform file checks if the respective processing flag is TRUE
  missing_file_warnings <- character()

  # Check SL4 files if sl4var is TRUE
  if (sl4var) {
    files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
    sl4_expected_files <- paste0(experiment, sl4_file_suffix)
    sl4_missing_files <- sl4_expected_files[!sl4_expected_files %in% files]

    if (length(sl4_missing_files) > 0) {
      missing_file_warnings <- c(missing_file_warnings,
                                 "Missing SL4 files:",
                                 paste(" -", sl4_missing_files))
    }
  }

  # Check HAR files if harvar is TRUE
  if (harvar) {
    files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
    har_expected_files <- paste0(experiment, har_file_suffix)
    har_missing_files <- har_expected_files[!har_expected_files %in% files]

    if (length(har_missing_files) > 0) {
      missing_file_warnings <- c(missing_file_warnings,
                                 "Missing HAR files:",
                                 paste(" -", har_missing_files))
    }
  }

  # If there are missing files, show warnings and ask for confirmation
  if (length(missing_file_warnings) > 0) {
    validation_results$status <- "warning"

    # Display specific warnings about missing files
    cat(paste(missing_file_warnings, collapse = "\n"), "\n")

    # Ask if user wants to proceed despite missing files
    proceed_without_files <- .ask_confirmation("Do you want to proceed with missing files? (Y/N): ")

    if (!proceed_without_files) {
      validation_results$proceed <- FALSE
      return(validation_results)
    }
  }

  # Mapping Info checks - only if mapping_info is "YES" or "MIX"
  if (toupper(mapping_info) %in% c("YES", "MIX")) {
    mapping_warnings <- character()

    # Check SL4 mapping only if sl4var is TRUE
    if (sl4var) {
      required_cols <- c("Variable", "Description", "Unit")

      if (is.null(sl4map)) {
        mapping_warnings <- c(mapping_warnings,
                              "Missing sl4map data frame. This is required when mapping_info is 'Yes' or 'Mix'.")
      } else if (!is.data.frame(sl4map)) {
        validation_results$status <- "error"
        validation_results$messages <- c(validation_results$messages,
                                         "sl4map must be a valid data frame.")
        validation_results$proceed <- FALSE
        return(validation_results)
      } else {
        missing_cols <- setdiff(required_cols, names(sl4map))
        if (length(missing_cols) > 0) {
          mapping_warnings <- c(mapping_warnings,
                                sprintf("SL4 mapping is missing required column(s): %s",
                                        paste(missing_cols, collapse = ", ")))
        }
      }
    }

    # Check HAR mapping only if harvar is TRUE
    if (harvar) {
      required_cols <- c("Variable", "Description", "Unit")

      if (is.null(harmap)) {
        mapping_warnings <- c(mapping_warnings,
                              "Missing harmap data frame. This is required when mapping_info is 'Yes' or 'Mix'.")
      } else if (!is.data.frame(harmap)) {
        validation_results$status <- "error"
        validation_results$messages <- c(validation_results$messages,
                                         "harmap must be a valid data frame.")
        validation_results$proceed <- FALSE
        return(validation_results)
      } else {
        missing_cols <- setdiff(required_cols, names(harmap))
        if (length(missing_cols) > 0) {
          mapping_warnings <- c(mapping_warnings,
                                sprintf("HAR mapping is missing required column(s): %s",
                                        paste(missing_cols, collapse = ", ")))
        }
      }
    }

    if (length(mapping_warnings) > 0) {
      validation_results$status <- "warning"

      # Display specific mapping warnings
      cat(paste(mapping_warnings, collapse = "\n"), "\n")
      cat("These columns are required for mapping_info = 'Yes' or 'Mix'.\n")

      use_gtapv7 <- .ask_confirmation("Do you want to proceed using GTAPv7 definitions for missing values? (Y/N): ")

      if (!use_gtapv7) {
        validation_results$proceed <- FALSE
        return(validation_results)
      }
    }
  }

  # Count found files
  successful_sl4 <- 0
  successful_har <- 0

  if (sl4var) {
    files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
    sl4_expected_files <- paste0(experiment, sl4_file_suffix)
    successful_sl4 <- sum(sl4_expected_files %in% files)
  }

  if (harvar) {
    files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
    har_expected_files <- paste0(experiment, har_file_suffix)
    successful_har <- sum(har_expected_files %in% files)
  }

  # Final Summary Message
  summary_messages <- character()

  if (sl4var && harvar) {
    # Calculate files that have both SL4 and HAR
    files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
    sl4_found <- experiment[paste0(experiment, sl4_file_suffix) %in% files]
    har_found <- experiment[paste0(experiment, har_file_suffix) %in% files]
    common_cases <- intersect(sl4_found, har_found)

    if (length(common_cases) == length(experiment)) {
      summary_messages <- c(summary_messages,
                            sprintf("All %d requested experiment files are found with both SL4 and HAR data.",
                                    length(experiment)))
    } else if (length(common_cases) > 0) {
      summary_messages <- c(summary_messages,
                            sprintf("%d/%d experiments have both SL4 and HAR data: %s",
                                    length(common_cases), length(experiment),
                                    paste(common_cases, collapse = ", ")))

      sl4_only <- setdiff(sl4_found, har_found)
      har_only <- setdiff(har_found, sl4_found)

      if (length(sl4_only) > 0) {
        summary_messages <- c(summary_messages,
                              sprintf("%d experiments have SL4 files only: %s",
                                      length(sl4_only),
                                      paste(sl4_only, collapse = ", ")))
      }

      if (length(har_only) > 0) {
        summary_messages <- c(summary_messages,
                              sprintf("%d experiments have HAR files only: %s",
                                      length(har_only),
                                      paste(har_only, collapse = ", ")))
      }
    } else {
      if (successful_sl4 == 0 && successful_har == 0) {
        validation_results$status <- "error"
        validation_results$messages <- c(validation_results$messages,
                                         "No requested experiment files were found. Please check input files and paths.")
        validation_results$proceed <- FALSE
        return(validation_results)
      } else {
        summary_messages <- c(summary_messages,
                              "No experiments have both SL4 and HAR data available.")
      }
    }
  } else if (sl4var) {
    # Only SL4 is enabled
    if (successful_sl4 == length(experiment)) {
      summary_messages <- c(summary_messages,
                            sprintf("All %d requested experiment SL4 files are found.",
                                    length(experiment)))
    } else if (successful_sl4 > 0) {
      files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
      sl4_found <- experiment[paste0(experiment, sl4_file_suffix) %in% files]

      summary_messages <- c(summary_messages,
                            sprintf("%d/%d experiment SL4 files found: %s",
                                    successful_sl4, length(experiment),
                                    paste(sl4_found, collapse = ", ")))
    } else {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "No requested experiment SL4 files were found. Please check input files and paths.")
      validation_results$proceed <- FALSE
      return(validation_results)
    }
  } else if (harvar) {
    # Only HAR is enabled
    if (successful_har == length(experiment)) {
      summary_messages <- c(summary_messages,
                            sprintf("All %d requested experiment HAR files are found.",
                                    length(experiment)))
    } else if (successful_har > 0) {
      files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
      har_found <- experiment[paste0(experiment, har_file_suffix) %in% files]

      summary_messages <- c(summary_messages,
                            sprintf("%d/%d experiment HAR files found: %s",
                                    successful_har, length(experiment),
                                    paste(har_found, collapse = ", ")))
    } else {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "No requested experiment HAR files were found. Please check input files and paths.")
      validation_results$proceed <- FALSE
      return(validation_results)
    }
  }

  validation_results$messages <- c(summary_messages,
                                   sprintf("Mapping method used: %s", mapping_info))

  return(validation_results)
}


#' @title Ask for User Confirmation (Internal)
#' @description Prompts the user for confirmation by displaying a message and reading input from the console. Returns TRUE if the user confirms with 'y', otherwise FALSE.
#' @param prompt A character string specifying the message to display to the user.
#' @return A logical value: TRUE if the user types "y" (case-insensitive), FALSE if "n".
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.ask_confirmation <- function(prompt) {
  cat("\n", prompt)
  while (TRUE) {
    user_input <- tolower(readline())
    if (user_input == "y") return(TRUE)
    if (user_input == "n") return(FALSE)
    cat("Please enter 'Y' for yes or 'N' for no: ")
  }
}

#' @title Create Variable Report for GTAP Data
#'
#' @description
#' Generates a comprehensive report of all variables exported during GTAP data processing.
#'
#' @param data_list A list containing different GTAP data types (e.g., macro, sl4, har)
#' @param output_path Character. Directory to save the report.
#' @param filename Character. Name of the report file.
#'
#' @return Invisible logical indicating success
#'
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.create_gtap_report <- function(data_list, output_path, filename = "Report_Table.xlsx") {
  # Create data frame for tracking variables
  report_data <- data.frame(
    Variable = character(),
    ExportFile = character(),
    InputFile = character(),
    stringsAsFactors = FALSE
  )

  # Mapping for input file types
  input_file_map <- c(
    "GTAPMacros" = ".SL4",
    "sl4_data" = ".SL4",
    "bilateral_data" = ".SL4",
    "decomposition_data" = ".HAR"
  )

  # Process each data type
  for (type_name in names(data_list)) {
    data <- data_list[[type_name]]

    if (is.null(data)) next

    input_file <- input_file_map[type_name]
    if (is.na(input_file)) input_file <- ".SL4"  # Default to SL4 if not found

    # Handle data frames with Variable column
    if (is.data.frame(data) && "Variable" %in% names(data)) {
      variables <- unique(data$Variable)
      new_rows <- data.frame(
        Variable = variables,
        ExportFile = rep(type_name, length(variables)),
        InputFile = rep(input_file, length(variables)),
        stringsAsFactors = FALSE
      )
      report_data <- rbind(report_data, new_rows)
      next
    }

    # Handle lists of data frames
    if (is.list(data) && !is.data.frame(data)) {
      process_list <- function(data_item, prefix = "") {
        result <- data.frame(
          Variable = character(),
          ExportFile = character(),
          InputFile = character(),
          stringsAsFactors = FALSE
        )

        if (is.data.frame(data_item) && "Variable" %in% names(data_item)) {
          variables <- unique(data_item$Variable)
          # Extract just the last part of the path for ExportFile
          export_file <- if (prefix == "") {
            ""
          } else {
            parts <- strsplit(prefix, "_")[[1]]
            tail(parts, 1)
          }

          new_rows <- data.frame(
            Variable = variables,
            ExportFile = rep(export_file, length(variables)),
            InputFile = rep(input_file, length(variables)),
            stringsAsFactors = FALSE
          )
          result <- rbind(result, new_rows)
        } else if (is.list(data_item) && !is.data.frame(data_item)) {
          for (name in names(data_item)) {
            sub_prefix <- if (prefix == "") name else paste(prefix, name, sep = "_")
            sub_result <- process_list(data_item[[name]], sub_prefix)
            result <- rbind(result, sub_result)
          }
        }

        return(result)
      }

      list_results <- process_list(data, type_name)
      report_data <- rbind(report_data, list_results)
    }
  }

  # If no data, return early
  if (nrow(report_data) == 0) {
    message("No variables to report.")
    return(invisible(FALSE))
  }

  # Create output directory if needed
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Clean and sort data
  report_data <- unique(report_data[order(report_data$Variable), ])

  # Replace empty export files with appropriate values
  report_data$ExportFile[report_data$ExportFile == ""] <- "Main"

  # Create Excel report
  wb <- openxlsx::createWorkbook()

  # Variables worksheet
  openxlsx::addWorksheet(wb, "Variables")
  openxlsx::writeData(wb, "Variables", report_data, startRow = 1, colNames = TRUE)

  # Style header
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "Bottom",
    borderStyle = "medium"
  )
  openxlsx::addStyle(wb, "Variables", style = header_style,
                     rows = 1, cols = 1:ncol(report_data))

  # Summary worksheet
  openxlsx::addWorksheet(wb, "Summary")

  input_file_counts <- table(report_data$InputFile)
  summary_data <- data.frame(
    InputFile = names(input_file_counts),
    VariableCount = as.numeric(input_file_counts),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "Summary", summary_data, startRow = 1, colNames = TRUE)
  openxlsx::addStyle(wb, "Summary", style = header_style,
                     rows = 1, cols = 1:ncol(summary_data))

  # Save workbook
  report_path <- file.path(output_path, filename)
  openxlsx::saveWorkbook(wb, report_path, overwrite = TRUE)

  message(sprintf("Created GTAP variable report: %s", normalizePath(report_path)))

  return(invisible(TRUE))
}

#' @title Apply Filters to GTAP Data (Internal)
#' @description Applies region, experiment, and sector filters to GTAP data structures.
#'
#' @param data List of data frames or single data frame to filter.
#' @param region_select Character vector of regions to include.
#' @param experiment_select Character vector of experiments to include.
#' @param sector_select Character vector of sectors to include.
#'
#' @return Filtered data in the same structure as input.
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.apply_filters <- function(data, region_select = NULL, experiment_select = NULL, sector_select = NULL) {

  filter_dataframe <- function(df) {
    if (!is.data.frame(df)) return(df)

    col_names <- tolower(names(df))
    modified_df <- df

    region_col <- names(df)[col_names %in% c("reg", "region", "source", "destination")]
    if (!is.null(region_select) && length(region_col) > 0) {
      for (col in region_col) {
        modified_df <- modified_df[modified_df[[col]] %in% region_select, ]
        if (nrow(modified_df) > 0) {
          modified_df[[col]] <- factor(modified_df[[col]], levels = region_select)
        }
      }
    }

    if (!is.null(experiment_select) && "Experiment" %in% names(modified_df)) {
      modified_df <- modified_df[modified_df$Experiment %in% experiment_select, ]
      if (nrow(modified_df) > 0) {
        modified_df$Experiment <- factor(modified_df$Experiment, levels = experiment_select)
      }
    }

    sector_col <- names(df)[col_names %in% c("comm", "acts", "sector")]
    if (!is.null(sector_select) && length(sector_col) > 0) {
      for (col in sector_col) {
        modified_df <- modified_df[modified_df[[col]] %in% sector_select, ]
        if (nrow(modified_df) > 0) {
          modified_df[[col]] <- factor(modified_df[[col]], levels = sector_select)
        }
      }
    }

    order_cols <- c()
    if ("Experiment" %in% names(modified_df)) {
      order_cols <- c(order_cols, "Experiment")
    }

    if (length(region_col) > 0) {
      order_cols <- c(order_cols, region_col[1])
    }

    if (length(sector_col) > 0) {
      order_cols <- c(order_cols, sector_col[1])
    }

    if (length(order_cols) > 0) {
      modified_df <- modified_df[do.call(order, lapply(order_cols, function(col) modified_df[[col]])), ]
    }

    return(modified_df)
  }

  if (is.data.frame(data)) {
    return(filter_dataframe(data))
  } else if (is.list(data)) {
    return(lapply(data, function(x) {
      if (is.data.frame(x)) {
        filter_dataframe(x)
      } else if (is.list(x)) {
        .apply_filters(x, region_select, experiment_select, sector_select)
      } else {
        x
      }
    }))
  }

  return(data)
}


#' @title Apply Function to Nested Data Structures (Internal)
#'
#' @description Recursively applies a function to all data frames within a potentially
#' nested data structure while preserving the original structure and attributes.
#'
#' @param data A list, data frame, or nested data structure to process
#' @param .f A function to apply to each data frame found in the structure
#' @param ... Additional arguments to pass to the function
#'
#' @return A data structure with the same form as the input, with the function applied to all data frames
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}
#'
.apply_to_dataframes <- function(data, .f, ...) {
  if (is.data.frame(data)) {
    return(.f(data, ...))
  }

  process_list <- function(lst) {
    result <- lapply(lst, function(x) {
      if (is.data.frame(x)) {
        return(.f(x, ...))
      } else if (is.list(x)) {
        return(process_list(x))
      } else {
        return(x)
      }
    })

    attributes(result) <- attributes(lst)
    class(result) <- class(lst)

    return(result)
  }

  return(process_list(data))
}


#' @title Add Scenario Ranking to GTAP Data
#' @description Adds a numeric rank column (ScenarioRank) to GTAP data structures based on the
#' order of experiments provided. This helps with sorting and visualization of experiments.
#'
#' @param data_list A data frame or list containing GTAP data to be enhanced.
#' @param experiment Character vector of experiment names in the desired order.
#' @param rank_column Character. Name of the column to add with ranking information. Default is "ScenarioRank".
#' @param experiment_column Character. Name of the column containing experiment names. Default is "Experiment".
#'
#' @return The input data structure with an added ranking column.
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{auto_gtap_data}}, \code{\link{auto_gtap_dynamic}}
#'
.add_scenario_rank <- function(data_list, experiment,
                               rank_column = "ScenarioRank",
                               experiment_column = "Experiment") {

  experiment_ranks <- setNames(seq_along(experiment), experiment)

  add_rank_to_df <- function(df, experiment_ranks, rank_column, experiment_column) {
    if (!is.data.frame(df) || nrow(df) == 0 ||
        !experiment_column %in% names(df)) {
      return(df)
    }

    rank_values <- sapply(df[[experiment_column]], function(exp_name) {
      if (exp_name %in% names(experiment_ranks)) {
        return(experiment_ranks[[exp_name]])
      } else {
        return(NA_integer_)
      }
    })

    temp_df <- df
    temp_df[[experiment_column]] <- NULL

    result_df <- data.frame(
      rank_values,
      df[[experiment_column]],
      temp_df,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    names(result_df)[1] <- rank_column
    names(result_df)[2] <- experiment_column

    result_df <- result_df[order(result_df[[rank_column]]), ]

    return(result_df)
  }

  if (is.data.frame(data_list)) {
    return(add_rank_to_df(data_list, experiment_ranks, rank_column, experiment_column))
  } else if (is.list(data_list)) {
    return(.apply_to_dataframes(data_list, add_rank_to_df,
                                experiment_ranks, rank_column, experiment_column))
  }

  return(data_list)
}
