#' @title Process Bilateral Trade Data from GTAP Output (Internal)
#' @description Extracts and processes bilateral trade data (typically QXS variables) from
#'   grouped GTAP data. Separates bilateral trade variables into a separate data structure
#'   and applies region name standardization.
#'
#' @param grouped_data A list containing grouped GTAP data with 3D dimensions.
#' @param output_path Character. Directory path where the extracted data will be saved if export=TRUE.
#' @param formats Character vector. Export formats (e.g., "csv", "xlsx").
#' @param var_pattern Character. Regular expression pattern to identify bilateral trade variables.
#'   Default is "qxs" to match QXS (bilateral trade) variables.
#' @param export Logical. If TRUE, exports the extracted data to specified formats.
#'
#' @return A list containing the extracted bilateral trade data with standardized region names
#'   (REG/REG.1 renamed to Source/Destination), or NULL if no bilateral data is found.
#'   The original grouped_data is modified by removing the extracted variables.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
#'
.process_bilateral_trade <- function(grouped_data, output_path, formats,
                                     var_pattern = "qxs", export = TRUE) {
  if (!("3D" %in% names(grouped_data))) {
    message("No 3D data found for bilateral trade processing")
    return(grouped_data)
  }

  bilateral_dim <- grep("COMM.*REG.*REG", names(grouped_data[["3D"]]), value = TRUE)

  if (length(bilateral_dim) == 0) {
    message("No bilateral trade dimensions found")
    return(grouped_data)
  }

  bilateral_df <- grouped_data[["3D"]][[bilateral_dim]]
  matched_data <- bilateral_df[grepl(var_pattern, bilateral_df$Variable, ignore.case = TRUE), ]

  if (nrow(matched_data) > 0) {
    message("Extracting bilateral trade data...")
    grouped_data[["3D"]][[bilateral_dim]] <-
      bilateral_df[!grepl(var_pattern, bilateral_df$Variable, ignore.case = TRUE), ]

    matched_data <- rename_GTAP_bilateral(matched_data)

    bilateral_list <- list(BilateralTrade = matched_data)

    if (export) {
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }

      HARplus::export_data(
        data = bilateral_list,
        output_path = output_path,
        format = formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = TRUE
      )
    }

    return(bilateral_list)
  }

  return(NULL)
}


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
#' @seealso \code{\link{process_gtap_data}}
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
#' @param sl4file Data frame, NULL, or FALSE. Mapping for SL4 variables.
#' @param harfile Data frame, NULL, or FALSE. Mapping for HAR variables.
#' @param output_formats Output format specification. Can be:
#'        - Character vector: Export formats (e.g., c("csv", "stata"))
#'        - "plot_data": For plotting data without export
#'        - List: Named list with "yes"/"no" values, can include plot_data
#'
#' @return A list with three elements:
#' \item{status}{Character indicating validation status ("ok", "error", or "warning").}
#' \item{messages}{Character vector of validation messages.}
#' \item{proceed}{Logical indicating whether processing should continue.}
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
#'
.validate_gtap_files <- function(input_dir, output_dir,
                                 experiment, mapping_info, sl4file, harfile, output_formats, plot_data) {
  validation_results <- list(
    status = "ok",
    messages = character(),
    proceed = TRUE
  )

  process_sl4 <- !identical(sl4file, FALSE)
  process_har <- !identical(harfile, FALSE)

  if (!plot_data && (is.null(output_formats) || length(output_formats) == 0)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "No outputs selected: both output_formats is empty and plot_data is FALSE.",
                                     "Please select at least one output option:",
                                     "  - Specify at least one output format (csv, stata, rds, txt)",
                                     "  - Set plot_data = TRUE to prepare data for plotting"
    )
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (is.null(mapping_info)) {
    mapping_info <- "GTAPv7"
    message("  mapping_info not specified, using default: GTAPv7")
  }

  if (!dir.exists(input_dir)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "Input folder does not exist",
                                     "Please check input_dir path and ensure it exists")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (process_sl4 && !is.null(sl4file)) {
    if (!is.data.frame(sl4file) || !"Variable" %in% names(sl4file)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "Invalid SL4File structure",
                                       "Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    if (toupper(mapping_info) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(sl4file))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("sl4file is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")))
        validation_results$messages <- c(validation_results$messages,
                                         "These are required for mapping_info = 'Yes' or 'Mix'")

        cat(paste(validation_results$messages, collapse = "\n"), "\n")
        use_gtapv7 <- .ask_confirmation(
          "Do you want to proceed using GTAPv7 definitions for missing values? (Y/N): ")

        if (!use_gtapv7) {
          validation_results$proceed <- FALSE
          return(validation_results)
        }
        validation_results$messages <- character()
      }
    }
  }

  if (process_har && !is.null(harfile)) {
    if (!is.data.frame(harfile) || !"Variable" %in% names(harfile)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "Invalid HARFile structure",
                                       "Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    if (toupper(mapping_info) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(harfile))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("harfile is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")))
        validation_results$messages <- c(validation_results$messages,
                                         "These are required for mapping_info = 'Yes' or 'Mix'")

        cat(paste(validation_results$messages, collapse = "\n"), "\n")
        use_gtapv7 <- .ask_confirmation(
          "Do you want to proceed using GTAPv7 definitions for missing values? (Y/N): ")

        if (!use_gtapv7) {
          validation_results$proceed <- FALSE
          return(validation_results)
        }
        validation_results$messages <- character()
      }
    }
  }

  if (process_sl4 && is.null(sl4file)) {
    validation_results$messages <- c(validation_results$messages,
                                     "sl4file is NULL - all SL4 variables will be extracted with GTAPv7 mapping")
  }

  if (process_har && is.null(harfile)) {
    validation_results$messages <- c(validation_results$messages,
                                     "harfile is NULL - all HAR variables will be extracted with GTAPv7 mapping")
  }

  if (!process_sl4) {
    validation_results$messages <- c(validation_results$messages,
                                     "sl4file is FALSE - SL4 processing will be skipped")
  }

  if (!process_har) {
    validation_results$messages <- c(validation_results$messages,
                                     "harfile is FALSE - HAR processing will be skipped")
  }

  if (length(experiment) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "No case names provided",
                                     "Please define experiment variable with experiment names")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (length(experiment) != length(unique(experiment))) {
    duplicate_cases <- experiment[duplicated(experiment)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "Duplicate case names found:",
                                     paste("   -", duplicate_cases),
                                     "Each case name must be unique")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)

  files_lower <- tolower(files)
  if (length(files_lower) != length(unique(files_lower))) {
    duplicate_files <- files[duplicated(files_lower)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "Duplicate file names found (case-insensitive):",
                                     paste("   -", duplicate_files),
                                     "File names must be unique (ignoring case)")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))
  case_names_lower <- tolower(trimws(experiment))

  if (process_sl4 && length(sl4_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "No .sl4 files found in the input folder but sl4file is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (process_har && length(har_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "No -WEL.har files found in the input folder but harfile is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  available_bases <- c()
  if (process_sl4) available_bases <- c(available_bases, sl4_bases)
  if (process_har) available_bases <- c(available_bases, har_bases)

  missing_cases <- setdiff(case_names_lower, available_bases)
  if (length(missing_cases) == length(case_names_lower)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("None of the specified cases were found: %s",
                                             paste(experiment[!case_names_lower %in% available_bases],
                                                   collapse = ", ")))
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (process_sl4 && process_har && length(sl4_files) != length(har_files)) {
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("Unequal number of files found: %d .sl4 files and %d -WEL.har files",
                                             length(sl4_files), length(har_files)))

    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    validation_results$proceed <- .ask_confirmation(
      "Do you want to proceed with the available files? (Y/N): ")

    if (!validation_results$proceed) {
      return(validation_results)
    }
    validation_results$messages <- character()
  }

  if (process_sl4 && process_har) {
    matched_pairs <- intersect(sl4_bases, har_bases)
    unmatched_sl4 <- setdiff(sl4_bases, har_bases)
    unmatched_har <- setdiff(har_bases, sl4_bases)

    if (length(unmatched_sl4) > 0 || length(unmatched_har) > 0) {
      validation_results$status <- "warning"
      if (length(unmatched_sl4) > 0) {
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("SL4 files without matching HAR files: %s",
                                                 paste(unmatched_sl4, collapse = ", ")))
      }
      if (length(unmatched_har) > 0) {
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("HAR files without matching SL4 files: %s",
                                                 paste(unmatched_har, collapse = ", ")))
      }

      cat(paste(validation_results$messages, collapse = "\n"), "\n")
      validation_results$proceed <- .ask_confirmation(
        "Do you want to proceed with only the matched pairs? (Y/N): ")

      if (!validation_results$proceed) {
        return(validation_results)
      }
      validation_results$messages <- character()
    }
  }

  available_bases <- c()
  if (process_sl4) available_bases <- c(available_bases, sl4_bases)
  if (process_har) available_bases <- c(available_bases, har_bases)

  partial_cases <- intersect(case_names_lower, available_bases)
  if (length(partial_cases) < length(case_names_lower) &&
      length(partial_cases) > 0) {
    missing_cases <- experiment[!case_names_lower %in% available_bases]
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("Some specified cases were not found: %s",
                                             paste(missing_cases, collapse = ", ")))

    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    validation_results$proceed <- .ask_confirmation(
      "Do you want to proceed with the available cases? (Y/N): ")

    if (!validation_results$proceed) {
      return(validation_results)
    }
    validation_results$messages <- character()
  }

  if (validation_results$status == "ok") {
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("All files verified successfully."))
    if (process_sl4 && process_har) {
      matched_pairs <- intersect(sl4_bases, har_bases)
      validation_results$messages <- c(validation_results$messages,
                                       sprintf("Found %d matched pairs.", length(matched_pairs)))
    }
  }

  return(validation_results)
}


#' @title Ask for User Confirmation (Internal)
#' @description Prompts the user for confirmation by displaying a message and reading input from the console. Returns TRUE if the user confirms with 'y', otherwise FALSE.
#' @param prompt A character string specifying the message to display to the user.
#' @return A logical value: TRUE if the user types "y" (case-insensitive), FALSE if "n".
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
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


#' @title Consolidate Reports (Internal)
#' @description Consolidates multiple Excel report files into a single report and optionally removes source files.
#'
#' @param output_folder Character. Directory where the Excel files are located.
#' @param pattern Character. Pattern to match Excel files (default: "\\.xlsx$").
#' @param final_name Character. Name for the final consolidated report file (default: "Report_Output.xlsx").
#' @param remove_sources Logical. If TRUE, source files will be removed after consolidation (default: TRUE).
#' @param chunk_size Numeric. Number of files to process in each chunk (default: 10).
#'
#' @return Invisibly returns TRUE if consolidation is successful, or FALSE if no files are found.
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
#'
.consolidate_reports <- function(output_folder, pattern = "\\.xlsx$",
                                 final_name = "Report_Output.xlsx",
                                 remove_sources = TRUE, chunk_size = 10) {
  if (!dir.exists(output_folder)) {
    stop("Output folder does not exist: ", output_folder)
  }
  xlsx_files <- list.files(
    path = output_folder,
    pattern = pattern,
    full.names = TRUE
  )
  final_path <- file.path(output_folder, final_name)
  xlsx_files <- xlsx_files[xlsx_files != final_path]
  if (length(xlsx_files) == 0) {
    message("No files found to consolidate.")
    return(invisible(FALSE))
  }
  message(sprintf("Found %d files to consolidate...", length(xlsx_files)))
  n_chunks <- ceiling(length(xlsx_files) / chunk_size)
  chunk_results <- vector("list", n_chunks)
  failed_files <- character()
  for (i in seq_len(n_chunks)) {
    chunk_start <- (i - 1) * chunk_size + 1
    chunk_end <- min(i * chunk_size, length(xlsx_files))
    current_files <- xlsx_files[chunk_start:chunk_end]
    message(sprintf("Processing chunk %d of %d (%d files)...",
                    i, n_chunks, length(current_files)))
    chunk_data <- vector("list", length(current_files))
    for (j in seq_along(current_files)) {
      file_path <- current_files[j]
      file_name <- basename(file_path)
      tryCatch({
        sheets <- readxl::excel_sheets(file_path)
        sheet_data <- vector("list", length(sheets))
        for (k in seq_along(sheets)) {
          result <- purrr::safely(readxl::read_excel)(file_path, sheet = sheets[k])
          if (!is.null(result$result)) {
            sheet_df <- result$result
            sheet_data[[k]] <- sheet_df
          } else {
            warning(sprintf("Failed to read sheet %s from %s: %s",
                            sheets[k], file_name, result$error))
          }
        }
        chunk_data[[j]] <- dplyr::bind_rows(sheet_data)
      }, error = function(e) {
        failed_files <- c(failed_files, file_path)
        warning(sprintf("Failed to process %s: %s", file_name, e$message))
        NULL
      })
    }
    chunk_results[[i]] <- dplyr::bind_rows(chunk_data)
    rm(chunk_data)
    gc()
  }
  message("Combining all processed data...")
  final_data <- dplyr::bind_rows(chunk_results)
  message(sprintf("Writing consolidated report to %s...", final_name))
  tryCatch({
    writexl::write_xlsx(final_data, final_path)
    if (remove_sources) {
      message("Removing source files...")
      successfully_removed <- file.remove(xlsx_files)
      if (!all(successfully_removed)) {
        warning(sprintf("Failed to remove %d source files",
                        sum(!successfully_removed)))
      }
    }
    message(sprintf("Successfully consolidated %d files into %s",
                    length(xlsx_files), final_name))
    if (length(failed_files) > 0) {
      warning(sprintf("Failed to process %d files:\n%s",
                      length(failed_files),
                      paste(basename(failed_files), collapse = "\n")))
    }
  }, error = function(e) {
    stop(sprintf("Failed to write consolidated report: %s", e$message))
  })
  invisible(TRUE)
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
#' @seealso \code{\link{process_gtap_data}}
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
#' @seealso \code{\link{process_gtap_data}}
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


