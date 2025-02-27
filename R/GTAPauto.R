#' @title Extract and Aggregate Scalar Macroeconomic Variables
#' @description Extracts scalar macroeconomic variables from multiple SL4 datasets and aggregates them into a structured data frame.
#'
#' @param ... One or more SL4 data objects. Can be either:
#'   - A single list containing multiple SL4 datasets
#'   - Multiple SL4 datasets as separate arguments
#' @param experiment_names Optional character vector of experiment names. If NULL, names are inferred.
#'
#' @return A data frame with three columns:
#'   - "Experiment": The scenario name
#'   - "Variable": The macroeconomic variable name
#'   - "Value": The extracted numeric value
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{add_unit_col}}, \code{\link{add_mapping_info}}
#' @export
#' @examples
#' # Import sample data
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- HARplus::load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Method 1: Using a list of datasets
#' data_list <- list(base = sl4_data1, policy = sl4_data2)
#' Macros1 <- gtap_macros_data(data_list)
#'
#' # Method 2: Using multiple arguments
#' Macros2 <- gtap_macros_data(sl4_data1, sl4_data2,
#'                            experiment_names = c("Base", "Policy"))
#'
gtap_macros_data <- function(..., experiment_names = NULL) {
  # Define the specific macro variables to extract
  vars <- c(
    "globalcgds", "pcgdswld", "pfactwld", "pgdpwld", "pt",
    "pxwwld", "qgdpwld", "qtm", "qxwwld", "rorg", "vgdpwld",
    "vxwwld", "walras_dem", "walras_sup", "walraslack",
    "WEV", "WEV_ALT", "ywld"
  )

  input_data <- list(...)
  if (length(input_data) == 1 && is.list(input_data[[1]])) {
    file_paths <- input_data[[1]]
    if (is.null(experiment_names)) {
      experiment_names <- names(file_paths)
    }
  } else {
    file_paths <- input_data
    if (is.null(experiment_names)) {
      dots <- match.call(expand.dots = FALSE)$...
      experiment_names <- if (length(dots) == 1) {
        deparse(dots[[1]])
      } else {
        vapply(dots, deparse, character(1))
      }
    }
  }

  if (length(experiment_names) != length(file_paths)) {
    stop("Number of experiment names must match number of file paths")
  }

  get_unit <- function(var_name) {
    if (var_name == toupper(var_name)) {
      "million USD"
    } else {
      "Percent"
    }
  }

  extracted_data <- lapply(seq_along(file_paths), function(i) {
    sl4_data <- HARplus::load_sl4x(file_paths[[i]])
    dataset <- sl4_data$data
    scenario <- experiment_names[i]

    values <- sapply(vars, function(var) {
      if (var %in% names(dataset) && is.numeric(dataset[[var]])) {
        if (!is.null(names(dataset[[var]])) && "TOTAL" %in% names(dataset[[var]])) {
          return(dataset[[var]][["TOTAL"]])
        } else {
          return(as.numeric(dataset[[var]]))
        }
      } else {
        return(NA)
      }
    })

    data.frame(
      Experiment = scenario,
      Variable = vars,
      Value = values,
      Unit = sapply(vars, get_unit),
      stringsAsFactors = FALSE
    )
  })

  Macros <- do.call(rbind, extracted_data)
  rownames(Macros) <- NULL

  return(Macros)
}


#' @title Determine Output Formats
#' @description Returns a character vector of output format names for which the corresponding global variable is set to "yes".
#' @return A character vector of output format names.
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
#'
.output_format <- function() {
  output_formats <- c(
    "csv"   = tolower(csv.output)   == "yes",
    "stata" = tolower(stata.output) == "yes",
    "rds"   = tolower(r.output)     == "yes",
    "txt"   = tolower(txt.output)   == "yes"
  )
  names(output_formats)[output_formats]
}


#' @title Validate GTAP Files
#' @description Validates input files, mapping configurations, and case names for GTAP processing.
#'
#' @param input_dir Character. Path to folder containing GTAP input files.
#' @param output_dir Character. Path where output files will be saved.
#' @param experiment Character vector. Case names (experiment names) to validate.
#' @param mapping_info Character. Mapping mode: "GTAPv7", "Yes", "No", or "Mix".
#' @param sl4file Data frame, NULL, or FALSE. Mapping for SL4 variables.
#' @param harfile Data frame, NULL, or FALSE. Mapping for HAR variables.
#' @param output_formats Character vector. Output formats to validate.
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
                                 experiment, mapping_info, sl4file, harfile, output_formats) {
  # Initialize validation results
  validation_results <- list(
    status = "ok",
    messages = character(),
    proceed = TRUE
  )

  # Check if at least one output format is selected
  if (length(output_formats) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No output format selected",
                                     "ℹ️ Please set at least one output format to 'YES':",
                                     "   - csv.output",
                                     "   - stata.output",
                                     "   - r.output",
                                     "   - txt.output")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check mapping_info - set default if missing
  if (is.null(mapping_info)) {
    mapping_info <- "GTAPv7"
    message("ℹ️ mapping_info not specified, using default: GTAPv7")
  }

  # Validate input folder
  if (!dir.exists(input_dir)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Input folder does not exist",
                                     "ℹ️ Please check input_dir path and ensure it exists")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Modified logic for sl4file/harfile validation
  process_sl4 <- !identical(sl4file, FALSE)
  process_har <- !identical(harfile, FALSE)

  # Check sl4file structure if not FALSE or NULL
  if (process_sl4 && !is.null(sl4file)) {
    if (!is.data.frame(sl4file) || !"Variable" %in% names(sl4file)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "❌ Invalid SL4File structure",
                                       "ℹ️ Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    # Check for Description and Unit if mapping_info is "Yes" or "Mix"
    if (toupper(mapping_info) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(sl4file))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ sl4file is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")))
        validation_results$messages <- c(validation_results$messages,
                                         "These are required for mapping_info = 'Yes' or 'Mix'")

        # Ask for confirmation
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

  # Check harfile structure if not FALSE or NULL
  if (process_har && !is.null(harfile)) {
    if (!is.data.frame(harfile) || !"Variable" %in% names(harfile)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "❌ Invalid HARFile structure",
                                       "ℹ️ Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    # Check for Description and Unit if mapping_info is "Yes" or "Mix"
    if (toupper(mapping_info) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(harfile))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ harfile is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")))
        validation_results$messages <- c(validation_results$messages,
                                         "These are required for mapping_info = 'Yes' or 'Mix'")

        # Ask for confirmation
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

  # Add messages for NULL mappings
  if (process_sl4 && is.null(sl4file)) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ sl4file is NULL - all SL4 variables will be extracted with GTAPv7 mapping")
  }

  if (process_har && is.null(harfile)) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ harfile is NULL - all HAR variables will be extracted with GTAPv7 mapping")
  }

  # Add messages for skipped processes
  if (!process_sl4) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ sl4file is FALSE - SL4 processing will be skipped")
  }

  if (!process_har) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ harfile is FALSE - HAR processing will be skipped")
  }

  # Case Names Validation
  if (length(experiment) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No case names provided",
                                     "ℹ️ Please define experiment variable with experiment names")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for duplicate case names
  if (length(experiment) != length(unique(experiment))) {
    duplicate_cases <- experiment[duplicated(experiment)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Duplicate case names found:",
                                     paste("   -", duplicate_cases),
                                     "ℹ️ Each case name must be unique")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Input Files Validation
  files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)

  # Check for duplicate file names (ignoring case)
  files_lower <- tolower(files)
  if (length(files_lower) != length(unique(files_lower))) {
    duplicate_files <- files[duplicated(files_lower)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Duplicate file names found (case-insensitive):",
                                     paste("   -", duplicate_files),
                                     "ℹ️ File names must be unique (ignoring case)")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Get specific file lists
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  # Extract base names, trimming spaces, and normalizing case
  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))
  case_names_lower <- tolower(trimws(experiment))

  # Check if required files exist
  if (process_sl4 && length(sl4_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No .sl4 files found in the input folder but sl4file is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (process_har && length(har_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No -WEL.har files found in the input folder but harfile is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for complete absence of case files
  available_bases <- c()
  if (process_sl4) available_bases <- c(available_bases, sl4_bases)
  if (process_har) available_bases <- c(available_bases, har_bases)

  missing_cases <- setdiff(case_names_lower, available_bases)
  if (length(missing_cases) == length(case_names_lower)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("❌ None of the specified cases were found: %s",
                                             paste(experiment[!case_names_lower %in% available_bases],
                                                   collapse = ", ")))
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for mismatched file counts - only if processing both SL4 and HAR
  if (process_sl4 && process_har && length(sl4_files) != length(har_files)) {
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("⚠️ Unequal number of files found: %d .sl4 files and %d -WEL.har files",
                                             length(sl4_files), length(har_files)))

    # Print messages before asking for confirmation
    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    validation_results$proceed <- .ask_confirmation(
      "Do you want to proceed with the available files? (Y/N): ")

    if (!validation_results$proceed) {
      return(validation_results)
    }
    validation_results$messages <- character()
  }

  # Check for file name mismatches - only if processing both SL4 and HAR
  if (process_sl4 && process_har) {
    matched_pairs <- intersect(sl4_bases, har_bases)
    unmatched_sl4 <- setdiff(sl4_bases, har_bases)
    unmatched_har <- setdiff(har_bases, sl4_bases)

    if (length(unmatched_sl4) > 0 || length(unmatched_har) > 0) {
      validation_results$status <- "warning"
      if (length(unmatched_sl4) > 0) {
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ SL4 files without matching HAR files: %s",
                                                 paste(unmatched_sl4, collapse = ", ")))
      }
      if (length(unmatched_har) > 0) {
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ HAR files without matching SL4 files: %s",
                                                 paste(unmatched_har, collapse = ", ")))
      }

      # Print messages before asking for confirmation
      cat(paste(validation_results$messages, collapse = "\n"), "\n")
      validation_results$proceed <- .ask_confirmation(
        "Do you want to proceed with only the matched pairs? (Y/N): ")

      if (!validation_results$proceed) {
        return(validation_results)
      }
      validation_results$messages <- character()
    }
  }

  # Check for partial case matches
  available_bases <- c()
  if (process_sl4) available_bases <- c(available_bases, sl4_bases)
  if (process_har) available_bases <- c(available_bases, har_bases)

  partial_cases <- intersect(case_names_lower, available_bases)
  if (length(partial_cases) < length(case_names_lower) &&
      length(partial_cases) > 0) {
    missing_cases <- experiment[!case_names_lower %in% available_bases]
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("⚠️ Some specified cases were not found: %s",
                                             paste(missing_cases, collapse = ", ")))

    # Print messages before asking for confirmation
    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    validation_results$proceed <- .ask_confirmation(
      "Do you want to proceed with the available cases? (Y/N): ")

    if (!validation_results$proceed) {
      return(validation_results)
    }
    validation_results$messages <- character()
  }

  # If everything is fine, add success message
  if (validation_results$status == "ok") {
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("✅ All files verified successfully."))
    if (process_sl4 && process_har) {
      matched_pairs <- intersect(sl4_bases, har_bases)
      validation_results$messages <- c(validation_results$messages,
                                       sprintf("Found %d matched pairs.", length(matched_pairs)))
    }
  }

  return(validation_results)
}

#' @title Ask for User Confirmation
#' @description Prompts the user for confirmation by displaying a message and reading input from the console. Returns TRUE if the user confirms with 'y', otherwise FALSE.
#' @param prompt A character string specifying the message to display to the user.
#' @return A logical value: TRUE if the user types "y" (case-insensitive), FALSE if "n".
#' @author Pattawee Puangchit
#' @keywords internal
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


#' @title Process Bilateral Trade Data
#' @description Processes bilateral trade data from GTAP output, focusing on QXS variables.
#'
#' @param grouped_data List. The grouped data containing 3D arrays with bilateral trade information.
#' @param output_path Character. Path where output files should be saved.
#' @param formats Character vector. Output formats (e.g., c("xlsx", "csv")).
#' @param var_pattern Character. Pattern to match bilateral trade variables (default: "qxs").
#'
#' @return Modified grouped_data with bilateral trade data extracted.
#' @author Pattawee Puangchit
#' @keywords internal
#' @importFrom HARplus export_data
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

#' @title Process Macro Data
#' @description Processes macro data from GTAP output and exports it using HARplus::export_data.
#'
#' @param input_files Character vector. Paths to input files.
#' @param experiment_names Character vector. Experiment names, defaulting to file names sans extension.
#' @param output_path Character. Directory where output files should be saved.
#' @param formats Character vector. Output formats (e.g., c("xlsx", "csv")).
#'
#' @return Invisibly returns NULL.
#' @author Pattawee Puangchit
#' @keywords internal
#' @importFrom tools file_path_sans_ext
#' @importFrom HARplus export_data
#' @seealso \code{\link{process_gtap_data}}
#'
.process_macro_data <- function(input_files, experiment_names = NULL,
                                output_path, formats, export = TRUE) {
  if (is.null(experiment_names)) {
    experiment_names <- tools::file_path_sans_ext(basename(input_files))
  }

  if (length(experiment_names) != length(input_files)) {
    stop("Number of experiment names must match number of input files")
  }

  tryCatch({
    macro_data <- do.call(
      gtap_macros_data,
      c(
        as.list(input_files),
        list(experiment_names = experiment_names)
      )
    )

    if (!is.null(macro_data)) {
      message("Processing macro data...")

      if (export) {
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }

        macro_list <- list(Macros = macro_data)
        HARplus::export_data(
          data = macro_list,
          output_path = output_path,
          format = formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = TRUE
        )
      }

      return(macro_data)
    }
  }, error = function(e) {
    warning("Error processing macro data: ", e$message)
  })

  return(NULL)
}


#' @title Consolidate Reports
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
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr safely
#' @importFrom dplyr bind_rows
#' @importFrom writexl write_xlsx
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


#' @title Process GTAP Data Automation
#' @description Processes GTAP data from SL4 and HAR files automatically using external mapping files.
#' By default, uses GTAPv7 definition for variable descriptions and units.
#'
#' @details
#' This function provides a complete automation workflow for processing GTAP model outputs.
#' It handles both SL4 files (solution files) and HAR files (welfare decomposition files).
#'
#' The key parameters `sl4file` and `harfile` each accept three different input types:
#' \itemize{
#'   \item \strong{Data frame}: Contains variable mappings with required "Variable" column and
#'         optional "Description" and "Unit" columns. When provided, only specified variables
#'         will be extracted.
#'   \item \strong{NULL}: Extracts all available variables from the respective file type,
#'         using GTAPv7 definitions for descriptions and units.
#'   \item \strong{FALSE}: Completely skips processing of that file type, allowing
#'         the function to focus only on the other file type.
#' }
#'
#' The `mapping_info` parameter controls how descriptions and units are assigned:
#' \itemize{
#'   \item \strong{GTAPv7}: Uses standard GTAPv7 definitions (default)
#'   \item \strong{Yes}: Uses only the supplied descriptions and units from sl4file/harfile
#'   \item \strong{No}: Does not add any descriptions or units
#'   \item \strong{Mix}: Prioritizes supplied descriptions and units, falling back to GTAPv7
#'         for any missing values
#' }
#'
#' @param sl4file Data frame, NULL, or FALSE. Mapping for SL4 variables. Set to NULL to extract all variables, or FALSE to skip SL4 processing.
#' @param harfile Data frame, NULL, or FALSE. Mapping for HAR variables. Set to NULL to extract all variables, or FALSE to skip HAR processing.
#' @param experiment Character vector. Case names to process.
#' @param mapping_info Character. Mapping mode: "GTAPv7" (default), "Yes", "No", or "Mix".
#' @param project_dir Character. Path to the project folder with "in" and "out" subfolders.
#' @param input_dir Character. Path to the input folder. Overrides project_dir/in if specified.
#' @param output_dir Character. Path to the output folder. Overrides project_dir/out if specified.
#' @param output_formats Character vector. Output formats (e.g., c("csv", "stata", "rds", "txt")).
#' @param export_data Logical. If TRUE, exports processed data to files.
#'
#' @return If export_data=TRUE, returns NULL invisibly. If export_data=FALSE, returns a list of processed data.
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{gtap_macros_data}}
#'
#' @examples
#' # Example 1: Process all variables from both SL4 and HAR files
#' \dontrun{
#' process_gtap_data(
#'   sl4file = NULL,
#'   harfile = NULL,
#'   experiment = c("baseline", "scenario1"),
#'   project_dir = "D:/GTAP_Project",
#'   mapping_info = "GTAPv7"
#' )
#' }
#'
#' # Example 2: Process only specific variables using custom mapping
#' \dontrun{
#' # Create mapping for selected SL4 variables
#' sl4_selected <- data.frame(
#'   Variable = c("qo", "qgdp", "EV"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create mapping for selected HAR variables
#' har_selected <- data.frame(
#'   Variable = c("A", "E1"),  # Welfare and ToT decomposition
#'   stringsAsFactors = FALSE
#' )
#'
#' process_gtap_data(
#'   sl4file = sl4_selected,
#'   harfile = har_selected,
#'   experiment = c("baseline", "policy"),
#'   input_dir = "D:/GTAP_inputs",
#'   output_dir = "D:/GTAP_results"
#' )
#' }
#'
#' # Example 3: Custom descriptions and units instead of GTAPv7 defaults
#' \dontrun{
#' # Create custom mapping with descriptions and units
#' custom_mapping <- data.frame(
#'   Variable = c("qo", "qgdp", "EV"),
#'   Description = c("Output by sector", "Real GDP", "Equivalent Variation"),
#'   Unit = c("% change", "% change", "million USD"),
#'   stringsAsFactors = FALSE
#' )
#'
#' process_gtap_data(
#'   sl4file = custom_mapping,
#'   harfile = FALSE,  # Skip HAR processing
#'   experiment = c("baseline", "policy"),
#'   project_dir = "D:/GTAP_Project",
#'   mapping_info = "Yes"  # Use only supplied descriptions and units
#' )
#' }
#'
process_gtap_data <- function(sl4file, harfile, experiment, mapping_info = "GTAPv7",
                              project_dir = NULL, input_dir = NULL,
                              output_dir = NULL, output_formats = NULL,
                              export_data = TRUE) {

  # Handle folder paths
  if (is.null(input_dir) && !is.null(project_dir)) {
    input_dir <- file.path(project_dir, "in")
  }

  if (is.null(output_dir) && !is.null(project_dir)) {
    output_dir <- file.path(project_dir, "out")
  }

  if (is.null(output_formats)) {
    output_formats <- .output_format()
  }

  if (export_data && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Initialize return list
  all_data <- list()

  # Define processing flags
  process_sl4 <- !identical(sl4file, FALSE)
  process_har <- !identical(harfile, FALSE)

  validation_result <- .validate_gtap_files(
    sl4file = sl4file,
    harfile = harfile,
    input_dir  = input_dir,
    output_dir = output_dir,
    experiment     = experiment,
    mapping_info     = mapping_info,
    output_formats = output_formats
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  if (validation_result$status != "ok") {
    message("Proceeding with available files as per user confirmation...")
  }

  files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))

  # Get valid cases based on which processing is enabled
  valid_sl4_cases <- experiment[tolower(experiment) %in% sl4_bases]
  valid_har_cases <- experiment[tolower(experiment) %in% har_bases]

  # Process SL4 files if sl4file is not FALSE
  if (process_sl4) {
    # Determine variables to extract - if sl4file is NULL, extract all variables
    sl4_variables <- if (is.null(sl4file)) NULL else sl4file$Variable

    sl4.data.raw <- setNames(
      lapply(valid_sl4_cases, function(scenario) {
        sl4_path <- file.path(input_dir, paste0(scenario, ".sl4"))
        if (file.exists(sl4_path)) {
          tryCatch({
            HARplus::load_sl4x(sl4_path, select_header = sl4_variables)
          }, error = function(e) {
            message(sprintf("Error processing %s.sl4: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s.sl4 (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_sl4_cases
    )

    sl4.data.raw <- sl4.data.raw[!sapply(sl4.data.raw, is.null)]

    if (length(sl4.data.raw) > 0) {
      # Check if only macros are requested or NULL (all variables)
      has_only_macros <- FALSE
      if (!is.null(sl4file) && "Variable" %in% names(sl4file)) {
        macros_vars <- sl4file$Variable[tolower(sl4file$Variable) %in% "macros"]
        has_only_macros <- length(macros_vars) > 0 && length(macros_vars) == length(sl4file$Variable)
      }

      if (!has_only_macros) {
        priority_list <- list(
          "Sector" = c("COMM", "ACTS"),
          "Region" = c("REG")
        )

        grouped_sl4 <- tryCatch({
          do.call(
            HARplus::group_data_by_dims,
            c(
              list(
                experiment_names = names(sl4.data.raw),
                auto_rename = TRUE,
                priority = priority_list,
                subtotal_level = TRUE
              ),
              sl4.data.raw
            )
          )
        }, error = function(e) {
          message(sprintf("Error in grouping SL4 data: %s", e$message))
          return(NULL)
        })

        if (!is.null(grouped_sl4)) {
          if ("3D" %in% names(grouped_sl4)) {
            bilateral_patterns <- grep("COMM.*REG.*REG",
                                       names(grouped_sl4[["3D"]]),
                                       value = TRUE)
            if (length(bilateral_patterns) > 0) {
              for (pattern in bilateral_patterns) {
                df <- grouped_sl4[["3D"]][[pattern]]
                if ("REG" %in% names(df) && "REG.1" %in% names(df)) {
                  names(df)[names(df) == "REG"] <- "Source"
                  names(df)[names(df) == "REG.1"] <- "Destination"
                  grouped_sl4[["3D"]][[pattern]] <- df
                }
              }
            }
          }

          # Apply mapping info according to mapping_info
          grouped_sl4 <- add_mapping_info(grouped_sl4, external_map = sl4file, mapping = mapping_info)
          all_data$sl4_data <- grouped_sl4

          # Process bilateral trade data if needed
          has_qxs <- FALSE
          if (is.null(sl4file)) {
            has_qxs <- TRUE
          } else if ("Variable" %in% names(sl4file)) {
            has_qxs <- any(grepl("qxs", sl4file$Variable, ignore.case = TRUE))
          }

          if (has_qxs) {
            bilateral_data <- .process_bilateral_trade(
              grouped_sl4,
              output_dir,
              output_formats,
              export = export_data
            )
            all_data$bilateral_data <- bilateral_data
          }

          if (export_data) {
            HARplus::export_data(
              data = grouped_sl4,
              output_path = output_dir,
              format = output_formats,
              create_subfolder = TRUE,
              multi_sheet_xlsx = TRUE,
              report_output = TRUE
            )
          }
        }
      }

      # Process macros if needed
      has_macros <- FALSE
      if (is.null(sl4file)) {
        has_macros <- TRUE
      } else if ("Variable" %in% names(sl4file)) {
        has_macros <- any(tolower(sl4file$Variable) %in% "macros")
      }

      if (has_macros) {
        available_sl4_files <- file.path(input_dir, paste0(valid_sl4_cases, ".sl4"))
        available_sl4_files <- available_sl4_files[file.exists(available_sl4_files)]

        if (length(available_sl4_files) > 0) {
          macro_data <- .process_macro_data(
            input_files = available_sl4_files,
            experiment_names = tools::file_path_sans_ext(basename(available_sl4_files)),
            output_path = output_dir,
            formats = output_formats,
            export = export_data
          )
          all_data$macro_data <- macro_data
        }
      }
    }
  }

  # Process HAR files if harfile is not FALSE
  if (process_har) {
    # Determine variables to extract - if harfile is NULL, extract all variables
    har_variables <- if (is.null(harfile)) NULL else harfile$Variable

    har.data.raw <- setNames(
      lapply(valid_har_cases, function(scenario) {
        har_path <- file.path(input_dir, paste0(scenario, "-WEL.har"))
        if (file.exists(har_path)) {
          tryCatch({
            HARplus::load_harx(har_path, select_header = har_variables)
          }, error = function(e) {
            message(sprintf("Error processing %s-WEL.har: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s-WEL.har (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_har_cases
    )

    har.data.raw <- har.data.raw[!sapply(har.data.raw, is.null)]

    if (length(har.data.raw) > 0) {
      har_data <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            experiment_names = names(har.data.raw),
            subtotal_level = TRUE,
            merge_data = TRUE
          ),
          har.data.raw
        )
      )

      # Apply mapping info according to mapping_info
      har_data <- add_mapping_info(har_data, external_map = harfile, mapping = mapping_info)
      processed_decomp <- lapply(har_data, function(df) {
        if ("Header" %in% names(df) && any(df$Header == "A")) {
          welfare_decomp <- df[df$Header == "A", ]
          if ("COLUMN" %in% names(welfare_decomp)) {
            names(welfare_decomp)[names(welfare_decomp) == "COLUMN"] <- "Component"
            welfare_totals <- stats::aggregate(
              Value ~ REG + Experiment + Header + Description + Unit,
              data = welfare_decomp,
              FUN = sum
            )
            welfare_totals$Component <- "Total"
            welfare_decomp <- rbind(welfare_decomp, welfare_totals)
            df[df$Header == "A", ] <- welfare_decomp
          }
        }
        return(df)
      })

      all_data$decomposition_data <- processed_decomp

      if (export_data) {
        decomposition_data <- list(Decomposition = processed_decomp)
        HARplus::export_data(
          data = decomposition_data,
          output_path = output_dir,
          format = output_formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = TRUE
        )
      }
    }
  }

  if (export_data && file.exists(output_dir)) {
    .consolidate_reports(
      output_folder = output_dir,
      final_name = "Report_Output.xlsx",
      remove_sources = TRUE
    )
  }

  message("GTAP data processing completed!")

  if (!export_data) {
    return(all_data)
  } else {
    invisible(NULL)
  }
}



# Extracting and Preparing Data for Plotting Functions ------------------

#' @title Apply Filters to GTAP Data
#' @description Applies region, experiment, and sector filters to GTAP data.
#' @param data List of data frames or single data frame to filter.
#' @param region_select Character vector of regions to include.
#' @param experiment_select Character vector of experiments to include.
#' @param sector_select Character vector of sectors to include.
#' @return Filtered data in the same structure as input.
#' @keywords internal
.apply_filters <- function(data, region_select = NULL, experiment_select = NULL, sector_select = NULL) {

  filter_dataframe <- function(df) {
    if (!is.data.frame(df)) return(df)

    if (!is.null(region_select) && "REG" %in% names(df)) {
      df <- dplyr::filter(df, REG %in% region_select)
      df$REG <- factor(df$REG, levels = region_select)
    }

    if (!is.null(experiment_select) && "Experiment" %in% names(df)) {
      df <- dplyr::filter(df, Experiment %in% experiment_select)
      df$Experiment <- factor(df$Experiment, levels = experiment_select)
    }

    sector_col <- if ("COMM" %in% names(df)) "COMM" else if ("ACTS" %in% names(df)) "ACTS" else NULL

    if (!is.null(sector_select) && !is.null(sector_col)) {
      df <- dplyr::filter(df, .data[[sector_col]] %in% sector_select)
      df[[sector_col]] <- factor(df[[sector_col]], levels = sector_select)
    }

    if (!is.null(sector_col)) {
      df <- dplyr::arrange(df, Experiment, REG, .data[[sector_col]])
    } else {
      df <- dplyr::arrange(df, Experiment, REG)
    }

    return(df)
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




#' @title Validate GTAP Files for Extraction
#' @description Validates input files, mapping configurations, and case names for GTAP data extraction.
#' @param sl4map Data frame, NULL, or FALSE. Mapping for SL4 variables.
#' @param harmap Data frame, NULL, or FALSE. Mapping for HAR variables.
#' @param input.folder Character. Path to folder containing GTAP input files.
#' @param case.name Character vector. Case names (experiment names) to validate.
#' @param info.mode Character. Mapping mode: "GTAPv7", "Yes", "No", or "Mix".
#' @return A list with validation status, messages, and proceed flag.
#' @keywords internal
.validate_extract_files <- function(sl4map, harmap, input.folder, case.name, info.mode) {
  # Initialize validation results
  validation_results <- list(
    status = "ok",
    messages = character(),
    proceed = TRUE
  )

  # Check info_mode - set default if missing
  if (is.null(info.mode)) {
    info.mode <- "GTAPv7"
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ info.mode not specified, using default: GTAPv7")
  }

  # Validate input folder
  if (!dir.exists(input.folder)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Input folder does not exist",
                                     "ℹ️ Please check input.folder path and ensure it exists")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Define processing flags
  process_sl4 <- !identical(sl4map, FALSE)
  process_har <- !identical(harmap, FALSE)

  # Check sl4map structure if not FALSE or NULL
  if (process_sl4 && !is.null(sl4map)) {
    if (!is.data.frame(sl4map) || !"Variable" %in% names(sl4map)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "❌ Invalid sl4map structure",
                                       "ℹ️ Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    # Check for Description and Unit if info.mode is "Yes" or "Mix"
    if (toupper(info.mode) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(sl4map))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ sl4map is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")),
                                         "These are required for info.mode = 'Yes' or 'Mix'")

        # Ask for confirmation
        cat(paste(validation_results$messages, collapse = "\n"), "\n")
        use_gtapv7 <- .ask_confirmation(
          "Do you want to proceed using GTAPv7 definitions for missing values? (Y/N): ")

        if (!use_gtapv7) {
          validation_results$proceed <- FALSE
          return(validation_results)
        }
        validation_results$messages <- character()  # Clear messages after confirmation
      }
    }
  }

  # Check harmap structure if not FALSE or NULL
  if (process_har && !is.null(harmap)) {
    if (!is.data.frame(harmap) || !"Variable" %in% names(harmap)) {
      validation_results$status <- "error"
      validation_results$messages <- c(validation_results$messages,
                                       "❌ Invalid harmap structure",
                                       "ℹ️ Required column: Variable")
      validation_results$proceed <- FALSE
      return(validation_results)
    }

    # Check for Description and Unit if info.mode is "Yes" or "Mix"
    if (toupper(info.mode) %in% c("YES", "MIX")) {
      missing_cols <- setdiff(c("Description", "Unit"), names(harmap))
      if (length(missing_cols) > 0) {
        validation_results$status <- "warning"
        validation_results$messages <- c(validation_results$messages,
                                         sprintf("⚠️ harmap is missing columns: %s",
                                                 paste(missing_cols, collapse = ", ")),
                                         "These are required for info.mode = 'Yes' or 'Mix'")

        # Ask for confirmation
        cat(paste(validation_results$messages, collapse = "\n"), "\n")
        use_gtapv7 <- .ask_confirmation(
          "Do you want to proceed using GTAPv7 definitions for missing values? (Y/N): ")

        if (!use_gtapv7) {
          validation_results$proceed <- FALSE
          return(validation_results)
        }
        validation_results$messages <- character()  # Clear messages after confirmation
      }
    }
  }

  # Add messages for NULL mappings
  if (process_sl4 && is.null(sl4map)) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ sl4map is NULL - all SL4 variables will be extracted with GTAPv7 mapping")
  }

  if (process_har && is.null(harmap)) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ harmap is NULL - all HAR variables will be extracted with GTAPv7 mapping")
  }

  # Add messages for skipped processes
  if (!process_sl4) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ sl4map is FALSE - SL4 processing will be skipped")
  }

  if (!process_har) {
    validation_results$messages <- c(validation_results$messages,
                                     "ℹ️ harmap is FALSE - HAR processing will be skipped")
  }

  # Case Names Validation
  if (length(case.name) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No experiment names provided",
                                     "ℹ️ Please define experiment variable with experiment names")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for duplicate case names
  if (length(case.name) != length(unique(case.name))) {
    duplicate_cases <- case.name[duplicated(case.name)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Duplicate experiment names found:",
                                     paste("   -", duplicate_cases),
                                     "ℹ️ Each experiment name must be unique")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Input Files Validation
  files <- list.files(input.folder, full.names = FALSE, ignore.case = TRUE)

  # Get specific file lists
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  # Extract base names, trimming spaces, and normalizing case
  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))
  case_names_lower <- tolower(trimws(case.name))

  # Check if required files exist
  if (process_sl4 && length(sl4_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No .sl4 files found in the input folder but sl4map is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (process_har && length(har_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No -WEL.har files found in the input folder but harmap is specified")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for complete absence of case files
  available_bases <- c()
  if (process_sl4) available_bases <- c(available_bases, sl4_bases)
  if (process_har) available_bases <- c(available_bases, har_bases)

  missing_cases <- setdiff(case_names_lower, available_bases)
  if (length(missing_cases) == length(case_names_lower)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("❌ None of the specified experiments were found: %s",
                                             paste(case.name[!case_names_lower %in% available_bases],
                                                   collapse = ", ")))
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for partial case matches
  partial_cases <- intersect(case_names_lower, available_bases)
  if (length(partial_cases) < length(case_names_lower) &&
      length(partial_cases) > 0) {
    missing_cases <- case.name[!case_names_lower %in% available_bases]
    validation_results$status <- "warning"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("⚠️ Some specified experiments were not found: %s",
                                             paste(missing_cases, collapse = ", ")))

    # Ask for confirmation
    cat(paste(validation_results$messages, collapse = "\n"), "\n")
    proceed <- .ask_confirmation(
      "Do you want to proceed with the available experiments? (Y/N): ")

    if (!proceed) {
      validation_results$proceed <- FALSE
      return(validation_results)
    }
    validation_results$messages <- character()  # Clear messages after confirmation
  }

  # If everything is fine, add success message
  if (validation_results$status == "ok") {
    validation_results$messages <- c(validation_results$messages,
                                     "✅ All files verified successfully.")
  }

  return(validation_results)
}


#' @title Extract GTAP Data
#' @description Extracts data from GTAP SL4 and HAR files with simplified filtering options.
#'
#' @param sl4file A data frame containing SL4 mapping information, or FALSE to skip SL4 processing.
#' @param harfile A data frame containing HAR mapping information, or FALSE to skip HAR processing.
#' @param experiment A character vector of experiment names to be processed.
#' @param region_select Optional. A vector specifying regions to filter the data.
#' @param sector_select Optional. A vector specifying sectors to filter the data.
#' @param mapping_info A character string indicating the mapping mode (e.g., "GTAPv7"). Default is "GTAPv7".
#' @param project_dir Optional. Path to the project directory containing the "in" folder.
#' @param input_dir Optional. Directory path for input files; overrides project_dir/in if provided.
#' @param sl4_list_name A character string specifying the global variable name for SL4 plotting data. Default is "sl4.plot.data".
#' @param har_list_name A character string specifying the global variable name for HAR plotting data. Default is "har.plot.data".
#' @param sl4_structure_name A character string specifying the global variable name for SL4 structure. Default is "sl4.structure".
#' @param har_structure_name A character string specifying the global variable name for HAR structure. Default is "har.structure".
#'
#' @return A list containing extracted data with applied filters.
#' @export
#'
#' @examples
#' # Extract specific variables with region filters
#' \dontrun{
#' sl4_selected <- data.frame(
#'   Variable = c("qo", "qgdp", "EV"),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- plot_gtap_data(
#'   sl4file = sl4_selected,
#'   harfile = FALSE,
#'   experiment = c("baseline", "policy"),
#'   region_select = c("USA", "CHN", "EU"),
#'   input_dir = "D:/GTAP_inputs"
#' )
#' }
#'
plot_gtap_data <- function(sl4file,
                           harfile,
                           experiment,
                           region_select = NULL,
                           sector_select = NULL,
                           mapping_info = "GTAPv7",
                           project_dir = NULL,
                           input_dir = NULL,
                           subtotal = FALSE,
                           sl4_list_name = "sl4.plot.data",
                           har_list_name = "har.plot.data",
                           sl4_structure_name = "sl4.structure",
                           har_structure_name = "har.structure") {

  if (is.null(input_dir) && !is.null(project_dir)) {
    input_dir <- file.path(project_dir, "in")
  }

  if (is.null(input_dir)) {
    stop("Either input_dir or project_dir must be specified")
  }

  process_sl4 <- !identical(sl4file, FALSE)
  process_har <- !identical(harfile, FALSE)

  validation_result <- .validate_extract_files(
    sl4map = sl4file,
    harmap = harfile,
    input.folder = input_dir,
    case.name = experiment,
    info.mode = mapping_info
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))

  valid_sl4_cases <- experiment[tolower(experiment) %in% sl4_bases]
  valid_har_cases <- experiment[tolower(experiment) %in% har_bases]

  # Initialize return objects
  result <- list()
  sl4_data <- NULL
  har_data <- NULL
  sl4structure <- NULL
  harstructure <- NULL

  # Process SL4 files
  if (process_sl4 && length(valid_sl4_cases) > 0) {
    sl4_variables <- if (is.null(sl4file)) NULL else sl4file$Variable

    message("Processing SL4 files...")

    # Load SL4 files
    sl4_data_raw <- setNames(
      lapply(valid_sl4_cases, function(scenario) {
        sl4_path <- file.path(input_dir, paste0(scenario, ".sl4"))
        if (file.exists(sl4_path)) {
          tryCatch({
            HARplus::load_sl4x(sl4_path, select_header = sl4_variables)
          }, error = function(e) {
            message(sprintf("Error processing %s.sl4: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s.sl4 (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_sl4_cases
    )

    sl4_data_raw <- sl4_data_raw[!sapply(sl4_data_raw, is.null)]

    if (length(sl4_data_raw) > 0) {
      # Generate SL4 variable structure
      sl4structure <- do.call(
        HARplus::compare_var_structure,
        c(list(NULL, keep_unique = TRUE), sl4_data_raw)
      )[["match"]]

      sl4file_name <- deparse(substitute(sl4file))
      sl4structure <- dplyr::left_join(sl4file, sl4structure[c("Variable", "Dimensions")], by = "Variable")
      sl4structure <- sl4structure[order(sl4structure$Dimensions), ]
      sl4structure$Unit <- NULL

      # Assign to global environment if name provided
      if (!is.null(sl4_structure_name)) {
        assign(sl4file_name, sl4structure, envir = .GlobalEnv)
      }

      # Use get_data_by_dims with merge=TRUE
      sl4_data <- do.call(
        HARplus::get_data_by_dims,
        c(
          list(
            patterns = NULL,
            experiment_names = names(sl4_data_raw),
            merge_data = TRUE,
            subtotal_level = subtotal
          ),
          sl4_data_raw
        )
      )

      # Apply mapping info
      sl4_data <- add_mapping_info(sl4_data, external_map = sl4file, mapping = mapping_info)

      # Apply filters
      sl4_data <- .apply_filters(
        sl4_data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )

      # Assign to global environment if name provided
      if (!is.null(sl4_list_name)) {
        assign(sl4_list_name, sl4_data, envir = .GlobalEnv)
      }

      # Add to result list
      result$sl4_data <- sl4_data
    }
  }

  # Process HAR files
  if (process_har && length(valid_har_cases) > 0) {
    har_variables <- if (is.null(harfile)) NULL else harfile$Variable

    message("Processing HAR files...")

    # Load HAR files
    har_data_raw <- setNames(
      lapply(valid_har_cases, function(scenario) {
        har_path <- file.path(input_dir, paste0(scenario, "-WEL.har"))
        if (file.exists(har_path)) {
          tryCatch({
            HARplus::load_harx(har_path, select_header = har_variables)
          }, error = function(e) {
            message(sprintf("Error processing %s-WEL.har: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s-WEL.har (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_har_cases
    )

    har_data_raw <- har_data_raw[!sapply(har_data_raw, is.null)]

    if (length(har_data_raw) > 0) {
      # Generate HAR variable structure
      harstructure <- do.call(
        HARplus::compare_var_structure,
        c(list(NULL, keep_unique = TRUE), har_data_raw)
      )[["match"]]

      harfile_name <- deparse(substitute(harfile))
      harstructure <- dplyr::left_join(harfile, harstructure[c("Variable", "Dimensions")], by = "Variable")
      harstructure <- harstructure[order(harstructure$Dimensions), ]
      harstructure$Unit <- NULL

      # Assign to global environment if name provided
      if (!is.null(har_structure_name)) {
        assign(harfile_name, harstructure, envir = .GlobalEnv)
      }

      # Use get_data_by_dims with merge=TRUE
      har_data <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            NULL,
            experiment_names = names(har_data_raw),
            merge_data = TRUE,
            subtotal_level = subtotal
          ),
          har_data_raw
        )
      )

      # Apply mapping info
      har_data <- add_mapping_info(har_data, external_map = harfile, mapping = mapping_info)

      # Apply filters
      har_data <- .apply_filters(
        har_data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )

      # Assign to global environment if name provided
      if (!is.null(har_list_name)) {
        assign(har_list_name, har_data, envir = .GlobalEnv)
      }

      # Add to result list
      result$har_data <- har_data
    }
  }

  message("GTAP plotting data extraction completed!")

  return(invisible(NULL))
}
