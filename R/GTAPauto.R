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
#' @description Validates the input folder, mapping file, mapping sheets, case names, and input files for GTAP processing.
#' @param input.folder A character string specifying the path to the folder containing GTAP files.
#' @param output.folder A character string specifying the path to the folder where output files will be saved.
#' @param map.folder A character string specifying the path to the folder containing the mapping file "OutputMapping.xlsx".
#' @param case.name A character vector of case names (experiment names) to validate.
#' @param info.mode A character string specifying the information mode. If \code{NULL}, defaults to "GTAPv7".
#' @param sl4map A data frame representing the SL4 mapping sheet. It must include the columns "Variable", "Description", and "Unit".
#' @param harmap A data frame representing the HAR mapping sheet. It must include the columns "Variable", "Description", and "Unit".
#' @param output_formats A character vector of output formats selected by the user. If \code{NULL}, the function automatically determines the formats using global defaults.
#' @return A list with three elements:
#' \item{status}{A character string indicating the validation status ("ok", "error", or "warning").}
#' \item{messages}{A character vector of messages generated during validation.}
#' \item{proceed}{A logical value indicating whether processing should continue.}
#' @importFrom readxl read_excel
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{process_gtap_data}}
#'
.validate_gtap_files <- function(input.folder, output.folder, map.folder,
                                 case.name, info.mode, sl4map, harmap, output_formats) {
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

  # Check info.mode - set default if missing
  if (is.null(info.mode)) {
    info.mode <- "GTAPv7"
    message("ℹ️ info.mode not specified, using default: GTAPv7")
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

  # Check mapping file existence
  mapping.output <- file.path(map.folder, "OutputMapping.xlsx")
  if (!file.exists(mapping.output)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Required mapping file 'OutputMapping.xlsx' not found in map folder",
                                     "ℹ️ Please ensure:",
                                     "   1. The file name is exactly 'OutputMapping.xlsx'",
                                     "   2. The file is located in the map folder",
                                     "   3. The file contains required sheets: 'SL4File' and 'HARFile'")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check sl4map and harmap existence and structure
  if (is.null(sl4map) || !all(c("Variable", "Description", "Unit") %in% names(sl4map))) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Invalid SL4File sheet structure in OutputMapping.xlsx",
                                     "ℹ️ Required columns: Variable, Description, Unit")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  if (is.null(harmap) || !all(c("Variable", "Description", "Unit") %in% names(harmap))) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Invalid HARFile sheet structure in OutputMapping.xlsx",
                                     "ℹ️ Required columns: Variable, Description, Unit")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Case Names Validation
  if (length(case.name) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No case names provided",
                                     "ℹ️ Please define case.name variable with experiment names")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for duplicate case names
  if (length(case.name) != length(unique(case.name))) {
    duplicate_cases <- case.name[duplicated(case.name)]
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ Duplicate case names found:",
                                     paste("   -", duplicate_cases),
                                     "ℹ️ Each case name must be unique")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Input Files Validation
  files <- list.files(input.folder, full.names = FALSE, ignore.case = TRUE)

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
  case_names_lower <- tolower(trimws(case.name))

  # Check if any files exist
  if (length(sl4_files) == 0 && length(har_files) == 0) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     "❌ No .sl4 or -WEL.har files found in the input folder")
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for complete absence of case files
  missing_cases <- setdiff(case_names_lower, union(sl4_bases, har_bases))
  if (length(missing_cases) == length(case_names_lower)) {
    validation_results$status <- "error"
    validation_results$messages <- c(validation_results$messages,
                                     sprintf("❌ None of the specified cases were found: %s",
                                             paste(case.name[!case_names_lower %in% union(sl4_bases, har_bases)],
                                                   collapse = ", ")))
    validation_results$proceed <- FALSE
    return(validation_results)
  }

  # Check for mismatched file counts
  if (length(sl4_files) != length(har_files)) {
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

  # Check for file name mismatches
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

  # Check for partial case matches
  partial_cases <- intersect(case_names_lower, union(sl4_bases, har_bases))
  if (length(partial_cases) < length(case_names_lower) &&
      length(partial_cases) > 0) {
    missing_cases <- case.name[!case_names_lower %in% union(sl4_bases, har_bases)]
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
                                     sprintf("✅ All files verified successfully. Found %d matched pairs.",
                                             length(matched_pairs)))
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
                                    var_pattern = "qxs") {
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

  return(grouped_data)
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
                               output_path, formats) {
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
      message("Exporting macro data...")

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
  }, error = function(e) {
    warning("Error processing macro data: ", e$message)
  })

  invisible(NULL)
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
#' @description Processes GTAP data from all listed SL4 and HAR files automatically
#' using external mapping files. By default, this function integrates all required
#' processing steps from the HARplus package according to the GTAPv7 definition.
#' @details This function integrates all required processing steps from the HARplus
#' package according to the GTAP version 7 model definition. The workflow includes:
#' \itemize{
#'   \item Loading SL4 data using \code{HARplus::load_sl4x}.
#'   \item Grouping SL4 data with \code{HARplus::group_data_by_dims}.
#'   \item Mapping and renaming data via \code{add_mapping_info}
#'   and \code{rename_GTAP_bilateral}.
#'   \item Processing macro data using \code{gtap_macros_data}.
#'   \item Loading HAR data using \code{HARplus::load_harx}.
#'   \item Extracting HAR variables with \code{HARplus::get_data_by_var}.
#'   \item Exporting processed data using \code{HARplus::export_data}.
#' }
#' @param sl4map List. Mapping for SL4 variables.
#' @param harmap List. Mapping for HAR variables.
#' @param case.name Character vector. Case names.
#' @param info.mode Any. Information mode for mapping.
#' @param input.folder Character. Path to the input folder.
#' @param output.folder Character. Path to the output folder.
#' @param output_formats Character vector. Output formats (e.g., c("xlsx", "csv")).
#'
#' @return Invisibly returns NULL.
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{gtap_macros_data}}
#'
process_gtap_data <- function(sl4map, harmap, case.name, info.mode, input.folder = NULL,
                              map.folder = NULL, output.folder  = NULL, output_formats = NULL) {

  if (is.null(map.folder)) {
    map.folder <- paste0(project.folder,"/map")
  }

  if (is.null(output_formats)) {
    output_formats <- .output_format()
  }

  if (is.null(input.folder)) {
    input.folder <- paste0(project.folder,"/in")
  }

  if (is.null(output.folder)) {
    output.folder <- paste0(project.folder,"/out")
  }

  if (!dir.exists(output.folder)) {
    dir.create(output.folder, recursive = TRUE)
  }

  validation_result <- .validate_gtap_files(
    input.folder  = input.folder,
    output.folder = output.folder,
    map.folder    = map.folder,
    case.name     = case.name,
    info.mode     = info.mode,
    sl4map        = sl4map,
    harmap        = harmap,
    output_formats = output_formats
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  if (validation_result$status != "ok") {
    message("Proceeding with available files as per user confirmation...")
  }

  files <- list.files(input.folder, full.names = FALSE, ignore.case = TRUE)
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))
  valid_cases <- case.name[tolower(case.name) %in% union(sl4_bases, har_bases)]

  if (length(sl4map$Variable) > 0) {
    sl4.data.raw <- setNames(
      lapply(valid_cases, function(scenario) {
        sl4_path <- file.path(input.folder, paste0(scenario, ".sl4"))
        if (file.exists(sl4_path)) {
          tryCatch({
            HARplus::load_sl4x(sl4_path, select_header = sl4map$Variable)
          }, error = function(e) {
            message(sprintf("Error processing %s.sl4: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s.sl4 (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_cases
    )

    sl4.data.raw <- sl4.data.raw[!sapply(sl4.data.raw, is.null)]

    if (length(sl4.data.raw) > 0) {
      has_only_macros <- all(tolower(sl4map$Variable) %in% "macros", na.rm = TRUE)
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

          grouped_sl4 <- add_mapping_info(grouped_sl4, sl4map, mapping = info.mode)
          if (any(grepl("qxs", sl4map$Variable, ignore.case = TRUE))) {
            grouped_sl4 <- .process_bilateral_trade(
              grouped_sl4,
              output.folder,
              output_formats
            )
          }

          HARplus::export_data(
            data = grouped_sl4,
            output_path = output.folder,
            format = output_formats,
            create_subfolder = TRUE,
            multi_sheet_xlsx = TRUE,
            report_output = TRUE
          )
        }
      }

      if (any(tolower(sl4map$Variable) %in% "macros", na.rm = TRUE)) {
        available_sl4_files <- file.path(input.folder, paste0(valid_cases, ".sl4"))
        available_sl4_files <- available_sl4_files[file.exists(available_sl4_files)]

        if (length(available_sl4_files) > 0) {
          .process_macro_data(
            input_files = available_sl4_files,
            experiment_names = tools::file_path_sans_ext(basename(available_sl4_files)),
            output_path = output.folder,
            formats = output_formats
          )
        }
      }
    }
  }

  if (length(harmap$Variable) > 0) {
    har.data.raw <- setNames(
      lapply(valid_cases, function(scenario) {
        har_path <- file.path(input.folder, paste0(scenario, "-WEL.har"))
        if (file.exists(har_path)) {
          tryCatch({
            HARplus::load_harx(har_path, select_header = harmap$Variable)
          }, error = function(e) {
            message(sprintf("Error processing %s-WEL.har: %s", scenario, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s-WEL.har (file not found)", scenario))
          return(NULL)
        }
      }),
      valid_cases
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

      har_data <- add_mapping_info(har_data, harmap, mapping = info.mode)
      processed_decomp <- lapply(har_data, function(df) {
        if ("A" %in% df$Header) {
          welfare_decomp <- df[df$Header == "A", ]
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
        return(df)
      })

      decomposition_data <- list(Decomposition = processed_decomp)
      HARplus::export_data(
        data = decomposition_data,
        output_path = output.folder,
        format = output_formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = TRUE
      )
    }
  }

  if (file.exists(output.folder)) {
    .consolidate_reports(
      output_folder = output.folder,
      final_name = "Report_Output.xlsx",
      remove_sources = TRUE
    )
  }

  message("GTAP data processing completed!")
  invisible(NULL)
}
