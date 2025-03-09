# GTAP Macro Data ---------------------------------------------------------

#' @title Extract and Aggregate Scalar Macroeconomic Variables
#'
#' @description
#' Extracts scalar macroeconomic variables from multiple SL4 datasets and aggregates them into a structured data frame.
#'
#' @param input_path Character. Path to the directory containing SL4 files.
#' @param output_path Character (optional). Directory to save exported data.
#' @param experiment Character vector. List of experiment names corresponding to SL4 files.
#' @param select_var Character vector (optional). List of specific variable names to filter from the final result. If NULL, all variables are returned.
#' @param subtotal_level Logical. Whether to include subtotal levels in the processed data.
#' @param output_formats Character vector (optional). List of output formats (e.g., "csv", "xlsx").
#'
#' @return A sorted data frame containing processed GTAP macro data.
#'
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{auto_gtap_data}}
#'
#' @examples
#' \dontrun{
#' # Extract all variables
#' macros <- gtap_macros_data(
#'   input_path = "path/to/sl4/files",
#'   experiment = c("EXP1", "EXP2"),
#'   subtotal_level = FALSE
#' )
#'
#' # Filter specific variables
#' macros <- gtap_macros_data(
#'   input_path = "path/to/sl4/files",
#'   experiment = c("EXP1", "EXP2"),
#'   select_var = c("qgdp", "pop", "gdpexp")
#' )
#' }
gtap_macros_data <- function(select_var = NULL,
                             experiment = NULL,
                             input_path = NULL,
                             output_path = NULL,
                             output_formats = NULL,
                             subtotal_level = FALSE) {
  macro_vars <- macro_info$Variable

  # Checking Input Files
  is_multiple_experiments <- function(experiment) {
    length(experiment) > 1
  }
  keep_unique_flag <- is_multiple_experiments(experiment)

  macro.raw <- setNames(
    lapply(experiment, function(scenario) {
      sl4_path <- file.path(input_path, paste0(scenario, ".sl4"))
      if (file.exists(sl4_path)) {
        tryCatch({
          HARplus::load_sl4x(sl4_path, select_header = macro_vars)
        }, error = function(e) {
          message(sprintf("Error processing %s.sl4: %s", scenario, e$message))
          return(NULL)
        })
      } else {
        message(sprintf("Skipping %s.sl4 (file not found)", scenario))
        return(NULL)
      }
    }),
    experiment
  )

  macro.raw <- macro.raw[!sapply(macro.raw, is.null)]

  GTAPMacros <- do.call(
    HARplus::get_data_by_var,
    c(
      list(
        experiment = names(macro.raw),
        subtotal_level = subtotal_level,
        merge_data = keep_unique_flag
      ),
      macro.raw
    )
  )

  GTAPMacros <- add_mapping_info(GTAPMacros, mapping = "GTAPv7")

  GTAPMacros_filtered <- .apply_to_dataframes(GTAPMacros, function(df) {
    df[, c("Variable", "Value", "Subtotal", "Experiment", "Description", "Unit"), drop = FALSE]
  })

  if (length(experiment) > 1) {
    GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
  } else {
    GTAPMacros_final <- do.call(rbind, unlist(GTAPMacros_filtered, recursive = FALSE))
  }
  rownames(GTAPMacros_final) <- NULL

  # Apply filtering by Variable if select_var is provided
  if (!is.null(select_var)) {
    GTAPMacros_final <- GTAPMacros_final[GTAPMacros_final$Variable %in% select_var, ]
  }

  GTAPMacros_final <- GTAPMacros_final[order(GTAPMacros_final$Experiment,
                                             GTAPMacros_final$Variable,
                                             GTAPMacros_final$Unit), ]

  if (!is.null(output_path) && !is.null(output_formats)) {
    export_formats <- .output_format(output_formats)
    if (length(export_formats) > 0) {
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }

      macro_list <- list(Macros = GTAPMacros_final)
      message("Exporting macro data...")
      HARplus::export_data(
        data = macro_list,
        output_path = output_path,
        format = export_formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = TRUE
      )
      message("Macro data exported to: ", output_path)
    }
  }

  return(GTAPMacros_final)
}

# Auto Process GTAP Data --------------------------------------------------


#' @title Process GTAP Data Automation with Flexible Output Options
#'
#' @description
#' Processes GTAP data from SL4 and HAR files with options for exporting and preparing plot-ready data.
#'
#' @details
#' This function automates the workflow for processing GTAP model outputs,
#' with flexible output options and optional filtering for region- and sector-specific data.
#'
#' The key parameters `process_sl4_vars` and `process_har_vars` accept three different input types:
#' \itemize{
#'   \item \strong{Data frame}: Contains variable mappings with required "Variable" column. When
#'         provided, only specified variables will be extracted.
#'   \item \strong{NULL}: Extracts all available variables from the respective file type.
#'   \item \strong{FALSE}: Completely skips processing of that file type, allowing
#'         the function to focus only on the other file type.
#' }
#'
#' The `mapping_info` parameter controls how descriptions and units are assigned:
#' \itemize{
#'   \item \strong{GTAPv7}: Uses standard GTAPv7 definitions (default).
#'   \item \strong{Yes}: Uses only the supplied descriptions and units from `sl4_mapping_info` / `har_mapping_info`.
#'   \item \strong{No}: Does not add any descriptions or units.
#'   \item \strong{Mix}: Prioritizes supplied descriptions and units, falling back to GTAPv7
#'         for any missing values.
#' }
#'
#' @param experiment Character vector. Case names to process.
#'
#' # Input Paths & Directories
#' @param project_path Character. Path to the project folder with "in" and "out" subfolders.
#' @param input_path Character. Path to the input folder. Overrides `project_path/in` if specified.
#' @param output_path Character. Path to the output folder. Overrides `project_path/out` if specified.
#'
#' # File Naming & Suffixes
#' @param sl4_suffix Character. Custom suffix for SL4 files (e.g., "" or "-custom").
#' @param har_suffix Character. Custom suffix for HAR files (e.g., "-WEL").
#'
#' # Data Processing & Extraction
#' @param mapping_info Character. Mapping mode: "GTAPv7" (default), "Yes", "No", or "Mix".
#' @param process_sl4_vars Data frame, NULL, or FALSE. Variables to extract from SL4 files. Set to NULL to extract all variables, or FALSE to skip SL4 processing.
#' @param process_har_vars Data frame, NULL, or FALSE. Variables to extract from HAR files. Set to NULL to extract all variables, or FALSE to skip HAR processing.
#' @param sl4_mapping_info Data frame or NULL. Mapping information for SL4 variables (with "Variable", "Description", and "Unit" columns).
#' @param har_mapping_info Data frame or NULL. Mapping information for HAR variables (with "Variable", "Description", and "Unit" columns).
#' @param sl4_extract_method Character. SL4 extraction method. Options: "get_data_by_dims", "get_data_by_var", or "group_data_by_dims".
#' @param har_extract_method Character. HAR extraction method. Options: "get_data_by_dims", "get_data_by_var", or "group_data_by_dims".
#' @param sl4_priority Optional list. Priority rules for SL4 data grouping.
#' @param har_priority Optional list. Priority rules for HAR data grouping.
#'
#' # Data Filtering
#' @param region_select Optional character vector. Specifies regions to filter the data.
#' @param sector_select Optional character vector. Specifies sectors to filter the data.
#' @param subtotal_level Logical. If TRUE, includes subtotal data. Default is FALSE.
#'
#' # Output Settings
#' @param plot_data Logical. If TRUE, prepares data for plotting and assigns to variables.
#' @param output_formats Character vector or list. Exports data in these formats (valid: "csv", "stata", "rds", "txt").
#'
#' # Output Variables for Plotting
#' @param sl4_output_name Character. Variable name for SL4 plotting data if generating plot data. Default is "sl4.plot.data".
#' @param har_output_name Character. Variable name for HAR plotting data if generating plot data. Default is "har.plot.data".
#' @param macro_output_name Character. Variable name for GTAP macro data if generating plot data. Default is "GTAPMacro".
#'
#' @return Returns the processed data invisibly, which will not be printed to the console.
#'
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{gtap_macros_data}}
#'
#' @examples
#' \dontrun{
#' # Extract data with region and experiment filters
#' auto_gtap_data(
#'   process_sl4_vars = NULL,
#'   process_har_vars = NULL,
#'   sl4_mapping_info = sl4_mapping_info,
#'   har_mapping_info = har_mapping_info,
#'   region_select = selected_regions,
#'   sector_select = NULL,
#'   subtotal_level = FALSE,
#'   experiment = experiment,
#'   mapping_info = "GTAPv7",
#'   project_path = project_folder,
#'   plot_data = TRUE,
#'   output_formats = list(
#'     "csv" = "No",
#'     "stata" = "No",
#'     "rds" = "No",
#'     "txt" = "No"))
#' }
#'
auto_gtap_data <- function(experiment,
                           project_path = NULL, input_path = NULL, output_path = NULL,
                           sl4_suffix = "", har_suffix = "-WEL",
                           mapping_info = "GTAPv7",
                           process_sl4_vars = NULL, process_har_vars = NULL,
                           sl4_mapping_info = NULL, har_mapping_info = NULL,
                           sl4_extract_method = "group_data_by_dims", har_extract_method = "get_data_by_var",
                           sl4_priority = NULL, har_priority = NULL,
                           region_select = NULL, sector_select = NULL, subtotal_level = FALSE,
                           plot_data = FALSE, output_formats = NULL,
                           sl4_output_name = "sl4.plot.data",
                           har_output_name = "har.plot.data",
                           macro_output_name = "GTAPMacro") {

  # Initial Setup--------------------------------------------------------------
  export_formats <- .output_format(output_formats)
  export_data <- length(export_formats) > 0
  process_log <- list()

  if (!is.null(project_path)) {
    if (is.null(input_path)) input_path <- file.path(project_path, "in")
    if (is.null(output_path)) output_path <- file.path(project_path, "out")
  }

  all_data <- list()

  # Extract Variable Names-----------------------------------------------------
  extract_var_names <- function(var_def) {
    if (is.data.frame(var_def) && "Variable" %in% names(var_def)) {
      return(var_def$Variable)
    } else {
      return(var_def)
    }
  }

  sl4var_vars <- extract_var_names(process_sl4_vars)
  harvar_vars <- extract_var_names(process_har_vars)

  process_sl4 <- !identical(process_sl4_vars, FALSE)
  process_har <- !identical(process_har_vars, FALSE)
  process_macro <- process_sl4 && (is.null(process_sl4_vars) ||
                                     any(tolower(sl4var_vars) %in% c("macro", "macros", "gtapmacro")))

  process_qxs <- process_sl4 && (is.null(process_sl4_vars) ||
                                   any(grepl("qxs", sl4var_vars, ignore.case = TRUE)))

  # Filtering QXS out of regular SL4 vars
  if (process_qxs && is.character(sl4var_vars) && !is.null(sl4var_vars)) {
    qxs_vars <- sl4var_vars[grepl("qxs", sl4var_vars, ignore.case = TRUE)]
    sl4var_vars <- sl4var_vars[!grepl("qxs", sl4var_vars, ignore.case = TRUE)]
    if (length(sl4var_vars) == 0) {
      sl4var_vars <- NULL
    }
  }

  # Define File Suffixes-------------------------------------------------------
  sl4_file_suffix <- if (nzchar(sl4_suffix)) paste0(sl4_suffix, ".sl4") else ".sl4"
  har_file_suffix <- if (nzchar(har_suffix)) paste0(har_suffix, ".har") else ".har"

  # Validate Inputs & Files-----------------------------------------------------
  validation_result <- .validate_gtap_files(
    input_dir = input_path,
    output_dir = output_path,
    experiment = experiment,
    mapping_info = mapping_info,
    sl4var = process_sl4,
    harvar = process_har,
    sl4map = sl4_mapping_info,
    harmap = har_mapping_info,
    output_formats = if(export_data) output_formats else NULL,
    plot_data = plot_data,
    sl4_file_suffix = sl4_file_suffix,
    har_file_suffix = har_file_suffix
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  # Identify Available Experiment Files-----------------------------------------
  files <- list.files(input_path, full.names = FALSE, ignore.case = TRUE)

  # Simple file pattern matching without excessive messages
  find_valid_cases <- function(file_suffix, experiments) {
    pattern_str <- paste0(file_suffix, "$")
    files_matching <- files[grepl(pattern_str, files, ignore.case = TRUE)]

    bases <- character(0)
    for (file in files_matching) {
      base <- substr(file, 1, nchar(file) - nchar(file_suffix))
      bases <- c(bases, tolower(trimws(base)))
    }

    valid_cases <- experiments[tolower(experiments) %in% bases]
    return(valid_cases)
  }

  valid_sl4_cases <- find_valid_cases(sl4_file_suffix, experiment)
  valid_har_cases <- find_valid_cases(har_file_suffix, experiment)

  # Process Data Function-----------------------------------
  transform_data <- function(data, external_map, apply_filters = TRUE) {
    if (is.null(data)) return(NULL)

    data <- rename_GTAP_bilateral(data)
    data <- add_mapping_info(data, mapping = mapping_info, external_map = external_map)

    if (apply_filters && (!is.null(region_select) || !is.null(sector_select))) {
      data <- .apply_filters(
        data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )
    }

    if (length(data) == 1 && is.list(data) && !is.data.frame(data)) {
      data <- data[[1]]
    }

    return(data)
  }

  process_data <- function(valid_cases, file_suffix, select_vars, method_name,
                           load_func, list_name, priority_list, data_name) {
    if (length(valid_cases) == 0) return(NULL)

    data_raw <- setNames(
      lapply(valid_cases, function(scenario) {
        file_path <- file.path(input_path, paste0(scenario, file_suffix))
        if (file.exists(file_path)) {
          tryCatch({
            load_func(file_path, select_header = select_vars)
          }, error = function(e) {
            message(sprintf("Error processing %s: %s", file_path, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s (file not found)", file_path))
          return(NULL)
        }
      }),
      valid_cases
    )

    data_raw <- data_raw[!sapply(data_raw, is.null)]
    if (length(data_raw) == 0) return(NULL)

    method_map <- list(
      "get_data_by_dims" = HARplus::get_data_by_dims,
      "get_data_by_var" = HARplus::get_data_by_var,
      "group_data_by_dims" = HARplus::group_data_by_dims
    )

    if (!is.character(method_name) || !method_name %in% names(method_map)) {
      stop(paste0("Invalid method provided. Choose from: '",
                  paste(names(method_map), collapse = "', '"), "'."))
    }

    keep_unique_flag <- length(data_raw) > 1

    if (method_name == "group_data_by_dims") {
      params <- list(
        experiment_names = names(data_raw),
        auto_rename = TRUE,
        priority = priority_list %||% list("Sector" = c("COMM", "ACTS"), "Region" = c("REG")),
        subtotal_level = subtotal_level
      )
    } else {
      params <- list(
        experiment_names = names(data_raw),
        subtotal_level = subtotal_level,
        merge_data = keep_unique_flag
      )
    }

    grouped_data <- tryCatch({
      do.call(method_map[[method_name]], c(params, data_raw))
    }, error = function(e) {
      message(sprintf("Error in grouping data: %s", e$message))
      return(NULL)
    })

    if (!keep_unique_flag && is.list(grouped_data) && length(grouped_data) == 1 && !is.data.frame(grouped_data)) {
      grouped_data <- grouped_data[[1]]
    }

    if (plot_data && !is.null(list_name) && !is.null(grouped_data)) {
      raw_data <- grouped_data
    }
    return(grouped_data)
  }

  # Export Processed Data-------------------------------------------------------
  export_processed_data <- function(data, name, output_path) {
    if (export_data && !is.null(data) && !is.null(output_path)) {
      message(paste("Exporting", name, "data..."))
      export_list <- if (is.list(data) && !is.data.frame(data) && length(names(data)) > 0) {
        data
      } else {
        setNames(list(data), name)
      }

      # Export the data without generating reports
      HARplus::export_data(
        data = export_list,
        output_path = output_path,
        format = export_formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = FALSE
      )

      message(paste(name, "data exported to:", output_path))
      return(TRUE)
    }
    return(FALSE)
  }

  # Process Macro Data----------------------------------------------------------
  if (process_macro && length(valid_sl4_cases) > 0) {
    message("Processing GTAP Macro Data")
    macro_data <- tryCatch({
      # Use the proper macro data function with custom suffix
      macro_raw <- setNames(
        lapply(valid_sl4_cases, function(scenario) {
          sl4_path <- file.path(input_path, paste0(scenario, sl4_file_suffix))
          if (file.exists(sl4_path)) {
            tryCatch({
              HARplus::load_sl4x(sl4_path, select_header = macro_info$Variable)
            }, error = function(e) {
              message(sprintf("Error processing %s: %s", sl4_path, e$message))
              return(NULL)
            })
          } else {
            message(sprintf("Skipping %s (file not found)", sl4_path))
            return(NULL)
          }
        }),
        valid_sl4_cases
      )

      macro_raw <- macro_raw[!sapply(macro_raw, is.null)]

      # Process macro data with consistent merge logic
      GTAPMacros <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            experiment_names = names(macro_raw),
            subtotal_level = subtotal_level,
            merge_data = length(macro_raw) > 1
          ),
          macro_raw
        )
      )

      # Add mapping info
      GTAPMacros <- add_mapping_info(GTAPMacros, mapping = "GTAPv7")

      # Filter columns
      GTAPMacros_filtered <- .apply_to_dataframes(GTAPMacros, function(df) {
        df[, c("Variable", "Value", "Subtotal", "Experiment", "Description", "Unit"), drop = FALSE]
      })

      # Simplify structure for single experiment
      if (length(valid_sl4_cases) == 1) {
        if (is.list(GTAPMacros_filtered) && length(GTAPMacros_filtered) == 1 && !is.data.frame(GTAPMacros_filtered)) {
          GTAPMacros_final <- do.call(rbind, unlist(GTAPMacros_filtered, recursive = FALSE))
        } else {
          GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
        }
      } else {
        GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
      }
      rownames(GTAPMacros_final) <- NULL

      GTAPMacros_final <- GTAPMacros_final[order(GTAPMacros_final$Experiment,
                                                 GTAPMacros_final$Variable,
                                                 GTAPMacros_final$Unit), ]

      rename_GTAP_bilateral(GTAPMacros_final)
    }, error = function(e) {
      process_log$macro <- sprintf("Error processing GTAP Macro Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(macro_data)) {
      process_log$macro <- "GTAP Macro Data processed successfully"
      all_data$GTAPMacros <- macro_data

      # Fixed issue: Assign to parent environment if plot_data is TRUE
      if (plot_data) {
        assign(macro_output_name, macro_data, envir = parent.frame())
      }

      # Export the macro data
      export_processed_data(macro_data, "GTAPMacros", output_path)
    }
  }

  # Process SL4 Data------------------------------------------------------------
  if (process_sl4 && length(valid_sl4_cases) > 0) {
    message("Processing SL4 Data")
    process_regular_sl4 <- TRUE
    if (process_macro && !is.null(sl4var_vars) && is.character(sl4var_vars)) {
      if (length(sl4var_vars) == 1 && tolower(sl4var_vars) == "macros") {
        process_regular_sl4 <- FALSE
      }
    }

    if (process_regular_sl4) {
      grouped_sl4 <- tryCatch({
        process_data(valid_sl4_cases, sl4_file_suffix, sl4var_vars, sl4_extract_method,
                     HARplus::load_sl4x, sl4_output_name, sl4_priority, "SL4")
      }, error = function(e) {
        process_log$sl4 <- sprintf("Error processing SL4 Data: %s", e$message)
        return(NULL)
      })

      if (!is.null(grouped_sl4)) {
        process_log$sl4 <- "SL4 Data processed successfully"
        grouped_sl4 <- transform_data(grouped_sl4, sl4_mapping_info)
        if (plot_data && !is.null(sl4_output_name)) {
          assign(sl4_output_name, grouped_sl4, envir = parent.frame())
        }
        all_data$sl4_data <- grouped_sl4

        # Export processed SL4 data
        export_processed_data(grouped_sl4, "SL4", output_path)
      }
    }
  }


  # Process Bilateral Trade -------------------------------------------------
  if (process_qxs && length(valid_sl4_cases) > 0) {
    message("Processing QXS Bilateral Trade Data")

    bilateral_data <- tryCatch({
      process_data(valid_sl4_cases, sl4_file_suffix, "qxs", "get_data_by_var",
                   HARplus::load_sl4x, "bilateral_data", NULL, "QXS")
    }, error = function(e) {
      process_log$qxs <- sprintf("Error processing QXS Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(bilateral_data)) {
      process_log$qxs <- "QXS Bilateral Data processed successfully"
      bilateral_data <- transform_data(bilateral_data, sl4_mapping_info)
      if (plot_data && !is.null("bilateral_data")) {
        assign("bilateral_data", bilateral_data, envir = parent.frame())
      }
      all_data$bilateral_data <- bilateral_data

      # Export using the same export_processed_data helper as SL4
      export_processed_data(bilateral_data, "BilateralTrade", output_path)
    }
  }


  # Process HAR Data------------------------------------------------------------
  if (process_har && length(valid_har_cases) > 0) {
    message("Processing HAR Data")
    har_data <- tryCatch({
      process_data(valid_har_cases, har_file_suffix, harvar_vars, har_extract_method,
                   HARplus::load_harx, har_output_name, har_priority, "HAR")
    }, error = function(e) {
      process_log$har <- sprintf("Error processing HAR Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(har_data)) {
      process_log$har <- "HAR Data processed successfully"
      har_data <- transform_data(har_data, har_mapping_info)
      if (plot_data && !is.null(har_output_name)) {
        assign(har_output_name, har_data, envir = parent.frame())
      }
      all_data$decomposition_data <- har_data

      # Export processed HAR data
      export_processed_data(har_data, "Decomposition", output_path)
    }
  }

  # Final Report Consolidation-------------------------------------------------
  if (export_data && !is.null(output_path)) {
    message("Generating GTAP variable report...")
    .create_gtap_report(all_data, output_path, "Report_Table.xlsx")
  }

  # Summary---------------------------------------------------------------------
  message("\nSummary of Processing:")
  if (!is.null(process_log$macro)) message(process_log$macro)
  if (!is.null(process_log$sl4)) message(process_log$sl4)
  if (!is.null(process_log$har)) message(process_log$har)

  if (all(vapply(process_log, function(x) grepl("successfully", x), logical(1)))) {
    message("\nGTAP data processing completed successfully!")
  } else {
    failed_processes <- names(process_log)[!vapply(process_log, function(x) grepl("successfully", x), logical(1))]
    message(sprintf("\nGTAP data processing completed with errors in: %s", paste(failed_processes, collapse = ", ")))
  }

  return(invisible(all_data))
}
