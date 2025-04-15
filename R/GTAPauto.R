# Auto Process GTAP Data --------------------------------------------------

#' @title Process GTAP Data Automation with Flexible Output Options
#'
#' @description Processes GTAP data from `sl4` and `har` files with options for exporting and preparing plot-ready data.
#'
#' @md
#'
#' @param experiment Character vector. Case names to process.
#' @param input_path Character. Path to the input folder; overrides `project_path/in` if specified.
#' @param output_path Character. Path to the output folder; overrides `project_path/out` if specified.
#' @param sl4_suffix Character. Custom suffix for SL4 files (e.g., `""`, `"-custom"`).
#' @param har_suffix Character. Custom suffix for HAR files (e.g., `"-WEL"`).
#' @param process_sl4_vars Character, `NULL`, or `FALSE`. Variables to extract from SL4 data:
#'   - Character vector: Specific variable names.
#'   - `NULL`: Extract all.
#'   - `FALSE`: Skip SL4 processing.
#'
#' @param process_har_vars Character, `NULL`, or `FALSE`. Variables to extract from HAR data:
#'   - Character vector: Specific variable names.
#'   - `NULL`: Extract all.
#'   - `FALSE`: Skip HAR processing.
#'
#' @param mapping_info Character. Metadata mode for variable descriptions and units.
#'   Options: `"GTAPv7"` (default), `"Yes"`, `"No"`, `"Mix"`.
#'   See [add_mapping_info()] for full details.
#' @param sl4_mapping_info Data frame or `NULL`. Mapping for SL4 variables. Must include columns `"Variable"`, `"Description"`, and `"Unit"`.
#' @param har_mapping_info Data frame or `NULL`. Mapping for HAR variables, structured as above.
#'
#' @param sl4_extract_method Character. SL4 extraction method: `"get_data_by_dims"`, `"get_data_by_var"`, or `"group_data_by_dims"`.
#' @param har_extract_method Character. HAR extraction method. Same options as above.
#'
#' @param sl4_priority Optional list. Required only when `sl4_extract_method` is `"group_data_by_dims"`. Specifies priority rules for SL4 data grouping.
#' @param har_priority Optional list. Required only when `har_extract_method` is `"group_data_by_dims"`. Specifies priority rules for HAR data grouping.
#'
#' @param sl4_convert_unit Character or `NULL`. Optional SL4 unit conversion.
#'   Valid options: `"mil2bil"`, `"bil2mil"`, `"pct2frac"`, `"frac2pct"`.
#'   See \code{\link{convert_units}} for details.
#' @param har_convert_unit Character or `NULL`. Optional HAR unit conversion. Same options as above.
#' @param decimals Integer or `NULL`. Number of decimal places to round numeric values. Set to `NULL` to disable rounding.
#'
#' @param region_select Optional character vector. Filters data to selected regions.
#'   Applies only to the `"REG"` column, which is fixed and cannot be modified.
#' @param sector_select Optional character vector. Filters data to selected sectors.
#'   Applies only to the `"ACTS"` and `"COMM"` columns, which are fixed and cannot be modified.
#'
#' @param subtotal_level Logical. If `TRUE`, includes subtotal rows. Default is `FALSE`.
#' @param rename_columns Logical. If `TRUE` (default), renames GTAP codes (e.g., `"REG"` → `"Region"`, `"COMM"` → `"Commodity"`).
#'
#' @param plot_data Logical. If `TRUE`, generates plot-ready data and assigns to specified variable names.
#' @param output_formats Character vector or list. Output formats for export. Valid values: `"csv"`, `"stata"`, `"rds"`, `"txt"`.
#'
#' @param sl4_output_name Character. Variable name to assign SL4 output. Default: `"sl4.plot.data"`.
#' @param har_output_name Character. Variable name to assign HAR output. Default: `"har.plot.data"`.
#' @param macro_output_name Character. Variable name to assign macro output. Default: `"GTAPMacro"`.
#'
#' @param add_scenario_ranking Logical or `"merged"`. Adds a numeric index for each scenario:
#'   - `TRUE`: Adds a ranking column.
#'   - `"merged"`: Also prefixes experiment names with the rank.
#'
#' @param rank_column Character. Name of the ranking column. Default is `"ScenarioRank"`.
#'
#' @details
#' - To prepare data for plotting and generating tables within the GTAPViz package, the `"Unit"` column must be included in the output.
#'
#' - When using the extraction method `"group_data_by_dims"`, the corresponding priority list must be defined via the `sl4_priority` or `har_priority` argument.
#'   See \code{\link[HARplus]{group_data_by_dims}} for more details.
#'
#' @return
#' A processed GTAP-formatted dataset with standardized structure and metadata, ready for analysis or visualization.
#'
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{convert_units}},
#' \code{\link{rename_value}}
#'
#' @examples
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#'
#' # GTAP Macro Variables from 2 .sl4 Files named (EXP1, EXP2)
#' # Note: No need to add .sl4 to the experiment name
#' gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
#'                             har_suffix = "-WEL",
#'                             input_path = input_path, subtotal_level = FALSE,
#'                             process_sl4_vars = NULL, process_har_vars = NULL,
#'                             mapping_info = "GTAPv7", plot_data = TRUE)
#'
auto_gtap_data <- function(experiment,
                           input_path = NULL, output_path = NULL,
                           sl4_suffix = "", har_suffix = "",
                           process_sl4_vars = NULL, process_har_vars = NULL,
                           mapping_info = "GTAPv7",
                           sl4_mapping_info = NULL, har_mapping_info = NULL,
                           sl4_extract_method = "get_data_by_dims", har_extract_method = "get_data_by_var",
                           sl4_priority = NULL, har_priority = NULL,
                           sl4_convert_unit = NULL, har_convert_unit = NULL,
                           decimals = 4, rename_columns = TRUE,
                           region_select = NULL, sector_select = NULL, subtotal_level = FALSE,
                           plot_data = TRUE, output_formats = NULL,
                           sl4_output_name = "sl4.plot.data",
                           har_output_name = "har.plot.data",
                           macro_output_name = "GTAPMacro",
                           add_scenario_ranking = FALSE,
                           rank_column = "ScenarioRank") {

  # Initial Setup--------------------------------------------------------------
  export_formats <- .output_format(output_formats)
  export_data <- length(export_formats) > 0
  process_log <- list()

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
    har_file_suffix = har_file_suffix,
    sl4_convert_unit = sl4_convert_unit,
    har_convert_unit = har_convert_unit
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

    data <- .apply_to_dataframes(data, rename_GTAP_bilateral)
    data <- add_mapping_info(data, mapping = mapping_info, external_map = external_map)

    if (apply_filters && (!is.null(region_select) || !is.null(sector_select))) {
      data <- .apply_filters(
        data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )
    }

    # Apply decimal formatting to numeric columns
    if (!is.null(decimals)) {
      data <- .format_decimal_places(data, decimals)
    }

    if (apply_filters && (!is.null(region_select) || !is.null(sector_select))) {
      data <- .apply_filters(
        data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )
    }

    # Add scenario ranking if requested
    if (isTRUE(add_scenario_ranking) ||
        (is.character(add_scenario_ranking) && tolower(add_scenario_ranking) == "merged")) {
      merged_display <- FALSE
      if (is.character(add_scenario_ranking) && tolower(add_scenario_ranking) == "merged") {
        merged_display <- TRUE
      }
      data <- .add_scenario_rank(data, experiment, rank_column, merged_display = merged_display)
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

      # Add standard column renaming if requested
      if (rename_columns) {
        params$rename_cols = c(REG = "Region", COMM = "Commodity", ACTS = "Activity")
      }
    } else {
      params <- list(
        experiment_names = names(data_raw),
        subtotal_level = subtotal_level,
        merge_data = keep_unique_flag
      )

      # Add standard column renaming if requested
      if (rename_columns) {
        params$rename_cols = c(REG = "Region", COMM = "Commodity", ACTS = "Activity")
      }
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

      # Add scenario ranking if requested
      if (add_scenario_ranking) {
        merged_display <- FALSE
        if (is.character(add_scenario_ranking) && tolower(add_scenario_ranking) == "merged") {
          merged_display <- TRUE
        }
        GTAPMacros_final <- .add_scenario_rank(GTAPMacros_final, experiment, rank_column, merged_display = merged_display)
      }

      rename_GTAP_bilateral(GTAPMacros_final)
    }, error = function(e) {
      process_log$macro <- sprintf("Error processing GTAP Macro Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(macro_data)) {
      process_log$macro <- "GTAP Macro Data processed successfully"
      all_data$GTAPMacros <- macro_data

      # Apply unit conversion to macro data if specified
      if (!is.null(sl4_convert_unit) && !is.null(macro_data)) {
        message("Applying unit conversion to macro data: ", sl4_convert_unit)
        all_data$GTAPMacros <- convert_units(macro_data, scale_auto = sl4_convert_unit)
        all_data$GTAPMacros <- .format_decimal_places(all_data$GTAPMacros, decimals)

        # Update the plot data variable if it's being generated
        if (plot_data) {
          assign(macro_output_name, list(macros = macro_data), envir = parent.frame())
        }
      }

      # Fixed issue: Assign to parent environment if plot_data is TRUE
      if (plot_data) {
        assign(macro_output_name, list(macros = macro_data), envir = parent.frame())
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
      # Exclude QXS variables from SL4 processing if we're handling them separately
      sl4_vars_to_use <- sl4var_vars
      if (process_qxs && !is.null(sl4_vars_to_use) && is.character(sl4_vars_to_use)) {
        sl4_vars_to_use <- sl4_vars_to_use[!grepl("qxs", sl4_vars_to_use, ignore.case = TRUE)]
        if (length(sl4_vars_to_use) == 0) sl4_vars_to_use <- NULL
      }

      grouped_sl4 <- tryCatch({
        process_data(valid_sl4_cases, sl4_file_suffix, sl4_vars_to_use, sl4_extract_method,
                     HARplus::load_sl4x, sl4_output_name, sl4_priority, "SL4")
      }, error = function(e) {
        process_log$sl4 <- sprintf("Error processing SL4 Data: %s", e$message)
        return(NULL)
      })

      if (!is.null(grouped_sl4)) {
        process_log$sl4 <- "SL4 Data processed successfully"
        grouped_sl4 <- transform_data(grouped_sl4, sl4_mapping_info)

        if (is.data.frame(grouped_sl4)) {
          name <- switch(
            sl4_extract_method,
            "get_data_by_var" = if ("Variable" %in% names(grouped_sl4)) as.character(grouped_sl4$Variable[1]) else "data",
            "get_data_by_dims" = if ("Dimension" %in% names(grouped_sl4)) as.character(grouped_sl4$Dimension[1]) else "data",
            "group_data_by_dims" = "data"
          )
          grouped_sl4 <- setNames(list(grouped_sl4), name)
        }

        if (plot_data && !is.null(sl4_output_name)) {
          assign(sl4_output_name, grouped_sl4, envir = parent.frame())
        }
        all_data$sl4_data <- grouped_sl4

        # Apply unit conversion to SL4 data if specified
        if (!is.null(sl4_convert_unit) && !is.null(grouped_sl4)) {
          message("Applying unit conversion to SL4 data: ", sl4_convert_unit)
          all_data$sl4_data <- convert_units(grouped_sl4, scale_auto = sl4_convert_unit)
          all_data$sl4_data <- .format_decimal_places(all_data$sl4_data, decimals)

          # Update the plot data variable if it's being generated
          if (plot_data && !is.null(sl4_output_name)) {
            assign(sl4_output_name, all_data$sl4_data, envir = parent.frame())
          }
        }

        # Export processed SL4 data
        export_processed_data(grouped_sl4, "SL4", output_path)
      }
    }
  }

  # Process Bilateral Trade -------------------------------------------------
  # Process Bilateral Trade -------------------------------------------------
  if (process_qxs && length(valid_sl4_cases) > 0) {
    message("Processing QXS Bilateral Trade Data")

    # Capture and suppress console output
    invisible(capture.output({
      bilateral_data <- tryCatch({
        process_data(valid_sl4_cases, sl4_file_suffix, "qxs", "get_data_by_var",
                     HARplus::load_sl4x, "bilateral_data", NULL, "QXS")
      }, error = function(e) {
        process_log$qxs <- sprintf("Error processing QXS Data: %s", e$message)
        return(NULL)
      })
    }))

    if (!is.null(bilateral_data)) {
      process_log$qxs <- "QXS Bilateral Data processed successfully"
      bilateral_data <- transform_data(bilateral_data, sl4_mapping_info)

      if (plot_data) {
        assign("bilateral_data", list(qxs = bilateral_data), envir = parent.frame())
      }
      all_data$bilateral_data <- bilateral_data

      # Apply unit conversion to bilateral data if specified
      if (!is.null(sl4_convert_unit) && !is.null(bilateral_data)) {
        message("Applying unit conversion to bilateral data: ", sl4_convert_unit)
        all_data$bilateral_data <- convert_units(bilateral_data, scale_auto = sl4_convert_unit)
        all_data$bilateral_data <- .format_decimal_places(all_data$bilateral_data, decimals)

        # Update the plot data variable if it's being generated
        if (plot_data) {
          assign("bilateral_data", list(qxs = all_data$bilateral_data), envir = parent.frame())
        }
      }

      # Export using the same export_processed_data helper as SL4
      export_processed_data(bilateral_data, "BilateralTrade", output_path)
    }
  }

  # Process HAR Data------------------------------------------------------------
  if (process_sl4 && length(valid_sl4_cases) > 0) {
    message("Processing SL4 Data")
    process_regular_sl4 <- TRUE
    if (process_macro && !is.null(sl4var_vars) && is.character(sl4var_vars)) {
      if (length(sl4var_vars) == 1 && tolower(sl4var_vars) == "macros") {
        process_regular_sl4 <- FALSE
      }
    }

    if (process_regular_sl4) {
      # Exclude QXS variables from SL4 processing if we're handling them separately
      sl4_vars_to_use <- sl4var_vars
      if (process_qxs && !is.null(sl4_vars_to_use) && is.character(sl4_vars_to_use)) {
        sl4_vars_to_use <- sl4_vars_to_use[!grepl("qxs", sl4_vars_to_use, ignore.case = TRUE)]
        if (length(sl4_vars_to_use) == 0) sl4_vars_to_use <- NULL
      }

      grouped_sl4 <- tryCatch({
        process_data(valid_sl4_cases, sl4_file_suffix, sl4_vars_to_use, sl4_extract_method,
                     HARplus::load_sl4x, sl4_output_name, sl4_priority, "SL4")
      }, error = function(e) {
        process_log$sl4 <- sprintf("Error processing SL4 Data: %s", e$message)
        return(NULL)
      })

      if (!is.null(grouped_sl4)) {
        process_log$sl4 <- "SL4 Data processed successfully"
        grouped_sl4 <- transform_data(grouped_sl4, sl4_mapping_info)

        if (is.data.frame(grouped_sl4)) {
          name <- switch(
            sl4_extract_method,
            "get_data_by_var" = if ("Variable" %in% names(grouped_sl4)) as.character(grouped_sl4$Variable[1]) else "data",
            "get_data_by_dims" = if ("Dimension" %in% names(grouped_sl4)) as.character(grouped_sl4$Dimension[1]) else "data",
            "group_data_by_dims" = "data"
          )
          grouped_sl4 <- setNames(list(grouped_sl4), name)
        }

        if (plot_data && !is.null(sl4_output_name)) {
          assign(sl4_output_name, grouped_sl4, envir = parent.frame())
        }
        all_data$sl4_data <- grouped_sl4

        # Apply unit conversion to SL4 data if specified
        if (!is.null(sl4_convert_unit) && !is.null(grouped_sl4)) {
          message("Applying unit conversion to SL4 data: ", sl4_convert_unit)
          all_data$sl4_data <- convert_units(grouped_sl4, scale_auto = sl4_convert_unit)
          all_data$sl4_data <- .format_decimal_places(all_data$sl4_data, decimals)

          # Update the plot data variable if it's being generated
          if (plot_data && !is.null(sl4_output_name)) {
            assign(sl4_output_name, all_data$sl4_data, envir = parent.frame())
          }
        }

        # Export processed SL4 data
        export_processed_data(grouped_sl4, "SL4", output_path)
      }
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
  if (!is.null(process_log$qxs)) message(process_log$qxs)

  if (all(vapply(process_log, function(x) grepl("successfully", x), logical(1)))) {
    message("GTAP data processing completed successfully!")
  } else {
    failed_processes <- names(process_log)[!vapply(process_log, function(x) grepl("successfully", x), logical(1))]
    message(sprintf("GTAP data processing completed with errors in: %s", paste(failed_processes, collapse = ", ")))
  }

  # Reset console output
  on.exit({
    cat("\r")  # Carriage return without newline
    flush.console()
  }, add = TRUE)

  return(invisible(all_data))
}
