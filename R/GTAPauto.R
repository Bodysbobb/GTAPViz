#' @title Extract and Aggregate Scalar Macroeconomic Variables
#' @description Extracts scalar macroeconomic variables from multiple SL4 datasets and aggregates them into a structured data frame.
#'
#' @param input_dir Character. Directory containing SL4 files.
#' @param experiment_names Character vector. List of experiment names corresponding to SL4 files.
#' @param output_dir Character (optional). Directory to save exported data.
#' @param output_formats Character vector (optional). List of output formats (e.g., "csv", "xlsx").
#' @param subtotal_level Logical. Whether to include subtotal levels in the processed data.
#' @param select_var Character vector (optional). List of specific variable names to filter from the final result. If NULL, all variables are returned.
#'
#' @return A sorted dataframe containing processed GTAP macro data.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{add_mapping_info}}, \code{\link{process_gtap_data}}
#' @export
#' @examples
#' \dontrun{
#' # Method 1: Extract all variables
#' input_dir <- paste0(input.folder)
#' experiment <- c("EXP1", "EXP2") # File name (.sl4)
#' Macros <- gtap_macros_data(input_dir, experiment_names = experiment,
#' subtotal_level = FALSE)
#'
#' # Method 2: Filter specific variables
#' Macros <- gtap_macros_data(input_dir, experiment_names = experiment,
#'                           select_var = c("qgdp", "pop", "gdpexp"))
#' }
gtap_macros_data <- function(input_dir = NULL,
                             experiment_names = NULL,
                             output_dir = NULL,
                             output_formats = NULL,
                             subtotal_level = FALSE,
                             select_var = NULL) {
  macro_vars <- macro_info$Variable

  macro.raw <- setNames(
    lapply(experiment_names, function(scenario) {
      sl4_path <- file.path(input_dir, paste0(scenario, ".sl4"))
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
    experiment_names
  )

  macro.raw <- macro.raw[!sapply(macro.raw, is.null)]

  GTAPMacros <- do.call(
    HARplus::get_data_by_var,
    c(
      list(
        experiment_names = names(macro.raw),
        subtotal_level = subtotal_level,
        merge_data = TRUE
      ),
      macro.raw
    )
  )

  GTAPMacros <- add_mapping_info(GTAPMacros, mapping = "GTAPv7")

  GTAPMacros_filtered <- .apply_to_dataframes(GTAPMacros, function(df) {
    df[, c("Variable", "Value", "Subtotal", "Experiment", "Description", "Unit"), drop = FALSE]
  })

  GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
  rownames(GTAPMacros_final) <- NULL

  # Apply filtering by Variable if select_var is provided
  if (!is.null(select_var)) {
    GTAPMacros_final <- GTAPMacros_final[GTAPMacros_final$Variable %in% select_var, ]
  }

  GTAPMacros_final <- dplyr::arrange(GTAPMacros_final, Experiment, Variable, Unit)

  if (!is.null(output_dir) && !is.null(output_formats)) {
    export_formats <- .output_format(output_formats)
    if (length(export_formats) > 0) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      macro_list <- list(Macros = GTAPMacros_final)
      message("Exporting macro data...")
      HARplus::export_data(
        data = macro_list,
        output_path = output_dir,
        format = export_formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = TRUE
      )
      message("Macro data exported to: ", output_dir)
    }
  }

  return(GTAPMacros_final)
}

#' @title Process GTAP Data Automation with Flexible Output Options
#' @description Processes GTAP data from SL4 and HAR files with options for exporting and preparing plot-ready data.
#'
#' @details
#' This function provides a complete automation workflow for processing GTAP model outputs,
#' with flexible output options and optional filtering for plot data.
#'
#' The key parameters `sl4var` and `harvar` each accept three different input types:
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
#'   \item \strong{GTAPv7}: Uses standard GTAPv7 definitions (default)
#'   \item \strong{Yes}: Uses only the supplied descriptions and units from sl4map/harmap
#'   \item \strong{No}: Does not add any descriptions or units
#'   \item \strong{Mix}: Prioritizes supplied descriptions and units, falling back to GTAPv7
#'         for any missing values
#' }
#'
#' @param experiment Character vector. Case names to process.
#' @param mapping_info Character. Mapping mode: "GTAPv7" (default), "Yes", "No", or "Mix".
#' @param project_dir Character. Path to the project folder with "in" and "out" subfolders.
#' @param input_dir Character. Path to the input folder. Overrides project_dir/in if specified.
#' @param output_dir Character. Path to the output folder. Overrides project_dir/out if specified.
#' @param output_formats Character vector or list. Exports data in these formats (valid: "csv", "stata", "rds", "txt").
#' @param plot_data Logical. If TRUE, prepares data for plotting and assigns to variables.
#' @param region_select Optional vector. Specifies regions to filter the data.
#' @param sector_select Optional vector. Specifies sectors to filter the data.
#' @param subtotal Logical. If TRUE, includes subtotal data. Default is FALSE.
#' @param sl4_list_name Character. Variable name for SL4 plotting data if generating plot data. Default is "sl4.plot.data".
#' @param har_list_name Character. Variable name for HAR plotting data if generating plot data. Default is "har.plot.data".
#' @param sl4_structure_name Character. Variable name for SL4 structure if generating plot data. Default is "sl4.structure".
#' @param har_structure_name Character. Variable name for HAR structure if generating plot data. Default is "har.structure".
#' @param GTAPMacro_name Character. Variable name for GTAP macro data if generating plot data. Default is "GTAPMacro".
#' @param sl4var Data frame, NULL, or FALSE. Variables to extract from SL4 files. Set to NULL to extract all variables, or FALSE to skip SL4 processing.
#' @param harvar Data frame, NULL, or FALSE. Variables to extract from HAR files. Set to NULL to extract all variables, or FALSE to skip HAR processing.
#' @param sl4map Data frame or NULL. Mapping information for SL4 variables (with "Variable", "Description", and "Unit" columns).
#' @param harmap Data frame or NULL. Mapping information for HAR variables (with "Variable", "Description", and "Unit" columns).
#'
#' @return Returns the processed data invisibly, which will not be printed to the console.
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{gtap_macros_data}}
#'
#' @examples
#' \dontrun{
#' # Extract data with region and experiment filters
#' process_gtap_data(
#'   sl4var = NULL,
#'   harvar = NULL,
#'   sl4map = sl4map,
#'   harmap = harmap,
#'   region_select = selected_regions,
#'   sector_select = NULL,
#'   subtotal = FALSE,
#'   experiment = experiment,
#'   mapping_info = info.mode,
#'   project_dir = project.folder,
#'   plot_data = TRUE,
#'   output_formats = list(
#'     "csv" = "No",
#'     "stata" = "No",
#'     "rds" = "No",
#'     "txt" = "No"))
#' }
#'
process_gtap_data <- function(experiment, mapping_info = "GTAPv7",
                              project_dir = NULL, input_dir = NULL, output_dir = NULL,
                              output_formats = NULL, plot_data = FALSE, region_select = NULL,
                              sector_select = NULL, subtotal = FALSE, sl4_list_name = "sl4.plot.data",
                              har_list_name = "har.plot.data", sl4_structure_name = "sl4.structure",
                              har_structure_name = "har.structure", GTAPMacro_name = "GTAPMacro",
                              sl4var = NULL, harvar = NULL, sl4map = NULL, harmap = NULL) {

  # Setup directories and determine export settings
  export_formats <- .output_format(output_formats)
  export_data <- length(export_formats) > 0

  if (is.null(input_dir) && !is.null(project_dir)) {
    input_dir <- file.path(project_dir, "in")
  }

  if (is.null(output_dir) && !is.null(project_dir)) {
    output_dir <- file.path(project_dir, "out")
  }

  if (export_data) {
    if (length(export_formats) > 0) {
      message("Data will be exported in formats: ", paste(export_formats, collapse = ", "))
      message("Output directory: ", output_dir)
    } else {
      warning("No valid output formats specified. Data will be processed but not exported.")
      export_data <- FALSE
    }
  }

  # Initialize return data container
  all_data <- list()

  # Extract variable names for checking purposes only (not for loading data)
  if (is.data.frame(sl4var) && "Variable" %in% names(sl4var)) {
    sl4var_vars <- sl4var$Variable
  } else {
    sl4var_vars <- sl4var
  }

  if (is.data.frame(harvar) && "Variable" %in% names(harvar)) {
    harvar_vars <- harvar$Variable
  } else {
    harvar_vars <- harvar
  }

  # Determine what to process
  process_sl4 <- !identical(sl4var, FALSE)
  process_har <- !identical(harvar, FALSE)
  process_macro <- FALSE

  if (process_sl4) {
    if (is.null(sl4var)) {
      process_macro <- TRUE
    } else if (is.character(sl4var_vars)) {
      process_macro <- any(tolower(sl4var_vars) %in% "macros")
    }
  }

  # Validate inputs and files
  validation_result <- .validate_gtap_files(
    input_dir = input_dir,
    output_dir = output_dir,
    experiment = experiment,
    mapping_info = mapping_info,
    sl4file = sl4var,
    harfile = harvar,
    output_formats = if(export_data) output_formats else NULL,
    plot_data = plot_data
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  if (validation_result$status != "ok") {
    message("Proceeding with available files as per user confirmation...")
  }

  # List files in input directory
  files <- list.files(input_dir, full.names = FALSE, ignore.case = TRUE)
  sl4_files <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
  har_files <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]

  sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_files, ignore.case = TRUE)))
  har_bases <- tolower(trimws(sub("-wel\\.har$", "", har_files, ignore.case = TRUE)))

  valid_sl4_cases <- experiment[tolower(experiment) %in% sl4_bases]
  valid_har_cases <- experiment[tolower(experiment) %in% har_bases]

  #===================================================================
  # PART 1: PROCESS MACRO DATA (if needed)
  #===================================================================
  if (process_macro && length(valid_sl4_cases) > 0) {
    message("Processing macro data...")

    # Get paths to all valid SL4 files
    macro_data <- gtap_macros_data(input_dir, experiment_names = valid_sl4_cases, subtotal_level = FALSE)

    # Apply rename_GTAP_bilateral to macro data
    macro_data <- rename_GTAP_bilateral(macro_data)

    # Store in results
    all_data$GTAPMacros <- macro_data

    # If plot_data is TRUE, assign macro data to the specified variable name in parent environment
    if (plot_data && !is.null(GTAPMacro_name)) {
      assign(GTAPMacro_name, macro_data, envir = parent.frame())
    }

    # Export if needed
    if (export_data) {
      # Export macro data separately
      macro_export <- list(GTAPMacros = macro_data)
      HARplus::export_data(
        data = macro_export,
        output_path = output_dir,
        format = export_formats,
        create_subfolder = TRUE,
        multi_sheet_xlsx = TRUE,
        report_output = TRUE
      )
      message("Macro data exported to: ", output_dir)
    }
  }


  #===================================================================
  # PART 2: PROCESS REGULAR SL4 DATA (if needed)
  #===================================================================
  if (process_sl4 && length(valid_sl4_cases) > 0) {
    message("Processing regular SL4 data...")

    # Get variables to extract (if processing macros only, we can skip this part)
    process_regular_sl4 <- TRUE
    if (process_macro) {
      if (!is.null(sl4var_vars) && is.character(sl4var_vars)) {
        if (length(sl4var_vars) == 1 && tolower(sl4var_vars) == "macros") {
          process_regular_sl4 <- FALSE
        }
      }
    }

    if (process_regular_sl4) {
      # Load SL4 data
      sl4.data.raw <- setNames(
        lapply(valid_sl4_cases, function(scenario) {
          sl4_path <- file.path(input_dir, paste0(scenario, ".sl4"))
          if (file.exists(sl4_path)) {
            tryCatch({
              # Determine what to pass to load_sl4x based on sl4var type
              select_vars <- if (is.data.frame(sl4var) && "Variable" %in% names(sl4var)) {
                sl4var$Variable
              } else {
                sl4var
              }
              HARplus::load_sl4x(sl4_path, select_header = select_vars)
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

      # Remove NULL entries
      sl4.data.raw <- sl4.data.raw[!sapply(sl4.data.raw, is.null)]

      if (length(sl4.data.raw) > 0) {
        # Create structure if plot_data is TRUE
        if (plot_data) {
          sl4structure <- do.call(
            HARplus::compare_var_structure,
            c(list(NULL, keep_unique = TRUE), sl4.data.raw)
          )[["match"]]

          if (!is.null(sl4map) && is.data.frame(sl4map)) {
            sl4structure_df <- dplyr::left_join(
              sl4map, sl4structure[c("Variable", "Dimensions")], by = "Variable"
            )
            sl4structure_df <- sl4structure_df[order(sl4structure_df$Dimensions), ]

            if ("Description" %in% names(sl4structure_df)) {
              names(sl4structure_df)[names(sl4structure_df) == "Description"] <- "PlotTitle"
            }

            sl4structure_df$Unit <- NULL
          } else {
            sl4structure_df <- sl4structure[, c("Variable", "Dimensions")]
            sl4structure_df$PlotTitle <- sl4structure_df$Variable
          }

          all_data$sl4structure <- sl4structure_df

          if (!is.null(sl4_structure_name)) {
            assign(sl4_structure_name, sl4structure_df, envir = parent.frame())
          }
        }

        # Group data by dimensions
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
                subtotal_level = subtotal
              ),
              sl4.data.raw
            )
          )
        }, error = function(e) {
          message(sprintf("Error in grouping SL4 data: %s", e$message))
          return(NULL)
        })

        if (!is.null(grouped_sl4)) {
          # Apply rename_GTAP_bilateral to all dataframes in grouped_sl4
          grouped_sl4 <- rename_GTAP_bilateral(grouped_sl4)

          # Add mapping information
          grouped_sl4 <- add_mapping_info(grouped_sl4, external_map = sl4map, mapping = mapping_info,
                                          description_info = TRUE, unit_info = TRUE)

          # Apply filters if specified
          if (!is.null(region_select) || !is.null(sector_select)) {
            grouped_sl4 <- .apply_filters(
              grouped_sl4,
              region_select = region_select,
              experiment_select = experiment,
              sector_select = sector_select
            )
          }

          # Store in results
          all_data$sl4_data <- grouped_sl4

          # Assign to environment for plotting
          if (plot_data && !is.null(sl4_list_name)) {
            assign(sl4_list_name, grouped_sl4, envir = parent.frame())
          }

          # Check for bilateral trade data (QXS)
          has_qxs <- FALSE
          if (is.null(sl4var)) {
            has_qxs <- TRUE
          } else if (is.character(sl4var_vars)) {
            has_qxs <- any(grepl("qxs", sl4var_vars, ignore.case = TRUE))
          } else if (is.data.frame(sl4map) && "Variable" %in% names(sl4map)) {
            has_qxs <- any(grepl("qxs", sl4map$Variable, ignore.case = TRUE))
          }

          if (has_qxs) {
            bilateral_data <- .process_bilateral_trade(
              grouped_sl4,
              output_dir,
              export_formats,
              export = export_data
            )

            # Apply rename_GTAP_bilateral to bilateral_data
            if (!is.null(bilateral_data)) {
              bilateral_data <- rename_GTAP_bilateral(bilateral_data)
              all_data$bilateral_data <- bilateral_data
            }
          }

          # Export if requested
          if (export_data) {
            message("Exporting grouped SL4 data...")
            HARplus::export_data(
              data = grouped_sl4,
              output_path = output_dir,
              format = export_formats,
              create_subfolder = TRUE,
              multi_sheet_xlsx = TRUE,
              report_output = TRUE
            )
            message("Grouped SL4 data exported to: ", output_dir)
          }
        }
      }
    }
  }

  #===================================================================
  # PART 3: PROCESS HAR DATA (if needed)
  #===================================================================
  if (process_har && length(valid_har_cases) > 0) {
    message("Processing HAR data...")

    har.data.raw <- setNames(
      lapply(valid_har_cases, function(scenario) {
        har_path <- file.path(input_dir, paste0(scenario, "-WEL.har"))
        if (file.exists(har_path)) {
          tryCatch({
            # Determine what to pass to load_harx based on harvar type
            select_vars <- if (is.data.frame(harvar) && "Variable" %in% names(harvar)) {
              harvar$Variable
            } else {
              harvar
            }
            HARplus::load_harx(har_path, select_header = select_vars)
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
      # Create structure if plot_data is TRUE
      if (plot_data) {
        harstructure <- do.call(
          HARplus::compare_var_structure,
          c(list(NULL, keep_unique = TRUE), har.data.raw)
        )[["match"]]

        if (!is.null(harmap) && is.data.frame(harmap)) {
          harstructure_df <- dplyr::left_join(
            harmap, harstructure[c("Variable", "Dimensions")], by = "Variable"
          )
          harstructure_df <- harstructure_df[order(harstructure_df$Dimensions), ]

          if ("Description" %in% names(harstructure_df)) {
            names(harstructure_df)[names(harstructure_df) == "Description"] <- "PlotTitle"
          }

          harstructure_df$Unit <- NULL
        } else {
          harstructure_df <- harstructure[, c("Variable", "Dimensions")]
          harstructure_df$PlotTitle <- harstructure_df$Variable
        }

        all_data$harstructure <- harstructure_df

        if (!is.null(har_structure_name)) {
          assign(har_structure_name, harstructure_df, envir = parent.frame())
        }
      }

      # Process HAR data
      har_data <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            experiment_names = names(har.data.raw),
            subtotal_level = subtotal,
            merge_data = TRUE
          ),
          har.data.raw
        )
      )

      # Apply rename_GTAP_bilateral to har_data
      har_data <- rename_GTAP_bilateral(har_data)

      # Add mapping information
      har_data <- add_mapping_info(har_data, external_map = harmap, mapping = mapping_info,
                                   description_info = TRUE, unit_info = TRUE)

      # Apply filters if specified
      if (!is.null(region_select) || !is.null(sector_select)) {
        har_data <- .apply_filters(
          har_data,
          region_select = region_select,
          experiment_select = experiment,
          sector_select = sector_select
        )
      }

      # Store in results
      all_data$decomposition_data <- har_data

      # Assign to environment for plotting
      if (plot_data && !is.null(har_list_name)) {
        assign(har_list_name, har_data, envir = parent.frame())
      }

      # Export if requested
      if (export_data) {
        decomposition_data <- list(Decomposition = har_data)
        message("Exporting decomposition data...")
        HARplus::export_data(
          data = decomposition_data,
          output_path = output_dir,
          format = export_formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = TRUE
        )
        message("Decomposition data exported to: ", output_dir)
      }
    }
  }

  #===================================================================
  # CONSOLIDATE EXCEL REPORTS (only once at the end)
  #===================================================================
  if (export_data && file.exists(output_dir)) {
    message("Consolidating reports...")
    .consolidate_reports(
      output_folder = output_dir,
      final_name = "Report_Output.xlsx",
      remove_sources = TRUE
    )
    message("Reports consolidated into: ", file.path(output_dir, "Report_Output.xlsx"))
  }

  message("GTAP data processing completed!")

  return(invisible(all_data))
}
