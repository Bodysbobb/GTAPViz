#' @title Add Mapping Information
#'
#' @description The description and unit are based on GTAPv7 model variables.
#' Full list refer to https://www.gtap.agecon.purdue.edu/models/setsvariables.asp.
#' If any variable is added apart from the default GTAPv7 model, the information will be left blank.
#'
#' @param data_list A list or data frame containing GTAP variables.
#' @param external_map **Optional**. A data frame containing external mapping information with columns `"Variable"`, `"Description"`, and `"Unit"`. If `NULL`, the default GTAPv7 mapping is used.
#' @param mapping **Optional**. Either `"GTAPv7"`, `"No"`, `"Yes"`, or `"Mix"`. Determines the mapping approach. Default is `"GTAPv7"`.
#' @param description_info **Optional**. Logical. If `TRUE`, adds description information. Default is `TRUE`.
#' @param unit_info **Optional**. Logical. If `TRUE`, adds unit information. Default is `TRUE`.
#'
#' @return A modified list or data frame with added description and/or unit information.
#'
#' @author Pattawee Puangchit
#'
#' @export
#'
#' @examples
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' result <- add_mapping_info(sl4_data1, mapping = "GTAPv7", unit_info = FALSE) # Add only Description
#' result <- add_mapping_info(sl4_data1, mapping = "GTAPv7", description_info = FALSE) # Add only Unit
#' custom_mapping <- data.frame(Variable = c("qgdp", "EV"),
#'                              Description = c("Real GDP", "Economic Value"),
#'                              Unit = c("Percent", "million USD"))
#' result <- add_mapping_info(sl4_data1, external_map = custom_mapping, mapping = "Mix") # Custom + GTAPv7
#'
add_mapping_info <- function(data_list, external_map = NULL, mapping = "GTAPv7",
                             description_info = TRUE, unit_info = TRUE) {
  if (!is.null(mapping)) {
    mapping <- toupper(mapping)
    if (!mapping %in% c("GTAPV7", "NO", "YES", "MIX")) {
      stop("mapping must be one of: 'GTAPv7', 'No', 'Yes', or 'Mix' (case-insensitive)")
    }
  }

  if (identical(mapping, "NO")) return(data_list)

  process_dataframe <- function(df, mapping_df, variables_to_map = NULL) {
    if (!is.data.frame(df) || !"Variable" %in% names(df)) return(df)

    var_col <- if ("GTAPVariable" %in% names(df)) "GTAPVariable" else "Variable"

    vars_to_process <- if (!is.null(variables_to_map)) {
      df[[var_col]] %in% variables_to_map
    } else {
      rep(TRUE, nrow(df))
    }

    if (description_info && "Description" %in% names(mapping_df)) {
      df$Description[vars_to_process] <- mapping_df$Description[match(df[[var_col]][vars_to_process],
                                                                      mapping_df$Variable)]
    }
    if (unit_info && "Unit" %in% names(mapping_df)) {
      df$Unit[vars_to_process] <- mapping_df$Unit[match(df[[var_col]][vars_to_process],
                                                        mapping_df$Variable)]
    }

    return(df)
  }

  process_list <- function(x, mapping_df, variables_to_map = NULL) {
    if (is.data.frame(x)) {
      return(process_dataframe(x, mapping_df, variables_to_map))
    }
    if (is.list(x)) {
      result <- lapply(x, function(y) process_list(y, mapping_df, variables_to_map))
      attributes(result) <- attributes(x)
      return(result)
    }
    return(x)
  }

  if (mapping == "MIX") {
    if (is.null(external_map)) stop("external_map must be provided for 'Mix' mode")

    vars_with_empty_desc <- if (description_info) {
      external_map$Variable[is.na(external_map$Description) | external_map$Description == ""]
    } else character(0)

    vars_with_empty_unit <- if (unit_info) {
      external_map$Variable[is.na(external_map$Unit) | external_map$Unit == ""]
    } else character(0)

    result <- process_list(data_list, external_map)

    if (length(vars_with_empty_desc) > 0 || length(vars_with_empty_unit) > 0) {
      result <- process_list(result, default_info,
                             unique(c(vars_with_empty_desc, vars_with_empty_unit)))
    }

  } else if (mapping == "GTAPV7") {
    result <- process_list(data_list, default_info)
  } else {
    result <- process_list(data_list, external_map)
  }

  class(result) <- class(data_list)
  return(result)
}


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
    sl4_data <- load_sl4x(file_paths[[i]])
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


#' @title Create Result Table from Variable Data
#' @description Creates a formatted hierarchical table from selected variables with proper cell merging
#' and Experiment grouping structure.
#'
#' @param data A data frame or list containing the data (output from get_data_by_var)
#' @param vars Character vector of variable names to include in the table
#' @param x_axis_from Character. Column name to use for grouping (e.g., "REG", "COMM")
#' @param output_dir Optional character. Directory to save the Excel file.
#'        If NULL, a temporary directory is used.
#' @param workbook_name Character. Name for the Excel workbook. Default is "result_table"
#' @param sheet_name Character. Name for the worksheet. Default is "Results"
#'
#' @return A data frame in the hierarchical format
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{scalar_table}}
#' @export
#'
#' @examples
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' temp_dir <- tempdir()
#' result_table <- report_table(
#'   data = sl4_data1,
#'   vars = c("qgdp", "pgdp", "vgdp"),
#'   x_axis_from = "REG",
#'   output_dir = temp_dir,
#'   workbook_name = "macro_indicators",
#'   sheet_name = "Regional Results"
#' )
#' print(result_table)
#'
report_table <- function(data, vars, x_axis_from, output_dir = NULL,
                                workbook_name = "result_table", sheet_name = "Results") {
  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }

  if (is.list(data) && !is.data.frame(data)) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
    }
    data <- data[vars]
    data <- do.call(rbind, data)
  }

  required_cols <- c("Experiment", "Variable", "Value", x_axis_from)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))
  }

  result <- data %>%
    dplyr::select(Experiment, dplyr::all_of(x_axis_from), Variable, Value) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value) %>%
    dplyr::rename(Group = dplyr::all_of(x_axis_from), Experiment = Experiment) %>%
    dplyr::arrange(Experiment, Group) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 2)))

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet_name)

    header_style <- openxlsx::createStyle(textDecoration = "bold", border = "TopBottom", borderStyle = "medium")
    number_style <- openxlsx::createStyle(numFmt = "0.00", halign = "right")

    openxlsx::writeData(wb, sheet_name, result, headerStyle = header_style)

    numeric_cols <- which(sapply(result, is.numeric))
    for (col in numeric_cols) {
      openxlsx::addStyle(wb, sheet_name, number_style, rows = 2:(nrow(result) + 1), cols = col)
    }

    Experiment_groups <- rle(as.character(result$Experiment))
    current_row <- 2
    for (i in seq_along(Experiment_groups$lengths)) {
      if (Experiment_groups$lengths[i] > 1) {
        openxlsx::mergeCells(wb, sheet_name, rows = current_row:(current_row + Experiment_groups$lengths[i] - 1), cols = 1)
      }
      current_row <- current_row + Experiment_groups$lengths[i]
    }

    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(result), widths = c(15, 12, rep(10, ncol(result)-2)))

    filepath <- file.path(output_dir, paste0(workbook_name, ".xlsx"))
    openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
    message("Table exported to: ", filepath)
  }

  return(result)
}


#' @title Create Scalar Result Table
#' @description Creates a wide-format table for scalar/2D data with scenarios as columns.
#' Specifically designed for variables that have single values per scenario (no regional dimension).
#'
#' @param data A data frame or list containing the data (output from get_data_by_var)
#' @param vars Character vector of variable names to include
#' @param output_dir Optional character. Directory to save the Excel file
#' @param workbook_name Character. Name for the Excel workbook. Default is "scalar_results"
#' @param sheet_name Character. Name for the worksheet. Default is "Results"
#'
#' @return A data frame in wide format with variables as rows and scenarios as columns
#' @export
#' @author Pattawee Puangchit
#' @seealso \code{\link{report_table}}
#'
#' @examples
#' \dontrun{
#' scalar_data <- HARplus::get_data_by_var(c("globalcgds", "vgdpwld", "WEV"), sl4_data)
#' scalar_table <- scalar_table(
#'   data = sl4_data1,
#'   vars = NULL,
#'   output_dir = temp_dir,
#'   workbook_name = "global_indicators",
#'   sheet_name = "Results"
#' )
#' print(scalar_table)
#'
scalar_table <- function(data, vars = NULL, output_dir = NULL,
                         workbook_name = "scalar_results",
                         sheet_name = "Results") {
  if (is.list(data) && !is.data.frame(data)) {
    if (is.null(vars)) {
      vars <- names(data)
    } else {
      missing_vars <- setdiff(vars, names(data))
      if (length(missing_vars) > 0) {
        stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
      }
    }
    data <- data[vars]
    data <- do.call(rbind, data)
  }

  required_cols <- c("Experiment", "Variable", "Value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))
  }

  result <- data %>%
    dplyr::select(Variable, Experiment, Value) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    tidyr::pivot_wider(
      names_from = Experiment,
      values_from = Value
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 2)))

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheet_name)

    header_style <- openxlsx::createStyle(
      textDecoration = "bold",
      border = "TopBottom",
      borderStyle = "medium"
    )

    number_style <- openxlsx::createStyle(
      numFmt = "0.00",
      halign = "right"
    )

    openxlsx::writeData(wb, sheet_name, result, headerStyle = header_style)

    numeric_cols <- which(sapply(result, is.numeric))
    for (col in numeric_cols) {
      openxlsx::addStyle(wb, sheet_name, number_style,
                         rows = 2:(nrow(result) + 1),
                         cols = col)
    }

    openxlsx::setColWidths(wb, sheet_name,
                           cols = 1:ncol(result),
                           widths = c(15, rep(12, ncol(result)-1)))

    filepath <- file.path(output_dir, paste0(workbook_name, ".xlsx"))
    openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
    message("Table exported to: ", filepath)
  }

  return(result)
}


#' @title Rename GTAP Bilateral Trade Columns
#' @description Renames bilateral trade columns in GTAP data output, specifically targeting REG columns
#' to Source and Destination in data frames with COMM*REG*REG dimension structure.
#'
#' @param data List or data frame. Output from get_data_by_var, get_data_by_dims, or group_data_by_dims.
#' @return Data with renamed columns, maintaining the original structure type.
#' @author Your Name
#' @export
#'
#' @examples
#' # For direct data frame output
#' df <- get_data_by_var("qxs", sl4_data)
#' df_renamed <- rename_GTAP_bilateral(df)
#'
#' # For list output with multiple variables
#' data_list <- get_data_by_var(c("qxs", "viws"), sl4_data)
#' renamed_list <- rename_GTAP_bilateral(data_list)
#'
#' # For grouped dimension output
#' grouped_data <- group_data_by_dims("COMM*REG*REG", sl4_data)
#' renamed_grouped <- rename_GTAP_bilateral(grouped_data)
#'
rename_GTAP_bilateral <- function(data) {
  rename_cols_in_df <- function(df) {
    if (!is.data.frame(df)) return(df)

    reg_cols <- grep("^REG", names(df))
    if (length(reg_cols) >= 2) {
      orig_names <- names(df)

      first_reg <- reg_cols[1]
      second_reg <- reg_cols[2]

      new_names <- orig_names
      new_names[first_reg] <- "Source"
      new_names[second_reg] <- "Destination"

      names(df) <- new_names
    }
    return(df)
  }

  if (is.data.frame(data)) {
    return(rename_cols_in_df(data))

  } else if (is.list(data)) {
    if ("data" %in% names(data) && "dimension_info" %in% names(data)) {
      data$data <- lapply(data$data, rename_cols_in_df)
      return(data)

    } else if (all(sapply(data, function(x)
      is.data.frame(x) || (is.list(x) && !is.null(attr(x, "class")))))) {
      return(lapply(data, function(x) {
        if (is.data.frame(x)) {
          rename_cols_in_df(x)
        } else {
          rename_GTAP_bilateral(x)
        }
      }))

    } else if ("3D" %in% names(data) || inherits(data, "grouped_dims_element")) {
      for (dim_key in names(data)) {
        if (is.list(data[[dim_key]])) {
          data[[dim_key]] <- lapply(data[[dim_key]], function(x) {
            if (is.data.frame(x)) {
              rename_cols_in_df(x)
            } else if (is.list(x)) {
              rename_GTAP_bilateral(x)
            } else {
              x
            }
          })
        }
      }
      return(data)
    }
  }

  return(data)
}


#' @title Create Decomposition Table from HAR Data
#' @description Transforms decomposition data from long to wide format with flexible column selection
#' and optional total calculation.
#'
#' @param data A data frame or list from get_data functions containing HAR file data
#' @param header Character. The header to process (e.g., "A" for welfare decomposition)
#' @param wide_cols Character vector. Column names to be converted to wide format
#' @param total_column Logical. If TRUE, adds a total column summing across numeric columns
#'
#' @return A data frame in wide format with decomposition components
#' @export
#'
#' @examples
#' # Load HAR data
#' har_data <- load_harx("path/to/har/file.har")
#'
#' # Get data using get_data_by_var
#' extracted_data <- get_data_by_var(var_names = c("A"), har_data)
#'
#' # Create decomposition table for welfare (A header)
#' # Converting COLUMN to wide format
#' decomp_table(extracted_data,
#'             header = "A",
#'             wide_cols = "COLUMN",
#'             total_column = TRUE)
#'
decomp_table <- function(data, header, wide_cols, total_column = FALSE) {
  if (missing(data) || is.null(data)) stop("Data input is required")
  if (missing(header) || is.null(header)) stop("Header specification is required")
  if (missing(wide_cols) || is.null(wide_cols)) stop("Column(s) for wide format conversion must be specified")

  process_df <- function(df) {
    if ("Header" %in% names(df)) {
      if (!(header %in% unique(df$Header))) return(NULL)
      df <- df[df$Header == header, ]
    }
    if (!"Experiment" %in% names(df)) stop("Data must contain an 'Experiment' column")
    missing_cols <- wide_cols[!wide_cols %in% names(df)]
    if (length(missing_cols) > 0) stop(sprintf("Column(s) not found in data: %s", paste(missing_cols, collapse = ", ")))
    keep_cols <- setdiff(names(df), c(wide_cols, "Value"))
    wide_data <- reshape(df, direction = "wide", timevar = wide_cols, idvar = keep_cols, v.names = "Value")
    names(wide_data) <- sub("^Value\\.", "", names(wide_data))
    all_cols <- names(wide_data)
    experiment_col <- which(all_cols == "Experiment")
    wide_data <- wide_data[, c(all_cols[experiment_col], all_cols[-experiment_col])]
    if (total_column) {
      numeric_cols <- sapply(wide_data, is.numeric)
      if (any(numeric_cols)) {
        wide_data$Total <- rowSums(wide_data[, numeric_cols, drop = FALSE], na.rm = TRUE)
      } else {
        warning("No numeric columns found for total calculation")
      }
    }
    return(wide_data)
  }

  process_element <- function(x) {
    if (is.data.frame(x)) {
      res <- process_df(x)
      return(if (!is.null(res)) res else x)
    } else if (is.list(x)) {
      return(lapply(x, process_element))
    } else {
      return(x)
    }
  }

  process_element(data)
}
