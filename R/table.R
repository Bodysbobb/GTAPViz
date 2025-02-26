#' @title Create Report Table
#'
#' @description Generates a structured report table from a given dataset, allowing for flexible
#' formatting and customization of column names. The function reshapes the data to present
#' variables in a wide format with experimental groups as rows.
#'
#' @param data A data frame or list containing the dataset. If a list, it is converted into a
#'        data frame before processing.
#' @param vars A character vector specifying the variables to include in the report.
#' @param x_axis_from A character string specifying the column used for grouping (e.g., `"REG"`, `"COMM"`).
#' @param output_dir Optional character string specifying the directory where the report
#'        will be saved as an Excel file. If `NULL`, the table is not exported.
#' @param workbook_name Optional character string for the Excel workbook name. Default is `"Report_table"`.
#' @param sheet_name Optional character string for the worksheet name. Default is `"Results"`.
#' @param col_names Optional data frame or character vector for renaming columns. If a data
#'        frame, it must contain `"Variable"` and `"PlotTitle"` columns to map variable names
#'        to custom titles. If a character vector, it must match the length of `vars`.
#'
#' @details
#' - The function checks for the required columns `"Experiment"`, `"Variable"`, `"Value"`,
#'   and the specified `x_axis_from` column.
#' - The table is formatted to ensure numerical values are rounded to two decimal places.
#' - If `output_dir` is provided, the table is exported as an Excel file with enhanced formatting.
#' - If column renaming (`col_names`) is specified, it is applied to improve readability.
#'
#' @return A formatted data frame in wide format with grouped variables as columns and
#' experimental groups as rows. If `output_dir` is provided, the function also saves
#' the output to an Excel file.
#'
#' @export
#' @author Pattawee Puangchit
#' @seealso \code{\link{scalar_table}}, \code{\link{decomp_table}}
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' data <- data.frame(
#'   Experiment = rep(c("Baseline", "Policy"), each = 3),
#'   REG = rep(c("USA", "CHN", "EU"), 2),
#'   Variable = rep(c("GDP", "Consumption", "Investment"), 2),
#'   Value = c(100, 50, 30, 105, 55, 35)
#' )
#'
#' # Generate report table
#' report <- report_table(data, vars = c("GDP", "Consumption"),
#'                        x_axis_from = "REG", output_dir = "results")
#'
#' # Custom column names
#' col_names_df <- data.frame(
#'   Variable = c("GDP", "Consumption"),
#'   PlotTitle = c("Gross Domestic Product", "Household Consumption")
#' )
#'
#' report_table(data, vars = c("GDP", "Consumption"), x_axis_from = "REG",
#'              output_dir = "results", col_names = col_names_df)
#' }
#'
report_table <- function(data, vars, x_axis_from, output_dir = NULL,
                         workbook_name = NULL, sheet_name = "Results",
                         col_names = NULL) {
  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }

  if (is.null(workbook_name)) {
    workbook_name <- "Report_table"
  }

  if (is.list(data) && !is.data.frame(data)) {
    # Check which variables exist in the data and filter vars
    existing_vars <- intersect(vars, names(data))
    missing_vars <- setdiff(vars, names(data))

    if (length(existing_vars) == 0) {
      warning("None of the specified variables found in data. Available variables: ",
              paste(names(data), collapse = ", "))
      return(NULL)
    }

    if (length(missing_vars) > 0) {
      warning("Variables not found in data and will be skipped: ",
              paste(missing_vars, collapse = ", "))
    }

    # Use only variables that exist in the data
    data <- data[existing_vars]
    data <- do.call(rbind, data)
    vars <- existing_vars
  } else if (is.data.frame(data)) {
    # For data frame input, check if specified variables exist in Variable column
    if ("Variable" %in% names(data)) {
      existing_vars <- intersect(vars, unique(data$Variable))
      missing_vars <- setdiff(vars, existing_vars)

      if (length(existing_vars) == 0) {
        warning("None of the specified variables found in data. Available variables: ",
                paste(unique(data$Variable), collapse = ", "))
        return(NULL)
      }

      if (length(missing_vars) > 0) {
        warning("Variables not found in data and will be skipped: ",
                paste(missing_vars, collapse = ", "))
      }

      # Filter data to include only existing variables
      data <- data[data$Variable %in% existing_vars, ]
      vars <- existing_vars  # Update vars to only include existing variables
    }
  }

  required_cols <- c("Experiment", "Variable", "Value", x_axis_from)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))
  }

  # Create a base result table
  result <- data %>%
    dplyr::select(Experiment, dplyr::all_of(x_axis_from), Variable, Value) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value) %>%
    dplyr::rename(Group = dplyr::all_of(x_axis_from), Experiment = Experiment) %>%
    dplyr::arrange(Experiment, Group) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 2)))

  # Handle column names with unit information
  if (!is.null(col_names)) {
    # Check if col_names is a data frame
    if (is.data.frame(col_names)) {
      if (!all(c("Variable", "PlotTitle") %in% names(col_names))) {
        stop("col_names data frame must contain both 'Variable' and 'PlotTitle' columns")
      }

      # Filter col_names to include only the variables that exist in the data
      col_names <- col_names[col_names$Variable %in% vars, ]
      name_mapping <- setNames(col_names$PlotTitle, col_names$Variable)
    } else {
      # Assume it's a character vector
      if (length(col_names) != length(vars)) {
        warning("Length of col_names does not match length of existing vars, using available mappings")
        name_mapping <- setNames(col_names[1:min(length(col_names), length(vars))], vars[1:min(length(col_names), length(vars))])
      } else {
        name_mapping <- setNames(col_names, vars)
      }
    }

    # Get unit information
    if ("Unit" %in% names(data)) {
      unit_info <- data %>%
        dplyr::filter(Variable %in% vars) %>%
        dplyr::select(Variable, Unit) %>%
        dplyr::distinct()

      # Create new column names with units
      new_col_names <- sapply(vars, function(var) {
        unit <- unit_info$Unit[unit_info$Variable == var][1]
        custom_name <- name_mapping[var]

        if (!is.na(unit)) {
          if (tolower(unit) == "percent") {
            paste0(custom_name, " (%)")
          } else {
            paste0(custom_name, " (", unit, ")")
          }
        } else {
          custom_name
        }
      })

      # Rename columns
      old_names <- vars
      cols_to_rename <- intersect(old_names, names(result))
      if (length(cols_to_rename) > 0) {
        names(result)[match(cols_to_rename, names(result))] <- new_col_names[match(cols_to_rename, old_names)]
      }
    } else {
      # No unit information, just use custom names
      old_names <- vars
      cols_to_rename <- intersect(old_names, names(result))
      if (length(cols_to_rename) > 0) {
        names(result)[match(cols_to_rename, names(result))] <- name_mapping[cols_to_rename]
      }
    }
  } else {
    # Use default variable names with unit information
    if ("Unit" %in% names(data)) {
      unit_info <- data %>%
        dplyr::filter(Variable %in% vars) %>%
        dplyr::select(Variable, Unit) %>%
        dplyr::distinct()

      new_col_names <- sapply(vars, function(var) {
        unit <- unit_info$Unit[unit_info$Variable == var][1]
        if (!is.na(unit)) {
          if (tolower(unit) == "percent") {
            paste0(var, " (%)")
          } else {
            paste0(var, " (", unit, ")")
          }
        } else {
          var
        }
      })

      old_names <- vars
      cols_to_rename <- intersect(old_names, names(result))
      if (length(cols_to_rename) > 0) {
        names(result)[match(cols_to_rename, names(result))] <- new_col_names[match(cols_to_rename, old_names)]
      }
    }
  }

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

  return(invisible(result))
}


#' @title Create Scalar Result Table
#'
#' @description Generates a formatted table summarizing scalar results from a given dataset.
#' The function transforms long-format data into a wide format with scenarios as columns,
#' making comparisons across experiments more accessible.
#'
#' @param data A data frame or list containing scalar data, where each observation consists of
#'        an experiment name, variable name, and corresponding value.
#' @param vars Optional character vector of variable names to include in the table.
#'        If `NULL`, all available variables in the dataset will be used.
#' @param output_dir Optional character. Directory where the generated table will be saved
#'        as an Excel file. If `NULL`, no file will be created.
#' @param workbook_name Optional character. Name of the output Excel workbook. Default is `"scalar_results"`.
#' @param sheet_name Optional character. Name of the worksheet in the Excel file. Default is `"Results"`.
#'
#' @details
#' - If `data` is a list and not a data frame, the function attempts to combine it into a data frame.
#' - The table is formatted to ensure numerical values are rounded to two decimal places.
#' - If `output_dir` is specified, the table is exported as an Excel file with proper formatting.
#' - Required columns in `data` are `"Experiment"`, `"Variable"`, and `"Value"`.
#'
#' @return A formatted data frame in wide format with variables as rows and experiments as columns.
#' If `output_dir` is provided, the function also saves the output to an Excel file.
#'
#' @export
#' @author Pattawee Puangchit
#' @seealso \code{\link{report_table}}, \code{\link{decomp_table}}
#'
#' @examples
#' \dontrun{
#' sl4_data1 <- HARplus::get_data_by_var(c("globalcgds", "vgdpwld", "WEV"), sl4_data)
#' scalar_table <- scalar_table(
#'   data = sl4_data1,
#'   vars = "vgdpwld",
#'   output_dir = temp_dir,
#'   workbook_name = "global_indicators",
#'   sheet_name = "Results"
#' )
#' }
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

  return(invisible(result))
}


#' @title Create Decomposition Table with Multi-Data Support
#'
#' @description Processes decomposition data, allowing multiple data frames as input.
#' Converts specified columns into wide format per dataset while supporting multiple
#' headers, independent column selections, and optional total calculations.
#'
#' @param data A data frame or a named list of data frames, each containing decomposition data.
#' @param header Optional. A character vector specifying headers to filter each dataset.
#'        Required if `data` is a list.
#' @param wide_cols A named list specifying the column(s) to be converted into wide format for each dataset.
#'        Example: `list(A = "COLUMN", E1 = "PRICES")`.
#' @param total_column Logical. If `TRUE`, calculates a total column for each dataset. Default is `FALSE`.
#' @param export_table Logical. If `TRUE`, exports the result to an Excel file. Default is `FALSE`.
#' @param multi_sheet Logical. If `TRUE`, exports each dataset to a separate sheet in the Excel file. Default is `FALSE`.
#' @param output_dir Optional character. Directory to save the output file if `export_table = TRUE`.
#' @param workbook_name Optional character. Name of the output Excel workbook if `export_table = TRUE`. Default is `"decomp_results.xlsx"`.
#'
#' @details
#' - If `data` is a single data frame, `header` is ignored, and `wide_cols` should be a single column name.
#' - If `data` is a list of data frames, `header` must be specified to select relevant subsets of each dataset.
#' - `wide_cols` must be a named list matching the names of `data` (e.g., `"A"`, `"E1"`).
#' - When `total_column = TRUE`, numerical values in each row are summed into a "Total" column.
#' - If `export_table = TRUE`, the function saves the result to an Excel file, either as a single sheet or multiple sheets (if `multi_sheet = TRUE`).
#'
#' @return A list of data frames, each containing wide-format decomposition data.
#' If `export_table = TRUE`, the results are also saved to an Excel file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example list of data frames
#' har.plot.data <- list(
#'   A = data.frame(Experiment = c("Base", "Policy"),
#'                  Header = "A", COLUMN = c("Tariff", "NTM"), Value = c(10, 5)),
#'   E1 = data.frame(Experiment = c("Base", "Policy"),
#'                   Header = "E1", PRICES = c("GDP", "Consumption"), Value = c(105, 55))
#' )
#'
#' # Convert decomposition tables for both datasets
#' result <- decomp_table(data = har.plot.data, header = c("A", "E1"),
#'                        wide_cols = list(A = "COLUMN", E1 = "PRICES"), total_column = TRUE)
#'
#' # Export as an Excel file with multiple sheets
#' decomp_table(data = har.plot.data, header = c("A", "E1"),
#'              wide_cols = list(A = "COLUMN", E1 = "PRICES"),
#'              total_column = TRUE, export_table = TRUE, multi_sheet = TRUE,
#'              output_dir = "results", workbook_name = "decomposition_summary.xlsx")
#' }
decomp_table <- function(data_list, header, wide_cols, total_column = FALSE,
                         export_table = FALSE, multi_sheet = FALSE, output_dir = NULL) {

  if (!is.list(data_list)) stop("Data must be a list of dataframes.")
  if (!all(header %in% names(data_list))) stop("Some headers do not match dataset names.")
  if (!all(header %in% names(wide_cols))) stop("Missing 'wide_cols' mapping for some datasets.")

  process_df <- function(df, wide_col) {
    if (!"Experiment" %in% names(df)) stop("Data must contain an 'Experiment' column")
    if (!wide_col %in% names(df)) stop(sprintf("Column '%s' not found in dataset", wide_col))

    keep_cols <- setdiff(names(df), c(wide_col, "Value"))
    wide_data <- reshape(df, direction = "wide", timevar = wide_col, idvar = keep_cols, v.names = "Value")
    names(wide_data) <- sub("^Value\\.", "", names(wide_data))

    if (total_column) {
      numeric_cols <- sapply(wide_data, is.numeric)
      if (any(numeric_cols)) {
        wide_data$Total <- rowSums(wide_data[, numeric_cols, drop = FALSE], na.rm = TRUE)
      }
    }

    return(wide_data)
  }

  result_list <- list()
  for (hdr in header) {
    df <- data_list[[hdr]]
    result_list[[hdr]] <- process_df(df, wide_cols[[hdr]])
  }

  if (export_table) {
    if (is.null(output_dir)) stop("Output directory must be specified for exporting.")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    file_path <- file.path(output_dir, "decomp_results.xlsx")
    wb <- openxlsx::createWorkbook()

    for (hdr in names(result_list)) {
      openxlsx::addWorksheet(wb, hdr)
      openxlsx::writeData(wb, hdr, result_list[[hdr]])
    }

    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
    message("Table exported to: ", file_path)
  }

  return(invisible(result_list))
}

