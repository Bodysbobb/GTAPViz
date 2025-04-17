#' @title Convert Long-Format Data into Wide-Format for Reporting (Internal)
#'
#' @description
#' Converts a long-format data frame into a wide-format structure, applying optional transformations
#' such as calculating totals, renaming columns, and rounding numeric values. It ensures grouping columns
#' are preserved while transforming the data into a structured table for reporting.
#'
#' @param df A data frame to be transformed.
#' @param wide_col Character. The column whose unique values will become new column headers in the wide-format table.
#' @param group_cols Character vector. Column(s) to retain as row identifiers in the wide-format structure.
#' @param rename_mapping Named list. Specifies mapping for renaming grouping columns, where names are
#' existing column names, and values are new column names.
#' @param total_column Logical. If `TRUE`, adds a "Total" column that sums all numeric columns. Default is `FALSE`.
#' @param decimal Numeric. Number of decimal places to round numeric columns. Default is `2`.
#'
#' @return A wide-format data frame with transformed column headers, optional total values,
#' renamed columns, and rounded numeric values.
#'
#' @details
#' This function preserves important grouping columns while converting a data frame into wide format.
#' It also ensures numeric columns are properly rounded and formatted, and allows renaming of specific
#' grouping variables for clearer reporting.
#'
#' - If `total_column = TRUE`, the function calculates the total of all numeric columns and
#'   adds a `"Total"` column.
#' - If `rename_mapping` is provided, the function renames matching columns.
#' - Sorting is applied to ensure proper column arrangement.
#'
#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
#'
.process_detail_data <- function(df, wide_col, group_cols,
                                 rename_mapping, total_column, decimal) {

  keep_ <- c(group_cols, wide_col, "Value")
  df <- df[, intersect(names(df), keep_), drop=FALSE]
  df$Value <- as.numeric(df$Value)
  id_cols <- setdiff(keep_, c(wide_col,"Value"))

  # Skip rows where wide_col is NA
  if (any(is.na(df[[wide_col]]))) {
    nas <- sum(is.na(df[[wide_col]]))
    warning(sprintf("Removed %d rows with NA in pivot column '%s'", nas, wide_col))
    df <- df[!is.na(df[[wide_col]]), ]
  }

  # Check for duplicates in the combination of id columns and wide column
  if (nrow(df) > 0) {
    check_ <- df[, c(id_cols, wide_col), drop=FALSE]
    dup_ <- duplicated(check_) | duplicated(check_, fromLast=TRUE)

    if (any(dup_)) {
      # Get duplicate rows to examine the specific issue
      dup_rows <- df[dup_, ]
      id_values <- paste(names(dup_rows), dup_rows[1,], sep=":", collapse=", ")

      # Count duplicates by group
      dup_keys <- do.call(paste, c(dup_rows[, c(id_cols, wide_col), drop=FALSE], sep="--"))
      dup_counts <- table(dup_keys)
      most_common <- names(sort(dup_counts, decreasing=TRUE))[1]

      # Build error message with useful diagnostic information
      err_msg <- sprintf(
        "Found %d duplicates in pivot_wider operation.\n", sum(dup_)/2)

      # Add example of a problematic row
      err_msg <- paste0(err_msg, "Example duplicate: ", id_values, "\n")

      # Add most common duplicate group
      err_msg <- paste0(err_msg, "Most common duplicate key: ", most_common,
                        " (appears ", dup_counts[most_common], " times)\n")

      # Suggest checking the data or using an aggregation function
      err_msg <- paste0(err_msg,
                        "Suggestion: Check your data for unexpected duplicates in the specified columns.",
                        " To proceed anyway, duplicate values could be aggregated (e.g., by taking the mean).")

      stop(err_msg)
    }
  }

  wdata <- tidyr::pivot_wider(df, id_cols=id_cols, names_from=wide_col, values_from="Value")

  if (total_column) {
    idx <- which(sapply(wdata, is.numeric))
    if (length(idx) > 0) wdata$Total <- rowSums(wdata[, idx, drop=FALSE], na.rm=TRUE)
  }

  if (length(rename_mapping) > 0) {
    for (rnm in names(rename_mapping)) {
      if (rnm %in% names(wdata)) {
        names(wdata)[names(wdata)==rnm] <- rename_mapping[[rnm]]
      }
    }
  }

  numc <- which(sapply(wdata, is.numeric))
  if (length(numc) > 0) {
    wdata[, numc] <- lapply(wdata[, numc, drop=FALSE], function(x) round(x, decimal))
  }

  sc <- character(0)
  if ("Unit" %in% names(wdata)) sc <- c(sc, "Unit")
  for (g_ in group_cols) {
    if (g_ != "Unit") {
      rename_ <- if (g_ %in% names(rename_mapping)) rename_mapping[[g_]] else g_
      sc <- c(sc, rename_)
    }
  }
  sc <- intersect(sc, names(wdata))
  if (length(sc) > 0) {
    wdata <- wdata[do.call(order, lapply(sc, function(z) wdata[[z]])), ]
  }

  final_col <- character(0)
  for (g_ in group_cols) {
    rn_ <- if (g_ %in% names(rename_mapping)) rename_mapping[[g_]] else g_
    if (rn_ %in% names(wdata) && !(rn_ %in% final_col)) final_col <- c(final_col, rn_)
  }
  nonnum <- setdiff(names(wdata)[!sapply(wdata, is.numeric)], final_col)
  final_col <- c(final_col, nonnum)
  dd_ <- names(wdata)[sapply(wdata, is.numeric)]
  if ("Total" %in% dd_) dd_ <- c(setdiff(dd_, "Total"), "Total")
  final_col <- c(final_col, dd_)
  if (all(final_col %in% names(wdata))) {
    wdata <- wdata[, final_col, drop=FALSE]
  }
  wdata
}


#' @title Export Detailed Tables (Internal)
#'
#' @description
#' Creates Excel workbooks from a list of data frames, applying styling, merging
#' repeated grouping values, and optionally generating separate files or multiple
#' sheets in a single file. This version also supports an optional black border
#' after each group in the first column if `add_group_line = TRUE`.
#'
#' @param result_list A named list of data frames to export.
#' @param output_path Character. The output directory path for saving the Excel file(s).
#' @param separate_file Logical. If `TRUE`, each data frame is exported as a separate Excel file.
#'   Otherwise, all data frames go into a single workbook.
#' @param sheet_names Optional named list for custom sheet or file naming.
#' @param repeat_label Logical. If `TRUE`, repeats merging in the first grouping column.
#' @param workbook_name Character. The base file name for the single-workbook option.
#' @param add_group_line Logical. If `TRUE`, places a black border to separate each group in the first column.
#'
#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
#'
.export_detail_tables <- function(result_list, output_path, separate_file, sheet_names,
                                  repeat_label, workbook_name,
                                  add_group_line = FALSE) {
  if (is.null(output_path)) {
    output_path <- tempdir()
    message("No output path specified. Using temporary directory: ", output_path)
  }

  # Check if output_path exists, if not create it with error handling
  if (!dir.exists(output_path)) {
    tryCatch({
      dir.create(output_path, recursive = TRUE)
    }, error = function(e) {
      warning("Could not create output directory: ", conditionMessage(e))
      output_path <- tempdir()
      message("Using temporary directory instead: ", output_path)
    })
  }

  # Check if directory is writable
  if (file.access(output_path, 2) != 0) {
    warning("Output directory is not writable: ", output_path)
    output_path <- tempdir()
    message("Using temporary directory instead: ", output_path)
  }

  # Define styles
  header_style_left <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "TopBottom",
    borderStyle = "medium",
    halign = "left",
    valign = "top"
  )
  header_style_right <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "TopBottom",
    borderStyle = "medium",
    halign = "right",
    valign = "top"
  )
  number_style <- openxlsx::createStyle(
    numFmt = "0.00",
    halign = "right",
    valign = "top"
  )
  text_style <- openxlsx::createStyle(
    halign = "left",
    valign = "top"
  )

  # Optional style to add a bottom border for each group in the first column
  group_line_style <- openxlsx::createStyle(
    border = "bottom",
    borderStyle = "thin",
    borderColour = "black"
  )

  # Helper for merging cells + optionally adding group line
  merge_and_add_line <- function(wb, sheet, df, group_cols, is_numeric, add_group_line, start_col) {
    # This is identical logic for merging repeated values
    # We'll focus on the first column for group lines
    if (nrow(df) > 1) {
      if (length(group_cols) > 0) {
        for (col_idx in seq_along(group_cols)) {
          col_name <- group_cols[col_idx]
          if (col_idx == 1 && repeat_label) next
          if (col_name %in% c("Description", "SheetSeparator", "Subtotal")) next

          if (col_idx == 1) {
            # For the very first group column
            col_values <- df[[col_name]]
            group_runs <- rle(as.character(col_values))
            current_row <- 2
            for (i in seq_along(group_runs$lengths)) {
              run_length <- group_runs$lengths[i]
              if (run_length > 1) {
                openxlsx::mergeCells(
                  wb, sheet,
                  rows = current_row:(current_row + run_length - 1),
                  cols = col_idx
                )
              }
              if (add_group_line) {
                # apply bottom border style to the last row of this group
                last_row <- current_row + run_length - 1
                openxlsx::addStyle(
                  wb, sheet, group_line_style,
                  rows = last_row,
                  cols = seq_len(ncol(df)),
                  gridExpand = TRUE,
                  stack = TRUE
                )
              }
              current_row <- current_row + run_length
            }
          } else {
            # For subsequent columns
            preceding_cols <- group_cols[1:col_idx]
            combined_values <- do.call(paste, c(lapply(preceding_cols, function(cc) df[[cc]]), sep = "_"))
            group_runs <- rle(combined_values)
            current_row <- 2
            for (j in seq_along(group_runs$lengths)) {
              run_length <- group_runs$lengths[j]
              if (run_length > 1) {
                openxlsx::mergeCells(
                  wb, sheet,
                  rows = current_row:(current_row + run_length - 1),
                  cols = col_idx
                )
              }
              current_row <- current_row + run_length
            }
          }
        }
      }
    }
  }

  # Handle separate_file vs. single workbook
  if (separate_file) {
    for (sheet_key in names(result_list)) {
      df <- result_list[[sheet_key]]
      file_name <- if (!is.null(sheet_names) && sheet_key %in% names(sheet_names)) {
        sheet_names[[sheet_key]]
      } else {
        gsub("[^[:alnum:]_]", "_", sheet_key)
      }
      wb <- openxlsx::createWorkbook()
      file_path <- file.path(output_path, paste0(file_name, ".xlsx"))
      openxlsx::addWorksheet(wb, "Sheet1")
      openxlsx::writeData(wb, "Sheet1", df)

      is_numeric <- sapply(df, is.numeric)
      numeric_cols <- which(is_numeric)
      text_cols <- which(!is_numeric)

      # Header styling
      for (col in text_cols) {
        openxlsx::addStyle(wb, "Sheet1", header_style_left, rows = 1, cols = col)
      }
      for (col in numeric_cols) {
        openxlsx::addStyle(wb, "Sheet1", header_style_right, rows = 1, cols = col)
      }
      # Body styling
      if (length(text_cols) > 0) {
        for (col in text_cols) {
          openxlsx::addStyle(wb, "Sheet1", text_style, rows = 2:(nrow(df) + 1), cols = col)
        }
      }
      if (length(numeric_cols) > 0) {
        for (col in numeric_cols) {
          openxlsx::addStyle(wb, "Sheet1", number_style, rows = 2:(nrow(df) + 1), cols = col)
        }
      }

      # Merge repeated grouping values + add optional group line
      group_cols <- names(df)[!is_numeric]
      merge_and_add_line(
        wb = wb, sheet = "Sheet1",
        df = df, group_cols = group_cols,
        is_numeric = is_numeric,
        add_group_line = add_group_line,
        start_col = 1
      )

      openxlsx::setColWidths(wb, "Sheet1", cols = 1:ncol(df), widths = c(15, 12, rep(15, ncol(df) - 2)))
      openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
      message("Table exported to: ", file_path)
    }
  } else {
    wb <- openxlsx::createWorkbook()
    file_path <- file.path(output_path, paste0(workbook_name, ".xlsx"))
    for (sheet_key in names(result_list)) {
      df <- result_list[[sheet_key]]
      sheet_name <- if (!is.null(sheet_names) && sheet_key %in% names(sheet_names)) {
        sheet_names[[sheet_key]]
      } else {
        substr(gsub("[^[:alnum:]_]", "_", sheet_key), 1, 31)
      }
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, df)

      is_numeric <- sapply(df, is.numeric)
      numeric_cols <- which(is_numeric)
      text_cols <- which(!is_numeric)

      # Header styling
      for (col in text_cols) {
        openxlsx::addStyle(wb, sheet_name, header_style_left, rows = 1, cols = col)
      }
      for (col in numeric_cols) {
        openxlsx::addStyle(wb, sheet_name, header_style_right, rows = 1, cols = col)
      }
      # Body styling
      if (length(text_cols) > 0) {
        for (col in text_cols) {
          openxlsx::addStyle(wb, sheet_name, text_style, rows = 2:(nrow(df) + 1), cols = col)
        }
      }
      if (length(numeric_cols) > 0) {
        for (col in numeric_cols) {
          openxlsx::addStyle(wb, sheet_name, number_style, rows = 2:(nrow(df) + 1), cols = col)
        }
      }

      # Merge repeated grouping values + add optional group line
      group_cols <- names(df)[!is_numeric]
      merge_and_add_line(
        wb = wb, sheet = sheet_name,
        df = df, group_cols = group_cols,
        is_numeric = is_numeric,
        add_group_line = add_group_line,
        start_col = 1
      )

      openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = c(15, 12, rep(15, ncol(df) - 2)))
    }
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
    message("Table exported to: ", file_path)
  }
}
