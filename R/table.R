#' @title Generate a Structured Report Table
#'
#' @description
#' Transforms multiple datasets into wide-format tables based on defined pivot columns,
#' hierarchical grouping, and renaming rules. Supports optional subtotal filtering
#' and exporting to Excel.
#'
#' @param data_list A named list of data frames to process.
#' @param pivot_col A named list specifying the column to pivot into a wide format for each dataset.
#'   Each dataset can have only one pivot column. Example:
#'   \code{pivot_col = list(A = "COLUMN", E1 = "PRICES")}
#' @param group_by A named list defining hierarchical grouping for each dataset. The order of columns
#'   in each list determines the priority. Example:
#'   \code{group_by = list(A = list("Experiment", "REG"), E1 = list("Experiment", "REG", "COMM"))}
#' @param rename_cols A named list for renaming columns across **all** datasets. Example:
#'   \code{rename_cols = list("REG" = "Region", "COMM" = "Commodities", "Experiment" = "Scenario")}
#' @param separate_sheet_by Optional column name to split sheets in Excel. If defined, each unique
#'   value in the specified column gets its own sheet. Example: \code{separate_sheet_by = "Scenario"}.
#'
#' @param total_column Logical. If `TRUE`, adds a "Total" column summing numeric values.
#' @param subtotal_level Logical. If `TRUE`, includes all subtotal values; otherwise, keeps only `TOTAL` rows.
#' @param repeat_label Logical. If `TRUE`, repeats the first group column in exports for clarity.
#' @param include_units Logical. If `TRUE`, includes "Unit" as a grouping column if applicable.
#' @param component_exclude Optional character vector specifying pivoted values to exclude.
#' @param decimal Numeric. Number of decimal places for rounding values.
#'
#' @param export_table Logical. If `TRUE`, saves the output as an Excel file.
#' @param output_path Character. Directory for saving Excel files when `export_table = TRUE`.
#' @param separate_file Logical. If `TRUE`, saves each dataset as a separate Excel file.
#' @param sheet_names Optional named list for custom sheet names.
#' @param workbook_name Character. Name of the Excel workbook (without extension).
#' @param add_group_line Logical. If `TRUE`, adds a thin line after each group in the exported table.
#'
#' @param var_name_by_description Logical. If `TRUE`, replaces variable codes with descriptions when available.
#' @param add_var_info Logical. If `TRUE`, appends variable codes in parentheses after descriptions.
#' @param unit_select Optional character. Specifies a unit to filter the dataset.
#'
#' @return A named list of transformed data frames. If `export_table = TRUE`, tables are saved as Excel files.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{add_mapping_info}}, \code{\link{convert_units}}, \code{\link{rename_value}}
#'
#' @examples
#' \donttest{
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#'
#' # Note: No need to add .sl4 to the experiment name
#' gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
#'                             input_path = input_path, subtotal_level = FALSE,
#'                             process_sl4_vars = c("qgdp", "EV"), process_har_vars = FALSE,
#'                             mapping_info = "GTAPv7", plot_data = TRUE)
#'
#' report_table(
#'   data_list = sl4.plot.data[["1D"]],
#'   pivot_col = list(Region = "Variable"),
#'   group_by = list(
#'     Region = list("Experiment", "Region")),
#'   rename_cols = list("Experiment" = "Scenario"),
#'
#'   total_column = FALSE,
#'   decimal = 4,
#'   subtotal_level = FALSE,
#'   repeat_label = FALSE,
#'   include_units = TRUE,
#'
#'   var_name_by_description = TRUE,
#'   add_var_info = TRUE,
#'   add_group_line = FALSE,
#'
#'   separate_sheet_by = "Unit",
#'   export_table = TRUE,
#'   output_path = "/your/folder/path",
#'   separate_file = FALSE,
#'   workbook_name = "Comparison Table"
#' )
#' }
report_table <- function(data_list,
                         pivot_col,
                         total_column = FALSE,
                         export_table = FALSE,
                         separate_file = FALSE,
                         output_path = NULL,
                         sheet_names = NULL,
                         include_units = FALSE,
                         component_exclude = NULL,
                         group_by = NULL,
                         rename_cols = NULL,
                         var_name_by_description = TRUE,
                         add_var_info = FALSE,
                         decimal = 2,
                         unit_select = NULL,
                         separate_sheet_by = NULL,
                         subtotal_level = FALSE,
                         repeat_label = FALSE,
                         workbook_name = "detail_results",
                         add_group_line = FALSE) {

  if (!is.list(data_list)) stop("data_list must be a list.")
  if (!is.list(pivot_col) || length(pivot_col) == 0) stop("pivot_col must be a non-empty named list.")
  hnames <- names(pivot_col)
  if (is.null(hnames) || any(hnames == "")) stop("pivot_col must have named elements.")
  miss <- setdiff(hnames, names(data_list))
  if (length(miss) > 0) stop("Data list is missing: ", paste(miss, collapse=", "))

  rename_mapping <- list()
  if (!is.null(rename_cols)) {
    if (!is.list(rename_cols)) {
      warning("rename_cols must be a list. Ignoring.")
    } else {
      rename_mapping <- rename_cols
    }
  }

  norm_group <- list()
  if (is.null(group_by)) {
    for (h in hnames) {
      d <- data_list[[h]]
      pot <- intersect(c("Experiment","EXPERIMENT","experiment","Case","CASE","case","Scenario","SCENARIO","scenario"),
                       names(d))
      if (length(pot) > 0) {
        norm_group[[h]] <- pot[1]
      } else {
        norm_group[[h]] <- NULL
        warning(sprintf("No grouping columns found for '%s'", h))
      }
    }
  } else if (any(names(group_by) %in% hnames)) {
    for (h in hnames) {
      if (h %in% names(group_by)) {
        gval <- group_by[[h]]
        if (is.character(gval)) {
          norm_group[[h]] <- gval
        } else if (is.list(gval)) {
          norm_group[[h]] <- unlist(gval)
        } else {
          norm_group[[h]] <- NULL
          warning(sprintf("Ignoring invalid group spec for '%s'", h))
        }
      } else {
        norm_group[[h]] <- NULL
      }
    }
  } else {
    if (is.character(group_by)) {
      for (h in hnames) norm_group[[h]] <- group_by
    } else if (is.list(group_by) && !any(sapply(group_by, is.list))) {
      uu <- unlist(group_by)
      for (h in hnames) norm_group[[h]] <- uu
    } else {
      norm_group <- NULL
      warning("group_by must be a character vector or list.")
    }
  }

  out_list <- list()

  for (hd in hnames) {
    df <- data_list[[hd]]
    piv <- pivot_col[[hd]]
    if (!piv %in% names(df)) stop(sprintf("Column '%s' not found in '%s'", piv, hd))
    if (!"Value" %in% names(df)) stop(sprintf("'Value' missing in '%s'", hd))

    gc <- character(0)
    if (!is.null(norm_group[[hd]])) {
      for (g_ in norm_group[[hd]]) {
        if (g_ %in% names(df)) {
          gc <- c(gc, g_)
        } else {
          warning(sprintf("Column '%s' not found in '%s'", g_, hd))
        }
      }
    }
    if (length(gc)==0) {
      guess <- grep("^experiment$|^reg$|^region$|^comm$|^sector$|^acts$|^source$|^destination$",
                    names(df), value=TRUE, ignore.case=TRUE)
      gc <- guess
      if (length(gc)==0) warning(sprintf("No grouping found for '%s'", hd))
    }

    if ("Subtotal" %in% names(df)) {
      if (!subtotal_level) {
        keep <- tolower(df$Subtotal)=="total"
        df <- df[keep, ]
        df$Subtotal <- NULL
      } else {
        if (!"Subtotal" %in% gc) gc <- c(gc,"Subtotal")
      }
    }

    if ("Unit" %in% names(df)) {
      if (!is.null(unit_select)) {
        fun_ <- function(x) tolower(gsub("\\s+","",x))
        df$.__u__ <- fun_(df$Unit)
        slct <- fun_(unit_select)
        df <- df[df$.__u__==slct, ]
        df$.__u__<-NULL
        if (nrow(df)==0) {
          warning(sprintf("No data found for unit='%s' in '%s'", unit_select, hd))
          next
        }
      }
      if (length(unique(df$Unit))>1) {
        if (!"Unit"%in%gc) gc<-c(gc,"Unit")
      } else {
        if (include_units && !"Unit"%in%gc) gc<-c(gc,"Unit")
      }
    }

    if (!is.null(component_exclude) && length(component_exclude)>0 && piv %in% names(df)) {
      old_n<-nrow(df)
      df<-df[!(df[[piv]] %in% component_exclude), ]
      removed<-old_n-nrow(df)
      if (removed>0) message(sprintf("Removed %d excluded in '%s'", removed, hd))
    }

    if ("Variable"%in%names(df) && "Description"%in%names(df) && nrow(df)>0) {
      if (var_name_by_description || add_var_info) {
        for (i in seq_len(nrow(df))) {
          var_ <- df$Variable[i]
          des_ <- df$Description[i]
          if (!nzchar(des_)) des_<-NA_character_
          if (is.na(des_)) des_<-var_
          if (var_name_by_description && add_var_info) {
            df$Variable[i] <- paste0(des_," (",var_,")")
          } else if (var_name_by_description && !add_var_info) {
            df$Variable[i]<-des_
          } else if (!var_name_by_description && add_var_info) {
            if (des_==var_) {
              df$Variable[i]<-var_
            } else {
              df$Variable[i]<-paste0(var_," (",des_,")")
            }
          } else {
            df$Variable[i]<-var_
          }
        }
      }
    }

    ssc<-separate_sheet_by

    if (!is.null(ssc) && ssc %in% names(df)) {
      uv<-unique(df[[ssc]])
      partres<-list()
      for (xx in uv) {
        subdf<-df[df[[ssc]]==xx, ]
        newdf<- .process_detail_data(
          subdf,piv,gc,rename_mapping,
          total_column, decimal
        )
        partres[[paste(hd,xx,sep="_")]]<-newdf
      }
      out_list<-c(out_list, partres)
    } else {
      newdf<- .process_detail_data(
        df,piv,gc,rename_mapping,
        total_column, decimal
      )
      out_list[[hd]]<-newdf
    }
  }

  if (export_table && length(out_list)>0) {
    .export_detail_tables(
      out_list,
      output_path,
      separate_file,
      sheet_names,
      repeat_label,
      workbook_name,
      add_group_line
    )
  }
  invisible(out_list)
}


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
#' @author Pattawee Puangchit
#'
.process_detail_data <- function(df, wide_col, group_cols,
                                 rename_mapping, total_column, decimal) {

  keep_ <- c(group_cols, wide_col, "Value")
  df <- df[, intersect(names(df), keep_), drop=FALSE]
  df$Value<-as.numeric(df$Value)
  id_cols <- setdiff(keep_, c(wide_col,"Value"))
  if (nrow(df)>0) {
    check_ <- df[, c(id_cols, wide_col), drop=FALSE]
    dup_ <- duplicated(check_)|duplicated(check_,fromLast=TRUE)
    if (any(dup_)) {
      ex_ <- df[dup_,]
      msg<-sprintf("Found %d duplicates in pivot_wider:\n", sum(dup_))
      for (z in seq_len(min(3,nrow(ex_)))) {
        line<-paste(names(ex_), ex_[z,], sep=":", collapse=",")
        msg<-paste(msg," -",line,"\n")
      }
      stop(msg)
    }
  }

  wdata<-tidyr::pivot_wider(df, id_cols=id_cols,names_from=wide_col,values_from="Value")

  if (total_column) {
    idx <- which(sapply(wdata,is.numeric))
    if (length(idx)>0) wdata$Total<-rowSums(wdata[, idx, drop=FALSE], na.rm=TRUE)
  }

  if (length(rename_mapping)>0) {
    for (rnm in names(rename_mapping)) {
      if (rnm %in% names(wdata)) {
        names(wdata)[names(wdata)==rnm]<-rename_mapping[[rnm]]
      }
    }
  }

  numc <- which(sapply(wdata,is.numeric))
  if (length(numc)>0) {
    wdata[,numc]<-lapply(wdata[,numc,drop=FALSE], function(x) round(x, decimal))
  }

  sc<-character(0)
  if ("Unit"%in%names(wdata)) sc<-c(sc,"Unit")
  for (g_ in group_cols) {
    if (g_!="Unit") {
      rename_ <- if (g_ %in% names(rename_mapping)) rename_mapping[[g_]] else g_
      sc<-c(sc, rename_)
    }
  }
  sc <- intersect(sc,names(wdata))
  if (length(sc)>0) {
    wdata <- wdata[do.call(order, lapply(sc, function(z) wdata[[z]])),]
  }

  final_col<-character(0)
  for (g_ in group_cols) {
    rn_ <- if (g_ %in% names(rename_mapping)) rename_mapping[[g_]] else g_
    if (rn_ %in% names(wdata) && !(rn_ %in% final_col)) final_col<-c(final_col, rn_)
  }
  nonnum<-setdiff(names(wdata)[!sapply(wdata,is.numeric)],final_col)
  final_col<-c(final_col,nonnum)
  dd_ <- names(wdata)[sapply(wdata,is.numeric)]
  if ("Total"%in%dd_) dd_<-c(setdiff(dd_,"Total"),"Total")
  final_col<-c(final_col, dd_)
  if (all(final_col%in%names(wdata))) {
    wdata<-wdata[, final_col, drop=FALSE]
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
#' @author Pattawee Puangchit
#'
.export_detail_tables <- function(result_list, output_path, separate_file, sheet_names,
                                  repeat_label, workbook_name,
                                  add_group_line = FALSE) {
  if (is.null(output_path)) stop("Output directory must be specified for exporting.")
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

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


# Pivot Table -------------------------------------------------------------


#' @title Export Data as an Excel Pivot Table
#'
#' @description Exports a dataset to an Excel file with both raw data and a generated pivot table.
#'
#' @param data Data frame. The dataset to be exported.
#' @param filter Character vector (optional). Columns to be used as filter fields in the pivot table.
#' @param rows Character vector (optional). Columns to be used as row fields in the pivot table.
#' @param cols Character vector (optional). Columns to be used as column fields in the pivot table.
#' @param data_fields Character. The data field(s) to be summarized in the pivot table (default: `"Value"`).
#' @param raw_sheet_name Character. Name of the sheet containing raw data (default: `"RawData"`).
#' @param pivot_sheet_name Character. Name of the sheet containing the pivot table (default: `"PivotTable"`).
#' @param dims Character. Cell reference where the pivot table starts (default: `"A3"`).
#' @param export Logical. Whether to save the Excel file (default: `TRUE`).
#' @param output_path Character. Directory where the file should be saved (default: current working directory).
#' @param workbook_name Character. Name of the output Excel file (default: `"GTAP_PivotTable.xlsx"`).
#'
#' @return An excel workbook object containing both raw data and the pivot table.
#'
#' @details
#' This function creates an Excel workbook with:
#' - A raw data sheet (`raw_sheet_name`) containing the provided dataset.
#' - A pivot table sheet (`pivot_sheet_name`) generated based on specified row, column, and data fields.
#'
#' If `export = TRUE`, the function saves the workbook to the specified `output_path`.
#' @author Pattawee Puangchit
#' @export
pivot_table_with_filter <- function(data,
                                    filter = NULL,
                                    rows = NULL,
                                    cols = NULL,
                                    data_fields = "Value",
                                    raw_sheet_name = "RawData",
                                    pivot_sheet_name = "PivotTable",
                                    dims = "A5",
                                    export = TRUE,
                                    output_path = getwd(),
                                    workbook_name = "GTAP_PivotTable.xlsx") {

  # Create workbook
  wb <- openxlsx2::wb_workbook()

  # Add first worksheet with raw data
  wb$add_worksheet(raw_sheet_name)$
    add_data(x = data)

  # Get data reference for the pivot table
  df <- openxlsx2::wb_data(wb)

  # Add second worksheet for pivot table
  wb$add_worksheet(pivot_sheet_name)

  # Add pivot table to the second worksheet
  wb$add_pivot_table(
    df,
    dims = dims,
    filter = filter,
    rows = rows,
    cols = cols,
    data = data_fields,
    sheet = pivot_sheet_name
  )

  # Export if requested
  if (export) {
    output_file <- file.path(output_path, workbook_name)
    # Save the workbook
    wb$save(output_file)
    message("Excel file with pivot table exported to: ", output_file)
  }

  # Return the workbook object
  return(wb)
}
