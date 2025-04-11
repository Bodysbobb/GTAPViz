#' @title Generate a Structured Report Table
#' @md
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
#' @details
#' This function requires a data list and can generate multiple output tables in a single setup.
#' That is, all data frames within the list can be processed simultaneously.
#' See the example for how to generate two data frames at once from the data list `sl4.plot.data`,
#' which is obtained via `auto_gtap_ddta(plot_data = TRUE)`.
#'
#' @return If `export_table = TRUE`, tables are saved as Excel files.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{add_mapping_info}}, \code{\link{convert_units}}, \code{\link{rename_value}},
#' \code{\link{pivot_table_with_filter}}
#'
#' @examples
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' @examples
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' report_table(
#'   data_list = sl4.plot.data,
#'
#'   # === Table Structure ===
#'   pivot_col = list(
#'     REG = "Variable",
#'     "COMM*REG" = "Commodity"
#'   ),
#'   group_by = list(
#'     REG = list("Experiment", "Region"),
#'     "COMM*REG" = list("Experiment", "Variable", "Region")
#'   ),
#'   rename_cols = list("Experiment" = "Scenario"),
#'
#'   # === Table Layout & Labels ===
#'   total_column = FALSE,
#'   decimal = 4,
#'   subtotal_level = FALSE,
#'   repeat_label = FALSE,
#'   include_units = TRUE,
#'   var_name_by_description = TRUE,
#'   add_var_info = TRUE,
#'   add_group_line = FALSE,
#'
#'   # === Export Options ===
#'   separate_sheet_by = "Unit",
#'   export_table = FALSE,
#'   output_path = NULL,
#'   separate_file = FALSE,
#'   workbook_name = "Comparison Table Default"
#' )
#' }
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


#' @title Export Data as an Excel Pivot Table
#'
#' @description Exports a dataset to an Excel file with both raw data and a generated pivot table.
#' @md
#' @param data Data frame. The dataset to be exported.
#' @param filter Character vector (optional). Columns to be used as filter fields in the pivot table.
#' @param rows Character vector (optional). Columns to be used as row fields in the pivot table.
#' @param cols Character vector (optional). Columns to be used as column fields in the pivot table.
#' @param data_fields Character. The data field(s) to be summarized in the pivot table (default: `"Value"`).
#' @param raw_sheet_name Character. Name of the sheet containing raw data (default: `"RawData"`).
#' @param pivot_sheet_name Character. Name of the sheet containing the pivot table (default: `"PivotTable"`).
#' @param dims Character. Cell reference where the pivot table starts (default: `"A3"`).
#' @param export_table Logical. Whether to save the Excel file (default: `TRUE`).
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
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' data_pivot_table <- sl4.plot.data[["REG"]]
#'
#' # Generate Pivot Table with Filter
#' # Only use columns that exist in the data
#' pivot_table_with_filter(
#'
#'   # === Input & Filter Settings ===
#'   data = data_pivot_table,
#'   filter = c("Variable", "Unit"),  # Allow filtering by variable type and unit
#'
#'   # === Pivot Structure ===
#'   rows = c("Region"),             # Rows: Regions (removed "Sector" which doesn't exist)
#'   cols = c("Experiment"),         # Columns: Experiments
#'   data_fields = "Value",          # Values to be aggregated
#'
#'   # === Sheet & Layout ===
#'   raw_sheet_name = "Raw_Data",         # Sheet name for raw data
#'   pivot_sheet_name = "Sector_Pivot",   # Sheet name for pivot table
#'   dims = "A3",                         # Starting cell for pivot table
#'
#'   # === Export Options ===
#'   export_table = FALSE,
#'   output_path = NULL,
#'   workbook_name = "Sectoral_Impact_Analysis.xlsx"
#' )
#' }
pivot_table_with_filter <- function(data,
                                    filter = NULL,
                                    rows = NULL,
                                    cols = NULL,
                                    data_fields = "Value",
                                    raw_sheet_name = "RawData",
                                    pivot_sheet_name = "PivotTable",
                                    dims = "A4",
                                    export_table = FALSE,
                                    output_path = NULL,
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
  if (isTRUE(export_table)) {
    if (!is.null(output_path) && dir.exists(output_path)) {
      output_file <- file.path(output_path, workbook_name)
      wb$save(output_file)
      message("Excel file with pivot table exported to: ", output_file)
    } else {
      message("`output_path` is not defined or does not exist. Please specify a valid output directory to export the table.")
    }
  }

  # Return the workbook object
  return(wb)
}
