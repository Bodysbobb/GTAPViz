## ----include = FALSE, eval = FALSE--------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>",
#   warning = FALSE,
#   message = FALSE,
#   eval = requireNamespace("GTAPViz", quietly = TRUE)
# )

## ----Dev Period, include = FALSE, eval = TRUE---------------------------------
rm(list=ls())

try(devtools::load_all(".."), silent = TRUE)  # go up one level from /vignettes/

input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))
macro.data    <- readRDS(file.path(input_path, "macro.data.rds"))

## ----Comparison Table, eval = FALSE-------------------------------------------
# report_table(
#   data_list = sl4.plot.data,
#   pivot_col = list(REG = "Variable"),
#   group_by =  list(
#     REG = list("Experiment", "Region")),
#   rename_cols = list("Experiment" = "Scenario"),
# 
#   total_column = FALSE,
#   decimal = 4,
#   subtotal_level = FALSE,
#   repeat_label = FALSE,
#   include_units = TRUE,
# 
#   var_name_by_description = TRUE,
#   add_var_info = TRUE,
#   add_group_line = FALSE,
# 
#   separate_sheet_by = "Unit",
#   export_table = FALSE,
#   output_path = NULL,
#   separate_file = FALSE,
#   workbook_name = "Comparison Table"
# )

## ----Decomposition Table, eval = FALSE----------------------------------------
# report_table(
#   data_list = har.plot.data,
#   pivot_col = list(A = "COLUMN",
#                    E1 = "PRICES"),
#   group_by = list(
#     A = list("Experiment", "Region"),
#     E1 = list("Experiment", "Commodity", "PRICES")
#   ),
#   rename_cols = list("Experiment" = "Scenario"),
# 
#   total_column = TRUE,
#   decimal = 6,
#   subtotal_level = FALSE,
#   repeat_label = FALSE,
#   include_units = TRUE,
# 
#   var_name_by_description = FALSE,
#   add_var_info = FALSE,
#   add_group_line = FALSE,
# 
#   separate_sheet_by = "Region",
#   export_table = FALSE,
#   output_path = NULL,
#   separate_file = FALSE,
#   workbook_name = "Decomposition Table"
# )

## ----Pivot Table with Filter, eval=FALSE--------------------------------------
# pivot_table_with_filter(
#   data = sl4.plot.data[["COMM*REG"]],
#   filter = c("Variable", "Unit"),    # Allow filtering by variable type and unit
#   rows = c("Region", "Commodity"),      # Regions and sectors as row fields
#   cols = c("Experiment"),            # Experiments as column fields
#   data_fields = "Value",             # Values to be aggregated
#   raw_sheet_name = "Raw_Data",       # Sheet name for raw data
#   pivot_sheet_name = "Sector_Pivot", # Sheet name for pivot table
#   export_table = FALSE,
#   output_path = NULL,
#   workbook_name = "Sectoral_Impact_Analysis.xlsx"
# )

