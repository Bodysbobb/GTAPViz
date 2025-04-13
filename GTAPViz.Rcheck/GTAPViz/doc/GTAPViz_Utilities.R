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

## ----Add Info, eval = FALSE---------------------------------------------------
# # Create mapping file
# mapping_df <- data.frame(
#   Variable = c("qgdp", "EV", "ppriv"),
#   Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
#   Unit = c("Percent", "million USD", "percent"),
#   stringsAsFactors = FALSE
# )
# 
# datasets <- add_mapping_info(mapping_df,
#                              external_map = mapping_df,
#                              mapping = "Yes")

## ----Converting Unit, eval = FALSE--------------------------------------------
# your_data <- sl4.plot.data   # Change this to your data list name
# 
# your_data <- convert_units(change_unit_from = c("BAHT", "million USD"),
#                            change_unit_to = c("USD", "billion USD"),
#                            adjustment = c("*34.5", "/1000"))

## ----Rename Column, eval = FALSE----------------------------------------------
# # Creating Mapping File
# rename_col <- data.frame(
#   old = c("REG", "COMM", "ACTS"),
#   new = c("Region", "Commodity", "Activity")
#   )
# 
# har.plot.data <- HARplus::rename_dims(har.plot.data,
#                                       mapping_df = rename_col,
#                                       rename_list_names = FALSE)

## ----Sorting Data Function, eval = FALSE--------------------------------------
# # Using the function
# sort_sl4_plot <- sort_plot_data(
#   sl4.plot.data,
#   sort_columns = list(
#     Experiment = c("EXP2", "EXP1"), # Column Name = Sorting Order
#     Region = c("SEAsia", "Oceania")
#     ),
#   sort_by_value_desc = NULL
# )

## ----Rename Value, eval = FALSE-----------------------------------------------
# # Creating Mapping File
# rename.region <- data.frame(
#   ColumnName = "REG",
#   OldName = c("USA", "CHN"),
#   NewName = c("United States", "China"),
#   stringsAsFactors = FALSE
# )
# 
# sl4.plot.data_rename <- rename_value(sl4.plot.data, mapping.file = rename.region)

## ----eval = FALSE-------------------------------------------------------------
# # Rename Value if needed
# wefare.decomp.rename <- data.frame(
#   ColumnName = "COLUMN",
#   OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
#   NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
#   stringsAsFactors = FALSE
# )
# 
# welfare_decomp <- rename_value(har.plot.data, mapping.file = wefare.decomp.rename)

## ----Manual Data Filtering, eval = FALSE--------------------------------------
# # Step 1: Filtering Data using Dataframe and lapply
# manual.data <- lapply(sl4data, function(x) {
#   if (is.data.frame(x)) {
#     x[x$REG %in% selected_regions & x$COMM %in% selected_sector & x$ACTS %in% selected_sector
#       & x$Experiment %in% selected_exp , ]
#   } else {
#     x
#   }
# })
# 
# # Step 2: Adding Unit and Description Column
# manual.data <- add_mapping_info(manual.data, external_map = "/your/mapping.xlsx",
#                                 description_info = TRUE,
#                                 unit_info = TRUE)

