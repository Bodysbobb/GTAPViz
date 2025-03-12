## ----include = FALSE, eval = FALSE--------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>",
#   warning = FALSE,
#   message = FALSE,
#   eval = requireNamespace("GTAPViz", quietly = TRUE)
# )
# 

## ----Dev Period, include = FALSE, eval = FALSE--------------------------------
# rm(list=ls())
# #------------------Dev Period######################################
# # Directory
# #project.folder <- "C:/Users/b_pat/OneDrive - purdue.edu/GTAPViz Data"
# project.folder <- "D:/One Drive/OneDrive - purdue.edu/GTAPViz Data"
# 
# input.folder <- paste0(project.folder, "/in")
# map.folder <- paste0(project.folder, "/map")
# output.folder <- paste0(project.folder, "/out")
# 
# sl4map <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "SL4File")
# harmap <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "HARFile")
# 
# # 2. Experiment / Region / Sector Filter
# experiment <- c("US_All", "US_All_RetalTar", "US_All_ReduceTar50", "US_All_RegReduceTar50",
#                 "US_All10", "US_All10_RetalTar", "US_All10_ReduceTar50", "US_All10_RegReduceTar50")
# 
# 
# selected_regions <- c("USA", "CHN", "CAN", "ROW")
# selected_sector <- NULL
# 
# # 3. Info Mode
# info.mode <- "Mix"
# 
# # 4. Choosing Output: (CSV, STATA, R, TEXT)
# csv.output <- "no"
# stata.output <- "no"
# r.output <- "no"
# txt.output <- "no"
# 
# # 5. For Plotting: (TRUE/FALSE)
# plot_data = TRUE
# 
# setwd <- "D:/GitHub/GTAPViz/R"
# devtools::load_all()
# 
# # Extracting Data
# auto_gtap_data(
#   process_sl4_vars = sl4map,
#   process_har_vars = harmap,
#   sl4_mapping_info = sl4map,
#   har_mapping_info = harmap,
#   region_select = selected_regions,
#   sector_select = selected_sector,
#   subtotal_level = FALSE,
#   experiment = experiment,
#   mapping_info = info.mode,
#   project_path = project.folder,
#   plot_data = plot_data,
#   output_formats = list(
#     "csv" = csv.output,
#     "stata" = stata.output,
#     "rds" = r.output,
#     "txt" = txt.output))
# 
# sl4.plot.data <- convert_units(sl4.plot.data,
#                                scale_auto = "mil2bil")
# 
# har.plot.data <- convert_units(har.plot.data,
#                                scale_auto = "mil2bil")
# 

## ----package, eval = FALSE----------------------------------------------------
# if (!requireNamespace("GTAPViz", quietly = TRUE)) {
#   devtools::install_github("Bodysbobb/GTAPViz")
# }
# 
# require(GTAPViz)

## ----Project Folder, eval = FALSE---------------------------------------------
# project.folder <- "your/folder/path"
# 
# # Optional: You might not need to adjust this
# input.folder <- paste0(project.folder, "/in")
# map.folder <- paste0(project.folder, "/map")
# output.folder <- paste0(project.folder, "/out")
# 

## ----Mapping File, eval = FALSE-----------------------------------------------
# sl4map <- readxl::read_xlsx(map.folder, sheet = "SL4File")
# harmap <- readxl::read_xlsx(map.folder, sheet = "HARFile")

## ----Munual Mapping File Create, eval=FALSE-----------------------------------
# mapping_df <- data.frame(
#   Variable = c("qgdp", "EV", "ppriv"),
#   Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
#   Unit = c("Percent", "million USD", "percent"),
#   stringsAsFactors = FALSE
# )

## ----Filtering Data, eval = FALSE---------------------------------------------
# filter.map <- readxl::read_xlsx(map.folder, sheet = "FilterData")
# 
# selected_regions <- if(length(filter.map$Region) > 0) filter.map$Region else NULL
# selected_sector  <- if(length(filter.map$Sector) > 0) filter.map$Sector else NULL

## ----Munual FilterData, eval=FALSE--------------------------------------------
# selected_regions <- c("EastAsia", "SEAsia", "Oceania")
# selected_sector  <- NULL

## ----Experiment Name, eval = FALSE--------------------------------------------
# experiment <- c("TAR10", "SUBT10")
# 
# # Automatically Processing These Inputs in the Input Folder
# # - TAR10.sl4 and TAR10-WEL.har
# # - SUBT10.sl4 and SUBT10-WEL.har

## ----Information Structure, eval = FALSE--------------------------------------
# info.mode <- "Mix"

## ----Output Formats, eval = FALSE---------------------------------------------
# csv.output <- "YES"
# stata.output <- "YES"
# r.output <- "YES"
# txt.output <- "YES"
# 
# plot_data = TRUE

## ----Config Summary, eval = FALSE---------------------------------------------
# # 1. Project Directory
# project.folder <- "your/project/folder"
# 
# # 2. Define the Input Names
# experiment <- c("TAR10", "SUBT10")
# 
# # 3. Adding Description / Unit (Yes/No/GTAPv7/Mix)
# info.mode <- "Mix"
# 
# # 4. Choosing Output: (CSV, STATA, R, TEXT)
# csv.output <- "No"
# stata.output <- "No"
# r.output <- "No"
# txt.output <- "No"
# 
# # 5. For Plotting: (TRUE/FALSE)
# plot_data = TRUE

## ----Default Input, eval = FALSE----------------------------------------------
# # Default Subdirectories:
# input.folder <- paste0(project.folder, "/in")
# map.folder <- paste0(project.folder, "/map")
# output.folder <- paste0(project.folder, "/out")
# 
# # Default Mapping File:
# sl4map <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "SL4File")
# harmap <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "HARFile")
# filter.map <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "FilterData")
# 
# # Filtering Data:
# selected_regions <- if(length(filter.map$Region) > 0) filter.map$Region else NULL
# selected_sector  <- if(length(filter.map$Sector) > 0) filter.map$Sector else NULL

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/sort_example.png")

## ----Preparing Data for Plot, eval = FALSE------------------------------------
# auto_gtap_data(
#   experiment = experiment,
#   process_sl4_vars = sl4map,
#   process_har_vars = harmap,
#   mapping_info = info.mode,
#   sl4_mapping_info = sl4map,
#   har_mapping_info = harmap,
#   region_select = selected_regions,
#   sector_select = selected_sector,
#   subtotal_level = FALSE,
#   project_path = project.folder,
#   plot_data = plot_data,
#   output_formats = list(
#     "csv" = csv.output,
#     "stata" = stata.output,
#     "rds" = r.output,
#     "txt" = txt.output))

## ----Optional Convert Unit, eval = FALSE--------------------------------------
# sl4.plot.data <- convert_units(sl4.plot.data,
#                                scale_auto = "mil2bil")
# 
# har.plot.data <- convert_units(har.plot.data,
#                                scale_auto = "mil2bil")

## ----eval = FALSE-------------------------------------------------------------
# ?comparison_plot
# ?detail_plot
# ?stack_plot

## ----eval = FALSE-------------------------------------------------------------
# ?get_plot_style_config
# 
# plot_config <- get_plot_style_config("default", as_dataframe = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# export_config <- get_export_config(as_dataframe = TRUE)

## ----color palette, eval=FALSE------------------------------------------------
# # Get all palettes as a list
# all_palettes <- print_palette_colors("all")
# 
# # Click or call a specific palette to view its colors
# all_palettes$winter()   # View the winter palette
# all_palettes$fall()     # View the fall palette
# all_palettes$gtap()     # View the GTAP palette

## ----eval = FALSE-------------------------------------------------------------
# get_plot_config(printing = TRUE)

## ----my style config, eval = FALSE--------------------------------------------
# my_style_config <- list(
# 
#   # Title settings
#   show_title = TRUE,
#   title_face = "bold",
#   title_size = 20,
#   title_hjust = 0.5,
#   add_unit_to_title = TRUE,
#   title_margin = margin(t = 10, r = 0, b = 10, l = 0),
#   title_format = list(
#     type = "standard", #option: prefix, suffix, full, dynamic
#     text = "",
#     sep = ""
#   ),
# 
#   # X-Axis settings
#   show_x_axis_title = TRUE,
#   x_axis_title_face = "bold",
#   x_axis_title_size = 16,
#   x_axis_title_margin = margin(t = 25, r = 25, b = 0, l = 0),
#   show_x_axis_labels = TRUE,
#   x_axis_text_face = "plain",
#   x_axis_text_size = 14,
#   x_axis_text_angle = 0,
#   x_axis_text_hjust = 0,
#   x_axis_description = "",
# 
#   # Y-Axis settings
#   show_y_axis_title = TRUE,
#   y_axis_title_face = "bold",
#   y_axis_title_size = 16,
#   y_axis_title_margin = margin(t = 25, r = 25, b = 0, l = 0),
#   show_y_axis_labels = TRUE,
#   y_axis_text_face = "plain",
#   y_axis_text_size = 14,
#   y_axis_text_angle = 0,
#   y_axis_text_hjust = 0,
#   y_axis_description = "",
#   show_axis_titles_on_all_facets = TRUE,
# 
#   # Value Labels
#   show_value_labels = TRUE,
#   value_label_face = "plain",
#   value_label_size = 5,
#   value_label_position = "above",
#   value_label_decimal_places = 2,
# 
#   # Legend
#   show_legend = FALSE,
#   show_legend_title = FALSE,
#   legend_position = "bottom",
#   legend_title_face = "bold",
#   legend_text_face = "plain",
#   legend_text_size = 14,
# 
#   # Panel Strip
#   strip_face = "bold",
#   strip_text_size = 16,
#   strip_background = "lightgrey",
#   strip_text_margin = margin(t = 10, r = 0, b = 10, l = 0),
# 
#   # Panel Layout
#   panel_spacing = 2,
#   panel_rows = NULL,
#   panel_cols = NULL,
#   theme = NULL,
# 
#   # Colors
#   color_tone = NULL,
#   positive_color = "#2E8B57",
#   negative_color = "#CD5C5C",
#   background_color = "white",
#   grid_color = "grey90",
#   show_grid_major_x = FALSE,
#   show_grid_major_y = FALSE,
#   show_grid_minor_x = FALSE,
#   show_grid_minor_y = FALSE,
# 
#   # Zero Line
#   show_zero_line = TRUE,
#   zero_line_type = "dashed",
#   zero_line_color = "black",
#   zero_line_size = 0.5,
#   zero_line_position = 0,
# 
#   # Bar Chart
#   bar_width = 0.9,
#   bar_spacing = 0.9,
# 
#   # Scale Settings
#   scale_limit = NULL,
#   scale_increment = NULL,
# 
#   # Scale Expansion
#   expansion_y_mult = c(0.05, 0.1),
#   expansion_x_mult = c(0.05, 0.05),
# 
#   # Font Size Control
#   all_font_size = 1,
# 
#   # Data Sorting
#   sort_data_by_value = FALSE
# )
# 
# # Example usage:
# # comparison_plot(data, x_axis_from = "REG",
# #                 plot_style_config = my_style_config)

## ----Comparison Plot, eval = FALSE--------------------------------------------
# reg_data = sl4.plot.data[["1D"]][["Region"]]
# 
# comparison_plot(data = reg_data,
#                 x_axis_from = "Region",
#                 split_by = "Variable",
#                 panel_var = "Experiment",
#                 variable_col = "Variable",
#                 filter_var = NULL,
#                 unit_col = "Unit",
#                 desc_col = "Description",
# 
#                 var_name_by_description = TRUE,
#                 add_var_info = TRUE,
# 
#                 invert_pane = FALSE,
#                 separate_figure = FALSE,
# 
#                 export_picture = TRUE,
#                 export_as_pdf = "merged",
#                 output_path = output.folder,
# 
#                 # See ?export_config
#                 export_config = list(
#                   width = 20,
#                   height = 12
#                   ),
# 
#                 # See ?plot_style_config
#                 plot_style_config = list(
#                   color_tone = "purdue",
#                   add_unit_to_title = TRUE,
#                   title_format = list(
#                     type = "prefix",
#                     text = "Impact on"
#                   ),
#                   panel_rows = 2
#                 ))

## ----Comparison Plot - Short Version, eval = FALSE----------------------------
# comparison_plot(reg_data,
#                 x_axis_from = "Region",
#                 split_by = "Variable",
#                 var_name_by_description = TRUE,
# 
#                 export_picture = TRUE,
#                 output_path = output.folder,
# 
#                 plot_style_config = list(
#                   color_tone = "purdue",
#                   title_format = list(
#                     type = "prefix",
#                     text = "Impact on"
#                   ),
#                   panel_rows = 2
#                 ))

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/cpsplot.png")

## ----GTAP Macro Plot, eval = FALSE--------------------------------------------
# comparison_plot(GTAPMacro,
#                 x_axis_from = "Variable",
#                 split_by = FALSE,
#                 filter_var = c("pgdpwld", "qgdpwld", "vgdpwld"),
# 
#                 export_picture = TRUE,
#                 export_as_pdf = "merged",
#                 export_config = list(
#                   width = 20,
#                   height = 15
#                   ),
#                 output_path = output.folder,
# 
#                 plot_style_config = list(
#                   color_tone = "blue",
#                   title_format = list(
#                     type = "full",
#                     text = "Global Economic Impacts"
#                   )
#                 ))

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/GTAPMacros.png")

## ----Detail Plot, eval = FALSE------------------------------------------------
# detail_plot(sl4.plot.data[["2D"]],
#             x_axis_from = "Sector",
#             split_by = "Region",
# 
#             top_impact = NULL,
#             var_name_by_description = TRUE,
# 
#             invert_pane = TRUE,
#             separate_figure = FALSE,
# 
#             export_config = list(
#               width = 45,
#               height = 20
#             ),
# 
#             export_picture = TRUE,
#             export_as_pdf = FALSE,
#             output_path = output.folder,
# 
#             plot_style_config = list(
#               positive_color = "#2E8B57",
#               negative_color = "#CD5C5C",
#               panel_rows = 1,
#               panel_cols = NULL,
#               show_axis_titles_on_all_facets = FALSE,
#               y_axis_text_size = 25,
#               bar_width = 0.6,
#               all_font_size = 1.1
#             ))

## ----echo=FALSE, fig.align="center", out.width="100%"-------------------------
  knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/detail_plot.png")

## ----detail plot top impact, echo=FALSE, out.width="100%"---------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/detail_plot_top10.png")

## ----Rename Decomposition File, eval = FALSE----------------------------------
# # Rename Value if needed
# wefare.decomp.rename <- data.frame(
#   ColumnName = "COLUMN",
#   OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
#   NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
#   stringsAsFactors = FALSE
# )
# 
# har.plot.data <- rename_value(har.plot.data, mapping.file = wefare.decomp.rename)
# 
# # Rename Column
# rename_col <- data.frame(
#   old = c("REG", "COMM", "ACTS"),
#   new = c("Region", "Commodity", "Activity")
#   )
# 
# har.plot.data <- HARplus::rename_dims(har.plot.data, rename_col)

## ----Decomposition Plot, eval = FALSE-----------------------------------------
# stack_plot(data = har.plot.data[["A"]],
#            x_axis_from = "Region",
#            stack_value_from = "COLUMN",
#            split_by = FALSE,
# 
#            show_total = TRUE,
#            unstack_plot = FALSE,
# 
#            var_name_by_description = TRUE,
# 
#            invert_pane = FALSE,
#            separate_figure = FALSE,
# 
#            export_picture = TRUE,
#            export_as_pdf = "merged",
#            export_config = list(
#              width = 28,
#              height = 15
#            ),
#            output_path = output.folder,
# 
#            plot_style_config = list(
#              color_tone = "gtap",
#              panel_rows = 2,
#              panel_cols = NULL,
#              show_legend = TRUE,
#              show_axis_titles_on_all_facets = FALSE
#            ))

## ----stack plot, echo=FALSE, out.width="100%"---------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/stack_plot.png")

## ----unstack plot, echo=FALSE, out.width="100%"-------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/unstack_plot.png")

## ----Terms of Trade Decomposition, eval = FALSE-------------------------------
# stack_plot(data = har.plot.data[["E1"]],
#            x_axis_from = "Commodity",
#            stack_value_from = "PRICES",
#            split_by = "Region",
# 
#            show_total = TRUE,
#            unstack_plot = FALSE,
# 
#            var_name_by_description = TRUE,
# 
#            invert_pane = TRUE,
#            separate_figure = FALSE,
# 
#            export_picture = TRUE,
#            export_as_pdf = FALSE,
#            export_config = list(
#              width = 50,
#              height = 30
#            ),
#            output_path = output.folder,
# 
#            plot_style_config = list(
#              title_format = list(
#                type = "prefix",
#                text = "Terms of Trade Decomposition",
#                sep = ": "
#              ),
#              color_tone = "blue",
#              panel_rows = 1,
#              show_axis_titles_on_all_facets = FALSE,
#              bar_width = 0.5,
#              bar_spacing = 0,
#              title_size = 48,
#              x_axis_title_size = 32,
#              y_axis_title_size = 32,
#              x_axis_text_size = 20,
#              y_axis_text_size = 20,
#              show_legend = TRUE
#            ))
# 

## ----stack multi plot, echo=FALSE, out.width="100%"---------------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/stack_multi_plot.png")

## ----eval = FALSE-------------------------------------------------------------
# ?report_table

## ----eval = FALSE-------------------------------------------------------------
# data_list = list(DataFrame = DataFrame)

## ----eval = FALSE-------------------------------------------------------------
# report_table(
#   data_list = data_list,
#   pivot_col = list(Region = "Variable"),
#   group_by =  list(
#     Region = list("Experiment", "Region")),
#   rename_cols = list("Experiment" = "Scenario"),
#   total_column = FALSE)

## ----Comparison Table, eval = FALSE-------------------------------------------
# report_table(
#   data_list = sl4.plot.data[["1D"]],
#   pivot_col = list(Region = "Variable"),
#   group_by =  list(
#     Region = list("Experiment", "Region")),
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
#   export_table = TRUE,
#   output_path = output.folder,
#   separate_file = FALSE,
#   workbook_name = "Comparison Table"
# )

## ----example comparison table, echo=FALSE, out.width="100%"-------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/comparison_table.png")

## ----GTAP Macro Table, eval = FALSE-------------------------------------------
# GTAPMacro <- list(GTAPMacro = GTAPMacro)
# 
# report_table(
#   data_list = GTAPMacro,
#   pivot_col = list(
#     GTAPMacro = "Experiment"),
#   group_by =  list(
#     GTAPMacro = list("Variable")),
#   rename_cols = list("Experiment" = "Scenario"),
# 
#   total_column = FALSE,
#   decimal = 4,
#   subtotal_level = FALSE,
#   repeat_label = FALSE,
#   include_units = TRUE,
# 
#   var_name_by_description = FALSE,
#   add_var_info = FALSE,
#   add_group_line = FALSE,
# 
#   separate_sheet_by = NULL,
#   export_table = TRUE,
#   output_path = output.folder,
#   separate_file = FALSE,
#   workbook_name = "GTAPMacro Table"
# )

## ----example GTAPMacro table, echo=FALSE, out.width="100%"--------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/GTAPMacro_table.png")

## ----Detail Table, eval = FALSE-----------------------------------------------
# report_table(
#   data_list = sl4.plot.data[["2D"]],
# 
#   pivot_col = list(Sector = "Sector"),
#   group_by = list(
#     Sector = list("Experiment", "Variable", "Region")),
# 
#   rename_cols = list("Experiment" = "Scenario"),
# 
#   total_column = FALSE,
#   decimal = 2,
#   subtotal_level = FALSE,
#   repeat_label = FALSE,
#   include_units = TRUE,
# 
#   var_name_by_description = TRUE,
#   add_var_info = TRUE,
#   add_group_line = TRUE,
# 
#   separate_sheet_by = NULL,
#   export_table = TRUE,
#   output_path = output.folder,
#   separate_file = FALSE,
#   workbook_name = "Detail Table"
# )

## ----example detail table, echo=FALSE, out.width="100%"-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/detail_table.png")

## ----Decomposition Table, eval = FALSE----------------------------------------
# report_table(
#   data_list = har.plot.data,
#   pivot_col = list(A = "COLUMN",
#                    E1 = "PRICES"),
#   group_by = list(
#     A = list("Experiment", "REG"),
#     E1 = list("Experiment", "REG", "COMM")
#   ),
#   rename_cols = list("REG" = "Region",
#                      "COMM" = "Commodities",
#                      "Experiment" = "Scenario"),
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
#   separate_sheet_by = "REG",
#   export_table = TRUE,
#   output_path = output.folder,
#   separate_file = FALSE,
#   workbook_name = "Decomposition Table"
# )

## ----example decomposition table, echo=FALSE, out.width="100%"----------------
knitr::include_graphics("https://raw.githubusercontent.com/Bodysbobb/GTAPViz/main/vignettes/images/decomp_table.png")

## ----Add Info, eval = FALSE---------------------------------------------------
# mapping_df <- data.frame(
#   Variable = c("qgdp", "EV", "ppriv"),
#   Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
#   Unit = c("Percent", "million USD", "percent"),
#   stringsAsFactors = FALSE
# )
# 
# datasets <- add_mapping_info(mapping_df, external_map = mapping_df, mapping = "Yes")

## ----Converting Unit, eval = FALSE--------------------------------------------
# datasets <- convert_units(change_unit_from = c("million USD", "percent"),
#                           change_unit_to = c("billion USD", "proportion"),
#                           adjustment = c("/1000", "/100"))

## ----Rename Column, eval = FALSE----------------------------------------------
# # Creating Mapping File
# rename_col <- data.frame(
#   old = c("REG", "COMM", "ACTS"),
#   new = c("Region", "Commodity", "Activity")
#   )
# 
# har.plot.data <- HARplus::rename_dims(har.plot.data, rename_col)

## ----Rename Value, eval = FALSE-----------------------------------------------
# # Creating Mapping File
# rename.region <- data.frame(
#   ColumnName = "REG",
#   OldName = c("USA", "CHN"),
#   NewName = c("United States", "China"),
#   stringsAsFactors = FALSE
# )
# 
# har.plot.data.rename <- rename_value(har.plot.data, mapping.file = rename.region)

## ----Sorting Data Function, eval = FALSE--------------------------------------
# # Creating Sorting File
# sorting_specs <- data.frame(
#   Experiment = c("US_All10_RetalTar", "US_All", "US_All10", "US_Specific"),
#   Region = c("CHN", "USA", "ROW", "CAN")
# )
# 
# regional_data <- sort_plot_data(
#   sl4.plot.data,
#   cols = sorting_specs$cols,
#   sort_by_value_desc = NULL
# )

## ----eval = FALSE-------------------------------------------------------------
# auto_gtap_data(
#   # Input Main File Names
#   experiment = experiment,
# 
#   # Input File Suffixes
#   sl4_suffix = "",
#   har_suffix = "-WEL",
# 
#   # Directories
#   input_path = input.folder,
#   output_path = output.folder,
# 
#   # Variable Selection
#   process_sl4_vars = sl4map,
#   process_har_vars = harmap,
# 
#   # Description and Unit Mapping (if `mapping_info` is set to "Yes" or "Mix")
#   sl4_mapping_info = sl4map,
#   har_mapping_info = harmap,
#   mapping_info = info.mode,
# 
#   # Region and Sector Filtering
#   region_select = selected_regions,
#   sector_select = selected_sector,
# 
#   # Data Extraction Process
#   sl4_extract_method = "get_data_by_dims",
#   har_extract_method = "get_data_by_var",
#   subtotal_level = FALSE,
# 
#   # If using `"group_data_by_dims"`, a `priority_list` is required. See `?HARplus::group_data_by_dims`.
#   sl4_priority = NULL,
#   har_priority = NULL,
# 
#   # Output Formats
#   plot_data = plot_data,
#   output_formats = list(
#     "csv" = csv.output,
#     "stata" = stata.output,
#     "rds" = r.output,
#     "txt" = txt.output
#   ),
# 
#   # Output Names for Plot Data
#   sl4_output_name = "sl4.plot.data",
#   har_output_name = "har.plot.data",
#   macro_output_name = "GTAPMacro"
# )

## ----Manual Data Filtering, eval = FALSE--------------------------------------
# # Step 1: Extracting Data
# sl4data1 <- HARplus::load_sl4x(system.file("extdata", "EXP1.sl4", package = "HARplus"))
# sl4data2 <- HARplus::load_sl4x(system.file("extdata", "EXP2.sl4", package = "HARplus"))
# sl4data <- HARplus::get_data_by_dims(NULL, sl4data1, sl4data2, merge_data = TRUE)
# 
# # Step 2: Filtering Data using Dataframe and lapply
# manual.data <- lapply(sl4data, function(x) {
#   if (is.data.frame(x)) {
#     x[x$REG %in% selected_regions & x$COMM %in% selected_sector & x$ACTS %in% selected_sector
#       & x$Experiment %in% selected_exp , ]
#   } else {
#     x
#   }
# })
# 
# # Step 3: Adding Unit and Description Column
# manual.data <- add_mapping_info(manual.data, external_map = "/your/mapping.xlsx",
#                                 description_info = TRUE,
#                                 unit_info = TRUE)

## ----GTAP Macro Data Extraction, eval = FALSE---------------------------------
# Macros <- gtap_macros_data(
#   input_path = input.folder,
#   experiment = experiment,
#   subtotal_level = FALSE
# )

