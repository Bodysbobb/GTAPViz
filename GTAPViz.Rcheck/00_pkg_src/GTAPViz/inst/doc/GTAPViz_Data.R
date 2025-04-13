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
# 
# try(devtools::load_all(".."), silent = TRUE)  # go up one level from /vignettes/
# 
# input_path <- system.file("extdata/in", package = "GTAPViz")
# sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
# har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))
# macro.data    <- readRDS(file.path(input_path, "macro.data.rds"))

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

## ----Experiment Name, eval = FALSE--------------------------------------------
# experiment <- c("TAR10", "SUBT10")
# 
# # Automatically Processing These Inputs in the Input Folder
# # - TAR10.sl4 and TAR10-WEL.har
# # - SUBT10.sl4 and SUBT10-WEL.har

## ----Information Structure, eval = FALSE--------------------------------------
# mapping_info <- "Mix"

## ----Output Formats, eval = FALSE---------------------------------------------
# csv.output <- "YES"
# stata.output <- "YES"
# r.output <- "YES"
# txt.output <- "YES"
# 
# plot_data = TRUE
# 
# # Convert units (optional)
# # Options: "mil2bil", "bil2mil", "pct2frac", "frac2pct" — see details in `?convert_units`
# sl4_convert_unit <- c("mil2bil")
# har_convert_unit <- c("mil2bil")

## ----Config Summary, eval = FALSE---------------------------------------------
# # 1. Project Directory
# project.folder <- "your/project/folder"
# 
# # 2. Define the Input Names
# experiment <- c("TAR10", "SUBT10")
# 
# # 3. Adding Description / Unit (Yes/No/GTAPv7/Mix)
# mapping_info <- "Mix"
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

## ----Preparing Data for Plot, eval = FALSE------------------------------------
# auto_gtap_data(
#   experiment = experiment,
#   process_sl4_vars = sl4map,
#   process_har_vars = harmap,
#   mapping_info = mapping_info,
#   sl4_mapping_info = sl4map,
#   har_mapping_info = harmap,
#   sl4_convert_unit ="mil2bil",
#   har_convert_unit = "mil2bil",
#   region_select = selected_regions,
#   sector_select = selected_sector,
#   subtotal_level = FALSE,
#   rename_columns = TRUE,
#   decimals = 4,
#   project_path = project.folder,
#   plot_data = plot_data,
#   output_formats = list(
#     "csv" = csv.output,
#     "stata" = stata.output,
#     "rds" = r.output,
#     "txt" = txt.output))

## ----Munual FilterData, eval=FALSE--------------------------------------------
# selected_regions <- c("EastAsia", "SEAsia", "Oceania")
# selected_sector  <- NULL

## ----Munual Mapping File Create, eval=FALSE-----------------------------------
# mapping_df <- data.frame(
#   Variable = c("qgdp", "EV", "ppriv"),
#   Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
#   Unit = c("Percent", "million USD", "percent"),
#   stringsAsFactors = FALSE
# )

