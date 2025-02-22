library(HARplus)
library(dplyr)
library(ggplot2)

rm(list=ls())

# Calling R code
# Define the folder where your scripts are located
script_dir <- "D:/GitHub/GTAP-Results-using-R/R"
script_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)
lapply(script_files, source)


# Setup paths and scenarios
project.dir <- "D:/GitHub/HARplus_Dev"
input.dir <- paste0(project.dir, "/inst/extdata")
output.dir <- paste0(project.dir, "/out/Summary")

# Input File Name (All of these are .SL4)
input.file <- c("US_All", "US_All_RetalTar", "US_All_ReduceTar50", "US_All_RegReduceTar50",
                    "US_All10", "US_All10_RetalTar", "US_All10_ReduceTar50", "US_All10_RegReduceTar50")

# Import all inputs
data <- setNames(
  lapply(input.file, function(scenario) {
    load_sl4x(file.path(input.dir, paste0(scenario, ".sl4")))
  }),
  input.file
)

# Extract all variables from all datasets
raw_compare.data <- do.call(
  get_data_by_var,
  c(list(
      var_names = NULL,
      experiment_names = names(data),
      merge_data = TRUE
      ),
    data))

# Add Unit Column to extracted data
raw_compare.data <- add_unit_col(raw_compare.data, mapping_df = "GTAPunit")

# Comparison Plot ---------------------------------------------------------

# Selecting Data
var.data <- c("qgdp", "ppriv", "EV")
selected_regions <- c("USA", "CHN", "CAN", "ASEAN", "ROW")
selected_exp <- input.file

# Apply both filters in one step
compare.data <- raw_compare.data[names(raw_compare.data) %in% var.data]
compare.data <- lapply(compare.data, function(x) {
  if (is.data.frame(x)) {
    x[x$REG %in% selected_regions & x$Experiment %in% selected_exp, ]
  } else {
    x  
  }
})

# Create variable name mapping
title_mapping  <- list(
  "qgdp" = "GDP",
  "ppriv" = "Private Consumption",
  "EV" = "Equivalent Variation"
)

# Plotting
lapply(compare.data, function(df) {
  comparison_plot(df,
                  x_axis_from = "REG", 
                  title_mapping = title_mapping,
                  output_dir = output.dir,
                  compare_by_x_axis = FALSE,
                  color_tone = "grey",
                  panel_cols = 8,
                  panel_rows = 1,
                  width = 20,
                  height = 12)})


# Detail plot -------------------------------------------------------------
# Specify variables to plot
var.data.detail <- c("qo","pmw")
selected.exp.detail <- input.file
  #c("US_All","US_All_RetalTar", "US_All_ReduceTar50", "US_All_RegReduceTar50")
#"US_All10", "US_All10_RetalTar", "US_All10_ReduceTar50", "US_All10_RegReduceTar50")


# Apply both filters in one step
detail_data <- raw_compare.data[names(raw_compare.data) %in% var.data.detail]
detail_data <- lapply(detail_data, function(x) {
  if (is.data.frame(x)) {
    x[x$REG %in% selected_regions & x$Experiment %in% selected.exp.detail, ]
  } else {
    x  
  }
})

# Create variable name mapping
title_mapping  <- list(
  "qo" = "Output",
  "pmw" = "Price of Import Index"
)


lapply(detail_data, function(df) {
  detail_plot(df,
              y_axis_from = c("ACTS", "COMM", "ENDWB"),
              figure_separate_by = "REG",
              title_mapping = title_mapping,
              output_dir = output.dir,
              panel_rows = 1,
              panel_cols = 8)})


# Global Data -------------------------------------------------------------
Macros <- do.call(gtap_macros_data, list(data))

macro_plot(Macros,
           color_tone = "grey",
           combine_var = TRUE,
           output_dir = output.dir,
           selected_vars = c("pxwwld", "qxwwld"),
           panel_rows = 1,
           width = 45)


# Extract by Dimension
grouped_data <- do.call(
  get_data_by_dims,
  c(list(
    NULL,
    experiment_names = names(data),
    merge_data = TRUE
  ),
  data))


# Pivot Data -------------------------------------------------------------
# Selecting Data
var.data <- c("qgdp", "pgdp", "vgdp")
selected_regions <- c("USA", "CHN", "CAN", "ASEAN", "ROW")
selected_exp <- input.file

# Apply both filters in one step
pivot.data <- raw_compare.data[names(raw_compare.data) %in% var.data]
pivot.data <- lapply(pivot.data, function(x) {
  if (is.data.frame(x)) {
    x[x$REG %in% selected_regions & x$Experiment %in% selected_exp, ]
  } else {
    x  
  }
})

# Create result table
result_table <- report_table(
  data = pivot.data,
  vars = c("qgdp", "pgdp", "vgdp"),
  x_axis_from = "REG",
  output_dir = output.dir,
  workbook_name = "macro_indicators",
  sheet_name = "Regional Results"
)



# Macro wide format table
macro_table <- scalar_table(
  data = Macros,
  vars = NULL,
  output_dir = output.dir,
  workbook_name = "global_indicators"
)