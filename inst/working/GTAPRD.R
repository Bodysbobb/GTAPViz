rm(list=ls())
project.folder <- "C:/RunDynam/CP_TPP_New"
setwd("D:/GitHub/GTAPViz/")
devtools::load_all()


agg_mapping <- readxl::read_xlsx("D:/One Drive/OneDrive - purdue.edu/GTAPViz Data/map/OutputMapping.xlsx", sheet = "AggregateMap")

dynamic_input_name(
  type = "prefix",
  base = "sum-bs1b-br1r-",
  pol = "sum-bs1b-br1r-pl1p-",
  pattern = "2018:2031",
  increment = 1,
  period_pattern = TRUE
)





# Extracting Data
auto_gtap_dynamic(
  process_sl4_vars = NULL,
  process_har_vars = NULL,
  subtotal_level = TRUE,
  experiment =  Input_map$Input,
  har_suffix = "",
  mapping_info = "No",
  input_path = project.folder,
  plot_data = TRUE,
  output_formats = list(
    "csv" = "no",
    "stata" = "no",
    "rds" = "no",
    "txt" = "no"),
  mapping_input = Input_map,
  agg_mapping = agg_mapping,
  aggregate = TRUE,
  add_world = TRUE,
  cal_deviation = TRUE,
  base_var = "base",
  policy_var = "pol",
  calculation_agg = "+",
  calculation_dev = "-")
