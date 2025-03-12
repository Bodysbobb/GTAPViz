# To create and save internal data to be used in the package
# Last update 28 Feb 2025
# GTAP Model version 7

# Main Folder
project.folder <- "D:/GitHub/GTAPViz/inst/GTAPDefault"
# All variables mapping (sl4 and har)
default_info <- readxl::read_xlsx(paste0(project.folder, "/default.xlsx"), sheet = "All")
# GTAP Macro Variables
macro_info <- readxl::read_xlsx(paste0(project.folder, "/default.xlsx"), sheet = "MacroVar")

# Create the default_info data frame
default_info <- data.frame(
  Variable = default_info$Variable,
  Description = default_info$Description,
  Unit = default_info$Unit,
  stringsAsFactors = FALSE
)

macro_info <- data.frame(
  Variable = macro_info$Variable,
  Description = macro_info$Description,
  Unit = macro_info$Unit,
  stringsAsFactors = FALSE
)

usethis::use_data(default_info, macro_info, internal = TRUE, overwrite = TRUE)
