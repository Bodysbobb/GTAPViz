cat("
==============================================================================
PROJECT: Importing GTAP Results from .SL4 and .HAR to CSV/STATA/R/TXT Using R  
==============================================================================
  
DATE: February 2025  
VERSION: 1.0  
  
AUTHORS:  
(1) Pattawee Puangchit, Ph.D. Candidate in Agricultural Economics  
    Purdue University & Research Assistant at GTAP  
    (Contact: ppuangch@purdue.edu)  

(2) Dr. Erwin Corong, Principal Research Economist and Associate Director at GTAP  

==============================================================================
DESCRIPTION:  
This script facilitates the conversion of GTAP results from .SL4 and .HAR formats  
into CSV, STATA, R, and TXT formats for further analysis.  

A fully detailed manuscript, including an R Notebook version of this script,  
is available at: <waiting for publication link>.  

A Python implementation of this script is also available. For more details, please see:  
<waiting for publication link>.  

==============================================================================
ACKNOWLEDGMENTS:  
This project utilizes the R package developed by Maros Ivanic (2023)  
for processing GTAP-related data.  

We acknowledge the contributions of the developers in making GTAP  
data handling more accessible in R.  

==============================================================================
LICENSE:  
This script is licensed under the MIT License.  
A copy of the full license is available in the LICENSE file in this repository.  

==============================================================================
ACADEMIC USE REQUIREMENT:  
If this software is used in academic work (e.g., journal articles, theses, 
conference papers, or reports), proper citation is **required**:  

Pattawee Puangchit (2025). 'Importing GTAP Results from .SL4 and .HAR using R'.  
GitHub Repository. Available at: [GitHub Repository Link]  

Proper citation helps acknowledge the work and supports continued development.  

==============================================================================
IMPORTANT NOTICE:  
The GTAP Model and database require a separate license, which can be obtained at:  
https://www.gtap.agecon.purdue.edu/databases/pricing.asp  
==============================================================================
", sep = "\n")

# INSTALLING AND LOADING REQUIRED PACKAGES ====================================
rm(list=ls())
packages <- c("tidyverse", "writexl", "dplyr", "devtools", 
              "openxlsx", "readxl", "knitr", "rmarkdown", "data.table", 
              "ggplot2", "tcltk", "gridExtra", "haven")

install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

if (!require("HARr")) {
  devtools::install_github('https://github.com/USDA-ERS/MTED-HARr.git')
}
library("HARr")

# ============================ EDIT THIS SECTION =============================== 
# SETTING UP THE DIRECTORY AND OUTPUT OPTIONS

## Project Directory
project.folder <- "D:/GitHub/GTAP-Results-using-R/TestData"

## Sub Directories (Optional)
input.folder <- paste0(project.folder,"/in")
output.folder <- paste0(project.folder,"/out")
map.folder <- paste0(project.folder,"/map")

## Define experiment name / output name 
case.name <- c("ExB14", "ExB15")

## Set output format preferences (YES/NO)
csv.output <- "YES"       # Output CSV files
stata.output <- "Yes"     # Output Stata files
r.output <- "Yes"         # Output R files   
txt.output <- "Yes"       # Output TXT files


# ===================== DO NOT EDIT BELOW THIS LINE ============================ 

# CHECKING, PREPARING INPUT FILES AND OUTPUT FOLDERS ===========================

## Load Input Data from Excel Mapping File
mapping.output <- paste0(map.folder, "/OutputMapping.xlsx")
keysolmap <- read_xlsx(mapping.output, sheet = "KeyVariable")
sl4var <- read_xlsx(mapping.output, sheet = "SL4Variables")
setmap <- read_xlsx(mapping.output, sheet = "SetMap")
decompmap <- read_xlsx(mapping.output, sheet = "Decomp")
welfare.har <- "-WEL.har" 

if (nrow(keysolmap %>% filter_all(any_vars(!is.na(.)))) > 0) {
  
  keysolmap <- keysolmap %>%
    left_join(sl4var %>% select(Variable, Dimension), 
              by = c("Ori.Var.Name" = "Variable")) %>%
    rename(Dimension = Dimension) %>%
    mutate(DimSize = str_count(Dimension, "\\*") + 1)
  
  ## Function to replace values in a string separated by "x"
  mapped.set <- function(str, mapping_df) {
    if (is.na(str)) return(str)
    parts <- strsplit(str, "\\*")[[1]]
    replaced_parts <- sapply(parts, function(x) {
      replacement <- mapping_df$GTAPSet[mapping_df$SubSet == x]
      if (length(replacement) > 0) replacement else x
    })
    paste(replaced_parts, collapse = "*")
  }
  
  ## Apply the replacement to the dataset
  keysolmap <- keysolmap %>%
    mutate(Dimension = unlist(lapply(Dimension, function(x) mapped.set(x, setmap))))
}

## Check for missing values in the 'Dimension' column
keysol.missing_vars <- tryCatch(
  keysolmap %>% filter(is.na(Dimension)), 
  error = function(e) data.frame(Ori.Var.Name = character(0))
)

## Organizing variables by dimension size
unique_sizes <- unique(keysolmap$DimSize[!is.na(keysolmap$DimSize)])
keyvar.list <- lapply(unique_sizes, function(size) {
  subset_data <- keysolmap[keysolmap$DimSize == size, ]
  
  ### Remove rows where Ori.Var.Name is NA
  subset_data <- subset_data[!is.na(subset_data$Ori.Var.Name), ]
  return(subset_data)
})
names(keyvar.list) <- unique_sizes

## Displaying Warnings -------------------------------

## Retrieve file names from the directory
dir_path <- input.folder
files <- list.files(dir_path, full.names = FALSE, ignore.case = TRUE)

## Get specific file lists
sl4_list <- files[grepl("\\.sl4$", files, ignore.case = TRUE)]
wel_list <- files[grepl("-wel\\.har$", files, ignore.case = TRUE)]
sl4_bases <- tolower(trimws(sub("\\.sl4$", "", sl4_list, ignore.case = TRUE)))
wel_bases <- tolower(trimws(sub("-wel\\.har$", "", wel_list, ignore.case = TRUE)))

## Match each SL4 file with a corresponding WEL file
matched_pairs <- intersect(sl4_bases, wel_bases)
unmatched_sl4 <- setdiff(sl4_bases, wel_bases)
unmatched_wel <- setdiff(wel_bases, sl4_bases)

## Count files and matches
total_sl4_files <- length(sl4_list)  # Total .sl4 files
total_wel_files <- length(wel_list)  # Total -WEL.har files
total_matched_files <- length(matched_pairs)  # Total matched pairs
total_unmatched_sl4 <- length(unmatched_sl4)  # Unmatched .sl4 files
total_unmatched_wel <- length(unmatched_wel)  # Unmatched -WEL.har files

## Check for unmatched files
if (total_unmatched_sl4 > 0 || total_unmatched_wel > 0) {
  stop(sprintf(
    "⚠️ There are unmatched files:\nUnmatched SL4 Files: %d\nUnmatched WEL Files: %d\nEnsure all file names follow the naming convention (e.g., ABC.sl4 and ABC-WEL.har).",
    total_unmatched_sl4, total_unmatched_wel
  ))
}

## Check for missing case files
case_names_lower <- tolower(trimws(case.name))
missing_cases <- setdiff(case_names_lower, matched_pairs)

### Stop execution if missing case files are found
if (length(missing_cases) > 0) {
  stop("⚠️ Please verify that all experiments defined in `case.name` have solution files (`.sl4`) and decomposition files (`-WEL.har`).")
}

## Print final report
report <- sprintf(
  "All files are correctly matched. There are %d total solution files and %d total decomposition files.\nProcessing experiments: %s",
  total_sl4_files, total_wel_files,
  paste(case.name, collapse = ", ")
)

### Add warning for missing values in `Dimension`
if (nrow(keysol.missing_vars) > 0) {
  report <- paste0(report, "\n\n⚠️ WARNING: The following variables do not exist or are not defined in the `SL4Variable` sheet:\n",
                   paste(keysol.missing_vars$`Ori.Var.Name`, collapse = ", "))
  cat(report)
  stop()  
}

# Print final report
cat(report)


# ========================== FUNCTION DEFINITIONS ============================== 

## Key variable functions ----------------------------------------
### One Dimension ------------------
execute.one.dimens <- function(case, keyvar.list) {
  current_data <- keyvar.list[["1"]]
  
  if (!all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) return(NULL)
  
  unique_pairs <- unique(current_data[c("Ori.Var.Name", "Dimension")])
  
  # Initialize lists for categorized storage
  region_dfs <- list()  # Stores variables mapped to Region
  sector_dfs <- list()  # Stores variables mapped to Sector
  other_dfs <- list()   # Stores variables mapped to Other (non-Region, non-Sector)
  
  for (i in seq_len(nrow(unique_pairs))) {
    var_name <- unique_pairs$Ori.Var.Name[i]
    dimension <- unique_pairs$Dimension[i]
    
    tryCatch({
      # Step 1: Read data file
      case.file <- paste0(input.folder, "/", case, ".sl4")
      case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
      df <- as.data.frame(case.dta[[var_name]])
      
      # Step 2: Convert to Long Format based on Dimension Mapping
      if (dimension == "REG") {
        # Region-based Mapping: Convert row names to "Region", pivot to long format
        df <- df %>%
          rownames_to_column(var = "Region") %>%
          pivot_longer(cols = -Region, names_to = "Type", values_to = "Value") %>%
          mutate(Variable = var_name, Experiment = case)
        
        region_dfs[[var_name]] <- df  # Store in Region list
        
      } else if (dimension %in% c("ACTS", "COMM")) {
        # Sector-based Mapping: Convert row names to "Sector", pivot to long format
        df <- df %>%
          rownames_to_column(var = "Sector") %>%
          pivot_longer(cols = -Sector, names_to = "Type", values_to = "Value") %>%
          mutate(Variable = var_name, Experiment = case)
        
        sector_dfs[[var_name]] <- df  # Store in Sector list
        
      } else {
        # Other Mappings: Convert row names to "Dim1", pivot to long format
        df <- df %>%
          rownames_to_column(var = "Dim1") %>%
          pivot_longer(cols = -Dim1, names_to = "Type", values_to = "Value") %>%
          mutate(Variable = var_name, Experiment = case)
        
        other_dfs[[var_name]] <- df  # Store in Other list
      }
      
    }, error = function(e) {
      warning(sprintf("Error processing %s with dimension %s: %s", var_name, dimension, e$message))
    })
  }
  
  # Step 3: Combine datasets into separate lists
  return(list(
    ByRegion = if (length(region_dfs) > 0) bind_rows(region_dfs) else NULL,
    BySector = if (length(sector_dfs) > 0) bind_rows(sector_dfs) else NULL,
    ByOther = if (length(other_dfs) > 0) bind_rows(other_dfs) else NULL
  ) %>% purrr::compact())  # Remove NULL elements
}

### Two Dimensions -----------------
execute.two.dimens <- function(case, keyvar.list) {
  current_data <- keyvar.list[["2"]]
  
  if (!all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) return(NULL)
  
  unique_pairs <- unique(current_data[c("Ori.Var.Name", "Dimension")])
  
  processed_dfs <- list()
  error_vars <- character()
  
  for (i in seq_len(nrow(unique_pairs))) {
    var_name <- unique_pairs$Ori.Var.Name[i]
    dimension <- unique_pairs$Dimension[i]
    
    tryCatch({
      # Read data
      case.file <- paste0(input.folder, "/", case, ".sl4")
      case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
      df <- as.data.frame(case.dta[[var_name]])
      
      # Extract dimension parts
      dim_parts <- strsplit(dimension, "\\*")[[1]]
      first_dim <- dim_parts[1]
      second_dim <- dim_parts[2]
      
      # Process based on dimensions
      if (first_dim %in% c("ACTS", "COMM")) {
        # ACTS/COMM in first position
        df_long <- df %>%
          rownames_to_column(var = "Sector") %>%
          pivot_longer(-Sector, names_to = "col_name", values_to = "Value") %>%
          separate(col_name, into = c("Region", "Type"), sep = "\\.") %>%
          mutate(
            Type = ifelse(is.na(Type), "TOTAL", Type),
            Variable = var_name,
            Experiment = case
          )
        processed_dfs$BySector[[var_name]] <- df_long
        
      } else if (second_dim %in% c("ACTS", "COMM")) {
        # ACTS/COMM in second position
        df_long <- df %>%
          rownames_to_column(var = "Region") %>%
          pivot_longer(-Region, names_to = "col_name", values_to = "Value") %>%
          separate(col_name, into = c("Sector", "Type"), sep = "\\.") %>%
          mutate(
            Type = ifelse(is.na(Type), "TOTAL", Type),
            Variable = var_name,
            Experiment = case
          )
        processed_dfs$BySector[[var_name]] <- df_long
        
      } else if (first_dim == "REG") {
        # REG in first position
        df_long <- df %>%
          rownames_to_column(var = "Region") %>%
          pivot_longer(-Region, names_to = "col_name", values_to = "Value") %>%
          separate(col_name, into = c("Dim1", "Type"), sep = "\\.") %>%
          mutate(
            Type = ifelse(is.na(Type), "TOTAL", Type),
            Variable = var_name,
            Experiment = case
          )
        processed_dfs$ByRegion[[var_name]] <- df_long
        
      } else if (second_dim == "REG") {
        # REG in second position
        df_long <- df %>%
          rownames_to_column(var = "Dim1") %>%
          pivot_longer(-Dim1, names_to = "col_name", values_to = "Value") %>%
          separate(col_name, into = c("Region", "Type"), sep = "\\.") %>%
          mutate(
            Type = ifelse(is.na(Type), "TOTAL", Type),
            Variable = var_name,
            Experiment = case
          )
        processed_dfs$ByRegion[[var_name]] <- df_long
        
      } else {
        # Everything else goes to ByOther
        df_long <- df %>%
          rownames_to_column(var = "Dim1") %>%
          pivot_longer(-Dim1, names_to = "col_name", values_to = "Value") %>%
          separate(col_name, into = c("Dim2", "Type"), sep = "\\.") %>%
          mutate(
            Type = ifelse(is.na(Type), "TOTAL", Type),
            Variable = var_name,
            Experiment = case
          )
        processed_dfs$ByOther[[var_name]] <- df_long
      }
      
    }, error = function(e) {
      error_vars <- c(error_vars, var_name)
      warning(sprintf("Error processing %s with dimension %s: %s", 
                      var_name, dimension, e$message))
    })
  }
  
  # Combine results
  result_list <- list()
  
  if (!is.null(processed_dfs$BySector)) {
    result_list$BySector <- bind_rows(processed_dfs$BySector)
  }
  if (!is.null(processed_dfs$ByRegion)) {
    result_list$ByRegion <- bind_rows(processed_dfs$ByRegion)
  }
  if (!is.null(processed_dfs$ByOther)) {
    result_list$ByOther <- bind_rows(processed_dfs$ByOther)
  }
  if (length(error_vars) > 0) {
    result_list$Errors <- error_vars
  }
  
  return(result_list)
}


### Three Dimensions ---------------
execute.three.dimens <- function(case, keyvar.list) {
  current_data <- keyvar.list[["3"]]
  
  if (!all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) return(NULL)
  
  unique_pairs <- unique(current_data[c("Ori.Var.Name", "Dimension")])
  
  three_dfs <- list()
  bilateral_trade <- list()
  error_vars <- character()
  
  for (i in seq_len(nrow(unique_pairs))) {
    var_name <- unique_pairs$Ori.Var.Name[i]
    dimension <- unique_pairs$Dimension[i]
    
    tryCatch({
      # Read data
      case.file <- paste0(input.folder, "/", case, ".sl4")
      case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
      df <- as.data.frame(case.dta[[var_name]])
      
      # Step 1: Clean Column Names
      colnames(df) <- gsub("\\.TOTAL$", "", colnames(df))
      colnames(df) <- make.unique(colnames(df))
      
      # Step 2: Define Mapping Based on Dimension
      dim_parts <- strsplit(dimension, "\\*")[[1]]
      
      row_type <- ifelse(dim_parts[1] == "REG", "Region",
                         ifelse(dim_parts[1] %in% c("ACTS", "COMM"), "Sector", "Dim1"))
      
      col_type1 <- ifelse(dim_parts[2] == "REG", "Source",
                          ifelse(dim_parts[2] %in% c("ACTS", "COMM"), "Sector", "Dim2"))
      
      col_type2 <- ifelse(length(dim_parts) > 2,
                          ifelse(dim_parts[3] == "REG", "Destination",
                                 ifelse(dim_parts[3] %in% c("ACTS", "COMM"), "Sector", "Dim3")),
                          NA)
      
      # Convert row names to appropriate column
      df <- df %>% rownames_to_column(var = row_type)
      
      # Step 3: Convert to Long Format (3D)
      df <- df %>%
        pivot_longer(cols = -!!sym(row_type), names_to = "Variable", values_to = "Value") %>%
        separate(Variable, into = c(col_type1, col_type2, "Type"), sep = "\\.", extra = "merge", fill = "right") %>%
        mutate(Type = ifelse(is.na(Type), "Total", Type),  # Replacing "Base" with "Total"
               Variable = var_name,
               Experiment = case)
      
      # Step 4: Ensure No Duplicate Column Names (Issue with Region)
      col_order <- c("Experiment", row_type, col_type1, col_type2, "Type", "Variable", "Value")
      col_order <- unique(col_order)  # Remove duplicates if any
      
      df <- df %>%
        select(any_of(col_order)) %>%
        mutate(across(all_of(col_order), as.character))
      
      # **Fix: Use Original Dimension Name for List Naming**
      dim_list_name <- paste0(
        toupper(substr(dim_parts[1], 1, 1)), tolower(substr(dim_parts[1], 2, nchar(dim_parts[1]))),
        toupper(substr(dim_parts[2], 1, 1)), tolower(substr(dim_parts[2], 2, nchar(dim_parts[2]))),
        ifelse(length(dim_parts) > 2,
               paste0(toupper(substr(dim_parts[3], 1, 1)), tolower(substr(dim_parts[3], 2, nchar(dim_parts[3])))),
               "")
      )
      
      # Step 5: Separate `qxs` for BilateralTrade
      if (var_name == "qxs") {
        bilateral_trade[[dim_list_name]] <- df  
      } else {
        three_dfs[[dim_list_name]] <- df  
      }
      
    }, error = function(e) {
      error_vars <- c(error_vars, var_name)
      warning(sprintf("Error processing %s with dimension %s: %s", var_name, dimension, e$message))
    })
  }
  
  return(list(
    Errors = error_vars,
    BilateralTrade = if (length(bilateral_trade) > 0) bilateral_trade else NULL,
    ThreeD = if (length(three_dfs) > 0) three_dfs else NULL
  ) %>% purrr::compact())
}

### Dynamic Dimensions ---------------
execute.dynamic.dimens <- function(case, keyvar.list) {
  # Combine all dimensions and filter only those > 3D
  all_data <- do.call(rbind, keyvar.list[as.numeric(names(keyvar.list)) > 3])
  
  if (nrow(all_data) == 0) return(NULL)
  if (!all(c("Ori.Var.Name", "Dimension") %in% names(all_data))) return(NULL)
  
  unique_pairs <- unique(all_data[c("Ori.Var.Name", "Dimension")])
  
  processed_dfs <- list()
  error_vars <- character()
  
  for (i in seq_len(nrow(unique_pairs))) {
    var_name <- unique_pairs$Ori.Var.Name[i]
    dimension <- unique_pairs$Dimension[i]
    dim_size <- length(strsplit(dimension, "\\*")[[1]])
    
    tryCatch({
      # Read data
      case.file <- paste0(input.folder, "/", case, ".sl4")
      case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
      df <- as.data.frame(case.dta[[var_name]])
      
      # Extract dimension parts
      dim_parts <- strsplit(dimension, "\\*")[[1]]
      
      # Step 1: Handle row names based on first dimension
      first_dim <- dim_parts[1]
      row_col_name <- case_when(
        first_dim == "COMM" ~ "Sector",
        first_dim == "ACTS" ~ "Activity",
        first_dim == "REG" ~ if(sum(dim_parts == "REG") > 1) "Source" else "Region",
        !first_dim %in% c("COMM", "ACTS", "REG") ~ paste0("Dim", 1),
        TRUE ~ first_dim
      )
      
      # Step 2: Convert row names and reshape
      df_long <- df %>%
        rownames_to_column(var = row_col_name) %>%
        pivot_longer(
          cols = -1,
          names_to = "col_name",
          values_to = "Value"
        )
      
      # Step 3: Generate column names for remaining dimensions
      remaining_dims <- dim_parts[-1]
      current_dim_number <- 1
      
      col_names <- map_chr(seq_along(remaining_dims), function(i) {
        dim_name <- remaining_dims[i]
        if (dim_name == "COMM") {
          "Sector"
        } else if (dim_name == "ACTS") {
          "Activity"
        } else if (dim_name == "REG") {
          if (sum(remaining_dims == "REG") > 1) {
            if (which(remaining_dims == "REG")[1] == i) "Source" else "Destination"
          } else {
            "Region"
          }
        } else {
          if (!first_dim %in% c("COMM", "ACTS", "REG") && i == 1) {
            current_dim_number <<- 2
          }
          paste0("Dim", current_dim_number + i - 1)
        }
      })
      
      # Add Type to column names
      col_names <- c(col_names, "Type")
      
      # Split column names and assign proper names
      df_long <- df_long %>%
        separate(
          col_name,
          into = col_names,
          sep = "\\.",
          extra = "merge",
          fill = "right"
        ) %>%
        mutate(
          Type = ifelse(is.na(Type), "TOTAL", Type),
          Variable = var_name,
          Experiment = case,
          Dimension = dimension,
          DimSize = dim_size
        )
      
      # Store with dimension size prefix and original dimension name as key
      dim_list_name <- paste0(dim_size, "D_", paste(dim_parts, collapse = ""))
      processed_dfs[[dim_list_name]] <- df_long
      
    }, error = function(e) {
      error_vars <- c(error_vars, var_name)
      warning(sprintf("Error processing %s with dimension %s: %s", 
                      var_name, dimension, e$message))
    })
  }
  
  # Return results
  result_list <- list(
    Data = if (length(processed_dfs) > 0) processed_dfs else NULL,
    Errors = error_vars
  ) %>% purrr::compact()
  
  return(result_list)
}

## Decomposition functions --------------------------------------
### Welfare Decomposition ---------------------
ev.decomp.dat <- function(case, variables) {
  case.file <- paste0(input.folder, "/", case, welfare.har)
  case.dta <- HARr::read_har(case.file, toLowerCase = FALSE)
  
  # Process all variables at once
  result <- lapply(variables, function(var) {
    
    # Define mapping for variable names
    ev.var.original.name <- c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", 
                              "tot_E1", "IS_F1", "pref_G1")  # Don't change
    ev.var.new.name <- c("AllocEff", "Endwb", "TechChg", "Pop", 
                         "ToT", "I_S", "Pref") # New name
    ev_name_mapping <- setNames(ev.var.new.name, ev.var.original.name)  
    
    df <- as.data.frame(case.dta[[var]])
    colnames(df) <- ev_name_mapping[colnames(df)]
    return(df)
  })
  names(result) <- variables
  return(result)
}

### Terms of Trade Decomposition --------------
tot.decom.dat <- function(case, variables) {
  case.file <- paste0(input.folder, "/", case, welfare.har)
  case.dta <- HARr::read_har(case.file, toLowerCase = FALSE)
  
  # Process all variables at once
  result <- lapply(variables, function(var) {
    # Step 1: Convert rownames to Sector column first
    df <- as.data.frame(case.dta[[var]]) %>%
      rownames_to_column("Sector")
    
    # Step 2: Get info from column names
    df_processed <- df %>%
      pivot_longer(
        cols = -Sector,  # Keep Sector column
        names_to = c("Region", "Type", "Unit"),
        names_sep = "\\.",  # Use dots as separator
        values_to = "Value"
      ) %>%
      # Step 3: Pivot to get Type columns (pworld, pexport, pimport)
      pivot_wider(
        names_from = Type,
        values_from = Value
      )
    
    return(df_processed)
  })
  
  names(result) <- variables
  return(result)
}


# ============================= MAIN EXECUTION ================================= 

## Process One Dimension -------------------------------------
if (!is.null(keyvar.list[["1"]])) {
  # Step 1: Process 1D data across all cases
  one.dim.result <- setNames(
    lapply(case.name, function(case) execute.one.dimens(case, keyvar.list)), 
    case.name
  ) %>%
    Filter(function(x) !is.null(x) && any(sapply(x, nrow) > 0), .)
  
  # Step 2: Load variable metadata (New Names, Descriptions, Units)
  one.dim.mapping <- keyvar.list[["1"]] %>% select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  # Step 3: Function to Process and Structure Data
  process_dimension <- function(dim_type, id_col) {
    if (any(sapply(one.dim.result, function(x) dim_type %in% names(x)))) {
      bind_rows(lapply(one.dim.result, `[[`, dim_type)) %>%
        left_join(one.dim.mapping, by = c("Variable" = "Ori.Var.Name")) %>%
        rename(GTAPVar = Variable, Variable = New.Var.Name) %>%
        select(Experiment, matches(id_col), Variable, GTAPVar, Description, Unit, Value, everything())
    } else {
      NULL
    }
  }
  
  # Step 4: Store Processed Data in Categorized Lists
  one.dimens.list <- list(
    ByRegion = process_dimension("ByRegion", "Region"),
    BySector = process_dimension("BySector", "Sector"),
    ByOther = process_dimension("ByOther", "Dim1")
  ) %>% purrr::compact()  # Remove NULL elements
  
  rm(one.dim.result, one.dim.mapping)  # Cleanup memory
} else {
  message("There is no 1-dimensional data")
}

## Process Two Dimensions -------------------------------------
if (!is.null(keyvar.list[["2"]])) {
  two.dim.result <- setNames(
    lapply(case.name, function(case) execute.two.dimens(case, keyvar.list)), 
    case.name
  ) %>%
    Filter(function(x) !is.null(x) && any(sapply(x, nrow) > 0), .)
  
  two.dim.mapping <- keyvar.list[["2"]] %>% 
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  process_dimension <- function(dim_type, id_col) {
    if (any(sapply(two.dim.result, function(x) dim_type %in% names(x)))) {
      bind_rows(lapply(two.dim.result, `[[`, dim_type)) %>%
        left_join(two.dim.mapping, 
                  by = c("Variable" = "Ori.Var.Name"), 
                  relationship = "many-to-many") %>%
        rename(GTAPVar = Variable, Variable = New.Var.Name) %>%
        select(Experiment, matches(id_col), Type, Variable, 
               GTAPVar, Description, Unit, Value, everything())
    } else {
      NULL
    }
  }
  
  two.dimens.list <- list(
    BySector = process_dimension("BySector", "Sector"),
    ByRegion = process_dimension("ByRegion", "Region"),
    ByOther = process_dimension("ByOther", "Dim1")
  ) %>% purrr::compact()
  
  rm(two.dim.result, two.dim.mapping)
} else {
  message("There is no 2-dimensional data")
}

## Process Three Dimensions -------------------------------------
if (!is.null(keyvar.list[["3"]])) {
  three.dim.result <- setNames(
    lapply(case.name, function(case) execute.three.dimens(case, keyvar.list)), 
    case.name
  ) %>%
    Filter(function(x) !is.null(x) && any(sapply(x, function(y) length(y) > 0)), .)
  
  three.dim.mapping <- keyvar.list[["3"]] %>%
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  process_dimension <- function(dim_list, id_cols) {
    if (length(dim_list) > 0) {
      bind_rows(dim_list) %>%
        left_join(three.dim.mapping, by = c("Variable" = "Ori.Var.Name"), relationship = "many-to-many") %>%
        rename(GTAPVar = Variable, Variable = New.Var.Name) %>%
        select(Experiment, matches(id_cols), Type, Variable, GTAPVar, Description, Unit, Value, everything())
    } else {
      NULL
    }
  }
  
  three.dimens.list <- list()
  
  # Process ThreeD Data
  if (!is.null(three.dim.result)) {
    for (case in names(three.dim.result)) {
      for (dim_name in names(three.dim.result[[case]]$ThreeD)) {
        three.dimens.list[[dim_name]] <- process_dimension(three.dim.result[[case]]$ThreeD[[dim_name]], 
                                                           c("Sector", "Source", "Destination", "Type"))
      }
    }
  }
  
  # Process BilateralTrade Data Separately
  if (!is.null(three.dim.result)) {
    for (case in names(three.dim.result)) {
      for (dim_name in names(three.dim.result[[case]]$BilateralTrade)) {
        three.dimens.list[[paste0("BilateralTrade")]] <- process_dimension(three.dim.result[[case]]$BilateralTrade[[dim_name]], 
                                                                                c("Sector", "Source", "Destination", "Type"))
      }
    }
  }
  
  three.dimens.list <- purrr::compact(three.dimens.list)
  
  rm(three.dim.result, three.dim.mapping)
} else {
  message("There is no 3-dimensional data")
}

## Process Dynamic Dimensions -------------------------------------
# Only process if dimensions > 3 exist
if (!is.null(keyvar.list) && any(as.numeric(names(keyvar.list)) > 3)) {
  
  dynamic.dim.result <- setNames(
    lapply(case.name, function(case) execute.dynamic.dimens(case, keyvar.list)), 
    case.name
  ) %>%
    Filter(function(x) !is.null(x) && any(sapply(x$Data, nrow) > 0), .)
  
  # Apply mapping
  dynamic.dim.mapping <- do.call(rbind, keyvar.list) %>%
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  process_dimension <- function(dim_data) {
    if (!is.null(dim_data)) {
      bind_rows(dim_data) %>%
        left_join(dynamic.dim.mapping, 
                  by = c("Variable" = "Ori.Var.Name"), 
                  relationship = "many-to-many") %>%
        rename(GTAPVar = Variable, Variable = New.Var.Name) %>%
        select(Experiment, Dimension, DimSize, everything())
    } else {
      NULL
    }
  }
  
  # Process and merge across all cases
  dynamic.dimens.list <- list()
  
  if (!is.null(dynamic.dim.result)) {
    # Get all unique dimension names across all cases
    all_dim_names <- unique(unlist(lapply(dynamic.dim.result, function(x) names(x$Data))))
    
    # For each unique dimension type
    for (dim_name in all_dim_names) {
      # Collect and merge data from all cases
      all_case_data <- lapply(dynamic.dim.result, function(x) {
        if (!is.null(x$Data[[dim_name]])) x$Data[[dim_name]] else NULL
      })
      
      # Process merged data
      dynamic.dimens.list[[dim_name]] <- process_dimension(
        bind_rows(all_case_data)
      )
    }
  }
  
  dynamic.dimens.list <- purrr::compact(dynamic.dimens.list)
  
  rm(dynamic.dim.result, dynamic.dim.mapping)
} else {
  message("There is no data with dimensions > 3")
}


## Process Welfare Decomposition -------------------------------------
ev.var <- if(any(decompmap$Ori.Var.Name == "A" & decompmap$`Option (1=yes, 0=no)` == 1)) {
  decompmap$Ori.Var.Name[decompmap$Ori.Var.Name == "A" & decompmap$`Option (1=yes, 0=no)` == 1]
} else {
  character(0)
}

if(length(ev.var) > 0) {
  ev.dat.list <- lapply(case.name, function(case) {
    ev.decomp.dat(case, ev.var)
  })
  names(ev.dat.list) <- case.name
  
  All_EVDecomp.sheet <- ev.dat.list %>%
    imap_dfr(function(case_data, case_name) {
      imap_dfr(case_data, function(var_data, var_name) {
        var_data %>%
          # Convert row names to Region column
          rownames_to_column("Region") %>%
          # Add experiment name and Unit
          mutate(
            Experiment = case_name,
            Unit = "million USD",
            Total = rowSums(across(where(is.numeric)))  
          )
      })
    }) %>%
    mutate(across(where(is.numeric), ~round(., 6))) %>%  
    arrange(Unit, Experiment, Region) %>%
    select(Experiment, Region, everything(), -Total, -Unit, Total, Unit)
  rm(list=c("ev.dat.list"))
}

## Process Terms of Trade Decomposition -------------------------------------
tot.var <- if(any(decompmap$Ori.Var.Name == "E1" & 
                  decompmap$`Option (1=yes, 0=no)` == 1)) {
  decompmap$Ori.Var.Name[decompmap$Ori.Var.Name == "E1" &
                           decompmap$`Option (1=yes, 0=no)` == 1]
} else {
  character(0)
}

if(length(tot.var) > 0) {
  tot.decomp.list <- lapply(case.name, function(case) {
    tot.decom.dat(case, tot.var)
  })
  names(tot.decomp.list) <- case.name
  
  All_ToTDecomp.sheet <- tot.decomp.list %>%
    imap_dfr(function(case_data, case_name) {
      imap_dfr(case_data, function(var_data, var_name) {
        var_data %>%
          as.data.frame() %>%
          mutate(Experiment = case_name)
      })
    })
  
  All_ToTDecomp.sheet <- All_ToTDecomp.sheet %>%
    mutate(Total = pworld + pexport + pimport) %>%
    select(Experiment, everything(), Total, Unit) %>%  
    arrange(Unit, Experiment, Sector)
}


# ================================ EXPORT OUTPUT =============================== 

## Create Output Report (README) ------------------------------------
get_output_location <- function(var_name, dimension, dimsize) {
  prefix <- paste0(dimsize, "D")
  
  if (var_name == "qxs" && dimsize == 3) {
    return(paste0(prefix, "_BilateralTrade"))
  }
  
  suffix <- case_when(
    str_detect(dimension, "REG") ~ "ByRegion",
    str_detect(dimension, "ACTS|COMM") ~ "BySector",
    TRUE ~ "ByOther"
  )
  
  paste0(prefix, "_", suffix)
}

output.report <- if (nrow(keysolmap) > 0 && "DimSize" %in% names(keysolmap)) {
  result <- keysolmap %>%
    rowwise() %>%
    mutate(OutputFile = get_output_location(Ori.Var.Name, Dimension, DimSize)) %>%
    select(Variable = Ori.Var.Name, OutputFile)
  
  decomposition_entries <- list()
  
  if (length(ev.var) > 0) {
    decomposition_entries <- append(decomposition_entries, tibble(Variable = "Welfare Decomposition", OutputFile = "All_EVDecomp"))
  }
  if (length(tot.var) > 0) {
    decomposition_entries <- append(decomposition_entries, tibble(Variable = "Terms of Trade Decomposition", OutputFile = "All_ToTDecomp"))
  }
  
  bind_rows(result, decomposition_entries)
} else {
  tibble(
    Variable = c(if (length(ev.var) > 0) "Welfare Decomposition", 
                 if (length(tot.var) > 0) "Terms of Trade Decomposition"),
    OutputFile = c(if (length(ev.var) > 0) "EVDecomp", 
                   if (length(tot.var) > 0) "ToTDecomp")
  )
}

## Generate and Copy README Report ------------------------------------
copy_readme_report <- function() {
  wb <- createWorkbook()
  addWorksheet(wb, "output.report")
  writeData(wb, "output.report", output.report)
  saveWorkbook(wb, file.path(output.folder, "0.README.xlsx"), overwrite = TRUE)
}

## Export Functions ------------------------------------
is_yes <- function(value) {
  tolower(trimws(value)) == "yes"
}

create_dir_if_missing <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

## Create Output Directories ------------------------------------
output_dirs <- list(
  CSV = if (is_yes(csv.output)) file.path(output.folder, "CSV"),
  STATA = if (is_yes(stata.output)) file.path(output.folder, "STATA"),
  R = if (is_yes(r.output)) file.path(output.folder, "R"),
  TXT = if (is_yes(txt.output)) file.path(output.folder, "TXT")
)

invisible(lapply(output_dirs, create_dir_if_missing))

## Data Export Function ------------------------------------
safe_export_data <- function(data, base_filename) {
  tryCatch({
    if (is_yes(csv.output)) write.csv(data, file.path(output_dirs$CSV, paste0(base_filename, ".csv")), row.names = FALSE)
    if (is_yes(stata.output)) write_dta(as.data.frame(data), file.path(output_dirs$STATA, paste0(base_filename, ".dta")))
    if (is_yes(r.output)) save(data, file = file.path(output_dirs$R, paste0(base_filename, ".RData")))
    if (is_yes(txt.output)) write.table(data, file.path(output_dirs$TXT, paste0(base_filename, ".txt")), sep = "\t", row.names = FALSE, quote = FALSE, na = "")
    message(sprintf("Successfully exported: %s", base_filename))
  }, error = function(e) {
    message(sprintf("Error exporting %s: %s", base_filename, e$message))
  })
}

## Process and Export Dimensional Lists ------------------------------------
safe_process_dim_list <- function(list_name, prefix) {
  tryCatch({
    if (exists(list_name, envir = .GlobalEnv)) {
      dim_list <- get(list_name, envir = .GlobalEnv)
      if (is.list(dim_list) && length(dim_list) > 0) {
        for (name in names(dim_list)) {
          safe_export_data(dim_list[[name]], paste0(prefix, "_", name))
        }
      }
    }
  }, error = function(e) {
    message(sprintf("Error processing %s: %s", list_name, e$message))
  })
}

## Export Decomposition Sheets ------------------------------------
safe_export_decomposition <- function() {
  if (length(ev.var) > 0 && exists("All_EVDecomp.sheet")) {
    safe_export_data(All_EVDecomp.sheet, "EVDecomp")
  }
  if (length(tot.var) > 0 && exists("All_ToTDecomp.sheet")) {
    safe_export_data(All_ToTDecomp.sheet, "ToTDecomp")
  }
}

## Check Enabled Output Types and Execute ------------------------------------
if (!any(sapply(c(csv.output, stata.output, r.output, txt.output), is_yes))) {
  stop("No output types are enabled. Set at least one output type to 'yes' to export data.")
} else {
  # Define Dimensional Lists
  dim_lists <- list(
    "one.dimens.list" = "1D",
    "two.dimens.list" = "2D",
    "three.dimens.list" = "3D"
  )
  
  # Add dynamic dimensions list if it exists
  if (exists("dynamic.dimens.list") && length(dynamic.dimens.list) > 0) {
    for (dim_name in names(dynamic.dimens.list)) {
      dim_lists[[paste0("dynamic.dimens.list[['", dim_name, "']]")]] <- dim_name
    }
  }
  
  # Generate README
  copy_readme_report()
  
  # Process & Export All Detected Dimensions
  lapply(names(dim_lists), function(list_name) {
    safe_process_dim_list(list_name, dim_lists[[list_name]])
  })
  
  # Export Decomposition Sheets
  safe_export_decomposition()
  
  # Print Export Summary
  message("\n=== Export Summary ===")
  lapply(names(output_dirs), function(type) {
    if (!is.null(output_dirs[[type]])) message(sprintf("✓ %s files saved successfully in: %s", type, output_dirs[[type]]))
  })
  message("==================")
}