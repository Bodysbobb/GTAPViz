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
project.folder <- "D:/R Directory/GEMPACK"

## Sub Directories (Optional)
input.folder <- paste0(project.folder,"/in")
output.folder <- paste0(project.folder,"/out")
map.folder <- paste0(project.folder,"/map")

## Define experiment name / output name 
case.name <- c("US_All", "US_All_RetalTar")

## Set output format preferences (YES/NO)
csv.output <- "YES"       # Output CSV files
stata.output <- "Yes"     # Output Stata files
r.output <- "Yes"         # Output R files   
txt.output <- "Yes"       # Output TXT files


# ===================== DO NOT EDIT BELOW THIS LINE ============================ 

# CHECKING, PREPARING INPUT FILES AND OUTPUT FOLDERS =====================================

## Load Input Data from Excel Mapping File
mapping.output <- paste0(map.folder, "/OutputMapping.xlsx")
keysolmap <- read_xlsx(mapping.output, sheet = "KeyVariable")
sl4var <- read_xlsx(mapping.output, sheet = "SL4Variables")
setmap <- read_xlsx(mapping.output, sheet = "SetMap")
decompmap <- read_xlsx(mapping.output, sheet = "Decomp")
welfare.har <- "-WEL.har" 

if (nrow(keysolmap %>% filter_all(any_vars(!is.na(.)))) > 0) {
  
  keysolmap <- keysolmap %>%
    left_join(sl4var %>% select(Variable, Size), 
              by = c("Ori.Var.Name" = "Variable")) %>%
    rename(Dimension = Size) %>%
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
keyvar.one.dimens <- function(case, variables, dimen = NULL) {
  case.file <- paste0(input.folder, "/", case, ".sl4")
  case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
  
  # Process variables based on dimension specification
  result <- lapply(variables, function(var) {
    df <- as.data.frame(case.dta[[var]])
    names(df) <- var
    
    # Handle different dimension cases if dimen is specified
    if (!is.null(dimen)) {
      # Convert rownames based on dimension type
      if (dimen == "REG") {
        df$Region <- rownames(df)
        df$DimensionType <- "Region"  # Add dimension type marker
      } else if (dimen %in% c("ACTS", "COMM")) {
        df$Sector <- rownames(df)
        df$DimensionType <- "Sector"  # Add dimension type marker
      } else {
        # Handle other dimensions
        df$Other <- "Other"  # Convert all rownames to "Other"
        df$OriginalName <- rownames(df)  # Store original name if needed
        df$DimensionType <- "Other"  # Add dimension type marker
      }
      
      # Reset rownames after converting to column
      rownames(df) <- NULL
    }
    
    return(df)
  })
  
  names(result) <- variables
  return(result)
}

execute.one.dimens <- function(case, keyvar.list) {
  # Process only sublist "1"
  current_data <- keyvar.list[["1"]]
  
  # Initialize empty lists for each dimension type
  region_dfs <- list()
  sector_dfs <- list()
  other_dfs <- list()
  
  if (all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) {
    var_names <- as.character(current_data$Ori.Var.Name)
    dimensions <- as.character(current_data$Dimension)
    
    unique_pairs <- unique(data.frame(
      var_name = var_names,
      dimension = dimensions,
      stringsAsFactors = FALSE
    ))
    
    for (i in 1:nrow(unique_pairs)) {
      var_name <- unique_pairs$var_name[i]
      dimension <- unique_pairs$dimension[i]
      
      tryCatch({
        result <- keyvar.one.dimens(
          case = case,
          variables = var_name,
          dimen = dimension
        )
        
        df <- result[[var_name]]
        
        # Route data to appropriate list based on DimensionType
        if (df$DimensionType[1] == "Region") {
          long_df <- df %>%
            select(-DimensionType) %>%
            pivot_longer(
              cols = -Region,
              names_to = "Variable",
              values_to = "Value"
            ) %>%
            mutate(Variable = var_name)
          region_dfs[[var_name]] <- long_df
          
        } else if (df$DimensionType[1] == "Sector") {
          long_df <- df %>%
            select(-DimensionType) %>%
            pivot_longer(
              cols = -Sector,
              names_to = "Variable",
              values_to = "Value"
            ) %>%
            mutate(Variable = var_name)
          sector_dfs[[var_name]] <- long_df
          
        } else if (df$DimensionType[1] == "Other") {
          long_df <- df %>%
            select(-DimensionType, -OriginalName) %>%
            pivot_longer(
              cols = -Other,
              names_to = "Variable",
              values_to = "Value"
            ) %>%
            mutate(Variable = var_name)
          other_dfs[[var_name]] <- long_df
        }
        
      }, error = function(e) {
        warning(sprintf("Error processing %s with dimension %s: %s", 
                        var_name, dimension, e$message))
      })
    }
  }
  
  # Combine datasets for each dimension type
  region_combined <- bind_rows(region_dfs) %>% 
    mutate(Experiment = case)
  
  sector_combined <- bind_rows(sector_dfs) %>% 
    mutate(Experiment = case)
  
  other_combined <- bind_rows(other_dfs) %>% 
    mutate(Experiment = case)
  
  # Return non-empty datasets
  return(list(
    Region = if (nrow(region_combined) > 0) region_combined else NULL,
    Sector = if (nrow(sector_combined) > 0) sector_combined else NULL,
    Other = if (nrow(other_combined) > 0) other_combined else NULL
  ) %>% purrr::compact())
}

### Two Dimensions -----------------
keyvar.two.dimens <- function(case, variables, dimen = NULL) {
  case.file <- paste0(input.folder, "/", case, ".sl4")
  case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
  
  result <- lapply(variables, function(var) {
    df <- as.data.frame(case.dta[[var]])
    
    if (!is.null(dimen)) {
      colnames(df) <- gsub("\\.TOTAL$", "", colnames(df))
      
      # Parse dimension pattern
      dim_parts <- strsplit(dimen, "\\*")[[1]]
      
      if (dimen %in% c("ACTS*REG", "COMM*REG")) {
        df <- df %>%
          rownames_to_column("Sector") %>%
          pivot_longer(
            cols = -Sector,
            names_to = "Region",
            values_to = "Value"
          ) %>%
          mutate(Variable = var)
        
      } else if (dim_parts[2] == "REG") {  # Any *REG pattern
        df <- df %>%
          rownames_to_column("Type") %>%
          pivot_longer(
            cols = -Type,
            names_to = "Region",
            values_to = "Value"
          ) %>%
          mutate(Variable = var)
        
      } else if (dim_parts[1] == "REG") {  # Any REG* pattern
        df <- df %>%
          rownames_to_column("Region") %>%
          pivot_longer(
            cols = -Region,
            names_to = "Type",
            values_to = "Value"
          ) %>%
          mutate(Variable = var)
      }
    }
    return(df)
  })
  
  names(result) <- variables
  return(result)
}

execute.two.dimens <- function(case, keyvar.list) {
  current_data <- keyvar.list[["2"]]
  sector_data <- list()
  other_data <- list()
  error_vars <- character()
  
  if (all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) {
    unique_pairs <- unique(data.frame(
      var_name = as.character(current_data$Ori.Var.Name),
      dimension = as.character(current_data$Dimension),
      stringsAsFactors = FALSE
    ))
    
    for (i in 1:nrow(unique_pairs)) {
      var_name <- unique_pairs$var_name[i]
      dimension <- unique_pairs$dimension[i]
      
      result <- tryCatch({
        res <- keyvar.two.dimens(case = case, variables = var_name, dimen = dimension)
        df <- res[[var_name]]
        
        if (dimension %in% c("ACTS*REG", "COMM*REG")) {
          sector_data[[var_name]] <- df
        } else {
          other_data[[var_name]] <- df
        }
        NULL
      }, error = function(e) {
        var_name
      })
      
      if (!is.null(result)) error_vars <- c(error_vars, result)
    }
  }
  
  combined_results <- list(
    BySector = if (length(sector_data) > 0) bind_rows(sector_data) else NULL,
    Other = if (length(other_data) > 0) bind_rows(other_data) else NULL,
    errors = error_vars
  )
  
  combined_results <- combined_results[!sapply(combined_results, is.null)]
  if (length(error_vars) > 0) {
    message("Variables that couldn't be processed: ", paste(error_vars, collapse = ", "))
  }
  
  return(combined_results)
}

### Three Dimensions ---------------
keyvar.three.dimens <- function(case, variables, dimen = NULL) {
  case.file <- paste0(input.folder, "/", case, ".sl4")
  case.dta <- HARr::read_SL4(case.file, toLowerCase = FALSE)
  
  # Process variables based on dimension specification
  result <- lapply(variables, function(var) {
    df <- as.data.frame(case.dta[[var]])
    
    if (!is.null(dimen)) {
      # Process based on dimension type
      if (dimen == "COMM*REG*REG") {
        df <- df %>%
          rownames_to_column("Sector") %>%
          pivot_longer(
            cols = -Sector,
            names_to = "original_name",
            values_to = "Value"
          ) %>%
          separate(
            original_name,
            into = c("Source", "Destination", "Type"),
            sep = "\\.",
            remove = TRUE,
            fill = "right"  # Handle cases where Type (.TOTAL) might not exist
          ) %>%
          mutate(Variable = var)
        
      } else if (dimen == "ENDW*ACTS*REG") {
        df <- df %>%
          rownames_to_column("Endowment") %>%
          pivot_longer(
            cols = -Endowment,
            names_to = "original_name",
            values_to = "Value"
          ) %>%
          separate(
            original_name,
            into = c("Activity", "Region", "Type"),
            sep = "\\.",
            remove = TRUE,
            fill = "right"
          ) %>%
          mutate(Variable = var)
        
      } else if (dimen == "COMM*ACTS*REG") {
        df <- df %>%
          rownames_to_column("Sector") %>%
          pivot_longer(
            cols = -Sector,
            names_to = "original_name",
            values_to = "Value"
          ) %>%
          separate(
            original_name,
            into = c("Activity", "Region", "Type"),
            sep = "\\.",
            remove = TRUE,
            fill = "right"
          ) %>%
          mutate(Variable = var)
        
      } else if (dimen == "ENDWF*ACTS*REG") {
        df <- df %>%
          rownames_to_column("Variable") %>%
          pivot_longer(
            cols = -Variable,
            names_to = "original_name",
            values_to = "Value"
          ) %>%
          separate(
            original_name,
            into = c("Sector", "Region", "Type"),
            sep = "\\.",
            remove = TRUE,
            fill = "right"
          ) %>%
          mutate(Variable = var)
      }
    }
    
    return(df)
  })
  
  names(result) <- variables
  return(result)
}

execute.three.dimens <- function(case, keyvar.list) {
  current_data <- keyvar.list[["3"]]
  
  # Initialize lists for different types
  comm_reg_reg_data <- list()
  endw_acts_reg_data <- list()
  comm_acts_reg_data <- list()
  endwf_acts_reg_data <- list()
  
  if (all(c("Ori.Var.Name", "Dimension") %in% names(current_data))) {
    var_names <- as.character(current_data$Ori.Var.Name)
    dimensions <- as.character(current_data$Dimension)
    
    unique_pairs <- unique(data.frame(
      var_name = var_names,
      dimension = dimensions,
      stringsAsFactors = FALSE
    ))
    
    for (i in 1:nrow(unique_pairs)) {
      var_name <- unique_pairs$var_name[i]
      dimension <- unique_pairs$dimension[i]
      
      tryCatch({
        result <- keyvar.three.dimens(
          case = case,
          variables = var_name,
          dimen = dimension
        )
        
        df <- result[[var_name]]
        
        # Sort into appropriate list based on dimension type
        if (dimension == "COMM*REG*REG") {
          comm_reg_reg_data[[var_name]] <- df
        } else if (dimension == "ENDW*ACTS*REG") {
          endw_acts_reg_data[[var_name]] <- df
        } else if (dimension == "COMM*ACTS*REG") {
          comm_acts_reg_data[[var_name]] <- df
        } else if (dimension == "ENDWF*ACTS*REG") {
          endwf_acts_reg_data[[var_name]] <- df
        }
        
      }, error = function(e) {
        warning(sprintf("Error processing %s with dimension %s: %s", 
                        var_name, dimension, e$message))
      })
    }
  }
  
  # Combine data within each category
  combined_results <- list(
    CommRegReg = if (length(comm_reg_reg_data) > 0) 
      bind_rows(comm_reg_reg_data) else NULL,
    EndwActsReg = if (length(endw_acts_reg_data) > 0) 
      bind_rows(endw_acts_reg_data) else NULL,
    CommActsReg = if (length(comm_acts_reg_data) > 0) 
      bind_rows(comm_acts_reg_data) else NULL,
    EndwfActsReg = if (length(endwf_acts_reg_data) > 0) 
      bind_rows(endwf_acts_reg_data) else NULL
  )
  
  # Filter out NULL elements
  combined_results <- combined_results[!sapply(combined_results, is.null)]
  
  return(combined_results)
}

## Decomposition functions --------------------------------------
### Welfare Decomposition ---------------------
ev.decomp.dat <- function(case, variables) {
  case.file <- paste0(input.folder, "/", case, welfare.har)
  case.dta <- HARr::read_har(case.file, toLowerCase = FALSE)
  
  # Process all variables at once
  result <- lapply(variables, function(var) {
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
  one.dim.result <- lapply(case.name, function(case) {
    execute.one.dimens(case, keyvar.list)
  })
  names(one.dim.result) <- case.name
  one.dim.result <- one.dim.result[!sapply(one.dim.result, function(x) is.null(x) || 
                                             (is.data.frame(x) && nrow(x) == 0))]
  
  # Get the mapping data
  one.dim.mapping <- keyvar.list[["1"]] %>%
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  # Initialize list for combining data
  one.dimens.list <- list()
  
  # Process ByRegion data
  if (any(sapply(one.dim.result, function(x) "Region" %in% names(x)))) {
    one.dimens.list$ByRegion <- bind_rows(lapply(one.dim.result, 
                                                 function(x) x$Region)) %>%
      left_join(one.dim.mapping, 
                by = c("Variable" = "Ori.Var.Name"),
                relationship = "many-to-many") %>%
      rename(GTAPVar = Variable, 
             Variable = New.Var.Name) %>%
      select(Experiment, Region, Variable, GTAPVar, Description, Unit, Value)
  }
  
  # Process BySector data
  if (any(sapply(one.dim.result, function(x) "Sector" %in% names(x)))) {
    one.dimens.list$BySector <- bind_rows(lapply(one.dim.result, 
                                                 function(x) x$Sector)) %>%
      left_join(one.dim.mapping, 
                by = c("Variable" = "Ori.Var.Name"),
                relationship = "many-to-many") %>%
      rename(GTAPVar = Variable, 
             Variable = New.Var.Name) %>%
      select(Experiment, Sector, Variable, GTAPVar, Description, Unit, Value)
  }
  
  # Process ByOther data
  if (any(sapply(one.dim.result, function(x) "Other" %in% names(x)))) {
    one.dimens.list$ByOther <- bind_rows(lapply(one.dim.result, function(x) x$Other)) %>%
      left_join(one.dim.mapping, 
                by = c("Variable" = "Ori.Var.Name"),
                relationship = "many-to-many") %>%
      rename(GTAPVar = Variable, 
             Variable = New.Var.Name) %>%
      select(Experiment, Other, Variable, GTAPVar, Description, Unit, Value)
  }
  
  rm(list = c("one.dim.result", "one.dim.mapping"))
} else {
  message("There is no 1-dimensional data")
}

## Process Two Dimensions -------------------------------------
if (!is.null(keyvar.list[["2"]])) {
  two.dim.result <- lapply(case.name, function(case) {
    execute.two.dimens(case, keyvar.list)
  })
  names(two.dim.result) <- case.name
  
  two.dimens.list <- list()
  
  # Process BySector data
  if (any(sapply(two.dim.result, function(x) "BySector" %in% names(x)))) {
    two.dimens.list$BySector <- bind_rows(lapply(two.dim.result, 
                                                 function(x) x$BySector), .id = "Experiment") %>%
      pivot_wider(names_from = Sector, values_from = Value)
  }
  
  # Add mapping information
  two.dimens.mapping <- keyvar.list[["2"]] %>%
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  # Process all other data (non-BySector)
  if (any(sapply(two.dim.result, function(x) "Other" %in% names(x)))) {
    # Combine all other data
    two.dimens.list$ByRegion <- bind_rows(lapply(two.dim.result, 
                                                 function(x) x$Other), .id = "Experiment") %>%
      left_join(two.dimens.mapping, by = c("Variable" = "Ori.Var.Name")) %>%
      rename(GTAPVar = Variable, Variable = New.Var.Name) %>%
      select(Experiment, Region, Variable, GTAPVar, Description, Type, Value, Unit) %>%
      distinct() %>%
      arrange(Experiment, Region, Variable, Type)
  }
  
  rm(list = c("two.dim.result", "two.dimens.mapping"))
} else {
  message("There is no 2-dimensional data")
}

## Process Three Dimensions -------------------------------------
if (!is.null(keyvar.list[["3"]])) {
  three.dim.result <- lapply(case.name, function(case) {
    execute.three.dimens(case, keyvar.list)
  })
  names(three.dim.result) <- case.name
  
  # Initialize the three-dimensional list
  three.dimens.list <- list()
  
  # For CommRegReg data
  if (any(sapply(three.dim.result, function(x) "CommRegReg" %in% names(x)))) {
    three.dimens.list$CommRegReg <- bind_rows(lapply(three.dim.result, function(x) x$CommRegReg), 
                                              .id = "Experiment")
  }
  
  # For EndwActsReg data
  if (any(sapply(three.dim.result, function(x) "EndwActsReg" %in% names(x)))) {
    three.dimens.list$EndwActsReg <- bind_rows(lapply(three.dim.result, function(x) x$EndwActsReg), 
                                               .id = "Experiment")
  }
  
  # For CommActsReg data
  if (any(sapply(three.dim.result, function(x) "CommActsReg" %in% names(x)))) {
    three.dimens.list$CommActsReg <- bind_rows(lapply(three.dim.result, function(x) x$CommActsReg), 
                                               .id = "Experiment")
  }
  
  # For EndwfActsReg data
  if (any(sapply(three.dim.result, function(x) "EndwfActsReg" %in% names(x)))) {
    three.dimens.list$EndwfActsReg <- bind_rows(lapply(three.dim.result, function(x) x$EndwfActsReg), 
                                                .id = "Experiment")
  }
  
  # Filter out NULL or empty datasets
  three.dimens.list <- three.dimens.list[!sapply(three.dimens.list, function(x) is.null(x) ||
                                                   (is.data.frame(x) && nrow(x) == 0))]
  
  # Adding Description, Unit, and renaming variables
  three.dimens.mapping <- keyvar.list[["3"]] %>%
    select(Ori.Var.Name, New.Var.Name, Description, Unit)
  
  # Map descriptions and units to each dataset in the list
  three.dimens.list <- lapply(three.dimens.list, function(df) {
    df %>%
      left_join(three.dimens.mapping, 
                by = c("Variable" = "Ori.Var.Name"),
                relationship = "many-to-many") %>%
      rename(GTAPVar = Variable, 
             Variable = New.Var.Name) %>%
      select(Experiment, Variable, GTAPVar, Description, Unit, everything())
  })
  
  rm(list = c("three.dim.result", "three.dimens.mapping"))
} else {
  message("There is no 3-dimensional data")
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

## README File ------------------------------------
get_output_location <- function(var_name, dimension, dimsize) {
  # Base prefix based on dimension size
  prefix <- paste0(dimsize, "D")
  
  if (var_name == "qxs" && dimsize == 3) {
    return(paste0(prefix, "_BilateralTrade"))
  }
  
  # For other cases, construct based on dimension type
  suffix <- if (str_detect(dimension, "REG")) {
    "ByRegion"
  } else if (str_detect(dimension, "ACTS|COMM")) {
    "BySector"
  } else {
    "ByOther"
  }
  
  paste0(prefix, "_", suffix)
}

## Create Output Folders ------------------------------------
output.report <- if(nrow(keysolmap) > 0 && "DimSize" %in% names(keysolmap)) {
  # Process main variables
  main_report <- keysolmap %>%
    rowwise() %>%
    mutate(
      OutputFile = get_output_location(Ori.Var.Name, Dimension, DimSize)
    ) %>%
    select(
      Variable = Ori.Var.Name,
      OutputFile
    )
  
  # Start with main report
  result <- main_report
  
  # Add decompositions if they exist
  if(length(ev.var) > 0) {
    result <- bind_rows(
      result,
      tibble(
        Variable = "Welfare Decomposition",
        OutputFile = "All_EVDecomp"
      )
    )
  }
  
  if(length(tot.var) > 0) {
    result <- bind_rows(
      result,
      tibble(
        Variable = "Terms of Trade Decomposition",
        OutputFile = "All_ToTDecomp"
      )
    )
  }
  
  result
  
} else {
  # If no keysolmap data, just check for decompositions
  decomp_report <- tibble(
    Variable = character(),
    OutputFile = character()
  )
  
  if(length(ev.var) > 0) {
    decomp_report <- bind_rows(
      decomp_report,
      tibble(
        Variable = "Welfare Decomposition",
        OutputFile = "EVDecomp"
      )
    )
  }
  
  if(length(tot.var) > 0) {
    decomp_report <- bind_rows(
      decomp_report,
      tibble(
        Variable = "Terms of Trade Decomposition",
        OutputFile = "ToTDecomp"
      )
    )
  }
  
  decomp_report
}

## Export Functions and Displaying Results ------------------------------------
# Validate output format settings first
is_yes <- function(value) {
  tolower(trimws(value)) == "yes"
}

is_no <- function(value) {
  tolower(trimws(value)) == "no"
}

# Function to validate input is either yes or no (case insensitive)
is_valid_input <- function(value) {
  clean_value <- tolower(trimws(value))
  clean_value %in% c("yes", "no")
}

# Check for invalid values (anything that's not a variation of yes/no)
invalid_values <- character()

if (!is_valid_input(csv.output)) invalid_values <- c(invalid_values, "csv.output")
if (!is_valid_input(stata.output)) invalid_values <- c(invalid_values, "stata.output")
if (!is_valid_input(r.output)) invalid_values <- c(invalid_values, "r.output")
if (!is_valid_input(txt.output)) invalid_values <- c(invalid_values, "txt.output")

if (length(invalid_values) > 0) {
  stop(sprintf("
Please specify either 'yes' or 'no' (case insensitive) under 'Directory' section for: %s", 
               paste(invalid_values, collapse = ", ")))
}

# Function to safely check if object exists
safe_exists <- function(x) {
  exists(deparse(substitute(x)))
}

# Function to create directory if it doesn't exist
create_dir_if_missing <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Only create directories for enabled output types
if (is_yes(csv.output)) {
  csv_output_dir <- file.path(output.folder, "CSV")
  create_dir_if_missing(csv_output_dir)
}
if (is_yes(stata.output)) {
  stata_output_dir <- file.path(output.folder, "STATA")
  create_dir_if_missing(stata_output_dir)
}
if (is_yes(r.output)) {
  r_output_dir <- file.path(output.folder, "R")
  create_dir_if_missing(r_output_dir)
}
if (is_yes(txt.output)) {
  txt_output_dir <- file.path(output.folder, "TXT")
  create_dir_if_missing(txt_output_dir)
}

# Function to process CommRegReg data with qxs separation
process_comm_reg_reg <- function(data) {
  if ("Variable" %in% names(data)) {
    bilateral_trade <- data[data$Variable == "qxs", ]
    other_data <- data[data$Variable != "qxs", ]
    
    return(list(
      main = if(nrow(other_data) > 0) other_data else NULL,
      bilateral = if(nrow(bilateral_trade) > 0) bilateral_trade else NULL
    ))
  }
  return(list(main = data, bilateral = NULL))
}

# Function to create and copy README report
copy_readme_report <- function() {
  # 1. Create workbook with output.report data
  wb <- createWorkbook()
  addWorksheet(wb, "output.report")
  writeData(wb, "output.report", output.report)
  
  # 2. Save to output.folder
  saveWorkbook(wb, file.path(output.folder, "0.README.xlsx"), overwrite = TRUE)
}

# Function to safely export data based on enabled output types
safe_export_data <- function(data, base_filename) {
  # CSV export
  if (is_yes(csv.output)) {
    tryCatch({
      write.csv(data, 
                file.path(csv_output_dir, paste0(base_filename, ".csv")), 
                row.names = FALSE)
      message(sprintf("Successfully exported %s to CSV", base_filename))
    }, error = function(e) {
      message(sprintf("Error exporting %s to CSV: %s", base_filename, e$message))
    })
  }
  
  # STATA export
  if (is_yes(stata.output)) {
    tryCatch({
      write_dta(as.data.frame(data), 
                file.path(stata_output_dir, paste0(base_filename, ".dta")))
      message(sprintf("Successfully exported %s to STATA", base_filename))
    }, error = function(e) {
      message(sprintf("Error exporting %s to STATA: %s", base_filename, e$message))
    })
  }
  
  # R export
  if (is_yes(r.output)) {
    tryCatch({
      data_to_save <- data
      save(data_to_save, 
           file = file.path(r_output_dir, paste0(base_filename, ".RData")))
      message(sprintf("Successfully exported %s to RData", base_filename))
    }, error = function(e) {
      message(sprintf("Error exporting %s to RData: %s", base_filename, e$message))
    })
  }
  
  # TXT export
  if (is_yes(txt.output)) {
    tryCatch({
      # Convert to basic data.frame first
      data_df <- as.data.frame(data, stringsAsFactors = FALSE)
      
      # Write using write.table with proper formatting
      write.table(data_df, 
                  file = file.path(txt_output_dir, paste0(base_filename, ".txt")),
                  sep = "\t",  # Tab separated
                  row.names = FALSE, 
                  quote = FALSE,
                  na = "")
      
      message(sprintf("Successfully exported %s to TXT", base_filename))
    }, error = function(e) {
      message(sprintf("Error exporting %s to TXT: %s", base_filename, e$message))
    })
  }
}

# Function to safely process dimensional lists with special handling
safe_process_dim_list <- function(list_name, prefix) {
  tryCatch({
    if (exists(list_name, envir = .GlobalEnv)) {
      dim_list <- get(list_name, envir = .GlobalEnv)
      if (is.list(dim_list) && length(dim_list) > 0) {
        for (name in names(dim_list)) {
          base_filename <- paste0(prefix, "_", name)
          data <- dim_list[[name]]
          
          # Special handling for CommRegReg
          if (name == "CommRegReg") {
            processed_data <- process_comm_reg_reg(data)
            
            # Export main CommRegReg data if it exists
            if (!is.null(processed_data$main)) {
              safe_export_data(processed_data$main, base_filename)
            }
            
            # Export bilateral trade data if it exists
            if (!is.null(processed_data$bilateral)) {
              bilateral_filename <- paste0(prefix, "_BilateralTrade")
              safe_export_data(processed_data$bilateral, bilateral_filename)
            }
          } else {
            # Normal export for other cases
            safe_export_data(data, base_filename)
          }
          
          message(sprintf("Completed processing %s - %s", list_name, name))
        }
      }
    }
  }, error = function(e) {
    message(sprintf("Note: Error processing %s: %s", list_name, e$message))
  })
}

# Function to export decomposition sheets if they exist
safe_export_decomposition <- function() {
  # Handle EV decomposition
  if(length(ev.var) > 0 && exists("All_EVDecomp.sheet")) {
    safe_export_data(All_EVDecomp.sheet, "EVDecomp")
  }
  
  # Handle ToT decomposition
  if(length(tot.var) > 0 && exists("All_ToTDecomp.sheet")) {
    safe_export_data(All_ToTDecomp.sheet, "ToTDecomp")
  }
}

# Check if any output type is enabled before proceeding
if (!any(sapply(c(csv.output, stata.output, r.output, txt.output), is_yes))) {
  stop("No output types are enabled. Set at least one output type to 'yes' to 
       export data.")
} else {
  # Process dimensional lists
  dim_lists <- list(
    "one.dimens.list" = "1D",
    "two.dimens.list" = "2D",
    "three.dimens.list" = "3D"
  )
  
  # First create and copy README report to all enabled folders
  copy_readme_report()
  
  # Then process all dimensional lists
  for (list_name in names(dim_lists)) {
    safe_process_dim_list(list_name, dim_lists[[list_name]])
  }
  
  # Export decomposition sheets if they exist
  safe_export_decomposition()
  
  # Print summary of enabled output types
  message("\n=== Export Summary ===")
  
  if (is_yes(csv.output)) {
    message("✓ CSV files saved successfully in: ", csv_output_dir)
  }
  if (is_yes(stata.output)) {
    message("✓ STATA files saved successfully in: ", stata_output_dir)
  }
  if (is_yes(r.output)) {
    message("✓ R files saved successfully in: ", r_output_dir)
  }
  if (is_yes(txt.output)) {  
    message("✓ TXT files saved successfully in: ", txt_output_dir)
  }
  
  message("==================")  
}



