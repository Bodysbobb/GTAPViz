#' @title Enhanced read_SL4 with dimension preservation
#' @name read_SL4_enhanced
#' @description This function reads SL4 solution files with preserved dimension information
#' @param filename Path to SL4 file
#' @param toLowerCase Logical, whether to convert names to lower case
#' @return A list containing variables and their dimension information
#' @export
read_SL4_enhanced <- function(filename, toLowerCase = TRUE) {
  # Check if file exists
  if (!file.exists(filename)) {
    stop(sprintf("File '%s' does not exist", filename))
  }
  
  # Read the base solution first using original HARr functionality
  solution <- read_har(filename, toLowerCase = toLowerCase)
  
  if(toLowerCase) {
    names(solution) <- toupper(names(solution))
  }
  
  # Define subtotals (was missing in original code)
  subtotals <- c('TOTAL', solution$STDS)
  
  # Store dimension information
  dimension_info <- list()
  
  # Process dimension information first
  for(v in 1:length(solution$VCNM)) {
    var_name <- solution$VCNM[v]
    if(solution$VCNI[v] > 0) {
      # Get dimensions for this variable
      dim_indices <- solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] - 1)
      dimensions <- solution$VCSN[dim_indices]
      
      # Store original dimension names (similar to HARPy)
      dim_names <- solution$STNM[dimensions]
      dim_sizes <- solution$SSZ[dimensions]
      
      # Create dimension string (e.g., "COMM*ACTS*REG")
      dim_string <- paste(dim_names, collapse="*")
      
      dimension_info[[var_name]] <- list(
        dimension_string = dim_string,
        dimension_names = dim_names,
        dimension_sizes = dim_sizes
      )
    }
  }
  
  # Now read the data using existing HARr functionality
  results <- Map(function(f) {
    if(solution$VCNI[f] > 0) {
      dimensions <- solution$VCSN[solution$VCSP[f]:(solution$VCSP[f]+solution$VCNI[f]-1)]
      sizes <- c(solution$SSZ[dimensions], length(subtotals))
      labels <- c(
        Map(
          function(g) if(solution$SSZ[g]==0) c() else solution$STEL[solution$ELAD[g]:(solution$ELAD[g]+solution$SSZ[g]-1)], 
          dimensions
        ),
        list(subtotals)
      )
      names(labels) <- c(solution$STNM[dimensions], 'subtotal')
    } else {
      sizes <- c(length(subtotals))
      labels <- list(subtotals)
      names(labels) <- c('subtotal')
    }
    array(NA, dim = sizes, dimnames = labels)
  }, 1:length(solution$VCNM))
  
  names(results) <- solution$VCNM
  
  # Process values similar to original HARr code
  partials <- solution$OREX > 0 & solution$OREX != solution$VNCP
  stHeaders <- c('CUMS', unlist(Map(function(f) 
    sprintf('%sS',formatC(f, width=3, zero.print = TRUE, flag = "0")), 
    1:length(solution$STDS))))
  
  # Assign values to the endogenous variables with no partials
  for (v in which(partials == FALSE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    results[[solution$VARS[v]]][] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  # Assign zeros to the exogenous variables with no partials
  for (v in which(partials == FALSE & solution$PCUM==0)) {
    results[[solution$VARS[v]]][] <- 0
  }
  
  # Handle partials
  start <- 1
  for (v in which(partials == TRUE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    positions <- solution$ORNL[start - 1 + (1:solution$ORND[v])]
    start <- solution$ORND[v]+start
    toFill <- rep(FALSE, solution$VNCP[v])
    toFill[positions] <- TRUE
    results[[solution$VARS[v]]][toFill] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  # Create the final enhanced output structure
  enhanced_results <- list(
    data = results,
    dimension_info = dimension_info,
    metadata = list(
      filename = filename,
      creation_date = as.character(Sys.time()),
      variables = solution$VCNM
    )
  )
  
  # Add methods to access dimension information
  class(enhanced_results) <- "enhanced_sl4"
  
  return(enhanced_results)
}

#' @export
print.enhanced_sl4 <- function(x, ...) {
  cat("Enhanced SL4 Object\n")
  cat("Number of variables:", length(x$data), "\n")
  cat("Variables with dimensions:\n")
  for(var in names(x$dimension_info)) {
    cat(sprintf("  %s: %s\n", var, x$dimension_info[[var]]$dimension_string))
  }
}

#' Get dimension information for a variable
#' @export
get_dimension_info <- function(sl4_obj, variable_name) {
  if(!variable_name %in% names(sl4_obj$dimension_info)) {
    stop(sprintf("Variable '%s' not found", variable_name))
  }
  return(sl4_obj$dimension_info[[variable_name]])
}

#' Get variable data with dimension information
#' @export
get_variable_data <- function(sl4_obj, variable_name) {
  if(!variable_name %in% names(sl4_obj$data)) {
    stop(sprintf("Variable '%s' not found", variable_name))
  }
  
  data <- sl4_obj$data[[variable_name]]
  dim_info <- sl4_obj$dimension_info[[variable_name]]
  
  return(list(
    data = data,
    dimensions = dim_info
  ))
}


# Example usage:
# Read SL4 file with enhanced dimension information
sl4_data <- read_SL4_enhanced(paste0(input.folder, "/ExB14.sl4"))

# Get dimension information for a variable
var_dims <- get_dimension_info(sl4_data, "aoall")
print(var_dims$dimension_string)  # Will show something like "ACTS*REG"

# Get variable data with dimensions
var_data <- get_variable_data(sl4_data, "aoall")