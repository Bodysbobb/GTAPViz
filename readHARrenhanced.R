#' @title Enhanced read_har with dimension preservation
#' @name read_har_enhanced
#' @description Reads HAR files with preserved dimension information
#' @param filename Path to HAR file
#' @param useCoefficientsAsNames If TRUE, use coefficient names instead of headers
#' @param toLowerCase Convert names to lower case
#' @param headersToRead Optional vector of headers to read
#' @return An enhanced_har object containing data and dimension information
#' @export
read_har_enhanced <- function(filename, 
                              useCoefficientsAsNames = FALSE,
                              toLowerCase = TRUE,
                              headersToRead = NULL) {
  # Read the base HAR file using original functionality
  har_data <- read_har(filename, 
                       useCoefficientsAsNames = useCoefficientsAsNames,
                       toLowerCase = toLowerCase,
                       headersToRead = headersToRead)
  
  # Store dimension information
  dimension_info <- list()
  
  # Process headers to extract dimension information
  for (header_name in names(har_data)) {
    header_dims <- dim(har_data[[header_name]])
    if (!is.null(header_dims)) {
      dim_names <- dimnames(har_data[[header_name]])
      
      # Create dimension string (e.g., "COMM*ACTS*REG")
      if (!is.null(dim_names)) {
        dim_string <- paste(names(dim_names), collapse="*")
      } else {
        dim_string <- paste0("DIM", 1:length(header_dims), collapse="*")
      }
      
      dimension_info[[header_name]] <- list(
        dimension_string = dim_string,
        dimension_names = if(!is.null(dim_names)) names(dim_names) else paste0("DIM", 1:length(header_dims)),
        dimension_sizes = header_dims,
        dimension_elements = if(!is.null(dim_names)) dim_names else lapply(header_dims, function(x) paste0("Element", 1:x))
      )
    }
  }
  
  # Create enhanced output structure
  enhanced_results <- list(
    data = har_data,
    dimension_info = dimension_info,
    metadata = list(
      filename = filename,
      creation_date = as.character(Sys.time()),
      headers = names(har_data)
    )
  )
  
  class(enhanced_results) <- "enhanced_har"
  return(enhanced_results)
}

#' @export
print.enhanced_har <- function(x, ...) {
  cat("Enhanced HAR Object\n")
  cat("Number of headers:", length(x$data), "\n")
  cat("Headers with dimensions:\n")
  for(header in names(x$dimension_info)) {
    cat(sprintf("  %s: %s\n", header, x$dimension_info[[header]]$dimension_string))
  }
}

#' Get header summary DataFrame
#' @description Creates a summary DataFrame of headers and their dimensions
#' @param har_obj An enhanced_har object
#' @return A data.frame containing header summaries
#' @export
get_header_summary <- function(har_obj) {
  # Initialize empty lists to store data
  headers <- character(0)
  sizes <- numeric(0)
  dimensions <- character(0)
  elements <- character(0)
  
  # Loop through all headers in dimension_info
  for (header_name in names(har_obj$dimension_info)) {
    dim_info <- har_obj$dimension_info[[header_name]]
    
    headers <- c(headers, header_name)
    sizes <- c(sizes, length(dim_info$dimension_names))
    dimensions <- c(dimensions, dim_info$dimension_string)
    elements <- c(elements, paste(dim_info$dimension_sizes, collapse=" x "))
  }
  
  # Create data frame
  summary_df <- data.frame(
    Header = headers,
    Size = sizes,
    Dimensions = dimensions,
    Elements = elements,
    stringsAsFactors = FALSE
  )
  
  # Sort by Header name
  summary_df <- summary_df[order(summary_df$Header), ]
  
  return(summary_df)
}

# Example usage:
har_data <- read_har_enhanced(paste0(input.folder, "/ExB14-WEL.har"))
summary_df <- get_header_summary(har_data)
