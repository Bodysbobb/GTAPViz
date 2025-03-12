#' @title Generate Dynamic Input Mapping for GTAP Simulations
#'
#' @description
#' Creates a mapping data frame of simulation experiments to cases and periods
#' for dynamic GE model analysis. Supports multiple naming conventions and
#' period patterns.
#'
#' @param type Character. Naming pattern: "prefix" (default) or "suffix".
#'   - "prefix": Pattern comes after base/policy identifier (e.g., base2020)
#'   - "suffix": Pattern comes before base/policy identifier (e.g., 2020-base)
#' @param base Character vector or NULL. Identifiers for baseline cases.
#' @param pol Character vector or NULL. Identifiers for policy cases.
#' @param base_rerun Character vector or NULL. Identifiers for baseline rerun cases.
#' @param other Character vector or NULL. Identifiers for other case types.
#' @param pattern Character or numeric. Pattern to be combined with case identifiers:
#'   - "start:end" string: Creates sequence from start to end (e.g., "2020:2030")
#'   - c(start, end) numeric: Creates sequence from start to end (e.g., c(2020, 2030))
#'   - Character vector: Used directly as patterns
#' @param increment Numeric. Step size for numeric sequences. Default is 1.
#' @param separator Character. Separator used between pattern and case identifier in suffix mode.
#'   Default is "-".
#' @param period_pattern Logical. If TRUE, creates period ranges between consecutive patterns
#'   (e.g., 2020-2025 from 2020, 2025, 2030). Default is FALSE.
#' @param period_prefix Character. Prefix for period names. Default is "yr_".
#' @param output Character. Name of the variable to assign the result to in the caller's environment.
#'   Default is "mapping_name".
#'
#' @return
#' A data frame with the following columns:
#'   - \strong{Input}: Experiment identifier used in file names
#'   - \strong{Case}: Type of simulation (base, pol, base_rerun, other)
#'   - \strong{Period}: Time period identifier
#' Also invisibly returns the same data frame.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{auto_gtap_dynamic}}, \code{\link{.auto_GTAPRd_info}}
#'
#' @examples
#' \donttest{
#' # Example 1: Generate mapping for years 2020, 2025, 2030 with base/policy cases
#' dynamic_input_name(
#'   type = "prefix",
#'   base = "base",
#'   pol = "pol",
#'   pattern = "2020:2030:5"
#' )
#' # Creates mapping_name data frame with 6 rows:
#' # base2020, base2025, base2030, pol2020, pol2025, pol2030
#'
#' # Example 2: Generate period patterns (e.g., for comparing between periods)
#' dynamic_input_name(
#'   type = "prefix",
#'   base = "base",
#'   pol = "pol",
#'   pattern = "2020:2030:5",
#'   period_pattern = TRUE,
#'   output = "period_map"
#' )
#' # Creates period_map with periods like: yr_2020-2025, yr_2025-2030
#'
#' # Example 3: Use suffix mode with multiple identifiers
#' dynamic_input_name(
#'   type = "suffix",
#'   base = c("bau", "baseline"),
#'   pol = c("carbon", "tariff"),
#'   pattern = c("v1", "v2", "v3"),
#'   separator = "_",
#'   output = "scenario_map"
#' )
#' # Creates scenario_map with entries like: v1_bau, v2_bau, v1_carbon, etc.
#' }
#'
dynamic_input_name <- function(type = "prefix",
                                    base = NULL,
                                    pol = NULL,
                                    base_rerun = NULL,
                                    other = NULL,
                                    pattern = NULL,
                                    increment = 1,
                                    separator = "-",
                                    period_pattern = FALSE,
                                    period_prefix = "yr_",
                                    output = "Input_map") {

  # Check if at least one case type is provided
  if (is.null(base) && is.null(pol) && is.null(base_rerun) && is.null(other)) {
    stop("At least one of 'base', 'pol', 'base_rerun', or 'other' must be provided")
  }

  # Process pattern if provided
  if (!is.null(pattern)) {
    # Check if pattern is in the format "start:end"
    if (is.character(pattern) && grepl(":", pattern)) {
      pattern_parts <- strsplit(pattern, ":")[[1]]
      if (length(pattern_parts) == 2) {
        start_val <- as.numeric(pattern_parts[1])
        end_val <- as.numeric(pattern_parts[2])

        if (!is.na(start_val) && !is.na(end_val)) {
          # Create sequence based on start, end, and increment
          if (start_val <= end_val) {
            pattern_values <- seq(start_val, end_val, by = increment)
          } else {
            pattern_values <- seq(start_val, end_val, by = -increment)
          }
        } else {
          stop("Invalid pattern format. Pattern should be 'start:end' with numeric values.")
        }
      } else {
        stop("Invalid pattern format. Pattern should be 'start:end'.")
      }
    } else if (is.numeric(pattern) && length(pattern) == 2) {
      # If pattern is a numeric vector of length 2
      start_val <- pattern[1]
      end_val <- pattern[2]

      if (start_val <= end_val) {
        pattern_values <- seq(start_val, end_val, by = increment)
      } else {
        pattern_values <- seq(start_val, end_val, by = -increment)
      }
    } else {
      # Treat pattern as is (could be a character vector, numeric vector, etc.)
      pattern_values <- pattern
    }

    # Store original pattern values before potentially creating period patterns
    original_pattern_values <- pattern_values

    # Create period patterns if requested
    if (period_pattern && length(pattern_values) > 1) {
      period_values <- character(length(pattern_values) - 1)
      period_raw_values <- character(length(pattern_values) - 1)

      for (i in 1:(length(pattern_values) - 1)) {
        period_values[i] <- paste(pattern_values[i], pattern_values[i + 1], sep = separator)
        period_raw_values[i] <- period_values[i]  # Store the raw period for the Period column
      }

      pattern_values <- period_values
      period_values <- paste0(period_prefix, period_raw_values)
    } else {
      # For non-period patterns, simply add the prefix
      period_values <- paste0(period_prefix, pattern_values)
    }
  } else {
    stop("Pattern must be provided")
  }

  # Initialize data frame components
  input_values <- character(0)
  case_values <- character(0)
  period_vals <- character(0)

  # Process each type of case (base, pol, base_rerun, other)
  process_case_type <- function(case_type, case_label) {
    if (is.null(case_type)) return(NULL)

    case_values <- rep(case_label, length(pattern_values) * length(case_type))

    case_input <- character(0)
    case_periods <- character(0)

    for (prefix_val in case_type) {
      for (i in seq_along(pattern_values)) {
        if (type == "prefix") {
          # Prefix mode: add pattern after the prefix
          case_input <- c(case_input, paste0(prefix_val, pattern_values[i]))
        } else if (type == "suffix") {
          # Suffix mode: add pattern before the suffix
          case_input <- c(case_input, paste0(pattern_values[i], separator, prefix_val))
        } else {
          stop("Type must be either 'prefix' or 'suffix'")
        }

        case_periods <- c(case_periods, period_values[i])
      }
    }

    return(list(
      input = case_input,
      case = case_values,
      period = case_periods
    ))
  }

  # Process each case type
  base_result <- process_case_type(base, "base")
  pol_result <- process_case_type(pol, "pol")
  base_rerun_result <- process_case_type(base_rerun, "base_rerun")
  other_result <- process_case_type(other, "other")

  # Combine all results
  all_inputs <- c(
    if (!is.null(base_result)) base_result$input else character(0),
    if (!is.null(pol_result)) pol_result$input else character(0),
    if (!is.null(base_rerun_result)) base_rerun_result$input else character(0),
    if (!is.null(other_result)) other_result$input else character(0)
  )

  all_cases <- c(
    if (!is.null(base_result)) base_result$case else character(0),
    if (!is.null(pol_result)) pol_result$case else character(0),
    if (!is.null(base_rerun_result)) base_rerun_result$case else character(0),
    if (!is.null(other_result)) other_result$case else character(0)
  )

  all_periods <- c(
    if (!is.null(base_result)) base_result$period else character(0),
    if (!is.null(pol_result)) pol_result$period else character(0),
    if (!is.null(base_rerun_result)) base_rerun_result$period else character(0),
    if (!is.null(other_result)) other_result$period else character(0)
  )

  # Create the data frame
  result_df <- data.frame(
    Input = all_inputs,
    Case = all_cases,
    Period = all_periods,
    stringsAsFactors = FALSE
  )

  # Assign the data frame to the specified output name in the caller's environment
  assign(output, result_df, envir = parent.frame())

  return(invisible(result_df))
}



#' @title Aggregate Regions and Sectors in GTAP Data (Internal)
#'
#' @description
#' Aggregates regions, sectors, or other dimensions in GTAP data structures based on provided
#' mapping configurations. Works with data frames, lists, and nested structures.
#'
#' @param data_list A data frame or list containing GTAP data to be aggregated.
#' @param agg_mapping A list containing aggregation mappings with dimensions as top level
#'        and group names as second level.
#' @param calculation Character. Operation for aggregation:
#'        - "+" (default): Sum values in the same group
#'        - "-": Subtract subsequent values from the first
#'        - "*": Multiply values within groups
#'        - "/": Divide subsequent values from the first
#' @param add_world Logical. If TRUE, adds a "World" aggregate for regional dimensions.
#'
#' @return The input data structure with aggregated values added.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{auto_gtap_dynamic}}, \code{\link{.create_agg_mapping}}
#'
.auto_GTAPRd_info <- function(data_list, external_map = NULL) {
  if (is.null(external_map)) {
    warning("external_map must be provided")
    return(data_list)
  }

  # Check required columns in external_map
  if (!all(c("Input", "Case", "Period") %in% names(external_map))) {
    warning("external_map must contain 'Input', 'Case', and 'Period' columns")
    return(data_list)
  }

  map_dataframe <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 || !"Experiment" %in% names(df)) {
      return(df)
    }

    # Find which rows in df$Experiment match with external_map$Input
    for (i in 1:nrow(external_map)) {
      input_val <- external_map$Input[i]
      case_val <- external_map$Case[i]
      period_val <- external_map$Period[i]

      # Find matching rows and directly update them
      matching_rows <- df$Experiment == input_val
      if (any(matching_rows)) {
        df$Case[matching_rows] <- case_val
        df$Period[matching_rows] <- period_val
      }
    }

    return(df)
  }

  # First ensure Case and Period columns exist
  modify_structure <- function(df) {
    if (is.data.frame(df) && "Experiment" %in% names(df)) {
      if (!"Case" %in% names(df)) df$Case <- NA_character_
      if (!"Period" %in% names(df)) df$Period <- NA_character_
    }
    return(df)
  }

  # First add necessary columns
  data_list <- .apply_to_dataframes(data_list, modify_structure)

  # Then do the mapping
  return(.apply_to_dataframes(data_list, map_dataframe))
}


#' @title Calculate Deviations Between Policy and Base Scenarios (Internal)
#'
#' @description
#' Computes deviations between policy and baseline scenarios in GTAP data.
#' Supports different calculation methods for analysis of policy impacts.
#'
#' @param data_list A data frame or list containing GTAP data with Case column.
#' @param base Character. The identifier for baseline scenario in the Case column.
#'        Default is "base".
#' @param policy Character. The identifier for policy scenario in the Case column.
#'        Default is "pol".
#' @param calculation Character. Operation for calculating deviations:
#'        - "-" (default): Simple difference (policy minus base)
#'        - "+": Sum of policy and base
#'        - "*": Product of policy and base
#'        - "/": Ratio of policy to base
#'        - "%": Percentage change calculation ((policy-base)/base*100)
#'
#' @return The input data structure with deviation values added as a new case.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{auto_gtap_dynamic}}, \code{\link{.auto_GTAPRd_info}}
#'
.gtap_rd_dev <- function(data_list, base = "base", policy = "pol", calculation = "-") {
  process_df <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 || !"Case" %in% names(df)) {
      return(df)
    }

    if (!all(c(base, policy) %in% df$Case)) {
      return(df)
    }

    val_col <- names(df)[sapply(df, is.numeric)][1]
    if (is.null(val_col)) return(df)

    cols_to_drop <- c("Experiment", "Dimension")
    cols_to_drop <- cols_to_drop[cols_to_drop %in% names(df)]
    if (length(cols_to_drop) > 0) {
      df <- dplyr::select(df, -dplyr::all_of(cols_to_drop))
    }

    df_wide <- tidyr::pivot_wider(df,
                                  names_from = "Case",
                                  values_from = dplyr::all_of(val_col))

    if (all(c(base, policy) %in% names(df_wide))) {
      if (calculation == "-") {
        df_wide$dev <- df_wide[[policy]] - df_wide[[base]]
      } else if (calculation == "+") {
        df_wide$dev <- df_wide[[policy]] + df_wide[[base]]
      } else if (calculation == "*") {
        df_wide$dev <- df_wide[[policy]] * df_wide[[base]]
      } else if (calculation == "/") {
        df_wide$dev <- df_wide[[policy]] / df_wide[[base]]
      } else if (calculation == "%") {
        df_wide$dev <- (df_wide[[policy]] - df_wide[[base]]) / df_wide[[base]] * 100
      }

      case_cols <- c(base, policy, "dev")
      df_long <- tidyr::pivot_longer(df_wide,
                                     cols = dplyr::all_of(case_cols),
                                     names_to = "Case",
                                     values_to = val_col)

      df_long <- dplyr::arrange(df_long, .data$Case)
      return(df_long)
    }

    return(df)
  }

  if (is.data.frame(data_list)) {
    return(process_df(data_list))
  }

  if (is.list(data_list)) {
    result <- lapply(data_list, function(item) {
      if (is.data.frame(item)) {
        return(process_df(item))
      } else if (is.list(item)) {
        return(.gtap_rd_dev(item, base, policy, calculation))
      } else {
        return(item)
      }
    })

    names(result) <- names(data_list)
    attributes(result) <- attributes(data_list)

    return(result)
  }

  return(data_list)
}


#' @title Create Aggregation Mapping Structure (Internal)
#'
#' @description
#' Creates a hierarchical mapping structure for use in region/sector aggregation
#' from a data frame containing mapping information.
#'
#' @param mapping_file A data frame or path to a file containing mapping information.
#'        Must have columns for dimension, item names, and group names.
#' @param dimension_col Character. Name of column containing dimension information.
#'        Default is "Dimension".
#' @param item_col Character. Name of column containing original GTAP names.
#'        Default is "GTAPName".
#' @param group_col Character. Name of column containing aggregation group names.
#'        Default is "Aggregate".
#'
#' @return A nested list where the top level is dimension names, second level is
#'         group names, and values are character vectors of items in each group.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{auto_gtap_dynamic}}, \code{\link{.gtap_rd_agg}}
#'
.create_agg_mapping <- function(mapping_file, dimension_col = "Dimension",
                                item_col = "GTAPName", group_col = "Aggregate") {

  # Check if mapping_file is a data frame
  if (is.character(mapping_file)) {
    # If it's a file path, try to read it
    if (file.exists(mapping_file)) {
      if (grepl("\\.xlsx$|\\.xls$", mapping_file)) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("The readxl package is required to read Excel files")
        }
        mapping_data <- readxl::read_excel(mapping_file)
      } else if (grepl("\\.csv$", mapping_file)) {
        mapping_data <- read.csv(mapping_file, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Please provide an Excel or CSV file.")
      }
    } else {
      stop("File not found: ", mapping_file)
    }
  } else if (is.data.frame(mapping_file)) {
    mapping_data <- mapping_file
  } else {
    stop("mapping_file must be a data frame or a path to a CSV/Excel file")
  }

  # Check required columns
  required_cols <- c(dimension_col, item_col, group_col)
  if (!all(required_cols %in% names(mapping_data))) {
    missing_cols <- required_cols[!required_cols %in% names(mapping_data)]
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Split the data by dimension
  dimensions <- unique(mapping_data[[dimension_col]])
  result <- list()

  for (dim in dimensions) {
    # Filter data for this dimension
    dim_data <- mapping_data[mapping_data[[dimension_col]] == dim, ]

    # Get unique groups
    groups <- unique(dim_data[[group_col]])

    # Create a list for this dimension
    dim_list <- list()

    for (grp in groups) {
      # Get items for this group
      items <- dim_data[dim_data[[group_col]] == grp, item_col]

      # Add to dimension list
      dim_list[[grp]] <- items
    }

    # Add dimension list to result
    result[[dim]] <- dim_list
  }

  return(result)
}



#' @title Aggregate Regions and Sectors in GTAP Data (Internal)
#'
#' @description
#' Aggregates regions, sectors, or other dimensions in GTAP data structures based on provided
#' mapping configurations. Works with data frames, lists, and nested structures.
#'
#' @param data_list A data frame or list containing GTAP data to be aggregated.
#' @param agg_mapping A list containing aggregation mappings with dimensions as top level
#'        and group names as second level.
#' @param calculation Character. Operation for aggregation:
#'        - "+" (default): Sum values in the same group
#'        - "-": Subtract subsequent values from the first
#'        - "*": Multiply values within groups
#'        - "/": Divide subsequent values from the first
#' @param add_world Logical. If TRUE, adds a "World" aggregate for regional dimensions.
#'
#' @return The input data structure with aggregated values added.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{auto_gtap_dynamic}}, \code{\link{.create_agg_mapping}}
#'
.gtap_rd_agg <- function(data_list, agg_mapping, calculation = "+", add_world = TRUE) {
  # Process a single dataframe with a specific pivot column
  process_df_with_pivot <- function(df, pivot_col, group_mappings) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(df)
    }

    # Find the pivot column with case-insensitive matching
    actual_pivot_col <- NULL
    for (col in names(df)) {
      if (tolower(col) == tolower(pivot_col)) {
        actual_pivot_col <- col
        break
      }
    }

    # Skip if pivot column not found
    if (is.null(actual_pivot_col)) {
      return(df)
    }

    # Remember original column name for later
    original_pivot_col_name <- actual_pivot_col

    val_col <- names(df)[sapply(df, is.numeric)][1]
    if (is.null(val_col)) return(df)

    # Get all columns except the pivot column and value column
    id_cols <- setdiff(names(df), c(actual_pivot_col, val_col))

    # Create wide format
    df_wide <- tidyr::pivot_wider(df,
                                  id_cols = dplyr::all_of(id_cols),
                                  names_from = dplyr::all_of(actual_pivot_col),
                                  values_from = dplyr::all_of(val_col))

    # Get the original columns (those created during pivot_wider)
    orig_cols <- setdiff(names(df_wide), id_cols)

    # Add World aggregation automatically if pivot_col is a region/country column and add_world is TRUE
    if (add_world) {
      region_keywords <- c("reg", "region", "regions", "country", "countries")
      is_region_col <- any(sapply(region_keywords, function(kw) grepl(kw, tolower(actual_pivot_col))))

      if (is_region_col && !("World" %in% names(group_mappings))) {
        # Add World calculation (sum of all regions)
        df_wide$World <- rowSums(df_wide[, orig_cols, drop = FALSE], na.rm = TRUE)
      }
    }

    # Process each group in the mapping
    for (group_name in names(group_mappings)) {
      # Extract items from the tibble
      items_tibble <- group_mappings[[group_name]]
      if (!is.null(items_tibble) && "GTAPName" %in% names(items_tibble)) {
        items <- items_tibble$GTAPName

        if (length(items) > 0) {
          # Find matching columns
          matched_cols <- character(0)
          for (item in items) {
            # Try exact match first
            if (item %in% orig_cols) {
              matched_cols <- c(matched_cols, item)
            } else {
              # Try case-insensitive match
              for (col in orig_cols) {
                if (tolower(item) == tolower(col)) {
                  matched_cols <- c(matched_cols, col)
                  break
                }
              }
            }
          }

          if (length(matched_cols) > 0) {
            # Calculate aggregate value based on calculation type
            if (calculation == "+") {
              df_wide[[group_name]] <- rowSums(df_wide[, matched_cols, drop = FALSE], na.rm = TRUE)
            } else if (calculation == "-") {
              df_wide[[group_name]] <- df_wide[[matched_cols[1]]]
              for (i in 2:length(matched_cols)) {
                df_wide[[group_name]] <- df_wide[[group_name]] - df_wide[[matched_cols[i]]]
              }
            } else if (calculation == "*") {
              df_wide[[group_name]] <- 1
              for (col in matched_cols) {
                df_wide[[group_name]] <- df_wide[[group_name]] * df_wide[[col]]
              }
            } else if (calculation == "/") {
              df_wide[[group_name]] <- df_wide[[matched_cols[1]]]
              for (i in 2:length(matched_cols)) {
                df_wide[[group_name]] <- df_wide[[group_name]] / df_wide[[matched_cols[i]]]
              }
            }
          }
        }
      }
    }

    # Get new aggregation columns
    agg_cols <- setdiff(names(df_wide), c(id_cols, orig_cols))
    all_value_cols <- c(orig_cols, agg_cols)

    # Convert back to long format
    df_long <- tidyr::pivot_longer(
      df_wide,
      cols = dplyr::all_of(all_value_cols),
      names_to = original_pivot_col_name,
      values_to = val_col
    )

    return(df_long)
  }

  # Process a single dataframe with all pivot columns in mapping
  process_df <- function(df) {
    result <- df

    # Process each pivot column in the mapping
    for (pivot_col in names(agg_mapping)) {
      group_mappings <- agg_mapping[[pivot_col]]
      result <- process_df_with_pivot(result, pivot_col, group_mappings)
    }

    return(result)
  }

  # Handle different input types
  if (is.data.frame(data_list)) {
    return(process_df(data_list))
  }

  if (is.list(data_list)) {
    result <- lapply(data_list, function(item) {
      if (is.data.frame(item)) {
        return(process_df(item))
      } else if (is.list(item)) {
        return(.gtap_rd_agg(item, agg_mapping, calculation, add_world))
      } else {
        return(item)
      }
    })

    names(result) <- names(data_list)
    attributes(result) <- attributes(data_list)

    return(result)
  }

  return(data_list)
}



#' @title Process GTAP Dynamic Data with Extended Analysis Options
#'
#' @description
#' Processes GTAP data from SL4 and HAR files with comprehensive support for dynamic analysis,
#' regional aggregation, and deviation calculations across experiments.
#'
#' @details
#' This function extends the base GTAP data processing workflow with additional capabilities
#' for dynamic GE model analysis. It processes SL4 and HAR files, performs regional aggregation,
#' calculates deviations between policy and baseline scenarios, and provides integrated mapping
#' for periods and experiments.
#'
#' The function supports multiple options for aggregation and deviation calculations:
#' \itemize{
#'   \item \strong{Mapping}: Apply experiment-to-case mapping for dynamic analysis periods
#'   \item \strong{Aggregation}: Create custom regional or sectoral groupings
#'   \item \strong{Deviation}: Calculate differences between policy and baseline scenarios
#' }
#'
#' @param experiment Character vector. Case names to process.
#' @param project_path Character. Path to the project folder with "in" and "out" subfolders.
#' @param input_path Character. Path to the input folder. Overrides `project_path/in` if specified.
#' @param output_path Character. Path to the output folder. Overrides `project_path/out` if specified.
#' @param sl4_suffix Character. Custom suffix for SL4 files (e.g., "" or "-custom").
#' @param har_suffix Character. Custom suffix for HAR files (e.g., "-WEL").
#' @param mapping_info Character. Mapping mode: "GTAPv7" (default), "Yes", "No", or "Mix".
#' @param process_sl4_vars Data frame, NULL, or FALSE. Variables to extract from SL4 files.
#'   - Set to NULL to extract all variables.
#'   - Set to FALSE to skip SL4 processing.
#' @param process_har_vars Data frame, NULL, or FALSE. Variables to extract from HAR files.
#'   - Set to NULL to extract all variables.
#'   - Set to FALSE to skip HAR processing.
#' @param sl4_mapping_info Data frame or NULL. Mapping information for SL4 variables (with "Variable", "Description", and "Unit" columns).
#' @param har_mapping_info Data frame or NULL. Mapping information for HAR variables (with "Variable", "Description", and "Unit" columns).
#' @param sl4_extract_method Character. SL4 extraction method. Options: "get_data_by_dims", "get_data_by_var", or "group_data_by_dims".
#' @param har_extract_method Character. HAR extraction method. Options: "get_data_by_dims", "get_data_by_var", or "group_data_by_dims".
#' @param sl4_priority Optional list. Priority rules for SL4 data grouping.
#' @param har_priority Optional list. Priority rules for HAR data grouping.
#' @param region_select Optional character vector. Specifies regions to filter the data.
#' @param sector_select Optional character vector. Specifies sectors to filter the data.
#' @param subtotal_level Logical. If TRUE, includes subtotal data. Default is FALSE.
#' @param plot_data Logical. If TRUE, prepares data for plotting and assigns to variables.
#' @param output_formats Character vector or list. Exports data in these formats (valid: "csv", "stata", "rds", "txt").
#' @param process_macro Logical. If TRUE, processes macro variables from SL4 files.
#' @param sl4_output_name Character. Variable name for SL4 plotting data if generating plot data. Default is "sl4.plot.data".
#' @param har_output_name Character. Variable name for HAR plotting data if generating plot data. Default is "har.plot.data".
#' @param macro_output_name Character. Variable name for GTAP macro data if generating plot data. Default is "GTAPMacro".
#' @param mapping_input Data frame. Maps experiment IDs to cases and periods. Must contain columns: "Input", "Case", and "Period".
#' @param agg_mapping List. Mapping for regional or sectoral aggregation.
#' @param aggregate Logical. If TRUE, performs regional/sectoral aggregation using `agg_mapping`.
#' @param add_world Logical. If TRUE, adds "World" totals when aggregating regions.
#' @param cal_deviation Logical. If TRUE, calculates deviations between policy and baseline scenarios.
#' @param base_var Character. Case identifier for baseline scenario. Default is "base".
#' @param policy_var Character. Case identifier for policy scenario. Default is "pol".
#' @param calculation_agg Character. Operation for aggregation: "+", "-", "*", or "/". Default is "+".
#' @param calculation_dev Character. Operation for deviation calculation: "-", "+", "*", "/" or "%". Default is "-".
#'
#' @return
#' A list containing the processed datasets with enhanced information for dynamic analysis.
#'
#' @author Pattawee Puangchit
#' @export
#' @seealso \code{\link{add_mapping_info}}, \code{\link{dynamic_input_name}}
#'
#' @examples
#' \donttest{
#' # Example 1: Basic processing with default settings
#' results <- auto_gtap_dynamic(
#'   experiment = c("base2020", "base2025", "pol2020", "pol2025"),
#'   project_path = "path/to/project",
#'   plot_data = TRUE
#' )
#'
#' # Example 2: Process with mapping and aggregation
#' # Create mapping for periods and cases
#' dynamic_input_name(
#'   type = "prefix",
#'   base = "base",
#'   pol = "pol",
#'   pattern = "2020:2030:5",
#'   period_pattern = TRUE,
#'   output = "run_map"
#' )
#'
#' # Define regional aggregation
#' reg_mapping <- .create_agg_mapping(
#'   data.frame(
#'     Dimension = rep("REG", 3),
#'     GTAPName = c("USA", "CAN", "MEX"),
#'     Aggregate = rep("NAmerica", 3)
#'   )
#' )
#'
#' # Process data with aggregation and deviation calculation
#' results <- auto_gtap_dynamic(
#'   experiment = c("base2020", "base2025", "pol2020", "pol2025"),
#'   project_path = "path/to/project",
#'   mapping_input = run_map,
#'   agg_mapping = reg_mapping,
#'   aggregate = TRUE,
#'   cal_deviation = TRUE,
#'   plot_data = TRUE
#' )
#'
#' # Example 3: Custom calculation methods
#' results <- auto_gtap_dynamic(
#'   experiment = c("base2020", "base2025", "pol2020", "pol2025"),
#'   project_path = "path/to/project",
#'   mapping_input = run_map,
#'   agg_mapping = reg_mapping,
#'   aggregate = TRUE,
#'   cal_deviation = TRUE,
#'   calculation_agg = "*",
#'   calculation_dev = "%",  # Percentage change calculation
#'   plot_data = TRUE
#' )
#' }
auto_gtap_dynamic <- function(experiment,
                              project_path = NULL, input_path = NULL, output_path = NULL,
                              sl4_suffix = "", har_suffix = "",
                              mapping_info = "GTAPv7",
                              process_sl4_vars = NULL, process_har_vars = NULL,
                              sl4_mapping_info = NULL, har_mapping_info = NULL,
                              sl4_extract_method = "get_data_by_var", har_extract_method = "get_data_by_var",
                              sl4_priority = NULL, har_priority = NULL,
                              region_select = NULL, sector_select = NULL, subtotal_level = FALSE,
                              plot_data = FALSE, output_formats = NULL,
                              process_macro = TRUE,
                              sl4_output_name = "sl4.plot.data",
                              har_output_name = "har.plot.data",
                              macro_output_name = "GTAPMacro",
                              mapping_input = NULL,
                              agg_mapping = NULL,
                              aggregate = FALSE,
                              add_world = TRUE,
                              cal_deviation = TRUE,
                              base_var = "base",
                              policy_var = "pol",
                              calculation_agg = "+",
                              calculation_dev = "-") {

  # Initial Setup--------------------------------------------------------------
  export_formats <- .output_format(output_formats)
  export_data <- length(export_formats) > 0
  process_log <- list()

  if (!is.null(project_path)) {
    if (is.null(input_path)) input_path <- file.path(project_path, "in")
    if (is.null(output_path)) output_path <- file.path(project_path, "out")
  }

  all_data <- list()

  # Extract Variable Names-----------------------------------------------------
  extract_var_names <- function(var_def) {
    if (is.data.frame(var_def) && "Variable" %in% names(var_def)) {
      return(var_def$Variable)
    } else {
      return(var_def)
    }
  }

  sl4var_vars <- extract_var_names(process_sl4_vars)
  harvar_vars <- extract_var_names(process_har_vars)

  process_sl4 <- !identical(process_sl4_vars, FALSE)
  process_har <- !identical(process_har_vars, FALSE)
  process_qxs <- process_sl4 && (is.null(process_sl4_vars) ||
                                   any(grepl("qxs", sl4var_vars, ignore.case = TRUE)))

  # Filtering QXS out of regular SL4 vars
  if (process_qxs && is.character(sl4var_vars) && !is.null(sl4var_vars)) {
    qxs_vars <- sl4var_vars[grepl("qxs", sl4var_vars, ignore.case = TRUE)]
    sl4var_vars <- sl4var_vars[!grepl("qxs", sl4var_vars, ignore.case = TRUE)]
    if (length(sl4var_vars) == 0) {
      sl4var_vars <- NULL
    }
  }

  # Define File Suffixes-------------------------------------------------------
  sl4_file_suffix <- if (nzchar(sl4_suffix)) paste0(sl4_suffix, ".sl4") else ".sl4"
  har_file_suffix <- if (nzchar(har_suffix)) paste0(har_suffix, ".har") else ".har"

  # Validate Inputs & Files-----------------------------------------------------
  validation_result <- .validate_gtap_files(
    input_dir = input_path,
    output_dir = output_path,
    experiment = experiment,
    mapping_info = mapping_info,
    sl4var = process_sl4,
    harvar = process_har,
    sl4map = sl4_mapping_info,
    harmap = har_mapping_info,
    output_formats = if(export_data) output_formats else NULL,
    plot_data = plot_data,
    sl4_file_suffix = sl4_file_suffix,
    har_file_suffix = har_file_suffix
  )

  cat(paste(validation_result$messages, collapse = "\n"), "\n")

  if (!validation_result$proceed) {
    stop("Process stopped due to validation errors.")
  }

  # Identify Available Experiment Files-----------------------------------------
  files <- list.files(input_path, full.names = FALSE, ignore.case = TRUE)

  # Simple file pattern matching without excessive messages
  find_valid_cases <- function(file_suffix, experiments) {
    pattern_str <- paste0(file_suffix, "$")
    files_matching <- files[grepl(pattern_str, files, ignore.case = TRUE)]

    bases <- character(0)
    for (file in files_matching) {
      base <- substr(file, 1, nchar(file) - nchar(file_suffix))
      bases <- c(bases, tolower(trimws(base)))
    }

    valid_cases <- experiments[tolower(experiments) %in% bases]
    return(valid_cases)
  }

  valid_sl4_cases <- find_valid_cases(sl4_file_suffix, experiment)
  valid_har_cases <- find_valid_cases(har_file_suffix, experiment)

  # Process Data Function-----------------------------------
  transform_data <- function(data, external_map, apply_filters = TRUE) {
    if (is.null(data)) return(NULL)

    data <- rename_GTAP_bilateral(data)
    data <- add_mapping_info(data, mapping = mapping_info, external_map = external_map)

    if (apply_filters && (!is.null(region_select) || !is.null(sector_select))) {
      data <- .apply_filters(
        data,
        region_select = region_select,
        experiment_select = experiment,
        sector_select = sector_select
      )
    }

    if (length(data) == 1 && is.list(data) && !is.data.frame(data)) {
      data <- data[[1]]
    }

    return(data)
  }

  process_data <- function(valid_cases, file_suffix, select_vars, method_name,
                           load_func, list_name, priority_list, data_name) {
    if (length(valid_cases) == 0) return(NULL)

    data_raw <- setNames(
      lapply(valid_cases, function(scenario) {
        file_path <- file.path(input_path, paste0(scenario, file_suffix))
        if (file.exists(file_path)) {
          tryCatch({
            load_func(file_path, select_header = select_vars)
          }, error = function(e) {
            message(sprintf("Error processing %s: %s", file_path, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s (file not found)", file_path))
          return(NULL)
        }
      }),
      valid_cases
    )

    data_raw <- data_raw[!sapply(data_raw, is.null)]
    if (length(data_raw) == 0) return(NULL)

    method_map <- list(
      "get_data_by_dims" = HARplus::get_data_by_dims,
      "get_data_by_var" = HARplus::get_data_by_var,
      "group_data_by_dims" = HARplus::group_data_by_dims
    )

    if (!is.character(method_name) || !method_name %in% names(method_map)) {
      stop(paste0("Invalid method provided. Choose from: '",
                  paste(names(method_map), collapse = "', '"), "'."))
    }

    keep_unique_flag <- length(data_raw) > 1

    if (method_name == "group_data_by_dims") {
      params <- list(
        experiment_names = names(data_raw),
        auto_rename = TRUE,
        priority = priority_list %||% list("Sector" = c("COMM", "ACTS"), "Region" = c("REG")),
        subtotal_level = subtotal_level
      )
    } else {
      params <- list(
        experiment_names = names(data_raw),
        subtotal_level = subtotal_level,
        merge_data = keep_unique_flag
      )
    }

    grouped_data <- tryCatch({
      do.call(method_map[[method_name]], c(params, data_raw))
    }, error = function(e) {
      message(sprintf("Error in grouping data: %s", e$message))
      return(NULL)
    })

    if (!keep_unique_flag && is.list(grouped_data) && length(grouped_data) == 1 && !is.data.frame(grouped_data)) {
      grouped_data <- grouped_data[[1]]
    }

    if (plot_data && !is.null(list_name) && !is.null(grouped_data)) {
      raw_data <- grouped_data
    }
    return(grouped_data)
  }

  # Process Macro Data----------------------------------------------------------
  if (process_macro && length(valid_sl4_cases) > 0) {
    message("Processing GTAP Macro Data")
    macro_data <- tryCatch({
      # Use the proper macro data function with custom suffix
      macro_raw <- setNames(
        lapply(valid_sl4_cases, function(scenario) {
          sl4_path <- file.path(input_path, paste0(scenario, sl4_file_suffix))
          if (file.exists(sl4_path)) {
            tryCatch({
              HARplus::load_sl4x(sl4_path, select_header = macro_info$Variable)
            }, error = function(e) {
              message(sprintf("Error processing %s: %s", sl4_path, e$message))
              return(NULL)
            })
          } else {
            message(sprintf("Skipping %s (file not found)", sl4_path))
            return(NULL)
          }
        }),
        valid_sl4_cases
      )

      macro_raw <- macro_raw[!sapply(macro_raw, is.null)]

      # Process macro data with consistent merge logic
      GTAPMacros <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            experiment_names = names(macro_raw),
            subtotal_level = subtotal_level,
            merge_data = length(macro_raw) > 1
          ),
          macro_raw
        )
      )

      # Add mapping info
      GTAPMacros <- add_mapping_info(GTAPMacros, mapping = "GTAPv7")

      # Filter columns
      GTAPMacros_filtered <- .apply_to_dataframes(GTAPMacros, function(df) {
        df[, c("Variable", "Value", "Subtotal", "Experiment", "Description", "Unit"), drop = FALSE]
      })

      # Simplify structure for single experiment
      if (length(valid_sl4_cases) == 1) {
        if (is.list(GTAPMacros_filtered) && length(GTAPMacros_filtered) == 1 && !is.data.frame(GTAPMacros_filtered)) {
          GTAPMacros_final <- do.call(rbind, unlist(GTAPMacros_filtered, recursive = FALSE))
        } else {
          GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
        }
      } else {
        GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
      }
      rownames(GTAPMacros_final) <- NULL

      GTAPMacros_final <- GTAPMacros_final[order(GTAPMacros_final$Experiment,
                                                 GTAPMacros_final$Variable,
                                                 GTAPMacros_final$Unit), ]

      rename_GTAP_bilateral(GTAPMacros_final)
    }, error = function(e) {
      process_log$macro <- sprintf("Error processing GTAP Macro Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(macro_data)) {
      process_log$macro <- "GTAP Macro Data processed successfully"
      all_data$GTAPMacros <- macro_data

      # Apply dynamic processing
      if (!is.null(mapping_input)) {
        macro_data <- .auto_GTAPRd_info(macro_data, external_map = mapping_input)
      }

      if (aggregate && !is.null(agg_mapping)) {
        agg_mapping <- .create_agg_mapping(agg_mapping)
        macro_data <- .gtap_rd_agg(macro_data, agg_mapping, calculation = calculation_agg, add_world = add_world)
      }

      if (cal_deviation) {
        macro_data <- .gtap_rd_dev(macro_data, base = base_var, policy = policy_var, calculation = calculation_dev)
      }

      all_data$GTAPMacros <- macro_data

      # Fixed issue: Assign to parent environment if plot_data is TRUE
      if (plot_data) {
        assign(macro_output_name, macro_data, envir = parent.frame())
      }

      # Export the macro data
      if (export_data && !is.null(output_path)) {
        message("Exporting GTAP Macro Data...")
        export_list <- if (is.list(macro_data) && !is.data.frame(macro_data) && length(names(macro_data)) > 0) {
          macro_data
        } else {
          setNames(list(macro_data), "GTAPMacros")
        }

        HARplus::export_data(
          data = export_list,
          output_path = output_path,
          format = export_formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = FALSE
        )

        message("GTAP Macro Data exported to:", output_path)
      }
    }
  }

  # Process SL4 Data------------------------------------------------------------
  if (process_sl4 && length(valid_sl4_cases) > 0) {
    message("Processing SL4 Data")
    process_regular_sl4 <- TRUE

    if (process_regular_sl4) {
      grouped_sl4 <- tryCatch({
        process_data(valid_sl4_cases, sl4_file_suffix, sl4var_vars, sl4_extract_method,
                     HARplus::load_sl4x, sl4_output_name, sl4_priority, "SL4")
      }, error = function(e) {
        process_log$sl4 <- sprintf("Error processing SL4 Data: %s", e$message)
        return(NULL)
      })

      if (!is.null(grouped_sl4)) {
        process_log$sl4 <- "SL4 Data processed successfully"
        grouped_sl4 <- transform_data(grouped_sl4, sl4_mapping_info)

        # Apply dynamic processing
        if (!is.null(mapping_input)) {
          grouped_sl4 <- .auto_GTAPRd_info(grouped_sl4, external_map = mapping_input)
        }

        if (aggregate && !is.null(agg_mapping)) {
          agg_mapping <- .create_agg_mapping(agg_mapping)
          grouped_sl4 <- .gtap_rd_agg(grouped_sl4, agg_mapping, calculation = calculation_agg, add_world = add_world)
        }

        if (cal_deviation) {
          grouped_sl4 <- .gtap_rd_dev(grouped_sl4, base = base_var, policy = policy_var, calculation = calculation_dev)
        }

        if (plot_data && !is.null(sl4_output_name)) {
          assign(sl4_output_name, grouped_sl4, envir = parent.frame())
        }
        all_data$sl4_data <- grouped_sl4

        # Export processed SL4 data
        if (export_data && !is.null(output_path)) {
          message("Exporting SL4 Data...")
          export_list <- if (is.list(grouped_sl4) && !is.data.frame(grouped_sl4) && length(names(grouped_sl4)) > 0) {
            grouped_sl4
          } else {
            setNames(list(grouped_sl4), "SL4")
          }

          HARplus::export_data(
            data = export_list,
            output_path = output_path,
            format = export_formats,
            create_subfolder = TRUE,
            multi_sheet_xlsx = TRUE,
            report_output = FALSE
          )

          message("SL4 Data exported to:", output_path)
        }
      }
    }
  }


  # Process Bilateral Trade -------------------------------------------------
  if (process_qxs && length(valid_sl4_cases) > 0) {
    message("Processing QXS Bilateral Trade Data")

    bilateral_data <- tryCatch({
      process_data(valid_sl4_cases, sl4_file_suffix, "qxs", "get_data_by_var",
                   HARplus::load_sl4x, "bilateral_data", NULL, "QXS")
    }, error = function(e) {
      process_log$qxs <- sprintf("Error processing QXS Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(bilateral_data)) {
      process_log$qxs <- "QXS Bilateral Data processed successfully"
      bilateral_data <- transform_data(bilateral_data, sl4_mapping_info)

      # Apply dynamic processing
      if (!is.null(mapping_input)) {
        bilateral_data <- .auto_GTAPRd_info(bilateral_data, external_map = mapping_input)
      }

      if (aggregate && !is.null(agg_mapping)) {
        agg_mapping <- .create_agg_mapping(agg_mapping)
        bilateral_data <- .gtap_rd_agg(bilateral_data, agg_mapping, calculation = calculation_agg, add_world = add_world)
      }

      if (cal_deviation) {
        bilateral_data <- .gtap_rd_dev(bilateral_data, base = base_var, policy = policy_var, calculation = calculation_dev)
      }

      if (plot_data) {
        assign("bilateral_data", bilateral_data, envir = parent.frame())
      }
      all_data$bilateral_data <- bilateral_data

      # Export bilateral data
      if (export_data && !is.null(output_path)) {
        message("Exporting Bilateral Trade Data...")
        export_list <- if (is.list(bilateral_data) && !is.data.frame(bilateral_data) && length(names(bilateral_data)) > 0) {
          bilateral_data
        } else {
          setNames(list(bilateral_data), "BilateralTrade")
        }

        HARplus::export_data(
          data = export_list,
          output_path = output_path,
          format = export_formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = FALSE
        )

        message("Bilateral Trade Data exported to:", output_path)
      }
    }
  }


  # Process HAR Data------------------------------------------------------------
  if (process_har && length(valid_har_cases) > 0) {
    message("Processing HAR Data")
    har_data <- tryCatch({
      process_data(valid_har_cases, har_file_suffix, harvar_vars, har_extract_method,
                   HARplus::load_harx, har_output_name, har_priority, "HAR")
    }, error = function(e) {
      process_log$har <- sprintf("Error processing HAR Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(har_data)) {
      process_log$har <- "HAR Data processed successfully"
      har_data <- transform_data(har_data, har_mapping_info)

      # Apply dynamic processing
      if (!is.null(mapping_input)) {
        har_data <- .auto_GTAPRd_info(har_data, external_map = mapping_input)
      }

      if (aggregate && !is.null(agg_mapping)) {
        agg_mapping <- .create_agg_mapping(agg_mapping)
        har_data <- .gtap_rd_agg(har_data, agg_mapping, calculation = calculation_agg, add_world = add_world)
      }

      if (cal_deviation) {
        har_data <- .gtap_rd_dev(har_data, base = base_var, policy = policy_var, calculation = calculation_dev)
      }

      if (plot_data && !is.null(har_output_name)) {
        assign(har_output_name, har_data, envir = parent.frame())
      }
      all_data$decomposition_data <- har_data

      # Export processed HAR data
      if (export_data && !is.null(output_path)) {
        message("Exporting HAR Decomposition Data...")
        export_list <- if (is.list(har_data) && !is.data.frame(har_data) && length(names(har_data)) > 0) {
          har_data
        } else {
          setNames(list(har_data), "Decomposition")
        }

        HARplus::export_data(
          data = export_list,
          output_path = output_path,
          format = export_formats,
          create_subfolder = TRUE,
          multi_sheet_xlsx = TRUE,
          report_output = FALSE
        )

        message("HAR Decomposition Data exported to:", output_path)
      }
    }
  }

  # Final Report Consolidation-------------------------------------------------
  if (export_data && !is.null(output_path)) {
    message("Generating GTAP variable report...")
    .create_gtap_report(all_data, output_path, "Report_Table.xlsx")
  }

  # Summary---------------------------------------------------------------------
  message("\nSummary of Processing:")
  if (!is.null(process_log$macro)) message(process_log$macro)
  if (!is.null(process_log$sl4)) message(process_log$sl4)
  if (!is.null(process_log$har)) message(process_log$har)
  if (!is.null(process_log$qxs)) message(process_log$qxs)

  if (all(vapply(process_log, function(x) grepl("successfully", x), logical(1)))) {
    message("\nGTAP data processing completed successfully!")
  } else {
    failed_processes <- names(process_log)[!vapply(process_log, function(x) grepl("successfully", x), logical(1))]
    message(sprintf("\nGTAP data processing completed with errors in: %s", paste(failed_processes, collapse = ", ")))
  }

  return(invisible(all_data))
}
