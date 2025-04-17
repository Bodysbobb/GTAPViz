#' @title Add Mapping Information to GTAP Data
#'
#' @description
#' Adds descriptions and unit information to GTAP data based on a specified mapping mode.
#' Supports external mappings or default GTAPv7 mappings, allowing users to enrich datasets with
#' standardized metadata.
#' @md
#' @param data_list A data structure containing GTAP variables.
#' @param external_map Optional data frame with mapping information (must include "Variable", "Description", and "Unit" columns).
#' @param mapping Character. Controls how descriptions and units are added:
#' - `"GTAPv7"` (default): Uses standard GTAPv7 definitions.
#' - `"Yes"`: Uses only the information provided in `external_map`.
#' - `"No"`: Skips adding mapping information.
#' - `"Mix"`: Prioritizes values from `external_map`, but fills in missing entries using GTAPv7 definitions.
#' @param description_info Logical. If `TRUE`, adds description information to the data.
#' @param unit_info Logical. If `TRUE`, adds unit information to the data.
#'
#' @md
#' @details
#' The `mapping` argument supports:
#'


#' @title Add Mapping Information to GTAP Data
#'
#' @description
#' Adds **description** and **unit** information to GTAP data structures based on a specified mapping mode.
#' This function supports internal GTAPv7 mappings, external mappings, or a combination of both.
#'
#' @md
#'
#' @param data_list A list or nested data structure containing GTAP output data frames.
#'
#' @param external_map Optional data frame. External mapping must include columns: `"Variable"`, `"Description"`, and `"Unit"`.
#'
#' @param mapping Character. Mapping mode for assigning metadata to variables.
#' Options:
#' - `"GTAPv7"`: Use GTAPv7 internal definitions (default).
#' - `"Yes"`: Use only the provided `external_map`.
#' - `"Mix"`: Use external definitions first, then fallback to GTAPv7 for missing values.
#' - `"No"`: Skip mapping entirely.
#'
#' @param description_info Logical. If `TRUE`, adds or updates variable descriptions. Default: `TRUE`.
#' @param unit_info Logical. If `TRUE`, adds or updates unit information. Default: `TRUE`.
#'
#' @author Pattawee Puangchit
#'
#' @return The same data structure as input with added `"Description"` and `"Unit"` columns, if applicable.
#'
#' @seealso \code{\link{convert_units}}, \code{\link{rename_value}}
#'
#' @examples
#' # Load GTAP SL4 data
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Add mapping using GTAPv7 defaults
#' gtap_data <- add_mapping_info(sl4.plot.data, mapping = "GTAPv7")
#'
#' # Use a custom mapping file
#' my_map <- data.frame(
#'   Variable = c("qgdp", "EV"),
#'   Description = c("Real GDP", "Welfare"),
#'   Unit = c("percent", "million USD")
#' )
#' gtap_data <- add_mapping_info(sl4.plot.data, external_map = my_map, mapping = "Mix")
#'
#' @export
add_mapping_info <- function(data_list, external_map = NULL, mapping = "GTAPv7",
                             description_info = TRUE, unit_info = TRUE) {
  if (!is.null(mapping)) {
    mapping <- toupper(mapping)
    if (!mapping %in% c("GTAPV7", "NO", "YES", "MIX")) {
      stop("mapping must be one of: 'GTAPv7', 'No', 'Yes', or 'Mix' (case-insensitive)")
    }
  }

  if (identical(mapping, "NO")) return(data_list)

  if (!exists("default_info") && (mapping %in% c("GTAPV7", "MIX"))) {
    warning("default_info not found. Using minimal default mapping.")
    default_info <- data.frame(
      Variable = "E1",
      Description = "Terms of Trade",
      Unit = "percent",
      stringsAsFactors = FALSE
    )
  }

  map_dataframe <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 || !"Variable" %in% names(df))
      return(df)

    is_e1_data <- ("Header" %in% names(df) && any(df$Header == "E1")) ||
      any(df$Variable == "E1")

    mapping_df <- if (mapping == "GTAPV7") {
      default_info
    } else if (mapping == "YES") {
      external_map
    } else if (mapping == "MIX") {
      if (is.null(external_map)) stop("external_map must be provided for 'Mix' mode")
      external_map
    }

    if ("FORM" %in% names(df)) {
      if ("Unit" %in% names(df)) df$Unit <- NULL

      names(df)[names(df) == "FORM"] <- "Unit"

      rename.unit <- data.frame(
        OldName = c("percent", "value"),
        NewName = c("Percent", "million USD"),
        stringsAsFactors = FALSE
      )
      df <- rename_value(df, "Unit", mapping.file = rename.unit)
    }

    if (description_info && !is.null(mapping_df) && "Description" %in% names(mapping_df)) {
      if (!"Description" %in% names(df)) {
        df$Description <- NA_character_
      }

      matched_desc <- mapping_df$Description[match(df$Variable, mapping_df$Variable)]
      if (!all(is.na(matched_desc))) {
        valid_matches <- !is.na(matched_desc)
        df$Description[valid_matches] <- matched_desc[valid_matches]
      }

      if (mapping == "MIX" && any(is.na(df$Description))) {
        missing_desc <- is.na(df$Description)
        default_matches <- default_info$Description[match(df$Variable[missing_desc], default_info$Variable)]
        valid_defaults <- !is.na(default_matches)
        if (any(valid_defaults)) {
          df$Description[missing_desc][valid_defaults] <- default_matches[valid_defaults]
        }
      }
    }

    if (unit_info && !is_e1_data && !is.null(mapping_df) && "Unit" %in% names(mapping_df)) {
      if (!"Unit" %in% names(df)) {
        df$Unit <- NA_character_
      }

      matched_units <- mapping_df$Unit[match(df$Variable, mapping_df$Variable)]
      if (!all(is.na(matched_units))) {
        valid_matches <- !is.na(matched_units)
        df$Unit[valid_matches] <- matched_units[valid_matches]
      }

      if (mapping == "MIX" && any(is.na(df$Unit))) {
        missing_unit <- is.na(df$Unit)
        default_matches <- default_info$Unit[match(df$Variable[missing_unit], default_info$Variable)]
        valid_defaults <- !is.na(default_matches)
        if (any(valid_defaults)) {
          df$Unit[missing_unit][valid_defaults] <- default_matches[valid_defaults]
        }
      }
    }

    return(df)
  }

  return(.apply_to_dataframes(data_list, map_dataframe))
}


#' @title Convert Units in GTAP Data
#'
#' @description
#' Converts values in a dataset to different units based on predefined transformations or custom scaling.
#' Supports manual and automatic conversions for economic and trade-related metrics.
#' @md
#' @param data A data structure (list, data frame, or nested combination).
#' @param change_unit_from Character vector. Units to be converted (case-insensitive).
#' @param change_unit_to Character vector. Target units corresponding to `change_unit_from`.
#' @param adjustment Character or numeric vector. Specifies conversion operations (e.g., `"/1000"` to convert million to billion).
#' @param value_col Character. Column name containing values to adjust (default: `"Value"`).
#' @param unit_col Character. Column name containing unit information (default: `"Unit"`).
#' @param variable_select Optional character vector. If provided, only these variables are converted.
#' @param variable_col Character. Column name containing variable identifiers (default: `"Variable"`).
#' @param scale_auto Optional character vector of predefined conversion rules:
#'   - `"mil2bil"`: Converts million USD to billion USD (divides by 1000).
#'   - `"bil2mil"`: Converts billion USD to million USD (multiplies by 1000).
#'   - `"pct2frac"`: Converts percent to fraction (divides by 100).
#'   - `"frac2pct"`: Converts fraction to percent (multiplies by 100).
#'
#' @details
#' If both `change_unit_from` and `scale_auto` are provided, the function prompts the user
#' to choose between manual and automatic conversion.
#'
#' @return A data structure with values converted to the specified units.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{add_mapping_info}}, \code{\link{rename_value}}, \code{\link{sort_plot_data}}
#'

#' @examples
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Convert million USD to billion USD
#' gtap_data <- convert_units(sl4.plot.data,
#'   change_unit_from = "million USD",
#'   change_unit_to = "billion USD",
#'   adjustment = "/1000"
#' )
#'
#' # Automatic conversion from percent to fraction
#' gtap_data <- convert_units(sl4.plot.data, scale_auto = "pct2frac")
#'
convert_units <- function(data, change_unit_from = NULL, change_unit_to = NULL,
                          adjustment = NULL, value_col = "Value", unit_col = "Unit",
                          variable_select = NULL, variable_col = "Variable",
                          scale_auto = NULL) {
  if (is.null(change_unit_from) && is.null(scale_auto)) {
    stop("Either change_unit_from or scale_auto must be provided")
  }

  if (!is.null(scale_auto)) {
    valid_scales <- c("mil2bil", "bil2mil", "pct2frac", "frac2pct")
    invalid_scales <- setdiff(scale_auto, valid_scales)

    if (length(invalid_scales) > 0) {
      stop("Invalid scale_auto values: ", paste(invalid_scales, collapse = ", "),
           ". Valid options are: ", paste(valid_scales, collapse = ", "))
    }

    has_mil2bil <- "mil2bil" %in% scale_auto
    has_bil2mil <- "bil2mil" %in% scale_auto
    has_pct2frac <- "pct2frac" %in% scale_auto
    has_frac2pct <- "frac2pct" %in% scale_auto

    if ((has_mil2bil && has_bil2mil) || (has_pct2frac && has_frac2pct)) {
      stop("Conflicting scale_auto options. Cannot use 'mil2bil' with 'bil2mil' or 'pct2frac' with 'frac2pct'")
    }

    if (!is.null(change_unit_from)) {
      use_auto <- .ask_confirmation(
        "Both manual conversion (change_unit_from/to) and automatic conversion (scale_auto) are provided. Use automatic conversion? (Y/N): ")

      if (use_auto) {
        change_unit_from <- NULL
        change_unit_to <- NULL
        adjustment <- NULL
      } else {
        scale_auto <- NULL
      }
    }

    if (!is.null(scale_auto)) {
      change_unit_from <- character(0)
      change_unit_to <- character(0)
      adjustment <- character(0)

      if ("mil2bil" %in% scale_auto) {
        change_unit_from <- c(change_unit_from, "million USD")
        change_unit_to <- c(change_unit_to, "billion USD")
        adjustment <- c(adjustment, "/1000")
      }

      if ("bil2mil" %in% scale_auto) {
        change_unit_from <- c(change_unit_from, "billion USD")
        change_unit_to <- c(change_unit_to, "million USD")
        adjustment <- c(adjustment, "*1000")
      }

      if ("pct2frac" %in% scale_auto) {
        change_unit_from <- c(change_unit_from, "percent")
        change_unit_to <- c(change_unit_to, "Fraction")
        adjustment <- c(adjustment, "/100")
      }

      if ("frac2pct" %in% scale_auto) {
        change_unit_from <- c(change_unit_from, "fraction")
        change_unit_to <- c(change_unit_to, "Percent")
        adjustment <- c(adjustment, "*100")
      }
    }
  }

  if (length(change_unit_from) != length(change_unit_to) ||
      length(change_unit_from) != length(adjustment)) {
    stop("change_unit_from, change_unit_to, and adjustment must all have the same length")
  }

  convert_dataframe <- function(df) {
    # Skip if dataframe doesn't have required columns
    if (!is.data.frame(df) || !all(c(value_col, unit_col) %in% names(df))) {
      return(df)
    }

    # Make a copy of the dataframe to modify
    result <- df

    # Handle variable filtering
    if (!is.null(variable_select) && variable_col %in% names(df)) {
      # Check if any variables match
      variables_match <- df[[variable_col]] %in% variable_select
      if (all(is.na(variables_match)) || sum(variables_match, na.rm = TRUE) == 0) {
        return(df)
      }
      # Handle NAs in filtering
      process_rows <- variables_match
      process_rows[is.na(process_rows)] <- FALSE
    } else {
      process_rows <- rep(TRUE, nrow(df))
    }

    conversions_made <- 0

    for (i in seq_along(change_unit_from)) {
      current_unit <- change_unit_from[i]
      new_unit <- change_unit_to[i]
      adjust_operation <- adjustment[i]

      # Normalize units for consistent matching
      normalized_current_unit <- gsub("[\\s()]", "", tolower(current_unit))

      # Handle NAs in unit column
      unit_values <- result[[unit_col]]
      unit_values[is.na(unit_values)] <- ""
      normalized_df_units <- gsub("[\\s()]", "", tolower(unit_values))

      # Find matching rows with NA safety
      matching_units <- normalized_df_units == normalized_current_unit
      matching_rows <- matching_units & process_rows

      # Skip if no matches found
      if (sum(matching_rows, na.rm = TRUE) == 0) {
        next
      }

      # Only proceed with non-NA values in value column
      valid_values <- !is.na(result[matching_rows, value_col])
      if (sum(valid_values) == 0) {
        next
      }

      # Apply the conversion to matching rows with valid values
      current_values <- result[matching_rows, value_col]

      # Handle different adjustment types
      if (is.function(adjust_operation)) {
        result[matching_rows, value_col] <- adjust_operation(current_values)
      } else if (is.character(adjust_operation)) {
        if (grepl("^/", adjust_operation)) {
          divisor <- as.numeric(gsub("^/\\s*", "", adjust_operation))
          result[matching_rows, value_col] <- current_values / divisor
        } else if (grepl("^\\*", adjust_operation)) {
          multiplier <- as.numeric(gsub("^\\*\\s*", "", adjust_operation))
          result[matching_rows, value_col] <- current_values * multiplier
        } else if (grepl("^\\+", adjust_operation)) {
          addend <- as.numeric(gsub("^\\+\\s*", "", adjust_operation))
          result[matching_rows, value_col] <- current_values + addend
        } else if (grepl("^-", adjust_operation)) {
          subtrahend <- as.numeric(gsub("^-\\s*", "", adjust_operation))
          result[matching_rows, value_col] <- current_values - subtrahend
        } else if (grepl("^.+[+\\-*/].+", adjust_operation)) {
          # Complex expression
          expr <- paste("current_values", adjust_operation)
          tryCatch({
            result[matching_rows, value_col] <- eval(parse(text = expr))
          }, error = function(e) {
            warning("Error in evaluating adjustment expression: ", e$message)
          })
        } else {
          # Try to parse as a numeric divisor
          divisor <- suppressWarnings(as.numeric(adjust_operation))
          if (!is.na(divisor)) {
            result[matching_rows, value_col] <- current_values / divisor
          }
        }
      } else if (is.numeric(adjust_operation)) {
        result[matching_rows, value_col] <- current_values / adjust_operation
      }

      # Update unit labels
      result[matching_rows, unit_col] <- new_unit
      conversions_made <- conversions_made + sum(matching_rows, na.rm = TRUE)
    }

    if (conversions_made > 0) {
      message(conversions_made, " observations converted to new unit")
    }

    return(result)
  }

  # Apply the conversion function to all dataframes in the structure
  return(.apply_to_dataframes(data, convert_dataframe))
}


#' @title Rename Values in a Column
#'
#' @description
#' Replaces specific values in a column based on a provided mapping file.
#' Supports renaming across nested data structures and preserves factor levels.
#'
#' @param data Data structure (data frame, list, or nested combination).
#' @param column_name Character. Column to modify. If `NULL`, the function extracts it from `mapping.file`.
#' @param mapping.file Data frame with `"OldName"` and `"NewName"` columns for renaming.
#'
#' @return The same data structure with specified values replaced.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{add_mapping_info}}, \code{\link{convert_units}}, \code{\link{sort_plot_data}}
#'
#' @examples
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))
#'
#' # Rename variables in a dataset
#' mapping_welfare <- data.frame(
#'   ColumnName = "COLUMN",
#'   OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
#'   NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
#'   stringsAsFactors = FALSE
#' )
#'
#' har.plot.data <- rename_value(har.plot.data, mapping.file = mapping_welfare)
rename_value <- function(data, column_name = NULL, mapping.file) {
  if (!all(c("OldName", "NewName") %in% names(mapping.file))) {
    stop("mapping.file must contain 'OldName' and 'NewName' columns.")
  }

  if (is.null(column_name)) {
    column_name <- unique(mapping.file$ColumnName)
    if (length(column_name) != 1) {
      stop("ColumnName in mapping.file must contain a single unique value or be specified manually.")
    }
  }

  rename_column <- function(df) {
    if (!column_name %in% names(df)) {
      return(df)
    }

    is_factor <- is.factor(df[[column_name]])
    if (is_factor) {
      original_levels <- levels(df[[column_name]])
      df[[column_name]] <- as.character(df[[column_name]])
    }

    for (i in 1:nrow(mapping.file)) {
      old_value <- mapping.file$OldName[i]
      new_value <- mapping.file$NewName[i]
      df[[column_name]] <- ifelse(df[[column_name]] == old_value, new_value, df[[column_name]])
    }

    if (is_factor) {
      new_levels <- unique(c(original_levels, df[[column_name]]))
      df[[column_name]] <- factor(df[[column_name]], levels = new_levels)
    }

    return(df)
  }

  return(.apply_to_dataframes(data, rename_column))
}

#' @title Sort GTAP Plot Data
#' @md
#'
#' @description
#' Sorts data frames in a GTAP plot list structure based on specified column orders.
#' Works with data frames, lists of data frames, or nested data structures.
#'
#' @param data A data frame or list structure containing data to be sorted.
#' @param sort_columns Named list. Specifies columns to sort by and their ordering.
#'        Each element should be a character vector of values in desired order.
#'        For example, `list(Region = c("USA", "EU", "CHN")`,
#'                          `Experiment = c("Base", "Shock1", "Shock2"))`.
#' @param sort_by_value_desc Logical or NULL. Controls sorting by the "Value" column:
#'        - NULL (default): Don't sort by value, only use column-based sorting.
#'        - TRUE: After column-based sorting, sort by value in descending order.
#'        - FALSE: After column-based sorting, sort by value in ascending order.
#' @param convert_to_factor Logical. Whether to convert sorted columns to factors with custom ordering.
#'        Default is TRUE, which preserves ordering in GTAP plotting functions.
#'
#' @return A data structure with the same form as the input, with all contained data frames sorted.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @seealso \code{\link{add_mapping_info}}, \code{\link{convert_units}}, \code{\link{rename_value}}
#'
#' @examples
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Creating Sorting Rule
#' sorting_specs <- list(
#'   Experiment = c("EXP2", "EXP1"),     # Show EXP2 first, then EXP1
#'   Region = c("EastAsia", "SEAsia", "Oceania")  # Custom region order
#' )
#'
#' # Sorting
#' sort_data <- sort_plot_data(sl4.plot.data, sort_columns = sorting_specs,
#'                             sort_by_value_desc = FALSE)
#'
sort_plot_data <- function(data, sort_columns = NULL,
                           sort_by_value_desc = NULL,
                           convert_to_factor = TRUE) {
  # If no sorting parameters provided, return the data as is
  if (is.null(sort_columns) && is.null(sort_by_value_desc)) {
    return(data)
  }

  # Function to sort a single dataframe
  sort_single_dataframe <- function(df) {
    if (!is.data.frame(df)) return(df)
    if (nrow(df) <= 1) return(df)

    # Create copies to work with
    original_df <- df
    working_df <- df
    sort_col_names <- c()

    # Keep track of column order info for factor conversion
    col_order_maps <- list()

    # Process column-based sorting
    if (!is.null(sort_columns)) {
      for (col_name in names(sort_columns)) {
        if (!col_name %in% colnames(df)) {
          next
        }

        col_values <- sort_columns[[col_name]]
        if (is.character(col_values) && length(col_values) > 0) {
          # Get all unique values from the dataframe column
          all_values <- unique(df[[col_name]])

          # Create order: specified values first in specified order, then others
          specified <- col_values[col_values %in% all_values]
          unspecified <- setdiff(all_values, specified)
          ordered_values <- c(specified, unspecified)

          # Save this ordering for later factor conversion
          col_order_maps[[col_name]] <- ordered_values

          # Create a numeric index for sorting
          order_map <- setNames(seq_along(ordered_values), ordered_values)

          # Create sorting column
          temp_col_name <- paste0("._sort_", col_name)
          working_df[[temp_col_name]] <- match(df[[col_name]], ordered_values)
          sort_col_names <- c(sort_col_names, temp_col_name)
        } else if (isTRUE(col_values)) {
          # Sort alphabetically
          ordered_values <- sort(unique(df[[col_name]]))
          col_order_maps[[col_name]] <- ordered_values

          temp_col_name <- paste0("._sort_", col_name)
          working_df[[temp_col_name]] <- match(df[[col_name]], ordered_values)
          sort_col_names <- c(sort_col_names, temp_col_name)
        }
      }
    }

    # Add value sorting if requested
    if (!is.null(sort_by_value_desc) && "Value" %in% colnames(df)) {
      if (isTRUE(sort_by_value_desc)) {
        working_df$._sort_Value <- -as.numeric(df$Value)  # Descending
      } else {
        working_df$._sort_Value <- as.numeric(df$Value)   # Ascending
      }
      sort_col_names <- c(sort_col_names, "._sort_Value")
    }

    # If no sorting columns created, return original
    if (length(sort_col_names) == 0) {
      return(original_df)
    }

    # Sort the dataframe using all sorting columns
    sort_order <- do.call(order, working_df[sort_col_names])
    sorted_df <- df[sort_order, ]

    # Convert to factors with explicit ordering if requested
    if (convert_to_factor) {
      for (col_name in names(col_order_maps)) {
        if (col_name %in% names(sorted_df)) {
          # Convert to factor with our specific ordering
          sorted_df[[col_name]] <- factor(sorted_df[[col_name]],
                                          levels = col_order_maps[[col_name]])
        }
      }
    }

    # Preserve original rownames if present
    if (!is.null(rownames(original_df))) {
      rownames(sorted_df) <- rownames(original_df)[sort_order]
    }

    return(sorted_df)
  }

  if (exists(".apply_to_dataframes")) {
    return(.apply_to_dataframes(data, sort_single_dataframe))
  } else {
    process_data <- function(x) {
      if (is.data.frame(x)) {
        return(sort_single_dataframe(x))
      } else if (is.list(x)) {
        return(lapply(x, process_data))
      } else {
        return(x)
      }
    }
    return(process_data(data))
  }
}
