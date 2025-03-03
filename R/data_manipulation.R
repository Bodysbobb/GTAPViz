#' Add Mapping Information to GTAP Data
#'
#' @description Adds description and unit information to GTAP data based on specified mapping.
#'
#' @param data_list Data structure containing GTAP variables
#' @param external_map Optional data frame with mapping information
#' @param mapping Mapping mode: "GTAPv7" (default), "No", "Yes", or "Mix"
#' @param description_info Logical. If TRUE, adds description information
#' @param unit_info Logical. If TRUE, adds unit information
#'
#' @return Data structure with added mapping information, preserving original structure
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data
#' har_data <- HARplus::load_harx(system.file("extdata/in", "EXP1.sl4",
#'                                           package = "GTAPViz"))
#'
#' # Corrected example with closing quotation
#' har_data <- add_mapping_info(har_data, mapping = "GTAPv7")
#' }
#'
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


#' Convert Multiple Units in Nested Data Structures
#'
#' @param data A data structure (list, data.frame, or nested combination)
#' @param change_unit_from Character vector of units to change (case-insensitive, space-insensitive, and parentheses-insensitive)
#' @param change_unit_to Character vector of new units (same length as change_unit_from, preserves exactly as entered)
#' @param adjustment Character vector of operations or numeric vector (same length as change_unit_from)
#' @param value_col Column name containing values to adjust (default: "Value")
#' @param unit_col Column name containing unit information (default: "Unit")
#' @param variable_select Optional character vector of variable names to process. If NULL, all variables are processed.
#' @param variable_col Column name containing variable identifiers (default: "Variable")
#' @param scale_auto Optional character vector of predefined conversion rules: "mil2bil", "bil2mil", "pct2frac", "frac2pct"
#'
#' @details
#' The function supports both manual and automatic unit conversion:
#'
#' For manual conversion, provide:
#' - change_unit_from: Units to convert from (case-insensitive)
#' - change_unit_to: Target units (preserved exactly as entered)
#' - adjustment: Conversion operations (e.g., "/1000", "*100")
#'
#' For automatic conversion, use scale_auto with these options:
#' - "mil2bil": Convert million USD to billion USD (divides by 1000)
#' - "bil2mil": Convert billion USD to million USD (multiplies by 1000)
#' - "pct2frac": Convert percent to fraction (divides by 100)
#' - "frac2pct": Convert fraction to percent (multiplies by 100)
#'
#' @return Data structure with same format as input but with adjusted values and units
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data
#' har_data <- HARplus::load_harx(system.file("extdata/in", "EXP1.sl4",
#'                                           package = "GTAPViz"))
#'
#' # Manual conversion: million USD to billion USD
#' har_data <- convert_units(har_data,
#'   change_unit_from = c("million USD"),
#'   change_unit_to = c("billion USD"),
#'   adjustment = c("/1000"))
#'
#' # Automatic conversion
#' har_data <- convert_units(har_data, scale_auto = c("mil2bil", "pct2frac"))
#'
#' # Convert units only for specific variables
#' har_data <- convert_units(har_data,
#'   change_unit_from = c("million USD"),
#'   change_unit_to = c("billion USD"),
#'   adjustment = c("/1000"),
#'   variable_select = c("qgdp", "pop"))
#' }
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
    if (!all(c(value_col, unit_col) %in% names(df))) {
      return(df)
    }

    if (!is.null(variable_select) && variable_col %in% names(df)) {
      variables_match <- df[[variable_col]] %in% variable_select
      if (sum(variables_match) == 0) {
        return(df)
      }
      process_rows <- variables_match
    } else {
      process_rows <- rep(TRUE, nrow(df))
    }

    result <- df

    for (i in seq_along(change_unit_from)) {
      current_unit <- change_unit_from[i]
      new_unit <- change_unit_to[i]
      adjust_operation <- adjustment[i]

      normalized_current_unit <- gsub("[\\s()]", "", tolower(current_unit))
      normalized_df_units <- gsub("[\\s()]", "", tolower(result[[unit_col]]))

      matching_rows <- normalized_df_units == normalized_current_unit & process_rows
      if (sum(matching_rows) == 0) {
        next
      }

      current_values <- result[matching_rows, value_col]

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
          expr <- paste("current_values", adjust_operation)
          result[matching_rows, value_col] <- eval(parse(text = expr))
        } else {
          divisor <- as.numeric(adjust_operation)
          if (!is.na(divisor)) {
            result[matching_rows, value_col] <- current_values / divisor
          }
        }
      } else if (is.numeric(adjust_operation)) {
        result[matching_rows, value_col] <- current_values / adjust_operation
      }

      result[matching_rows, unit_col] <- new_unit
    }

    return(result)
  }

  return(.apply_to_dataframes(data, convert_dataframe))
}


#' Rename Values in Data Structures
#'
#' @description Replaces specified values in a column across nested data structures
#'
#' @param data Data structure containing data to rename
#' @param column_name Column name to modify. If NULL, extracted from mapping file
#' @param mapping.file Data frame with OldName and NewName columns for renaming
#'
#' @return A modified data frame or list of data frames with specified values replaced.
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Example mapping file
#' wefare.decomp.rename <- data.frame(
#'   OldName = c("alloc_A1", "ENDWB1", "tech_C1"),
#'   NewName = c("Allocation", "Endowment", "Technology"),
#'   ColumnName = "Variable",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Load example data
#' har_data <- HARplus::load_harx(system.file("extdata/in", "EXP1.sl4",
#'                                           package = "GTAPViz"))
#'
#' # Apply renaming
#' modified_data <- rename_value(har_data, mapping.file = wefare.decomp.rename)
#' }
#'
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


#' Rename GTAP Bilateral Trade Columns
#'
#' @description Renames bilateral trade columns in GTAP data output
#'
#' @param data Data structure containing GTAP bilateral trade data
#'
#' @return Data structure with renamed bilateral trade columns
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Load Sample Data
#' sl4_data <- HARplus::load_sl4x(system.file("extdata/in", "EXP1-WEL.sl4",
#'                                           package = "GTAPViz"))
#' sl4_data <- rename_GTAP_bilateral(df)
#' }
#'
rename_GTAP_bilateral <- function(data) {
  rename_bilateral_cols <- function(df) {
    if (!is.data.frame(df)) return(df)

    reg_cols <- grep("^REG", names(df), value = TRUE, ignore.case = TRUE)
    region_cols <- grep("^REGION", names(df), value = TRUE, ignore.case = TRUE)

    all_reg_cols <- c(reg_cols, region_cols)

    first_col_pattern <- "^REG$|^REGION$"
    first_col <- grep(first_col_pattern, all_reg_cols, value = TRUE, ignore.case = TRUE)

    second_col_pattern <- "^REG\\.1$|^REG_1$|^REG1$|^REGION\\.1$|^REGION_1$|^REGION1$"
    second_col <- grep(second_col_pattern, all_reg_cols, value = TRUE, ignore.case = TRUE)

    if (length(first_col) >= 1 && length(second_col) >= 1) {
      first_col <- first_col[1]
      second_col <- second_col[1]

      orig_names <- names(df)
      new_names <- orig_names

      new_names[new_names == first_col] <- "Source"
      new_names[new_names == second_col] <- "Destination"

      names(df) <- new_names
    }

    return(df)
  }

  return(.apply_to_dataframes(data, rename_bilateral_cols))
}
