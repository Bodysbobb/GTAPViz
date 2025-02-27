#' @title Add Mapping Information
#'
#' @description The description and unit are based on GTAPv7 model variables.
#' Full list refer to https://www.gtap.agecon.purdue.edu/models/setsvariables.asp.
#' If any variable is added apart from the default GTAPv7 model, the information will be left blank.
#'
#' @details
#' For the "E1" header in HAR files, if using GTAPv7 or Mix mapping mode, it renames the "FORM" column to "Unit".
#' When using "Yes" mapping mode with an external map, it will use provided Description and Unit values.
#'
#' @param data_list A list or data frame containing GTAP variables.
#' @param external_map **Optional**. A data frame containing external mapping information with columns `"Variable"`, `"Description"`, and `"Unit"`. If `NULL`, the default GTAPv7 mapping is used.
#' @param mapping **Optional**. Either `"GTAPv7"`, `"No"`, `"Yes"`, or `"Mix"`. Determines the mapping approach. Default is `"GTAPv7"`.
#' @param description_info **Optional**. Logical. If `TRUE`, adds description information. Default is `TRUE`.
#' @param unit_info **Optional**. Logical. If `TRUE`, adds unit information. Default is `TRUE`.
#'
#' @return A modified list or data frame with added description and/or unit information.
#'
#' @author Pattawee Puangchit
#'
#' @export
#'
#' @examples
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' result <- add_mapping_info(sl4_data1, mapping = "GTAPv7", unit_info = FALSE) # Add only Description
#' result <- add_mapping_info(sl4_data1, mapping = "GTAPv7", description_info = FALSE) # Add only Unit
#' custom_mapping <- data.frame(Variable = c("qgdp", "EV"),
#'                              Description = c("Real GDP", "Economic Value"),
#'                              Unit = c("Percent", "million USD"))
#' result <- add_mapping_info(sl4_data1, external_map = custom_mapping, mapping = "Mix") # Custom + GTAPv7
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

  # Load default mapping if not available in the environment
  if (!exists("default_info") && (mapping %in% c("GTAPV7", "MIX"))) {
    warning("default_info not found. Using minimal default mapping.")
    default_info <- data.frame(
      Variable = "E1",
      Description = "Terms of Trade",
      Unit = "percent",
      stringsAsFactors = FALSE
    )
  }

  if (is.list(data_list) && !is.data.frame(data_list)) {
    # Case 1: If data_list is a list of data frames (e.g., from get_data_by_var)
    result <- lapply(names(data_list), function(header_name) {
      df <- data_list[[header_name]]

      # Special handling for E1 header
      if (header_name %in% c("E1", "COMM*REG*PRICES*FORM") && "FORM" %in% names(df)) {
        # Drop Unit column if it already exists
        if ("Unit" %in% names(df)) {
          df[["Unit"]] <- NULL
        }

        # Rename FORM to Unit
        names(df)[names(df) == "FORM"] <- "Unit"
        rename.unit <- data.frame(
          OldName = c("percent", "value"),
          NewName = c("Percent", "million USD"),
          stringsAsFactors = FALSE
        )
        df <- rename_value(df, "Unit", mapping.file = rename.unit)

        # Add description if needed
        if (description_info) {
          if (mapping %in% c("GTAPV7", "MIX")) {
            e1_desc <- default_info$Description[default_info$Variable == "E1"]
            if (length(e1_desc) > 0) {
              df$Description <- e1_desc[1]
            }
          } else if (mapping == "YES" && !is.null(external_map)) {
            e1_desc <- external_map$Description[external_map$Variable == "E1"]
            if (length(e1_desc) > 0) {
              df$Description <- e1_desc[1]
            }
          }
        }
      } else {
        # Regular variable handling for non-E1 headers
        if ("Variable" %in% names(df)) {
          # Add descriptions
          if (description_info) {
            mapping_df <- if (mapping == "GTAPV7") {
              default_info
            } else if (mapping == "YES") {
              external_map
            } else if (mapping == "MIX") {
              # For MIX, prefer external_map but fall back to default_info
              if (is.null(external_map)) stop("external_map must be provided for 'Mix' mode")
              external_map
            }

            if (!is.null(mapping_df) && "Description" %in% names(mapping_df)) {
              df$Description <- mapping_df$Description[match(df$Variable, mapping_df$Variable)]

              # For MIX mode, fill in missing descriptions from default_info
              if (mapping == "MIX" && any(is.na(df$Description))) {
                missing_desc <- is.na(df$Description)
                df$Description[missing_desc] <- default_info$Description[
                  match(df$Variable[missing_desc], default_info$Variable)]
              }
            }
          }

          # Add units (except for E1 which was handled separately)
          if (unit_info && header_name != "E1") {
            mapping_df <- if (mapping == "GTAPV7") {
              default_info
            } else if (mapping == "YES") {
              external_map
            } else if (mapping == "MIX") {
              external_map
            }

            if (!is.null(mapping_df) && "Unit" %in% names(mapping_df)) {
              df$Unit <- mapping_df$Unit[match(df$Variable, mapping_df$Variable)]

              # For MIX mode, fill in missing units from default_info
              if (mapping == "MIX" && any(is.na(df$Unit))) {
                missing_unit <- is.na(df$Unit)
                df$Unit[missing_unit] <- default_info$Unit[
                  match(df$Variable[missing_unit], default_info$Variable)]
              }
            }
          }
        }
      }

      return(df)
    })

    names(result) <- names(data_list)
    attributes(result) <- attributes(data_list)
    class(result) <- class(data_list)

  } else if (is.data.frame(data_list)) {
    # Case 2: If data_list is a single data frame
    result <- data_list

    # Special handling for E1 if it has a "Header" column
    if ("Header" %in% names(result) && "FORM" %in% names(result)) {
      e1_rows <- result$Header == "E1"

      if (any(e1_rows)) {
        # Remove Unit column if it exists
        if ("Unit" %in% names(result)) {
          result$Unit <- NULL
        }

        # Create Unit column and copy values from FORM for E1 rows
        result$Unit <- NA_character_
        result$Unit[e1_rows] <- result$FORM[e1_rows]

        # Add description for E1 rows
        if (description_info) {
          if (!"Description" %in% names(result)) {
            result$Description <- NA_character_
          }

          if (mapping %in% c("GTAPV7", "MIX")) {
            e1_desc <- default_info$Description[default_info$Variable == "E1"]
            if (length(e1_desc) > 0) {
              result$Description[e1_rows] <- e1_desc[1]
            }
          } else if (mapping == "YES" && !is.null(external_map)) {
            e1_desc <- external_map$Description[external_map$Variable == "E1"]
            if (length(e1_desc) > 0) {
              result$Description[e1_rows] <- e1_desc[1]
            }
          }
        }

        # For non-E1 rows, apply standard mapping
        non_e1_rows <- !e1_rows
        if (any(non_e1_rows) && "Variable" %in% names(result)) {
          if (description_info) {
            mapping_df <- if (mapping == "GTAPV7") {
              default_info
            } else if (mapping == "YES") {
              external_map
            } else if (mapping == "MIX") {
              external_map
            }

            if (!is.null(mapping_df) && "Description" %in% names(mapping_df)) {
              result$Description[non_e1_rows] <- mapping_df$Description[
                match(result$Variable[non_e1_rows], mapping_df$Variable)]

              if (mapping == "MIX") {
                missing_desc <- non_e1_rows & is.na(result$Description)
                if (any(missing_desc)) {
                  result$Description[missing_desc] <- default_info$Description[
                    match(result$Variable[missing_desc], default_info$Variable)]
                }
              }
            }
          }

          if (unit_info) {
            mapping_df <- if (mapping == "GTAPV7") {
              default_info
            } else if (mapping == "YES") {
              external_map
            } else if (mapping == "MIX") {
              external_map
            }

            if (!is.null(mapping_df) && "Unit" %in% names(mapping_df)) {
              result$Unit[non_e1_rows] <- mapping_df$Unit[
                match(result$Variable[non_e1_rows], mapping_df$Variable)]

              if (mapping == "MIX") {
                missing_unit <- non_e1_rows & is.na(result$Unit)
                if (any(missing_unit)) {
                  result$Unit[missing_unit] <- default_info$Unit[
                    match(result$Variable[missing_unit], default_info$Variable)]
                }
              }
            }
          }
        }
      }
    } else if ("Variable" %in% names(result)) {

      # Check for E1 variables
      e1_rows <- result$Variable == "E1"
      if (any(e1_rows) && "FORM" %in% names(result)) {
        if ("Unit" %in% names(result)) {
          result$Unit <- NULL
        }
        result$Unit <- NA_character_
        result$Unit[e1_rows] <- result$FORM[e1_rows]
      }

      if (description_info) {
        mapping_df <- if (mapping == "GTAPV7") {
          default_info
        } else if (mapping == "YES") {
          external_map
        } else if (mapping == "MIX") {
          external_map
        }

        if (!is.null(mapping_df) && "Description" %in% names(mapping_df)) {
          if (!"Description" %in% names(result)) {
            result$Description <- NA_character_
          }

          result$Description <- mapping_df$Description[match(result$Variable, mapping_df$Variable)]

          if (mapping == "MIX" && any(is.na(result$Description))) {
            missing_desc <- is.na(result$Description)
            result$Description[missing_desc] <- default_info$Description[
              match(result$Variable[missing_desc], default_info$Variable)]
          }
        }
      }

      if (unit_info) {
        mapping_df <- if (mapping == "GTAPV7") {
          default_info
        } else if (mapping == "YES") {
          external_map
        } else if (mapping == "MIX") {
          external_map
        }

        if (!is.null(mapping_df) && "Unit" %in% names(mapping_df)) {
          if (!"Unit" %in% names(result)) {
            result$Unit <- NA_character_
          }

          apply_rows <- if ("FORM" %in% names(result)) !e1_rows else rep(TRUE, nrow(result))
          if (any(apply_rows)) {
            result$Unit[apply_rows] <- mapping_df$Unit[
              match(result$Variable[apply_rows], mapping_df$Variable)]

            if (mapping == "MIX") {
              missing_unit <- apply_rows & is.na(result$Unit)
              if (any(missing_unit)) {
                result$Unit[missing_unit] <- default_info$Unit[
                  match(result$Variable[missing_unit], default_info$Variable)]
              }
            }
          }
        }
      }
    }
  } else {
    result <- data_list
  }

  return(result)
}


#' @title Convert Multiple Units in Nested Data Structures
#' @param data A data structure (list, data.frame, or nested combination)
#' @param change_unit_from Character vector of units to change (case-insensitive)
#' @param change_unit_to Character vector of new units (same length as change_unit_from)
#' @param adjustment Character vector of operations or numeric vector (same length as change_unit_from)
#' @param value_col Column name containing values to adjust (default: "Value")
#' @param unit_col Column name containing unit information (default: "Unit")
#' @return Data structure with same format as input but with adjusted values and units
#' @author Pattawee Puangchit
#' @export
convert_units <- function(data, change_unit_from, change_unit_to,
                          adjustment, value_col = "Value", unit_col = "Unit") {

  # Input validation
  if (length(change_unit_from) != length(change_unit_to) ||
      length(change_unit_from) != length(adjustment)) {
    stop("change_unit_from, change_unit_to, and adjustment must all have the same length")
  }

  # Process nested data structures recursively
  if (is.list(data) && !is.data.frame(data)) {
    result <- lapply(data, function(item) {
      convert_units(item, change_unit_from, change_unit_to,
                    adjustment, value_col, unit_col)
    })
    # Preserve attributes and class
    attributes(result) <- attributes(data)
    return(result)
  }

  # Process data frame
  if (is.data.frame(data)) {
    if (!all(c(value_col, unit_col) %in% names(data))) {
      return(data) # Skip if required columns don't exist
    }

    # Create a copy of the input data
    result <- data

    # Process each unit conversion
    for (i in seq_along(change_unit_from)) {
      current_unit <- change_unit_from[i]
      new_unit <- change_unit_to[i]
      adjust_operation <- adjustment[i]

      # Find matching rows (case-insensitive)
      matching_rows <- tolower(result[[unit_col]]) == tolower(current_unit)

      if (sum(matching_rows) == 0) {
        next
      }

      # Apply adjustment to values
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
          # Assume numeric and divide
          divisor <- as.numeric(adjust_operation)
          if (!is.na(divisor)) {
            result[matching_rows, value_col] <- current_values / divisor
          }
        }
      } else if (is.numeric(adjust_operation)) {
        result[matching_rows, value_col] <- current_values / adjust_operation
      }

      # Update unit labels
      result[matching_rows, unit_col] <- new_unit
    }

    return(result)
  }

  # Return unchanged if not a list or data frame
  return(data)
}


#' @title Rename Values in a Data Frame or List of Data Frames
#'
#' @description Replaces specified values in a given column with new values based on a mapping file.
#' If `column_name` is not provided, it will be automatically extracted from `mapping.file` if available.
#'
#' @param data A data frame or a list of data frames containing the column to modify.
#' @param column_name Optional. Character. The name of the column where values will be changed.
#' If NULL, the function will attempt to extract `ColumnName` from `mapping.file`.
#' @param mapping.file A data frame containing `OldName` and `NewName` columns that specify
#' the mapping of values to be replaced. Optionally, a `ColumnName` column may define which
#' column should be modified.
#'
#' @return A modified data frame or list of data frames with specified values replaced.
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Example mapping file
#' wefare.decomp.rename <- data.frame(
#'   OldName = c("alloc_A1", "ENDWB1", "tech_C1"),
#'   NewName = c("Allocation", "Endowment", "Technology"),
#'   ColumnName = "Variable",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Load example data
#' har_data <- HARplus::load_harx(c("globalcgds", "vgdpwld", "WEV"))
#' har_data <- HARplus::get_data_by_var(c("globalcgds", "vgdpwld", "WEV"), har_data)
#'
#' # Apply renaming
#' modified_list <- rename_value(har_data, mapping.file = wefare.decomp.rename)
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

  rename_column <- function(df, column_name, mapping.file) {
    if (!column_name %in% names(df)) {
      return(df)
    }

    # Convert the column to character if it's a factor
    is_factor <- is.factor(df[[column_name]])
    if (is_factor) {
      original_levels <- levels(df[[column_name]])
      df[[column_name]] <- as.character(df[[column_name]])
    }

    # Do the replacements
    for (i in 1:nrow(mapping.file)) {
      old_value <- mapping.file$OldName[i]
      new_value <- mapping.file$NewName[i]
      df[[column_name]] <- ifelse(df[[column_name]] == old_value, new_value, df[[column_name]])
    }

    # Convert back to factor if it was a factor originally
    if (is_factor) {
      new_levels <- unique(c(original_levels, df[[column_name]]))
      df[[column_name]] <- factor(df[[column_name]], levels = new_levels)
    }

    return(df)
  }

  if (is.data.frame(data)) {
    return(rename_column(data, column_name, mapping.file))
  } else if (is.list(data) && !is.data.frame(data)) {
    result <- lapply(data, function(df) {
      if (is.data.frame(df)) {
        rename_column(df, column_name, mapping.file)
      } else {
        df
      }
    })
    names(result) <- names(data)
    attributes(result) <- attributes(data)
    class(result) <- class(data)
    return(result)
  } else {
    stop("Unsupported data type. Input should be a data frame or a list of data frames.")
  }
}



#' @title Rename GTAP Bilateral Trade Columns
#' @description Renames bilateral trade columns in GTAP data output, specifically targeting REG columns
#' to Source and Destination in data frames with COMM*REG*REG dimension structure.
#'
#' @param data List or data frame. Output from get_data_by_var, get_data_by_dims, or group_data_by_dims.
#' @return Data with renamed columns, maintaining the original structure type.
#' @author Your Name
#' @export
#'
#' @examples
#' # For direct data frame output
#' df <- get_data_by_var("qxs", sl4_data)
#' df_renamed <- rename_GTAP_bilateral(df)
#'
#' # For list output with multiple variables
#' data_list <- get_data_by_var(c("qxs", "viws"), sl4_data)
#' renamed_list <- rename_GTAP_bilateral(data_list)
#'
#' # For grouped dimension output
#' grouped_data <- group_data_by_dims("COMM*REG*REG", sl4_data)
#' renamed_grouped <- rename_GTAP_bilateral(grouped_data)
#'
rename_GTAP_bilateral <- function(data) {
  rename_cols_in_df <- function(df) {
    if (!is.data.frame(df)) return(df)

    reg_cols <- grep("^REG", names(df))
    if (length(reg_cols) >= 2) {
      orig_names <- names(df)

      first_reg <- reg_cols[1]
      second_reg <- reg_cols[2]

      new_names <- orig_names
      new_names[first_reg] <- "Source"
      new_names[second_reg] <- "Destination"

      names(df) <- new_names
    }
    return(df)
  }

  if (is.data.frame(data)) {
    return(rename_cols_in_df(data))

  } else if (is.list(data)) {
    if ("data" %in% names(data) && "dimension_info" %in% names(data)) {
      data$data <- lapply(data$data, rename_cols_in_df)
      return(data)

    } else if (all(sapply(data, function(x)
      is.data.frame(x) || (is.list(x) && !is.null(attr(x, "class")))))) {
      return(lapply(data, function(x) {
        if (is.data.frame(x)) {
          rename_cols_in_df(x)
        } else {
          rename_GTAP_bilateral(x)
        }
      }))

    } else if ("3D" %in% names(data) || inherits(data, "grouped_dims_element")) {
      for (dim_key in names(data)) {
        if (is.list(data[[dim_key]])) {
          data[[dim_key]] <- lapply(data[[dim_key]], function(x) {
            if (is.data.frame(x)) {
              rename_cols_in_df(x)
            } else if (is.list(x)) {
              rename_GTAP_bilateral(x)
            } else {
              x
            }
          })
        }
      }
      return(data)
    }
  }

  return(data)
}

