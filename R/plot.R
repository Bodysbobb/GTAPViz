# Get Plot Style Help -----------------------------------------------------

#' @title Get Plot Style Configuration
#'
#' @description
#' Returns configuration settings for plot styles, with options to view as a structured dataframe
#' or to look up specific parameters. Also provides parameter validation for custom configurations.
#'
#' @param plot_type Character. Type of plot: "comparison" (default), "detail", or "stack".
#' @param parameter_name Character or NULL. Name of specific parameter to return information about.
#' @param show_docs Logical. Whether to include documentation in the output.
#' @param validate_custom List or NULL. Custom configuration settings to validate.
#' @param as_dataframe Logical. Whether to return settings as a dataframe.
#'
#' @return
#' If \code{parameter_name} is specified, returns a list with the value and documentation for that parameter.
#' If \code{validate_custom} is specified, returns the validated configuration list.
#' If \code{as_dataframe} is TRUE, returns a dataframe with configuration settings.
#' Otherwise, returns a list with all configuration settings.
#'
#' @details
#' This function applies default styling for different types of plots and allows users to customize the appearance.
#' The parameters are grouped as follows:
#'
#' ## **Title Settings**
#' - `show_title`: Logical. Show or hide the plot title. Default: `TRUE`
#' - `title_face`: Character. Font face (`"bold"`, `"plain"`, `"italic"`). Default: `"bold"`
#' - `title_size`: Numeric. Font size of title. Default: `20`
#' - `title_hjust`: Numeric. Horizontal alignment (0 = left, 1 = right). Default: `0.5`
#' - `add_unit_to_title`: Logical. Append unit to title if applicable. Default: `TRUE`
#' - `title_margin`: ggplot2 `margin()` object. Default: `ggplot2::margin(10, 0, 10, 0)`
#' - `title_format`: List. Defines title formatting options (`type = "standard"`, `text = ""`)
#'
#' ## **X-Axis Settings**
#' - `show_x_axis_title`: Logical. Show or hide x-axis title. Default: `FALSE`
#' - `x_axis_title_face`: Character. Font face for x-axis title. Default: `"bold"`
#' - `x_axis_title_size`: Numeric. Font size of x-axis title. Default: `16`
#' - `x_axis_title_margin`: ggplot2 `margin()`. Default: `ggplot2::margin(t = 20)`
#' - `show_x_axis_labels`: Logical. Show or hide x-axis labels. Default: `TRUE`
#' - `x_axis_text_face`: Character. Font face for x-axis labels. Default: `"bold"`
#' - `x_axis_text_size`: Numeric. Font size of x-axis labels. Default: `14`
#' - `x_axis_text_angle`: Numeric. Angle of x-axis labels. Default: `45`
#' - `x_axis_text_hjust`: Numeric. Horizontal justification of x-axis labels. Default: `1`
#' - `x_axis_description`: Character. Optional description for the x-axis. Default: `""`
#'
#' ## **Y-Axis Settings**
#' - `show_y_axis_title`: Logical. Show or hide y-axis title. Default: `TRUE`
#' - `y_axis_title_face`: Character. Font face for y-axis title. Default: `"bold"`
#' - `y_axis_title_size`: Numeric. Font size of y-axis title. Default: `16`
#' - `y_axis_title_margin`: ggplot2 `margin()`. Default: `ggplot2::margin(r = 20)`
#' - `show_y_axis_labels`: Logical. Show or hide y-axis labels. Default: `TRUE`
#' - `y_axis_text_face`: Character. Font face for y-axis labels. Default: `"plain"`
#' - `y_axis_text_size`: Numeric. Font size of y-axis labels. Default: `14`
#' - `y_axis_text_angle`: Numeric. Angle of y-axis labels. Default: `0`
#' - `y_axis_text_hjust`: Numeric. Horizontal justification of y-axis labels. Default: `0`
#' - `y_axis_description`: Character. Optional description for the y-axis. Default: `""`
#' - `show_axis_titles_on_all_facets`: Logical. Show axis titles on all facets. Default: `TRUE`
#'
#' ## **Value Label Settings**
#' - `show_value_labels`: Logical. Show or hide value labels. Default: `TRUE`
#' - `value_label_face`: Character. Font face for value labels. Default: `"plain"`
#' - `value_label_size`: Numeric. Font size of value labels. Default: `5`
#' - `value_label_position`: Character. Position of value labels (`"above"`, `"outside"`, `"top"`). Default: `"above"`
#' - `value_label_decimal_places`: Numeric. Number of decimal places in value labels. Default: `2`
#'
#' ## **Legend Settings**
#' - `show_legend`: Logical. Show or hide legend. Default: `FALSE`
#' - `show_legend_title`: Logical. Show or hide legend title. Default: `FALSE`
#' - `legend_position`: Character. Legend position (`"none"`, `"bottom"`, `"right"`). Default: `"none"`
#' - `legend_title_face`: Character. Font face for legend title. Default: `"bold"`
#' - `legend_text_face`: Character. Font face for legend text. Default: `"plain"`
#' - `legend_text_size`: Numeric. Font size for legend text. Default: `14`
#'
#' ## **Panel Strip Settings**
#' - `strip_face`: Character. Font face for panel strip. Default: `"bold"`
#' - `strip_text_size`: Numeric. Font size for panel strip. Default: `16`
#' - `strip_background`: Character. Background color of strip. Default: `"lightgrey"`
#' - `strip_text_margin`: ggplot2 `margin()`. Default: `ggplot2::margin(10, 0, 10, 0)`
#'
#' ## **Panel Layout**
#' - `panel_spacing`: Numeric. Spacing between panels. Default: `2`
#' - `panel_rows`: Numeric or `NULL`. Number of rows in panel layout. Default: `NULL`
#' - `panel_cols`: Numeric or `NULL`. Number of columns in panel layout. Default: `NULL`
#' - `theme`: ggplot2 theme object or `NULL`. Custom ggplot theme. Default: `NULL`
#'
#' ## **Color Settings**
#' - `color_tone`: Character or `NULL`. Base color theme. Default: `NULL`
#' - `positive_color`: Character. Color for positive values. Default: `"#2E8B57"`
#' - `negative_color`: Character. Color for negative values. Default: `"#CD5C5C"`
#' - `background_color`: Character. Background color of plot. Default: `"white"`
#' - `grid_color`: Character. Color of grid lines. Default: `"grey90"`
#' - `show_grid_major_x`: Logical. Show major grid lines on x-axis. Default: `FALSE`
#' - `show_grid_major_y`: Logical. Show major grid lines on y-axis. Default: `TRUE`
#' - `show_grid_minor_x`: Logical. Show minor grid lines on x-axis. Default: `FALSE`
#' - `show_grid_minor_y`: Logical. Show minor grid lines on y-axis. Default: `FALSE`
#'
#' ## **Zero Line Settings**
#' - `show_zero_line`: Logical. Show or hide zero line. Default: `TRUE`
#' - `zero_line_type`: Character. Line type (`"solid"`, `"dashed"`, `"dotted"`). Default: `"dashed"`
#' - `zero_line_color`: Character. Color of zero line. Default: `"black"`
#' - `zero_line_size`: Numeric. Line thickness of zero line. Default: `0.5`
#' - `zero_line_position`: Numeric. Position of the zero line. Default: `0`
#'
#' ## **Bar Chart Settings**
#' - `bar_width`: Numeric. Width of bars. Default: `0.9`
#' - `bar_spacing`: Numeric. Spacing between bars. Default: `0.9`
#'
#' ## **Scale Expansion Settings**
#' - `expansion_y_mult`: Numeric vector. Y-axis expansion. Default: `c(0.05, 0.1)`
#' - `expansion_x_mult`: Numeric vector. X-axis expansion. Default: `c(0.05, 0.05)`
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#' # Get default configuration
#' config <- get_plot_style_config("comparison")
#'
#' # Get information about a specific parameter
#' param_info <- get_plot_style_config("detail", "bar_width")
#'
#' # Get as structured dataframe
#' config_df <- get_plot_style_config("stack", as_dataframe = TRUE)
#'
#' # Validate custom configuration
#' custom_config <- list(title_size = 24, bar_width = 0.7)
#' validated <- get_plot_style_config("comparison", validate_custom = custom_config)
#'
get_plot_style_config <- function(plot_type = "comparison",
                                  parameter_name = NULL,
                                  show_docs = FALSE,
                                  validate_custom = NULL,
                                  as_dataframe = FALSE) {
  config <- .calculate_plot_style_config(NULL, plot_type)

  param_docs <- list(
    show_title = "Logical. Show or hide the plot title.",
    title_face = "Character. Font face for title ('bold', 'plain', 'italic').",
    title_size = "Numeric. Font size of title.",
    title_hjust = "Numeric. Horizontal justification of title (0 = left, 1 = right).",
    add_unit_to_title = "Logical. Add unit information to title.",
    title_margin = "ggplot2 margin object. Margin around title.",
    title_format = "List. Format of title, with elements 'type' and 'text'.",
    show_x_axis_title = "Logical. Show or hide the x-axis title.",
    x_axis_title_face = "Character. Font face for x-axis title.",
    x_axis_title_size = "Numeric. Font size of x-axis title.",
    x_axis_title_margin = "ggplot2 margin object. Margin around x-axis title.",
    show_x_axis_labels = "Logical. Show or hide x-axis tick labels.",
    x_axis_text_face = "Character. Font face for x-axis tick labels.",
    x_axis_text_size = "Numeric. Font size of x-axis tick labels.",
    x_axis_text_angle = "Numeric. Angle of x-axis tick labels in degrees.",
    x_axis_text_hjust = "Numeric. Horizontal justification of x-axis tick labels.",
    x_axis_description = "Character. Optional description for x-axis.",
    show_y_axis_title = "Logical. Show or hide the y-axis title.",
    y_axis_title_face = "Character. Font face for y-axis title.",
    y_axis_title_size = "Numeric. Font size of y-axis title.",
    y_axis_title_margin = "ggplot2 margin object. Margin around y-axis title.",
    show_y_axis_labels = "Logical. Show or hide y-axis tick labels.",
    y_axis_text_face = "Character. Font face for y-axis tick labels.",
    y_axis_text_size = "Numeric. Font size of y-axis tick labels.",
    y_axis_text_angle = "Numeric. Angle of y-axis tick labels in degrees.",
    y_axis_text_hjust = "Numeric. Horizontal justification of y-axis tick labels.",
    y_axis_description = "Character. Optional description for y-axis.",
    show_axis_titles_on_all_facets = "Logical. Show axis titles on all facets.",
    show_value_labels = "Logical. Show or hide value labels.",
    value_label_face = "Character. Font face for value labels.",
    value_label_size = "Numeric. Font size of value labels.",
    value_label_position = "Character. Position of value labels ('above', 'outside', 'top').",
    value_label_decimal_places = "Numeric. Number of decimal places in value labels.",
    show_legend = "Logical. Show or hide the legend.",
    show_legend_title = "Logical. Show or hide the legend title.",
    legend_position = "Character. Position of the legend ('none', 'right', 'bottom', etc.).",
    legend_title_face = "Character. Font face for legend title.",
    legend_text_face = "Character. Font face for legend text.",
    legend_text_size = "Numeric. Font size of legend text.",
    strip_face = "Character. Font face for panel strip labels.",
    strip_text_size = "Numeric. Font size of panel strip labels.",
    strip_background = "Character. Background color of panel strips.",
    strip_text_margin = "ggplot2 margin object. Margin around panel strip labels.",
    panel_spacing = "Numeric. Spacing between panels in centimeters.",
    panel_rows = "Numeric or NULL. Number of rows in panel layout.",
    panel_cols = "Numeric or NULL. Number of columns in panel layout.",
    theme = "ggplot2 theme object or NULL. Custom theme to apply.",
    color_tone = "Character or NULL. Base color tone for the plot (e.g., 'academic', 'purdue').",
    positive_color = "Character. Color for positive values.",
    negative_color = "Character. Color for negative values.",
    background_color = "Character. Background color of the plot.",
    grid_color = "Character. Color of grid lines.",
    show_grid_major_x = "Logical. Show major grid lines on x-axis.",
    show_grid_major_y = "Logical. Show major grid lines on y-axis.",
    show_grid_minor_x = "Logical. Show minor grid lines on x-axis.",
    show_grid_minor_y = "Logical. Show minor grid lines on y-axis.",
    show_zero_line = "Logical. Show or hide the zero line.",
    zero_line_type = "Character. Line type for zero line ('solid', 'dashed', 'dotted').",
    zero_line_color = "Character. Color of zero line.",
    zero_line_size = "Numeric. Line thickness of zero line.",
    zero_line_position = "Numeric. Position of the zero line.",
    bar_width = "Numeric. Width of bars (0-1).",
    bar_spacing = "Numeric. Spacing between groups of bars.",
    expansion_y_mult = "Numeric vector of length 2. Expansion multiplier for y-axis.",
    expansion_x_mult = "Numeric vector of length 2. Expansion multiplier for x-axis."
  )

  if (!is.null(parameter_name)) {
    if (parameter_name %in% names(config)) {
      result <- list(value = config[[parameter_name]])

      if (show_docs && parameter_name %in% names(param_docs)) {
        result$documentation <- param_docs[[parameter_name]]
      }

      return(result)
    } else {
      all_params <- names(config)
      distances <- stringdist::stringdist(parameter_name, all_params, method = "lv")
      closest_matches <- all_params[order(distances)][1:3]

      warning(sprintf("Parameter '%s' not found. Did you mean: %s?",
                      parameter_name,
                      paste(closest_matches, collapse = ", ")))

      return(NULL)
    }
  }

  if (!is.null(validate_custom)) {
    if (!is.list(validate_custom)) {
      warning("validate_custom must be a list. Ignoring validation.")
    } else {
      invalid_params <- setdiff(names(validate_custom), names(config))

      if (length(invalid_params) > 0) {
        suggestions <- lapply(invalid_params, function(param) {
          distances <- stringdist::stringdist(param, names(config), method = "lv")
          closest_matches <- names(config)[order(distances)][1:3]
          list(
            invalid = param,
            suggestions = closest_matches
          )
        })

        suggestion_msgs <- sapply(suggestions, function(sugg) {
          sprintf("- '%s': Did you mean %s?",
                  sugg$invalid,
                  paste(sprintf("'%s'", sugg$suggestions), collapse = ", "))
        })

        warning(paste("Invalid parameters found in custom configuration:",
                      paste(suggestion_msgs, collapse = "\n"), sep = "\n"))

        valid_params <- intersect(names(validate_custom), names(config))
        valid_custom <- validate_custom[valid_params]

        return(valid_custom)
      } else {
        return(validate_custom)
      }
    }
  }

  if (show_docs) {
    result <- lapply(names(config), function(param) {
      param_info <- list(value = config[[param]])

      if (param %in% names(param_docs)) {
        param_info$documentation <- param_docs[[param]]
      }

      return(param_info)
    })

    names(result) <- names(config)
    return(result)
  }

  if (as_dataframe) {
    param_categories <- list(
      "Title" = c("show_title", "title_face", "title_size", "title_hjust",
                  "add_unit_to_title", "title_margin", "title_format"),

      "X-Axis" = c("show_x_axis_title", "x_axis_title_face", "x_axis_title_size",
                   "x_axis_title_margin", "show_x_axis_labels", "x_axis_text_face",
                   "x_axis_text_size", "x_axis_text_angle", "x_axis_text_hjust",
                   "x_axis_description"),

      "Y-Axis" = c("show_y_axis_title", "y_axis_title_face", "y_axis_title_size",
                   "y_axis_title_margin", "show_y_axis_labels", "y_axis_text_face",
                   "y_axis_text_size", "y_axis_text_angle", "y_axis_text_hjust",
                   "y_axis_description", "show_axis_titles_on_all_facets"),

      "Value Labels" = c("show_value_labels", "value_label_face", "value_label_size",
                         "value_label_position", "value_label_decimal_places"),

      "Legend" = c("show_legend", "show_legend_title", "legend_position",
                   "legend_title_face", "legend_text_face", "legend_text_size"),

      "Panel Strip" = c("strip_face", "strip_text_size", "strip_background", "strip_text_margin"),

      "Panel Layout" = c("panel_spacing", "panel_rows", "panel_cols", "theme"),

      "Colors" = c("color_tone", "positive_color", "negative_color", "background_color",
                   "grid_color", "show_grid_major_x", "show_grid_major_y",
                   "show_grid_minor_x", "show_grid_minor_y"),

      "Zero Line" = c("show_zero_line", "zero_line_type", "zero_line_color",
                      "zero_line_size", "zero_line_position"),

      "Bar Chart" = c("bar_width", "bar_spacing"),

      "Scale Expansion" = c("expansion_y_mult", "expansion_x_mult")
    )

    result_cols <- c("Category", "Parameter", "Value", "Type", "Description", "Example")

    result <- data.frame(matrix(ncol = length(result_cols), nrow = 0))
    colnames(result) <- result_cols

    for (category in names(param_categories)) {
      params <- param_categories[[category]]

      for (param in params) {
        if (param %in% names(config)) {
          param_value <- config[[param]]

          if (inherits(param_value, "unit")) {
            param_value <- paste("margin:", paste(as.numeric(param_value), collapse = ","))
          } else if (is.list(param_value) && !is.data.frame(param_value)) {
            param_value <- paste(utils::capture.output(utils::str(param_value, max.level = 1)), collapse = " ")
          }

          param_type <- class(config[[param]])[1]

          description <- if (param %in% names(param_docs)) param_docs[[param]] else ""

          example <- paste0(param, " = ", if(is.character(param_value)) {
            paste0('"', param_value, '"')
          } else if(is.logical(param_value)) {
            ifelse(param_value, "TRUE", "FALSE")
          } else if(is.numeric(param_value) && length(param_value) == 1) {
            as.character(param_value)
          } else if(is.numeric(param_value) && length(param_value) > 1) {
            paste0("c(", paste(param_value, collapse = ", "), ")")
          } else if(is.list(param_value)) {
            "list(...)"
          } else {
            as.character(param_value)
          })

          row_data <- c(category, param, paste(param_value, collapse = ", "), param_type, description, example)
          result <- rbind(result, row_data)
        }
      }
    }

    rownames(result) <- NULL

    attr(result, "plot_type") <- plot_type

    colnames(result) <- c("Topic", "Arguments", "Default Value", "Input Format", "Description", "Example")

    if (nrow(result) > 0) {
      current_topic <- result$Topic[1]
      for (i in 2:nrow(result)) {
        if (result$Topic[i] == current_topic) {
          result$Topic[i] <- ""
        } else {
          current_topic <- result$Topic[i]
        }
      }
    }

    return(result)
  }

  return(c(list(plot_type = plot_type), config))
}

# Comparison Plot ---------------------------------------------------------

#' @title Create Comparative Bar Charts from HAR and SL4 Data
#'
#' @description
#' Generates comparative bar charts using GTAP data, allowing multiple visualization options
#' such as panel facets, split grouping, and customizable styles.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param split_by Character or vector. Column name(s) for data splitting (e.g., "COMM", "REG", "ACTS").
#' Set to NULL for no splitting, which is suitable for macro-level analysis (i.e., aggregated values without additional dimensions).
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param output_dir Character. Directory path to save plots as `.png` files. If NULL, plots are only returned in R without saving.
#' @param width Numeric. Width of the output figure in inches. If NULL, it is calculated automatically.
#' @param height Numeric. Height of the output figure in inches. If NULL, it is calculated automatically.
#' @param plot_style_config List. Custom style configuration for plots. If NULL, defaults from `get_plot_style_config("comparison")` are applied.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @details
#' This function allows for extensive customization of plots through the `plot_style_config` parameter,
#' which integrates ggplot2 configurations.
#' To view and adjust available style options, use:
#' `get_plot_style_config("comparison", as_dataframe = TRUE)`.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{detail_plot}}, \code{\link{stack_plot}}, \code{\link{get_plot_style_config}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with data frame
#' p1 <- comparison_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   panel_var = "Experiment"
#' )
#'
#' # Split by commodity with custom styling
#' p2 <- comparison_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   split_by = "COMM",
#'   panel_var = "Experiment",
#'   var_name_by_description = TRUE,
#'   plot_style_config = list(
#'     color_tone = "economic",
#'     title_size = 16,
#'     show_grid_major_y = TRUE
#'   )
#' )
#'
#' # Save plots to directory with inverted orientation
#' plots <- comparison_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   split_by = "COMM",
#'   invert_pane = TRUE,
#'   output_dir = "path/to/output/directory"
#' )
#' }
#'
comparison_plot <- function(data, filter_var = NULL,
                            x_axis_from,
                            split_by = NULL,
                            panel_var = "Experiment",
                            variable_col = "Variable",
                            unit_col = "Unit",
                            desc_col = "Description",
                            invert_pane = FALSE,
                            separate_figure = FALSE,
                            var_name_by_description = FALSE,
                            add_var_info = FALSE,
                            output_dir = NULL,
                            width = NULL,
                            height = NULL,
                            plot_style_config = NULL) {

  # PREPARE DATA SOURCE
  if (is.list(data) && !is.data.frame(data)) {
    data_found <- FALSE
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df) && x_axis_from %in% names(df)) {
        data <- df
        data_found <- TRUE
        break
      }
    }

    if (!data_found) {
      stop(paste("No suitable dataframe found with column:", x_axis_from))
    }
  }

  # CHECK FOR UNIT COLUMN
  if (!(unit_col %in% names(data))) {
    stop("Missing 'Unit' column in data frame. See add_mapping_info for help.")
  }

  # PROCESS SPLIT_BY PARAMETER
  is_macro_mode <- FALSE
  if (is.null(split_by) || (is.logical(split_by) && !split_by)) {
    is_macro_mode <- TRUE
  } else {
    if (length(split_by) > 1) {
      for (col in split_by) {
        if (!(col %in% names(data))) {
          warning(paste("Split-by column", col, "not found. Creating default column."))
          data[[col]] <- "Default"
        }
      }
    } else {
      if (!(split_by %in% names(data))) {
        warning(paste("Split-by column", split_by, "not found. Creating default column."))
        data[[split_by]] <- "Default"
      }
    }
  }

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      data <- data[data[[x_axis_from]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    if (var_name_by_description || add_var_info) {
      result <- data

      for (i in seq_len(nrow(result))) {
        var_ <- result[[variable_col]][i]
        des_ <- result[[desc_col]][i]

        if (is.na(des_) || !nzchar(des_))
          des_ <- var_

        if (var_name_by_description && add_var_info) {
          result[[variable_col]][i] <- paste0(des_, " (", var_, ")")
        } else if (var_name_by_description && !add_var_info) {
          result[[variable_col]][i] <- des_
        } else if (!var_name_by_description && add_var_info) {
          if (des_ != var_) {
            result[[variable_col]][i] <- paste0(var_, " (", des_, ")")
          }
        }
      }

      data <- result
    }

    # Create mapping from original Variable to formatted Variable for titles
    unique_vars <- unique(data.frame(
      OrigVar = data[[variable_col]],
      stringsAsFactors = FALSE
    ))
    title_mapping <- setNames(as.list(unique_vars$OrigVar), unique_vars$OrigVar)
  } else {
    title_mapping <- setNames(as.list(unique(data[[variable_col]])), unique(data[[variable_col]]))
  }

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    .calculate_plot_style_config(plot_style_config, "comparison")
  } else {
    .calculate_plot_style_config(NULL, "comparison")
  }

  # PROCESS BY UNIT GROUPS (different units need separate plots)
  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    # HANDLE MACRO MODE (no split_by)
    if (is_macro_mode) {
      if (separate_figure) {
        if (variable_col %in% names(unit_data)) {
          panel_list <- split(unit_data, unit_data[[variable_col]])
        } else {
          panel_list <- list(data = unit_data)
        }

        for (panel_name in names(panel_list)) {
          panel_data <- panel_list[[panel_name]]

          # Calculate dimensions - use direct values from style_config
          panel_layout <- .calculate_panel_layout(panel_data, style_config$panel_rows, style_config$panel_cols, panel_var)

          if (is.null(width) || is.null(height)) {
            dims <- .calculate_plot_dimensions(panel_data, panel_layout)
            width_val <- ifelse(is.null(width), dims$width, width)
            height_val <- ifelse(is.null(height), dims$height, height)
          } else {
            width_val <- width
            height_val <- height
          }

          # Format title
          panel_title <- if (variable_col %in% names(panel_data) && panel_name %in% names(title_mapping)) {
            title_mapping[[panel_name]]
          } else {
            panel_name
          }

          plot_title <- panel_title

          # Apply title format if specified
          if (!is.null(style_config$title_format)) {
            if (style_config$title_format$type == "prefix") {
              plot_title <- paste0(style_config$title_format$text, " ", plot_title)
            } else if (style_config$title_format$type == "suffix") {
              plot_title <- paste0(plot_title, " ", style_config$title_format$text)
            } else if (style_config$title_format$type == "full") {
              plot_title <- style_config$title_format$text
            }
          }

          # Add unit to title if configured
          if (style_config$add_unit_to_title) {
            if (tolower(unit_name) == "percent") {
              plot_title <- paste0(plot_title, " (%)")
            } else {
              plot_title <- paste0(plot_title, " (", unit_name, ")")
            }
          }

          # Create plot - use panel_layout for dimensions
          p <- .create_single_comparison_plot(
            data = panel_data,
            x_axis_from = x_axis_from,
            plot_title = plot_title,
            unit = unit_name,
            panel_rows = panel_layout$rows,
            panel_cols = panel_layout$cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            plot_style_config = style_config
          )

          # Save plot if output_dir provided
          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
            clean_panel <- gsub("[^[:alnum:]]", "_", panel_name)
            clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
            filename <- file.path(output_dir, paste0("macro_", clean_panel, "_", clean_unit, ".png"))
            ggplot2::ggsave(filename, p, width = width_val, height = height_val, dpi = 300, bg = "white", limitsize = FALSE)
            message("Saved plot: ", filename)
          }

          plot_list[[paste("macro", panel_name, unit_name, sep = "_")]] <- p
        }
      } else {
        # Calculate panel layout - use direct values from style_config
        panel_layout <- .calculate_panel_layout(unit_data, style_config$panel_rows, style_config$panel_cols, panel_var)

        # Auto-calculate dimensions
        if (is.null(width) || is.null(height)) {
          dims <- .calculate_plot_dimensions(unit_data, panel_layout)
          width_val <- ifelse(is.null(width), dims$width, width)
          height_val <- ifelse(is.null(height), dims$height, height)
        } else {
          width_val <- width
          height_val <- height
        }

        # Format title for combined plot
        plot_title <- "Global Economic Impacts"

        # Apply title format if specified
        if (!is.null(style_config$title_format)) {
          if (style_config$title_format$type == "prefix") {
            plot_title <- paste0(style_config$title_format$text, " ", plot_title)
          } else if (style_config$title_format$type == "suffix") {
            plot_title <- paste0(plot_title, " ", style_config$title_format$text)
          } else if (style_config$title_format$type == "full") {
            plot_title <- style_config$title_format$text
          }
        }

        # Add unit to title if configured
        if (style_config$add_unit_to_title) {
          if (tolower(unit_name) == "percent") {
            plot_title <- paste0(plot_title, " (%)")
          } else {
            plot_title <- paste0(plot_title, " (", unit_name, ")")
          }
        }

        # Create plot - use panel_layout for dimensions
        p <- .create_single_comparison_plot(
          data = unit_data,
          x_axis_from = x_axis_from,
          plot_title = plot_title,
          unit = unit_name,
          panel_rows = panel_layout$rows,
          panel_cols = panel_layout$cols,
          panel_var = panel_var,
          invert_pane = invert_pane,
          plot_style_config = style_config
        )

        # Save plot if output_dir provided
        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
          clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
          filename <- file.path(output_dir, paste0("GTAPMacros_", clean_unit, ".png"))
          ggplot2::ggsave(filename, p, width = width_val, height = height_val, dpi = 300, bg = "white", limitsize = FALSE)
          message("Saved plot: ", filename)
        }

        plot_list[[paste("macro", unit_name, sep = "_")]] <- p
      }
    } else {
      # HANDLE SPLIT_BY MODE
      # Get unique values for separate plots
      if (length(split_by) > 1) {
        # For multiple split_by columns, create a display name that combines all values
        unit_data$split_display <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
        separate_values <- unique(unit_data$split_display)
        split_col <- "split_display"
      } else {
        separate_values <- unique(unit_data[[split_by]])
        split_col <- split_by
      }

      for (sep_value in separate_values) {
        # Filter data for the current separate value
        if (split_col == "split_display") {
          filtered_data <- unit_data[unit_data$split_display == sep_value, ]
        } else {
          filtered_data <- unit_data[unit_data[[split_col]] == sep_value, ]
        }

        # Calculate panel layout - use direct values from style_config
        if (separate_figure) {
          panel_values <- unique(filtered_data[[panel_var]])

          # Create separate figures for each panel value
          for (panel_val in panel_values) {
            panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]

            # Calculate dimensions - use layout directly from style_config
            panel_layout <- .calculate_panel_layout(panel_data, style_config$panel_rows, style_config$panel_cols, panel_var)

            if (is.null(width) || is.null(height)) {
              dims <- .calculate_plot_dimensions(panel_data, panel_layout)
              width_val <- ifelse(is.null(width), dims$width, width)
              height_val <- ifelse(is.null(height), dims$height, height)
            } else {
              width_val <- width
              height_val <- height
            }

            # Format title
            plot_title <- paste0(sep_value, " - ", panel_val)

            # Apply title format if specified
            if (!is.null(style_config$title_format)) {
              if (style_config$title_format$type == "prefix") {
                plot_title <- paste0(style_config$title_format$text, " ", plot_title)
              } else if (style_config$title_format$type == "suffix") {
                plot_title <- paste0(plot_title, " ", style_config$title_format$text)
              } else if (style_config$title_format$type == "full") {
                plot_title <- style_config$title_format$text
              }
            }

            # Add unit to title if configured
            if (style_config$add_unit_to_title) {
              if (tolower(unit_name) == "percent") {
                plot_title <- paste0(plot_title, " (%)")
              } else {
                plot_title <- paste0(plot_title, " (", unit_name, ")")
              }
            }

            # Create plot
            p <- .create_single_comparison_plot(
              data = panel_data,
              x_axis_from = x_axis_from,
              plot_title = plot_title,
              unit = unit_name,
              panel_rows = panel_layout$rows,
              panel_cols = panel_layout$cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              plot_style_config = style_config
            )

            # Save plot if output_dir provided
            if (!is.null(output_dir)) {
              if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
              clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
              clean_panel <- gsub("[^[:alnum:]]", "_", panel_val)
              clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
              filename <- file.path(output_dir, paste0(clean_sep, "_", clean_panel, "_", clean_unit, ".png"))
              ggplot2::ggsave(filename, p, width = width_val, height = height_val, dpi = 300, bg = "white", limitsize = FALSE)
              message("Saved plot: ", filename)
            }

            plot_list[[paste(sep_value, panel_val, unit_name, sep = "_")]] <- p
          }
        } else {
          # Panel plot for all values in the group
          panel_layout <- .calculate_panel_layout(filtered_data, style_config$panel_rows, style_config$panel_cols, panel_var)

          # Calculate dimensions
          if (is.null(width) || is.null(height)) {
            dims <- .calculate_plot_dimensions(filtered_data, panel_layout)
            width_val <- ifelse(is.null(width), dims$width, width)
            height_val <- ifelse(is.null(height), dims$height, height)
          } else {
            width_val <- width
            height_val <- height
          }

          # Format title
          plot_title <- sep_value

          # Apply title format if specified
          if (!is.null(style_config$title_format)) {
            if (style_config$title_format$type == "prefix") {
              plot_title <- paste0(style_config$title_format$text, " ", plot_title)
            } else if (style_config$title_format$type == "suffix") {
              plot_title <- paste0(plot_title, " ", style_config$title_format$text)
            } else if (style_config$title_format$type == "full") {
              plot_title <- style_config$title_format$text
            }
          }

          # Add unit to title if configured
          if (style_config$add_unit_to_title) {
            if (tolower(unit_name) == "percent") {
              plot_title <- paste0(plot_title, " (%)")
            } else {
              plot_title <- paste0(plot_title, " (", unit_name, ")")
            }
          }

          # Create plot
          p <- .create_single_comparison_plot(
            data = filtered_data,
            x_axis_from = x_axis_from,
            plot_title = plot_title,
            unit = unit_name,
            panel_rows = panel_layout$rows,
            panel_cols = panel_layout$cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            plot_style_config = style_config
          )

          # Save plot if output_dir provided
          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
            clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
            clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
            filename <- file.path(output_dir, paste0(clean_sep, "_", clean_unit, ".png"))
            ggplot2::ggsave(filename, p, width = width_val, height = height_val, dpi = 300, bg = "white", limitsize = FALSE)
            message("Saved plot: ", filename)
          }

          plot_list[[paste(sep_value, unit_name, sep = "_")]] <- p
        }
      }
    }
  }

  # Return single plot or list of plots based on number of plots
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}


#' @title Create Single Comparison Plot (Internal)
#'
#' @description
#' Create a single comparison plot from GTAP data for internal use.
#'
#' @param data A data frame containing plotting data.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Numeric. Number of rows in panel layout.
#' @param panel_cols Numeric. Number of columns in panel layout.
#' @param panel_var Character. Column for panel facets. Default is "Experiment".
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the comparison plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}
#'
.create_single_comparison_plot <- function(data, x_axis_from, plot_title, unit,
                                           panel_rows, panel_cols,
                                           panel_var = "Experiment",
                                           invert_pane = FALSE,
                                           plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "comparison")
  }

  # SET UP VARIABLES FOR PLOTTING
  x_var <- x_axis_from
  facet_var <- panel_var

  n_panels <- length(unique(data[[facet_var]]))

  # CALCULATE Y-AXIS LIMITS
  value_range <- range(data$Value)
  y_range <- diff(value_range)

  # Add more padding to ensure no clipping of bars
  y_limits <- if (tolower(unit) == "percent") {
    max_abs_value <- max(abs(value_range))
    c(-max_abs_value * 1.35, max_abs_value * 1.35)
  } else {
    # More generous margins to avoid clipping
    if (all(data$Value >= 0)) {
      # For all positive values
      c(0, value_range[2] * 1.3)
    } else if (all(data$Value <= 0)) {
      # For all negative values
      c(value_range[1] * 1.3, 0)
    } else {
      # For mixed values
      c(value_range[1] * 1.3, value_range[2] * 1.3)
    }
  }

  # FORMAT AXIS LABELS
  # y-axis label shows the unit
  y_axis_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  # x-axis label uses column name if no description provided
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    x_axis_from
  }

  # CALCULATE LABEL POSITIONS
  if (invert_pane) {
    # For horizontal bars (coord_flip), need more space for labels
    label_position <- sapply(data$Value, function(x) {
      if (x >= 0) x + diff(y_limits) * 0.08 else x - diff(y_limits) * 0.08
    })
  } else {
    # For vertical bars, regular spacing
    label_position <- sapply(data$Value, function(x) {
      if (x >= 0) x + diff(y_limits) * 0.03 else x - diff(y_limits) * 0.03
    })
  }

  # GENERATE COLORS IF PROVIDED
  if (!is.null(style_config$color_tone)) {
    color_palette <- .generate_comparison_colors(data, style_config$color_tone, x_var)
  }

  # GET BAR STYLING FROM CONFIG
  bar_width <- style_config$bar_width
  bar_spacing <- style_config$bar_spacing

  # CREATE THE BASIC PLOT
  if (invert_pane) {
    # For horizontal bars (flipped coordinates)
    p <- ggplot2::ggplot(data, ggplot2::aes_string(
      y = x_var,
      x = "Value",
      fill = x_var)) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = bar_spacing),
                        width = bar_width)

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = label_position,
                     label = sprintf(paste0("%.", decimal_places, "f"), Value)),
        position = ggplot2::position_dodge(width = bar_spacing),
        size = value_size,
        color = "black"
      )
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    p <- p + ggplot2::scale_x_continuous(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # ADD ZERO LINE IF CONFIGURED (VERTICAL LINE FOR HORIZONTAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }
  } else {
    # For vertical bars (normal orientation)
    p <- ggplot2::ggplot(data, ggplot2::aes_string(
      x = x_var,
      y = "Value",
      fill = x_var)) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = bar_spacing),
                        width = bar_width)

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        ggplot2::aes(y = label_position,
                     label = sprintf(paste0("%.", decimal_places, "f"), Value)),
        position = ggplot2::position_dodge(width = bar_spacing),
        size = value_size,
        color = "black"
      )
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    p <- p + ggplot2::scale_y_continuous(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # ADD ZERO LINE IF CONFIGURED (HORIZONTAL LINE FOR VERTICAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }
  }

  # APPLY COLORS IF PROVIDED
  if (!is.null(style_config$color_tone)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  # ADD FACET WRAP IF WE HAVE MULTIPLE PANELS
  if (n_panels > 1) {
    p <- p + ggplot2::facet_wrap(
      as.formula(paste("~", facet_var)),
      scales = if (style_config$show_axis_titles_on_all_facets) "free" else "fixed",
      nrow = panel_rows,
      ncol = panel_cols
    )
  }

  # APPLY THEME STYLING
  p <- p + ggplot2::theme_minimal()
  p <- .apply_plot_style_config(p, style_config)

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  return(p)
}


# Detail Plot -------------------------------------------------------------

#' @title Create Comprehensive Detailed Bar Charts for Impact Distribution
#'
#' @description
#' Generates detailed bar charts to visualize the distribution of impacts across multiple dimensions,
#' automatically handling multi-dimensional data. The function supports top impact filtering,
#' color coding for positive and negative values, and flexible visualization settings.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param split_by Character or vector. Column name(s) for data splitting (e.g., "COMM", "REG", "ACTS").
#' Set to NULL for no splitting, which is suitable for macro-level analysis (i.e., aggregated values without additional dimensions).
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param output_dir Character. Directory path to save plots as `.png` files. If NULL, plots are only returned in R without saving.
#' @param top_impact Numeric or NULL. If specified, filters to show only the top N impactful values. NULL shows all values.
#' @param width Numeric. Width of the output figure in inches. If NULL, it is calculated automatically.
#' @param height Numeric. Height of the output figure in inches. If NULL, it is calculated automatically.
#' @param separate_figure Logical. If TRUE, creates a separate figure for each panel value (default: FALSE).
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param plot_style_config List. Custom style configuration for plots. If NULL, defaults from `get_plot_style_config("detail")` are applied.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @details
#' This function allows for extensive customization of plots through the `plot_style_config` parameter,
#' which integrates ggplot2 configurations.
#' To view and adjust available style options, use:
#' `get_plot_style_config("detail", as_dataframe = TRUE)`.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{comparison_plot}}, \code{\link{stack_plot}}, \code{\link{get_plot_style_config}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage showing all impacts
#' p1 <- detail_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   variable_col = "Variable",
#'   top_impact = NULL
#' )
#'
#' # Show only top 10 impacts (balanced between positive and negative)
#' p2 <- detail_plot(
#'   data = gtap_results,
#'   x_axis_from = "Sector",
#'   split_by = "Region",
#'   panel_var = "Experiment",
#'   variable_col = "Variable",
#'   top_impact = 10,
#'   invert_pane = TRUE
#' )
#'
#' # Create detailed plots split by commodity
#' plots <- detail_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   split_by = "COMM",
#'   variable_col = "Variable",
#'   var_name_by_description = TRUE,
#'   add_var_info = TRUE,
#'   output_dir = "path/to/output/directory"
#' )
#' }
#'
detail_plot <- function(data, filter_var = NULL,
                        x_axis_from,
                        split_by = NULL,
                        panel_var = "Experiment",
                        variable_col = "Variable",
                        unit_col = "Unit",
                        desc_col = "Description",
                        var_name_by_description = FALSE,
                        add_var_info = FALSE,
                        output_dir = NULL,
                        top_impact = NULL,
                        width = NULL,
                        height = NULL,
                        separate_figure = FALSE,
                        invert_pane = FALSE,
                        plot_style_config = NULL) {

  # PREPARE DATA SOURCE
  if (is.list(data) && !is.data.frame(data)) {
    data_found <- FALSE
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df) && x_axis_from %in% names(df)) {
        data <- df
        data_found <- TRUE
        break
      }
    }

    if (!data_found) {
      stop(paste("No suitable dataframe found with column:", x_axis_from))
    }
  }

  # CHECK FOR REQUIRED COLUMNS
  if (!(unit_col %in% names(data))) {
    stop("Missing 'Unit' column in data frame. See add_mapping_info for help.")
  }

  if (!("Value" %in% names(data))) {
    stop("Missing 'Value' column in data frame.")
  }

  # PROCESS SPLIT_BY PARAMETER
  is_macro_mode <- FALSE
  if (is.null(split_by) || (is.logical(split_by) && !split_by)) {
    is_macro_mode <- TRUE
  } else {
    if (length(split_by) > 1) {
      for (col in split_by) {
        if (!(col %in% names(data))) {
          warning(paste("Split-by column", col, "not found. Creating default column."))
          data[[col]] <- "Default"
        }
      }
    } else {
      if (!(split_by %in% names(data))) {
        warning(paste("Split-by column", split_by, "not found. Creating default column."))
        data[[split_by]] <- "Default"
      }
    }
  }

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      data <- data[data[[x_axis_from]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    if (var_name_by_description || add_var_info) {
      result <- data

      for (i in seq_len(nrow(result))) {
        var_ <- result[[variable_col]][i]
        des_ <- result[[desc_col]][i]

        if (is.na(des_) || !nzchar(des_))
          des_ <- var_

        if (var_name_by_description && add_var_info) {
          result[[variable_col]][i] <- paste0(des_, " (", var_, ")")
        } else if (var_name_by_description && !add_var_info) {
          result[[variable_col]][i] <- des_
        } else if (!var_name_by_description && add_var_info) {
          if (des_ != var_) {
            result[[variable_col]][i] <- paste0(var_, " (", des_, ")")
          }
        }
      }

      data <- result
    }

    # Create mapping from original Variable to formatted Variable for titles
    unique_vars <- unique(data.frame(
      OrigVar = data[[variable_col]],
      stringsAsFactors = FALSE
    ))
    title_mapping <- setNames(as.list(unique_vars$OrigVar), unique_vars$OrigVar)
  } else {
    title_mapping <- setNames(as.list(unique(data[[variable_col]])), unique(data[[variable_col]]))
  }

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    .calculate_plot_style_config(plot_style_config, "detail")
  } else {
    .calculate_plot_style_config(NULL, "detail")
  }

  # APPLY TOP_IMPACT FILTER
  if (!is.null(top_impact)) {
    if (!is_macro_mode && length(split_by) > 1) {
      data$._split_group_ <- apply(data[, split_by, drop = FALSE], 1, paste, collapse = "-")
      top_impact_filter_col <- "._split_group_"
    } else if (!is_macro_mode) {
      top_impact_filter_col <- split_by
    } else {
      top_impact_filter_col <- x_axis_from
    }

    # Pass all required parameters to the filter function
    data <- .filter_top_impact_values_detail(
      data = data,
      top_impact = top_impact,
      group_col = top_impact_filter_col,
      panel_var = panel_var,
      x_axis_from = x_axis_from,
      variable_col = variable_col,
      unit_col = unit_col
    )

    # Clean up temporary column if created
    if ("._split_group_" %in% names(data)) {
      data$._split_group_ <- NULL
    }
  }

  # PROCESS BY UNIT GROUPS (different units need separate plots)
  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    # HANDLE MACRO MODE (no split_by)
    if (is_macro_mode) {
      if (separate_figure) {
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]
          panel_values <- unique(var_data[[panel_var]])

          for (panel_val in panel_values) {
            panel_data <- var_data[var_data[[panel_var]] == panel_val, ]

            # Calculate panel layout using direct style_config values
            panel_layout <- .calculate_panel_layout(panel_data, style_config$panel_rows, style_config$panel_cols, panel_var)

            if (is.null(width) || is.null(height)) {
              dims <- .calculate_plot_dimensions(panel_data, panel_layout)
              width_val <- ifelse(is.null(width), dims$width, width)
              height_val <- ifelse(is.null(height), dims$height, height)
            } else {
              width_val <- width
              height_val <- height
            }

            # Format title
            plot_title <- paste0(var_name, " (", panel_val, ")")

            # Apply title format if specified
            if (!is.null(style_config$title_format)) {
              if (style_config$title_format$type == "prefix") {
                plot_title <- paste0(style_config$title_format$text, " ", plot_title)
              } else if (style_config$title_format$type == "suffix") {
                plot_title <- paste0(plot_title, " ", style_config$title_format$text)
              } else if (style_config$title_format$type == "full") {
                plot_title <- style_config$title_format$text
              }
            }

            # Add unit to title if configured
            if (style_config$add_unit_to_title) {
              if (tolower(unit_name) == "percent") {
                plot_title <- paste0(plot_title, " (%)")
              } else {
                plot_title <- paste0(plot_title, " (", unit_name, ")")
              }
            }

            # Create plot
            p <- .create_single_detail_plot(
              data = panel_data,
              x_axis_from = x_axis_from,
              plot_title = plot_title,
              unit = unit_name,
              panel_rows = panel_layout$rows,
              panel_cols = panel_layout$cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            # Save plot if output_dir provided
            if (!is.null(output_dir)) {
              if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
              clean_var <- gsub("[^[:alnum:]]", "_", var_name)
              clean_panel <- gsub("[^[:alnum:]]", "_", panel_val)
              clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
              top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

              filename <- file.path(output_dir, paste0("detail_", clean_var, "_", clean_panel, "_",
                                                       clean_unit, top_n_suffix, ".png"))
              ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                              dpi = 300, bg = "white", limitsize = FALSE)
              message("Saved plot: ", filename)
            }

            plot_list[[paste("detail", var_name, panel_val, unit_name, sep = "_")]] <- p
          }
        }
      } else {
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]

          # Calculate panel layout using direct style_config values
          panel_layout <- .calculate_panel_layout(var_data, style_config$panel_rows, style_config$panel_cols, panel_var)

          if (is.null(width) || is.null(height)) {
            dims <- .calculate_plot_dimensions(var_data, panel_layout)
            width_val <- ifelse(is.null(width), dims$width, width)
            height_val <- ifelse(is.null(height), dims$height, height)
          } else {
            width_val <- width
            height_val <- height
          }

          # Format title
          plot_title <- var_name

          # Apply title format if specified
          if (!is.null(style_config$title_format)) {
            if (style_config$title_format$type == "prefix") {
              plot_title <- paste0(style_config$title_format$text, " ", plot_title)
            } else if (style_config$title_format$type == "suffix") {
              plot_title <- paste0(plot_title, " ", style_config$title_format$text)
            } else if (style_config$title_format$type == "full") {
              plot_title <- style_config$title_format$text
            }
          }

          # Add unit to title if configured
          if (style_config$add_unit_to_title) {
            if (tolower(unit_name) == "percent") {
              plot_title <- paste0(plot_title, " (%)")
            } else {
              plot_title <- paste0(plot_title, " (", unit_name, ")")
            }
          }

          # Create plot
          p <- .create_single_detail_plot(
            data = var_data,
            x_axis_from = x_axis_from,
            plot_title = plot_title,
            unit = unit_name,
            panel_rows = panel_layout$rows,
            panel_cols = panel_layout$cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            top_impact = top_impact,
            plot_style_config = style_config
          )

          # Save plot if output_dir provided
          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
            clean_var <- gsub("[^[:alnum:]]", "_", var_name)
            clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
            top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

            filename <- file.path(output_dir, paste0("detail_", clean_var, "_",
                                                     clean_unit, top_n_suffix, ".png"))
            ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                            dpi = 300, bg = "white", limitsize = FALSE)
            message("Saved plot: ", filename)
          }

          plot_list[[paste("detail", var_name, unit_name, sep = "_")]] <- p
        }
      }
    } else {
      # HANDLE SPLIT_BY MODE
      if (length(split_by) > 1) {
        unit_data$split_display <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
        separate_values <- unique(unit_data$split_display)
        split_col <- "split_display"
      } else {
        separate_values <- unique(unit_data[[split_by]])
        split_col <- split_by
      }

      for (sep_value in separate_values) {
        if (split_col == "split_display") {
          filtered_data <- unit_data[unit_data$split_display == sep_value, ]
        } else {
          filtered_data <- unit_data[unit_data[[split_col]] == sep_value, ]
        }

        var_combinations <- unique(filtered_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- filtered_data[filtered_data[[variable_col]] == var_name, ]

          if (separate_figure) {
            panel_values <- unique(var_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_data <- var_data[var_data[[panel_var]] == panel_val, ]

              # Calculate panel layout using direct style_config values
              panel_layout <- .calculate_panel_layout(panel_data, style_config$panel_rows, style_config$panel_cols, panel_var)

              if (is.null(width) || is.null(height)) {
                dims <- .calculate_plot_dimensions(panel_data, panel_layout)
                width_val <- ifelse(is.null(width), dims$width, width)
                height_val <- ifelse(is.null(height), dims$height, height)
              } else {
                width_val <- width
                height_val <- height
              }

              # Format title
              plot_title <- paste0(sep_value, " - ", var_name, " - ", panel_val)

              # Apply title format if specified
              if (!is.null(style_config$title_format)) {
                if (style_config$title_format$type == "prefix") {
                  plot_title <- paste0(style_config$title_format$text, " ", plot_title)
                } else if (style_config$title_format$type == "suffix") {
                  plot_title <- paste0(plot_title, " ", style_config$title_format$text)
                } else if (style_config$title_format$type == "full") {
                  plot_title <- style_config$title_format$text
                }
              }

              # Add unit to title if configured
              if (style_config$add_unit_to_title) {
                if (tolower(unit_name) == "percent") {
                  plot_title <- paste0(plot_title, " (%)")
                } else {
                  plot_title <- paste0(plot_title, " (", unit_name, ")")
                }
              }

              # Create plot
              p <- .create_single_detail_plot(
                data = panel_data,
                x_axis_from = x_axis_from,
                plot_title = plot_title,
                unit = unit_name,
                panel_rows = panel_layout$rows,
                panel_cols = panel_layout$cols,
                panel_var = panel_var,
                invert_pane = invert_pane,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              # Save plot if output_dir provided
              if (!is.null(output_dir)) {
                if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
                clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
                clean_var <- gsub("[^[:alnum:]]", "_", var_name)
                clean_panel <- gsub("[^[:alnum:]]", "_", panel_val)
                clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
                top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

                filename <- file.path(output_dir, paste0(clean_sep, "_", clean_var, "_",
                                                         clean_panel, "_", clean_unit, top_n_suffix, ".png"))
                ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                                dpi = 300, bg = "white", limitsize = FALSE)
                message("Saved plot: ", filename)
              }

              plot_list[[paste(sep_value, var_name, panel_val, unit_name, sep = "_")]] <- p
            }
          } else {
            # Calculate panel layout using direct style_config values
            panel_layout <- .calculate_panel_layout(var_data, style_config$panel_rows, style_config$panel_cols, panel_var)

            if (is.null(width) || is.null(height)) {
              dims <- .calculate_plot_dimensions(var_data, panel_layout)
              width_val <- ifelse(is.null(width), dims$width, width)
              height_val <- ifelse(is.null(height), dims$height, height)
            } else {
              width_val <- width
              height_val <- height
            }

            # Format title
            plot_title <- paste0(sep_value, " - ", var_name)

            # Apply title format if specified
            if (!is.null(style_config$title_format)) {
              if (style_config$title_format$type == "prefix") {
                plot_title <- paste0(style_config$title_format$text, " ", plot_title)
              } else if (style_config$title_format$type == "suffix") {
                plot_title <- paste0(plot_title, " ", style_config$title_format$text)
              } else if (style_config$title_format$type == "full") {
                plot_title <- style_config$title_format$text
              }
            }

            # Add unit to title if configured
            if (style_config$add_unit_to_title) {
              if (tolower(unit_name) == "percent") {
                plot_title <- paste0(plot_title, " (%)")
              } else {
                plot_title <- paste0(plot_title, " (", unit_name, ")")
              }
            }

            # Create plot
            p <- .create_single_detail_plot(
              data = var_data,
              x_axis_from = x_axis_from,
              plot_title = plot_title,
              unit = unit_name,
              panel_rows = panel_layout$rows,
              panel_cols = panel_layout$cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            # Save plot if output_dir provided
            if (!is.null(output_dir)) {
              if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
              clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
              clean_var <- gsub("[^[:alnum:]]", "_", var_name)
              clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
              top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

              filename <- file.path(output_dir, paste0(clean_sep, "_", clean_var, "_",
                                                       clean_unit, top_n_suffix, ".png"))
              ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                              dpi = 300, bg = "white", limitsize = FALSE)
              message("Saved plot: ", filename)
            }

            plot_list[[paste(sep_value, var_name, unit_name, sep = "_")]] <- p
          }
        }
      }
    }
  }

  # Return single plot or list of plots based on number of plots
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}


#' @title Create Single Detail Plot (Internal)
#'
#' @description
#' Create a single detail plot from GTAP data for internal use.
#'
#' @param data A data frame containing plotting data.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Numeric. Number of rows in panel layout.
#' @param panel_cols Numeric. Number of columns in panel layout.
#' @param panel_var Character. Column for panel facets. Default is "Experiment".
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param top_impact Numeric. Number of top impacts to display. Default is NULL.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the detail plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{detail_plot}}
#'
.create_single_detail_plot <- function(data, x_axis_from, plot_title, unit,
                                       panel_rows, panel_cols,
                                       panel_var = "Experiment",
                                       invert_pane = FALSE,
                                       top_impact = NULL,
                                       plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "detail")
  }

  # SETUP COLOR PALETTE
  positive_color <- style_config$positive_color
  negative_color <- style_config$negative_color
  color_palette <- .generate_color_palette(positive_color, negative_color)

  # CATEGORIZE VALUES
  data <- dplyr::mutate(
    data,
    value_category = dplyr::case_when(
      Value > 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_positive",
      Value < 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_negative",
      Value > 0 ~ "normal_positive",
      Value < 0 ~ "normal_negative",
      TRUE ~ "neutral"
    )
  )

  # PREPARE DATA
  max_abs_value <- max(abs(data$Value))
  decimal_places <- style_config$value_label_decimal_places
  data$Label <- sprintf(paste0("%.", decimal_places, "f"), data$Value)

  n_vars <- length(unique(data[[x_axis_from]]))

  # CALCULATE Y-AXIS LIMITS
  if (all(data$Value >= 0)) {
    # All positive values
    y_limits <- c(0, max_abs_value * 1.5)
  } else if (all(data$Value <= 0)) {
    # All negative values
    y_limits <- c(-max_abs_value * 1.5, 0)
  } else {
    # Mixed values
    y_limits <- c(-max_abs_value * 1.5, max_abs_value * 1.5)
  }

  # FORMAT AXIS LABELS
  # y-axis label shows the unit
  y_axis_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  # x-axis label uses column name if no description provided
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    x_axis_from
  }

  # PREPARE DATA FOR PLOTTING
  data <- data[order(data$Value), ]
  x_factor_col <- paste0(x_axis_from, "_factor")
  data[[x_factor_col]] <- factor(data[[x_axis_from]], levels = unique(data[[x_axis_from]]))

  # CREATE BASE PLOT WITH APPROPRIATE ORDERING
  if (invert_pane) {
    # For horizontal bars (flipped coordinates)
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(
        data = data,
        mapping = ggplot2::aes_string(
          y = x_factor_col,
          x = "Value",
          fill = "value_category"
        ),
        width = style_config$bar_width
      )

    # Add value labels for horizontal bars
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes_string(
          y = x_factor_col,
          x = "Value",
          label = "Label"
        ),
        hjust = ifelse(data$Value >= 0, -0.2, 1.2),
        size = style_config$value_label_size
      )
    }
  } else {
    # For vertical bars (normal coordinates)
    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(
        data = data,
        mapping = ggplot2::aes_string(
          x = x_factor_col,
          y = "Value",
          fill = "value_category"
        ),
        width = style_config$bar_width
      )

    # Add value labels for vertical bars
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes_string(
          x = x_factor_col,
          y = "Value",
          label = "Label"
        ),
        vjust = ifelse(data$Value >= 0, -0.5, 1.5),
        size = style_config$value_label_size
      )
    }
  }

  # SETUP PLOT APPEARANCE
  p <- p +
    ggplot2::scale_fill_manual(values = color_palette, guide = "none") +
    ggplot2::theme_minimal()

  # ADD ZERO LINE - need to handle differently based on orientation
  if (style_config$show_zero_line) {
    if (invert_pane) {
      # Zero line is vertical for horizontal bars
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    } else {
      # Zero line is horizontal for vertical bars
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }
  }

  # APPLY APPROPRIATE AXIS SCALES - need to handle differently based on orientation
  if (invert_pane) {
    # For horizontal bars, we scale the x-axis (value)
    p <- p + ggplot2::scale_x_continuous(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  } else {
    # For vertical bars, we scale the y-axis (value)
    p <- p + ggplot2::scale_y_continuous(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  }

  # ADD FACETS IF NEEDED
  if (panel_rows > 1 || panel_cols > 1) {
    if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        nrow = panel_rows,
        ncol = panel_cols,
        scales = "free"
      )
    } else {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        nrow = panel_rows,
        ncol = panel_cols,
        scales = "fixed"
      )
    }
  }

  # FIRST APPLY BASE THEME STYLING
  p <- .apply_plot_style_config(p, style_config)

  # THEN SET AXIS LABELS (after theme is applied)
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  return(p)
}


#' @title Filter Top Impact Values (Internal)
#'
#' @description
#' Filter data to show only top impactful values from GTAP data for internal use.
#'
#' @param data A data frame or list to filter.
#' @param top_impact Numeric. Number of top impacts to display.
#' @param group_col Character. Column for grouping data.
#' @param panel_var Character. Column for panel facets.
#' @param x_axis_from Character. Column for x-axis categories.
#' @param variable_col Character. Column for variable identification.
#' @param unit_col Character. Column for unit information.
#'
#' @return A filtered data frame or list.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{detail_plot}}
#'
.filter_top_impact_values_detail <- function(data, top_impact, group_col, panel_var, x_axis_from, variable_col, unit_col) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .filter_top_impact_values_detail, top_impact, group_col, panel_var, x_axis_from, variable_col, unit_col))
  }

  if (!is.data.frame(data)) return(data)
  if (!("Value" %in% names(data))) return(data)
  if (is.null(top_impact) || nrow(data) <= top_impact) return(data)

  data$Value <- as.numeric(data$Value)

  # Create grouping based on variable, unit, panel and group columns
  group_cols <- c(variable_col, unit_col, panel_var, group_col)
  group_cols <- group_cols[group_cols %in% names(data)]

  # Create a group identifier
  group_id_parts <- lapply(group_cols, function(col) data[[col]])
  group_id <- do.call(paste, c(group_id_parts, sep = "_"))
  data$._group_id_ <- group_id

  # Split data by groups
  data_grouped <- split(data, data$._group_id_)

  # Filter logic
  filtered_list <- lapply(data_grouped, function(df) {
    if (nrow(df) <= top_impact) return(df)

    df_pos <- df[df$Value > 0, , drop = FALSE]
    df_neg <- df[df$Value < 0, , drop = FALSE]

    pos_count <- min(nrow(df_pos), ceiling(top_impact / 2))
    neg_count <- min(nrow(df_neg), ceiling(top_impact / 2))

    if (neg_count < ceiling(top_impact / 2)) {
      pos_count <- min(nrow(df_pos), top_impact - neg_count)
    }

    if (pos_count < ceiling(top_impact / 2)) {
      neg_count <- min(nrow(df_neg), top_impact - pos_count)
    }

    rbind(
      if (pos_count > 0) df_pos[order(-df_pos$Value), , drop = FALSE][seq_len(pos_count), , drop = FALSE] else NULL,
      if (neg_count > 0) df_neg[order(df_neg$Value), , drop = FALSE][seq_len(neg_count), , drop = FALSE] else NULL
    )
  })

  filtered_data <- do.call(rbind, filtered_list)

  # Remove the temporary group_id column
  filtered_data$._group_id_ <- NULL

  # Calculate average values for sorting the axis variable
  if (x_axis_from %in% names(filtered_data)) {
    avg_formula <- as.formula(paste("Value ~", x_axis_from))
    avg_values <- stats::aggregate(avg_formula, data = filtered_data, mean, na.rm = TRUE)
    sorted_groups <- avg_values[order(avg_values$Value), 1]
    filtered_data[[x_axis_from]] <- factor(filtered_data[[x_axis_from]], levels = sorted_groups)
  }

  # Return filtered data
  return(filtered_data)
}


# Stack Plot --------------------------------------------------------------

#' @title Create Comprehensive Stacked Bar Charts for Decomposition Analysis
#'
#' @description
#' Generates stacked bar charts to visualize the composition of values across multiple dimensions,
#' automatically handling multi-dimensional data. The function supports both stacked and unstacked
#' presentations, making it particularly useful for decomposition analysis.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Name of the column to use for x-axis categories (e.g., "REG", "Sector").
#' @param stack_value_from Character. Name of the column containing stack component categories (e.g., "COMM" for commodities).
#' @param split_by Character or vector. Column name(s) for data splitting (e.g., "COMM", "REG", "ACTS").
#' Set to NULL for no splitting, which is suitable for macro-level analysis (i.e., aggregated values without additional dimensions).
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, creates a separate figure for each panel value (default: FALSE).
#' @param unstack_plot Logical. If TRUE, creates separate bar plots for each `x_axis_from` value instead of stacked bars (default: FALSE).
#' @param output_dir Character. Directory path to save plots as `.png` files. If NULL, plots are only returned in R without saving.
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param show_total Logical. If TRUE, displays total values above stacked bars (default: TRUE).
#' @param y_limit Numeric vector. Manual limits for the y-axis. If NULL, it is calculated automatically.
#' @param width Numeric. Width of the output figure in inches. If NULL, it is calculated automatically.
#' @param height Numeric. Height of the output figure in inches. If NULL, it is calculated automatically.
#' @param top_impact Numeric or NULL. If specified, filters to show only the top N impactful values. NULL shows all values.
#' @param plot_style_config List. Custom style configuration for plots. If NULL, defaults from `get_plot_style_config("stack")` are applied.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @details
#' This function allows for extensive customization of plots through the `plot_style_config` parameter, which integrates ggplot2 configurations.
#' To view and adjust available style options, use:
#' `get_plot_style_config("stack", as_dataframe = TRUE)`.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{detail_plot}}, \code{\link{comparison_plot}}, \code{\link{get_plot_style_config}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic stacked bar chart by commodity
#' p1 <- stack_plot(
#'   data = gtap_results,
#'   x_axis_from = "REG",
#'   stack_value_from = "COMM"
#' )
#'
#' # Welfare Decomposition
#' p2 <- stack_plot(
#'   data = headerA,
#'   x_axis_from = "Region",
#'   stack_value_from = "COLUMN",
#'   split_by = FALSE,
#'   unstack_plot = TRUE,
#'   show_total = TRUE,
#'
#' )
#'
#' # Terms of trade decomposition
#' plots <- stack_plot(
#'   data = headerE1,
#'   x_axis_from = "Commodity",
#'   stack_value_from = "PRICES",
#'   split_by = "Region",
#'   top_impact = 10,
#'   invert_pane = TRUE,
#'   output_dir = "path/to/output/directory",
#'   plot_style_config = list(
#'     color_tone = "economic",
#'     show_legend = TRUE,
#'     legend_position = "bottom"
#'   )
#' )
#' }
#'
stack_plot <- function(data, filter_var = NULL,
                       x_axis_from,
                       stack_value_from,
                       split_by = NULL,
                       panel_var = "Experiment",
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       invert_pane = FALSE,
                       separate_figure = FALSE,
                       unstack_plot = FALSE,
                       output_dir = NULL,
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       show_total = TRUE,
                       y_limit = NULL,
                       width = NULL,
                       height = NULL,
                       top_impact = NULL,
                       plot_style_config = NULL) {

  # PREPARE DATA SOURCE
  if (is.list(data) && !is.data.frame(data)) {
    data_found <- FALSE
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df) &&
          x_axis_from %in% names(df) &&
          stack_value_from %in% names(df)) {
        data <- df
        data_found <- TRUE
        break
      }
    }

    if (!data_found) {
      stop(paste("No suitable dataframe found with required columns:",
                 x_axis_from, "and", stack_value_from))
    }
  }

  # CHECK FOR REQUIRED COLUMNS
  if (!(unit_col %in% names(data))) {
    stop("Missing 'Unit' column in data frame. See add_mapping_info for help.")
  }

  if (!("Value" %in% names(data))) {
    stop("Missing 'Value' column in data frame.")
  }

  # PROCESS SPLIT_BY PARAMETER
  is_macro_mode <- FALSE
  if (is.null(split_by) || (is.logical(split_by) && !split_by)) {
    is_macro_mode <- TRUE
  } else {
    if (length(split_by) > 1) {
      for (col in split_by) {
        if (!(col %in% names(data))) {
          warning(paste("Split-by column", col, "not found. Creating default column."))
          data[[col]] <- "Default"
        }
      }
    } else {
      if (!(split_by %in% names(data))) {
        warning(paste("Split-by column", split_by, "not found. Creating default column."))
        data[[split_by]] <- "Default"
      }
    }
  }

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      data <- data[data[[x_axis_from]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    if (var_name_by_description || add_var_info) {
      result <- data

      for (i in seq_len(nrow(result))) {
        var_ <- result[[variable_col]][i]
        des_ <- result[[desc_col]][i]

        if (is.na(des_) || !nzchar(des_))
          des_ <- var_

        if (var_name_by_description && add_var_info) {
          result[[variable_col]][i] <- paste0(des_, " (", var_, ")")
        } else if (var_name_by_description && !add_var_info) {
          result[[variable_col]][i] <- des_
        } else if (!var_name_by_description && add_var_info) {
          if (des_ != var_) {
            result[[variable_col]][i] <- paste0(var_, " (", des_, ")")
          }
        }
      }

      data <- result
    }
  }

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    .calculate_plot_style_config(plot_style_config, "stack")
  } else {
    .calculate_plot_style_config(NULL, "stack")
  }

  # PROCESS BY UNIT GROUPS (different units need separate plots)
  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    # DETERMINE SEPARATE VALUES
    if (is_macro_mode) {
      separate_values <- "All Data"
    } else if (length(split_by) > 1) {
      unit_data$split_display <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
      separate_values <- unique(unit_data$split_display)
      split_col <- "split_display"
    } else {
      separate_values <- unique(unit_data[[split_by]])
      split_col <- split_by
    }

    for (sep_value in separate_values) {
      # FILTER DATA FOR CURRENT SEPARATE VALUE
      if (is_macro_mode) {
        filtered_data <- unit_data
      } else if (split_col == "split_display") {
        filtered_data <- unit_data[unit_data$split_display == sep_value, ]
      } else {
        filtered_data <- unit_data[unit_data[[split_col]] == sep_value, ]
      }

      # CALCULATE PANEL LAYOUT
      panel_layout <- .calculate_panel_layout(filtered_data, style_config$panel_rows, style_config$panel_cols, panel_var)

      # CALCULATE DIMENSIONS
      if (is.null(width) || is.null(height)) {
        dims <- .calculate_plot_dimensions(filtered_data, panel_layout)
        width_val <- ifelse(is.null(width), dims$width, width)
        height_val <- ifelse(is.null(height), dims$height, height)
      } else {
        width_val <- width
        height_val <- height
      }

      # FORMAT TITLE
      plot_title <- sep_value

      # APPLY TITLE FORMAT IF SPECIFIED
      if (!is.null(style_config$title_format)) {
        if (style_config$title_format$type == "prefix") {
          plot_title <- paste0(style_config$title_format$text, " ", plot_title)
        } else if (style_config$title_format$type == "suffix") {
          plot_title <- paste0(plot_title, " ", style_config$title_format$text)
        } else if (style_config$title_format$type == "full") {
          plot_title <- style_config$title_format$text
        }
      }

      # ADD UNIT TO TITLE IF CONFIGURED
      if (style_config$add_unit_to_title) {
        if (tolower(unit_name) == "percent") {
          plot_title <- paste0(plot_title, " (%)")
        } else {
          plot_title <- paste0(plot_title, " (", unit_name, ")")
        }
      }

      # FORMAT Y-AXIS LABEL
      y_axis_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
        style_config$y_axis_description
      } else if (tolower(unit_name) == "percent") {
        "Percentage (%)"
      } else {
        unit_name
      }

      # CALCULATE TOTALS USING HELPER FUNCTION
      total_data <- .calculate_stack_totals(filtered_data, x_axis_from, panel_var)

      # APPLY TOP_IMPACT FILTER IF SPECIFIED
      if (!is.null(top_impact)) {
        if (!is_macro_mode && length(split_by) > 1) {
          filtered_data$._split_group_ <- apply(filtered_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
          top_impact_filter_col <- "._split_group_"
        } else if (!is_macro_mode) {
          top_impact_filter_col <- split_by
        } else {
          top_impact_filter_col <- x_axis_from
        }

        # USE STACK-SPECIFIC FILTER FUNCTION
        filtered_data <- .filter_top_impact_values_stack(
          data = filtered_data,
          total_data = total_data,
          top_impact = top_impact,
          group_col = top_impact_filter_col,
          panel_var = panel_var,
          x_axis_from = x_axis_from,
          variable_col = variable_col,
          unit_col = unit_col,
          stack_value_from = stack_value_from
        )

        # RE-CALCULATE TOTALS WITH FILTERED DATA
        total_data <- .calculate_stack_totals(filtered_data, x_axis_from, panel_var)

        # CLEAN UP TEMPORARY COLUMN IF CREATED
        if ("._split_group_" %in% names(filtered_data)) {
          filtered_data$._split_group_ <- NULL
        }
      }

      # CREATE APPROPRIATE PLOT TYPE
      if (unstack_plot) {
        x_axis_values <- unique(filtered_data[[x_axis_from]])

        for (x_val in x_axis_values) {
          x_data <- filtered_data[filtered_data[[x_axis_from]] == x_val, ]
          x_totals <- total_data[total_data[[x_axis_from]] == x_val, ]

          x_plot_title <- paste0(plot_title, " - ", x_val)

          p <- .create_single_unstacked_plot(
            data = x_data,
            total_data = x_totals,
            x_axis_from = x_axis_from,
            stack_value_from = stack_value_from,
            plot_title = x_plot_title,
            unit = y_axis_label,
            panel_rows = panel_layout$rows,
            panel_cols = panel_layout$cols,
            panel_var = panel_var,
            y_limit = y_limit,
            invert_pane = invert_pane,
            top_impact = top_impact,
            plot_style_config = style_config
          )

          # SAVE PLOT IF OUTPUT_DIR PROVIDED
          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
            clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
            clean_x_val <- gsub("[^[:alnum:]]", "_", as.character(x_val))
            clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
            top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

            filename <- file.path(output_dir, paste0(clean_sep, "_unstack_", clean_x_val,
                                                     "_", clean_unit, top_n_suffix, ".png"))
            ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                            dpi = 300, bg = "white", limitsize = FALSE)
            message("Saved plot: ", filename)
          }

          plot_list[[paste(sep_value, "unstack", x_val, unit_name, sep = "_")]] <- p
        }
      } else {
        # CREATE STACKED PLOT
        p <- .create_single_stacked_plot(
          data = filtered_data,
          total_data = total_data,
          x_axis_from = x_axis_from,
          stack_value_from = stack_value_from,
          plot_title = plot_title,
          unit = y_axis_label,
          panel_rows = panel_layout$rows,
          panel_cols = panel_layout$cols,
          panel_var = panel_var,
          show_total = show_total,
          y_limit = y_limit,
          invert_pane = invert_pane,
          top_impact = top_impact,
          plot_style_config = style_config
        )

        # SAVE PLOT IF OUTPUT_DIR PROVIDED
        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
          clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
          clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
          top_n_suffix <- if (!is.null(top_impact)) paste0("_top", top_impact) else ""

          filename <- file.path(output_dir, paste0(clean_sep, "_stack_",
                                                   clean_unit, top_n_suffix, ".png"))
          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white", limitsize = FALSE)
          message("Saved plot: ", filename)
        }

        plot_list[[paste(sep_value, "stack", unit_name, sep = "_")]] <- p
      }
    }
  }

  # RETURN SINGLE PLOT OR LIST OF PLOTS
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}


#' @title Calculate Stack Totals
#'
#' @description Internal function to calculate totals for stack components.
#'
#' @param data Data frame containing stack components.
#' @param x_axis_from Column for x-axis categories.
#' @param panel_var Column for panel facets.
#'
#' @return Data frame with calculated totals.
#'
#' @keywords internal
#'
.calculate_stack_totals <- function(data, x_axis_from, panel_var) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .calculate_stack_totals, x_axis_from, panel_var))
  }

  # INPUT VALIDATION
  if (!is.data.frame(data)) return(NULL)
  if (!all(c(x_axis_from, panel_var, "Value") %in% names(data))) return(NULL)

  # SETUP VARIABLES
  group_cols <- c(panel_var, x_axis_from)
  total_data <- data.frame()

  # GET UNIQUE VALUES
  unique_x_values <- unique(data[[x_axis_from]])
  unique_panel_values <- unique(data[[panel_var]])

  # CALCULATE TOTALS FOR EACH COMBINATION
  for (x_val in unique_x_values) {
    for (panel_val in unique_panel_values) {
      subset_data <- data[data[[x_axis_from]] == x_val & data[[panel_var]] == panel_val, ]

      if (nrow(subset_data) > 0) {
        positive_total <- sum(pmax(subset_data$Value, 0), na.rm = TRUE)
        negative_total <- sum(pmin(subset_data$Value, 0), na.rm = TRUE)
        total_value <- sum(subset_data$Value, na.rm = TRUE)

        # CREATE ROW WITH TOTALS
        row <- data.frame(
          panel_val = panel_val,
          x_val = x_val,
          Total = total_value,
          PositiveTotal = positive_total,
          NegativeTotal = negative_total,
          stringsAsFactors = FALSE
        )
        names(row)[1] <- panel_var
        names(row)[2] <- x_axis_from

        total_data <- rbind(total_data, row)
      }
    }
  }

  # FORMAT TOTAL LABELS
  total_data$TotalLabel <- sprintf("Total\n%.2f", total_data$Total)

  return(total_data)
}


#' @title Filter Top Impact Values for Stack Plots (Internal)
#'
#' @description
#' Filter data for stack plots to show only top impactful values from GTAP data for internal use.
#'
#' @param data A data frame to filter.
#' @param total_data A data frame with total values.
#' @param top_impact Numeric. Number of top impacts to display.
#' @param group_col Character. Column for grouping data.
#' @param panel_var Character. Column for panel facets.
#' @param x_axis_from Character. Column for x-axis categories.
#' @param variable_col Character. Column for variable identification.
#' @param unit_col Character. Column for unit information.
#' @param stack_value_from Character. Column for stack components.
#'
#' @return A filtered data frame.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{stack_plot}}
#'
.filter_top_impact_values_stack <- function(data, total_data, top_impact, group_col, panel_var, x_axis_from, variable_col, unit_col, stack_value_from) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .filter_top_impact_values_stack, total_data, top_impact, group_col, panel_var, x_axis_from, variable_col, unit_col, stack_value_from))
  }

  # INPUT VALIDATION
  if (!is.data.frame(data)) return(data)
  if (!("Value" %in% names(data))) return(data)
  if (is.null(top_impact) || nrow(total_data) <= top_impact) return(data)

  # GET GROUP COLUMNS
  group_cols <- c(variable_col, unit_col, panel_var, group_col)
  group_cols <- group_cols[group_cols %in% names(total_data)]

  # CREATE GROUP IDENTIFIER
  group_id_parts <- lapply(group_cols, function(col) total_data[[col]])
  group_id <- do.call(paste, c(group_id_parts, sep = "_"))
  total_data$._group_id_ <- group_id

  # FILTER TOTALS BY IMPACT
  filtered_total_list <- list()
  for (group in unique(group_id)) {
    group_data <- total_data[total_data$._group_id_ == group, ]

    # SEPARATE POSITIVE AND NEGATIVE IMPACTS
    group_pos <- group_data[group_data$Total > 0, , drop = FALSE]
    group_neg <- group_data[group_data$Total < 0, , drop = FALSE]

    # CALCULATE NUMBER OF EACH TO INCLUDE
    pos_count <- min(nrow(group_pos), ceiling(top_impact / 2))
    neg_count <- min(nrow(group_neg), ceiling(top_impact / 2))

    # ADJUST COUNTS IF NEEDED
    if (neg_count < ceiling(top_impact / 2)) {
      pos_count <- min(nrow(group_pos), top_impact - neg_count)
    }
    if (pos_count < ceiling(top_impact / 2)) {
      neg_count <- min(nrow(group_neg), top_impact - pos_count)
    }

    # COMBINE TOP POSITIVE AND NEGATIVE IMPACTS
    filtered_group <- rbind(
      if (pos_count > 0) group_pos[order(-group_pos$Total), , drop = FALSE][seq_len(pos_count), , drop = FALSE] else NULL,
      if (neg_count > 0) group_neg[order(group_neg$Total), , drop = FALSE][seq_len(neg_count), , drop = FALSE] else NULL
    )

    filtered_total_list[[group]] <- filtered_group
  }

  # COMBINE FILTERED GROUPS
  filtered_total <- do.call(rbind, filtered_total_list)

  # GET X-AXIS VALUES TO KEEP
  keep_x_axis <- filtered_total[[x_axis_from]]

  # FILTER ORIGINAL DATA
  filtered_data <- data[data[[x_axis_from]] %in% keep_x_axis, ]

  # CLEAN UP
  filtered_total$._group_id_ <- NULL

  # SORT DATA IF POSSIBLE
  if (x_axis_from %in% names(filtered_data)) {
    avg_formula <- as.formula(paste("Value ~", x_axis_from))
    avg_values <- stats::aggregate(avg_formula, data = filtered_data, mean, na.rm = TRUE)
    sorted_groups <- avg_values[order(avg_values$Value), 1]
    filtered_data[[x_axis_from]] <- factor(filtered_data[[x_axis_from]], levels = sorted_groups)
  }

  return(filtered_data)
}


#' @title Create Single Stacked Plot (Internal)
#'
#' @description
#' Create a single stacked plot from GTAP data for internal use.
#'
#' @param data A data frame containing plotting data.
#' @param total_data A data frame with total values.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param stack_value_from Character. Column for stack components.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Numeric. Number of rows in panel layout.
#' @param panel_cols Numeric. Number of columns in panel layout.
#' @param panel_var Character. Column for panel facets. Default is "Experiment".
#' @param show_total Logical. Whether to display total values. Default is TRUE.
#' @param y_limit Numeric vector. Manual y-axis limits. Default is NULL.
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param top_impact Numeric. Number of top impacts to display. Default is NULL.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the stacked plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{stack_plot}}
#'
.create_single_stacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                        plot_title, unit,
                                        panel_rows, panel_cols,
                                        panel_var = "Experiment",
                                        show_total = TRUE,
                                        y_limit = NULL,
                                        invert_pane = FALSE,
                                        top_impact = NULL,
                                        plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "stack")
  }

  # SETUP VARIABLES
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # GENERATE COLOR PALETTE
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone)

  # CALCULATE Y LIMITS
  if (is.null(y_limit)) {
    # Calculate more generous limits for stacked plot
    max_total <- max(abs(total_data$Total), na.rm = TRUE)

    # Determine if we have all positive, all negative, or mixed values
    if (all(total_data$Total >= 0, na.rm = TRUE)) {
      # All positive
      y_limit <- c(0, max_total * 1.4)
    } else if (all(total_data$Total <= 0, na.rm = TRUE)) {
      # All negative
      y_limit <- c(-max_total * 1.4, 0)
    } else {
      # Mixed values - symmetrical with extra padding
      y_limit <- c(-max_total * 1.4, max_total * 1.4)
    }
  }

  # FORMAT AXIS LABELS
  # y-axis label shows the unit
  y_axis_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  # x-axis label uses column name if no description provided
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    x_axis_from
  }

  # CREATE BASE PLOT BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars (categories on y-axis, values on x-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes_string(
          y = x_axis_from,
          x = "Value",
          fill = stack_value_from
        ),
        position = "stack",
        width = style_config$bar_width
      )

    # ADD TOTAL LABELS IF CONFIGURED
    if (show_total) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        data = total_data,
        ggplot2::aes(
          y = !!rlang::sym(x_axis_from),
          x = ifelse(Total >= 0,
                     PositiveTotal + abs(Total) * 0.15,
                     NegativeTotal - abs(Total) * 0.15),
          label = sprintf(paste0("Total\n%.", decimal_places, "f"), Total)
        ),
        hjust = ifelse(total_data$Total >= 0, 0, 1),
        vjust = 0.5,
        size = value_size,
        fontface = "bold"
      )
    }

    # ADD ZERO LINE IF CONFIGURED (VERTICAL FOR HORIZONTAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    p <- p + ggplot2::scale_x_continuous(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  } else {
    # For vertical bars (categories on x-axis, values on y-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes_string(
          x = x_axis_from,
          y = "Value",
          fill = stack_value_from
        ),
        position = "stack",
        width = style_config$bar_width
      )

    # ADD TOTAL LABELS IF CONFIGURED
    if (show_total) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        data = total_data,
        ggplot2::aes(
          x = !!rlang::sym(x_axis_from),
          y = ifelse(Total >= 0,
                     PositiveTotal + abs(Total) * 0.05,
                     NegativeTotal - abs(Total) * 0.05),
          label = sprintf(paste0("Total\n%.", decimal_places, "f"), Total)
        ),
        vjust = ifelse(total_data$Total >= 0, 0, 1.5),
        size = value_size,
        fontface = "bold"
      )
    }

    # ADD ZERO LINE IF CONFIGURED (HORIZONTAL FOR VERTICAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    p <- p + ggplot2::scale_y_continuous(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  }

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        scales = "free",
        nrow = panel_rows,
        ncol = panel_cols
      )
    } else {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        scales = "fixed",
        nrow = panel_rows,
        ncol = panel_cols
      )
    }
  }

  # SETUP APPEARANCE
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

  # APPLY THEME STYLING
  p <- .apply_plot_style_config(p, style_config)

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  return(p)
}


#' @title Create Single Unstacked Plot (Internal)
#'
#' @description
#' Create a single unstacked plot from GTAP data for internal use.
#'
#' @param data A data frame containing plotting data.
#' @param total_data A data frame with total values.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param stack_value_from Character. Column for stack components.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Numeric. Number of rows in panel layout.
#' @param panel_cols Numeric. Number of columns in panel layout.
#' @param panel_var Character. Column for panel facets. Default is "Experiment".
#' @param y_limit Numeric vector. Manual y-axis limits. Default is NULL.
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param top_impact Numeric. Number of top impacts to display. Default is NULL.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the unstacked plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @seealso \code{\link{stack_plot}}
#'
.create_single_unstacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                          plot_title, unit,
                                          panel_rows, panel_cols,
                                          panel_var = "Experiment",
                                          y_limit = NULL,
                                          invert_pane = FALSE,
                                          top_impact = NULL,
                                          plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "stack")
  }

  # SETUP VARIABLES
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # GENERATE COLOR PALETTE
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone)

  # CALCULATE Y LIMITS
  if (is.null(y_limit)) {
    # Calculate more generous limits for unstacked plot
    value_range <- range(data$Value, na.rm = TRUE)
    max_abs <- max(abs(value_range), na.rm = TRUE)

    # Determine if we have all positive, all negative, or mixed values
    if (all(data$Value >= 0, na.rm = TRUE)) {
      # All positive values
      y_limit <- c(0, max_abs * 1.5)
    } else if (all(data$Value <= 0, na.rm = TRUE)) {
      # All negative values
      y_limit <- c(-max_abs * 1.5, 0)
    } else {
      # Mixed values - symmetrical with extra padding
      y_limit <- c(-max_abs * 1.5, max_abs * 1.5)
    }
  }

  # FORMAT AXIS LABELS
  # y-axis label shows the unit
  y_axis_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  # x-axis label uses column name if no description provided
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    stack_value_from  # For unstacked plot, stack_value_from is the category column
  }

  # FORMAT VALUE LABELS
  data$Label <- sprintf("%.2f", data$Value)

  # CREATE BASE PLOT BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars (categories on y-axis, values on x-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes_string(
          y = stack_value_from,
          x = "Value",
          fill = stack_value_from
        ),
        width = style_config$bar_width
      ) +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes_string(
          y = stack_value_from,
          x = "Value",
          label = "Label"
        ),
        hjust = ifelse(data$Value >= 0, -0.2, 1.2),
        size = style_config$value_label_size
      )

    # ADD ZERO LINE IF CONFIGURED (VERTICAL FOR HORIZONTAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    p <- p + ggplot2::scale_x_continuous(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  } else {
    # For vertical bars (categories on x-axis, values on y-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes_string(
          x = stack_value_from,
          y = "Value",
          fill = stack_value_from
        ),
        width = style_config$bar_width
      ) +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes_string(
          x = stack_value_from,
          y = "Value",
          label = "Label"
        ),
        vjust = ifelse(data$Value >= 0, -0.5, 1.5),
        size = style_config$value_label_size
      )

    # ADD ZERO LINE IF CONFIGURED (HORIZONTAL FOR VERTICAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        size = style_config$zero_line_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    p <- p + ggplot2::scale_y_continuous(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  }

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        scales = "free",
        nrow = panel_rows,
        ncol = panel_cols
      )
    } else {
      p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", panel_var)),
        scales = "fixed",
        nrow = panel_rows,
        ncol = panel_cols
      )
    }
  }

  # SETUP APPEARANCE
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

  # APPLY THEME STYLING
  p <- .apply_plot_style_config(p, style_config)

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)

      # Force both axis titles to be visible
      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        ),
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)

      p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = style_config$x_axis_title_margin
        )
      )
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")

      p <- p + ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = style_config$y_axis_title_margin
        )
      )
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  return(p)
}
