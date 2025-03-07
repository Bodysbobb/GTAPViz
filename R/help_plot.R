# Get Plot Style Table of Contents -----------------------------------------------------
#' @title Get Plot and Export Configurations
#' @description
#' Returns a list containing plot style configuration and export configuration.
#' The function supports retrieving a single plot style or all available styles.
#'
#' @param plot_style Character. The plot style to retrieve: "comparison", "detail", "stack", or "all".
#'        Default is "all", which returns configurations for all styles.
#' @param config List. Optional custom style configuration to override defaults.
#' @param export_config List. Optional custom export configuration to override defaults.
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{plot_style_config}: Plot style configuration(s). If \code{plot_style="all"}, contains
#'         a nested list with configurations for all styles.
#'   \item \code{export_config}: Export configuration as a data frame.
#' }
#'
#' @examples
#' # Get all plot configurations with default settings
#' all_configs <- get_plot_config()
#'
#' # Get only comparison plot configuration
#' comp_config <- get_plot_config("comparison")
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
get_plot_config <- function(plot_style = "all", config = NULL, export_config = NULL) {
  valid_styles <- c("comparison", "detail", "stack", "all")
  if (!plot_style %in% valid_styles) {
    stop("Plot style must be one of: 'comparison', 'detail', 'stack', or 'all'")
  }

  result <- list()

  if (plot_style == "all") {
    result$plot_style_config <- list(
      comparison = .calculate_plot_style_config(config, "comparison"),
      detail = .calculate_plot_style_config(config, "detail"),
      stack = .calculate_plot_style_config(config, "stack")
    )
  } else {
    result$plot_style_config <- .calculate_plot_style_config(config, plot_style)
  }

  result$export_config <- get_export_config(as_dataframe = TRUE)

  return(result)
}

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
#' ## **Scale Settings**
#' - `scale_limit`: Numeric vector of length 2. Manual limits for value axis. Example: `c(-10, 10)`
#' - `scale_increment`: Numeric. Step size for axis tick marks. Example: `2`
#'
#' ## **Scale Expansion Settings**
#' - `expansion_y_mult`: Numeric vector. Y-axis expansion. Default: `c(0.05, 0.1)`
#' - `expansion_x_mult`: Numeric vector. X-axis expansion. Default: `c(0.05, 0.05)`
#'
#' ## **All Font Adjustment**
#' - `all_font_size`: Numeric. Master control for all font sizes. Default: `1`
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
    scale_limit = "Numeric vector of length 2. Manual limits for value axis (min, max).",
    scale_increment = "Numeric. Step size for axis tick marks.",
    expansion_y_mult = "Numeric vector of length 2. Expansion multiplier for y-axis.",
    expansion_x_mult = "Numeric vector of length 2. Expansion multiplier for x-axis.",
    all_font_size = "Numeric. Master control for all font sizes. Values > 1 increase all fonts, values < 1 decrease all fonts."
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

      "Scale Settings" = c("scale_limit", "scale_increment"),

      "Scale Expansion" = c("expansion_y_mult", "expansion_x_mult"),

      "Font Size Control" = c("all_font_size")
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

    # Add this check to avoid the error when the result is empty
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

#' Get Export Configuration Options
#'
#' @description
#' Returns documentation and default values for export configuration options used in plotting functions.
#'
#' @param as_dataframe Logical. Whether to return settings as a dataframe. Default is FALSE.
#'
#' @return
#' If \code{as_dataframe} is TRUE, returns a dataframe with export configuration settings.
#' Otherwise, returns a list with configuration settings.
#'
#' @details
#' ## **Export Configuration Parameters**
#' - `file_name`: Character. Base name for exported files. Default: `"gtap_plots"`
#' - `width`: Numeric or `NULL`. Plot width in inches. Default: `NULL` (automatically calculated)
#' - `height`: Numeric or `NULL`. Plot height in inches. Default: `NULL` (automatically calculated)
#' - `dpi`: Numeric. Resolution for PNG export. Default: `300`
#' - `bg`: Character. Background color. Default: `"white"`
#' - `limitsize`: Logical. Whether to limit size. Default: `FALSE`
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#' # Get export configuration as list
#' export_info <- get_export_config()
#'
#' # Get as a formatted dataframe
#' export_df <- get_export_config(as_dataframe = TRUE)
#'
get_export_config <- function(as_dataframe = FALSE) {
  # Export config parameters
  export_config_params <- list(
    file_name = "gtap_plots",
    width = NULL,
    height = NULL,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )

  # Documentation for export_config parameters
  export_config_docs <- list(
    file_name = "Character. Base name for exported files. Default is 'gtap_plots'.",
    width = "Numeric. Plot width in inches. Default is automatically calculated.",
    height = "Numeric. Plot height in inches. Default is automatically calculated.",
    dpi = "Numeric. Resolution for PNG export. Default is 300.",
    bg = "Character. Background color. Default is 'white'.",
    limitsize = "Logical. Whether to limit size. Default is FALSE."
  )

  if (as_dataframe) {
    # Create the dataframe
    result <- data.frame(
      Topic = character(),
      Arguments = character(),
      `Default Value` = character(),
      `Input Format` = character(),
      Description = character(),
      Example = character(),
      stringsAsFactors = FALSE
    )

    # Add export_config params
    current_topic <- "Export Config"
    for (param in names(export_config_params)) {
      param_value <- export_config_params[[param]]
      param_type <- if (is.null(param_value)) "numeric" else class(param_value)[1]

      example <- if (param == "file_name") {
        'export_config = list(file_name = "regional_impacts")'
      } else if (param == "width") {
        "export_config = list(width = 12)"
      } else if (param == "height") {
        "export_config = list(height = 8)"
      } else if (param == "dpi") {
        "export_config = list(dpi = 600)"
      } else if (param == "bg") {
        'export_config = list(bg = "white")'
      } else if (param == "limitsize") {
        "export_config = list(limitsize = FALSE)"
      }

      val_text <- if (is.null(param_value)) "NULL" else
        if (is.logical(param_value)) ifelse(param_value, "TRUE", "FALSE") else
          param_value

      # Only show "Export Config" in the first row
      display_topic <- if (param == names(export_config_params)[1]) current_topic else ""

      result <- rbind(result, data.frame(
        Topic = display_topic,
        Arguments = param,
        `Default Value` = val_text,
        `Input Format` = param_type,
        Description = export_config_docs[[param]],
        Example = example,
        stringsAsFactors = FALSE
      ))
    }

    return(result)
  }

  # Return as list
  full_config <- list(
    export_config = export_config_params,
    export_config_docs = export_config_docs
  )

  return(full_config)
}

# PLOT STYLE CONFIG HELPERS -----------------------------------------

#' @title Calculate Plot Style Configuration
#'
#' @description Merges user-defined plot style configurations with defaults for different plot types.
#' This function is internal and used by plotting functions to define visual styling.
#'
#' @param config Optional list with custom style configuration parameters.
#' @param plot_type Type of plot: `"comparison"`, `"detail"`, or `"stack"`.
#'
#' @return A list with complete style configuration for the specified plot type.
#' @importFrom utils modifyList
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_style_config <- function(config = NULL, plot_type = "comparison") {
  # Default configurations specific to each plot type
  comparison_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 20,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(10, 0, 10, 0),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 16,
    x_axis_title_margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "bold",
    x_axis_text_size = 14,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 16,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 14,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 0,
    y_axis_description = "",

    # Axis Label across panel
    show_axis_titles_on_all_facets = TRUE,

    # Value label settings
    show_value_labels = TRUE,
    value_label_face = "plain",
    value_label_size = 5,
    value_label_position = "above",
    value_label_decimal_places = 2,

    # Legend settings
    show_legend = FALSE,
    show_legend_title = FALSE,
    legend_position = "bottom",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 14,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 16,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 2,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = FALSE,
    show_grid_major_y = TRUE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,

    # Zero line settings
    show_zero_line = TRUE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,

    # Bar chart settings
    bar_width = 0.9,
    bar_spacing = 0.9,

    # Scale settings
    scale_limit = NULL,
    scale_increment = NULL,

    # Scale expansion settings
    expansion_y_mult = c(0.05, 0.1),
    expansion_x_mult = c(0.05, 0.05),

    # Font size settings
    all_font_size = 1
  )

  detail_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 48,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(b = 30),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 32,
    x_axis_title_margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "plain",
    x_axis_text_size = 32,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 32,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 50, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 32,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 1,
    y_axis_description = "",

    # Axis Label across panel
    show_axis_titles_on_all_facets = TRUE,

    # Value label settings
    show_value_labels = TRUE,
    value_label_face = "plain",
    value_label_size = 7,
    value_label_position = "outside",
    value_label_decimal_places = 2,

    # Legend settings
    show_legend = FALSE,
    show_legend_title = FALSE,
    legend_position = "bottom",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 14,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 25,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 1,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = FALSE,
    show_grid_major_y = FALSE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,

    # Zero line settings
    show_zero_line = TRUE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,

    # Bar chart settings
    bar_width = 0.4,
    bar_spacing = 0.5,

    # Scale settings
    scale_limit = NULL,
    scale_increment = NULL,

    # Scale expansion settings
    expansion_y_mult = c(0.2, 0.2),
    expansion_x_mult = c(0.05, 0.05),

    # Font size settings
    all_font_size = 1
  )

  stack_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 32,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(b = 15),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 20,
    x_axis_title_margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "bold",
    x_axis_text_size = 18,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 20,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 18,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 0,
    y_axis_description = "",

    # Axis Label across panel
    show_axis_titles_on_all_facets = TRUE,

    # Value label settings
    show_value_labels = TRUE,
    value_label_face = "plain",
    value_label_size = 5,
    value_label_position = "top",
    value_label_decimal_places = 2,

    # Legend settings
    show_legend = TRUE,
    show_legend_title = FALSE,
    legend_position = "bottom",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 18,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 18,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 1,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = FALSE,
    show_grid_major_y = TRUE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,

    # Zero line settings
    show_zero_line = TRUE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,

    # Bar chart settings
    bar_width = 0.7,
    bar_spacing = 0,

    # Scale settings
    scale_limit = NULL,
    scale_increment = NULL,

    # Scale expansion settings
    expansion_y_mult = c(0.1, 0.1),
    expansion_x_mult = c(0.05, 0.05),

    # Font size settings
    all_font_size = 1
  )

  # Select the appropriate default based on plot type
  default_config <- switch(plot_type,
                           "comparison" = comparison_defaults,
                           "detail" = detail_defaults,
                           "stack" = stack_defaults,
                           comparison_defaults)

  # If no config is provided, return the default
  if (is.null(config)) {
    return(default_config)
  }

  # Merge user config with defaults (user settings take precedence)
  final_config <- modifyList(default_config, config)

  # Handle dynamic title format
  if (!is.null(final_config$title_format) &&
      final_config$title_format$type == "dynamic" &&
      !is.null(data)) {

    # Get the columns specified for dynamic title
    cols_to_use <- final_config$title_format$text

    # Ensure the columns exist in the data
    valid_cols <- cols_to_use[cols_to_use %in% names(data)]

    if (length(valid_cols) > 0) {
      # Create title from unique values of specified columns
      dynamic_title <- paste(
        unique(do.call(paste, data[, valid_cols, drop = FALSE])),
        collapse = " - "
      )

      # Override the text in title_format
      final_config$title_format$text <- dynamic_title
    }
  }

  # Override font sizes with all_font_size if provided
  if (!is.null(final_config$all_font_size)) {
    font_sizes <- .calculate_font_sizes(NULL, NULL, final_config$all_font_size)

    # Only override font sizes if not explicitly set in user config
    font_size_fields <- c(
      "title_size", "x_axis_title_size", "y_axis_title_size",
      "strip_text_size", "x_axis_text_size", "y_axis_text_size",
      "legend_title_size", "legend_text_size", "value_label_size"
    )

    for (field in font_size_fields) {
      if (is.null(config) || is.null(config[[field]])) {
        final_config[[field]] <- font_sizes[[field]]
      }
    }
  }

  return(final_config)
}


#' @title Apply Plot Style Configuration to a ggplot Object
#'
#' @description Applies a style configuration to a ggplot object. This is an internal function
#' used by the plotting functions.
#'
#' @param p A ggplot object to modify.
#' @param config A list containing style configuration parameters.
#'
#' @return A ggplot object with applied styling.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.apply_plot_style_config <- function(p, config) {

  # Apply theme modifications
  p <- p + ggplot2::theme(
    # Title settings
    plot.title = if (config$show_title) {
      ggplot2::element_text(
        hjust = config$title_hjust,
        size = config$title_size,
        face = config$title_face,
        margin = config$title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # X axis title settings
    axis.title.x = if (config$show_x_axis_title) {
      ggplot2::element_text(
        size = config$x_axis_title_size,
        face = config$x_axis_title_face,
        margin = config$x_axis_title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # Y axis title settings
    axis.title.y = if (config$show_y_axis_title) {
      ggplot2::element_text(
        size = config$y_axis_title_size,
        face = config$y_axis_title_face,
        margin = config$y_axis_title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # X axis text settings
    axis.text.x = if (config$show_x_axis_labels) {
      ggplot2::element_text(
        size = config$x_axis_text_size,
        face = config$x_axis_text_face,
        angle = config$x_axis_text_angle,
        hjust = config$x_axis_text_hjust
      )
    } else {
      ggplot2::element_blank()
    },

    # Y axis text settings
    axis.text.y = if (config$show_y_axis_labels) {
      ggplot2::element_text(
        size = config$y_axis_text_size,
        face = config$y_axis_text_face,
        angle = config$y_axis_text_angle,
        hjust = config$y_axis_text_hjust
      )
    } else {
      ggplot2::element_blank()
    },

    # Legend settings
    legend.position = if (config$show_legend) config$legend_position else "none",
    legend.title = if (config$show_legend_title) {
      ggplot2::element_text(face = config$legend_title_face)
    } else {
      ggplot2::element_blank()
    },
    legend.text = ggplot2::element_text(
      face = config$legend_text_face,
      size = config$legend_text_size
    ),

    # Panel strip settings
    strip.text = ggplot2::element_text(
      face = config$strip_face,
      size = config$strip_text_size,
      margin = config$strip_text_margin
    ),
    strip.background = ggplot2::element_rect(fill = config$strip_background),

    # Panel spacing
    panel.spacing.x = ggplot2::unit(config$panel_spacing, "cm"),

    # Background and grid settings
    plot.background = ggplot2::element_rect(fill = config$background_color, color = NA),
    panel.background = ggplot2::element_rect(fill = config$background_color, color = NA),
    panel.grid.major.x = if (config$show_grid_major_x) {
      ggplot2::element_line(color = config$grid_color)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.major.y = if (config$show_grid_major_y) {
      ggplot2::element_line(color = config$grid_color)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor.x = if (config$show_grid_minor_x) {
      ggplot2::element_line(color = config$grid_color)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor.y = if (config$show_grid_minor_y) {
      ggplot2::element_line(color = config$grid_color)
    } else {
      ggplot2::element_blank()
    }
  )

  # Apply zero line if configured
  if (config$show_zero_line) {
    # Remove any existing zero line (geom_hline with yintercept=0)
    p$layers <- p$layers[!sapply(p$layers, function(l) {
      if ("geom_hline" %in% class(l$geom) && !is.null(l$data)) {
        if (!is.null(l$data$yintercept) && any(l$data$yintercept == 0)) {
          return(TRUE)
        }
      }
      return(FALSE)
    })]

    # Add the new zero line with configured properties
    p <- p + ggplot2::geom_hline(
      yintercept = config$zero_line_position,
      linetype = config$zero_line_type,
      color = config$zero_line_color,
      size = config$zero_line_size
    )
  }

  # Apply custom theme if provided
  if (!is.null(config$theme)) {
    p <- p + config$theme
  }

  return(p)
}


#' @title Calculate Font Sizes Based on All Font Size Parameter
#'
#' @description
#' Calculates appropriate font sizes for different plot elements based on a single all_font_size parameter.
#' All font sizes are scaled proportionally to maintain consistent relative sizes.
#'
#' @param width Numeric. Width of the plot in inches (maintained for compatibility).
#' @param height Numeric. Height of the plot in inches (maintained for compatibility).
#' @param all_font_size Numeric. Base value to adjust all font sizes proportionally. Default is 100.
#'
#' @return A list containing font sizes for various plot elements (title, axis labels, etc.).
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_font_sizes <- function(width, height, all_font_size = 1) {
  # Calculate scaling factor based on all_font_size
  factor <- all_font_size

  # Define proportional font sizes at reference level of all_font_size
  font_sizes <- list(
    title_size = round(20 * factor),
    x_axis_title_size = round(14 * factor),
    y_axis_title_size = round(14 * factor),
    strip_text_size = round(16 * factor),
    x_axis_text_size = round(12 * factor),
    y_axis_text_size = round(12 * factor),
    legend_title_size = round(14 * factor),
    legend_text_size = round(10 * factor),
    value_label_size = round(5 * factor)
  )

  return(font_sizes)
}


#' @title Process Plot Title
#'
#' @description
#' Processes the plot title based on formatting options and data content.
#'
#' @param default_title Character. The default title to use.
#' @param title_format List or NULL. Formatting options for the title with elements:
#'   \itemize{
#'     \item \code{type}: "prefix", "suffix", "full", or "dynamic"
#'     \item \code{text}: Text to add or columns to use (for dynamic type)
#'     \item \code{sep}: Separator to use for dynamic titles (default: " - ")
#'   }
#' @param add_unit_to_title Logical. Whether to add unit information to the title.
#' @param unit_name Character or NULL. Unit name to add to the title if add_unit_to_title is TRUE.
#' @param data Data frame or NULL. Data to extract values from for dynamic titles.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{title}: The formatted plot title
#'     \item \code{export_name}: Export-friendly version of the title for filenames
#'   }
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.process_plot_title <- function(
    default_title,
    title_format = NULL,
    add_unit_to_title = FALSE,
    unit_name = NULL,
    data = NULL) {
  plot_title <- default_title

  if (!is.null(title_format)) {
    switch(title_format$type,
           "prefix" = {
             plot_title <- paste0(title_format$text, " ", plot_title)
           },
           "suffix" = {
             plot_title <- paste0(plot_title, " ", title_format$text)
           },
           "full" = {
             plot_title <- title_format$text
           },
           "dynamic" = {
             if (!is.null(data) && !is.null(title_format$text)) {
               separator <- if (!is.null(title_format$sep)) title_format$sep else " - "

               cols_to_use <- title_format$text
               valid_cols <- cols_to_use[cols_to_use %in% names(data)]

               if (length(valid_cols) > 0) {
                 unique_values <- list()
                 for (col in valid_cols) {
                   vals <- unique(as.character(data[[col]]))
                   unique_values[[col]] <- vals
                 }

                 all_values <- unlist(unique_values)
                 deduped_values <- unique(all_values)

                 plot_title <- paste(deduped_values, collapse = separator)
               }
             }
           }
    )
  }

  if (add_unit_to_title && !is.null(unit_name)) {
    if (tolower(unit_name) == "percent") {
      plot_title <- paste0(plot_title, " (%)")
    } else {
      plot_title <- paste0(plot_title, " (", unit_name, ")")
    }
  }

  title_parts <- unlist(strsplit(plot_title, " - |\\||,|;|\\s+"))
  title_parts <- trimws(title_parts)
  title_parts <- title_parts[title_parts != ""]
  unique_parts <- unique(title_parts)
  clean_title <- paste(unique_parts, collapse = "_")

  export_name <- gsub("[\\/:*?\"<>|()%]", "_", clean_title)
  export_name <- gsub("__+", "_", export_name)
  export_name <- gsub("^_|_$", "", export_name)

  return(list(
    title = plot_title,
    export_name = export_name
  ))
}


# COLUMN HANDLING HELPERS -----------------------------------------
#' @title Validate Column Parameters in Data Frame
#'
#' @description
#' Checks whether specified parameter values exist as column names in the given data frame.
#' If a parameter refers to a non-existent column, a warning is issued.
#'
#' @param data A data frame or a named list of data frames to validate.
#' @param params A named list where names are parameter names and values are expected column names (single or multiple).
#'
#' @return This function does not return a value but issues warnings for unmatched columns.
#'
#' @details
#' This function ensures that user-specified parameter values correspond to existing columns in the given data structure.
#' It supports both individual data frames and lists of data frames, recursively checking each data frame in the list.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.validate_column_params <- function(data, params) {
  if (is.list(data) && !is.data.frame(data)) {
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df)) {
        .validate_column_params(df, params)
        return(invisible())
      }
    }
    return(invisible())
  }

  if (!is.data.frame(data)) {
    return(invisible())
  }

  for (param_name in names(params)) {
    param_value <- params[[param_name]]

    # Skip if NULL or logical
    if (is.null(param_value) || is.logical(param_value)) {
      next
    }

    # For list of values (like in split_by with multiple columns)
    if (is.character(param_value) && length(param_value) > 1) {
      for (single_value in param_value) {
        if (!single_value %in% names(data)) {
          warning(sprintf("Parameter '%s' value '%s' does not match any column in the data.",
                          param_name, single_value))
        }
      }
    } else {
      # For single values
      if (!param_value %in% names(data)) {
        warning(sprintf("Parameter '%s' value '%s' does not match any column in the data.",
                        param_name, param_value))
      }
    }
  }

  return(invisible())
}


#' @title Find Column in Data Frame
#'
#' @description Finds a column in a data frame using case-insensitive matching.
#'
#' @param data A data frame to search for the column.
#' @param col_name Name of the column to find.
#' @param is_required Logical. If TRUE, stops with error if column not found.
#' @param default_name Optional default name to return if column not found.
#'
#' @return The actual column name if found, default_name if provided, or NULL.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.find_column <- function(data, col_name, is_required = FALSE, default_name = NULL) {
  # Find a column case-insensitively
  if (col_name %in% names(data)) {
    return(col_name)  # Exact match
  }

  idx <- which(tolower(names(data)) == tolower(col_name))
  if (length(idx) > 0) {
    return(names(data)[idx[1]])  # Case-insensitive match
  }

  if (is_required) {
    stop(paste("Required column not found:", col_name))
  }

  return(default_name)  # Return default or NULL
}


#' @title Prepare Data Source from List or Data Frame
#'
#' @description Finds a suitable data frame from a list or returns the input data frame.
#'
#' @param data A data frame or list of data frames.
#' @param required_columns Optional vector of column names that must be present.
#'
#' @return A data frame that contains the required columns.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.prepare_data_source <- function(data, required_columns = NULL) {
  # Handle the case where data is a list of data frames
  if (is.list(data) && !is.data.frame(data)) {
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (!is.data.frame(df)) next

      # If no required columns, return the first dataframe
      if (is.null(required_columns)) return(df)

      # Check if df has the required columns (case-insensitive)
      has_required <- sapply(required_columns, function(col) {
        any(tolower(names(df)) == tolower(col))
      })

      if (all(has_required)) {
        return(df)
      }
    }

    if (!is.null(required_columns)) {
      stop(paste("No suitable dataframe found with required columns:",
                 paste(required_columns, collapse=", ")))
    } else {
      stop("No suitable dataframe found")
    }
  }

  # If data is already a data frame, return it directly
  if (is.data.frame(data)) {
    return(data)
  }

  stop("Input must be a data frame or a list of data frames")
}


#' @title Check and Standardize Unit Column
#'
#' @description Checks for the presence of a unit column and adds a default if missing.
#'
#' @param data A data frame to check.
#' @param unit_col Name of the unit column to check for (default: "Unit").
#'
#' @return A list with two elements: data (the possibly modified data frame) and unit_col (the actual unit column name).
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.check_unit_column <- function(data, unit_col = "Unit") {
  # Check if unit column exists
  actual_unit_col <- .find_column(data, unit_col)

  if (is.null(actual_unit_col)) {
    warning(paste("Unit column", unit_col, "not found. Using default 'Unit'"))
    data$Unit <- "data"
    return(list(data = data, unit_col = "Unit"))
  }

  return(list(data = data, unit_col = actual_unit_col))
}


#' @title Process Split-By Parameter
#'
#' @description Processes the split_by parameter to handle different cases consistently.
#'
#' @param data A data frame to process.
#' @param split_by NULL, a logical value, or a character vector of column names.
#'
#' @return A list with three elements: data (the possibly modified data frame), is_macro_mode (logical), and split_by (the processed split-by column names).
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.process_split_by <- function(data, split_by) {
  # Process split_by parameter
  if (is.null(split_by) || (is.logical(split_by) && !split_by)) {
    return(list(data = data, is_macro_mode = TRUE, split_by = NULL))
  }

  # For single column split_by
  if (length(split_by) == 1) {
    actual_split_by <- .find_column(data, split_by)

    if (is.null(actual_split_by)) {
      warning(paste("Split-by column", split_by, "not found. Using default values."))
      data[[split_by]] <- "Default"
      actual_split_by <- split_by
    }

    return(list(data = data, is_macro_mode = FALSE, split_by = actual_split_by))
  }

  # For multiple split_by columns
  actual_split_by <- character(length(split_by))
  for (i in seq_along(split_by)) {
    found_col <- .find_column(data, split_by[i])

    if (is.null(found_col)) {
      warning(paste("Split-by column", split_by[i], "not found. Using default values."))
      data[[split_by[i]]] <- "Default"
      actual_split_by[i] <- split_by[i]
    } else {
      actual_split_by[i] <- found_col
    }
  }

  return(list(data = data, is_macro_mode = FALSE, split_by = actual_split_by))
}


#' @title Format Variable Names Using Descriptions
#'
#' @description Formats variable names using available descriptions for better readability.
#'
#' @param data A data frame containing variable and description columns.
#' @param variable_col Name of the column containing variable identifiers.
#' @param desc_col Name of the column containing descriptions.
#' @param var_name_by_description Logical. If TRUE, replaces variable names with descriptions.
#' @param add_var_info Logical. If TRUE, adds additional information in parentheses.
#'
#' @return A data frame with formatted variable names.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.format_variable_names <- function(data, variable_col, desc_col,
                                   var_name_by_description = TRUE, add_var_info = FALSE) {
  if (!is.data.frame(data) || !variable_col %in% names(data))
    return(data)

  # If no Description column, return data unchanged
  if (!desc_col %in% names(data))
    return(data)

  result <- data

  for (i in seq_len(nrow(result))) {
    var_ <- result[[variable_col]][i]
    des_ <- result[[desc_col]][i]

    # Handle missing or empty description
    if (is.na(des_) || !nzchar(des_))
      des_ <- var_

    if (var_name_by_description && add_var_info) {
      # Both: Description (Variable)
      result[[variable_col]][i] <- paste0(des_, " (", var_, ")")
    } else if (var_name_by_description && !add_var_info) {
      # Description only
      result[[variable_col]][i] <- des_
    } else if (!var_name_by_description && add_var_info) {
      # Variable (Description), but only if different
      if (des_ != var_) {
        result[[variable_col]][i] <- paste0(var_, " (", des_, ")")
      }
    } else {
      # Variable only (default, do nothing)
    }
  }

  return(result)
}


#' @title Get Title Mapping From Variable Names and Descriptions
#'
#' @description Creates a mapping from original variable names to formatted display names.
#'
#' @param data A data frame or list of data frames containing variable and description columns.
#' @param variable_col Name of the column containing variable identifiers.
#' @param desc_col Name of the column containing descriptions.
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable names.
#' @param add_var_info Logical. If TRUE, adds additional information in parentheses.
#'
#' @return A named list mapping original variable names to formatted display names.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.get_title_mapping <- function(data, variable_col, desc_col,
                               var_name_by_description = TRUE, add_var_info = FALSE) {
  extract_title_mapping <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    if (!variable_col %in% names(df)) return(NULL)

    # Apply the variable name formatting
    formatted_df <- .format_variable_names(df, variable_col, desc_col, var_name_by_description, add_var_info)

    # Create mapping from original Variable to formatted Variable
    unique_vars <- unique(data.frame(
      OrigVar = df[[variable_col]],
      DisplayVar = formatted_df[[variable_col]],
      stringsAsFactors = FALSE
    ))

    setNames(unique_vars$DisplayVar, unique_vars$OrigVar)
  }

  if (is.data.frame(data)) {
    return(extract_title_mapping(data))
  } else if (is.list(data)) {
    result <- lapply(data, extract_title_mapping)
    result <- result[!sapply(result, is.null)]
    return(do.call(c, result))
  } else {
    stop("Unsupported data type. Input should be a data frame or a list of data frames.")
  }
}


# COLOR PALETTE  -----------------------------------

#' @title Create Academic and Themed Color Palettes
#'
#' @description Creates professionally designed color palettes for various plot types
#' based on the specified theme or color tone.
#'
#' @param color_tone Character. Base color, palette name, or theme ("academic", "purdue", etc.).
#' @param n_colors Numeric. Number of colors needed in the palette.
#' @param palette_type Character. Type of palette to generate: "sequential", "diverging", or "qualitative".
#'
#' @return A character vector of colors in hexadecimal format, or NULL if color_tone isn't a recognized theme.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.create_color_palette <- function(color_tone = NULL, n_colors = 5,
                                  palette_type = "qualitative") {

  themed_palettes <- list(
    academic = list(
      qualitative = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB"),
      sequential = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
      diverging = c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
    ),
    purdue = list(
      qualitative = c("#9D9E9E", "#DAAA00", "#C28E0E", "#000000", "#7A6E0B", "#98700D", "#4D4038"),
      sequential = c("#FFFFFF", "#F6F0D8", "#EBE1B2", "#E2D48E", "#DAAA00", "#C28E0E", "#98700D", "#7A6E0B", "#000000"),
      diverging = c("#000000", "#4D4038", "#98700D", "#C28E0E", "#DAAA00", "#E2D48E", "#EBE1B2", "#F6F0D8", "#FFFFFF")
    ),
    colorblind = list(
      qualitative = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
      sequential = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"),
      diverging = c("#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F")
    ),
    economic = list(
      qualitative = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
      sequential = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"),
      diverging = c("#1A1A40", "#306BAC", "#84B5D5", "#DEF5F7", "#FEFEFE", "#FEDEBE", "#DB7352", "#A6313A", "#67001F")
    ),
    trade = list(
      qualitative = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F"),
      sequential = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
      diverging = c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
    )
  )

  if (!is.null(color_tone) && tolower(color_tone) %in% names(themed_palettes)) {
    palette <- themed_palettes[[tolower(color_tone)]][[palette_type]]

    if (length(palette) < n_colors) {
      palette <- grDevices::colorRampPalette(palette)(n_colors)
    } else if (length(palette) > n_colors) {
      palette <- palette[1:n_colors]
    }

    return(palette)
  }

  return(NULL)
}


#' @title Generate Comparison Colors for Bar Charts
#'
#' @description Generates a color palette for comparison plots based on a base color tone.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param color_tone Character. Base color for generating shades, or a theme name.
#' @param axis_col Column name used for determining the number of unique colors needed.
#'
#' @return A vector of colors in hexadecimal format, or NULL if color_tone is NULL.
#'
#' @importFrom colorspace hex2RGB hex polarLUV
#' @importFrom grDevices col2rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}
#'
.generate_comparison_colors <- function(data, color_tone = NULL, axis_col) {
  if(is.null(color_tone)) return(NULL)

  n_colors <- length(unique(data[[axis_col]]))

  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_colors,
                                          palette_type = "qualitative")

  if (!is.null(themed_palette)) {
    return(themed_palette)
  }

  base_color <- if(startsWith(color_tone, "#")) {
    color_tone
  } else {
    colorspace::hex(colorspace::sRGB(t(col2rgb(color_tone) / 255)))
  }

  base_rgb <- colorspace::hex2RGB(base_color)
  base_hcl <- as(base_rgb, "polarLUV")

  hue <- base_hcl@coords[, "H"]
  chroma_range <- seq(max(30, base_hcl@coords[, "C"] - 20),
                      min(100, base_hcl@coords[, "C"] + 20),
                      length.out = n_colors)
  luminance_range <- seq(max(30, base_hcl@coords[, "L"] - 20),
                         min(90, base_hcl@coords[, "L"] + 20),
                         length.out = n_colors)

  sapply(1:n_colors, function(i) {
    colorspace::hex(colorspace::polarLUV(L = luminance_range[i],
                                         C = chroma_range[i],
                                         H = hue))
  })
}


#' @title Generate Color Palette for Positive/Negative Comparison
#'
#' @description Generates a color palette with shades for positive, negative, and neutral values.
#'
#' @param positive_color Character. Hex code or color name for the positive color (default: "#2E8B57", sea green).
#' @param negative_color Character. Hex code or color name for the negative color (default: "#CD5C5C", indian red).
#'
#' @return A named vector containing hex codes for different value categories.
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{detail_plot}}
#'
.generate_color_palette <- function(positive_color = "#2E8B57", negative_color = "#CD5C5C") {
  adjust_shade <- function(color, factor = 0.7) {
    rgb_col <- col2rgb(color)
    lighter <- rgb_col + (255 - rgb_col) * (1 - factor)
    return(rgb(lighter[1], lighter[2], lighter[3], maxColorValue = 255))
  }

  c(
    "extreme_positive" = positive_color,
    "normal_positive" = adjust_shade(positive_color),
    "extreme_negative" = negative_color,
    "normal_negative" = adjust_shade(negative_color),
    "neutral" = "gray"
  )
}


#' @title Generate Colors for Stacked Bar Components
#'
#' @description Generates a color palette for stacked bar components, ensuring good contrast between items.
#'
#' @param data A data frame containing the stack value column.
#' @param stack_value_from Column name containing the stack categories.
#' @param color_tone Optional base color to influence the palette.
#'
#' @return A named vector of colors for each stack component.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{stack_plot}}
#'
.generate_stack_colors <- function(data, stack_value_from, color_tone = NULL) {
  components <- unique(data[[stack_value_from]])
  n_components <- length(components)

  if (n_components <= 1) {
    return(setNames(c("#4477AA"), components))
  }

  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_components,
                                          palette_type = "qualitative")

  if (!is.null(themed_palette)) {
    return(setNames(themed_palette, components))
  }

  default_palette <- c("#4C78A8", "#F58518", "#E45756", "#72B7B2", "#54A24B", "#EECA3B", "#B279A2", "#FF9DA6")

  if (n_components > length(default_palette)) {
    default_palette <- rep(default_palette, ceiling(n_components / length(default_palette)))
  }

  default_palette <- default_palette[1:n_components]
  return(setNames(default_palette, components))
}


# LAYOUT AND DIMENSIONS HELPERS ---------------------------------------

#' @title Calculate Optimal Panel Layout
#'
#' @description Determines the optimal panel layout (rows and columns) for plotting based on the number of panels needed.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param panel_rows Optional. Number of rows for the panel layout.
#' @param panel_cols Optional. Number of columns for the panel layout.
#' @param panel_var Column name to use for determining unique panels.
#'
#' @return A list with 'rows' and 'cols' specifying the calculated panel layout.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL,
                                    panel_var = "Experiment") {
  # DETERMINE NUMBER OF PANELS
  num_panels <- length(unique(data[[panel_var]]))

  # HANDLE WHEN ONLY ONE DIMENSION IS SPECIFIED
  if (!is.null(panel_rows) && is.null(panel_cols)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (is.null(panel_rows) && !is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_rows) && !is.null(panel_cols)) {
    if (panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  }

  # AUTO CALCULATE LAYOUT WHEN NEITHER DIMENSION IS SPECIFIED
  if (num_panels <= 1) {
    return(list(rows = 1, cols = 1))
  } else if (num_panels <= 3) {
    return(list(rows = 1, cols = num_panels))
  } else if (num_panels <= 4) {
    return(list(rows = 2, cols = 2))
  } else if (num_panels <= 6) {
    return(list(rows = 2, cols = 3))
  } else if (num_panels <= 9) {
    return(list(rows = 3, cols = 3))
  } else if (num_panels <= 12) {
    return(list(rows = 3, cols = 4))
  } else {
    factors <- c()
    for (i in 1:sqrt(num_panels)) {
      if (num_panels %% i == 0) {
        factors <- c(factors, i)
      }
    }

    if (length(factors) > 0) {
      best_factor <- factors[length(factors)]
      rows <- best_factor
      cols <- num_panels / best_factor
    } else {
      cols <- ceiling(sqrt(num_panels))
      rows <- ceiling(num_panels / cols)
    }

    if (cols > 2 * rows) {
      new_cols <- ceiling(sqrt(num_panels))
      new_rows <- ceiling(num_panels / new_cols)
      rows <- new_rows
      cols <- new_cols
    }

    return(list(rows = rows, cols = cols))
  }
}

#' @title Calculate Plot Dimensions
#'
#' @description
#' Calculates appropriate width and height for a plot based on the panel layout.
#' Uses a simple formula that increases dimensions with the number of panels.
#'
#' @param data A data frame containing the plotting data (not used in dimension calculation).
#' @param panel_layout A list containing 'rows' and 'cols' specifying the panel layout.
#'
#' @return A list with 'width' and 'height' specifying the calculated plot dimensions in inches.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_dimensions <- function(data, panel_layout) {
  num_panels <- panel_layout$rows * panel_layout$cols
  base_width <- 20
  base_height <- 30

  width <- if(num_panels <= 4) {
    base_width
  } else {
    min(base_width + (num_panels - 4) * 3.5, 50)
  }

  height <- base_height * 0.75

  return(list(width = width, height = height))
}


#  EXPORT -----------------------------------------------------------------

#' @title Export Plots to Files
#'
#' @description
#' Exports one or more ggplot objects to PNG and/or PDF files.
#' Supports exporting to separate files or creating a single merged PDF with multiple pages.
#'
#' @param plots A ggplot object or a list of ggplot objects to export.
#' @param output_path Character. Directory where the plots will be saved. Default is current working directory.
#' @param export_picture Logical. If TRUE, exports the plots as PNG files. Default is TRUE.
#' @param export_as_pdf Logical or character. If TRUE, exports the plots as PDF files.
#'        If "merged", creates a single multi-page PDF. Default is FALSE.
#' @param export_config List. Configuration parameters for export:
#'        \itemize{
#'          \item \code{width}: Plot width in inches.
#'          \item \code{height}: Plot height in inches.
#'          \item \code{dpi}: Resolution for PNG export (default: 300).
#'          \item \code{bg}: Background color (default: "white").
#'          \item \code{limitsize}: Whether to limit size (default: FALSE).
#'          \item \code{file_name}: Base name for exported files (default: "gtap_plots").
#'        }
#' @param data A data frame used for automatic dimension calculation if not specified in export_config.
#' @param panel_layout A list with 'rows' and 'cols' for automatic dimension calculation.
#'
#' @return Invisibly returns the input plots.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.export_plot_output <- function(plots,
                                output_path = NULL,
                                export_picture = TRUE,
                                export_as_pdf = FALSE,
                                export_config = NULL,
                                data = NULL,
                                panel_layout = NULL) {

  if (!export_picture) {
    return(invisible(plots))
  }

  # Prepare export configuration
  if (is.null(export_config)) {
    export_config <- list()
  }

  # Default export settings if not specified
  if (is.null(export_config$dpi)) export_config$dpi <- 300
  if (is.null(export_config$bg)) export_config$bg <- "white"
  if (is.null(export_config$limitsize)) export_config$limitsize <- FALSE
  if (is.null(export_config$file_name)) export_config$file_name <- "gtap_plots"

  # Handle custom dimensions if provided
  if (is.null(export_config$width) || is.null(export_config$height)) {
    dimensions <- .calculate_plot_dimensions(data, panel_layout)
    export_config$width <- dimensions$width
    export_config$height <- dimensions$height
  }

  # Create output directory if needed
  if (is.null(output_path)) {
    output_path <- getwd()
  }

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  is_single_plot <- inherits(plots, "gg")

  if (is_single_plot) {
    plots <- list(plot = plots)
  }

  if (!is.list(plots) || length(plots) == 0) {
    stop("plots must be a ggplot object or a non-empty list of ggplot objects")
  }

  if (!all(sapply(plots, function(p) inherits(p, "gg")))) {
    stop("All elements in plots must be ggplot objects")
  }

  n_plots <- length(plots)

  # For individual plots, use the export_config file name
  base_file_name <- export_config$file_name

  is_merge_pdf <- FALSE
  if (is.character(export_as_pdf)) {
    if (tolower(export_as_pdf) == "merged") {
      is_merge_pdf <- TRUE
      export_as_pdf <- TRUE
    }
  }

  if (export_as_pdf) {
    if (is_merge_pdf && n_plots >= 1) {
      # Determine the plot type based on the calling function
      calling_func <- sys.call(-1)[[1]]
      if (is.name(calling_func)) {
        calling_func <- as.character(calling_func)

        if (grepl("comparison_plot", calling_func)) {
          pdf_base_name <- "Comparison_plot"
        } else if (grepl("detail_plot", calling_func)) {
          pdf_base_name <- "Detail_plot"
        } else if (grepl("stack_plot", calling_func)) {
          pdf_base_name <- "Stack_plot"
        } else {
          pdf_base_name <- "Plots"
        }
      } else {
        pdf_base_name <- "Plots"
      }

      # Add number of plots to the filename
      pdf_file_name <- paste0(pdf_base_name, "_", n_plots)
      pdf_path <- file.path(output_path, paste0(pdf_file_name, ".pdf"))

      grDevices::pdf(
        file = pdf_path,
        width = export_config$width,
        height = export_config$height,
        useDingbats = FALSE,
        title = pdf_file_name
      )

      on.exit(grDevices::dev.off())

      for (i in seq_along(plots)) {
        print(plots[[i]])
      }

      message("Combined PDF exported to: ", pdf_path)
    } else {
      # Individual PDF export
      if (n_plots == 1) {
        p <- plots[[1]]
        pdf_path <- file.path(output_path, paste0(base_file_name, ".pdf"))

        ggplot2::ggsave(
          filename = pdf_path,
          plot = p,
          device = "pdf",
          width = export_config$width,
          height = export_config$height,
          dpi = export_config$dpi,
          bg = export_config$bg,
          limitsize = export_config$limitsize
        )

        message("PDF figure exported to: ", pdf_path)
      } else {
        # Multiple plots: use individual plot names
        for (i in seq_along(plots)) {
          p <- plots[[i]]
          plot_name <- names(plots)[[i]]

          pdf_path <- file.path(output_path, paste0(plot_name, ".pdf"))

          ggplot2::ggsave(
            filename = pdf_path,
            plot = p,
            device = "pdf",
            width = export_config$width,
            height = export_config$height,
            dpi = export_config$dpi,
            bg = export_config$bg,
            limitsize = export_config$limitsize
          )

          message("PDF figure exported to: ", pdf_path)
        }
      }
    }
  }

  if (export_picture) {
    # If there's only one plot, use the base filename without numbering
    if (n_plots == 1) {
      p <- plots[[1]]
      plot_name <- names(plots)[[1]]

      png_path <- file.path(output_path, paste0(plot_name, ".png"))

      ggplot2::ggsave(
        filename = png_path,
        plot = p,
        device = "png",
        width = export_config$width,
        height = export_config$height,
        dpi = export_config$dpi,
        bg = export_config$bg,
        limitsize = export_config$limitsize
      )

      message("PNG figure exported to: ", png_path)
    } else {
      # Multiple plots: use individual plot names
      for (i in seq_along(plots)) {
        p <- plots[[i]]
        plot_name <- names(plots)[[i]]

        png_path <- file.path(output_path, paste0(plot_name, ".png"))

        ggplot2::ggsave(
          filename = png_path,
          plot = p,
          device = "png",
          width = export_config$width,
          height = export_config$height,
          dpi = export_config$dpi,
          bg = export_config$bg,
          limitsize = export_config$limitsize
        )

        message("PNG figure exported to: ", png_path)
      }
    }
  }

  invisible(plots)
}
