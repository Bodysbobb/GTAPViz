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
#' @param printing Logical. If TRUE, prints a formatted code snippet that can be copied
#'   and pasted to recreate the configuration. Default is FALSE.
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
#' all_configs <- get_all_config()
#'
#' # Get only comparison plot configuration
#' comp_config <- get_all_config("default")
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
get_all_config <- function(plot_style = "default", config = NULL,
                           export_config = NULL, printing = FALSE) {
  valid_styles <- c("default")
  if (!plot_style %in% valid_styles) {
    stop("Plot style must be one of: 'default'")
  }

  # If printing mode, we'll just print and return nothing
  if (printing) {
    # Print the plot style configuration
    get_plot_style_config(plot_type = plot_style, validate_custom = config, printing = TRUE)

    # Add some separation between the two sections
    cat("\n\n")

    # Print the export configuration
    get_export_config(printing = TRUE)

    # Return invisibly to avoid printing any values
    return(invisible(NULL))
  }

  # Normal (non-printing) mode continues here
  result <- list()

  if (plot_style == "all") {
    result$plot_style_config <- list(
      comparison = .calculate_plot_style_config(config, "default")
    )
  } else {
    # Get plot style as dataframe
    result$plot_style_config <- get_plot_style_config(plot_type = plot_style,
                                                      validate_custom = config,
                                                      as_dataframe = TRUE)
  }

  # Get export configuration as dataframe
  result$export_config <- get_export_config(as_dataframe = TRUE)

  return(result)
}


#' @title Get Plot Style Configuration
#'
#' @description
#' Returns configuration settings for plot styles, with options to view as a structured dataframe
#' or to look up specific parameters. Also provides parameter validation for custom configurations.
#'
#' @param plot_type Character. Type of plot: "default" (default).
#' @param parameter_name Character or NULL. Name of specific parameter to return information about.
#' @param show_docs Logical. Whether to include documentation in the output.
#' @param validate_custom List or NULL. Custom configuration settings to validate.
#' @param as_dataframe Logical. Whether to return settings as a dataframe.
#' @param printing Logical. If TRUE, prints a formatted code snippet that can be copied
#'   and pasted to recreate the configuration. Default is FALSE.
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
#' - `title_margin`: Numeric vector `c(top, right, bottom, left)`. Default: `c(10, 0, 10, 0)`
#' - `title_format`: List or NULL. Formatting options for the title, with elements:
#'   \itemize{
#'     \item \code{type}: Character. One of "prefix", "suffix", "full", or "dynamic".
#'     \item \code{text}: Character. Text to add, or column names used for dynamic titles.
#'     \item \code{sep}: Character. Separator used for dynamic titles. Default: " - ".
#'   }
#'
#' ## **X-Axis Settings**
#' - `show_x_axis_title`: Logical. Show or hide x-axis title. Default: `TRUE`
#' - `x_axis_title_face`: Character. Font face for x-axis title. Default: `"bold"`
#' - `x_axis_title_size`: Numeric. Font size of x-axis title. Default: `16`
#' - `x_axis_title_margin`: Numeric vector `c(top, right, bottom, left)`. Default: `c(25, 25, 0, 0)`
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
#' - `y_axis_title_margin`: Numeric vector `c(top, right, bottom, left)`. Default: `c(25, 25, 0, 0)`
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
#' - `strip_text_margin`: Numeric vector `c(top, right, bottom, left)`. Default: `c(10, 0, 10, 0)`
#'
#' ## **Panel Layout**
#' - `panel_spacing`: Numeric. Spacing between panels. Default: `2`
#' - `panel_rows`: Numeric or `NULL`. Number of rows in panel layout. Default: `NULL`
#' - `panel_cols`: Numeric or `NULL`. Number of columns in panel layout. Default: `NULL`
#' - `theme`: ggplot2 theme object or `NULL`. Custom ggplot theme. Default: `NULL`
#'
#' ## **Color and Grid Settings**
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
#' ## **Plot Margin Settings**
#' - `plot.margin`: Numeric vector `c(top, right, bottom, left)`. Margins around the entire plot. Default: `c(10, 25, 10, 10)`
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#' # Get all default configuration
#' get_plot_style_config(printing = TRUE)
#'
#' # Get information about a specific parameter
#' param_info <- get_plot_style_config("default", "bar_width")
#'
#' # Get as structured dataframe
#' config_df <- get_plot_style_config("default", as_dataframe = TRUE)
#'
#' # Validate custom configuration
#' custom_config <- list(title_size = 24, bar_width = 0.7, plot.margin = c(10, 50, 10, 10))
#' validated <- get_plot_style_config("default", validate_custom = custom_config)
#'
get_plot_style_config <- function(plot_type = "default",
                                  parameter_name = NULL,
                                  show_docs = FALSE,
                                  validate_custom = NULL,
                                  as_dataframe = FALSE,
                                  printing = TRUE) {
  config <- .calculate_plot_style_config(NULL, plot_type)

  param_docs <- list(
    show_title = "Logical. Show or hide the plot title.",
    title_face = "Character. Font face for title ('bold', 'plain', 'italic').",
    title_size = "Numeric. Font size of title.",
    title_hjust = "Numeric. Horizontal justification of title (0 = left, 1 = right).",
    add_unit_to_title = "Logical. Add unit information to title.",
    title_margin = "Numeric vector c(top, right, bottom, left). Margin around title.",
    title_format = "List with components: 'type' (options: 'standard', 'prefix', 'suffix', 'full', 'dynamic'), 'text' (content to display or column names for dynamic titles), and 'sep' (separator for dynamic titles, default: ' - ').",
    show_x_axis_title = "Logical. Show or hide the x-axis title.",
    x_axis_title_face = "Character. Font face for x-axis title.",
    x_axis_title_size = "Numeric. Font size of x-axis title.",
    x_axis_title_margin = "Numeric vector c(top, right, bottom, left). Margin around x-axis title.",
    show_x_axis_labels = "Logical. Show or hide x-axis tick labels.",
    x_axis_text_face = "Character. Font face for x-axis tick labels.",
    x_axis_text_size = "Numeric. Font size of x-axis tick labels.",
    x_axis_text_angle = "Numeric. Angle of x-axis tick labels in degrees.",
    x_axis_text_hjust = "Numeric. Horizontal justification of x-axis tick labels.",
    x_axis_description = "Character. Optional description for x-axis.",
    show_y_axis_title = "Logical. Show or hide the y-axis title.",
    y_axis_title_face = "Character. Font face for y-axis title.",
    y_axis_title_size = "Numeric. Font size of y-axis title.",
    y_axis_title_margin = "Numeric vector c(top, right, bottom, left). Margin around y-axis title.",
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
    strip_text_margin = "Numeric vector c(top, right, bottom, left). Margin around panel strip labels.",
    panel_spacing = "Numeric. Spacing between panels in centimeters.",
    panel_rows = "Numeric or NULL. Number of rows in panel layout.",
    panel_cols = "Numeric or NULL. Number of columns in panel layout.",
    theme = "ggplot2 theme object or NULL. Custom theme to apply.",
    color_tone = "Character or NULL. Base color tone for the plot (e.g., 'academic', 'purdue').",
    color_palette_type = "Character. Type of color palette ('qualitative', 'sequential', or 'diverging'). Default: 'qualitative'",
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
    all_font_size = "Numeric. Master control for all font sizes. Values > 1 increase all fonts, values < 1 decrease all fonts.",
    plot.margin = "Numeric vector c(top, right, bottom, left). Margins around the entire plot."
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

  # Create a dataframe for as_dataframe = TRUE
  if (as_dataframe) {
    # Count the total number of parameters
    params_count <- 71

    # Create vectors with the exact same length
    topics <- character(params_count)
    arguments <- character(params_count)
    default_values <- character(params_count)
    input_formats <- character(params_count)
    descriptions <- character(params_count)
    examples <- character(params_count)

    # Fill in the values (double check the counts!)

    # Title section (7 parameters)
    topics[1:7] <- c("Title", "", "", "", "", "", "")
    arguments[1:7] <- c("show_title", "title_face", "title_size", "title_hjust", "add_unit_to_title", "title_margin", "title_format")
    default_values[1:7] <- c("TRUE", "bold", "20", "0.5", "TRUE", "c(10, 0, 10, 0)",
                             "list(type=\"standard\" [options: standard/prefix/suffix/full/dynamic], text=\"\", sep=\" - \")")
    input_formats[1:7] <- c("logical", "character", "numeric", "numeric", "logical", "numeric vector", "list")
    descriptions[1:7] <- c(
      "Show or hide the plot title.",
      "Font face for title ('bold', 'plain', 'italic').",
      "Font size of title.",
      "Horizontal justification of title (0 = left, 1 = right).",
      "Add unit information to title.",
      "Margin around title (top, right, bottom, left).",
      "List with components: 'type' (options: 'standard', 'prefix', 'suffix', 'full', 'dynamic'), 'text' (content to display or column names for dynamic titles), and 'sep' (separator for dynamic titles, default: ' - ')."
    )
    examples[1:7] <- c(
      "show_title = TRUE",
      "title_face = \"bold\"",
      "title_size = 20",
      "title_hjust = 0.5",
      "add_unit_to_title = TRUE",
      "title_margin = c(10, 0, 10, 0)",
      "title_format = list(\n  type = \"standard\", # options: standard/prefix/suffix/full/dynamic\n  text = \"\",\n  sep = \"\"\n)"
    )

    # X-Axis section (10 parameters)
    idx <- 8:17
    topics[idx] <- c("X-Axis", rep("", length(idx)-1))
    arguments[idx] <- c("show_x_axis_title", "x_axis_title_face", "x_axis_title_size", "x_axis_title_margin",
                        "show_x_axis_labels", "x_axis_text_face", "x_axis_text_size", "x_axis_text_angle",
                        "x_axis_text_hjust", "x_axis_description")
    default_values[idx] <- c("TRUE", "bold", "16", "c(25, 25, 0, 0)", "TRUE", "plain", "14", "0", "0", "")
    input_formats[idx] <- c("logical", "character", "numeric", "numeric vector", "logical", "character", "numeric", "numeric", "numeric", "character")
    descriptions[idx] <- c(
      "Show or hide the x-axis title.",
      "Font face for x-axis title.",
      "Font size of x-axis title.",
      "Margin around x-axis title (top, right, bottom, left).",
      "Show or hide x-axis tick labels.",
      "Font face for x-axis tick labels.",
      "Font size of x-axis tick labels.",
      "Angle of x-axis tick labels in degrees.",
      "Horizontal justification of x-axis tick labels.",
      "Optional description for x-axis."
    )
    examples[idx] <- c(
      "show_x_axis_title = TRUE",
      "x_axis_title_face = \"bold\"",
      "x_axis_title_size = 16",
      "x_axis_title_margin = c(25, 25, 0, 0)",
      "show_x_axis_labels = TRUE",
      "x_axis_text_face = \"plain\"",
      "x_axis_text_size = 14",
      "x_axis_text_angle = 0",
      "x_axis_text_hjust = 0",
      "x_axis_description = \"\""
    )

    # Y-Axis section (11 parameters)
    idx <- 18:28
    topics[idx] <- c("Y-Axis", rep("", length(idx)-1))
    arguments[idx] <- c("show_y_axis_title", "y_axis_title_face", "y_axis_title_size", "y_axis_title_margin",
                        "show_y_axis_labels", "y_axis_text_face", "y_axis_text_size", "y_axis_text_angle",
                        "y_axis_text_hjust", "y_axis_description", "show_axis_titles_on_all_facets")
    default_values[idx] <- c("TRUE", "bold", "16", "c(25, 25, 0, 0)", "TRUE", "plain", "14", "0", "0", "", "TRUE")
    input_formats[idx] <- c("logical", "character", "numeric", "numeric vector", "logical", "character", "numeric", "numeric", "numeric", "character", "logical")
    descriptions[idx] <- c(
      "Show or hide the y-axis title.",
      "Font face for y-axis title.",
      "Font size of y-axis title.",
      "Margin around y-axis title (top, right, bottom, left).",
      "Show or hide y-axis tick labels.",
      "Font face for y-axis tick labels.",
      "Font size of y-axis tick labels.",
      "Angle of y-axis tick labels in degrees.",
      "Horizontal justification of y-axis tick labels.",
      "Optional description for y-axis.",
      "Show axis titles on all facets."
    )
    examples[idx] <- c(
      "show_y_axis_title = TRUE",
      "y_axis_title_face = \"bold\"",
      "y_axis_title_size = 16",
      "y_axis_title_margin = c(25, 25, 0, 0)",
      "show_y_axis_labels = TRUE",
      "y_axis_text_face = \"plain\"",
      "y_axis_text_size = 14",
      "y_axis_text_angle = 0",
      "y_axis_text_hjust = 0",
      "y_axis_description = \"\"",
      "show_axis_titles_on_all_facets = TRUE"
    )

    # Value Labels section (5 parameters)
    idx <- 29:33
    topics[idx] <- c("Value Labels", rep("", length(idx)-1))
    arguments[idx] <- c("show_value_labels", "value_label_face", "value_label_size", "value_label_position", "value_label_decimal_places")
    default_values[idx] <- c("TRUE", "plain", "5", "above", "2")
    input_formats[idx] <- c("logical", "character", "numeric", "character", "numeric")
    descriptions[idx] <- c(
      "Show or hide value labels.",
      "Font face for value labels.",
      "Font size of value labels.",
      "Position of value labels ('above', 'outside', 'top').",
      "Number of decimal places in value labels."
    )
    examples[idx] <- c(
      "show_value_labels = TRUE",
      "value_label_face = \"plain\"",
      "value_label_size = 5",
      "value_label_position = \"above\"",
      "value_label_decimal_places = 2"
    )

    # Legend section (6 parameters)
    idx <- 34:39
    topics[idx] <- c("Legend", rep("", length(idx)-1))
    arguments[idx] <- c("show_legend", "show_legend_title", "legend_position", "legend_title_face", "legend_text_face", "legend_text_size")
    default_values[idx] <- c("FALSE", "FALSE", "bottom", "bold", "plain", "14")
    input_formats[idx] <- c("logical", "logical", "character", "character", "character", "numeric")
    descriptions[idx] <- c(
      "Show or hide the legend.",
      "Show or hide the legend title.",
      "Position of the legend ('none', 'right', 'bottom', etc.).",
      "Font face for legend title.",
      "Font face for legend text.",
      "Font size of legend text."
    )
    examples[idx] <- c(
      "show_legend = FALSE",
      "show_legend_title = FALSE",
      "legend_position = \"bottom\"",
      "legend_title_face = \"bold\"",
      "legend_text_face = \"plain\"",
      "legend_text_size = 14"
    )

    # Panel Strip section (4 parameters)
    idx <- 40:43
    topics[idx] <- c("Panel Strip", rep("", length(idx)-1))
    arguments[idx] <- c("strip_face", "strip_text_size", "strip_background", "strip_text_margin")
    default_values[idx] <- c("bold", "16", "lightgrey", "c(10, 0, 10, 0)")
    input_formats[idx] <- c("character", "numeric", "character", "numeric vector")
    descriptions[idx] <- c(
      "Font face for panel strip labels.",
      "Font size of panel strip labels.",
      "Background color of panel strips.",
      "Margin around panel strip labels (top, right, bottom, left)."
    )
    examples[idx] <- c(
      "strip_face = \"bold\"",
      "strip_text_size = 16",
      "strip_background = \"lightgrey\"",
      "strip_text_margin = c(10, 0, 10, 0)"
    )

    # Panel Layout section (4 parameters)
    idx <- 44:47
    topics[idx] <- c("Panel Layout", rep("", length(idx)-1))
    arguments[idx] <- c("panel_spacing", "panel_rows", "panel_cols", "theme")
    default_values[idx] <- c("2", "NULL", "NULL", "NULL")
    input_formats[idx] <- c("numeric", "NULL or numeric", "NULL or numeric", "ggplot2 theme")
    descriptions[idx] <- c(
      "Spacing between panels in centimeters.",
      "Number of rows in panel layout.",
      "Number of columns in panel layout.",
      "Custom theme to apply."
    )
    examples[idx] <- c(
      "panel_spacing = 2",
      "panel_rows = NULL",
      "panel_cols = NULL",
      "theme = NULL"
    )

    # Colors section (10 parameters) - Updated to include color_palette_type
    idx <- 48:57
    topics[idx] <- c("Colors", rep("", length(idx)-1))
    arguments[idx] <- c("color_tone", "color_palette_type", "positive_color", "negative_color", "background_color", "grid_color",
                        "show_grid_major_x", "show_grid_major_y", "show_grid_minor_x", "show_grid_minor_y")
    default_values[idx] <- c("NULL", "\"qualitative\"", "#2E8B57", "#CD5C5C", "white", "grey90", "FALSE", "FALSE", "FALSE", "FALSE")
    input_formats[idx] <- c("character", "character", "character", "character", "character", "character", "logical", "logical", "logical", "logical")
    descriptions[idx] <- c(
      "Base color tone for the plot (e.g., 'academic', 'purdue').",
      "Type of color palette ('qualitative', 'sequential', or 'diverging').",
      "Color for positive values.",
      "Color for negative values.",
      "Background color of the plot.",
      "Color of grid lines.",
      "Show major grid lines on x-axis.",
      "Show major grid lines on y-axis.",
      "Show minor grid lines on x-axis.",
      "Show minor grid lines on y-axis."
    )
    examples[idx] <- c(
      "color_tone = NULL",
      "color_palette_type = \"qualitative\"",
      "positive_color = \"#2E8B57\"",
      "negative_color = \"#CD5C5C\"",
      "background_color = \"white\"",
      "grid_color = \"grey90\"",
      "show_grid_major_x = FALSE",
      "show_grid_major_y = FALSE",
      "show_grid_minor_x = FALSE",
      "show_grid_minor_y = FALSE"
    )

    # Zero Line section (5 parameters)
    idx <- 58:62
    topics[idx] <- c("Zero Line", rep("", length(idx)-1))
    arguments[idx] <- c("show_zero_line", "zero_line_type", "zero_line_color", "zero_line_size", "zero_line_position")
    default_values[idx] <- c("TRUE", "dashed", "black", "0.5", "0")
    input_formats[idx] <- c("logical", "character", "numeric", "numeric", "numeric")
    descriptions[idx] <- c(
      "Show or hide the zero line.",
      "Line type for zero line ('solid', 'dashed', 'dotted').",
      "Color of zero line.",
      "Line thickness of zero line.",
      "Position of the zero line."
    )
    examples[idx] <- c(
      "show_zero_line = TRUE",
      "zero_line_type = \"dashed\"",
      "zero_line_color = \"black\"",
      "zero_line_size = 0.5",
      "zero_line_position = 0"
    )

    # Bar Chart section (2 parameters)
    idx <- 63:64
    topics[idx] <- c("Bar Chart", rep("", length(idx)-1))
    arguments[idx] <- c("bar_width", "bar_spacing")
    default_values[idx] <- c("0.9", "0.9")
    input_formats[idx] <- c("numeric", "numeric")
    descriptions[idx] <- c(
      "Width of bars (0-1).",
      "Spacing between groups of bars."
    )
    examples[idx] <- c(
      "bar_width = 0.9",
      "bar_spacing = 0.9"
    )

    # Scale Settings section (2 parameters)
    idx <- 65:66
    topics[idx] <- c("Scale Settings", rep("", length(idx)-1))
    arguments[idx] <- c("scale_limit", "scale_increment")
    default_values[idx] <- c("NULL", "NULL")
    input_formats[idx] <- c("numeric vector", "numeric")
    descriptions[idx] <- c(
      "Manual limits for value axis (min, max).",
      "Step size for axis tick marks."
    )
    examples[idx] <- c(
      "scale_limit = NULL",
      "scale_increment = NULL"
    )

    # Scale Expansion section (2 parameters)
    idx <- 67:68
    topics[idx] <- c("Scale Expansion", rep("", length(idx)-1))
    arguments[idx] <- c("expansion_y_mult", "expansion_x_mult")
    default_values[idx] <- c("c(0.05, 0.1)", "c(0.05, 0.05)")
    input_formats[idx] <- c("numeric vector", "numeric vector")
    descriptions[idx] <- c(
      "Expansion multiplier for y-axis.",
      "Expansion multiplier for x-axis."
    )
    examples[idx] <- c(
      "expansion_y_mult = c(0.05, 0.1)",
      "expansion_x_mult = c(0.05, 0.05)"
    )

    # Font Size Control section (1 parameter)
    idx <- 69
    topics[idx] <- "Font Size Control"
    arguments[idx] <- "all_font_size"
    default_values[idx] <- "1"
    input_formats[idx] <- "numeric"
    descriptions[idx] <- "Master control for all font sizes. Values > 1 increase all fonts, values < 1 decrease all fonts."
    examples[idx] <- "all_font_size = 1"

    # Data Sorting section (1 parameter)
    idx <- 70
    topics[idx] <- "Data Sorting"
    arguments[idx] <- "sort_data_by_value"
    default_values[idx] <- "FALSE"
    input_formats[idx] <- "logical"
    descriptions[idx] <- "Whether to sort data by value for better visualization."
    examples[idx] <- "sort_data_by_value = TRUE"

    # Plot Margin section (1 parameter)
    idx <- 71
    topics[idx] <- "Plot Margin"
    arguments[idx] <- "plot.margin"
    default_values[idx] <- "c(10, 25, 10, 10)"
    input_formats[idx] <- "numeric vector"
    descriptions[idx] <- "Margins around the entire plot (top, right, bottom, left)."
    examples[idx] <- "plot.margin = c(10, 25, 10, 10)"

    # Create the result dataframe
    result <- data.frame(
      Topic = topics,
      Arguments = arguments,
      "Default Value" = default_values,
      "Input Format" = input_formats,
      Description = descriptions,
      Example = examples,
      stringsAsFactors = FALSE
    )

    # Assign to plot_style_config in parent environment
    assign("plot_style_config", result, envir = parent.frame())

    return(result)
  }

  if (printing) {
    cat("my_style_config <- list(\n")

    # Title settings
    cat("\n  # Title settings\n")
    cat("  show_title = ", ifelse(config$show_title, "TRUE", "FALSE"), ",\n", sep="")
    cat("  title_face = \"", config$title_face, "\",\n", sep="")
    cat("  title_size = ", config$title_size, ",\n", sep="")
    cat("  title_hjust = ", config$title_hjust, ",\n", sep="")
    cat("  add_unit_to_title = ", ifelse(config$add_unit_to_title, "TRUE", "FALSE"), ",\n", sep="")

    # Format margin objects as simple vectors with description
    margin_values <- as.numeric(config$title_margin)
    cat("  title_margin = c(", margin_values[1], ", ", margin_values[2],
        ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n", sep="")

    # Format title_format as a properly structured list
    tf <- config$title_format
    cat("  title_format = list(\n")
    cat("    type = \"", .coalesce(tf$type, "standard"), "\", #option: prefix, suffix, full, dynamic\n", sep="")
    cat("    text = \"", .coalesce(tf$text, ""), "\",\n", sep="")
    cat("    sep = \"", .coalesce(tf$sep, ""), "\"\n", sep="")
    cat("  ),\n")

    # X-Axis settings
    cat("\n  # X-Axis settings\n")
    cat("  show_x_axis_title = ", ifelse(config$show_x_axis_title, "TRUE", "FALSE"), ",\n", sep="")
    cat("  x_axis_title_face = \"", config$x_axis_title_face, "\",\n", sep="")
    cat("  x_axis_title_size = ", config$x_axis_title_size, ",\n", sep="")

    margin_values <- as.numeric(config$x_axis_title_margin)
    cat("  x_axis_title_margin = c(", margin_values[1], ", ", margin_values[2],
        ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n", sep="")

    cat("  show_x_axis_labels = ", ifelse(config$show_x_axis_labels, "TRUE", "FALSE"), ",\n", sep="")
    cat("  x_axis_text_face = \"", config$x_axis_text_face, "\",\n", sep="")
    cat("  x_axis_text_size = ", config$x_axis_text_size, ",\n", sep="")
    cat("  x_axis_text_angle = ", config$x_axis_text_angle, ",\n", sep="")
    cat("  x_axis_text_hjust = ", config$x_axis_text_hjust, ",\n", sep="")
    cat("  x_axis_description = \"", config$x_axis_description, "\",\n", sep="")

    # Y-Axis settings
    cat("\n  # Y-Axis settings\n")
    cat("  show_y_axis_title = ", ifelse(config$show_y_axis_title, "TRUE", "FALSE"), ",\n", sep="")
    cat("  y_axis_title_face = \"", config$y_axis_title_face, "\",\n", sep="")
    cat("  y_axis_title_size = ", config$y_axis_title_size, ",\n", sep="")

    margin_values <- as.numeric(config$y_axis_title_margin)
    cat("  y_axis_title_margin = c(", margin_values[1], ", ", margin_values[2],
        ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n", sep="")

    cat("  show_y_axis_labels = ", ifelse(config$show_y_axis_labels, "TRUE", "FALSE"), ",\n", sep="")
    cat("  y_axis_text_face = \"", config$y_axis_text_face, "\",\n", sep="")
    cat("  y_axis_text_size = ", config$y_axis_text_size, ",\n", sep="")
    cat("  y_axis_text_angle = ", config$y_axis_text_angle, ",\n", sep="")
    cat("  y_axis_text_hjust = ", config$y_axis_text_hjust, ",\n", sep="")
    cat("  y_axis_description = \"", config$y_axis_description, "\",\n", sep="")
    cat("  show_axis_titles_on_all_facets = ", ifelse(config$show_axis_titles_on_all_facets, "TRUE", "FALSE"), ",\n", sep="")

    # Value Labels
    cat("\n  # Value Labels\n")
    cat("  show_value_labels = ", ifelse(config$show_value_labels, "TRUE", "FALSE"), ",\n", sep="")
    cat("  value_label_face = \"", config$value_label_face, "\",\n", sep="")
    cat("  value_label_size = ", config$value_label_size, ",\n", sep="")
    cat("  value_label_position = \"", config$value_label_position, "\",\n", sep="")
    cat("  value_label_decimal_places = ", config$value_label_decimal_places, ",\n", sep="")

    # Legend
    cat("\n  # Legend\n")
    cat("  show_legend = ", ifelse(config$show_legend, "TRUE", "FALSE"), ",\n", sep="")
    cat("  show_legend_title = ", ifelse(config$show_legend_title, "TRUE", "FALSE"), ",\n", sep="")
    cat("  legend_position = \"", config$legend_position, "\",\n", sep="")
    cat("  legend_title_face = \"", config$legend_title_face, "\",\n", sep="")
    cat("  legend_text_face = \"", config$legend_text_face, "\",\n", sep="")
    cat("  legend_text_size = ", config$legend_text_size, ",\n", sep="")

    # Panel Strip
    cat("\n  # Panel Strip\n")
    cat("  strip_face = \"", config$strip_face, "\",\n", sep="")
    cat("  strip_text_size = ", config$strip_text_size, ",\n", sep="")
    cat("  strip_background = \"", config$strip_background, "\",\n", sep="")

    margin_values <- as.numeric(config$strip_text_margin)
    cat("  strip_text_margin = c(", margin_values[1], ", ", margin_values[2],
        ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n", sep="")

    # Panel Layout
    cat("\n  # Panel Layout\n")
    cat("  panel_spacing = ", config$panel_spacing, ",\n", sep="")
    cat("  panel_rows = ", if(is.null(config$panel_rows)) "NULL" else config$panel_rows, ",\n", sep="")
    cat("  panel_cols = ", if(is.null(config$panel_cols)) "NULL" else config$panel_cols, ",\n", sep="")
    cat("  theme = ", if(is.null(config$theme)) "NULL" else "custom_theme", ",\n", sep="")

    # Color
    cat("\n  # Colors and Grid \n")
    cat("  color_tone = ", if(is.null(config$color_tone)) "NULL" else paste0("\"", config$color_tone, "\""), ",\n", sep="")
    cat("  color_palette_type = \"", config$color_palette_type, "\", #option: qualitative, sequential, diverging\n", sep="")
    cat("  positive_color = \"", config$positive_color, "\",\n", sep="")
    cat("  negative_color = \"", config$negative_color, "\",\n", sep="")
    cat("  background_color = \"", config$background_color, "\",\n", sep="")
    cat("  grid_color = \"", config$grid_color, "\",\n", sep="")
    cat("  show_grid_major_x = ", ifelse(config$show_grid_major_x, "TRUE", "FALSE"), ",\n", sep="")
    cat("  show_grid_major_y = ", ifelse(config$show_grid_major_y, "TRUE", "FALSE"), ",\n", sep="")
    cat("  show_grid_minor_x = ", ifelse(config$show_grid_minor_x, "TRUE", "FALSE"), ",\n", sep="")
    cat("  show_grid_minor_y = ", ifelse(config$show_grid_minor_y, "TRUE", "FALSE"), ",\n", sep="")

    # Zero Line
    cat("\n  # Zero Line\n")
    cat("  show_zero_line = ", ifelse(config$show_zero_line, "TRUE", "FALSE"), ",\n", sep="")
    cat("  zero_line_type = \"", config$zero_line_type, "\",\n", sep="")
    cat("  zero_line_color = \"", config$zero_line_color, "\",\n", sep="")
    cat("  zero_line_size = ", config$zero_line_size, ",\n", sep="")
    cat("  zero_line_position = ", config$zero_line_position, ",\n", sep="")

    # Bar Chart
    cat("\n  # Bar Chart\n")
    cat("  bar_width = ", config$bar_width, ",\n", sep="")
    cat("  bar_spacing = ", config$bar_spacing, ",\n", sep="")

    # Scale Settings
    cat("\n  # Scale Settings\n")
    if (is.null(config$scale_limit)) {
      cat("  scale_limit = NULL,\n", sep="")
    } else {
      cat("  scale_limit = c(", paste(config$scale_limit, collapse=", "), "),\n", sep="")
    }
    cat("  scale_increment = ", if(is.null(config$scale_increment)) "NULL" else config$scale_increment, ",\n", sep="")

    # Scale Expansion
    cat("\n  # Scale Expansion\n")
    cat("  expansion_y_mult = c(", paste(config$expansion_y_mult, collapse=", "), "),\n", sep="")
    cat("  expansion_x_mult = c(", paste(config$expansion_x_mult, collapse=", "), "),\n", sep="")

    # Font Size Control
    cat("\n  # Font Size Control\n")
    cat("  all_font_size = ", config$all_font_size, ",\n", sep="")

    # Data Sorting
    cat("\n  # Data Sorting\n")
    cat("  sort_data_by_value = ", ifelse(config$sort_data_by_value, "TRUE", "FALSE"), ",\n", sep="")

    # Plot Margin Settings
    cat("\n  # Plot Margin\n")
    margin_values <- as.numeric(config$plot.margin)
    cat("  plot.margin = c(", margin_values[1], ", ", margin_values[2],
        ", ", margin_values[3], ", ", margin_values[4], ") #c(top, right, bottom, left)\n", sep="")

    cat(")\n\n")
    cat("# Example usage:\n")
    cat("# comparison_plot(data, x_axis_from = \"REG\", plot_style_config = my_style_config)\n")

    return(invisible(config))
  }

  return(c(list(plot_type = plot_type), config))
}

#' @title Get Export Configuration Options
#'
#' @description
#' Returns documentation and default values for export configuration options used in plotting functions.
#'
#' @param as_dataframe Logical. Whether to return settings as a dataframe. Default is FALSE.
#' @param printing Logical. If TRUE, prints a formatted code snippet that can be copied
#'   and pasted to recreate the configuration. Default is FALSE.
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
get_export_config <- function(as_dataframe = TRUE, printing = FALSE) {
  # Export config parameters with default values
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

  # Add printing functionality
  if (printing) {
    cat("my_export_config <- list(\n")

    # Print file_name
    cat("  file_name = \"", export_config_params$file_name, "\",\n", sep="")

    # Print width
    cat("  width = ", if(is.null(export_config_params$width)) "NULL" else export_config_params$width, ",\n", sep="")

    # Print height
    cat("  height = ", if(is.null(export_config_params$height)) "NULL" else export_config_params$height, ",\n", sep="")

    # Print dpi
    cat("  dpi = ", export_config_params$dpi, ",\n", sep="")

    # Print bg
    cat("  bg = \"", export_config_params$bg, "\",\n", sep="")

    # Print limitsize (last item, no comma)
    cat("  limitsize = ", ifelse(export_config_params$limitsize, "TRUE", "FALSE"), "\n", sep="")

    cat(")\n\n")
    cat("# Example usage:\n")
    cat("# comparison_plot(data, x_axis_from = \"REG\", export_config = my_export_config)\n")

    return(invisible(export_config_params))
  }

  # Create a dataframe for as_dataframe = TRUE
  if (as_dataframe) {
    # Count the total number of parameters
    params_count <- 6 # Number of export_config parameters

    # Create vectors with the exact same length
    topics <- character(params_count)
    arguments <- character(params_count)
    default_values <- character(params_count)
    input_formats <- character(params_count)
    descriptions <- character(params_count)
    examples <- character(params_count)

    # Fill in the values - all under "Export Config" topic
    topics[1:params_count] <- c("Export Config", rep("", params_count-1))

    # Parameters
    arguments[1:params_count] <- c(
      "file_name", "width", "height", "dpi", "bg", "limitsize"
    )

    # Default values
    default_values[1:params_count] <- c(
      "\"gtap_plots\"",
      "NULL",
      "NULL",
      "300",
      "\"white\"",
      "FALSE"
    )

    # Input formats
    input_formats[1:params_count] <- c(
      "character",
      "numeric or NULL",
      "numeric or NULL",
      "numeric",
      "character",
      "logical"
    )

    # Descriptions
    descriptions[1:params_count] <- c(
      "Base name for exported files. Default is 'gtap_plots'.",
      "Plot width in inches. Default is automatically calculated.",
      "Plot height in inches. Default is automatically calculated.",
      "Resolution for PNG export. Default is 300.",
      "Background color. Default is 'white'.",
      "Whether to limit size. Default is FALSE."
    )

    # Examples
    examples[1:params_count] <- c(
      "file_name = \"regional_impacts\"",
      "width = 12",
      "height = 8",
      "dpi = 600",
      "bg = \"white\"",
      "limitsize = FALSE"
    )

    # Create the result dataframe
    result <- data.frame(
      Topic = topics,
      Arguments = arguments,
      "Default Value" = default_values,
      "Input Format" = input_formats,
      Description = descriptions,
      Example = examples,
      stringsAsFactors = FALSE
    )

    # Assign to export_config in parent environment
    assign("export_config", result, envir = parent.frame())

    return(result)
  }

  # Return as a list
  return(list(
    export_config = export_config_params,
    export_config_docs = export_config_docs
  ))
}


#' @title Print and Visualize Themed Color Palettes
#'
#' @description
#' This function prints and visualizes predefined color palettes. Users can specify a `color_tone`
#' and `palette_type` to display the corresponding colors. Additionally, calling `color_tone = "all"`
#' returns a list of callable functions, where each entry dynamically generates a color palette visualization.
#'
#' @param color_tone Character. The name of the predefined color theme. Default is `NULL`, which requires specification.
#' Available themes include:
#' \itemize{
#'   \item \strong{Academic}: A balanced set of colors for research-oriented visuals.
#'   \item \strong{Purdue}: Themed after Purdue University branding.
#'   \item \strong{Colorblind}: Designed for colorblind-friendly visualization.
#'   \item \strong{Economic}: Used for economic data visualization.
#'   \item \strong{Trade}: Suited for trade-related data.
#'   \item \strong{GTAP}: Based on Global Trade Analysis Project visuals.
#'   \item \strong{GTAP2}: An alternative GTAP color set.
#'   \item \strong{Earth}: Inspired by natural, earthy tones.
#'   \item \strong{Vibrant}: High-contrast and energetic colors.
#'   \item \strong{Bright}: Bright and playful color combinations.
#'   \item \strong{Minimal}: Monochrome and muted colors for minimalistic designs.
#'   \item \strong{Energetic}: Warm and dynamic tones.
#'   \item \strong{Pastel}: Soft pastel shades.
#'   \item \strong{Spring}: Fresh, floral-inspired hues.
#'   \item \strong{Summer}: Sunny, warm color gradients.
#'   \item \strong{Winter}: Cool, icy tones for winter visuals.
#'   \item \strong{Fall}: Autumn-inspired warm color palette.
#'   \item \strong{Blue_mono}, \strong{Green_mono}, \strong{Red_mono}, \strong{Grey_mono}: Monochromatic shades for respective colors.
#' }
#'
#' Use `"all"` to return a list of callable functions for all palettes.
#'
#' @param palette_type Character. The type of color palette to use.
#' Options include:
#' \itemize{
#'   \item \strong{"qualitative"} - Best for categorical data.
#'   \item \strong{"sequential"} - Used for ordered, continuous scales.
#'   \item \strong{"diverging"} - Ideal for highlighting contrasts.
#' }
#' Default is `"qualitative"`.
#'
#' @details
#' The function retrieves colors from `.create_color_palette()` and provides a
#' visual preview of the selected theme.
#'
#' If `color_tone = "all"`, the function returns a **list of functions**,
#' where calling any element (e.g., `all_palettes$winter()`) generates the corresponding color palette visualization.
#'
#' If the requested `color_tone` does not exist or is empty, the function throws an error.
#'
#' @return
#' \itemize{
#'   \item If a specific `color_tone` is provided, it prints the color palette and returns `NULL`.
#'   \item If `color_tone = "all"`, it returns a **list of callable functions** to visualize each palette on demand.
#' }
#'
#' @examples
#' \dontrun{
#' # Print & visualize a specific palette
#' get_color_palette("winter")
#'
#' # Print & visualize another palette with different types
#' get_color_palette("fall", "sequential")
#' get_color_palette("academic", "diverging")
#'
#' # Get all palettes as a list of callable functions
#' all_palettes <- get_color_palette("all")
#'
#' # Click or call a specific palette function to view its colors
#' all_palettes$winter()   # View the winter palette
#' all_palettes$fall()     # View the fall palette
#' all_palettes$gtap()     # View the GTAP palette
#' }
#'
#' @export
#'
get_color_palette <- function(color_tone = NULL, palette_type = "qualitative") {
  # Define available themes
  available_palettes <- c(
    "academic", "purdue", "colorblind", "economic", "trade", "gtap", "gtap2",
    "earth", "vibrant", "bright", "minimal", "energetic", "pastel", "spring",
    "summer", "winter", "fall", "blue_mono", "green_mono", "red_mono", "grey_mono"
  )
  # If color_tone is "all", return a list of functions (lazy evaluation)
  if (!is.null(color_tone) && color_tone == "all") {
    plot_list <- list()
    for (palette in available_palettes) {
      plot_list[[palette]] <- local({
        pal <- palette  # Store the palette name (to prevent overwriting issues)
        function() { get_color_palette(pal, palette_type) }
      })
    }
    return(plot_list)  # Returns a list of callable functions
  }
  # Generate the color palette using the existing function
  colors <- .create_color_palette(color_tone = color_tone, n_colors = 10, palette_type = palette_type)
  # Validate output
  if (is.null(colors) || length(colors) == 0) {
    stop("Invalid color tone or empty palette. Please choose a valid color_tone from .create_color_palette().")
  }
  # Print colors in console
  cat("\nPalette:", color_tone, "-", palette_type, "\n")
  cat(" Colors: ", paste(colors, collapse = ", "), "\n")
  # Base R visualization
  n_colors <- length(colors)
  bar_x <- seq_len(n_colors)
  bar_y <- rep(1, n_colors)
  graphics::par(mar = c(2, 2, 2, 2))  # Adjust margins for visualization
  graphics::plot(bar_x, bar_y, type = "n", xlab = "", ylab = "", axes = FALSE,
                 main = paste("Palette:", color_tone, "-", palette_type))
  graphics::rect(bar_x - 0.5, 0, bar_x + 0.5, 1, col = colors, border = "black")
  # Add labels
  graphics::text(bar_x, rep(-0.2, n_colors), labels = seq_along(colors), cex = 0.8, col = "black")
}

# PLOT STYLE CONFIG HELPERS -----------------------------------------

#' @title Calculate Plot Style Configuration
#'
#' @description Merges user-defined plot style configurations with defaults for different plot types.
#' This function is internal and used by plotting functions to define visual styling.
#'
#' @param config Optional list with custom style configuration parameters.
#' @param plot_type Type of plot: `"default"`.
#'
#' @return A list with complete style configuration for the specified plot type.
#' @importFrom utils modifyList
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_style_config <- function(config = NULL, plot_type = "default") {
  # Default configurations specific to each plot type
  style_default <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 20,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = c(10, 0, 10, 0),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 16,
    x_axis_title_margin = c(25, 25, 0, 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "plain",
    x_axis_text_size = 14,
    x_axis_text_angle = 0,
    x_axis_text_hjust = 0,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 16,
    y_axis_title_margin = c(25, 25, 0, 0),
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
    strip_text_margin = c(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 2,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    color_palette_type = "qualitative",
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
    bar_width = 0.9,
    bar_spacing = 0.9,

    # Scale settings
    scale_limit = NULL,
    scale_increment = NULL,

    # Scale expansion settings
    expansion_y_mult = c(0.05, 0.1),
    expansion_x_mult = c(0.05, 0.05),

    # Font size settings
    all_font_size = 1,

    # Sorting Data
    sort_data_by_value = FALSE,

    # Plot Margin
    plot.margin = c(10, 25, 10, 10)
  )

  # Select the appropriate default based on plot type
  default_config <- switch(plot_type,
                           "default" = style_default,
                           style_default)

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
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.apply_plot_style_config <- function(p, config) {
  # Helper function to convert numeric vector to margin object
  vector_to_margin <- function(vec) {
    if (is.numeric(vec) && length(vec) == 4) {
      return(ggplot2::margin(t = vec[1], r = vec[2], b = vec[3], l = vec[4]))
    }
    return(vec)
  }

  # Convert numeric margin vectors to ggplot2 margin objects
  title_margin <- vector_to_margin(config$title_margin)
  x_title_margin <- vector_to_margin(config$x_axis_title_margin)
  y_title_margin <- vector_to_margin(config$y_axis_title_margin)
  strip_margin <- vector_to_margin(config$strip_text_margin)
  plot_margin <- vector_to_margin(config$plot.margin)

  # Apply theme modifications
  p <- p + ggplot2::theme(
    # Title settings
    plot.title = if (config$show_title) {
      ggplot2::element_text(
        hjust = config$title_hjust,
        size = config$title_size,
        face = config$title_face,
        margin = title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # X axis title settings
    axis.title.x = if (config$show_x_axis_title) {
      ggplot2::element_text(
        size = config$x_axis_title_size,
        face = config$x_axis_title_face,
        margin = x_title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # Y axis title settings
    axis.title.y = if (config$show_y_axis_title) {
      ggplot2::element_text(
        size = config$y_axis_title_size,
        face = config$y_axis_title_face,
        margin = y_title_margin
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
      margin = strip_margin
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

  # Explicitly apply plot margin as a separate theme element to ensure it's not overridden
  p <- p + ggplot2::theme(plot.margin = plot_margin)

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
      linewidth = config$zero_line_size
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
#' @noRd
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


#' @title Handle Plot Title and Export Naming
#'
#' @description
#' Generates a formatted plot title and an export-friendly filename based on the provided parameters.
#' The function adjusts titles dynamically based on various conditions, such as macro mode, variable duplication,
#' title formatting options, and unit inclusion.
#'
#' @param var_name Character or NULL. The name of the variable being plotted.
#' @param sep_value Character or NULL. Separator value to combine with `var_name` if applicable.
#' @param x_value Character or NULL. X-axis value for additional context.
#' @param plot_type Character or NULL. The type of plot, one of "comparison", "detail", "stack", or "unstack".
#' @param is_macro_mode Logical. If `TRUE`, uses a simplified macro-style title.
#' @param split_by Character or NULL. Column used for splitting data; checked for duplication with `variable_col`.
#' @param x_axis_from Character or NULL. Source column for the X-axis.
#' @param variable_col Character or NULL. Column representing the variable in the dataset.
#' @param unit_name Character or NULL. Unit name to be appended to the title if applicable.
#' @param style_config List or NULL. Formatting options for the title, containing:
#'   \itemize{
#'     \item \code{title_format}: List specifying format type ("prefix", "suffix", "full", or "dynamic").
#'     \item \code{add_unit_to_title}: Logical indicating whether to append the unit name.
#'   }
#' @param data Data frame or NULL. Used for extracting values in dynamic title generation.
#'
#' @return A named list containing:
#'   \itemize{
#'     \item \code{title}: The final formatted plot title.
#'     \item \code{export_name}: A cleaned, export-friendly version of the title.
#'   }
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.handle_plot_title_and_export <- function(
    var_name = NULL,
    sep_value = NULL,
    x_value = NULL,
    plot_type = NULL,
    is_macro_mode = FALSE,
    split_by = NULL,
    x_axis_from = NULL,
    variable_col = NULL,
    unit_name = NULL,
    style_config = NULL,
    data = NULL,
    separate_figure = FALSE,
    panel_val = NULL) {

  # Generate basic title without panel value
  if (is_macro_mode) {
    plot_title <- .coalesce(
      var_name,
      .coalesce(
        if (!is.null(data) && variable_col %in% names(data))
          unique(data[[variable_col]])[1],
        "Global Economic Impacts"
      )
    )
  } else {
    plot_title <- if (!is.null(sep_value) && !is.null(var_name)) {
      paste0(sep_value, " - ", var_name)
    } else if (!is.null(sep_value)) {
      sep_value
    } else if (!is.null(var_name)) {
      var_name
    } else {
      "GTAP Analysis"
    }
  }

  dynamic_title_has_unit <- FALSE

  if (!is.null(style_config$title_format)) {
    title_format <- style_config$title_format

    if (title_format$type == "dynamic") {
      if (!is.null(data) && !is.null(title_format$text) && nrow(data) > 0) {
        if (!requireNamespace("glue", quietly = TRUE)) {
          warning("The 'glue' package is required for dynamic titles but is not installed. Using standard title format.")
        } else {
          referenced_cols <- regmatches(
            title_format$text,
            gregexpr("\\{([^}]+)\\}", title_format$text)
          )

          if (length(referenced_cols) > 0 && length(referenced_cols[[1]]) > 0) {
            referenced_cols <- gsub("\\{|\\}", "", referenced_cols[[1]])

            # Check if "Unit" is included in the dynamic template
            if (any(referenced_cols %in% c("Unit", "unit", "UNIT"))) {
              dynamic_title_has_unit <- TRUE
            }

            missing_cols <- setdiff(referenced_cols, names(data))

            if (length(missing_cols) > 0) {
              warning(paste("Columns referenced in dynamic title template but not found in data:",
                            paste(missing_cols, collapse=", ")))
            } else {
              plot_title <- glue::glue_data(data[1, ], title_format$text)
            }
          } else {
            plot_title <- title_format$text
          }
        }
      }
    }
    else if (title_format$type == "full") {
      plot_title <- title_format$text
    }
    else if (title_format$type == "prefix") {
      separator <- if (!is.null(title_format$sep)) title_format$sep else " "
      plot_title <- paste0(title_format$text, separator, plot_title)
    }
    else if (title_format$type == "suffix") {
      separator <- if (!is.null(title_format$sep)) title_format$sep else " "
      plot_title <- paste0(plot_title, separator, title_format$text)
    }
  }

  # Add unit to title if appropriate
  if ((title_format$type != "dynamic" ||
       (title_format$type == "dynamic" && !dynamic_title_has_unit)) &&
      style_config$add_unit_to_title && !is.null(unit_name)) {
    if (tolower(unit_name) == "percent") {
      plot_title <- paste0(plot_title, " (%)")
    } else {
      plot_title <- paste0(plot_title, " (", unit_name, ")")
    }
  }

  # Store the base title before adding panel value
  base_title <- plot_title

  # Add panel value to plot title if needed
  if (separate_figure && !is.null(panel_val)) {
    plot_title <- paste0(base_title, " - ", panel_val)
  }

  # Create initial export name from base title (without panel value)
  export_name <- base_title

  # Process parentheses in export name
  parentheses_content <- list()
  parentheses_pattern <- "\\(([^()]*)\\)"
  matches <- gregexpr(parentheses_pattern, export_name)
  match_list <- regmatches(export_name, matches)

  if (length(match_list) > 0 && length(match_list[[1]]) > 0) {
    for (i in seq_along(match_list[[1]])) {
      placeholder <- paste0("__PLACEHOLDER_", i, "__")
      parentheses_content[[placeholder]] <- match_list[[1]][i]
      export_name <- sub(match_list[[1]][i], placeholder, export_name, fixed = TRUE)
    }
  }

  # Clean export name
  export_name <- gsub("[^a-zA-Z0-9\\s_]", " ", export_name)

  # Restore parentheses
  for (placeholder in names(parentheses_content)) {
    export_name <- sub(placeholder, parentheses_content[[placeholder]], export_name, fixed = TRUE)
  }

  export_name <- gsub("\\s+", " ", export_name)
  export_name <- trimws(export_name)

  # Add panel value as parenthetical suffix
  if (separate_figure && !is.null(panel_val)) {
    clean_panel_val <- gsub("[^a-zA-Z0-9\\s]", " ", panel_val)
    clean_panel_val <- gsub("\\s+", " ", clean_panel_val)
    clean_panel_val <- trimws(clean_panel_val)

    # Add panel value in parentheses to export name
    export_name <- paste0(export_name, " (", clean_panel_val, ")")
  }

  # Add plot type suffix if needed
  if (!is.null(plot_type)) {
    if (plot_type == "stack") {
      export_name <- paste(export_name, "stack")
    } else if (plot_type == "unstack") {
      export_name <- paste(export_name, "unstack")
    }
  }

  return(list(
    title = plot_title,
    export_name = export_name
  ))
}

#' Generate a Custom Title Column Using a Glue Template (Internal)
#'
#' @description
#' Creates a new column in the data frame based on a user-defined glue-style template.
#' This allows dynamic construction of titles using column values.
#'
#' @param df A data frame.
#' @param template A string template using `{}` to refer to column names (e.g., "Impact on {Variable} in {Region} ({Unit})").
#' @param new_col Character. Name of the new column to be created. Default is "Title".
#'
#' @return The original data frame with an added column based on the template.
#'
#' @importFrom glue glue_data
#'
#' @keywords internal
#' @noRd
.generate_custom_title_column <- function(df, template, new_col = "Title") {
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("The 'glue' package is required but not installed. Please install it with install.packages('glue').")
  }

  df[[new_col]] <- glue::glue_data(df, template)
  return(df)
}

#' @title Prepare Data Source for Plotting
#'
#' @description
#' Validates and extracts a suitable data frame from the provided input. Ensures that required
#' columns exist before returning the data. Supports both single data frames and lists of data
#' frames.
#'
#' @param data A data frame or a named list of data frames.
#' @param x_axis_from Character. The column name to be used as the x-axis in plotting.
#' @param stack_value_from Character or NULL. Optional column name for stacked values
#'   (used in stack plots).
#' @param variable_col Character or NULL. Optional column name representing variable names.
#'
#' @return A validated data frame containing the required columns.
#'
#' @details
#' If `data` is a single data frame, the function checks whether the required columns exist.
#' If `data` is a list of data frames, it attempts to find the first data frame containing the
#' required columns. If no suitable data frame is found, the function stops with an error.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}},
#'   \code{\link{stack_plot}}
#'
.prepare_data_source <- function(data, x_axis_from,
                                 stack_value_from = NULL,
                                 variable_col = NULL) {
  # If already a data frame, validate columns
  if (is.data.frame(data)) {
    # Check x_axis_from column
    if (!(x_axis_from %in% names(data))) {
      stop(paste("Required column", x_axis_from, "not found in the data frame."))
    }

    # Check stack_value_from if provided (for stack_plot)
    if (!is.null(stack_value_from) && !(stack_value_from %in% names(data))) {
      stop(paste("Required column", stack_value_from, "not found in the data frame."))
    }

    # Check variable_col if provided
    if (!is.null(variable_col) && !(variable_col %in% names(data))) {
      stop(paste("Required column", variable_col, "not found in the data frame."))
    }

    return(data)
  }

  # If a list of data frames, find first matching data frame
  if (is.list(data)) {
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df)) {
        # Check x_axis_from column
        if (x_axis_from %in% names(df)) {
          # Check stack_value_from if provided
          if (!is.null(stack_value_from) && !(stack_value_from %in% names(df))) {
            next
          }

          # Check variable_col if provided
          if (!is.null(variable_col) && !(variable_col %in% names(df))) {
            next
          }

          return(df)
        }
      }
    }

    # If no suitable data frame found
    stop(paste("No suitable data frame found with required column:", x_axis_from))
  }

  stop("Input must be a data frame or a list of data frames.")
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
    ),
    gtap = list(
      qualitative = c("#003366", "#0055A4", "#009CDE", "#F2A900", "#666666", "#000000"),
      sequential = c("#FFFFFF", "#D6E6F7", "#A0C4E8", "#6AA2D8", "#347FC8", "#0055A4", "#003F88", "#002A66", "#000000"),
      diverging = c("#000000", "#003366", "#0055A4", "#347FC8", "#009CDE", "#A0C4E8", "#D6E6F7", "#F2A900", "#FFFFFF")
    ),
    gtap2 = list(
      qualitative = c("#002F5F", "#0072C6", "#00A3E0", "#F2A900", "#C28E0E", "#666666", "#4D4038"),
      sequential = c("#FFFFFF", "#E1EFF7", "#B3DFF0", "#7FC6E0", "#009CDE", "#DAAA00", "#C28E0E", "#98700D", "#000000"),
      diverging = c("#000000", "#4D4038", "#666666", "#0055A4", "#009CDE", "#00A3E0", "#DAAA00", "#E2D48E", "#FFFFFF")
    ),
    earth = list(
      qualitative = c("#8B4513", "#A0522D", "#CD853F", "#D2B48C", "#8FBC8F", "#556B2F", "#2E8B57"),
      sequential = c("#F5F5DC", "#E3DAC9", "#C4A484", "#A67B5B", "#806040", "#594028", "#3B2F2F"),
      diverging = c("#8B0000", "#B22222", "#CD5C5C", "#D3D3D3", "#4682B4", "#1E90FF", "#00008B")
    ),
    vibrant = list(
      qualitative = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#9400D3"),
      sequential = c("#FFE5B4", "#FFCC66", "#FF9933", "#FF6600", "#FF3300", "#CC0000", "#990000"),
      diverging = c("#800000", "#FF4500", "#FFD700", "#FFFFFF", "#32CD32", "#008080", "#000080")
    ),
    bright = list(
      qualitative = c("#FF69B4", "#FF4500", "#FFD700", "#32CD32", "#00FFFF", "#1E90FF", "#8A2BE2"),
      sequential = c("#FFFACD", "#FFD700", "#FFA500", "#FF4500", "#DC143C", "#8B0000", "#4B0082"),
      diverging = c("#D2691E", "#FFA07A", "#FFDEAD", "#FFFFFF", "#ADD8E6", "#4682B4", "#00008B")
    ),
    minimal = list(
      qualitative = c("#222222", "#444444", "#666666", "#888888", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
      sequential = c("#F8F9FA", "#E9ECEF", "#DEE2E6", "#CED4DA", "#ADB5BD", "#6C757D", "#343A40"),
      diverging = c("#5A5A5A", "#878787", "#B4B4B4", "#FFFFFF", "#CCCCCC", "#888888", "#444444")
    ),
    energetic = list(
      qualitative = c("#FF0000", "#FFAA00", "#FFFF00", "#00FF00", "#00AAAA", "#0000FF", "#5500AA"),
      sequential = c("#FFF5E6", "#FFDAB9", "#FFB07F", "#FF7F50", "#FF4500", "#DC143C", "#8B0000"),
      diverging = c("#8B0000", "#FF4500", "#FFD700", "#FFFFFF", "#00FFFF", "#0000FF", "#4B0082")
    ),
    pastel = list(
      qualitative = c("#FFB6C1", "#FFDAC1", "#FAFAD2", "#C1E1C1", "#B0E0E6", "#DDA0DD", "#E6E6FA"),
      sequential = c("#FFF5EE", "#FFE4E1", "#FFC0CB", "#FFB6C1", "#DB7093", "#C71585", "#800080"),
      diverging = c("#CD5C5C", "#FFA07A", "#FFDAB9", "#FFFFFF", "#ADD8E6", "#4682B4", "#00008B")
    ),
    spring = list(
      qualitative = c("#FF69B4", "#FFB6C1", "#FFD700", "#32CD32", "#87CEEB", "#9370DB", "#8B008B"),
      sequential = c("#FFF0F5", "#FFDAB9", "#FFC0CB", "#FFB6C1", "#FF69B4", "#DB7093", "#8B008B"),
      diverging = c("#FF4500", "#FFD700", "#FFFACD", "#FFFFFF", "#00FF7F", "#20B2AA", "#008080")
    ),
    summer = list(
      qualitative = c("#FF4500", "#FFA500", "#FFD700", "#00FF00", "#00CED1", "#1E90FF", "#8A2BE2"),
      sequential = c("#FFEBCD", "#FFD700", "#FFA500", "#FF4500", "#DC143C", "#8B0000", "#4B0082"),
      diverging = c("#FF4500", "#FFD700", "#FFFACD", "#FFFFFF", "#00CED1", "#1E90FF", "#00008B")
    ),
    winter = list(
      qualitative = c("#00FFFF", "#4682B4", "#87CEEB", "#5F9EA0", "#B0E0E6", "#ADD8E6", "#E0FFFF"),
      sequential = c("#FFFFFF", "#E0FFFF", "#B0E0E6", "#87CEEB", "#4682B4", "#4169E1", "#00008B"),
      diverging = c("#00008B", "#1E90FF", "#87CEEB", "#FFFFFF", "#FFDAB9", "#FF6347", "#8B0000")
    ),
    fall = list(
      qualitative = c("#FF4500", "#D2691E", "#8B4513", "#A0522D", "#CD853F", "#F4A460", "#FFD700"),
      sequential = c("#FFE4B5", "#FFD700", "#FFA500", "#FF8C00", "#D2691E", "#A0522D", "#8B0000"),
      diverging = c("#8B0000", "#D2691E", "#FFA07A", "#FFFFFF", "#87CEFA", "#4682B4", "#00008B")
    ),
  )

  # Check if color_tone is a recognized theme
  if (!is.null(color_tone) && tolower(color_tone) %in% names(themed_palettes)) {
    palette <- themed_palettes[[tolower(color_tone)]][[palette_type]]

    # Ensure we have the right number of colors
    if (length(palette) < n_colors) {
      palette <- grDevices::colorRampPalette(palette)(n_colors)
    } else if (length(palette) > n_colors) {
      palette <- palette[1:n_colors]
    }

    return(palette)
  }

  # For mono-color themes that aren't predefined
  if (grepl("_mono$", color_tone)) {
    # Extract base color name (remove _mono suffix)
    base_color <- sub("_mono$", "", color_tone)

    # Get the representative color
    color_hex <- NULL

    # If base_color is a named color like "blue", "red", etc.
    if (base_color %in% colors()) {
      color_hex <- base_color
    }
    # If base_color is a hex code
    else if (grepl("^#[0-9A-Fa-f]{6}$", base_color)) {
      color_hex <- base_color
    }
    # If base_color is one of our predefined palettes, use its first color
    else if (base_color %in% names(color_palette)) {
      color_hex <- color_palette[[base_color]]$qualitative[1]
    }
    # Default fallback
    else {
      color_hex <- "#000000"  # Default to black if color not recognized
    }

    # Create monochrome palette with the single color
    mono_palette <- list(
      qualitative = rep(color_hex, n_colors),
      sequential = rep(color_hex, n_colors),
      diverging = rep(color_hex, n_colors)
    )

    return(mono_palette)
  }

  # NEW: Try to interpret any standard R color
  if (!is.null(color_tone)) {
    tryCatch({
      # Try to validate if it's a valid R color
      base_col <- grDevices::col2rgb(color_tone)

      # If we get here, it's a valid color - create a palette of shades
      darken_factor <- if (palette_type == "diverging") {
        seq(0.4, 1.3, length.out = n_colors)
      } else {
        seq(0.5, 1.5, length.out = n_colors)
      }

      # Create different shades based on the base color
      colors <- sapply(darken_factor, function(factor) {
        r <- min(255, max(0, base_col[1,1] * factor))
        g <- min(255, max(0, base_col[2,1] * factor))
        b <- min(255, max(0, base_col[3,1] * factor))
        grDevices::rgb(r, g, b, maxColorValue = 255)
      })

      return(colors)
    }, error = function(e) {
      # Color wasn't valid, return NULL
      return(NULL)
    })
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
#' @param palette_type Character. Optional color palette by default is qualitative.
#'
#' @return A vector of colors in hexadecimal format, or NULL if color_tone is NULL.
#'
#' @importFrom colorspace hex2RGB polarLUV
#' @importFrom grDevices col2rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{comparison_plot}}
#'
.generate_comparison_colors <- function(data, color_tone = NULL, axis_col, palette_type = "qualitative") {
  if(is.null(color_tone)) return(NULL)

  n_colors <- length(unique(data[[axis_col]]))

  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_colors,
                                          palette_type = palette_type)

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
#' @param color_tone Character. Optional color tone to override the positive/negative colors.
#' @param palette_type Character. Optional color palette by default is qualitative.
#'
#' @return A named vector containing hex codes for different value categories.
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{detail_plot}}
#'
.generate_color_palette <- function(positive_color = "#2E8B57", negative_color = "#CD5C5C",
                                    color_tone = NULL, palette_type = "qualitative") {
  # If color_tone is specified, we use it to generate colors instead of positive/negative colors
  if (!is.null(color_tone)) {
    mono_palette <- .create_color_palette(color_tone = color_tone, n_colors = 5,
                                          palette_type = palette_type)

    if (!is.null(mono_palette)) {
      if (length(mono_palette) >= 5) {
        # If we have at least 5 colors, use them directly
        return(c(
          "extreme_positive" = mono_palette[1],
          "normal_positive" = mono_palette[2],
          "extreme_negative" = mono_palette[3],
          "normal_negative" = mono_palette[4],
          "neutral" = mono_palette[5]
        ))
      } else if (length(mono_palette) == 1) {
        # For monochromatic palettes with a single color, use different lightness levels
        base_rgb <- col2rgb(mono_palette[1])

        # Calculate lighter and darker variants
        lighter <- function(rgb_val, factor = 0.3) {
          pmax(0, pmin(255, rgb_val + (255 - rgb_val) * factor))
        }

        darker <- function(rgb_val, factor = 0.3) {
          pmax(0, pmin(255, rgb_val * (1 - factor)))
        }

        # Create variants with different lightness levels
        lighter1 <- rgb(lighter(base_rgb[1], 0.3), lighter(base_rgb[2], 0.3), lighter(base_rgb[3], 0.3), maxColorValue = 255)
        lighter2 <- rgb(lighter(base_rgb[1], 0.6), lighter(base_rgb[2], 0.6), lighter(base_rgb[3], 0.6), maxColorValue = 255)
        darker1 <- rgb(darker(base_rgb[1], 0.3), darker(base_rgb[2], 0.3), darker(base_rgb[3], 0.3), maxColorValue = 255)
        darker2 <- rgb(darker(base_rgb[1], 0.6), darker(base_rgb[2], 0.6), darker(base_rgb[3], 0.6), maxColorValue = 255)

        return(c(
          "extreme_positive" = darker1,
          "normal_positive" = mono_palette[1],
          "extreme_negative" = darker2,
          "normal_negative" = lighter1,
          "neutral" = lighter2
        ))
      }
    }
  }

  # If we don't have a color_tone or couldn't generate a palette from it,
  # fall back to the traditional positive/negative colors
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
#' @param palette_type Character. Optional color palette by default is qualitative.
#'
#' @return A named vector of colors for each stack component.
#' @importFrom colorspace hex2RGB hex polarLUV
#' @importFrom grDevices hcl
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{stack_plot}}
#'
.generate_stack_colors <- function(data, stack_value_from, color_tone = NULL, palette_type = "qualitative") {
  components <- unique(data[[stack_value_from]])
  n_components <- length(components)

  if (n_components <= 1) {
    return(setNames(c("#4477AA"), components))
  }

  # Try to generate a diverse palette based on the provided color_tone
  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_components,
                                          palette_type = palette_type)

  if (!is.null(themed_palette)) {
    # Modify colors to increase distinction in the stack plot
    adjusted_colors <- colorspace::lighten(themed_palette, amount = seq(0.1, 0.5, length.out = n_components))
    return(setNames(adjusted_colors, components))
  }

  # If color_tone is a standard color, generate variations with more differentiation
  if (!is.null(color_tone)) {
    tryCatch({
      base_col <- grDevices::col2rgb(color_tone) / 255  # Normalize to 0-1
      hue_shifts <- seq(0, 360, length.out = n_components + 1)[-1]  # Rotate hues
      saturation_shifts <- seq(0.6, 1, length.out = n_components)  # Vary saturation

      colors <- sapply(seq_len(n_components), function(i) {
        hcl_col <- colorspace::HLS(base_col[1,1] * 360, base_col[2,1], base_col[3,1])
        grDevices::hcl(hue = (hcl_col@coords[1] + hue_shifts[i]) %% 360,
                       chroma = saturation_shifts[i] * 100,
                       luminance = hcl_col@coords[3] * 100)
      })

      return(setNames(colors, components))
    }, error = function(e) {
      # Fallback in case of any issues
    })
  }

  # Default High-Contrast Palette for Stack Plot
  default_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

  if (n_components > length(default_palette)) {
    default_palette <- colorspace::rainbow_hcl(n_components, c = 100, l = 65)
  }

  return(setNames(default_palette[1:n_components], components))
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
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL,
                                    panel_var = "Experiment") {
  # DETERMINE NUMBER OF PANELS
  num_panels <- length(unique(data[[panel_var]]))

  # CASE 1: Only panel_rows is specified (panel_cols is NULL)
  if (!is.null(panel_rows) && is.null(panel_cols)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  }
  # CASE 2: Only panel_cols is specified (panel_rows is NULL)
  else if (is.null(panel_rows) && !is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  }
  # CASE 3: Both panel_rows and panel_cols are specified (neither is NULL)
  else if (!is.null(panel_rows) && !is.null(panel_cols)) {
    # Check if there are enough panels and adjust if needed
    if (panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
      # Preferentially adjust columns to fit all panels
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  }

  # CASE 4: AUTO CALCULATE LAYOUT WHEN NEITHER DIMENSION IS SPECIFIED
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
    # For larger numbers, try to find a balanced layout
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
      # If not divisible evenly, use a layout that can fit all panels
      cols <- ceiling(sqrt(num_panels))
      rows <- ceiling(num_panels / cols)
    }

    # Ensure layout is not too wide compared to height
    if (cols > 2 * rows) {
      new_cols <- ceiling(sqrt(num_panels))
      new_rows <- ceiling(num_panels / new_rows)
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
#' @noRd
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_dimensions <- function(data, panel_layout) {
  num_panels <- panel_layout$rows * panel_layout$cols
  base_width <- 20
  base_height <- 12

  width <- if(num_panels <= 4) {
    base_width
  } else {
    min(base_width + (num_panels - 4) * 3.5, 50)
  }

  height <- base_height * 0.75

  return(list(width = width, height = height))
}


#  EXPORT -----------------------------------------------------------------

#' @title Display Plot Export Dimensions
#'
#' @description
#' Displays information about plot dimensions (width and height) during the export process.
#'
#' @param dimensions A list containing 'width' and 'height' values.
#' @param plots A ggplot object or list of ggplot objects.
#' @param phase Character. The phase of the export process ("start" or "end").
#' @param dpi Numeric. DPI value for the export.
#'
#' @return Invisibly returns NULL.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
#' @seealso \code{\link{.export_plot_output}}
#'
.display_export_dimensions <- function(dimensions, plots, phase = "start", dpi = 300) {
  if (!is.list(dimensions) || is.null(dimensions$width) || is.null(dimensions$height)) {
    return(invisible(NULL))
  }

  num_plots <- if (inherits(plots, "gg")) 1 else length(plots)

  if (phase == "start") {
    message(sprintf(">>> Starting plot export process: %d plot(s) with dimensions (widthxheight): %.1f x %.1f inches",
                    num_plots, dimensions$width, dimensions$height))
    message(sprintf(">>> DPI: %d", dpi))
  } else if (phase == "end") {
    message(sprintf(">>> Completed plot export: %d plot(s) exported with dimensions (widthxheight): %.1f x %.1f inches",
                    num_plots, dimensions$width, dimensions$height))
    message(sprintf(">>> DPI: %d", dpi))
  }

  invisible(NULL)
}

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
#' @noRd
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

  # Display dimensions at the start of export process
  .display_export_dimensions(list(width = export_config$width, height = export_config$height), plots, "start", export_config$dpi)

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

      # Create base filename with plot type and number of plots
      pdf_file_name <- paste0(pdf_base_name, "_", n_plots)

      # Find an available filename by adding suffix numbers if needed
      pdf_path <- file.path(output_path, paste0(pdf_file_name, ".pdf"))
      suffix_counter <- 1

      while (file.exists(pdf_path)) {
        pdf_path <- file.path(output_path, paste0(pdf_file_name, "_", suffix_counter, ".pdf"))
        suffix_counter <- suffix_counter + 1
      }

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
        plot_name <- names(plots)[[1]]
        # Use the plot name directly - preserve spaces
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
      } else {
        # Multiple plots: use individual plot names
        for (i in seq_along(plots)) {
          p <- plots[[i]]
          plot_name <- names(plots)[[i]]
          # Use the plot name directly - preserve spaces
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
      # Use the plot name directly - preserve spaces
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
        # Use the plot name directly - preserve spaces
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

  # Display dimensions at the end of export process
  .display_export_dimensions(list(width = export_config$width, height = export_config$height), plots, "end", export_config$dpi)

  # Always return NULL invisibly to suppress output
  return(invisible(NULL))
}
