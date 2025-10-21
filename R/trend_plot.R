# TREND PLOT MAIN FUNCTION -----------------------------------------------

#' @title Create Time Trend Line Plots from GTAP Data
#' @md
#' @description
#' Generates time trend line plots for GTAP data, showing changes over time periods.
#' Supports panel facets, grouping by multiple variables, and customizable line styling.
#'
#' **Input Data**
#' @param data A data frame or list of data frames containing GTAP results with time series data.
#' @param filter_var NULL, a vector, a data frame, or a named list specifying filtering conditions.
#' For example: \code{list(Variable = c("EV", "qgdp"), REG = c("USA", "THA"))}.
#' @param period_col Character. Column name for time periods (x-axis). Default is `"Period"`.
#' @param split_by Character or vector. Column(s) used to create separate plots.
#' If `NULL`, a single aggregated plot is generated. Example: `"Variable"` or `c("Variable", "AREG")`.
#' @param line_group_by Character or vector. Column(s) that define line grouping and coloring.
#' For example, `"Case"` will create one line per case. Default is `"Case"`.
#' @param panel_var Character. Column for panel facets. Default is `NULL` (no faceting).
#' @param variable_col Character. Column name for variable codes. Default is `"Variable"`.
#' @param unit_col Character. Column name for units. Default is `"Unit"`.
#' @param desc_col Character. Column name for variable descriptions. Default is `"Description"`.
#'
#' **Plot Behavior**
#' @param separate_figure Logical. If `TRUE`, generates a separate plot for each value in `panel_var`. Default is `FALSE`.
#'
#' **Variable Display**
#' @param var_name_by_description Logical. If `TRUE`, uses descriptions instead of variable codes in titles. Default is `FALSE`.
#' @param add_var_info Logical. If `TRUE`, appends variable codes in parentheses after the description. Default is `FALSE`.
#'
#' **Export Settings**
#' @param output_path Character. Directory to save plots. If `NULL`, plots are returned but not saved.
#' @param export_picture Logical. If `TRUE`, exports plots as PNG images. Default is `TRUE`.
#' @param export_as_pdf Logical or `"merged"`.
#' - `FALSE` (default): disables PDF export.
#' - `TRUE`: exports each plot as a separate PDF file.
#' - `"merged"`: combines all plots into a single PDF file.
#' @param export_config List. Export options including dimensions, DPI, and background.
#' See \code{\link{create_export_config}} or \code{\link{get_all_config}}.
#'
#' **Styling**
#' @param trend_style_config List. Custom plot appearance settings for trend plots.
#' See \code{\link{create_trend_style}} for available options.
#'
#' @return A ggplot object or a named list of ggplot objects depending on input settings.
#' If `export_picture` or `export_as_pdf` is enabled, plots are also saved to `output_path`.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic trend plot with default settings
#' trend_plot(data, period_col = "Period", line_group_by = "Case", split_by = c("Variable", "AREG"))
#'
#' # Customize with trend styling
#' my_trend_style <- create_trend_style(
#'   show_points = TRUE,
#'   line_size = 1.5,
#'   smooth_line = FALSE
#' )
#' trend_plot(data, trend_style_config = my_trend_style)
#' }
trend_plot <- function(data,
                       filter_var = NULL,
                       period_col = "Period",
                       split_by = NULL,
                       line_group_by = "Case",
                       panel_var = NULL,
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       separate_figure = FALSE,
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       output_path = NULL,
                       export_picture = TRUE,
                       export_as_pdf = FALSE,
                       export_config = NULL,
                       trend_style_config = NULL) {

  # Validate inputs
  if (!is.data.frame(data) && !is.list(data)) {
    stop("'data' must be a data frame or a list of data frames.")
  }

  # Convert single data frame to list for uniform processing
  if (is.data.frame(data)) {
    data <- list(data)
  }

  # Validate period column exists in data
  if (!period_col %in% colnames(data[[1]])) {
    stop(paste0("Period column '", period_col, "' not found in data."))
  }

  # Validate line_group_by column(s) exist
  if (!all(line_group_by %in% colnames(data[[1]]))) {
    stop(paste0("Line grouping column(s) '", paste(line_group_by, collapse = ", "), "' not found in data."))
  }

  # Get style configuration
  style_config <- if (!is.null(trend_style_config)) {
    .calculate_trend_style_config(trend_style_config)
  } else {
    .calculate_trend_style_config(NULL)
  }

  # Get export configuration
  export_config <- if (!is.null(export_config)) {
    export_config
  } else {
    create_export_config()
  }

  # Create plot list
  plot_list <- .create_trend_plots(
    data = data,
    filter_var = filter_var,
    period_col = period_col,
    split_by = split_by,
    line_group_by = line_group_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    separate_figure = separate_figure,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info,
    style_config = style_config
  )

  # Export plots if requested
  if (!is.null(output_path) && (export_picture || export_as_pdf)) {
    # Set default dimensions for trend plots if not specified
    if (is.null(export_config$width)) {
      export_config$width <- 16
    }
    if (is.null(export_config$height)) {
      export_config$height <- 10
    }

    .export_plot_output(
      plots = plot_list,
      export_picture = export_picture,
      export_as_pdf = export_as_pdf,
      output_path = output_path,
      export_config = export_config,
      data = NULL,
      panel_layout = list(rows = 1, cols = 1),  # Simple layout for trend plots
      default_filename = "trend_plot"
    )
  }

  return(invisible(plot_list))
}


# TREND PLOT STYLE CONFIGURATION ------------------------------------------

#' @title Create Trend Plot Style Configuration
#' @md
#' @description
#' Creates a configuration list for controlling trend plot appearance and behavior.
#' Extends the base plot style with line-specific options.
#'
#' @param show_points Logical. Whether to show points at each period. Default: TRUE
#' @param point_size Numeric. Size of points on lines. Default: 2
#' @param point_shape Numeric or character. Shape of points (0-25 or "circle", "square", etc.). Default: 19 (solid circle)
#' @param point_alpha Numeric. Transparency of points (0-1). Default: 1
#'
#' @param line_size Numeric. Thickness of trend lines. Default: 1
#' @param line_type Character or numeric. Line type ("solid", "dashed", "dotted", etc.). Default: "solid"
#' @param line_alpha Numeric. Transparency of lines (0-1). Default: 1
#'
#' @param smooth_line Logical. Whether to use smoothed lines (loess). Default: FALSE
#' @param smooth_se Logical. Show confidence interval for smoothed lines. Default: FALSE
#' @param smooth_span Numeric. Smoothing parameter for loess (0-1). Default: 0.75
#'
#' @param color_palette Character or vector. Color scheme for lines:
#' - Character: named palette ("gtap", "viridis", "Set1", "Dark2", etc.)
#' - Vector: custom colors (e.g., c("#FF0000", "#00FF00", "#0000FF"))
#' Default: NULL (automatic)
#'
#' @param legend_title Character or NULL. Custom legend title. If NULL, uses line_group_by column name. Default: NULL
#'
#' @param period_breaks Character or numeric vector. X-axis breaks strategy:
#' - "all": Show all period values (default)
#' - "auto": Let ggplot2 decide breaks automatically
#' - Numeric vector: Custom breaks (e.g., c(2020, 2025, 2030))
#' Default: "all"
#'
#' @param vertical_lines List or NULL. Add vertical reference lines at specific periods.
#' Each element should be a list with: period (numeric), label (character),
#' color (character), linetype (character).
#' Example: list(list(period = 2019, label = "Shock 1", color = "black", linetype = "dashed"))
#' Default: NULL
#'
#' @param vline_label_size Numeric. Font size for vertical line labels. Default: 3.5
#' @param vline_label_angle Numeric. Angle for vertical line labels (0-360). Default: 90
#' @param vline_label_vjust Numeric. Vertical adjustment for labels. Default: -0.5
#'
#' @param show_title Logical. Show or hide plot title. Default: TRUE
#' @param title_face Character. Font face for title ("plain", "bold", "italic"). Default: "bold"
#' @param title_size Numeric. Font size of title. Default: 20
#' @param title_hjust Numeric. Horizontal justification of title (0-1). Default: 0.5
#' @param add_unit_to_title Logical. Add unit information to title. Default: TRUE
#' @param title_margin Numeric vector c(top, right, bottom, left). Margins around title. Default: c(10, 0, 10, 0)
#' @param title_format List. Title format configuration from create_title_format(). Default: list(type = "standard", text = "", sep = "")
#'
#' @param show_x_axis_title Logical. Show or hide x-axis title. Default: TRUE
#' @param x_axis_title_face Character. Font face for x-axis title. Default: "bold"
#' @param x_axis_title_size Numeric. Font size of x-axis title. Default: 16
#' @param x_axis_title_margin Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)
#' @param show_x_axis_labels Logical. Show or hide x-axis labels. Default: TRUE
#' @param x_axis_text_face Character. Font face for x-axis text. Default: "plain"
#' @param x_axis_text_size Numeric. Font size of x-axis text. Default: 14
#' @param x_axis_text_angle Numeric. Angle of x-axis text (0-360). Default: 0
#' @param x_axis_text_hjust Numeric. Horizontal justification of x-axis text. Default: 0
#' @param x_axis_description Character. Custom x-axis label. Default: ""
#'
#' @param show_y_axis_title Logical. Show or hide y-axis title. Default: TRUE
#' @param y_axis_title_face Character. Font face for y-axis title. Default: "bold"
#' @param y_axis_title_size Numeric. Font size of y-axis title. Default: 16
#' @param y_axis_title_margin Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)
#' @param show_y_axis_labels Logical. Show or hide y-axis labels. Default: TRUE
#' @param y_axis_text_face Character. Font face for y-axis text. Default: "plain"
#' @param y_axis_text_size Numeric. Font size of y-axis text. Default: 14
#' @param y_axis_text_angle Numeric. Angle of y-axis text. Default: 0
#' @param y_axis_text_hjust Numeric. Horizontal justification of y-axis text. Default: 0
#' @param y_axis_description Character. Custom y-axis label. Default: ""
#' @param show_axis_titles_on_all_facets Logical. Show axis titles on all facets. Default: TRUE
#'
#' @param show_legend Logical. Show or hide legend. Default: TRUE
#' @param show_legend_title Logical. Show or hide legend title. Default: FALSE
#' @param legend_position Character. Legend position ("right", "left", "top", "bottom", "none"). Default: "right"
#' @param legend_title_face Character. Font face for legend title. Default: "bold"
#' @param legend_text_face Character. Font face for legend text. Default: "plain"
#' @param legend_text_size Numeric. Font size of legend text. Default: 14
#'
#' @param strip_face Character. Font face for panel strip. Default: "bold"
#' @param strip_text_size Numeric. Font size for panel strip. Default: 16
#' @param strip_background Character. Background color of strip. Default: "lightgrey"
#' @param strip_text_margin Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)
#'
#' @param panel_spacing Numeric. Spacing between panels. Default: 2
#' @param panel_rows Numeric or NULL. Number of rows in panel layout. Default: NULL
#' @param panel_cols Numeric or NULL. Number of columns in panel layout. Default: NULL
#' @param theme ggplot2 theme or NULL. Custom ggplot theme. Default: NULL
#'
#' @param color_tone Character or NULL. Base color theme. Default: NULL
#' @param color_palette_type Character. Type of color palette ('qualitative', 'sequential', 'diverging'). Default: "qualitative"
#' @param positive_color Character. Color for positive values. Default: "#2E8B57"
#' @param negative_color Character. Color for negative values. Default: "#CD5C5C"
#' @param background_color Character. Background color of plot. Default: "white"
#' @param grid_color Character. Color of grid lines. Default: "grey90"
#' @param show_grid_major_x Logical. Show major grid lines on x-axis. Default: TRUE
#' @param show_grid_major_y Logical. Show major grid lines on y-axis. Default: TRUE
#' @param show_grid_minor_x Logical. Show minor grid lines on x-axis. Default: FALSE
#' @param show_grid_minor_y Logical. Show minor grid lines on y-axis. Default: FALSE
#'
#' @param show_zero_line Logical. Show or hide zero line. Default: TRUE
#' @param zero_line_type Character. Line type ("solid", "dashed", "dotted"). Default: "dashed"
#' @param zero_line_color Character. Color of zero line. Default: "black"
#' @param zero_line_size Numeric. Line thickness of zero line. Default: 0.5
#' @param zero_line_position Numeric. Position of the zero line. Default: 0
#'
#' @param scale_limit Numeric vector of length 2 or NULL. Manual limits for value axis. Default: NULL
#' @param scale_increment Numeric or NULL. Step size for axis tick marks. Default: NULL
#'
#' @param expansion_y_mult Numeric vector. Y-axis expansion. Default: c(0.05, 0.1)
#' @param expansion_x_mult Numeric vector. X-axis expansion. Default: c(0.05, 0.05)
#'
#' @param all_font_size Numeric. Master control for all font sizes. Default: 1
#'
#' @param plot.margin Numeric vector c(top, right, bottom, left). Margins around the entire plot. Default: c(10, 25, 10, 10)
#'
#' @return A list containing trend plot style configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Create basic trend style
#' trend_style <- create_trend_style(show_points = TRUE, line_size = 1.5)
#'
#' # Create smooth trend with confidence interval
#' smooth_trend <- create_trend_style(
#'   smooth_line = TRUE,
#'   smooth_se = TRUE,
#'   show_points = FALSE
#' )
#'
#' # Custom colors and styling
#' custom_trend <- create_trend_style(
#'   color_palette = c("#E41A1C", "#377EB8", "#4DAF4A"),
#'   line_size = 2,
#'   point_size = 3,
#'   show_legend = TRUE,
#'   legend_position = "bottom"
#' )
create_trend_style <- function(
    # Line-specific settings
  show_points = TRUE,
  point_size = 2,
  point_shape = 19,
  point_alpha = 1,

  line_size = 1,
  line_type = "solid",
  line_alpha = 1,

  smooth_line = FALSE,
  smooth_se = FALSE,
  smooth_span = 0.75,

  # Color settings
  color_palette = NULL,
  legend_title = NULL,

  # Period axis settings
  period_breaks = "all",

  # Vertical line settings
  vertical_lines = NULL,
  vline_label_size = 3.5,
  vline_label_angle = 90,
  vline_label_vjust = -0.5,

  # Title settings (from create_plot_style)
  show_title = TRUE,
  title_face = "bold",
  title_size = 20,
  title_hjust = 0.5,
  add_unit_to_title = TRUE,
  title_margin = c(10, 0, 10, 0),
  title_format = list(type = "standard", text = "", sep = ""),

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
  show_axis_titles_on_all_facets = TRUE,

  # Legend settings (override defaults for trend plots)
  show_legend = TRUE,
  show_legend_title = FALSE,
  legend_position = "right",
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

  # Color settings (from create_plot_style)
  color_tone = NULL,
  color_palette_type = "qualitative",
  positive_color = "#2E8B57",
  negative_color = "#CD5C5C",
  background_color = "white",
  grid_color = "grey90",
  show_grid_major_x = TRUE,
  show_grid_major_y = TRUE,
  show_grid_minor_x = FALSE,
  show_grid_minor_y = FALSE,

  # Zero line settings
  show_zero_line = TRUE,
  zero_line_type = "dashed",
  zero_line_color = "black",
  zero_line_size = 0.5,
  zero_line_position = 0,

  # Scale settings
  scale_limit = NULL,
  scale_increment = NULL,

  # Scale expansion settings
  expansion_y_mult = c(0.05, 0.1),
  expansion_x_mult = c(0.05, 0.05),

  # Font size settings
  all_font_size = 1,

  # Plot margin
  plot.margin = c(10, 25, 10, 10)
) {
  # Process title_format if it was created using create_title_format()
  if (is.function(title_format)) {
    title_format <- title_format()
  }

  # Collect all arguments into a list
  style_config <- as.list(environment())

  # Return the style configuration list
  return(style_config)
}


# INTERNAL HELPER FUNCTIONS -----------------------------------------------

#' @keywords internal
#' @noRd
.calculate_trend_style_config <- function(config = NULL) {
  # Get default trend configuration
  default_config <- create_trend_style()

  # If user provided config, merge with defaults
  if (!is.null(config)) {
    for (param_name in names(config)) {
      if (param_name %in% names(default_config)) {
        default_config[[param_name]] <- config[[param_name]]
      }
    }
  }

  # Apply font size multiplier
  if (!is.null(default_config$all_font_size)) {
    font_params <- c("title_size", "x_axis_title_size", "x_axis_text_size",
                     "y_axis_title_size", "y_axis_text_size", "legend_text_size",
                     "strip_text_size", "vline_label_size")
    for (param in font_params) {
      if (param %in% names(default_config)) {
        default_config[[param]] <- default_config[[param]] * default_config$all_font_size
      }
    }
  }

  return(default_config)
}


#' @keywords internal
#' @noRd
.create_trend_plots <- function(data, filter_var, period_col, split_by, line_group_by,
                                panel_var, variable_col, unit_col, desc_col,
                                separate_figure, var_name_by_description,
                                add_var_info, style_config) {

  plot_list <- list()

  for (df in data) {
    # Apply filters if specified
    if (!is.null(filter_var)) {
      df <- .apply_filters(df, filter_var)
      if (nrow(df) == 0) next
    }

    # Ensure period column is numeric
    if (!is.numeric(df[[period_col]])) {
      df[[period_col]] <- as.numeric(as.character(df[[period_col]]))
    }

    # Sort by ScenarioRank if available
    if ("ScenarioRank" %in% colnames(df)) {
      df <- df[order(df$ScenarioRank), ]
    }

    # Create line group identifier
    if (length(line_group_by) > 1) {
      df$LineGroup <- apply(df[, line_group_by, drop = FALSE], 1, paste, collapse = " | ")
    } else {
      df$LineGroup <- df[[line_group_by]]
    }

    # Sort LineGroup by ScenarioRank if available
    if ("ScenarioRank" %in% colnames(df)) {
      # Create a mapping of LineGroup to ScenarioRank
      group_rank <- unique(df[, c("LineGroup", "ScenarioRank")])
      group_rank <- group_rank[order(group_rank$ScenarioRank), ]
      df$LineGroup <- factor(df$LineGroup, levels = group_rank$LineGroup)
    }

    # Process each split combination
    if (!is.null(split_by)) {
      split_combinations <- unique(df[, split_by, drop = FALSE])

      for (i in seq_len(nrow(split_combinations))) {
        sep_value <- split_combinations[i, , drop = FALSE]
        filtered_data <- df
        for (col_name in split_by) {
          filtered_data <- filtered_data[filtered_data[[col_name]] == sep_value[[col_name]], ]
        }

        if (nrow(filtered_data) == 0) next

        # Get unit name
        unit_name <- if (unit_col %in% colnames(filtered_data)) {
          unique(filtered_data[[unit_col]])[1]
        } else {
          "Value"
        }

        # Handle panel separation
        if (!is.null(panel_var) && separate_figure) {
          panel_values <- unique(filtered_data[[panel_var]])

          for (panel_val in panel_values) {
            panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]

            title_info <- .handle_trend_title_and_export(
              sep_value = sep_value,
              split_by = split_by,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = panel_data,
              var_name_by_description = var_name_by_description,
              add_var_info = add_var_info,
              desc_col = desc_col,
              panel_val = panel_val
            )

            p <- .create_single_trend_plot(
              data = panel_data,
              period_col = period_col,
              plot_title = title_info$title,
              unit = unit_name,
              panel_var = NULL,
              style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        } else {
          title_info <- .handle_trend_title_and_export(
            sep_value = sep_value,
            split_by = split_by,
            variable_col = variable_col,
            unit_name = unit_name,
            style_config = style_config,
            data = filtered_data,
            var_name_by_description = var_name_by_description,
            add_var_info = add_var_info,
            desc_col = desc_col
          )

          p <- .create_single_trend_plot(
            data = filtered_data,
            period_col = period_col,
            plot_title = title_info$title,
            unit = unit_name,
            panel_var = panel_var,
            style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
        }
      }
    } else {
      # No split_by - create single plot
      unit_name <- if (unit_col %in% colnames(df)) {
        unique(df[[unit_col]])[1]
      } else {
        "Value"
      }

      title_info <- .handle_trend_title_and_export(
        sep_value = NULL,
        split_by = NULL,
        variable_col = variable_col,
        unit_name = unit_name,
        style_config = style_config,
        data = df,
        var_name_by_description = var_name_by_description,
        add_var_info = add_var_info,
        desc_col = desc_col
      )

      p <- .create_single_trend_plot(
        data = df,
        period_col = period_col,
        plot_title = title_info$title,
        unit = unit_name,
        panel_var = panel_var,
        style_config = style_config
      )

      plot_list[[title_info$export_name]] <- p
    }
  }

  return(plot_list)
}


#' @keywords internal
#' @noRd
.create_single_trend_plot <- function(data, period_col, plot_title, unit,
                                      panel_var = NULL, style_config) {

  # Calculate Y-Axis Limits
  y_limits <- .calculate_value_axis_limits(data, unit, style_config)

  # Format axis labels
  y_axis_label <- .format_y_axis_label(unit, style_config)
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    period_col
  }

  # Generate color palette
  n_groups <- length(unique(data$LineGroup))
  if (!is.null(style_config$color_palette)) {
    if (length(style_config$color_palette) == 1 && is.character(style_config$color_palette)) {
      # Named palette
      colors <- .generate_line_colors(n_groups, style_config$color_palette)
    } else {
      # Custom color vector
      colors <- rep_len(style_config$color_palette, n_groups)
    }
  } else {
    colors <- .generate_line_colors(n_groups, "default")
  }

  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[period_col]],
    y = .data[["Value"]],
    color = .data[["LineGroup"]],
    group = .data[["LineGroup"]]
  ))

  # Add lines
  if (style_config$smooth_line) {
    p <- p + ggplot2::geom_smooth(
      method = "loess",
      se = style_config$smooth_se,
      span = style_config$smooth_span,
      linewidth = style_config$line_size,
      linetype = style_config$line_type,
      alpha = style_config$line_alpha
    )
  } else {
    p <- p + ggplot2::geom_line(
      linewidth = style_config$line_size,
      linetype = style_config$line_type,
      alpha = style_config$line_alpha
    )
  }

  # Add points if configured
  if (style_config$show_points) {
    p <- p + ggplot2::geom_point(
      size = style_config$point_size,
      shape = style_config$point_shape,
      alpha = style_config$point_alpha
    )
  }

  # Apply color scale
  legend_label <- if (!is.null(style_config$legend_title)) {
    style_config$legend_title
  } else {
    "LineGroup"
  }

  p <- p + ggplot2::scale_color_manual(values = colors, name = legend_label)

  # Apply Y-axis scale
  p <- p + .apply_axis_scale(y_limits, style_config, axis = "y")

  # Apply X-axis scale with period breaks
  if (!is.null(style_config$period_breaks)) {
    if (is.character(style_config$period_breaks) && style_config$period_breaks == "all") {
      # Show all period values
      all_periods <- sort(unique(data[[period_col]]))
      p <- p + ggplot2::scale_x_continuous(breaks = all_periods)
    } else if (is.character(style_config$period_breaks) && style_config$period_breaks == "auto") {
      # Let ggplot2 decide automatically (default behavior)
      p <- p + ggplot2::scale_x_continuous()
    } else if (is.numeric(style_config$period_breaks)) {
      # Custom breaks provided
      p <- p + ggplot2::scale_x_continuous(breaks = style_config$period_breaks)
    }
  }

  # Add zero line if configured
  if (style_config$show_zero_line) {
    p <- .add_zero_line(p, invert_axis = FALSE, style_config)
  }

  # Add vertical lines if configured
  if (!is.null(style_config$vertical_lines) && is.list(style_config$vertical_lines)) {
    for (vline in style_config$vertical_lines) {
      if (!is.null(vline$period)) {
        # Add vertical line
        p <- p + ggplot2::geom_vline(
          xintercept = vline$period,
          linetype = if (!is.null(vline$linetype)) vline$linetype else "dashed",
          color = if (!is.null(vline$color)) vline$color else "black",
          linewidth = if (!is.null(vline$size)) vline$size else 0.5
        )

        # Add label if provided
        if (!is.null(vline$label) && nchar(vline$label) > 0) {
          p <- p + ggplot2::annotate(
            "text",
            x = vline$period,
            y = Inf,
            label = vline$label,
            angle = style_config$vline_label_angle,
            vjust = style_config$vline_label_vjust,
            hjust = 1,
            size = style_config$vline_label_size,
            color = if (!is.null(vline$color)) vline$color else "black"
          )
        }
      }
    }
  }

  # Set labels
  p <- .set_axis_labels(p, FALSE, plot_title, x_axis_label, y_axis_label, style_config)

  # Add faceting if panel_var is specified
  if (!is.null(panel_var)) {
    p <- p + .create_facet_wrap(panel_var, style_config$panel_rows, style_config$panel_cols, style_config)
  }

  # Apply style configuration (theme, fonts, colors, etc.)
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}


#' @keywords internal
#' @noRd
.handle_trend_title_and_export <- function(sep_value, split_by, variable_col,
                                           unit_name, style_config, data,
                                           var_name_by_description, add_var_info,
                                           desc_col, panel_val = NULL) {

  title_parts <- c()
  export_parts <- c()

  # Add split_by information to title
  if (!is.null(split_by) && !is.null(sep_value)) {
    for (col in split_by) {
      val <- as.character(sep_value[[col]])

      # Handle variable column specially
      if (col == variable_col) {
        if (var_name_by_description && desc_col %in% colnames(data)) {
          desc <- unique(data[data[[variable_col]] == val, desc_col])[1]
          if (add_var_info) {
            title_parts <- c(title_parts, paste0(desc, " (", val, ")"))
          } else {
            title_parts <- c(title_parts, desc)
          }
        } else {
          title_parts <- c(title_parts, val)
        }
        export_parts <- c(export_parts, val)
      } else {
        title_parts <- c(title_parts, paste0(col, ": ", val))
        export_parts <- c(export_parts, val)
      }
    }
  }

  # Add panel value if separate figure
  if (!is.null(panel_val)) {
    title_parts <- c(title_parts, as.character(panel_val))
    export_parts <- c(export_parts, as.character(panel_val))
  }

  # Build title
  if (length(title_parts) > 0) {
    title <- paste(title_parts, collapse = " | ")
  } else {
    title <- "Trend Plot"
  }

  # Add unit if configured
  if (style_config$add_unit_to_title && !is.null(unit_name)) {
    title <- paste0(title, " (", unit_name, ")")
  }

  # Build export name
  if (length(export_parts) > 0) {
    export_name <- paste(export_parts, collapse = "_")
    export_name <- gsub("[^A-Za-z0-9_-]", "_", export_name)
  } else {
    export_name <- "trend_plot"
  }

  return(list(title = title, export_name = export_name))
}


#' @keywords internal
#' @noRd
.generate_line_colors <- function(n, palette_name = "default") {
  if (palette_name == "gtap") {
    base_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                     "#FFFF33", "#A65628", "#F781BF", "#999999")
    return(rep_len(base_colors, n))
  } else if (palette_name %in% c("Set1", "Set2", "Set3", "Dark2", "Paired", "Accent", "Pastel1", "Pastel2")) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(max_colors, palette_name))(n))
  } else if (palette_name %in% c("viridis", "magma", "plasma", "inferno", "cividis")) {
    option_char <- switch(palette_name,
                          "viridis" = "D",
                          "magma" = "A",
                          "plasma" = "C",
                          "inferno" = "B",
                          "cividis" = "E",
                          "D")
    return(viridisLite::viridis(n, option = option_char))
  } else {
    # Default palette - try to use existing color creation function
    colors_result <- .create_color_palette(color_tone = palette_name, n_colors = n, palette_type = "qualitative")
    if (!is.null(colors_result)) {
      return(colors_result)
    }

    # Fallback to RColorBrewer Set1
    if (n <= 9) {
      return(RColorBrewer::brewer.pal(max(3, n), "Set1")[1:n])
    } else {
      return(grDevices::rainbow(n))
    }
  }
}


# HELPER FUNCTION FOR GETTING TREND STYLE CONFIG --------------------------

#' @title Get Trend Plot Style Configuration Helper
#'
#' @description
#' Prints a complete trend plot style configuration template to the console
#' that can be copied, modified, and used with trend_plot().
#'
#' @return Invisibly returns NULL after printing configuration template
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Print trend style configuration template
#' get_trend_style_config()
#'
#' # Copy the output and customize as needed
get_trend_style_config <- function() {
  config <- create_trend_style()

  # Start building the message string
  msg <- "my_trend_style <- create_trend_style(\n"

  # Line-specific settings
  msg <- paste0(msg, "\n  # Line and Point settings\n")
  msg <- paste0(msg, "  show_points = ", ifelse(config$show_points, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  point_size = ", config$point_size, ",\n")
  msg <- paste0(msg, "  point_shape = ", config$point_shape, ", # 0-25 or 'circle', 'square', etc.\n")
  msg <- paste0(msg, "  point_alpha = ", config$point_alpha, ",\n\n")

  msg <- paste0(msg, "  line_size = ", config$line_size, ",\n")
  msg <- paste0(msg, "  line_type = \"", config$line_type, "\", # 'solid', 'dashed', 'dotted'\n")
  msg <- paste0(msg, "  line_alpha = ", config$line_alpha, ",\n\n")

  # Smoothing settings
  msg <- paste0(msg, "  # Smoothing settings\n")
  msg <- paste0(msg, "  smooth_line = ", ifelse(config$smooth_line, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  smooth_se = ", ifelse(config$smooth_se, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  smooth_span = ", config$smooth_span, ",\n\n")

  # Color settings
  msg <- paste0(msg, "  # Color settings\n")
  if (is.null(config$color_palette)) {
    msg <- paste0(msg, "  color_palette = NULL, # 'gtap', 'viridis', 'Set1', or c('#FF0000', '#00FF00')\n")
  } else if (length(config$color_palette) == 1) {
    msg <- paste0(msg, "  color_palette = \"", config$color_palette, "\",\n")
  } else {
    msg <- paste0(msg, "  color_palette = c(\"", paste(config$color_palette, collapse = "\", \""), "\"),\n")
  }
  msg <- paste0(msg, "  legend_title = ", ifelse(is.null(config$legend_title), "NULL", paste0("\"", config$legend_title, "\"")), ",\n\n")

  # Period breaks
  msg <- paste0(msg, "  # Period axis settings\n")
  if (is.character(config$period_breaks)) {
    msg <- paste0(msg, "  period_breaks = \"", config$period_breaks, "\", # 'all', 'auto', or numeric vector\n\n")
  } else {
    msg <- paste0(msg, "  period_breaks = c(", paste(config$period_breaks, collapse = ", "), "),\n\n")
  }

  # Vertical lines example
  msg <- paste0(msg, "  # Vertical reference lines\n")
  msg <- paste0(msg, "  vertical_lines = NULL, # list(list(period=2019, label='Shock 1', color='black', linetype='dashed'))\n")
  msg <- paste0(msg, "  vline_label_size = ", config$vline_label_size, ",\n")
  msg <- paste0(msg, "  vline_label_angle = ", config$vline_label_angle, ",\n")
  msg <- paste0(msg, "  vline_label_vjust = ", config$vline_label_vjust, ",\n\n")

  # Title settings
  msg <- paste0(msg, "  # Title settings\n")
  msg <- paste0(msg, "  show_title = ", ifelse(config$show_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  title_size = ", config$title_size, ",\n")
  msg <- paste0(msg, "  add_unit_to_title = ", ifelse(config$add_unit_to_title, "TRUE", "FALSE"), ",\n\n")

  # Legend settings
  msg <- paste0(msg, "  # Legend settings\n")
  msg <- paste0(msg, "  show_legend = ", ifelse(config$show_legend, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  legend_position = \"", config$legend_position, "\", # 'right', 'left', 'top', 'bottom'\n")
  msg <- paste0(msg, "  legend_text_size = ", config$legend_text_size, ",\n\n")

  # Grid settings
  msg <- paste0(msg, "  # Grid settings\n")
  msg <- paste0(msg, "  show_grid_major_x = ", ifelse(config$show_grid_major_x, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_grid_major_y = ", ifelse(config$show_grid_major_y, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  grid_color = \"", config$grid_color, "\",\n\n")

  # Zero line
  msg <- paste0(msg, "  # Zero line settings\n")
  msg <- paste0(msg, "  show_zero_line = ", ifelse(config$show_zero_line, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  zero_line_type = \"", config$zero_line_type, "\",\n\n")

  # Font size
  msg <- paste0(msg, "  # Font settings\n")
  msg <- paste0(msg, "  all_font_size = ", config$all_font_size, " # Multiplier for all text sizes\n")

  msg <- paste0(msg, ")\n\n")
  msg <- paste0(msg, "# Example usage:\n")
  msg <- paste0(msg, "# trend_plot(data, period_col = 'Period', line_group_by = 'Case', \n")
  msg <- paste0(msg, "#            split_by = c('Variable', 'AREG'), trend_style_config = my_trend_style)\n")

  # Output the message
  message(msg)

  return(invisible(NULL))
}


# HELPER FUNCTION FOR CREATING VERTICAL LINES ----------------------------

#' @title Create Vertical Line Configuration
#'
#' @description
#' Helper function to create vertical line specifications for trend plots.
#' Makes it easier to add reference lines at specific periods.
#'
#' @param period Numeric. The period (x-axis value) where the line should appear
#' @param label Character. Text label for the line. Default: ""
#' @param color Character. Color of the line. Default: "black"
#' @param linetype Character. Line type ("solid", "dashed", "dotted", etc.). Default: "dashed"
#' @param size Numeric. Line thickness. Default: 0.5
#'
#' @return A list with vertical line configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Create single vertical line
#' vline1 <- create_vline(period = 2019, label = "Shock 1", color = "red")
#'
#' # Create multiple vertical lines
#' vlines <- list(
#'   create_vline(2019, "Shock 1", "black", "dashed"),
#'   create_vline(2023, "Shock 2", "blue", "dotted"),
#'   create_vline(2025, "Shock 3", "darkgreen", "dashed")
#' )
#'
#' # Use in trend plot
#' trend_plot(data,
#'            trend_style_config = create_trend_style(vertical_lines = vlines))
create_vline <- function(period, label = "", color = "black",
                         linetype = "dashed", size = 0.5) {
  list(
    period = period,
    label = label,
    color = color,
    linetype = linetype,
    size = size
  )
}
