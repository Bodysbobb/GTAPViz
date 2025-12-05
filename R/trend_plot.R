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
#' @param panel_var Character. Column for panel facets (e.g., `"Case"`). When specified,
#' creates separate figures by default for each unique value. Set `separate_figure = FALSE` to use facet panels instead.
#' Default is `NULL` (no faceting).
#' @param variable_col Character. Column name for variable codes. Default is `"Variable"`.
#' @param unit_col Character. Column name for units. Default is `"Unit"`.
#' @param desc_col Character. Column name for variable descriptions. Default is `"Description"`.
#'
#' **Plot Behavior**
#' @param separate_figure Logical. When `panel_var` is specified, controls whether to create separate plots
#' (TRUE) or use facet panels (FALSE). Automatically set to `TRUE` when `panel_var` is specified. Default is `FALSE`.
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
#' @param title_format List or NULL. Title format configuration created with \code{\link{create_title_format}}.
#' Supports "standard", "prefix", "suffix", "full", and "dynamic" types. If `NULL`, uses standard formatting.
#' Example: \code{create_title_format(type = "dynamic", text = "Impacts on {Description}", sep = "")}.
#'
#' @return A ggplot object or a named list of ggplot objects depending on input settings.
#' If `export_picture` or `export_as_pdf` is enabled, plots are also saved to `output_path`.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Create simple example dataset
#' data <- data.frame(
#'   Period = rep(2017:2025, each = 3),
#'   Case = rep(c("Uniform Country", "Uniform Sector", "Heterogeneous"), times = 9),
#'   Variable = "GDP",
#'   AREG = "ASEAN",
#'   Value = cumsum(rnorm(27, 0, 0.2))
#' )
#'
#' # Define vertical lines for key shock years
#' vlines <- list(
#'   create_vline(2019, "Shock 1", "black", "dashed"),
#'   create_vline(2023, "Shock 2", "blue", "dotted"),
#'   create_vline(2025, "Shock 3", "darkgreen", "dashed")
#' )
#'
#' # Define trend style configuration
#' trend_style <- create_trend_style(
#'   show_points = TRUE,
#'   smooth_line = FALSE,
#'   line_size = 1,
#'   vertical_lines = vlines,
#'   title_format = create_title_format(type = "prefix", text = "Impact on")
#' )
#'
#' # Generate example trend plot
#' trend_plot(
#'   data = data,
#'   period_col = "Period",
#'   line_group_by = "Case",
#'   split_by = "Variable",
#'   trend_style_config = trend_style
#' )
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
                       trend_style_config = NULL,
                       title_format = NULL) {

  if (!is.data.frame(data) && !is.list(data)) {
    stop("'data' must be a data frame or a list of data frames.")
  }

  if (is.data.frame(data)) {
    data <- list(data)
  }

  # Auto-set separate_figure to TRUE when panel_var is specified (unless explicitly set to FALSE by user)
  if (!is.null(panel_var)) {
    # Check if separate_figure was explicitly provided by the user
    call_args <- as.list(match.call())
    if (!"separate_figure" %in% names(call_args)) {
      separate_figure <- TRUE
    }
  }

  if (!period_col %in% colnames(data[[1]])) {
    stop(paste0("Period column '", period_col, "' not found in data."))
  }

  if (!all(line_group_by %in% colnames(data[[1]]))) {
    stop(paste0("Line grouping column(s) '", paste(line_group_by, collapse = ", "), "' not found in data."))
  }

  style_config <- if (!is.null(trend_style_config)) {
    .calculate_trend_style_config(trend_style_config)
  } else {
    .calculate_trend_style_config(NULL)
  }

  if (!is.null(title_format)) {
    style_config$title_format <- title_format
  }

  export_config <- if (!is.null(export_config)) {
    export_config
  } else {
    create_export_config()
  }

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

  if (!is.null(output_path) && (export_picture || export_as_pdf)) {
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
      panel_layout = list(rows = 1, cols = 1),
      default_filename = "trend_plot"
    )
  }

  return(invisible(plot_list))
}


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
#' @param line_size Numeric. Thickness of trend lines. Default: 1
#' @param line_type Character. Line type ("solid", "dashed", "dotted", etc.). Default: "solid"
#' @param line_alpha Numeric. Transparency of lines (0-1). Default: 1
#' @param smooth_line Logical. Use smoothed lines (loess) instead of straight lines. Default: FALSE
#' @param smooth_se Logical. Show confidence interval around smooth lines. Default: FALSE
#' @param smooth_span Numeric. Smoothing parameter for loess (0-1). Default: 0.75
#' @param color_palette Character or vector. Color palette for lines. Options include: "default", "gtap", "viridis", "magma", "plasma", "inferno", "cividis", RColorBrewer palettes, or custom color vector. Default: NULL
#' @param legend_title Character. Custom title for the legend. Default: NULL
#' @param period_breaks Character or numeric vector. X-axis breaks: "all", "auto", or custom numeric vector. Default: "auto"
#' @param vertical_lines List or NULL. List of vertical line specifications created with create_vline(). Default: NULL
#' @param vline_label_size Numeric. Font size for vertical line labels. Default: 3.5
#' @param vline_label_angle Numeric. Angle for vertical line labels. Default: 90
#' @param vline_label_vjust Numeric. Vertical adjustment for vline labels. Default: -0.2
#' @param show_title Logical. Show plot title. Default: TRUE
#' @param title_face Character. Title font face. Default: "bold"
#' @param title_size Numeric. Title size. Default: 20
#' @param title_hjust Numeric. Title horizontal justification. Default: 0.5
#' @param add_unit_to_title Logical. Append unit to title. Default: TRUE
#' @param title_margin Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)
#' @param title_format List. Title formatting. See \code{\link{create_title_format}}
#' @param show_x_axis_title Logical. Show x-axis title. Default: TRUE
#' @param x_axis_title_face Character. X-axis title font face. Default: "bold"
#' @param x_axis_title_size Numeric. X-axis title size. Default: 16
#' @param x_axis_title_margin Numeric vector. X-axis title margins. Default: c(25, 25, 0, 0)
#' @param show_x_axis_labels Logical. Show x-axis labels. Default: TRUE
#' @param x_axis_text_face Character. X-axis text font face. Default: "plain"
#' @param x_axis_text_size Numeric. X-axis text size. Default: 14
#' @param x_axis_text_angle Numeric. X-axis text angle. Default: 0
#' @param x_axis_text_hjust Numeric. X-axis text horizontal justification. Default: 0
#' @param x_axis_description Character. Custom x-axis title. Default: ""
#' @param show_y_axis_title Logical. Show y-axis title. Default: TRUE
#' @param y_axis_title_face Character. Y-axis title font face. Default: "bold"
#' @param y_axis_title_size Numeric. Y-axis title size. Default: 16
#' @param y_axis_title_margin Numeric vector. Y-axis title margins. Default: c(25, 25, 0, 0)
#' @param show_y_axis_labels Logical. Show y-axis labels. Default: TRUE
#' @param y_axis_text_face Character. Y-axis text font face. Default: "plain"
#' @param y_axis_text_size Numeric. Y-axis text size. Default: 14
#' @param y_axis_text_angle Numeric. Y-axis text angle. Default: 0
#' @param y_axis_text_hjust Numeric. Y-axis text horizontal justification. Default: 0
#' @param y_axis_description Character. Custom y-axis title. Default: ""
#' @param show_legend Logical. Show legend. Default: TRUE
#' @param legend_position Character. Legend position ("right", "left", "top", "bottom"). Default: "right"
#' @param legend_text_face Character. Legend text font face. Default: "plain"
#' @param legend_text_size Numeric. Legend text size. Default: 14
#' @param strip_face Character. Facet strip font face. Default: "bold"
#' @param strip_text_size Numeric. Facet strip text size. Default: 16
#' @param strip_background Character. Facet strip background color. Default: "lightgrey"
#' @param strip_text_margin Numeric vector. Strip text margins. Default: c(10, 0, 10, 0)
#' @param panel_spacing Numeric. Spacing between facet panels. Default: 2
#' @param panel_rows Numeric or NULL. Number of panel rows. Default: NULL
#' @param panel_cols Numeric or NULL. Number of panel columns. Default: NULL
#' @param background_color Character. Plot background color. Default: "white"
#' @param grid_color Character. Grid line color. Default: "grey90"
#' @param show_grid_major_x Logical. Show major x-axis grid lines. Default: TRUE
#' @param show_grid_major_y Logical. Show major y-axis grid lines. Default: TRUE
#' @param show_grid_minor_x Logical. Show minor x-axis grid lines. Default: FALSE
#' @param show_grid_minor_y Logical. Show minor y-axis grid lines. Default: FALSE
#' @param show_zero_line Logical. Show zero reference line. Default: TRUE
#' @param zero_line_type Character. Zero line type. Default: "dashed"
#' @param zero_line_color Character. Zero line color. Default: "black"
#' @param zero_line_size Numeric. Zero line thickness. Default: 0.5
#' @param zero_line_position Numeric. Position of zero line. Default: 0
#' @param scale_limit Numeric vector or NULL. Y-axis limits c(min, max). Default: NULL
#' @param scale_increment Numeric or NULL. Y-axis scale increment. Default: NULL
#' @param expansion_y_mult Numeric vector. Y-axis expansion multipliers c(bottom, top). Default: c(0.05, 0.1)
#' @param expansion_x_mult Numeric vector. X-axis expansion multipliers c(left, right). Default: c(0.05, 0.05)
#' @param plot.margin Numeric vector c(top, right, bottom, left). Margins around plot in mm. Default: c(10, 25, 10, 10)
#' @param all_font_size Numeric. Master control for all font sizes. Default: 1
#'
#' @return List with trend plot style configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' basic_trend_style <- create_trend_style()
#'
#' custom_trend_style <- create_trend_style(
#'   show_points = TRUE,
#'   point_size = 3,
#'   line_size = 1.5,
#'   color_palette = "viridis",
#'   smooth_line = TRUE,
#'   vertical_lines = list(
#'     create_vline(2019, "Shock 1", "red", "dashed"),
#'     create_vline(2023, "Shock 2", "blue", "dotted")
#'   )
#' )
create_trend_style <- function(
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
    color_palette = NULL,
    legend_title = NULL,
    period_breaks = "auto",
    vertical_lines = NULL,
    vline_label_size = 3.5,
    vline_label_angle = 90,
    vline_label_vjust = -0.2,
    show_title = TRUE,
    title_face = "bold",
    title_size = 20,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = c(10, 0, 10, 0),
    title_format = list(type = "standard", text = "", sep = ""),
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
    show_legend = TRUE,
    legend_position = "right",
    legend_text_face = "plain",
    legend_text_size = 14,
    strip_face = "bold",
    strip_text_size = 16,
    strip_background = "lightgrey",
    strip_text_margin = c(10, 0, 10, 0),
    panel_spacing = 2,
    panel_rows = NULL,
    panel_cols = NULL,
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = TRUE,
    show_grid_major_y = TRUE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,
    show_zero_line = TRUE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,
    scale_limit = NULL,
    scale_increment = NULL,
    expansion_y_mult = c(0.05, 0.1),
    expansion_x_mult = c(0.05, 0.05),
    plot.margin = c(10, 25, 10, 10),
    all_font_size = 1
) {

  if (is.function(title_format)) {
    title_format <- title_format()
  }

  style_config <- as.list(environment())

  return(style_config)
}


#' @keywords internal
#' @noRd
.calculate_trend_style_config <- function(config = NULL) {
  default_config <- create_trend_style()

  if (!is.null(config)) {
    for (param_name in names(config)) {
      if (param_name %in% names(default_config)) {
        default_config[[param_name]] <- config[[param_name]]
      }
    }
  }

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
    if (!is.null(filter_var)) {
      df <- .apply_filters_trend_plot(df, filter_var)
      if (nrow(df) == 0) next
    }

    if (!is.numeric(df[[period_col]])) {
      df[[period_col]] <- as.numeric(as.character(df[[period_col]]))
    }

    if ("ScenarioRank" %in% colnames(df) && "Case" %in% line_group_by) {
      df <- df[order(df$ScenarioRank), ]
    }

    if (length(line_group_by) > 1) {
      df$LineGroup <- apply(df[, line_group_by, drop = FALSE], 1, paste, collapse = " | ")
      legend_name <- paste(line_group_by, collapse = " | ")
    } else {
      df$LineGroup <- df[[line_group_by]]
      legend_name <- line_group_by
    }

    if ("ScenarioRank" %in% colnames(df) && "Case" %in% line_group_by) {
      group_rank <- unique(df[, c("LineGroup", "ScenarioRank")])
      group_rank <- group_rank[order(group_rank$ScenarioRank), ]
      group_rank <- group_rank[!duplicated(group_rank$LineGroup), ]
      df$LineGroup <- factor(df$LineGroup, levels = group_rank$LineGroup)
    }

    if (!is.null(split_by)) {
      split_combinations <- unique(df[, split_by, drop = FALSE])

      for (i in seq_len(nrow(split_combinations))) {
        sep_value <- split_combinations[i, , drop = FALSE]
        filtered_data <- df
        for (col_name in split_by) {
          filtered_data <- filtered_data[filtered_data[[col_name]] == sep_value[[col_name]], ]
        }

        if (nrow(filtered_data) == 0) next

        unit_name <- if (unit_col %in% colnames(filtered_data)) {
          units <- unique(filtered_data[[unit_col]])
          units <- units[!is.na(units)]
          if (length(units) > 0) units[1] else "Value"
        } else {
          "Value"
        }

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
              style_config = style_config,
              legend_name = legend_name
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
            style_config = style_config,
            legend_name = legend_name
          )

          plot_list[[title_info$export_name]] <- p
        }
      }
    } else {
      unit_name <- if (unit_col %in% colnames(df)) {
        units <- unique(df[[unit_col]])
        units <- units[!is.na(units)]
        if (length(units) > 0) units[1] else "Value"
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
        style_config = style_config,
        legend_name = legend_name
      )

      plot_list[[title_info$export_name]] <- p
    }
  }

  return(plot_list)
}


#' @keywords internal
#' @noRd
.smart_x_axis_angle <- function(data, period_col, style_config, panel_var = NULL) {
  # If user explicitly set an angle (not the default 0), respect it
  if (!is.null(style_config$x_axis_text_angle) && style_config$x_axis_text_angle != 0) {
    return(list(
      angle = style_config$x_axis_text_angle,
      hjust = style_config$x_axis_text_hjust
    ))
  }

  # Count unique periods
  n_periods <- length(unique(data[[period_col]]))

  # Calculate effective number of panels if using panel_var
  n_panels <- if (!is.null(panel_var)) {
    length(unique(data[[panel_var]]))
  } else {
    1
  }

  # Estimate periods per panel (for faceted plots)
  periods_per_panel <- n_periods

  # Smart rotation thresholds
  # These are heuristic - adjust based on your typical plot dimensions
  if (periods_per_panel <= 10) {
    # Few periods: horizontal is fine
    return(list(angle = 0, hjust = 0.5))
  } else if (periods_per_panel <= 20) {
    # Medium density: 45 degrees
    return(list(angle = 45, hjust = 1))
  } else {
    # High density: 90 degrees
    return(list(angle = 90, hjust = 1))
  }
}


#' @keywords internal
#' @noRd
.create_single_trend_plot <- function(data, period_col, plot_title, unit,
                                      panel_var = NULL, style_config, legend_name = NULL) {

  # Apply smart x-axis angle detection
  smart_angle <- .smart_x_axis_angle(data, period_col, style_config, panel_var)
  style_config$x_axis_text_angle <- smart_angle$angle
  style_config$x_axis_text_hjust <- smart_angle$hjust

  y_limits <- .calculate_value_axis_limits(data, unit, style_config)

  y_axis_label <- .format_y_axis_label(unit, style_config)
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    period_col
  }

  n_groups <- length(unique(data$LineGroup))
  if (!is.null(style_config$color_palette)) {
    if (length(style_config$color_palette) == 1 && is.character(style_config$color_palette)) {
      colors <- .generate_line_colors(n_groups, style_config$color_palette)
    } else {
      colors <- rep_len(style_config$color_palette, n_groups)
    }
  } else {
    colors <- .generate_line_colors(n_groups, "default")
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[period_col]],
    y = .data[["Value"]],
    color = .data[["LineGroup"]],
    group = .data[["LineGroup"]]
  ))

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

  if (style_config$show_points) {
    p <- p + ggplot2::geom_point(
      size = style_config$point_size,
      shape = style_config$point_shape,
      alpha = style_config$point_alpha
    )
  }

  legend_label <- if (!is.null(style_config$legend_title)) {
    style_config$legend_title
  } else if (!is.null(legend_name)) {
    legend_name
  } else {
    "LineGroup"
  }

  p <- p + ggplot2::scale_color_manual(values = colors, name = legend_label)

  has_vline_labels <- FALSE
  if (!is.null(style_config$vertical_lines) && is.list(style_config$vertical_lines)) {
    for (vline in style_config$vertical_lines) {
      if (!is.null(vline$label) && nchar(vline$label) > 0) {
        has_vline_labels <- TRUE
        break
      }
    }
  }

  if (has_vline_labels) {
    current_expansion <- style_config$expansion_y_mult
    expanded_top <- current_expansion[2] + 0.15
    adjusted_config <- style_config
    adjusted_config$expansion_y_mult <- c(current_expansion[1], expanded_top)
    p <- p + .apply_axis_scale(y_limits, adjusted_config, axis = "y")
  } else {
    p <- p + .apply_axis_scale(y_limits, style_config, axis = "y")
  }

  if (!is.null(style_config$period_breaks)) {
    if (is.character(style_config$period_breaks) && style_config$period_breaks == "all") {
      all_periods <- sort(unique(data[[period_col]]))
      p <- p + ggplot2::scale_x_continuous(breaks = all_periods)
    } else if (is.character(style_config$period_breaks) && style_config$period_breaks == "auto") {
      p <- p + ggplot2::scale_x_continuous()
    } else if (is.numeric(style_config$period_breaks)) {
      p <- p + ggplot2::scale_x_continuous(breaks = style_config$period_breaks)
    }
  }

  if (style_config$show_zero_line) {
    p <- p + ggplot2::geom_hline(
      yintercept = style_config$zero_line_position,
      linetype = style_config$zero_line_type,
      color = style_config$zero_line_color,
      linewidth = style_config$zero_line_size
    )
  }

  if (!is.null(style_config$vertical_lines) && is.list(style_config$vertical_lines)) {
    for (vline in style_config$vertical_lines) {
      if (!is.null(vline$period)) {
        p <- p + ggplot2::geom_vline(
          xintercept = vline$period,
          linetype = if (!is.null(vline$linetype)) vline$linetype else "dashed",
          color = if (!is.null(vline$color)) vline$color else "black",
          linewidth = if (!is.null(vline$size)) vline$size else 0.5
        )

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

  p <- p +
    ggplot2::labs(
      x = if (style_config$show_x_axis_title) x_axis_label else NULL,
      y = if (style_config$show_y_axis_title) y_axis_label else NULL,
      title = if (style_config$show_title) plot_title else NULL
    ) +
    ggplot2::theme_minimal(base_size = 13 * style_config$all_font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = style_config$title_size,
        face = style_config$title_face,
        hjust = style_config$title_hjust,
        margin = ggplot2::margin(
          t = style_config$title_margin[1],
          r = style_config$title_margin[2],
          b = style_config$title_margin[3],
          l = style_config$title_margin[4]
        )
      ),
      axis.title.x = if (style_config$show_x_axis_title) {
        ggplot2::element_text(
          size = style_config$x_axis_title_size,
          face = style_config$x_axis_title_face,
          margin = ggplot2::margin(
            t = style_config$x_axis_title_margin[1],
            r = style_config$x_axis_title_margin[2],
            b = style_config$x_axis_title_margin[3],
            l = style_config$x_axis_title_margin[4]
          )
        )
      } else {
        ggplot2::element_blank()
      },
      axis.title.y = if (style_config$show_y_axis_title) {
        ggplot2::element_text(
          size = style_config$y_axis_title_size,
          face = style_config$y_axis_title_face,
          margin = ggplot2::margin(
            t = style_config$y_axis_title_margin[1],
            r = style_config$y_axis_title_margin[2],
            b = style_config$y_axis_title_margin[3],
            l = style_config$y_axis_title_margin[4]
          )
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text.x = if (style_config$show_x_axis_labels) {
        ggplot2::element_text(
          size = style_config$x_axis_text_size,
          face = style_config$x_axis_text_face,
          angle = style_config$x_axis_text_angle,
          hjust = style_config$x_axis_text_hjust
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = if (style_config$show_y_axis_labels) {
        ggplot2::element_text(
          size = style_config$y_axis_text_size,
          face = style_config$y_axis_text_face,
          angle = style_config$y_axis_text_angle,
          hjust = style_config$y_axis_text_hjust
        )
      } else {
        ggplot2::element_blank()
      },
      legend.position = style_config$legend_position,
      legend.text = ggplot2::element_text(
        size = style_config$legend_text_size,
        face = style_config$legend_text_face
      ),
      strip.text = ggplot2::element_text(
        size = style_config$strip_text_size,
        face = style_config$strip_face,
        margin = ggplot2::margin(
          t = style_config$strip_text_margin[1],
          r = style_config$strip_text_margin[2],
          b = style_config$strip_text_margin[3],
          l = style_config$strip_text_margin[4]
        )
      ),
      strip.background = ggplot2::element_rect(fill = style_config$strip_background, color = NA),
      panel.spacing = ggplot2::unit(style_config$panel_spacing, "lines"),
      panel.background = ggplot2::element_rect(fill = style_config$background_color, color = NA),
      plot.background = ggplot2::element_rect(fill = style_config$background_color, color = NA),
      panel.grid.major.x = if (style_config$show_grid_major_x) {
        ggplot2::element_line(color = style_config$grid_color)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = if (style_config$show_grid_major_y) {
        ggplot2::element_line(color = style_config$grid_color)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor.x = if (style_config$show_grid_minor_x) {
        ggplot2::element_line(color = style_config$grid_color)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor.y = if (style_config$show_grid_minor_y) {
        ggplot2::element_line(color = style_config$grid_color)
      } else {
        ggplot2::element_blank()
      },
      plot.margin = ggplot2::margin(
        t = style_config$plot.margin[1],
        r = style_config$plot.margin[2],
        b = style_config$plot.margin[3],
        l = style_config$plot.margin[4]
      )
    )

  if (!is.null(panel_var)) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data[[panel_var]]),
      nrow = style_config$panel_rows,
      ncol = style_config$panel_cols
    )
  }

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

  if (!is.null(split_by) && !is.null(sep_value)) {
    for (col in split_by) {
      val <- as.character(sep_value[[col]])

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

  if (!is.null(panel_val)) {
    title_parts <- c(title_parts, as.character(panel_val))
    export_parts <- c(export_parts, as.character(panel_val))
  }

  if (length(title_parts) > 0) {
    plot_title <- paste(title_parts, collapse = " | ")
  } else {
    plot_title <- "Trend Plot"
  }

  dynamic_title_has_unit <- FALSE
  if (!is.null(style_config) && !is.null(style_config$title_format)) {
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
            if (any(referenced_cols %in% c("Unit", "unit", "UNIT"))) {
              dynamic_title_has_unit <- TRUE
            }

            missing_cols <- setdiff(referenced_cols, names(data))
            if (length(missing_cols) > 0) {
              warning(sprintf(
                "Dynamic title references columns [%s] which are not in the data. Using standard title format.",
                paste(missing_cols, collapse = ", ")
              ))
            } else {
              title_values <- lapply(referenced_cols, function(col_name) {
                unique(data[[col_name]])[1]
              })
              names(title_values) <- referenced_cols
              if (dynamic_title_has_unit && "Unit" %in% referenced_cols) {
                title_values$Unit <- .convert_unit_for_display(title_values$Unit)
              }
              tryCatch({
                plot_title <- glue::glue_data(title_values, title_format$text)
              }, error = function(e) {
                warning(sprintf("Failed to apply dynamic title format: %s. Using standard title format.", e$message))
              })
            }
          }
        }
      }
    } else if (title_format$type == "prefix") {
      sep <- if (!is.null(title_format$sep)) title_format$sep else ": "
      plot_title <- paste0(title_format$text, sep, plot_title)
    } else if (title_format$type == "suffix") {
      sep <- if (!is.null(title_format$sep)) title_format$sep else ": "
      plot_title <- paste0(plot_title, sep, title_format$text)
    } else if (title_format$type == "full") {
      plot_title <- title_format$text
    }
  }

  if (style_config$add_unit_to_title && !is.null(unit_name) && !is.na(unit_name) && unit_name != "" && unit_name != "Value") {
    if (!dynamic_title_has_unit && !grepl(unit_name, plot_title, fixed = TRUE)) {
      converted_unit <- .convert_unit_for_display(unit_name)
      plot_title <- paste0(plot_title, " (", converted_unit, ")")
    }
  }

  if (length(export_parts) > 0) {
    export_name <- paste0("trend_", paste(export_parts, collapse = "_"))
  } else {
    export_name <- "trend_plot"
  }

  return(list(title = plot_title, export_name = export_name))
}


#' @keywords internal
#' @noRd
.calculate_value_axis_limits <- function(data, unit, style_config) {
  # Use scale_limit if provided
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    return(style_config$scale_limit)
  }

  # Calculate appropriate limits based on data
  value_range <- range(data[["Value"]], na.rm = TRUE)
  max_abs_value <- max(abs(value_range), na.rm = TRUE)

  # Different limit calculations based on data and unit
  if (!is.na(unit) && !is.null(unit) && tolower(unit) == "percent") {
    # For percentage values, use symmetric limits
    return(c(-max_abs_value * 1.35, max_abs_value * 1.35))
  } else {
    # For other units, base on value range
    if (all(data[["Value"]] >= 0, na.rm = TRUE)) {
      # All positive values
      return(c(0, value_range[2] * 1.3))
    } else if (all(data[["Value"]] <= 0, na.rm = TRUE)) {
      # All negative values
      return(c(value_range[1] * 1.3, 0))
    } else {
      # Mixed values
      return(c(value_range[1] * 1.3, value_range[2] * 1.3))
    }
  }
}


#' @keywords internal
#' @noRd
.format_y_axis_label <- function(unit, style_config) {
  if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    return(style_config$y_axis_description)
  } else if (!is.na(unit) && !is.null(unit) && tolower(unit) == "percent") {
    return("Percentage (%)")
  } else {
    return(unit)
  }
}


#' @keywords internal
#' @noRd
.convert_unit_for_display <- function(unit) {
  if (is.null(unit) || is.na(unit) || unit == "") return("")

  unit_lower <- tolower(trimws(unit))

  conversions <- c(
    "percent" = "%",
    "percentage" = "%",
    "million usd" = "M USD",
    "million us$" = "M USD",
    "billion usd" = "B USD",
    "billion us$" = "B USD",
    "thousand usd" = "K USD",
    "thousand us$" = "K USD",
    "usd million" = "M USD",
    "usd billion" = "B USD",
    "usd thousand" = "K USD"
  )

  if (unit_lower %in% names(conversions)) {
    return(conversions[unit_lower])
  }

  return(unit)
}


#' @keywords internal
#' @noRd
.apply_axis_scale <- function(limits, style_config, axis = "y") {
  scale_args <- list(
    limits = limits,
    oob = scales::oob_keep,
    expand = ggplot2::expansion(mult = if (axis == "y") style_config$expansion_y_mult else style_config$expansion_x_mult)
  )

  # Add breaks if scale_increment is specified
  if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
    scale_args$breaks <- seq(limits[1], limits[2], by = style_config$scale_increment)
  }

  # Apply appropriate scale function
  if (axis == "y") {
    return(do.call(ggplot2::scale_y_continuous, scale_args))
  } else {
    return(do.call(ggplot2::scale_x_continuous, scale_args))
  }
}


#' @keywords internal
#' @noRd
.apply_filters_trend_plot <- function(data, filter_var) {
  if (is.null(filter_var)) return(data)

  if (is.data.frame(filter_var)) {
    filter_var <- as.list(filter_var)
  } else if (is.vector(filter_var) && !is.list(filter_var)) {
    filter_var <- list(filter_var)
  }

  for (col_name in names(filter_var)) {
    if (col_name %in% colnames(data)) {
      data <- data[data[[col_name]] %in% filter_var[[col_name]], ]
    }
  }

  return(data)
}


#' @keywords internal
#' @noRd
.generate_line_colors <- function(n, palette_name = "default") {
  if (palette_name == "gtap") {
    colors_result <- .create_color_palette(color_tone = "gtap", n_colors = n, palette_type = "qualitative")
    if (!is.null(colors_result)) {
      return(colors_result)
    }
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
    colors_result <- .create_color_palette(color_tone = palette_name, n_colors = n, palette_type = "qualitative")
    if (!is.null(colors_result)) {
      return(colors_result)
    }

    if (n <= 9) {
      return(RColorBrewer::brewer.pal(max(3, n), "Set1")[1:n])
    } else {
      return(grDevices::rainbow(n))
    }
  }
}


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
#' get_trend_style_config()
get_trend_style_config <- function() {
  config <- create_trend_style()

  msg <- "my_trend_style <- create_trend_style(\n"

  msg <- paste0(msg, "\n  # Line and Point settings\n")
  msg <- paste0(msg, "  show_points = ", ifelse(config$show_points, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  point_size = ", config$point_size, ",\n")
  msg <- paste0(msg, "  point_shape = ", config$point_shape, ", # 0-25 or 'circle', 'square', etc.\n")
  msg <- paste0(msg, "  point_alpha = ", config$point_alpha, ",\n\n")

  msg <- paste0(msg, "  line_size = ", config$line_size, ",\n")
  msg <- paste0(msg, "  line_type = \"", config$line_type, "\", # 'solid', 'dashed', 'dotted'\n")
  msg <- paste0(msg, "  line_alpha = ", config$line_alpha, ",\n\n")

  msg <- paste0(msg, "  # Smoothing settings\n")
  msg <- paste0(msg, "  smooth_line = ", ifelse(config$smooth_line, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  smooth_se = ", ifelse(config$smooth_se, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  smooth_span = ", config$smooth_span, ",\n\n")

  msg <- paste0(msg, "  # Color settings\n")
  if (is.null(config$color_palette)) {
    msg <- paste0(msg, "  color_palette = NULL, # 'gtap', 'viridis', 'Set1', or c('#FF0000', '#00FF00')\n")
  } else if (length(config$color_palette) == 1) {
    msg <- paste0(msg, "  color_palette = \"", config$color_palette, "\",\n")
  } else {
    msg <- paste0(msg, "  color_palette = c(\"", paste(config$color_palette, collapse = "\", \""), "\"),\n")
  }
  msg <- paste0(msg, "  legend_title = ", ifelse(is.null(config$legend_title), "NULL", paste0("\"", config$legend_title, "\"")), ",\n\n")

  msg <- paste0(msg, "  # Period axis settings\n")
  if (is.character(config$period_breaks)) {
    msg <- paste0(msg, "  period_breaks = \"", config$period_breaks, "\", # 'all', 'auto', or numeric vector\n\n")
  } else {
    msg <- paste0(msg, "  period_breaks = c(", paste(config$period_breaks, collapse = ", "), "),\n\n")
  }

  msg <- paste0(msg, "  # Vertical reference lines\n")
  msg <- paste0(msg, "  vertical_lines = NULL, # list(list(period=2019, label='Shock 1', color='black', linetype='dashed'))\n")
  msg <- paste0(msg, "  vline_label_size = ", config$vline_label_size, ",\n")
  msg <- paste0(msg, "  vline_label_angle = ", config$vline_label_angle, ",\n")
  msg <- paste0(msg, "  vline_label_vjust = ", config$vline_label_vjust, ",\n\n")

  msg <- paste0(msg, "  # Title settings\n")
  msg <- paste0(msg, "  show_title = ", ifelse(config$show_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  title_size = ", config$title_size, ",\n")
  msg <- paste0(msg, "  add_unit_to_title = ", ifelse(config$add_unit_to_title, "TRUE", "FALSE"), ",\n\n")

  msg <- paste0(msg, "  # Legend settings\n")
  msg <- paste0(msg, "  show_legend = ", ifelse(config$show_legend, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  legend_position = \"", config$legend_position, "\", # 'right', 'left', 'top', 'bottom'\n")
  msg <- paste0(msg, "  legend_text_size = ", config$legend_text_size, ",\n\n")

  msg <- paste0(msg, "  # Grid settings\n")
  msg <- paste0(msg, "  show_grid_major_x = ", ifelse(config$show_grid_major_x, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_grid_major_y = ", ifelse(config$show_grid_major_y, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  grid_color = \"", config$grid_color, "\",\n\n")

  msg <- paste0(msg, "  # Zero line settings\n")
  msg <- paste0(msg, "  show_zero_line = ", ifelse(config$show_zero_line, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  zero_line_type = \"", config$zero_line_type, "\",\n\n")

  msg <- paste0(msg, "  # Font settings\n")
  msg <- paste0(msg, "  all_font_size = ", config$all_font_size, " # Multiplier for all text sizes\n")

  msg <- paste0(msg, ")\n\n")
  msg <- paste0(msg, "# Example usage:\n")
  msg <- paste0(msg, "# trend_plot(data, period_col = 'Period', line_group_by = 'Case', \n")
  msg <- paste0(msg, "#            split_by = c('Variable', 'AREG'), trend_style_config = my_trend_style)\n")

  message(msg)

  return(invisible(NULL))
}


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
#' vline1 <- create_vline(period = 2019, label = "Shock 1", color = "red")
#'
#' vlines <- list(
#'   create_vline(2019, "Shock 1", "black", "dashed"),
#'   create_vline(2023, "Shock 2", "blue", "dotted"),
#'   create_vline(2025, "Shock 3", "darkgreen", "dashed")
#' )
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
