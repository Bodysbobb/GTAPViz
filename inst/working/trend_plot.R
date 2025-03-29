# Trend Plot --------------------------------------------------------------

#' @title Create Time-Series Trend Plots from GTAP Data
#'
#' @description
#' Generates line plots to visualize trends over time across multiple dimensions,
#' with flexible grouping, splitting, and styling options.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `group_by`.
#'   If a data frame, filters `variable_col` based on matching values.
#' @param panel_var Character. Column containing time variable for x-axis (e.g., "Year").
#' @param group_by Character. Column for grouping lines (e.g., "REG" or "Country").
#' @param split_by Character or vector. Column name(s) for data splitting (e.g., "COMM").
#'   Set to NULL for no splitting, suitable for macro-level analysis.
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param invert_pane Logical. If TRUE, creates horizontal trend lines (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per variable (default: FALSE).
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds variable code in parentheses after description (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF.
#' @param export_config List. Export settings like width, height, and file name.
#' @param plot_style_config List. Custom plot styles (see `?get_plot_style_config`).
#' @param line_size Numeric. Thickness of trend lines (default: 1.5).
#' @param add_points Logical. If TRUE, adds point markers at each data point (default: TRUE).
#' @param point_size Numeric. Size of point markers (default: 3).
#' @param add_smooth Logical. If TRUE, adds smoothed trend lines (default: FALSE).
#' @param smooth_method Character. Method for smoothing ("loess", "lm", etc.) (default: "loess").
#' @param vertical_lines Named list. Specifications for vertical reference lines, each containing:
#'   - position: Numeric. X-axis position for the vertical line
#'   - color: Character. Color of the line
#'   - linetype: Character. Line type (e.g., "dashed", "solid")
#'   - size: Numeric. Line thickness
#'   - text: Character. Optional label for the line
#'   - text_size: Numeric. Size of the text label
#'   - text_angle: Numeric. Angle of the text label (default: 90)
#'   - text_color: Character. Color of the text label (defaults to line color)
#'   - x_offset: Numeric. Horizontal offset for the text label
#'   - y_offset: Numeric. Vertical position for the text label
#' @param add_average_line Logical. If TRUE, adds a line showing average values (default: FALSE).
#' @param avg_line_color Character. Color for the average line (default: "red").
#' @param avg_line_size Numeric. Thickness for the average line (default: 1).
#' @param avg_line_type Character. Line type for the average line (default: "dashed").
#' @param avg_line_label Character. Label for the average line (default: "Average").
#'
#' @return A list of ggplot2 objects (invisibly) containing the generated plots.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Basic trend plot with vertical reference lines
#' trend_plot(
#'   data = time_series_data,
#'   panel_var = "Year",
#'   group_by = "Region",
#'   vertical_lines = list(
#'     recession = list(
#'       position = 2008,
#'       color = "red",
#'       linetype = "dashed",
#'       text = "2008 Recession",
#'       text_angle = 90
#'     ),
#'     policy_change = list(
#'       position = 2015,
#'       color = "blue",
#'       text = "Policy Change"
#'     )
#'   ),
#'   add_average_line = TRUE
#' )
#' }
trend_plot <- function(data, filter_var = NULL,
                       panel_var,
                       group_by,
                       split_by = NULL,
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       invert_pane = FALSE,
                       separate_figure = FALSE,
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       output_path = NULL,
                       export_picture = TRUE,
                       export_as_pdf = FALSE,
                       export_config = NULL,
                       plot_style_config = NULL,
                       line_size = 1.5,
                       add_points = TRUE,
                       point_size = 3,
                       add_smooth = FALSE,
                       smooth_method = "loess",
                       vertical_lines = NULL,
                       add_average_line = FALSE,
                       avg_line_color = "red",
                       avg_line_size = 1,
                       avg_line_type = "dashed",
                       avg_line_label = "Average") {

  .validate_column_params(data, list(panel_var = panel_var, group_by = group_by, split_by = split_by,
                                     variable_col = variable_col, unit_col = unit_col, desc_col = desc_col))
  data <- .prepare_data_source(data, panel_var, variable_col = variable_col)
  unit_check_result <- .check_unit_column(data, unit_col)
  data <- unit_check_result$data
  unit_col <- unit_check_result$unit_col
  split_by_result <- .process_split_by(data, split_by)
  data <- split_by_result$data
  is_macro_mode <- split_by_result$is_macro_mode
  split_by <- split_by_result$split_by

  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      data <- data[data[[group_by]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    data <- .format_variable_names(data, variable_col = variable_col, desc_col = desc_col,
                                   var_name_by_description = var_name_by_description, add_var_info = add_var_info)
  }

  panel_layout <- list(rows = 1, cols = 1)
  dimensions <- if (!is.null(export_config) && !is.null(export_config$width) && !is.null(export_config$height)) {
    list(width = export_config$width, height = export_config$height)
  } else {
    .calculate_plot_dimensions(data, panel_layout)
  }

  # Add line_width parameter to style config if provided
  base_style_config <- list(
    panel_rows = panel_layout$rows,
    panel_cols = panel_layout$cols,
    all_font_size = if(!is.null(plot_style_config) && !is.null(plot_style_config$all_font_size))
      plot_style_config$all_font_size else 1,
    line_width = if(!is.null(plot_style_config) && !is.null(plot_style_config$line_width))
      plot_style_config$line_width else line_size
  )

  style_config <- .calculate_plot_style_config(
    config = if (!is.null(plot_style_config)) modifyList(base_style_config, plot_style_config) else base_style_config,
    plot_type = "default"
  )

  # Override line_size if line_width is specified in style_config
  if (!is.null(style_config$line_width)) {
    line_size <- style_config$line_width
  }

  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    if (is_macro_mode) {
      if (separate_figure) {
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]
          title_info <- .handle_plot_title_and_export(var_name = var_name, plot_type = "trend",
                                                      is_macro_mode = TRUE, variable_col = variable_col,
                                                      unit_name = unit_name, style_config = style_config,
                                                      data = var_data)

          p <- .create_single_trend_plot(data = var_data, panel_var = panel_var, group_by = group_by,
                                         plot_title = title_info$title, unit = unit_name,
                                         invert_pane = invert_pane, plot_style_config = style_config,
                                         line_size = line_size, add_points = add_points,
                                         point_size = point_size, add_smooth = add_smooth,
                                         smooth_method = smooth_method, vertical_lines = vertical_lines,
                                         add_average_line = add_average_line,
                                         avg_line_color = avg_line_color,
                                         avg_line_size = avg_line_size,
                                         avg_line_type = avg_line_type,
                                         avg_line_label = avg_line_label)

          plot_list[[title_info$export_name]] <- p
        }
      } else {
        title_info <- .handle_plot_title_and_export(var_name = "Trend Analysis", plot_type = "trend",
                                                    is_macro_mode = TRUE, unit_name = unit_name,
                                                    style_config = style_config, data = unit_data)

        p <- .create_single_trend_plot(data = unit_data, panel_var = panel_var, group_by = group_by,
                                       plot_title = title_info$title, unit = unit_name,
                                       invert_pane = invert_pane, plot_style_config = style_config,
                                       line_size = line_size, add_points = add_points,
                                       point_size = point_size, add_smooth = add_smooth,
                                       smooth_method = smooth_method, vertical_lines = vertical_lines,
                                       add_average_line = add_average_line,
                                       avg_line_color = avg_line_color,
                                       avg_line_size = avg_line_size,
                                       avg_line_type = avg_line_type,
                                       avg_line_label = avg_line_label)

        plot_list[[title_info$export_name]] <- p
      }
    } else {
      if (length(split_by) > 1) {
        unit_data$split_display <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
        separate_values <- unique(unit_data$split_display)
        split_col <- "split_display"
      } else {
        separate_values <- unique(unit_data[[split_by]])
        split_col <- split_by
      }

      for (sep_value in separate_values) {
        filtered_data <- if (split_col == "split_display") {
          unit_data[unit_data$split_display == sep_value, ]
        } else {
          unit_data[unit_data[[split_col]] == sep_value, ]
        }

        if (separate_figure) {
          var_combinations <- unique(filtered_data[[variable_col]])

          for (var_name in var_combinations) {
            var_data <- filtered_data[filtered_data[[variable_col]] == var_name, ]
            title_info <- .handle_plot_title_and_export(var_name = var_name, sep_value = sep_value,
                                                        plot_type = "trend", is_macro_mode = FALSE,
                                                        split_by = split_by, variable_col = variable_col,
                                                        unit_name = unit_name, style_config = style_config,
                                                        data = var_data)

            p <- .create_single_trend_plot(data = var_data, panel_var = panel_var, group_by = group_by,
                                           plot_title = title_info$title, unit = unit_name,
                                           invert_pane = invert_pane, plot_style_config = style_config,
                                           line_size = line_size, add_points = add_points,
                                           point_size = point_size, add_smooth = add_smooth,
                                           smooth_method = smooth_method, vertical_lines = vertical_lines,
                                           add_average_line = add_average_line,
                                           avg_line_color = avg_line_color,
                                           avg_line_size = avg_line_size,
                                           avg_line_type = avg_line_type,
                                           avg_line_label = avg_line_label)

            plot_list[[title_info$export_name]] <- p
          }
        } else {
          title_info <- .handle_plot_title_and_export(sep_value = sep_value, plot_type = "trend",
                                                      is_macro_mode = FALSE, split_by = split_by,
                                                      variable_col = variable_col, unit_name = unit_name,
                                                      style_config = style_config, data = filtered_data)

          p <- .create_single_trend_plot(data = filtered_data, panel_var = panel_var, group_by = group_by,
                                         plot_title = title_info$title, unit = unit_name,
                                         invert_pane = invert_pane, plot_style_config = style_config,
                                         line_size = line_size, add_points = add_points,
                                         point_size = point_size, add_smooth = add_smooth,
                                         smooth_method = smooth_method, vertical_lines = vertical_lines,
                                         add_average_line = add_average_line,
                                         avg_line_color = avg_line_color,
                                         avg_line_size = avg_line_size,
                                         avg_line_type = avg_line_type,
                                         avg_line_label = avg_line_label)

          plot_list[[title_info$export_name]] <- p
        }
      }
    }
  }

  if (is.null(export_config) || is.null(export_config$file_name)) {
    export_config <- .coalesce(export_config, list())
    export_config$file_name <- "trend_plots"
  }

  export_config$width <- dimensions$width
  export_config$height <- dimensions$height

  .export_plot_output(plots = plot_list, output_path = output_path,
                      export_picture = export_picture, export_as_pdf = export_as_pdf,
                      export_config = export_config, data = data, panel_layout = panel_layout)

  return(invisible(plot_list))
}

#' @title Create Single Trend Plot (Internal)
#'
#' @description
#' Create a single trend line plot from GTAP data showing values over time.
#'
#' @param data A data frame containing plotting data.
#' @param panel_var Character. Column used for time axis.
#' @param group_by Character. Column used to define different lines.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param plot_style_config List. Custom plot styling configuration.
#' @param line_size Numeric. Thickness of trend lines. Default is 1.5.
#' @param add_points Logical. Whether to add point markers. Default is TRUE.
#' @param point_size Numeric. Size of point markers. Default is 3.
#' @param add_smooth Logical. Whether to add smoothed trend lines. Default is FALSE.
#' @param smooth_method Character. Method for smoothing. Default is "loess".
#' @param vertical_lines Named list. Specifications for vertical reference lines.
#' @param add_average_line Logical. Whether to add average trend line. Default is FALSE.
#' @param avg_line_color Character. Color for average line. Default is "red".
#' @param avg_line_size Numeric. Line thickness for average line. Default is 1.
#' @param avg_line_type Character. Line type for average line. Default is "dashed".
#' @param avg_line_label Character. Label for average line. Default is "Average".
#'
#' @return A ggplot2 object representing the trend plot.
#'
#' @keywords internal
#' @noRd
.create_single_trend_plot <- function(data, panel_var, group_by,
                                      plot_title, unit,
                                      invert_pane = FALSE,
                                      plot_style_config = NULL,
                                      line_size = 1.5,
                                      add_points = TRUE,
                                      point_size = 3,
                                      add_smooth = FALSE,
                                      smooth_method = "loess") {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # Ensure panel_var is numeric or correctly ordered factor for time series
  if (is.character(data[[panel_var]])) {
    data[[panel_var]] <- factor(data[[panel_var]], levels = sort(unique(data[[panel_var]])))
  }

  # GENERATE COLORS IF PROVIDED
  if (!is.null(style_config$color_tone)) {
    palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
    color_palette <- .generate_comparison_colors(data, style_config$color_tone, group_by, palette_type)
  } else {
    color_palette <- NULL
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

  # x-axis label uses panel_var if no description provided
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    panel_var
  }

  # CALCULATE Y-AXIS LIMITS - Use scale_limit if provided, otherwise calculate automatically
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limits <- style_config$scale_limit
  } else {
    value_range <- range(data$Value, na.rm = TRUE)
    y_range <- diff(value_range)

    # Add padding to ensure no clipping
    padding <- 0.1 * y_range
    y_limits <- c(value_range[1] - padding, value_range[2] + padding)
  }

  # CREATE THE BASIC PLOT
  if (invert_pane) {
    # For inverted coordinates (x and y swapped)
    p <- ggplot2::ggplot(data, ggplot2::aes(
      y = .data[[panel_var]],
      x = .data[["Value"]],
      color = .data[[group_by]],
      group = .data[[group_by]])) +
      ggplot2::geom_line(linewidth = line_size)

    if (add_points) {
      p <- p + ggplot2::geom_point(size = point_size)
    }

    if (add_smooth) {
      p <- p + ggplot2::geom_smooth(method = smooth_method, se = FALSE,
                                    linetype = "dashed", linewidth = 0.7)
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    scale_args <- list(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limits[1], y_limits[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_x_continuous, scale_args)
  } else {
    # For normal orientation (x is time, y is value)
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x = .data[[panel_var]],
      y = .data[["Value"]],
      color = .data[[group_by]],
      group = .data[[group_by]])) +
      ggplot2::geom_line(linewidth = line_size)

    if (add_points) {
      p <- p + ggplot2::geom_point(size = point_size)
    }

    if (add_smooth) {
      p <- p + ggplot2::geom_smooth(method = smooth_method, se = FALSE,
                                    linetype = "dashed", linewidth = 0.7)
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    scale_args <- list(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limits[1], y_limits[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  # APPLY COLORS IF PROVIDED
  if (!is.null(color_palette)) {
    p <- p + ggplot2::scale_color_manual(values = color_palette)
  }

  # APPLY THEME STYLING
  p <- p + ggplot2::theme_minimal()

  # Set up legend before applying style config
  if (style_config$show_legend) {
    p <- p + ggplot2::theme(
      legend.position = style_config$legend_position,
      legend.title = if (style_config$show_legend_title) {
        ggplot2::element_text(face = style_config$legend_title_face)
      } else {
        ggplot2::element_blank()
      },
      legend.text = ggplot2::element_text(
        face = style_config$legend_text_face,
        size = style_config$legend_text_size
      )
    )
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal trend (x is Value axis and y is Time axis)
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only time axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical trend (x is Time axis and y is Value axis)
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only time axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  # Apply style config LAST
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

# Function to add vertical reference lines to trend plots
#' @noRd
.add_vertical_trend_lines <- function(p,
                                      vertical_lines = NULL,
                                      y_limits = NULL,
                                      invert_pane = FALSE) {

  if (is.null(vertical_lines)) {
    return(p)
  }

  # Ensure vertical_lines is a list
  if (!is.list(vertical_lines) || is.null(names(vertical_lines))) {
    stop("vertical_lines must be a named list of parameters")
  }

  required_fields <- c("position", "color")

  for (line_name in names(vertical_lines)) {
    line_params <- vertical_lines[[line_name]]

    # Check for required fields
    missing_fields <- setdiff(required_fields, names(line_params))
    if (length(missing_fields) > 0) {
      warning(paste("Skipping vertical line '", line_name,
                    "': missing required fields: ",
                    paste(missing_fields, collapse = ", ")))
      next
    }

    position <- line_params$position
    color <- line_params$color
    linetype <- line_params$linetype %||% "dashed"
    size <- line_params$size %||% 0.5

    # Add text if specified
    if (!is.null(line_params$text)) {
      text <- line_params$text
      text_size <- line_params$text_size %||% 3
      text_angle <- line_params$text_angle %||% 90
      text_color <- line_params$text_color %||% color
      text_vjust <- line_params$text_vjust %||% 0
      text_hjust <- line_params$text_hjust %||% 0

      # Calculate text position
      y_offset <- if (!is.null(line_params$y_offset)) {
        line_params$y_offset
      } else if (!is.null(y_limits)) {
        # Default offset as 5% of y-axis range
        y_range <- y_limits[2] - y_limits[1]
        y_limits[1] + y_range * 0.05
      } else {
        NULL
      }

      x_offset <- line_params$x_offset %||% 0.01

      if (invert_pane) {
        # For horizontal trend (vertical reference lines on y-axis)
        p <- p + ggplot2::geom_hline(
          yintercept = position,
          linetype = linetype,
          color = color,
          size = size
        )

        if (!is.null(y_offset)) {
          p <- p + ggplot2::annotate(
            "text",
            y = position + x_offset,  # Swap x and y offsets for inverted pane
            x = y_offset,
            label = text,
            color = text_color,
            size = text_size,
            vjust = text_vjust,
            hjust = text_hjust,
            angle = if (text_angle == 90) 0 else text_angle
          )
        }
      } else {
        # For vertical trend (vertical reference lines on x-axis)
        p <- p + ggplot2::geom_vline(
          xintercept = position,
          linetype = linetype,
          color = color,
          size = size
        )

        if (!is.null(y_offset)) {
          p <- p + ggplot2::annotate(
            "text",
            x = position + x_offset,
            y = y_offset,
            label = text,
            color = text_color,
            size = text_size,
            vjust = text_vjust,
            hjust = text_hjust,
            angle = text_angle
          )
        }
      }
    } else {
      # Just add the line without text
      if (invert_pane) {
        p <- p + ggplot2::geom_hline(
          yintercept = position,
          linetype = linetype,
          color = color,
          size = size
        )
      } else {
        p <- p + ggplot2::geom_vline(
          xintercept = position,
          linetype = linetype,
          color = color,
          size = size
        )
      }
    }
  }

  return(p)
}

# Function to calculate and add average trend line
#' @noRd
.add_average_trend_line <- function(p, data, panel_var, group_by,
                                    avg_line_color = "red",
                                    avg_line_size = 1,
                                    avg_line_type = "dashed",
                                    avg_line_label = "Average",
                                    invert_pane = FALSE) {
  # Calculate average values per time period
  avg_data <- stats::aggregate(Value ~ panel_var, data = data, FUN = mean)

  # Add the average line based on orientation
  if (invert_pane) {
    p <- p + ggplot2::geom_path(
      data = avg_data,
      mapping = ggplot2::aes_string(y = panel_var, x = "Value"),
      color = avg_line_color,
      size = avg_line_size,
      linetype = avg_line_type
    )

    # Add label if there are enough points
    if (nrow(avg_data) > 1 && !is.null(avg_line_label)) {
      last_point <- avg_data[nrow(avg_data), ]
      p <- p + ggplot2::annotate(
        "text",
        y = last_point[[panel_var]],
        x = last_point$Value,
        label = avg_line_label,
        color = avg_line_color,
        hjust = -0.2,
        vjust = 0.5
      )
    }
  } else {
    p <- p + ggplot2::geom_path(
      data = avg_data,
      mapping = ggplot2::aes_string(x = panel_var, y = "Value"),
      color = avg_line_color,
      size = avg_line_size,
      linetype = avg_line_type
    )

    # Add label if there are enough points
    if (nrow(avg_data) > 1 && !is.null(avg_line_label)) {
      last_point <- avg_data[nrow(avg_data), ]
      p <- p + ggplot2::annotate(
        "text",
        x = last_point[[panel_var]],
        y = last_point$Value,
        label = avg_line_label,
        color = avg_line_color,
        hjust = -0.2,
        vjust = 0.5
      )
    }
  }

  return(p)
}
