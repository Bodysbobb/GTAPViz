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
#' @param split_by Character or vector. Column name(s) to generate separate plots for each unique value (e.g., "COMM", "REG", "Variable").
#' NULL creates a single aggregated plot, appropriate for macro-level analysis.
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned in R.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF; if FALSE, skips PDF (default: FALSE).
#' @param export_config List. Export settings, including:
#' \itemize{
#'   \item `width`: Output file width (in inches).
#'   \item `height`: Output file height (in inches).
#'   \item Additional settings—see `?get_export_config`.
#' }
#' @param plot_style_config List. Custom plot styles—see `?get_plot_style_config`.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#' \donttest{
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#'
#' # GTAP Macro Variables from 2 .sl4 Files named (EXP1, EXP2)
#' # Note: No need to add .sl4 to the experiment name
#' gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
#'                             input_path = input_path, subtotal_level = FALSE,
#'                             process_sl4_vars = NULL, process_har_vars = NULL,
#'                             mapping_info = "GTAPv7", plot_data = TRUE)
#'
#' # Basic usage with data frame
#' p1 <- comparison_plot(
#'   data = sl4.plot.data[["1D"]][["Region"]],
#'   x_axis_from = "Region",
#'   panel_var = "Experiment",
#'   filter_var = c("qgdp", "EV"),
#'   output_path = tempdir(),
#'   export_picture = FALSE,
#'   export_as_pdf = FALSE,
#' )
#'
#' # Split by commodity with custom styling and export options
#' p2 <- comparison_plot(
#'   data = sl4.plot.data[["1D"]][["Region"]],
#'   x_axis_from = "Region",
#'   split_by = "Variable",
#'   panel_var = "Experiment",
#'   filter_var = c("qgdp", "EV"),
#'   var_name_by_description = TRUE,
#'   output_path = tempdir(),
#'   export_picture = FALSE,
#'   export_as_pdf = FALSE,
#'   export_config = list(
#'     file_name = "commodity_impacts",
#'     width = 12,
#'     height = 8
#'   ),
#'   plot_style_config = list(
#'     color_tone = "economic",
#'     title_size = 16,
#'     show_grid_major_y = TRUE
#'   )
#' )
#' }
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
                            output_path = NULL,
                            export_picture = TRUE,
                            export_as_pdf = FALSE,
                            export_config = NULL,
                            plot_style_config = NULL) {

  # Validate the column parameters
  .validate_column_params(data, list(
    x_axis_from = x_axis_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col
  ))

  # PREPARE DATA SOURCE
  data <- .prepare_data_source(data, x_axis_from, variable_col = variable_col)

  # CHECK FOR UNIT COLUMN
  unit_check_result <- .check_unit_column(data, unit_col)
  data <- unit_check_result$data
  unit_col <- unit_check_result$unit_col

  # PROCESS SPLIT_BY PARAMETER
  split_by_result <- .process_split_by(data, split_by)
  data <- split_by_result$data
  is_macro_mode <- split_by_result$is_macro_mode
  split_by <- split_by_result$split_by

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      # Corrected to filter by variable_col instead of x_axis_from
      data <- data[data[[variable_col]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    data <- .format_variable_names(
      data,
      variable_col = variable_col,
      desc_col = desc_col,
      var_name_by_description = var_name_by_description,
      add_var_info = add_var_info
    )
  }

  # Ensure panel_var column maintains original order for facets
  if (panel_var %in% names(data)) {
    panel_levels <- unique(data[[panel_var]])
    data[[panel_var]] <- factor(data[[panel_var]], levels = panel_levels)
  }

  # Calculate panel layout before style configuration
  panel_layout <- .calculate_panel_layout(data,
                                          panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
                                          panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
                                          panel_var = panel_var)

  # Check if custom dimensions are provided
  dimensions <- if (!is.null(export_config) &&
                    !is.null(export_config$width) &&
                    !is.null(export_config$height)) {
    list(
      width = export_config$width,
      height = export_config$height
    )
  } else {
    .calculate_plot_dimensions(data, panel_layout)
  }

  # Prepare style configuration
  base_style_config <- list(
    panel_rows = panel_layout$rows,
    panel_cols = panel_layout$cols,
    all_font_size = plot_style_config$all_font_size
  )

  # Merge user config if provided
  style_config <- .calculate_plot_style_config(
    config = if (!is.null(plot_style_config))
      modifyList(base_style_config, plot_style_config)
    else
      base_style_config,
    plot_type = "default"
  )

  # PROCESS BY UNIT GROUPS
  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    if (is_macro_mode) {
      if (separate_figure) {
        panel_values <- unique(unit_data[[panel_var]])

        for (panel_val in panel_values) {
          panel_data <- unit_data[unit_data[[panel_var]] == panel_val, ]

          title_info <- .handle_plot_title_and_export(
            var_name = "Global Economic Impacts",
            sep_value = panel_val,
            plot_type = "comparison",
            is_macro_mode = TRUE,
            variable_col = variable_col,
            unit_name = unit_name,
            style_config = style_config,
            data = panel_data,
            separate_figure = separate_figure,
            panel_val = panel_val
          )

          p <- .create_single_comparison_plot(
            data = panel_data,
            x_axis_from = x_axis_from,
            plot_title = title_info$title,
            unit = unit_name,
            panel_rows = style_config$panel_rows,
            panel_cols = style_config$panel_cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            plot_style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
        }
      } else {
        title_info <- .handle_plot_title_and_export(
          var_name = "Global Economic Impacts",
          plot_type = "comparison",
          is_macro_mode = TRUE,
          unit_name = unit_name,
          style_config = style_config,
          data = unit_data
        )

        p <- .create_single_comparison_plot(
          data = unit_data,
          x_axis_from = x_axis_from,
          plot_title = title_info$title,
          unit = unit_name,
          panel_rows = style_config$panel_rows,
          panel_cols = style_config$panel_cols,
          panel_var = panel_var,
          invert_pane = invert_pane,
          plot_style_config = style_config
        )

        plot_list[[title_info$export_name]] <- p
      }
    } else {
      separate_values <- if (length(split_by) > 1) {
        unit_data$split_display <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
        unique(unit_data$split_display)
      } else {
        unique(unit_data[[split_by]])
      }

      split_col <- if (length(split_by) > 1) "split_display" else split_by

      for (sep_value in separate_values) {
        filtered_data <- if (split_col == "split_display") {
          unit_data[unit_data$split_display == sep_value, ]
        } else {
          unit_data[unit_data[[split_col]] == sep_value, ]
        }

        if (separate_figure) {
          panel_values <- unique(filtered_data[[panel_var]])

          for (panel_val in panel_values) {
            panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]

            title_info <- .handle_plot_title_and_export(
              sep_value = sep_value,
              x_value = panel_val,
              plot_type = "comparison",
              is_macro_mode = FALSE,
              split_by = split_by,
              x_axis_from = x_axis_from,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = panel_data,
              separate_figure = separate_figure,
              panel_val = panel_val
            )

            p <- .create_single_comparison_plot(
              data = panel_data,
              x_axis_from = x_axis_from,
              plot_title = title_info$title,
              unit = unit_name,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        } else {
          title_info <- .handle_plot_title_and_export(
            sep_value = sep_value,
            plot_type = "comparison",
            is_macro_mode = FALSE,
            split_by = split_by,
            x_axis_from = x_axis_from,
            variable_col = variable_col,
            unit_name = unit_name,
            style_config = style_config,
            data = filtered_data
          )

          p <- .create_single_comparison_plot(
            data = filtered_data,
            x_axis_from = x_axis_from,
            plot_title = title_info$title,
            unit = unit_name,
            panel_rows = style_config$panel_rows,
            panel_cols = style_config$panel_cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            plot_style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
        }
      }
    }
  }

  # SET DEFAULT FILE_NAME IN EXPORT_CONFIG
  if (is.null(export_config) || is.null(export_config$file_name)) {
    export_config <- .coalesce(export_config, list())
    export_config$file_name <- "comparison_plots"
  }

  # Add calculated dimensions to export_config
  export_config$width <- dimensions$width
  export_config$height <- dimensions$height

  # EXPORT PLOTS
  .export_plot_output(
    plots = plot_list,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    data = data,
    panel_layout = panel_layout
  )

  # RETURN SINGLE PLOT OR LIST OF PLOTS
  return(invisible(plot_list))
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
#' @noRd
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
    .calculate_plot_style_config(NULL, "default")
  }

  # SET UP VARIABLES FOR PLOTTING
  x_var <- x_axis_from
  facet_var <- panel_var

  # Sort the Data of X_AXIS_FROM
  data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))

  n_panels <- length(unique(data[[facet_var]]))

  # CALCULATE Y-AXIS LIMITS - Use scale_limit if provided, otherwise calculate automatically
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limits <- style_config$scale_limit
  } else {
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
    palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
    color_palette <- .generate_comparison_colors(data, style_config$color_tone, x_var, palette_type)
  }

  # GET BAR STYLING FROM CONFIG
  bar_width <- style_config$bar_width
  bar_spacing <- style_config$bar_spacing

  # CREATE THE BASIC PLOT
  if (invert_pane) {
    # For horizontal bars (flipped coordinates)
    p <- ggplot2::ggplot(data, ggplot2::aes(
      y = .data[[x_var]],
      x = .data[["Value"]],
      fill = .data[[x_var]])) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = bar_spacing),
                        width = bar_width)

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = label_position,
                     label = sprintf(paste0("%.", decimal_places, "f"), .data[["Value"]])),
        position = ggplot2::position_dodge(width = bar_spacing),
        size = value_size,
        color = "black"
      )
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

    # ADD ZERO LINE IF CONFIGURED (VERTICAL LINE FOR HORIZONTAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    }
  } else {
    # For vertical bars (normal orientation)
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x = .data[[x_var]],
      y = .data[["Value"]],
      fill = .data[[x_var]])) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = bar_spacing),
                        width = bar_width)

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size

      p <- p + ggplot2::geom_text(
        ggplot2::aes(y = label_position,
                     label = sprintf(paste0("%.", decimal_places, "f"), .data[["Value"]])),
        position = ggplot2::position_dodge(width = bar_spacing),
        size = value_size,
        color = "black"
      )
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

    # ADD ZERO LINE IF CONFIGURED (HORIZONTAL LINE FOR VERTICAL BARS)
    if (style_config$show_zero_line) {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    }
  }

  # APPLY COLORS IF PROVIDED
  if (!is.null(style_config$color_tone)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  # ADD FACET WRAP IF WE HAVE MULTIPLE PANELS
  if (n_panels > 1) {
    facet_args <- list(
      as.formula(paste("~", facet_var)),
      scales = if (style_config$show_axis_titles_on_all_facets) "free" else "fixed"
    )
    if (!is.null(panel_rows)) {
      facet_args$nrow <- panel_rows
    }
    if (!is.null(panel_cols)) {
      facet_args$ncol <- panel_cols
    }
    p <- p + do.call(ggplot2::facet_wrap, facet_args)
  }

  # APPLY THEME STYLING
  p <- p + ggplot2::theme_minimal()

  # Handle axis labels BEFORE applying style config
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
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


# Detail Plot -------------------------------------------------------------

#' @title Create Comprehensive Bar Charts from HAR and SL4 Data
#'
#' @description
#' Generates detailed bar charts to visualize the distribution of impacts across multiple dimensions.
#' The function supports top impact filtering, color coding, and flexible visualization settings.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param split_by Character or vector. Column name(s) to generate separate plots for each unique value (e.g., "COMM", "REG", "Variable").
#' NULL creates a single aggregated plot, appropriate for macro-level analysis.
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param top_impact Numeric or NULL. If specified, shows only the top N impactful values; NULL shows all values.
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned in R.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF; if FALSE, skips PDF (default: FALSE).
#' @param export_config List. Export settings, including:
#' \itemize{
#'   \item `width`: Output file width (in inches).
#'   \item `height`: Output file height (in inches).
#'   \item Additional settings—see `?get_export_config`.
#' }
#' @param plot_style_config List. Custom plot styles—see `?get_plot_style_config`.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{comparison_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#'
#' \donttest{
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#'
#' # GTAP Macro Variables from 2 .sl4 Files named (EXP1, EXP2)
#' # Note: No need to add .sl4 to the experiment name
#' gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
#'                             input_path = input_path, subtotal_level = FALSE,
#'                             process_sl4_vars = NULL, process_har_vars = NULL,
#'                             mapping_info = "GTAPv7", plot_data = TRUE)
#' # Basic usage with data frame
#' detail_plot(sl4.plot.data[["2D"]],
#'             x_axis_from = "Sector",
#'             split_by = "Region",
#'             filter_var = "qo",
#'
#'             top_impact = NULL,
#'             var_name_by_description = TRUE,
#'
#'             invert_pane = TRUE,
#'             separate_figure = FALSE,
#'
#'             export_config = list(
#'               width = 45,
#'               height = 20
#'             ),
#'
#'             export_picture = FALSE,
#'             export_as_pdf = FALSE,
#'             output_path = tempdir(),
#'
#'             plot_style_config = list(
#'               positive_color = "#2E8B57",
#'               negative_color = "#CD5C5C",
#'               panel_rows = 1,
#'               panel_cols = NULL,
#'               show_axis_titles_on_all_facets = FALSE,
#'               y_axis_text_size = 25,
#'               bar_width = 0.6,
#'               all_font_size = 1.1
#'             ))
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
                        top_impact = NULL,
                        invert_pane = FALSE,
                        separate_figure = FALSE,
                        output_path = NULL,
                        export_picture = TRUE,
                        export_as_pdf = FALSE,
                        export_config = NULL,
                        plot_style_config = NULL) {

  # Validate the column parameters
  .validate_column_params(data, list(
    x_axis_from = x_axis_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col
  ))

  # PREPARE DATA SOURCE - Using the same approach as comparison_plot
  data <- .prepare_data_source(data, x_axis_from, variable_col = variable_col)

  # CHECK FOR REQUIRED COLUMNS
  unit_check_result <- .check_unit_column(data, unit_col)
  data <- unit_check_result$data
  unit_col <- unit_check_result$unit_col

  if (!("Value" %in% names(data))) {
    stop("Missing 'Value' column in data frame.")
  }

  # PROCESS SPLIT_BY PARAMETER
  split_by_result <- .process_split_by(data, split_by)
  data <- split_by_result$data
  is_macro_mode <- split_by_result$is_macro_mode
  split_by <- split_by_result$split_by

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      data <- data[data[[variable_col]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
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

    data <- .filter_top_impact_values_detail(
      data = data,
      top_impact = top_impact,
      group_col = top_impact_filter_col,
      panel_var = panel_var,
      x_axis_from = x_axis_from,
      variable_col = variable_col,
      unit_col = unit_col
    )

    if ("._split_group_" %in% names(data)) {
      data$._split_group_ <- NULL
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    data <- .format_variable_names(
      data,
      variable_col = variable_col,
      desc_col = desc_col,
      var_name_by_description = var_name_by_description,
      add_var_info = add_var_info
    )
  }

  # Ensure panel_var column maintains original order for facets
  if (panel_var %in% names(data)) {
    panel_levels <- unique(data[[panel_var]])
    data[[panel_var]] <- factor(data[[panel_var]], levels = panel_levels)
  }

  # Calculate panel layout
  panel_layout <- .calculate_panel_layout(data,
                                          panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
                                          panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
                                          panel_var = panel_var)

  # Check if custom dimensions are provided
  dimensions <- if (!is.null(export_config) &&
                    !is.null(export_config$width) &&
                    !is.null(export_config$height)) {
    list(
      width = export_config$width,
      height = export_config$height
    )
  } else {
    .calculate_plot_dimensions(data, panel_layout)
  }

  # Prepare base style configuration
  base_style_config <- list(
    panel_rows = panel_layout$rows,
    panel_cols = panel_layout$cols,
    all_font_size = if(!is.null(plot_style_config$all_font_size)) plot_style_config$all_font_size else 1
  )

  # Calculate plot style configuration
  style_config <- .calculate_plot_style_config(
    config = if (!is.null(plot_style_config))
      modifyList(base_style_config, plot_style_config)
    else
      base_style_config,
    plot_type = "default"
  )

  # PROCESS BY UNIT GROUPS
  unit_groups <- split(data, data[[unit_col]])
  plot_list <- list()

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    # HANDLE MACRO MODE
    if (is_macro_mode) {
      if (separate_figure) {
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]
          panel_values <- unique(var_data[[panel_var]])

          for (panel_val in panel_values) {
            panel_data <- var_data[var_data[[panel_var]] == panel_val, ]

            title_info <- .handle_plot_title_and_export(
              var_name = var_name,
              sep_value = panel_val,
              plot_type = "detail",
              is_macro_mode = is_macro_mode,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = panel_data,
              separate_figure = separate_figure,
              panel_val = panel_val
            )

            p <- .create_single_detail_plot(
              data = panel_data,
              x_axis_from = x_axis_from,
              plot_title = title_info$title,
              unit = unit_name,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      } else {
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]

          title_info <- .handle_plot_title_and_export(
            var_name = var_name,
            plot_type = "detail",
            is_macro_mode = is_macro_mode,
            variable_col = variable_col,
            unit_name = unit_name,
            style_config = style_config,
            data = var_data
          )

          p <- .create_single_detail_plot(
            data = var_data,
            x_axis_from = x_axis_from,
            plot_title = title_info$title,
            unit = unit_name,
            panel_rows = style_config$panel_rows,
            panel_cols = style_config$panel_cols,
            panel_var = panel_var,
            invert_pane = invert_pane,
            top_impact = top_impact,
            plot_style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
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
        filtered_data <- if (split_col == "split_display") {
          unit_data[unit_data$split_display == sep_value, ]
        } else {
          unit_data[unit_data[[split_col]] == sep_value, ]
        }

        var_combinations <- unique(filtered_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- filtered_data[filtered_data[[variable_col]] == var_name, ]

          if (separate_figure) {
            panel_values <- unique(var_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_data <- var_data[var_data[[panel_var]] == panel_val, ]

              title_info <- .handle_plot_title_and_export(
                var_name = var_name,
                sep_value = sep_value,
                x_value = panel_val,
                plot_type = "detail",
                is_macro_mode = is_macro_mode,
                split_by = split_by,
                x_axis_from = x_axis_from,
                variable_col = variable_col,
                unit_name = unit_name,
                style_config = style_config,
                data = panel_data,
                separate_figure = separate_figure,
                panel_val = panel_val
              )

              p <- .create_single_detail_plot(
                data = panel_data,
                x_axis_from = x_axis_from,
                plot_title = title_info$title,
                unit = unit_name,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                invert_pane = invert_pane,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
            title_info <- .handle_plot_title_and_export(
              var_name = var_name,
              sep_value = sep_value,
              plot_type = "detail",
              is_macro_mode = is_macro_mode,
              split_by = split_by,
              x_axis_from = x_axis_from,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = var_data
            )

            p <- .create_single_detail_plot(
              data = var_data,
              x_axis_from = x_axis_from,
              plot_title = title_info$title,
              unit = unit_name,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      }
    }
  }

  # SET DEFAULT FILE_NAME IN EXPORT_CONFIG
  if (is.null(export_config) || is.null(export_config$file_name)) {
    export_config <- .coalesce(export_config, list())
    export_config$file_name <- if (!is.null(top_impact)) {
      paste0("detail_plots_top", top_impact)
    } else {
      "detail_plots"
    }
  }

  # Add calculated dimensions to export_config
  export_config$width <- dimensions$width
  export_config$height <- dimensions$height

  # EXPORT PLOTS
  .export_plot_output(
    plots = plot_list,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    data = data,
    panel_layout = panel_layout
  )

  # RETURN PLOTS INVISIBLY
  return(invisible(plot_list))
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
#' @noRd
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
    .calculate_plot_style_config(NULL, "default")
  }

  # SETUP COLOR PALETTE
  positive_color <- style_config$positive_color
  negative_color <- style_config$negative_color
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_color_palette(positive_color, negative_color, style_config$color_tone, palette_type)

  # PREPARE DATA
  max_abs_value <- max(abs(data$Value))
  decimal_places <- style_config$value_label_decimal_places
  data$Label <- sprintf(paste0("%.", decimal_places, "f"), data$Value)

  n_vars <- length(unique(data[[x_axis_from]]))
  n_panels <- length(unique(data[[panel_var]]))

  # CATEGORIZE VALUES
  if (n_panels > 1) {
    # Split by panel for panel-specific categorization
    panel_groups <- split(data, data[[panel_var]])

    # Process each panel separately for relative color intensity
    result <- lapply(panel_groups, function(panel_data) {
      # Get max absolute value within this panel only
      max_abs_panel <- max(abs(panel_data$Value), na.rm = TRUE)

      # Categorize based on panel-specific thresholds
      panel_data$value_category <- dplyr::case_when(
        panel_data$Value > 0 & abs(panel_data$Value) >= 0.7 * max_abs_panel ~ "extreme_positive",
        panel_data$Value < 0 & abs(panel_data$Value) >= 0.7 * max_abs_panel ~ "extreme_negative",
        panel_data$Value > 0 ~ "normal_positive",
        panel_data$Value < 0 ~ "normal_negative",
        TRUE ~ "neutral"
      )

      return(panel_data)
    })

    # Combine results back together
    data <- do.call(rbind, result)
    rownames(data) <- NULL

  } else {
    # Single panel - use original calculation
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
  }

  # CALCULATE Y-AXIS LIMITS
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limits <- style_config$scale_limit
  } else {
    if (all(data$Value >= 0)) {
      # All positive values
      y_limits <- c(0, max_abs_value * 1.3)
    } else if (all(data$Value <= 0)) {
      # All negative values
      y_limits <- c(-max_abs_value * 1.3, 0)
    } else {
      # Mixed values
      y_limits <- c(-max_abs_value * 1.3, max_abs_value * 1.3)
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

  # PREPARE DATA FOR PLOTTING
  if (!is.null(top_impact)) {
    data <- data[order(data$Value), ]
    x_factor_col <- paste0(x_axis_from, "_factor")
    data[[x_factor_col]] <- factor(data[[x_axis_from]], levels = unique(data[[x_axis_from]]))
  } else {
    x_factor_col <- paste0(x_axis_from, "_factor")
    if (is.factor(data[[x_axis_from]])) {
      data[[x_factor_col]] <- data[[x_axis_from]]
    } else {
      data[[x_factor_col]] <- factor(data[[x_axis_from]], levels = unique(data[[x_axis_from]]))
    }
  }

  # CREATE BASE PLOT WITH APPROPRIATE ORDERING
  if (invert_pane) {
    # For horizontal bars (flipped coordinates)
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(
        data = data,
        mapping = ggplot2::aes(
          y = .data[[x_factor_col]],
          x = .data[["Value"]],
          fill = .data[["value_category"]]
        ),
        width = style_config$bar_width
      )

    # Add value labels for horizontal bars
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          y = .data[[x_factor_col]],
          x = .data[["Value"]],
          label = .data[["Label"]]
        ),
        hjust = ifelse(data$Value >= 0, -0.2, 1.2),
        size = style_config$value_label_size
      )
    }

    # Create scale arguments for x-axis (value axis when horizontal)
    scale_args <- list(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limits[1], y_limits[2], by = style_config$scale_increment)
    }

    # Apply scale to x-axis (value axis when horizontal)
    p <- p + do.call(ggplot2::scale_x_continuous, scale_args)

  } else {
    # For vertical bars (normal coordinates)
    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(
        data = data,
        mapping = ggplot2::aes(
          x = .data[[x_factor_col]],
          y = .data[["Value"]],
          fill = .data[["value_category"]]
        ),
        width = style_config$bar_width
      )

    # Add value labels for vertical bars
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          x = .data[[x_factor_col]],
          y = .data[["Value"]],
          label = .data[["Label"]]
        ),
        vjust = ifelse(data$Value >= 0, -0.5, 1.5),
        size = style_config$value_label_size
      )
    }

    # Create scale arguments for y-axis (value axis when vertical)
    scale_args <- list(
      limits = y_limits,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limits[1], y_limits[2], by = style_config$scale_increment)
    }

    # Apply scale to y-axis (value axis when vertical)
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  # SETUP PLOT APPEARANCE
  p <- p +
    ggplot2::scale_fill_manual(values = color_palette, guide = "none") +
    ggplot2::theme_minimal()

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    facet_args <- list(
      as.formula(paste("~", panel_var)),
      scales = if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) "free" else "fixed"
    )

    if (!is.null(panel_rows)) {
      facet_args$nrow <- panel_rows
    }

    if (!is.null(panel_cols)) {
      facet_args$ncol <- panel_cols
    }

    p <- p + do.call(ggplot2::facet_wrap, facet_args)
  }

  # Handle axis labels BEFORE applying style config
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  # ADD ZERO LINE IF CONFIGURED
  if (style_config$show_zero_line) {
    if (invert_pane) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    } else {
      p <- p + ggplot2::geom_hline(
        yintercept =style_config$zero_line_position0,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    }
  }

  # Apply style config LAST
  p <- .apply_plot_style_config(p, style_config)

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
#' @noRd
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

#' @title Create Stacked Bar Charts for Decomposition Analysis
#'
#' @description
#' Generates stacked bar charts to visualize value compositions across multiple dimensions.
#' The function supports stacked and unstacked presentations for decomposition analysis.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param stack_value_from Character. Column containing stack component categories (e.g., "COMM" for commodities).
#' @param split_by Character or vector. Column name(s) to generate separate plots for each unique value (e.g., "COMM", "REG", "Variable").
#' NULL creates a single aggregated plot, appropriate for macro-level analysis.
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param show_total Logical. If TRUE, displays total values above stacked bars (default: TRUE).
#' @param unstack_plot Logical. If TRUE, creates separate bar plots for each `x_axis_from` value instead of stacked bars (default: FALSE).
#' @param top_impact Numeric or NULL. If specified, shows only the top N impactful values; NULL shows all values.
#' @param invert_pane Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned in R.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF; if FALSE, skips PDF (default: FALSE).
#' @param export_config List. Export settings, including:
#' \itemize{
#'   \item `width`: Output file width (in inches).
#'   \item `height`: Output file height (in inches).
#'   \item Additional settings—see `?get_export_config`.
#' }
#' @param plot_style_config List. Custom plot styles—see `?get_plot_style_config`.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{comparison_plot}}, \code{\link{detail_plot}}
#' @export
#'
#' @examples
#' \donttest{
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#'
#' # GTAP Macro Variables from 2 .sl4 Files named (EXP1, EXP2)
#' # Note: No need to add .sl4 to the experiment name
#' gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
#'                             input_path = input_path, subtotal_level = FALSE,
#'                             process_sl4_vars = NULL, process_har_vars = NULL,
#'                             mapping_info = "GTAPv7", plot_data = TRUE)
#'
#' stack_plot(data = har.plot.data[["A"]],
#'            x_axis_from = "REG",
#'            stack_value_from = "COLUMN",
#'            split_by = FALSE,
#'
#'            show_total = TRUE,
#'            unstack_plot = FALSE,
#'
#'            var_name_by_description = TRUE,
#'
#'            invert_pane = FALSE,
#'            separate_figure = FALSE,
#'
#'            export_picture = FALSE,
#'            export_as_pdf = FALSE,
#'            export_config = list(
#'              width = 28,
#'              height = 15
#'            ),
#'            output_path = tempdir(),
#'
#'            plot_style_config = list(
#'              color_tone = "gtap",
#'              panel_rows = 2,
#'              panel_cols = NULL,
#'              show_legend = TRUE,
#'              show_axis_titles_on_all_facets = FALSE
#'            ))
#' }
stack_plot <- function(data, filter_var = NULL,
                       x_axis_from,
                       stack_value_from,
                       split_by = NULL,
                       panel_var = "Experiment",
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       show_total = TRUE,
                       unstack_plot = FALSE,
                       top_impact = NULL,
                       invert_pane = FALSE,
                       separate_figure = FALSE,
                       output_path = NULL,
                       export_picture = TRUE,
                       export_as_pdf = FALSE,
                       export_config = NULL,
                       plot_style_config = NULL) {

  # Validate the column parameters
  .validate_column_params(data, list(
    x_axis_from = x_axis_from,
    stack_value_from = stack_value_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col
  ))

  # PREPARE DATA SOURCE
  data <- .prepare_data_source(data, x_axis_from, stack_value_from, variable_col)

  # PROCESS SPLIT_BY PARAMETER
  split_by_result <- .process_split_by(data, split_by)
  data <- split_by_result$data
  is_macro_mode <- split_by_result$is_macro_mode
  split_by <- split_by_result$split_by

  # FILTER DATA BY FILTER_VAR IF PROVIDED
  if (!is.null(filter_var)) {
    if (is.data.frame(filter_var) && variable_col %in% names(filter_var)) {
      data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]
    } else {
      # Corrected to filter by variable_col instead of x_axis_from
      data <- data[data[[variable_col]] %in% filter_var, ]
    }

    if (nrow(data) == 0) {
      warning("No matching data found for the specified filter_var values.")
      return(NULL)
    }
  }

  # FORMAT VARIABLE NAMES
  if (variable_col %in% names(data) && desc_col %in% names(data)) {
    data <- .format_variable_names(
      data,
      variable_col = variable_col,
      desc_col = desc_col,
      var_name_by_description = var_name_by_description,
      add_var_info = add_var_info
    )
  }

  # Ensure panel_var column maintains original order for facets
  if (panel_var %in% names(data)) {
    panel_levels <- unique(data[[panel_var]])
    data[[panel_var]] <- factor(data[[panel_var]], levels = panel_levels)
  }

  # Calculate panel layout
  panel_layout <- .calculate_panel_layout(data,
                                          panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
                                          panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
                                          panel_var = panel_var)

  # Check if custom dimensions are provided
  dimensions <- if (!is.null(export_config) &&
                    !is.null(export_config$width) &&
                    !is.null(export_config$height)) {
    list(
      width = export_config$width,
      height = export_config$height
    )
  } else {
    .calculate_plot_dimensions(data, panel_layout)
  }

  # Prepare base style configuration
  base_style_config <- list(
    panel_rows = panel_layout$rows,
    panel_cols = panel_layout$cols,
    all_font_size = .coalesce(plot_style_config$all_font_size, 1)
  )

  # Calculate plot style configuration
  style_config <- .calculate_plot_style_config(
    config = if (!is.null(plot_style_config))
      modifyList(base_style_config, plot_style_config)
    else
      base_style_config,
    plot_type = "default"
  )

  # PROCESS BY UNIT GROUPS
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
      filtered_data <- if (is_macro_mode) {
        unit_data
      } else if (split_col == "split_display") {
        unit_data[unit_data$split_display == sep_value, ]
      } else {
        unit_data[unit_data[[split_col]] == sep_value, ]
      }

      # CALCULATE TOTALS
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

        total_data <- .calculate_stack_totals(filtered_data, x_axis_from, panel_var)

        if ("._split_group_" %in% names(filtered_data)) {
          filtered_data$._split_group_ <- NULL
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

      # CREATE APPROPRIATE PLOT TYPE
      if (unstack_plot) {
        x_axis_values <- unique(filtered_data[[x_axis_from]])

        for (x_val in x_axis_values) {
          x_data <- filtered_data[filtered_data[[x_axis_from]] == x_val, ]
          x_totals <- total_data[total_data[[x_axis_from]] == x_val, ]

          if (separate_figure) {
            panel_values <- unique(x_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_x_data <- x_data[x_data[[panel_var]] == panel_val, ]
              panel_x_totals <- x_totals[x_totals[[panel_var]] == panel_val, ]

              title_info <- .handle_plot_title_and_export(
                var_name = NULL,
                sep_value = sep_value,
                x_value = x_val,
                plot_type = "unstack",
                is_macro_mode = is_macro_mode,
                split_by = split_by,
                x_axis_from = x_axis_from,
                variable_col = variable_col,
                unit_name = unit_name,
                style_config = style_config,
                data = panel_x_data,
                separate_figure = TRUE,
                panel_val = panel_val
              )

              p <- .create_single_unstacked_plot(
                data = panel_x_data,
                total_data = panel_x_totals,
                x_axis_from = x_axis_from,
                stack_value_from = stack_value_from,
                plot_title = title_info$title,
                unit = y_axis_label,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                invert_pane = invert_pane,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
            title_info <- .handle_plot_title_and_export(
              var_name = NULL,
              sep_value = sep_value,
              x_value = x_val,
              plot_type = "unstack",
              is_macro_mode = is_macro_mode,
              split_by = split_by,
              x_axis_from = x_axis_from,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = x_data
            )

            p <- .create_single_unstacked_plot(
              data = x_data,
              total_data = x_totals,
              x_axis_from = x_axis_from,
              stack_value_from = stack_value_from,
              plot_title = title_info$title,
              unit = y_axis_label,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      } else {
        # For stacked plots
        if (separate_figure) {
          panel_values <- unique(filtered_data[[panel_var]])

          for (panel_val in panel_values) {
            panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]
            panel_totals <- total_data[total_data[[panel_var]] == panel_val, ]

            title_info <- .handle_plot_title_and_export(
              var_name = NULL,
              sep_value = sep_value,
              plot_type = "stack",
              is_macro_mode = is_macro_mode,
              split_by = split_by,
              x_axis_from = x_axis_from,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = panel_data,
              separate_figure = TRUE,
              panel_val = panel_val
            )

            p <- .create_single_stacked_plot(
              data = panel_data,
              total_data = panel_totals,
              x_axis_from = x_axis_from,
              stack_value_from = stack_value_from,
              plot_title = title_info$title,
              unit = y_axis_label,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              show_total = show_total,
              invert_pane = invert_pane,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        } else {
          title_info <- .handle_plot_title_and_export(
            var_name = NULL,
            sep_value = sep_value,
            plot_type = "stack",
            is_macro_mode = is_macro_mode,
            split_by = split_by,
            x_axis_from = x_axis_from,
            variable_col = variable_col,
            unit_name = unit_name,
            style_config = style_config,
            data = filtered_data
          )

          p <- .create_single_stacked_plot(
            data = filtered_data,
            total_data = total_data,
            x_axis_from = x_axis_from,
            stack_value_from = stack_value_from,
            plot_title = title_info$title,
            unit = y_axis_label,
            panel_rows = style_config$panel_rows,
            panel_cols = style_config$panel_cols,
            panel_var = panel_var,
            show_total = show_total,
            invert_pane = invert_pane,
            top_impact = top_impact,
            plot_style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
        }
      }
    }
  }

  # SET DEFAULT FILE_NAME IN EXPORT_CONFIG
  if (is.null(export_config) || is.null(export_config$file_name)) {
    export_config <- .coalesce(export_config, list())

    # Extract data source information for PDF naming
    data_source_info <- ""
    if (variable_col %in% names(data) && length(unique(data[[variable_col]])) == 1) {
      data_source_info <- paste0("_", unique(data[[variable_col]])[1])
    } else if (!is.null(split_by) && split_by %in% names(data) && length(unique(data[[split_by]])) == 1) {
      data_source_info <- paste0("_", unique(data[[split_by]])[1])
    }

    data_source_info <- gsub("[^a-zA-Z0-9_]", "", data_source_info)
    plot_type_name <- if (unstack_plot) "Unstacked_plots" else "Stacked_plots"
    n_plots <- length(plot_list)
    export_config$file_name <- paste0(plot_type_name, data_source_info, "_", n_plots)
  }

  # Add calculated dimensions to export_config
  export_config$width <- dimensions$width
  export_config$height <- dimensions$height

  # EXPORT PLOTS
  .export_plot_output(
    plots = plot_list,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    data = data,
    panel_layout = panel_layout
  )

  # RETURN SINGLE PLOT OR LIST OF PLOTS
  return(invisible(plot_list))
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
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param top_impact Numeric. Number of top impacts to display. Default is NULL.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the stacked plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @noRd
#' @seealso \code{\link{stack_plot}}
#'
.create_single_stacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                        plot_title, unit,
                                        panel_rows, panel_cols,
                                        panel_var = "Experiment",
                                        show_total = TRUE,
                                        invert_pane = FALSE,
                                        top_impact = NULL,
                                        plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # SETUP VARIABLES
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # RESPECT EXISTING FACTOR LEVELS FOR CONSISTENT SORTING
  if (!is.null(top_impact)) {
    # For top_impact case, allow automatic sorting
  } else {
    # Preserve existing factor levels for x_axis_from if it's already a factor
    if (is.factor(data[[x_axis_from]])) {
      # Already a factor, keep levels as is
    } else {
      # If not a factor, create one with current ordering
      data[[x_axis_from]] <- factor(data[[x_axis_from]], levels = unique(data[[x_axis_from]]))
    }

    # Also preserve factor levels for stack_value_from
    if (is.factor(data[[stack_value_from]])) {
      # Already a factor, keep levels as is
    } else {
      # If not a factor, create one with current ordering
      data[[stack_value_from]] <- factor(data[[stack_value_from]], levels = unique(data[[stack_value_from]]))
    }
  }

  # GENERATE COLOR PALETTE
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone, palette_type)

  # CALCULATE Y LIMITS
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limit <- style_config$scale_limit
  } else {
    # Check if total_data exists and has the Total column
    if (is.data.frame(total_data) && nrow(total_data) > 0 && "Total" %in% names(total_data)) {
      # Calculate appropriate limits based on actual values
      max_abs_total <- max(abs(total_data$Total), na.rm = TRUE)

      if (is.finite(max_abs_total) && max_abs_total > 0) {
        if (all(total_data$Total >= 0, na.rm = TRUE)) {
          # All positive values
          y_limit <- c(0, max(total_data$Total, na.rm = TRUE) * 1.4)
        } else if (all(total_data$Total <= 0, na.rm = TRUE)) {
          # All negative values
          y_limit <- c(min(total_data$Total, na.rm = TRUE) * 1.4, 0)
        } else {
          # Mixed values
          y_limit <- c(min(total_data$Total, na.rm = TRUE) * 1.4,
                       max(total_data$Total, na.rm = TRUE) * 1.4)
        }
      } else {
        # Handle invalid data within total_data
        max_abs_value <- max(abs(data$Value), na.rm = TRUE)
        if (is.finite(max_abs_value) && max_abs_value > 0) {
          y_limit <- c(-max_abs_value * 1.4, max_abs_value * 1.4)
        } else {
          y_limit <- c(-10, 10)  # Last resort fallback
        }
      }
    } else {
      # If total_data is invalid, calculate directly from data
      # Group by x-axis and panel to calculate stacked totals manually
      totals <- tapply(data$Value, list(data[[x_axis_from]], data[[panel_var]]), sum, na.rm = TRUE)

      if (!is.null(totals) && length(totals) > 0) {
        max_total <- max(abs(totals), na.rm = TRUE)
        if (is.finite(max_total) && max_total > 0) {
          min_val <- min(totals, na.rm = TRUE)
          max_val <- max(totals, na.rm = TRUE)

          if (min_val >= 0) {
            # All positive
            y_limit <- c(0, max_val * 1.4)
          } else if (max_val <= 0) {
            # All negative
            y_limit <- c(min_val * 1.4, 0)
          } else {
            # Mixed
            y_limit <- c(min_val * 1.4, max_val * 1.4)
          }
        } else {
          y_limit <- c(-10, 10)  # Fallback
        }
      } else {
        y_limit <- c(-10, 10)  # Fallback if tapply fails
      }
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
        ggplot2::aes(
          y = .data[[x_axis_from]],
          x = .data[["Value"]],
          fill = .data[[stack_value_from]]
        ),
        position = "stack",
        width = style_config$bar_width
      )

    # ADD TOTAL LABELS IF CONFIGURED
    if (show_total) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size
      value_label_face <- style_config$value_label_face

      p <- p + ggplot2::geom_text(
        data = total_data,
        ggplot2::aes(
          y = .data[[x_axis_from]],
          x = ifelse(Total >= 0,
                     PositiveTotal + pmax(abs(Total) * 0.1, 0.1),
                     NegativeTotal - pmax(abs(Total) * 0.1, 0.1)),
          label = sprintf(paste0("Total\n%.", decimal_places, "f"), Total)
        ),
        hjust = ifelse(total_data$Total >= 0, 0, 1),
        vjust = 0.5,
        size = value_size,
        fontface = value_label_face
      )
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    scale_args <- list(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limit[1], y_limit[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_x_continuous, scale_args)

  } else {
    # For vertical bars (categories on x-axis, values on y-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes(
          x = .data[[x_axis_from]],
          y = .data[["Value"]],
          fill = .data[[stack_value_from]]
        ),
        position = "stack",
        width = style_config$bar_width
      )

    # ADD TOTAL LABELS IF CONFIGURED
    if (show_total) {
      decimal_places <- style_config$value_label_decimal_places
      value_size <- style_config$value_label_size
      value_label_face <- style_config$value_label_face

      p <- p + ggplot2::geom_text(
        data = total_data,
        ggplot2::aes(
          x = .data[[x_axis_from]],
          y = ifelse(Total >= 0,
                     PositiveTotal + abs(Total) * 0.05,
                     NegativeTotal - abs(Total) * 0.05),
          label = sprintf(paste0("Total\n%.", decimal_places, "f"), Total)
        ),
        vjust = ifelse(total_data$Total >= 0, 0, 1.5),
        size = value_size,
        fontface = value_label_face
      )
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    scale_args <- list(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limit[1], y_limit[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  # ADD ZERO LINE IF CONFIGURED
  if (style_config$show_zero_line) {
    if (invert_pane) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    } else {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    }
  }

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    facet_args <- list(
      as.formula(paste("~", panel_var)),
      scales = if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) "free" else "fixed"
    )

    if (!is.null(panel_rows)) {
      facet_args$nrow <- panel_rows
    }

    if (!is.null(panel_cols)) {
      facet_args$ncol <- panel_cols
    }

    p <- p + do.call(ggplot2::facet_wrap, facet_args)
  }

  # SETUP APPEARANCE
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

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
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
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


#' @title Create Single Unstacked Plot (Internal)
#'
#' @description
#' Create a single unstacked plot from GTAP data for internal use. This function shows
#' individual components as separate bars rather than stacked.
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
#' @param invert_pane Logical. Whether to flip plot orientation. Default is FALSE.
#' @param top_impact Numeric. Number of top impacts to display. Default is NULL.
#' @param plot_style_config List. Custom plot styling configuration.
#'
#' @return A ggplot2 object representing the unstacked plot.
#'
#' @author Pattawee Puangchit
#'
#' @keywords internal
#' @noRd
#' @seealso \code{\link{stack_plot}}
.create_single_unstacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                          plot_title, unit,
                                          panel_rows, panel_cols,
                                          panel_var = "Experiment",
                                          invert_pane = FALSE,
                                          top_impact = NULL,
                                          plot_style_config = NULL) {

  # GET STYLE CONFIGURATION
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # SETUP VARIABLES
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # RESPECT EXISTING FACTOR LEVELS FOR CONSISTENT SORTING
  if (!is.null(top_impact)) {
    # For top_impact case, allow automatic sorting
  } else {
    # Preserve existing factor levels for x_axis_from if it's already a factor
    if (is.factor(data[[x_axis_from]])) {
      # Already a factor, keep levels as is
    } else {
      # If not a factor, create one with current ordering
      data[[x_axis_from]] <- factor(data[[x_axis_from]], levels = unique(data[[x_axis_from]]))
    }

    # Also preserve factor levels for stack_value_from
    if (is.factor(data[[stack_value_from]])) {
      # Already a factor, keep levels as is
    } else {
      # If not a factor, create one with current ordering
      data[[stack_value_from]] <- factor(data[[stack_value_from]], levels = unique(data[[stack_value_from]]))
    }
  }

  # GENERATE COLOR PALETTE
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone, palette_type)

  # CALCULATE Y LIMITS
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limit <- style_config$scale_limit
  } else {
    # Calculate more generous limits for unstacked plot
    value_range <- range(data$Value, na.rm = TRUE)
    max_abs <- max(abs(value_range), na.rm = TRUE)

    # Determine if we have all positive, all negative, or mixed values
    if (all(data$Value >= 0, na.rm = TRUE)) {
      # All positive values
      y_limit <- c(0, max_abs * 1.3)
    } else if (all(data$Value <= 0, na.rm = TRUE)) {
      # All negative values
      y_limit <- c(-max_abs * 1.3, 0)
    } else {
      # Mixed values - symmetrical with extra padding
      y_limit <- c(-max_abs * 1.3, max_abs * 1.3)
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
    stack_value_from
  }

  # FORMAT VALUE LABELS
  decimal_places <- style_config$value_label_decimal_places
  data$Label <- sprintf(paste0("%.", decimal_places, "f"), data$Value)

  # CREATE BASE PLOT BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars (categories on y-axis, values on x-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes(
          y = .data[[stack_value_from]],
          x = .data[["Value"]],
          fill = .data[[stack_value_from]]
        ),
        width = style_config$bar_width
      )

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        ggplot2::aes(
          y = .data[[stack_value_from]],
          x = .data[["Value"]],
          label = .data[["Label"]]
        ),
        hjust = ifelse(data$Value >= 0, -0.2, 1.2),
        size = style_config$value_label_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (X-AXIS)
    scale_args <- list(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limit[1], y_limit[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_x_continuous, scale_args)

  } else {
    # For vertical bars (categories on x-axis, values on y-axis)
    p <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data,
        ggplot2::aes(
          x = .data[[stack_value_from]],
          y = .data[["Value"]],
          fill = .data[[stack_value_from]]
        ),
        width = style_config$bar_width
      )

    # ADD VALUE LABELS IF CONFIGURED
    if (style_config$show_value_labels) {
      p <- p + ggplot2::geom_text(
        data = data,
        ggplot2::aes(
          x = .data[[stack_value_from]],
          y = .data[["Value"]],
          label = .data[["Label"]]
        ),
        vjust = ifelse(data$Value >= 0, -0.5, 1.5),
        size = style_config$value_label_size
      )
    }

    # APPLY SCALE TO VALUE AXIS (Y-AXIS)
    scale_args <- list(
      limits = y_limit,
      oob = scales::oob_keep,
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )

    # Add breaks if scale_increment is specified
    if (!is.null(style_config$scale_increment) && is.numeric(style_config$scale_increment)) {
      scale_args$breaks <- seq(y_limit[1], y_limit[2], by = style_config$scale_increment)
    }

    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  # ADD ZERO LINE IF CONFIGURED
  if (style_config$show_zero_line) {
    if (invert_pane) {
      p <- p + ggplot2::geom_vline(
        xintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    } else {
      p <- p + ggplot2::geom_hline(
        yintercept = style_config$zero_line_position,
        linetype = style_config$zero_line_type,
        color = style_config$zero_line_color,
        linewidth = style_config$zero_line_size
      )
    }
  }

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    facet_args <- list(
      as.formula(paste("~", panel_var)),
      scales = if (!is.null(top_impact) || style_config$show_axis_titles_on_all_facets) "free" else "fixed"
    )

    if (!is.null(panel_rows)) {
      facet_args$nrow <- panel_rows
    }

    if (!is.null(panel_cols)) {
      facet_args$ncol <- panel_cols
    }

    p <- p + do.call(ggplot2::facet_wrap, facet_args)
  }

  # SETUP APPEARANCE
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  if (invert_pane) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      p <- p + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      p <- p + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      p <- p + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
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
#' @noRd
#'
.calculate_stack_totals <- function(data, x_axis_from, panel_var) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .calculate_stack_totals, x_axis_from, panel_var))
  }

  # INPUT VALIDATION
  if (!is.data.frame(data)) return(NULL)
  if (!all(c(x_axis_from, panel_var, "Value") %in% names(data))) return(NULL)

  # Ensure Value column is numeric
  data$Value <- as.numeric(data$Value)

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
#' @noRd
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
