#' @title Create Decomposition Area Charts Over Time
#' @md
#' @description
#' Generates stacked area charts showing how different components contribute to a total value
#' over time periods. Ideal for visualizing GDP decomposition, welfare contributions, or any
#' time-series decomposition analysis where you want to see both the evolution of individual
#' components and their cumulative effect.
#'
#' **Input Data**
#' @param data A data frame or list of data frames containing GTAP time-series results.
#' @param filter_var NULL, a vector, a data frame, or a named list specifying filtering conditions.
#' For example: \code{list(Variable = c("GDP"), Country = c("USA", "CHN"))}.
#' @param period_col Character. Column name for time periods (x-axis). Default is `"Period"`.
#' @param component_col Character. Column containing decomposition components (e.g., "Component", "Sector").
#' This defines the areas that will be stacked.
#' @param split_by Character or vector.
#' - Column(s) used to create separate plots (e.g., `"Country"` or `c("Country", "Variable")`).
#' - If `NULL`, a single aggregated plot is generated.
#' @param panel_var Character. Column for panel facets. Default is `"Experiment"`.
#' @param variable_col Character. Column name for variable codes. Default is `"Variable"`.
#' @param unit_col Character. Column name for units. Default is `"Unit"`.
#' @param desc_col Character. Column name for variable descriptions. Default is `"Description"`.
#'
#' **Plot Behavior**
#' @param separate_figure Logical. If `TRUE`, generates a separate plot for each value in `panel_var`. Default is `FALSE`.
#' @param show_total_line Logical. If `TRUE`, overlays a line showing the total across all components. Default is `TRUE`.
#' @param normalize Logical. If `TRUE`, shows percentage contribution (0-100\%) instead of absolute values. Default is `FALSE`.
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
#' @param decomp_style_config List. Custom plot appearance settings for decomposition area plots.
#' See \code{\link{create_decomp_style}} for available options.
#' @param title_format List or NULL. Title format configuration created with \code{\link{create_title_format}}.
#' Supports "standard", "prefix", "suffix", "full", and "dynamic" types. If `NULL`, uses standard formatting.
#' Example: \code{create_title_format(type = "dynamic", text = "Decomposition: {Description}", sep = "")}.
#'
#' @return A ggplot object or a named list of ggplot objects depending on input settings.
#' If `export_picture` or `export_as_pdf` is enabled, plots are also saved to `output_path`.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic GDP decomposition by country
#' decomp_area_plot(
#'   data = gdp_data,
#'   period_col = "Year",
#'   component_col = "Component", # Consumption, Investment, Net Exports, etc.
#'   split_by = "Country"
#' )
#'
#' # Example 2: Normalized view with custom styling
#' my_decomp_style <- create_decomp_style(
#'   color_palette = "economic",
#'   show_total_line = TRUE,
#'   total_line_size = 1.5,
#'   area_alpha = 0.7,
#'   vertical_lines = list(
#'     create_vline(2008, "Financial Crisis", "red", "dashed"),
#'     create_vline(2020, "COVID-19", "darkred", "dashed")
#'   )
#' )
#'
#' decomp_area_plot(
#'   data = welfare_data,
#'   period_col = "Year",
#'   component_col = "Source",
#'   split_by = "Region",
#'   normalize = TRUE,
#'   decomp_style_config = my_decomp_style,
#'   title_format = create_title_format(
#'     type = "dynamic",
#'     text = "Welfare Decomposition: {Description}",
#'     sep = " - "
#'   )
#' )
#' }
decomp_area_plot <- function(data,
                             filter_var = NULL,
                             period_col = "Period",
                             component_col,
                             split_by = NULL,
                             panel_var = "Experiment",
                             variable_col = "Variable",
                             unit_col = "Unit",
                             desc_col = "Description",
                             separate_figure = FALSE,
                             show_total_line = TRUE,
                             normalize = FALSE,
                             var_name_by_description = FALSE,
                             add_var_info = FALSE,
                             output_path = NULL,
                             export_picture = TRUE,
                             export_as_pdf = FALSE,
                             export_config = NULL,
                             decomp_style_config = NULL,
                             title_format = NULL) {

  if (!is.data.frame(data) && !is.list(data)) {
    stop("'data' must be a data frame or a list of data frames.")
  }

  if (is.data.frame(data)) {
    data <- list(data)
  }

  if (!period_col %in% colnames(data[[1]])) {
    stop(paste0("Period column '", period_col, "' not found in data."))
  }

  if (!component_col %in% colnames(data[[1]])) {
    stop(paste0("Component column '", component_col, "' not found in data."))
  }

  style_config <- if (!is.null(decomp_style_config)) {
    .calculate_decomp_style_config(decomp_style_config)
  } else {
    .calculate_decomp_style_config(NULL)
  }

  if (!is.null(title_format)) {
    style_config$title_format <- title_format
  }

  export_config <- if (!is.null(export_config)) {
    export_config
  } else {
    create_export_config()
  }

  plot_list <- .create_decomp_area_plots(
    data = data,
    filter_var = filter_var,
    period_col = period_col,
    component_col = component_col,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    separate_figure = separate_figure,
    show_total_line = show_total_line,
    normalize = normalize,
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
      default_filename = "decomp_area"
    )
  }

  return(invisible(plot_list))
}


#' @title Create Decomposition Area Plot Style Configuration
#' @description
#' Creates a configuration list for controlling decomposition area plot appearance and behavior.
#'
#' @param area_alpha Numeric. Transparency of area fills
#' @param area_position Character. Stacking method
#' @param show_area_lines Logical. Whether to show lines at area boundaries
#' @param area_line_color Character. Color of area boundary lines
#' @param area_line_size Numeric. Thickness of area boundary lines
#' @param color_palette Character or vector. Color palette for areas
#' @param reverse_stack_order Logical. Reverse the stacking order
#' @param show_total_line Logical. Overlay a line showing total
#' @param total_line_color Character. Color of the total line
#' @param total_line_size Numeric. Thickness of the total line
#' @param total_line_type Character. Line type
#' @param show_points Logical. Show points on the total line
#' @param point_size Numeric. Size of points
#' @param legend_title Character or NULL. Custom legend title
#' @param vertical_lines List or NULL. List of vertical line specs
#' @param vline_label_size Numeric. Font size for vertical line labels
#' @param vline_label_angle Numeric. Angle for vertical line labels
#' @param vline_label_vjust Numeric. Vertical adjustment for labels
#' @param period_breaks Character or numeric. Period axis breaks
#' @param show_title Logical. Show plot title
#' @param title_face Character. Title font face
#' @param title_size Numeric. Title font size
#' @param title_hjust Numeric. Title horizontal alignment
#' @param add_unit_to_title Logical. Append unit to title
#' @param title_margin Numeric vector
#' @param title_format List. Title format configuration
#' @param show_x_axis_title Logical. Show x-axis title
#' @param x_axis_title_face Character. X-axis title font face
#' @param x_axis_title_size Numeric. X-axis title size
#' @param x_axis_title_margin Numeric vector
#' @param show_x_axis_labels Logical. Show x-axis labels
#' @param x_axis_text_face Character. X-axis text face
#' @param x_axis_text_size Numeric. X-axis text size
#' @param x_axis_text_angle Numeric. X-axis text rotation
#' @param x_axis_text_hjust Numeric. X-axis text horizontal alignment
#' @param x_axis_description Character. Custom x-axis label
#' @param show_y_axis_title Logical. Show y-axis title
#' @param y_axis_title_face Character. Y-axis title font face
#' @param y_axis_title_size Numeric. Y-axis title size
#' @param y_axis_title_margin Numeric vector
#' @param show_y_axis_labels Logical. Show y-axis labels
#' @param y_axis_text_face Character. Y-axis text face
#' @param y_axis_text_size Numeric. Y-axis text size
#' @param y_axis_text_angle Numeric. Y-axis text rotation
#' @param y_axis_text_hjust Numeric. Y-axis text horizontal alignment
#' @param y_axis_description Character. Custom y-axis label
#' @param show_legend Logical. Show legend
#' @param show_legend_title Logical. Show legend title
#' @param legend_position Character. Legend position
#' @param legend_title_face Character. Legend title font face
#' @param legend_text_face Character. Legend text face
#' @param legend_text_size Numeric. Legend text size
#' @param strip_face Character. Facet strip text face
#' @param strip_text_size Numeric. Facet strip text size
#' @param strip_background Character. Facet strip background color
#' @param strip_text_margin Numeric vector
#' @param panel_spacing Numeric. Spacing between facet panels
#' @param panel_rows Integer or NULL. Number of facet rows
#' @param panel_cols Integer or NULL. Number of facet columns
#' @param show_axis_titles_on_all_facets Logical. Show axis titles on all panels
#' @param background_color Character. Plot background color
#' @param grid_color Character. Grid line color
#' @param show_grid_major_x Logical. Show major x-grid lines
#' @param show_grid_major_y Logical. Show major y-grid lines
#' @param show_grid_minor_x Logical. Show minor x-grid lines
#' @param show_grid_minor_y Logical. Show minor y-grid lines
#' @param show_zero_line Logical. Show horizontal zero line
#' @param zero_line_type Character. Zero line type
#' @param zero_line_color Character. Zero line color
#' @param zero_line_size Numeric. Zero line thickness
#' @param zero_line_position Numeric. Y-position of zero line
#' @param scale_limit Numeric vector or NULL. Y-axis limits
#' @param scale_increment Numeric or NULL. Y-axis tick increment
#' @param expansion_y_mult Numeric vector. Y-axis expansion
#' @param expansion_x_mult Numeric vector. X-axis expansion
#' @param plot.margin Numeric vector. Margins in mm
#' @param all_font_size Numeric. Master font size multiplier
#'
#' @return List with decomposition area plot style configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' basic_decomp_style <- create_decomp_style()
create_decomp_style <- function(
    area_alpha = 0.7,
    area_position = "stack",
    show_area_lines = FALSE,
    area_line_color = "white",
    area_line_size = 0.3,
    color_palette = NULL,
    reverse_stack_order = FALSE,
    show_total_line = TRUE,
    total_line_color = "black",
    total_line_size = 1.2,
    total_line_type = "solid",
    show_points = FALSE,
    point_size = 2,
    legend_title = NULL,
    vertical_lines = NULL,
    vline_label_size = 3.5,
    vline_label_angle = 90,
    vline_label_vjust = -0.2,
    period_breaks = "auto",
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
    show_legend_title = FALSE,
    legend_position = "right",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 14,
    strip_face = "bold",
    strip_text_size = 16,
    strip_background = "lightgrey",
    strip_text_margin = c(10, 0, 10, 0),
    panel_spacing = 2,
    panel_rows = NULL,
    panel_cols = NULL,
    show_axis_titles_on_all_facets = FALSE,
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = TRUE,
    show_grid_major_y = TRUE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,
    show_zero_line = FALSE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,
    scale_limit = NULL,
    scale_increment = NULL,
    expansion_y_mult = c(0, 0.1),
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
.calculate_decomp_style_config <- function(config = NULL) {
  default_config <- create_decomp_style()

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
.create_decomp_area_plots <- function(data, filter_var, period_col, component_col, split_by,
                                      panel_var, variable_col, unit_col, desc_col,
                                      separate_figure, show_total_line, normalize,
                                      var_name_by_description, add_var_info, style_config) {

  plot_list <- list()

  for (df in data) {
    if (!is.null(filter_var)) {
      df <- .apply_filters(df, filter_var)
      if (nrow(df) == 0) next
    }

    if (!is.numeric(df[[period_col]])) {
      df[[period_col]] <- as.numeric(as.character(df[[period_col]]))
    }

    # Sort panel_var by ScenarioRank if it exists
    if ("ScenarioRank" %in% colnames(df) && panel_var %in% colnames(df)) {
      panel_rank <- unique(df[, c(panel_var, "ScenarioRank")])
      panel_rank <- panel_rank[order(panel_rank$ScenarioRank), ]
      panel_rank <- panel_rank[!duplicated(panel_rank[[panel_var]]), ]
      df[[panel_var]] <- factor(df[[panel_var]], levels = panel_rank[[panel_var]])
    }

    # Sort components if ComponentRank exists
    if ("ComponentRank" %in% colnames(df)) {
      df <- df[order(df$ComponentRank), ]
      df[[component_col]] <- factor(df[[component_col]],
                                    levels = unique(df[[component_col]][order(df$ComponentRank)]))
    }

    # Process split_by logic
    is_macro_mode <- is.null(split_by) || (is.logical(split_by) && !split_by)

    if (!is_macro_mode) {
      if (!is.null(split_by) && !all(split_by %in% colnames(df))) {
        stop(paste0("Split-by column(s) not found: ", paste(setdiff(split_by, colnames(df)), collapse = ", ")))
      }

      sep_values <- if (length(split_by) > 1) {
        unique(apply(df[, split_by, drop = FALSE], 1, paste, collapse = " | "))
      } else {
        unique(df[[split_by]])
      }

      for (sep_value in sep_values) {
        if (length(split_by) > 1) {
          sep_data <- df[apply(df[, split_by, drop = FALSE], 1, paste, collapse = " | ") == sep_value, ]
        } else {
          sep_data <- df[df[[split_by]] == sep_value, ]
        }

        unit_name <- if (unit_col %in% colnames(sep_data)) {
          unique(sep_data[[unit_col]])[1]
        } else {
          "Value"
        }

        title_info <- .handle_decomp_title_and_export(
          sep_value = sep_value,
          split_by = split_by,
          variable_col = variable_col,
          unit_name = unit_name,
          style_config = style_config,
          data = sep_data,
          var_name_by_description = var_name_by_description,
          add_var_info = add_var_info,
          desc_col = desc_col,
          normalize = normalize
        )

        p <- .create_single_decomp_area_plot(
          data = sep_data,
          period_col = period_col,
          component_col = component_col,
          plot_title = title_info$title,
          unit = unit_name,
          panel_var = panel_var,
          show_total_line = show_total_line,
          normalize = normalize,
          style_config = style_config
        )

        plot_list[[title_info$export_name]] <- p
      }
    } else {
      unit_name <- if (unit_col %in% colnames(df)) {
        unique(df[[unit_col]])[1]
      } else {
        "Value"
      }

      title_info <- .handle_decomp_title_and_export(
        sep_value = NULL,
        split_by = NULL,
        variable_col = variable_col,
        unit_name = unit_name,
        style_config = style_config,
        data = df,
        var_name_by_description = var_name_by_description,
        add_var_info = add_var_info,
        desc_col = desc_col,
        normalize = normalize
      )

      p <- .create_single_decomp_area_plot(
        data = df,
        period_col = period_col,
        component_col = component_col,
        plot_title = title_info$title,
        unit = unit_name,
        panel_var = panel_var,
        show_total_line = show_total_line,
        normalize = normalize,
        style_config = style_config
      )

      plot_list[[title_info$export_name]] <- p
    }
  }

  return(plot_list)
}

#' @keywords internal
#' @noRd
.create_single_decomp_area_plot <- function(data, period_col, component_col, plot_title,
                                            unit, panel_var = NULL, show_total_line = TRUE,
                                            normalize = FALSE, style_config) {

  # Determine position based on normalize flag
  position_type <- if (normalize) "fill" else "stack"

  # Calculate y-axis label using helper function (like other plots)
  y_axis_label <- if (normalize) {
    "Percentage (%)"
  } else if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    period_col
  }

  # Generate colors
  n_components <- length(unique(data[[component_col]]))
  if (!is.null(style_config$color_palette)) {
    if (length(style_config$color_palette) == 1 && is.character(style_config$color_palette)) {
      colors <- .generate_area_colors(n_components, style_config$color_palette)
    } else {
      colors <- rep_len(style_config$color_palette, n_components)
    }
  } else {
    colors <- .generate_area_colors(n_components, "economic")
  }

  # Reverse stack order if requested
  if (style_config$reverse_stack_order) {
    component_levels <- rev(levels(data[[component_col]]))
    data[[component_col]] <- factor(data[[component_col]], levels = component_levels)
    colors <- rev(colors)
  }

  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[period_col]],
    y = .data[["Value"]],
    fill = .data[[component_col]],
    group = .data[[component_col]]
  ))

  # Add stacked area
  if (style_config$show_area_lines) {
    p <- p + ggplot2::geom_area(
      position = position_type,
      alpha = style_config$area_alpha,
      color = style_config$area_line_color,
      linewidth = style_config$area_line_size
    )
  } else {
    p <- p + ggplot2::geom_area(
      position = position_type,
      alpha = style_config$area_alpha
    )
  }

  # Add total line if requested and not normalized
  if (show_total_line && !normalize) {
    total_data <- stats::aggregate(
      stats::as.formula(paste("Value ~", period_col)),
      data = data,
      FUN = sum
    )

    p <- p + ggplot2::geom_line(
      data = total_data,
      ggplot2::aes(x = .data[[period_col]], y = .data[["Value"]]),
      inherit.aes = FALSE,
      color = style_config$total_line_color,
      linewidth = style_config$total_line_size,
      linetype = style_config$total_line_type
    )

    if (style_config$show_points) {
      p <- p + ggplot2::geom_point(
        data = total_data,
        ggplot2::aes(x = .data[[period_col]], y = .data[["Value"]]),
        inherit.aes = FALSE,
        color = style_config$total_line_color,
        size = style_config$point_size
      )
    }
  }

  # Apply colors
  legend_label <- if (!is.null(style_config$legend_title)) {
    style_config$legend_title
  } else {
    component_col
  }

  p <- p + ggplot2::scale_fill_manual(values = colors, name = legend_label)

  # Set up period breaks
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

  # Set up y-axis scale
  if (normalize) {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      expand = ggplot2::expansion(mult = style_config$expansion_y_mult)
    )
  } else {
    y_limits <- if (!is.null(style_config$scale_limit)) {
      style_config$scale_limit
    } else {
      NULL
    }
    p <- p + .apply_axis_scale(y_limits, style_config, axis = "y")
  }

  # Add zero line if configured
  if (style_config$show_zero_line && !normalize) {
    p <- p + ggplot2::geom_hline(
      yintercept = style_config$zero_line_position,
      linetype = style_config$zero_line_type,
      color = style_config$zero_line_color,
      linewidth = style_config$zero_line_size
    )
  }

  # Add vertical reference lines
  if (!is.null(style_config$vertical_lines) && is.list(style_config$vertical_lines)) {
    for (vline in style_config$vertical_lines) {
      if (!is.null(vline$period)) {
        p <- p + ggplot2::geom_vline(
          xintercept = vline$period,
          linetype = vline$linetype,
          color = vline$color,
          linewidth = vline$size
        )

        if (!is.null(vline$label) && nchar(vline$label) > 0) {
          y_max <- if (normalize) 1 else max(data$Value, na.rm = TRUE)
          p <- p + ggplot2::annotate(
            "text",
            x = vline$period,
            y = y_max,
            label = vline$label,
            angle = style_config$vline_label_angle,
            vjust = style_config$vline_label_vjust,
            size = style_config$vline_label_size,
            color = vline$color
          )
        }
      }
    }
  }

  # Add facets if needed
  n_panels <- length(unique(data[[panel_var]]))
  if (n_panels > 1) {
    p <- p + .create_facet_wrap(
      panel_var = panel_var,
      panel_rows = style_config$panel_rows,
      panel_cols = style_config$panel_cols,
      style_config = style_config,
      free_scales = FALSE
    )
  }

  # Apply theme
  p <- p + ggplot2::theme_minimal()

  # Set axis labels
  p <- .set_axis_labels(p, FALSE, plot_title, x_axis_label, y_axis_label, style_config)

  # Apply style configuration
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

#' @keywords internal
#' @noRd
.handle_decomp_title_and_export <- function(sep_value, split_by, variable_col, unit_name,
                                            style_config, data, var_name_by_description,
                                            add_var_info, desc_col, normalize = FALSE) {

  # Build base title
  base_title <- if (var_name_by_description && desc_col %in% colnames(data)) {
    var_desc <- unique(data[[desc_col]])[1]
    if (add_var_info && variable_col %in% colnames(data)) {
      var_code <- unique(data[[variable_col]])[1]
      paste0(var_desc, " (", var_code, ")")
    } else {
      var_desc
    }
  } else if (variable_col %in% colnames(data)) {
    unique(data[[variable_col]])[1]
  } else {
    "Decomposition"
  }

  if (!is.null(sep_value)) {
    base_title <- paste(base_title, sep_value, sep = " - ")
  }

  plot_title <- base_title

  # Apply title format transformations from style config (like other plots)
  dynamic_title_has_unit <- FALSE
  if (!is.null(style_config) && !is.null(style_config$title_format)) {
    title_format <- style_config$title_format

    if (title_format$type == "dynamic") {
      if (!is.null(data) && !is.null(title_format$text) && nrow(data) > 0) {
        if (!requireNamespace("glue", quietly = TRUE)) {
          warning("The 'glue' package is required for dynamic titles but is not installed. Using standard title format.")
        } else {
          # Check if Unit is referenced in the dynamic template
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

  # Add unit to title if appropriate (following standard pattern)
  if (!is.null(style_config) &&
      (!is.null(style_config$title_format) &&
       (style_config$title_format$type != "dynamic" ||
        (style_config$title_format$type == "dynamic" && !dynamic_title_has_unit))) &&
      style_config$add_unit_to_title && !is.null(unit_name)) {
    if (normalize) {
      plot_title <- paste0(plot_title, " (%)")
    } else if (tolower(unit_name) == "percent") {
      plot_title <- paste0(plot_title, " (%)")
    } else {
      plot_title <- paste0(plot_title, " (", unit_name, ")")
    }
  }

  # Create export name (file-safe)
  export_name <- plot_title

  # Format the unit in the filename differently from the plot title
  if (!is.null(unit_name)) {
    # For percent/percentage units, use (%) in the filename just like in the title
    if (grepl("percent", tolower(unit_name))) {
      export_name <- gsub("\\s*\\([^)]*\\)", " (%)", export_name)
    }
    # For other units, remove spaces in the unit name for the filename
    else if (grepl(" ", unit_name)) {
      # First, extract the unit part
      unit_pattern <- paste0("\\(", unit_name, "\\)")
      compact_unit <- gsub(" ", "", unit_name)
      export_name <- gsub(unit_pattern, paste0("(", compact_unit, ")"), export_name)
    }
  }

  # Make export name file-safe but preserve special characters (like %)
  export_name <- gsub("[^a-zA-Z0-9_\\-\\. ()%]", "-", export_name)
  export_name <- gsub("\\s+", " ", trimws(export_name))

  if (nchar(export_name) > 200) {
    export_name <- paste0(substr(export_name, 1, 197), "...")
  }

  return(list(
    title = plot_title,
    export_name = export_name
  ))
}

#' @keywords internal
#' @noRd
.generate_area_colors <- function(n_colors, palette_name = "economic") {
  .create_color_palette(color_tone = palette_name, n_colors = n_colors, palette_type = "qualitative")
}
