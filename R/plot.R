# Comparison Plot ---------------------------------------------------------


#' @title Update Single Comparison Plot (Internal)
#'
#' @description Creates a single comparison plot with title unit formatting - "%" instead of "Percent".
#'
#' @param data Data frame containing the data for the plot
#' @param x_axis_from Column name to use for x-axis
#' @param plot_title Title for the plot
#' @param unit Unit of measurement
#' @param compare_by_x_axis Logical. If TRUE, compares x-axis values within experiments
#' @param color_tone Base color for the plot
#' @param panel_rows Number of panel rows
#' @param panel_cols Number of panel columns
#' @param legend_position Position of the legend: "none", "bottom", "top", "left", or "right"
#'
#' @return A ggplot object
#'
#' @keywords internal
.create_single_comparison_plot <- function(data, x_axis_from, plot_title, unit,
                                           compare_by_x_axis, color_tone,
                                           panel_rows, panel_cols,
                                           legend_position = "none") {

  n_panels <- if (compare_by_x_axis) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  panel_layout <- if (!is.null(panel_rows) && !is.null(panel_cols)) {
    list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
  } else {
    .calculate_panel_layout(data, panel_rows, panel_cols,
                            compare_by_x_axis, x_axis_from)
  }

  value_range <- range(data$Value)
  y_range <- diff(value_range)

  y_limits <- if (tolower(unit) == "percent") {
    max_abs_value <- max(abs(value_range))
    c(-max_abs_value * 1.15, max_abs_value * 1.15)
  } else {
    c(value_range[1] - 0.1 * y_range, value_range[2] + 0.1 * y_range)
  }

  label_position <- sapply(data$Value, function(x) {
    if(x >= 0) x + diff(y_limits) * 0.03 else x - diff(y_limits) * 0.03
  })

  if (!is.null(color_tone)) {
    color_palette <- .generate_comparison_colors(data, color_tone,
                                                 compare_by_x_axis, x_axis_from)
  }

  # Format the unit for axis label - match detail_plot style
  y_axis_label <- ifelse(tolower(unit) == "percent", "Percentage (%)", unit)

  p <- ggplot2::ggplot(data, ggplot2::aes_string(
    x = if (compare_by_x_axis) "Experiment" else x_axis_from,
    y = "Value",
    fill = if (compare_by_x_axis) "Experiment" else x_axis_from)) +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_text(
      ggplot2::aes(y = label_position,
                   label = sprintf("%.2f", Value)),
      position = ggplot2::position_dodge(width = 0.9),
      size = 5,
      color = "black"
    ) +
    ggplot2::labs(title = plot_title, x = NULL, y = y_axis_label) +
    ggplot2::scale_y_continuous(limits = y_limits, oob = scales::squish) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "black") +
    ggplot2::theme_minimal()

  if (!is.null(color_tone)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  if (n_panels > 1) {
    p <- p + ggplot2::facet_wrap(
      as.formula(
        if (compare_by_x_axis)
          paste("~", x_axis_from)
        else "~ Experiment"
      ),
      scales = "fixed",
      nrow = panel_layout$rows,
      ncol = panel_layout$cols
    )
  }

  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 16, face = "bold",
                                         margin = ggplot2::margin(0, 20, 0, 0)),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold",
                                       margin = ggplot2::margin(10, 0, 10, 0)),
    strip.background = ggplot2::element_rect(fill = "lightgrey"),
    strip.text = ggplot2::element_text(size = 16, face = "bold",
                                       margin = ggplot2::margin(10, 0, 10, 0)),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.spacing.x = ggplot2::unit(2, "cm"),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 14),
    legend.position = legend_position
  )

  return(p)
}


#' @title Generate Comparison Plot
#'
#' @description Creates comparison plots with enhanced features for unit handling and figure separation.
#' Uses a separate dataframe to specify variables to plot and their titles.
#' Auto-adjusts dimensions and panel layout if not specified.
#'
#' @param data A data frame containing the full dataset.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables in the data will be plotted with default titles.
#' @param x_axis_from Character. Column name to use for the x-axis. Default is "Region"
#' @param title_prefix Optional character string to prepend to the title
#' @param title_suffix Optional character string to append to the title
#' @param compare_by_x_axis Logical. If TRUE, compares experiments within x-axis categories
#' @param separate_figure Logical. If TRUE, creates separate figures instead of panels. Default is FALSE
#' @param output_dir Optional character. Directory to save the output plot
#' @param panel_rows Optional numeric. Number of panel rows
#' @param panel_cols Optional numeric. Number of panel columns
#' @param color_tone Optional character. Base color for the plot
#' @param legend_position Character. Position of the legend: "none" (default), "bottom", "top", "left", or "right"
#' @param width Optional numeric. Width of the output plot
#' @param height Optional numeric. Height of the output plot
#'
#' @return A list of ggplot objects or a single ggplot object depending on settings
#'
#' @export
comparison_plot <- function(data, plot_var = NULL,
                            x_axis_from = "Region",
                            title_prefix = "", title_suffix = "",
                            compare_by_x_axis = FALSE, separate_figure = FALSE,
                            output_dir = NULL,
                            panel_rows = NULL, panel_cols = NULL,
                            color_tone = NULL,
                            legend_position = "none",
                            width = NULL, height = NULL) {

  if (!"Unit" %in% names(data)) stop('Missing "Unit" column, see "add_unit_col" function')
  if (!"Variable" %in% names(data)) stop('Missing "Variable" column in the data frame')

  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) stop("plot_var must be a data frame")
    if (!all(c("Variable", "PlotTitle") %in% names(plot_var)))
      stop('plot_var data frame must contain both "Variable" and "PlotTitle" columns')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")
    title_mapping <- setNames(plot_var$PlotTitle, plot_var$Variable)
  } else {
    variables_in_data <- unique(data$Variable)
    title_mapping <- setNames(variables_in_data, variables_in_data)
  }

  variables_to_plot <- unique(data$Variable)
  plot_list <- list()

  for (var in variables_to_plot) {
    var_data <- data[data$Variable == var, ]
    plot_title <- title_mapping[var]

    plot_title <- paste0(
      if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
      plot_title,
      if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
    )

    unit_groups <- split(var_data, var_data$Unit)

    for (unit in names(unit_groups)) {
      unit_data <- unit_groups[[unit]]

      if (separate_figure) {
        split_column <- if (compare_by_x_axis) x_axis_from else "Experiment"
        figure_groups <- split(unit_data, unit_data[[split_column]])

        for (group_name in names(figure_groups)) {
          group_data <- figure_groups[[group_name]]

          # Calculate panel layout
          panel_layout <- list(rows = 1, cols = 1)

          # Auto-calculate dimensions if not specified
          if (is.null(width) || is.null(height)) {
            dims <- .calculate_plot_dimensions(group_data, panel_layout)
            width_val <- ifelse(is.null(width), dims$width, width)
            height_val <- ifelse(is.null(height), dims$height, height)
          } else {
            width_val <- width
            height_val <- height
          }

          # Format title for unit
          group_title <- paste(plot_title, "-", group_name)
          if (tolower(unit) == "percent") {
            # Use % instead of "Percent" in title
            group_title <- paste0(group_title, " (%)")
          } else {
            group_title <- paste0(group_title, " (", unit, ")")
          }

          p <- .create_single_comparison_plot(
            data = group_data,
            x_axis_from = x_axis_from,
            plot_title = group_title,
            unit = unit,
            compare_by_x_axis = compare_by_x_axis,
            color_tone = color_tone,
            panel_rows = 1,
            panel_cols = 1,
            legend_position = legend_position
          )

          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

            clean_unit <- gsub("[^[:alnum:]]", "_", unit)
            filename <- file.path(output_dir,
                                  paste0(var, "_", clean_unit, "_", group_name, ".png"))

            ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                            dpi = 300, bg = "white")
            message("Saved plot: ", filename)
          }

          plot_list[[paste(var, unit, group_name, sep = "_")]] <- p
        }
      } else {
        # Calculate panel layout if not specified
        panel_layout <- if (!is.null(panel_rows) && !is.null(panel_cols)) {
          list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
        } else {
          .calculate_panel_layout(unit_data, panel_rows, panel_cols,
                                  compare_by_x_axis, x_axis_from)
        }

        if (is.null(width) || is.null(height)) {
          dims <- .calculate_plot_dimensions(unit_data, panel_layout)
          width_val <- ifelse(is.null(width), dims$width, width)
          height_val <- ifelse(is.null(height), dims$height, height)
        } else {
          width_val <- width
          height_val <- height
        }

        # Format title with unit
        if (tolower(unit) == "percent") {
          plot_title_with_unit <- paste0(plot_title, " (%)")
        } else {
          plot_title_with_unit <- paste0(plot_title, " (", unit, ")")
        }

        p <- .create_single_comparison_plot(
          data = unit_data,
          x_axis_from = x_axis_from,
          plot_title = plot_title_with_unit,
          unit = unit,
          compare_by_x_axis = compare_by_x_axis,
          color_tone = color_tone,
          panel_rows = panel_layout$rows,
          panel_cols = panel_layout$cols,
          legend_position = legend_position
        )

        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

          clean_unit <- gsub("[^[:alnum:]]", "_", unit)
          filename <- file.path(output_dir, paste0(var, "_", clean_unit, ".png"))

          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white")
          message("Saved plot: ", filename)
        }

        plot_list[[paste(var, unit, sep = "_")]] <- p
      }
    }
  }

  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}



# Detail Plot -------------------------------------------------------------


#' @title Generate Detailed Plot
#'
#' @description Creates a detailed plot reporting all regions or sectors depending on the variable selection.
#' Uses a separate dataframe to specify variables to plot and their titles.
#'
#' @param data A data frame containing the full dataset.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables in the data will be plotted with default titles.
#' @param y_axis_from Character vector. Column names to use for the y-axis.
#' @param figure_separate_by Character. Column used to separate different plots.
#' @param title_prefix Optional character string to prepend to the title.
#' @param title_suffix Optional character string to append to the title.
#' @param panel_rows Optional numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols Optional numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param positive_color Character. Color for positive values.
#' @param negative_color Character. Color for negative values.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param top_impact Optional numeric. Number of top impact factors to include. If left blank, all factors will be included.
#' @param width Optional numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height Optional numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#'
#' @return A `ggplot` object or NULL if no plots could be generated.
#'
#' @export
detail_plot <- function(data, plot_var = NULL,
                        y_axis_from, figure_separate_by,
                        title_prefix = "", title_suffix = "",
                        panel_rows = NULL, panel_cols = NULL,
                        positive_color = "#2E8B57", negative_color = "#CD5C5C",
                        output_dir = NULL, top_impact = NULL,
                        width = NULL, height = NULL) {

  if (!"Unit" %in% names(data)) stop('Missing "Unit" column')
  if (!"Variable" %in% names(data)) stop('Missing "Variable" column in the data frame')

  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) stop("plot_var must be a data frame")
    if (!all(c("Variable", "PlotTitle") %in% names(plot_var)))
      stop('plot_var data frame must contain both "Variable" and "PlotTitle" columns')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")
    title_mapping <- setNames(plot_var$PlotTitle, plot_var$Variable)
  } else {
    variables_in_data <- unique(data$Variable)
    title_mapping <- setNames(variables_in_data, variables_in_data)
  }

  available_y_axis <- intersect(y_axis_from, names(data))
  if (length(available_y_axis) == 0) {
    warning(sprintf("None of the y-axis candidates %s found in data. Available columns: %s",
                    paste(y_axis_from, collapse = ", "),
                    paste(names(data), collapse = ", ")))
    return(NULL)
  }

  y_axis_col <- available_y_axis[1]
  plot_combinations <- unique(data[c(figure_separate_by, "Variable")])

  # Calculate panel layout once for all plots
  num_experiments <- length(unique(data$Experiment))
  if(!is.null(panel_rows)) {
    panel_cols <- ceiling(num_experiments/panel_rows)
  } else if(!is.null(panel_cols)) {
    panel_rows <- ceiling(num_experiments/panel_cols)
  } else {
    layout <- .calculate_panel_layout(data)
    panel_rows <- layout$rows
    panel_cols <- layout$cols
  }

  # Store panel layout for dimension calculation
  panel_layout <- list(rows = panel_rows, cols = panel_cols)

  # Auto-calculate plot dimensions if not specified
  if (is.null(width) || is.null(height)) {
    dims <- .calculate_plot_dimensions(data, panel_layout)
    width_val <- ifelse(is.null(width), dims$width, width)
    height_val <- ifelse(is.null(height), dims$height, height)
  } else {
    width_val <- width
    height_val <- height
  }

  color_palette <- .generate_color_palette(positive_color, negative_color)
  plots_generated <- list()

  for (i in seq_len(nrow(plot_combinations))) {
    var_name <- plot_combinations$Variable[i]
    sep_value <- plot_combinations[[figure_separate_by]][i]

    filtered_data <- dplyr::filter(data, !!rlang::sym(figure_separate_by) == sep_value, Variable == var_name)

    if (!is.null(top_impact)) {
      filtered_data <- .select_top_impact(filtered_data, top_impact, figure_separate_by)
    }

    filtered_data <- dplyr::mutate(
      filtered_data,
      value_category = dplyr::case_when(
        Value > 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_positive",
        Value < 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_negative",
        Value > 0 ~ "normal_positive",
        Value < 0 ~ "normal_negative",
        TRUE ~ "neutral"
      ),
      Label = sprintf("%.2f", Value)
    )

    n_vars <- length(unique(filtered_data[[y_axis_col]]))
    value_range <- range(filtered_data$Value)
    max_abs <- max(abs(value_range))
    y_limits <- c(-max_abs, max_abs)

    unit_value <- filtered_data$Unit[1]
    unit_clean <- ifelse(tolower(unit_value) == "percent", "(%)", paste0("(", unit_value, ")"))
    y_axis_label <- ifelse(tolower(unit_value) == "percent", "Percentage (%)", unit_value)

    plot_title <- title_mapping[var_name]
    plot_title <- paste0(
      if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
      plot_title,
      if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
    )

    plot_title <- paste(plot_title, ":", sep_value, unit_clean)

    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(data = filtered_data,
                        ggplot2::aes_string(x = sprintf("factor(%s, levels = unique(%s))", y_axis_col, y_axis_col),
                                            y = "Value",
                                            fill = "value_category"),
                        width = 0.4) +
      ggplot2::geom_text(data = filtered_data,
                         ggplot2::aes_string(x = sprintf("factor(%s, levels = unique(%s))", y_axis_col, y_axis_col),
                                             y = "Value",
                                             label = "Label"),
                         hjust = ifelse(filtered_data$Value >= 0, -0.2, 1.2),
                         size = 9) +
      ggplot2::facet_wrap(~ Experiment, nrow = panel_rows, ncol = panel_cols) +
      ggplot2::scale_fill_manual(values = color_palette, guide = "none") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = plot_title,
                    y = y_axis_label,
                    x = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 32, face = "bold", hjust = 1),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 32, margin = ggplot2::margin(t = 50), hjust = 0.5, face = "bold"),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 48, face = "bold", margin = ggplot2::margin(b = 30)),
        panel.grid = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "lightgrey"),
        strip.text = ggplot2::element_text(size = 25, face = "bold", margin = ggplot2::margin(10, 0, 10, 0)),
        panel.spacing.x = ggplot2::unit(1, "cm"),
        legend.position = NULL,
        plot.margin = ggplot2::margin(10, 5, 10, 5)
      ) +
      ggplot2::scale_y_continuous(limits = y_limits, expand = ggplot2::expansion(mult = c(0.3, 0.3))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

    plots_generated[[paste(sep_value, var_name, sep = "_")]] <- p

    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      plot_name <- paste0(sep_value, "_", var_name, ".png")
      filename <- file.path(output_dir, plot_name)
      ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                      dpi = 300, bg = "white", limitsize = FALSE)
      message("Saved plot: ", filename)
    }
  }

  if (length(plots_generated) == 1) {
    return(plots_generated[[1]])
  } else if (length(plots_generated) > 1) {
    return(plots_generated)
  } else {
    return(NULL)
  }
}


# Macro Plot --------------------------------------------------------------


#' @title Generate Macro Plot
#'
#' @description Creates figures of macro variables from the Macros header in the SL4.
#' Uses a separate dataframe to specify variables to plot and their titles.
#'
#' @param data A data frame containing the full dataset with macroeconomic variables.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables in the data will be plotted with default titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param title_suffix Optional character string to append to the title.
#' @param compare_by_x_axis Logical. If `TRUE`, compares experiments within x-axis categories.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param panel_rows Optional numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols Optional numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param color_tone Optional character. Base color for the plot. If left blank, a default color will be used.
#' @param separate_figure Logical. If `FALSE`, combines multiple variables into one plot.
#' @param width Optional numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height Optional numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#' @param legend_position Position of the legend: "none", "bottom", "top", "left", or "right"
#'
#' @return A `ggplot` object or a list of plots if `separate_figure = TRUE`.
#'
#' @export
macro_plot <- function(data, plot_var = NULL,
                       title_prefix = "", title_suffix = "",
                       compare_by_x_axis = FALSE, output_dir = NULL,
                       panel_rows = NULL, panel_cols = NULL,
                       color_tone = NULL, separate_figure = FALSE,
                       width = NULL, height = NULL, legend_position = "none") {

  if (!all(c("Experiment", "Variable", "Value") %in% names(data))) {
    stop('Required columns missing. Data must contain "Experiment", "Variable", and "Value" columns')
  }

  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) stop("plot_var must be a data frame")
    if (!all(c("Variable", "PlotTitle") %in% names(plot_var)))
      stop('plot_var data frame must contain both "Variable" and "PlotTitle" columns')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")
    title_mapping <- setNames(plot_var$PlotTitle, plot_var$Variable)
  } else {
    variables_in_data <- unique(data$Variable)
    title_mapping <- setNames(variables_in_data, variables_in_data)
  }

  n_panels <- if (compare_by_x_axis) {
    length(unique(data$Variable))
  } else {
    length(unique(data$Experiment))
  }

  if (!is.null(panel_rows) && !is.null(panel_cols)) {
    panel_layout <- list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
  } else {
    panel_layout <- .calculate_panel_layout(data, panel_rows, panel_cols, compare_by_x_axis, "Experiment")
  }

  panel_rows <- as.numeric(panel_layout$rows)
  panel_cols <- as.numeric(panel_layout$cols)

  if (is.null(width) || is.null(height)) {
    dims <- .calculate_plot_dimensions(data, panel_layout)
    width <- ifelse(is.null(width), dims$width, as.numeric(width))
    height <- ifelse(is.null(height), dims$height, as.numeric(height))
  }

  variables_to_plot <- unique(data$Variable)
  width_bar <- max(0.05, min(0.8, 3 / length(variables_to_plot)))
  width_dodge <- max(0.05, min(0.9, 2.5 / length(variables_to_plot)))

  create_plot <- function(plot_data, vars) {
    value_range <- range(plot_data$Value)
    y_range <- diff(value_range)
    y_limits <- c(value_range[1] - 0.15 * y_range, value_range[2] + 0.15 * y_range)

    label_position <- sapply(plot_data$Value, function(x) {
      if(x >= 0) x + diff(y_limits) * 0.03 else x - diff(y_limits) * 0.03
    })

    var_display_names <- sapply(vars, function(v) title_mapping[v])

    plot_title <- if (length(vars) > 1) {
      paste0(
        if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
        "Global Economic Impacts (%)",
        if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
      )
    } else {
      paste0(
        if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
        var_display_names[1],
        if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
      )
    }

    if (!is.null(color_tone)) {
      color_palette <- .generate_comparison_colors(plot_data, color_tone, compare_by_x_axis, "Variable")
    }

    if (compare_by_x_axis) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Experiment, y = Value, fill = Experiment)) +
        ggplot2::facet_wrap(~ Variable, scales = "fixed",
                            nrow = panel_rows, ncol = panel_cols)
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Value, fill = Variable)) +
        ggplot2::facet_wrap(~ Experiment, scales = "fixed",
                            nrow = panel_rows, ncol = panel_cols)
    }

    p <- p +
      ggplot2::geom_bar(stat = "identity", width = width_bar, position = ggplot2::position_dodge(width = width_dodge)) +
      ggplot2::geom_text(ggplot2::aes(y = label_position, label = sprintf("%.2f", Value)),
                         position = ggplot2::position_dodge(width = width_dodge),
                         size = 9, color = "black") +
      ggplot2::labs(title = plot_title, x = NULL, y = "Percentage (%)")

    if (!is.null(color_tone)) {
      p <- p + ggplot2::scale_fill_manual(values = color_palette)
    }

    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 32),
        axis.title.y = ggplot2::element_text(size = 32, face = "bold", margin = ggplot2::margin(0, 20, 0, 0)),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 48, face = "bold", margin = ggplot2::margin(10, 0, 10, 0)),
        strip.background = ggplot2::element_rect(fill = "lightgrey"),
        strip.text = ggplot2::element_text(size = 25, face = "bold", margin = ggplot2::margin(10, 0, 10, 0)),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.spacing.x = ggplot2::unit(1, "cm"),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 32),
        legend.position = legend_position,
        plot.margin = ggplot2::margin(10, 5, 10, 5)
      ) +
      ggplot2::scale_y_continuous(limits = y_limits, oob = scales::squish, expand = ggplot2::expansion(mult = c(0.3, 0.3))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

    return(p)
  }

  if (separate_figure) {
    plots <- list()

    for (var in variables_to_plot) {
      var_data <- data[data$Variable == var, ]
      p <- create_plot(var_data, var)
      plots[[var]] <- p

      if (!is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        clean_name <- gsub("[^[:alnum:]]", "_", var)
        filename <- file.path(output_dir, paste0("macro_", clean_name, ".png"))

        ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white")
        message("Saved plot: ", filename)
      }
    }

    return(plots)

  } else {
    p <- create_plot(data, variables_to_plot)

    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      filename <- file.path(output_dir, "GTAPMacros.png")
      ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white")
      message("Saved plot: ", filename)
    }

    return(p)
  }
}


# Stack Plot --------------------------------------------------------------

#' @title Generate Color Palette for Stacked Plots (Internal)
#'
#' @description Generates a diverse color palette for stacked bar components, ensuring good contrast between items.
#'
#' @param data Data frame containing the stack value column
#' @param stack_value_from Column name containing the stack categories
#' @param color_tone Optional base color to influence the palette
#'
#' @return A named vector of colors for each stack component
#'
#' @keywords internal
.generate_stack_colors <- function(data, stack_value_from, color_tone = NULL) {
  # Get unique stack components
  components <- unique(data[[stack_value_from]])
  n_components <- length(components)

  if (n_components <= 1) {
    return(setNames(c("#4477AA"), components))
  }

  # Academic color palettes by theme
  academic_palettes <- list(
    # Blues theme (default)
    "blue" = c("#1F77B4", "#AEC7E8", "#3A8FB7", "#6BAED6", "#08519C", "#B6D0E8", "#4292C6", "#C6DBEF"),

    # Greens theme
    "green" = c("#2CA02C", "#98DF8A", "#578B45", "#74C476", "#005A32", "#A1D99B", "#41AB5D", "#C7E9C0"),

    # Reds theme
    "red" = c("#D62728", "#FF9896", "#B82E2E", "#FC8D59", "#7F0000", "#FCBBA1", "#FB6A4A", "#FEE0D2"),

    # Purples theme
    "purple" = c("#9467BD", "#C5B0D5", "#8C6BB1", "#9E9AC8", "#54278F", "#DADAEB", "#807DBA", "#BCBDDC"),

    # Grays theme (professional/neutral)
    "gray" = c("#7F7F7F", "#C7C7C7", "#525252", "#969696", "#252525", "#D9D9D9", "#737373", "#F7F7F7"),

    # Mixed (highly distinguishable)
    "mixed" = c("#4C78A8", "#F58518", "#E45756", "#72B7B2", "#54A24B", "#EECA3B", "#B279A2", "#FF9DA6")
  )

  # Select palette based on color_tone
  selected_palette <- "mixed"  # Default to mixed if no color_tone

  if (!is.null(color_tone)) {
    # Convert color to lowercase for matching
    color_lower <- tolower(color_tone)

    # Match color_tone to a palette theme
    if (grepl("blue|azure|navy|teal|cyan", color_lower)) {
      selected_palette <- "blue"
    } else if (grepl("green|olive|lime|emerald", color_lower)) {
      selected_palette <- "green"
    } else if (grepl("red|crimson|maroon|orange|pink", color_lower)) {
      selected_palette <- "red"
    } else if (grepl("purple|violet|magenta|lavender", color_lower)) {
      selected_palette <- "purple"
    } else if (grepl("gr[ae]y|black|silver", color_lower)) {
      selected_palette <- "gray"
    }

    # If the color is given as hex, try to match to closest palette
    if (startsWith(color_tone, "#")) {
      # Convert hex to RGB
      rgb_col <- grDevices::col2rgb(color_tone) / 255

      # Simple logic to determine the dominant color
      r <- rgb_col[1]
      g <- rgb_col[2]
      b <- rgb_col[3]

      if (max(r, g, b) == r) {
        selected_palette <- "red"
      } else if (max(r, g, b) == g) {
        selected_palette <- "green"
      } else if (max(r, g, b) == b) {
        selected_palette <- "blue"
      }

      # Check for gray (when R, G, B are close to each other)
      if (max(abs(r - g), abs(r - b), abs(g - b)) < 0.1) {
        selected_palette <- "gray"
      }
    }
  }

  # Get the selected palette
  palette <- academic_palettes[[selected_palette]]

  # Ensure we have enough colors by recycling if needed
  if (n_components > length(palette)) {
    palette <- rep(palette, ceiling(n_components / length(palette)))
  }

  # Trim to the number needed and assign names
  palette <- palette[1:n_components]
  return(setNames(palette, components))
}


#' @title Create Single Stack Plot (Internal)
#'
#' @description Creates a single stacked bar plot with given parameters and total labels.
#'
#' @param data Data frame containing the data for the plot
#' @param total_data Data frame containing total values per group
#' @param x_axis_from Column name to use for x-axis
#' @param stack_value_from Column name containing the stack categories
#' @param plot_title Title for the plot
#' @param y_axis_title Label for the y-axis
#' @param compare_by_x_axis Logical. If TRUE, compares x-axis values within experiments
#' @param color_tone Base color for the plot
#' @param panel_rows Number of panel rows
#' @param panel_cols Number of panel columns
#' @param legend_position Position of the legend
#' @param y_limit Optional y-axis limits
#'
#' @return A ggplot object
#'
#' @keywords internal
.create_single_stack_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                      plot_title, y_axis_title,
                                      compare_by_x_axis, color_tone,
                                      panel_rows, panel_cols,
                                      legend_position = "bottom",
                                      y_limit = NULL) {

  n_panels <- if (compare_by_x_axis) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  # Generate better differentiated colors for stacks
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone)

  # Calculate y limits if not provided
  if (is.null(y_limit)) {
    max_total <- max(abs(total_data$Total), na.rm = TRUE)
    y_limit <- c(-max_total * 1.1, max_total * 1.1)
  }

  # Basic plot structure
  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = data,
      ggplot2::aes_string(
        x = if(compare_by_x_axis) "Experiment" else x_axis_from,
        y = "Value",
        fill = stack_value_from
      ),
      position = "stack",
      width = 0.7
    )

  # Add total labels above each stack
  p <- p + ggplot2::geom_text(
    data = total_data,
    ggplot2::aes(
      x = if(compare_by_x_axis) Experiment else !!rlang::sym(x_axis_from),
      y = ifelse(Total >= 0,
                 PositiveTotal + abs(Total) * 0.05,
                 NegativeTotal - abs(Total) * 0.05),
      label = sprintf("Total\n%.2f", Total)
    ),
    vjust = ifelse(total_data$Total >= 0, 0, 1.2),
    size = 5,
    fontface = "bold"
  )

  # Add faceting if needed
  if (n_panels > 1) {
    p <- p + ggplot2::facet_wrap(
      as.formula(if(compare_by_x_axis)
        paste("~", x_axis_from) else "~ Experiment"),
      scales = "free_x",
      nrow = panel_rows,
      ncol = panel_cols
    )
  }

  # Add styling
  p <- p +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 18, angle = 45, hjust = 1, face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 18),
      axis.title.y = ggplot2::element_text(size = 20, face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 32, face = "bold",
                                         margin = ggplot2::margin(b = 15)),
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(size = 18, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 18)
    ) +
    ggplot2::scale_y_continuous(limits = y_limit) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

  # Add titles
  if (!is.null(plot_title)) p <- p + ggplot2::ggtitle(plot_title)
  if (!is.null(y_axis_title)) p <- p + ggplot2::ylab(y_axis_title)

  return(p)
}

#' @title Create Unstacked Plot (Internal)
#'
#' @description Creates a side-by-side bar plot for components with given parameters.
#'
#' @param data Data frame containing the data for the plot
#' @param total_data Data frame containing total values per group
#' @param x_axis_from Column name to use for x-axis
#' @param stack_value_from Column name containing the component categories
#' @param plot_title Title for the plot
#' @param y_axis_title Label for the y-axis
#' @param compare_by_x_axis Logical. If TRUE, compares x-axis values within experiments
#' @param color_tone Base color for the plot
#' @param panel_rows Number of panel rows
#' @param panel_cols Number of panel columns
#' @param legend_position Position of the legend
#' @param y_limit Optional y-axis limits
#'
#' @return A ggplot object
#'
#' @keywords internal
.create_unstacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                   plot_title, y_axis_title,
                                   compare_by_x_axis, color_tone,
                                   panel_rows, panel_cols,
                                   legend_position = "bottom",
                                   y_limit = NULL) {

  n_panels <- if (compare_by_x_axis) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  # Generate better differentiated colors
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone)

  # Calculate y limits if not provided
  if (is.null(y_limit)) {
    value_range <- range(data$Value, na.rm = TRUE)
    max_abs <- max(abs(value_range), na.rm = TRUE)
    y_limit <- c(-max_abs * 1.2, max_abs * 1.2)
  }

  # Format labels for values
  data$Label <- sprintf("%.2f", data$Value)

  # Create the plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = data,
      ggplot2::aes_string(
        x = stack_value_from,
        y = "Value",
        fill = stack_value_from
      ),
      width = 0.7
    ) +
    ggplot2::geom_text(
      data = data,
      ggplot2::aes_string(
        x = stack_value_from,
        y = "Value",
        label = "Label"
      ),
      hjust = ifelse(data$Value >= 0, -0.2, 1.2),
      size = 3.5
    )

  # Add faceting
  facet_formula <- if (compare_by_x_axis) {
    # Facet by region if comparing experiments
    as.formula(paste("~", x_axis_from))
  } else if (n_panels > 1) {
    # Facet by experiment
    as.formula("~ Experiment")
  } else {
    NULL
  }

  if (!is.null(facet_formula)) {
    p <- p + ggplot2::facet_wrap(
      facet_formula,
      scales = "free_y",
      nrow = panel_rows,
      ncol = panel_cols
    )
  }

  # Complete the styling
  p <- p +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 18, face = "bold"),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 18, face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold",
                                         margin = ggplot2::margin(b = 15)),
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(size = 18, face = "bold"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 5, 10, 5)
    ) +
    ggplot2::scale_y_continuous(limits = y_limit, expand = ggplot2::expansion(mult = c(0.2, 0.2))) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

  # Add titles
  if (!is.null(plot_title)) p <- p + ggplot2::ggtitle(plot_title)
  if (!is.null(y_axis_title)) p <- p + ggplot2::ylab(y_axis_title)

  return(p)
}

#' @title Generate Stacked Bar Plot with Auto-Dimensions
#'
#' @description Creates stacked bar plots with automatic total calculations and flexible grouping.
#' Uses a separate dataframe to specify variables to plot and their titles.
#' Auto-adjusts dimensions and panel layout if not specified.
#'
#' @param data A data frame containing the full dataset.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables in the data will be plotted with default titles.
#' @param x_axis_from Character. Column name to use for x-axis grouping
#' @param stack_value_from Character. Column name containing the stack categories
#' @param title_prefix Optional character string to prepend to the title
#' @param title_suffix Optional character string to append to the title
#' @param y_axis_title Character. Y-axis label. If NULL, uses Unit if available.
#' @param compare_by_x_axis Logical. If TRUE, compares x-axis values within experiments
#' @param separate_figure Logical. If TRUE, creates separate figures instead of panels
#' @param unstack_plot Logical. If TRUE, displays components side by side instead of stacked and creates separate figures for each x_axis_from value
#' @param output_dir Optional character. Directory to save the output plot
#' @param panel_rows Optional numeric. Number of panel rows
#' @param panel_cols Optional numeric. Number of panel columns
#' @param color_tone Optional character. Base color for plot color generation
#' @param legend_position Character. Position of the legend: "bottom" (default), "top", "left", "right", or "none"
#' @param y_limit Optional numeric vector. Custom y-axis limits (e.g., c(-5, 5))
#' @param width Optional numeric. Width of the output plot
#' @param height Optional numeric. Height of the output plot
#'
#' @return A list of ggplot objects or a single ggplot object depending on settings
#'
#' @export
stack_plot <- function(data, plot_var = NULL,
                       x_axis_from, stack_value_from,
                       title_prefix = "", title_suffix = "",
                       y_axis_title = NULL,
                       compare_by_x_axis = FALSE,
                       separate_figure = FALSE,
                       unstack_plot = FALSE,
                       output_dir = NULL,
                       panel_rows = NULL,
                       panel_cols = NULL,
                       color_tone = NULL,
                       legend_position = "bottom",
                       y_limit = NULL,
                       width = NULL,
                       height = NULL) {

  # Keep separate_figure independent of unstack_plot

  required_cols <- c("Experiment", "Value", x_axis_from, stack_value_from)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!"Variable" %in% names(data)) {
    stop('Missing "Variable" column in the data frame')
  }

  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) {
      # Convert character vector to data frame if needed
      if (is.character(plot_var)) {
        plot_var <- data.frame(
          Variable = plot_var,
          PlotTitle = plot_var,
          stringsAsFactors = FALSE
        )
      } else {
        stop("plot_var must be a data frame or character vector")
      }
    }

    if (!all(c("Variable", "PlotTitle") %in% names(plot_var)))
      stop('plot_var data frame must contain both "Variable" and "PlotTitle" columns')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")
    title_mapping <- setNames(plot_var$PlotTitle, plot_var$Variable)
  } else {
    variables_in_data <- unique(data$Variable)
    title_mapping <- setNames(variables_in_data, variables_in_data)
  }

  variables_to_plot <- unique(data$Variable)
  plot_list <- list()

  for (var in variables_to_plot) {
    var_data <- data[data$Variable == var, ]
    plot_title <- title_mapping[var]

    plot_title <- paste0(
      if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
      plot_title,
      if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
    )

    # Format plot titles with unit information
    if ("Unit" %in% names(var_data)) {
      unit_value <- unique(var_data$Unit)[1]

      # Add unit to the plot title consistent with comparison_plot
      if (!is.na(unit_value) && unit_value != "") {
        if (tolower(unit_value) == "percent") {
          plot_title <- paste0(plot_title, " (%)")
        } else if (unit_value != "data") {
          plot_title <- paste0(plot_title, " (", unit_value, ")")
        }
      }
    }

    if ("Unit" %in% names(var_data)) {
      unit_groups <- split(var_data, var_data$Unit)
    } else {
      unit_groups <- list(data = var_data)
    }

    # Format y_axis_title based on unit if not provided by user
    if (is.null(y_axis_title) && "Unit" %in% names(var_data)) {
      # Get the unit value
      unit_value <- unique(var_data$Unit)[1]
      # Format the y-axis label like in comparison_plot: "Percentage (%)" for "Percent" unit
      if (tolower(unit_value) == "percent") {
        y_axis_title <- "Percentage (%)"
      } else {
        y_axis_title <- unit_value
      }
    }

    for (unit_name in names(unit_groups)) {
      unit_data <- unit_groups[[unit_name]]

      # Calculate totals for each x-axis group and experiment
      total_data <- unit_data %>%
        dplyr::group_by_at(c("Experiment", x_axis_from)) %>%
        dplyr::summarise(
          Total = sum(Value, na.rm = TRUE),
          PositiveTotal = sum(pmax(Value, 0), na.rm = TRUE),
          NegativeTotal = sum(pmin(Value, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(TotalLabel = sprintf("Total\n%.2f", Total))

      if (separate_figure) {
        split_column <- if (compare_by_x_axis) x_axis_from else "Experiment"
        figure_groups <- split(unit_data, unit_data[[split_column]])

        for (group_name in names(figure_groups)) {
          group_data <- figure_groups[[group_name]]
          group_totals <- total_data[total_data[[split_column]] == group_name, ]

          panel_layout <- list(rows = 1, cols = 1)
          if (is.null(width) || is.null(height)) {
            dims <- .calculate_plot_dimensions(group_data, panel_layout)
            width_val <- ifelse(is.null(width), dims$width, width)
            height_val <- ifelse(is.null(height), dims$height, height)
          } else {
            width_val <- width
            height_val <- height
          }

          # Format title to include unit, consistent with comparison_plot
          group_title <- paste(plot_title, "-", group_name)

          if (unstack_plot) {
            # For unstack_plot + separate_figure, we create a figure for each combo of experiment and x_axis value
            x_axis_values <- unique(group_data[[x_axis_from]])

            for (x_val in x_axis_values) {
              x_data <- group_data[group_data[[x_axis_from]] == x_val, ]
              x_totals <- group_totals[group_totals[[x_axis_from]] == x_val, ]

              p <- .create_unstacked_plot(
                data = x_data,
                total_data = x_totals,
                x_axis_from = x_axis_from,
                stack_value_from = stack_value_from,
                plot_title = paste(plot_title, "-", group_name, "-", x_val),
                y_axis_title = y_axis_title,
                compare_by_x_axis = compare_by_x_axis,
                color_tone = color_tone,
                panel_rows = 1,
                panel_cols = 1,
                legend_position = legend_position,
                y_limit = y_limit
              )

              if (!is.null(output_dir)) {
                if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
                clean_unit <- if ("Unit" %in% names(var_data)) {
                  paste0("_", gsub("[^[:alnum:]]", "_", unit_name))
                } else ""

                filename <- file.path(output_dir,
                                      paste0(var, "_unstack_", group_name, "_", x_val, clean_unit, ".png"))

                ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                                dpi = 300, bg = "white")
                message("Saved plot: ", filename)
              }

              plot_list[[paste(var, unit_name, group_name, x_val, sep = "_")]] <- p
            }

            # Skip the regular plot creation since we've handled each x_axis value separately
            next
          } else {
            p <- .create_single_stack_plot(
              data = group_data,
              total_data = group_totals,
              x_axis_from = x_axis_from,
              stack_value_from = stack_value_from,
              plot_title = group_title,
              y_axis_title = y_axis_title,
              compare_by_x_axis = compare_by_x_axis,
              color_tone = color_tone,
              panel_rows = 1,
              panel_cols = 1,
              legend_position = legend_position,
              y_limit = y_limit
            )
          }

          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
            clean_unit <- if ("Unit" %in% names(var_data)) {
              paste0("_", gsub("[^[:alnum:]]", "_", unit_name))
            } else ""

            plot_type <- if (unstack_plot) "unstack" else "stack"
            filename <- file.path(output_dir,
                                  paste0(var, "_", plot_type, "_", group_name, clean_unit, ".png"))

            ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                            dpi = 300, bg = "white")
            message("Saved plot: ", filename)
          }

          plot_list[[paste(var, unit_name, group_name, sep = "_")]] <- p
        }
      } else {
        if (is.null(panel_rows) || is.null(panel_cols)) {
          layout <- .calculate_panel_layout(unit_data, panel_rows, panel_cols,
                                            compare_by_x_axis, x_axis_from)
          panel_rows <- layout$rows
          panel_cols <- layout$cols
        }

        panel_layout <- list(rows = panel_rows, cols = panel_cols)

        if (is.null(width) || is.null(height)) {
          dims <- .calculate_plot_dimensions(unit_data, panel_layout)
          width_val <- ifelse(is.null(width), dims$width, width)
          height_val <- ifelse(is.null(height), dims$height, height)
        } else {
          width_val <- width
          height_val <- height
        }

        # Format title with unit information is already handled
        plot_title_with_unit <- plot_title

        if (unstack_plot) {
          # For unstack_plot without separate_figure, we make a figure for each x_axis value
          # but each figure still contains all experiments as panels
          x_axis_values <- unique(unit_data[[x_axis_from]])

          for (x_val in x_axis_values) {
            x_data <- unit_data[unit_data[[x_axis_from]] == x_val, ]
            x_totals <- total_data[total_data[[x_axis_from]] == x_val, ]

            p <- .create_unstacked_plot(
              data = x_data,
              total_data = x_totals,
              x_axis_from = x_axis_from,
              stack_value_from = stack_value_from,
              plot_title = paste(plot_title, "-", x_val),
              y_axis_title = y_axis_title,
              compare_by_x_axis = compare_by_x_axis,
              color_tone = color_tone,
              panel_rows = panel_rows,
              panel_cols = panel_cols,
              legend_position = legend_position,
              y_limit = y_limit
            )

            if (!is.null(output_dir)) {
              if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
              clean_unit <- if ("Unit" %in% names(var_data)) {
                paste0("_", gsub("[^[:alnum:]]", "_", unit_name))
              } else ""

              filename <- file.path(output_dir, paste0(var, "_unstack_", x_val, clean_unit, ".png"))

              ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                              dpi = 300, bg = "white")
              message("Saved plot: ", filename)
            }

            plot_list[[paste(var, unit_name, x_val, sep = "_")]] <- p
          }

          next
        } else {
          p <- .create_single_stack_plot(
            data = unit_data,
            total_data = total_data,
            x_axis_from = x_axis_from,
            stack_value_from = stack_value_from,
            plot_title = plot_title_with_unit,
            y_axis_title = y_axis_title,
            compare_by_x_axis = compare_by_x_axis,
            color_tone = color_tone,
            panel_rows = panel_rows,
            panel_cols = panel_cols,
            legend_position = legend_position,
            y_limit = y_limit
          )
        }

        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
          clean_unit <- if ("Unit" %in% names(var_data)) {
            paste0("_", gsub("[^[:alnum:]]", "_", unit_name))
          } else ""

          plot_type <- if (unstack_plot) "unstack" else "stack"
          filename <- file.path(output_dir, paste0(var, "_", plot_type, clean_unit, ".png"))

          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white")
          message("Saved plot: ", filename)
        }

        plot_list[[paste(var, unit_name, sep = "_")]] <- p
      }
    }
  }

  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}
