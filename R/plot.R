#' @title Generate Comparison Plot
#'
#' @description Creates a comparison plot using `ggplot2`, comparing different experiments or categories.
#' A comparison of the selected variable across countries and experiments.
#'
#' @param data A data frame containing the data to plot.
#' @param x_axis_from Character. Column name to use for the x-axis. Default is `"Region"`.
#' @param title_mapping Optional named list mapping variables to display titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param compare_by_x_axis Logical. If `TRUE`, compares experiments within x-axis categories.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param panel_rows **Optional** numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols **Optional** numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param color_tone **Optional** character. Base color for the plot. If left blank, a default color will be used.
#' @param width **Optional** numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height **Optional** numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#'
#' @return A `ggplot` object.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{detail_plot}}, \code{\link{macro_plot}}
#'
#' @export
#'
#' @examples
#' # Import sample data
#' sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Extracting Variable
#' sl4_data <- get_data_by_var(c("qgdp", "ppriv", "EV"), sl4_data1, sl4_data2,
#' merge_data = TRUE)
#'
#' # Adding GTAP Unit Column
#' sl4_data <- add_unit_col(sl4_data, mapping_df = "GTAPunit")
#'
#' # Create variable name mapping
#' title_mapping  <- list(
#' "qgdp" = "GDP",
#' "ppriv" = "Private Consumption",
#' "EV" = "Equivalent Variation")
#'
#' # Plot
#' comparison_plot(sl4_data,
#'                 x_axis_from = "REG",
#'                 title_mapping = title_mapping,
#'                 output_dir = output.dir,
#'                 compare_by_x_axis = FALSE,
#'                 color_tone = "grey",
#'                 panel_cols = 2,
#'                 panel_rows = 1,
#'                 width = 20,
#'                 height = 12)
#'
comparison_plot <- function(data, x_axis_from = "Region",
                            title_mapping = NULL, title_prefix = "",
                            compare_by_x_axis = FALSE, output_dir = NULL,
                            panel_rows = NULL, panel_cols = NULL,
                            color_tone = NULL,
                            width = NULL, height = NULL) {
  if (!"Unit" %in% names(data)) {
    stop('Missing "Unit" column, see "add_unit_col" function')
  }

  n_panels <- if (compare_by_x_axis) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  panel_layout <- if (!is.null(panel_rows) && !is.null(panel_cols)) {
    list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
  } else {
    .calculate_panel_layout(data, panel_rows, panel_cols, compare_by_x_axis, x_axis_from)
  }

  panel_rows <- as.numeric(panel_layout$rows)
  panel_cols <- as.numeric(panel_layout$cols)

  if (is.null(width) || is.null(height)) {
    dims <- .calculate_plot_dimensions(data, panel_layout)
    width <- ifelse(is.null(width), dims$width, as.numeric(width))
    height <- ifelse(is.null(height), dims$height, as.numeric(height))
  }

  unit <- unique(data$Unit)
  if (length(unit) > 1) unit <- unit[1]

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

  display_name <- if (!is.null(title_mapping) && data$Variable[1] %in% names(title_mapping)) {
    title_mapping[[data$Variable[1]]]
  } else {
    data$Variable[1]
  }

  plot_title <- if (nchar(title_prefix) > 0) {
    paste(title_prefix, display_name, paste0("(", unit, ")"))
  } else {
    paste(display_name, paste0("(", unit, ")"))
  }

  if (!is.null(color_tone)) {
    color_palette <- .generate_comparison_colors(data, color_tone, compare_by_x_axis, x_axis_from)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = if (compare_by_x_axis) "Experiment" else x_axis_from,
                                                 y = "Value", fill = if (compare_by_x_axis) "Experiment" else x_axis_from)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_text(ggplot2::aes(y = label_position, label = sprintf("%.2f", Value)),
                       position = ggplot2::position_dodge(width = 0.9), size = 5, color = "black") +
    ggplot2::labs(title = plot_title, x = NULL, y = unit) +
    ggplot2::scale_y_continuous(limits = y_limits, oob = scales::squish) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold", margin = ggplot2::margin(0, 20, 0, 0)),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold", margin = ggplot2::margin(10, 0, 10, 0)),
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(size = 16, face = "bold", margin = ggplot2::margin(10, 0, 10, 0)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing.x = ggplot2::unit(2, "cm"),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "none"
    ) +
    ggplot2::facet_wrap(as.formula(if (compare_by_x_axis) paste("~", x_axis_from) else "~ Experiment"),
                        scales = "fixed", nrow = panel_rows, ncol = panel_cols)

  if (!is.null(color_tone)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    filename <- file.path(output_dir, paste0(data$Variable[1], "_comparison.png"))
    ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white")
    message("Saved plot: ", filename)
  }

  return(p)
}



#' @title Generate Detailed Plot
#'
#' @description Creates a detailed plot reporting all regions or sectors depending on the variable selection.
#'
#' @param data A data frame containing the data to plot.
#' @param y_axis_from Character. Column name to use for the y-axis.
#' @param figure_separate_by Character. Column used to separate different plots.
#' @param title_mapping Optional named list mapping variables to display titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param panel_rows **Optional** numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols **Optional** numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param positive_color Character. Color for positive values.
#' @param negative_color Character. Color for negative values.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param top_impact **Optional** numeric. Number of top impact factors to include. If left blank, all factors will be included.
#' @param width **Optional** numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height **Optional** numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#'
#' @return A `ggplot` object.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{macro_plot}}
#' @export
#'
#' @examples
#' # Import sample data
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- HARplus::load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Extracting Variable
#' sl4_data <- HARplus::get_data_by_var(c("qgdp", "ppriv", "EV"), sl4_data1, sl4_data2,
#' merge_data = TRUE)
#'
#' # Extracting Variable
#' sl4_data <- HARplus::get_data_by_var(c("qo", "pmw"), sl4_data)
#'
#' # Adding GTAP Unit Column
#' sl4_data <- add_unit_col(sl4_data, mapping_df = "GTAPunit")
#'
#' # Create variable name mapping
#' title_mapping  <- list(
#' "qo" = "Output",
#' "pmw" = "Price of Import Index")
#'
#' # Plot
#' detail_plot(sl4_data,
#'                 y_axis_from = c("ACTS", "COMM", "ENDWB"),
#'                 figure_separate_by = "REG",
#'                 title_mapping = title_mapping,
#'                 output_dir = output.dir,
#'                 compare_by_x_axis = FALSE,
#'                 positive_color = "green",
#'                 negative_color = "red",
#'                 panel_cols = 2,
#'                 panel_rows = 1,
#'                 width = 20,
#'                 height = 12)
#'
detail_plot <- function(data, y_axis_from, figure_separate_by,
                        title_mapping = NULL, title_prefix = "",
                        panel_rows = NULL, panel_cols = NULL,
                        positive_color = "#2E8B57", negative_color = "#CD5C5C",
                        output_dir = NULL, top_impact = NULL,
                        width = NULL, height = NULL) {

  if (!"Unit" %in% names(data)) {
    stop('Missing "Unit" column')
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

  color_palette <- .generate_color_palette(positive_color, negative_color)

  for (i in seq_len(nrow(plot_combinations))) {
    var_name <- plot_combinations$Variable[i]
    sep_value <- plot_combinations[[figure_separate_by]][i]

    filtered_data <- dplyr::filter(data, !!rlang::sym(figure_separate_by) == sep_value, Variable == var_name)

    if (!is.null(top_impact)) {
      filtered_data <- select_top_impact(filtered_data, top_impact, figure_separate_by)
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

    if(is.null(width) || is.null(height)) {
      dims <- .calculate_plot_dimensions(filtered_data, list(rows = panel_rows, cols = panel_cols))
      width <- ifelse(is.null(width), dims$width, width)
      height <- ifelse(is.null(height), dims$height, height)
    }

    n_vars <- length(unique(filtered_data[[y_axis_col]]))
    value_range <- range(filtered_data$Value)
    max_abs <- max(abs(value_range))
    y_limits <- c(-max_abs, max_abs)

    unit_value <- filtered_data$Unit[1]
    unit_clean <- ifelse(tolower(unit_value) == "percent", "(%)", paste0("(", unit_value, ")"))
    y_axis_label <- ifelse(tolower(unit_value) == "percent", "Percentage (%)", unit_value)

    display_name <- if (!is.null(title_mapping) && var_name %in% names(title_mapping)) {
      title_mapping[[var_name]]
    } else {
      var_name
    }

    plot_title <- paste(display_name, ":", sep_value, unit_clean)

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
        axis.title.x = ggplot2::element_text(size = 36, margin = ggplot2::margin(t = 40), hjust = 0.5, face = "bold"),
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

    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      plot_name <- paste0(sep_value, "_", var_name, ".png")
      filename <- file.path(output_dir, plot_name)
      ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white", limitsize = FALSE)
      message("Saved plot: ", filename)
    }
  }
}




#' @title Generate Macro Plot
#'
#' @description The figure of macro variables from the Macros header in the SL4.
#'
#' @param data A data frame containing macroeconomic variables.
#' @param title_mapping Optional named list mapping variables to display titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param compare_by_x_axis Logical. If `TRUE`, compares experiments within x-axis categories.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param panel_rows **Optional** numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols **Optional** numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param color_tone **Optional** character. Base color for the plot. If left blank, a default color will be used.
#' @param combine_var Logical. If `TRUE`, combines multiple variables into one plot.
#' @param selected_vars **Optional** character vector. Subset of variables to include. If left blank, all variables will be included.
#' @param width **Optional** numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height **Optional** numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#'
#' @return A `ggplot` object or a list of plots if `combine_var = FALSE`.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{comparison_plot}}, \code{\link{comparison_plot}}, \code{\link{gtap_macros_data}}
#' @export
#'
#' @examples
#' # Import sample data
#' sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- HARplus::load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Extracting Macro Variable
#' Macros <- gtap_macros_data(sl4_data1, sl4_data2)
#'
#' macro_plot(Macros,
#'           color_tone = "grey",
#'           combine_var = TRUE,
#'           output_dir = "output/plots",
#'           selected_vars = c("pxwwld", "qxwwld"),
#'           panel_rows = 1,
#'           width = 45)
#'
macro_plot <- function(data, title_mapping = NULL, title_prefix = "",
                       compare_by_x_axis = FALSE, output_dir = NULL,
                       panel_rows = NULL, panel_cols = NULL,
                       color_tone = NULL, combine_var = TRUE,
                       selected_vars = NULL, width = NULL, height = NULL) {

  if (!all(c("Experiment", "Variable", "Value") %in% names(data))) {
    stop('Required columns missing. Data must contain "Experiment", "Variable", and "Value" columns')
  }

  if (!is.null(selected_vars)) {
    data <- data[data$Variable %in% selected_vars, ]
    if (nrow(data) == 0) {
      stop("No data found for selected variables")
    }
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

  width_bar <- max(0.2, min(0.8, 2.5 / length(selected_vars)))
  width_dodge <- max(0.2, min(0.9, 2 / length(selected_vars)))

  create_plot <- function(plot_data, vars) {
    value_range <- range(plot_data$Value)
    y_range <- diff(value_range)
    y_limits <- c(value_range[1] - 0.15 * y_range, value_range[2] + 0.15 * y_range)

    label_position <- sapply(plot_data$Value, function(x) {
      if(x >= 0) x + diff(y_limits) * 0.03 else x - diff(y_limits) * 0.03
    })

    if (!is.null(title_mapping) && !is.null(selected_vars)) {
      vars <- sapply(vars, function(v) {
        if (v %in% selected_vars && v %in% names(title_mapping)) {
          title_mapping[[v]]
        } else {
          v
        }
      })
    }

    plot_title <- if (length(vars) > 1) {
      if (nchar(title_prefix) > 0) {
        paste(title_prefix, "Macro Variables")
      } else {
        "Macro Variables"
      }
    } else {
      if (nchar(title_prefix) > 0) {
        paste(title_prefix, vars)
      } else {
        vars
      }
    }

    if (!is.null(color_tone)) {
      n_colors <- if(compare_by_x_axis) {
        length(unique(plot_data$Experiment))
      } else {
        length(unique(plot_data$Variable))
      }

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
      ggplot2::labs(title = plot_title, x = NULL, y = "Value")

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
        legend.title = ggplot2::element_text(size = 24),
        legend.text = ggplot2::element_text(size = 24),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(10, 5, 10, 5)
      ) +
      ggplot2::scale_y_continuous(limits = y_limits, oob = scales::squish, expand = ggplot2::expansion(mult = c(0.3, 0.3))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

    return(p)
  }

  if (combine_var) {
    p <- create_plot(data, unique(data$Variable))

    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      filename <- file.path(output_dir, "macro_variables_combined.png")
      ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white")
      message("Saved plot: ", filename)
    }

    return(p)

  } else {
    plots <- list()
    for (var in unique(data$Variable)) {
      var_data <- data[data$Variable == var, ]
      p <- create_plot(var_data, var)
      plots[[var]] <- p

      if (!is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        filename <- file.path(output_dir, paste0("macro_", var, ".png"))
        ggplot2::ggsave(filename, p, width = width, height = height, dpi = 300, bg = "white")
        message("Saved plot: ", filename)
      }
    }

    return(plots)
  }
}
