# Comparison Plot ---------------------------------------------------------

#' @title Create Single Comparison Plot (Internal)
#'
#' @description Creates a single comparison plot with title unit formatting.
#'
#' @param data Data frame containing the data for the plot
#' @param x_axis_from Column name to use for x-axis
#' @param plot_title Title for the plot
#' @param unit Unit of measurement
#' @param compare_by_experiment Logical. If TRUE, compares experiments within x-axis categories
#' @param color_tone Base color for the plot
#' @param panel_rows Number of panel rows
#' @param panel_cols Number of panel columns
#' @param legend_position Position of the legend: "none", "bottom", "top", "left", or "right"
#' @param invert_panel Logical. If TRUE, swaps the roles of Experiment and split_by in faceting
#' @param panel_var Character. Column name to use for panel faceting
#'
#' @return A ggplot object
#'
#' @keywords internal
.create_single_comparison_plot <- function(data, x_axis_from, plot_title, unit,
                                           compare_by_experiment, color_tone,
                                           panel_rows, panel_cols,
                                           legend_position = "none",
                                           invert_panel = FALSE,
                                           panel_var = "Experiment") {

  # For compare_by_experiment=TRUE, swap x-axis and panels
  if (compare_by_experiment) {
    x_var <- "Experiment"
    facet_var <- x_axis_from
  } else {
    x_var <- x_axis_from
    facet_var <- panel_var
  }

  n_panels <- length(unique(data[[facet_var]]))

  # For the single comparison plot, ensure we have a proper panel layout
  n_panels <- length(unique(data[[facet_var]]))

  if (!is.null(panel_rows) && !is.null(panel_cols)) {
    # Check if the provided dimensions are sufficient
    if (panel_rows * panel_cols < n_panels) {
      # Adjust columns to fit all panels
      panel_cols <- ceiling(n_panels / panel_rows)
    }
    panel_layout <- list(rows = panel_rows, cols = panel_cols)
  } else if (!is.null(panel_rows)) {
    panel_layout <- list(rows = panel_rows, cols = ceiling(n_panels / panel_rows))
  } else if (!is.null(panel_cols)) {
    panel_layout <- list(rows = ceiling(n_panels / panel_cols), cols = panel_cols)
  } else {
    # Auto calculate layout
    if (n_panels <= 1) {
      panel_layout <- list(rows = 1, cols = 1)
    } else if (n_panels <= 3) {
      panel_layout <- list(rows = 1, cols = n_panels)
    } else if (n_panels <= 4) {
      panel_layout <- list(rows = 2, cols = 2)
    } else if (n_panels <= 6) {
      panel_layout <- list(rows = 2, cols = 3)
    } else if (n_panels <= 9) {
      panel_layout <- list(rows = 3, cols = 3)
    } else if (n_panels <= 12) {
      panel_layout <- list(rows = 3, cols = 4)
    } else {
      # For larger numbers, try to keep the grid roughly square
      cols <- ceiling(sqrt(n_panels))
      rows <- ceiling(n_panels / cols)
      panel_layout <- list(rows = rows, cols = cols)
    }
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
                                                 compare_by_experiment, x_var)
  }

  # Format the unit for axis label
  y_axis_label <- ifelse(tolower(unit) == "percent", "Percentage (%)", unit)

  p <- ggplot2::ggplot(data, ggplot2::aes_string(
    x = x_var,
    y = "Value",
    fill = x_var)) +
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
      as.formula(paste("~", facet_var)),
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



#' @title Generate Comparison Plots
#'
#' @description Creates bar plots comparing values across categories with flexible panel configuration.
#'
#' @param data Data frame containing "Variable", "Value", "Experiment", "Unit" columns plus x_axis_from and split_by variables.
#' @param plot_var Optional. Character vector or data frame with "Variable" column to specify which variables to plot.
#' @param x_axis_from Column name to use for x-axis values (or panels if compare_by_experiment = TRUE).
#' @param split_by Column name for separating plots (or panels if invert_panel = TRUE).
#' @param title_prefix Optional text to prepend to plot titles.
#' @param title_suffix Optional text to append to plot titles.
#' @param compare_by_experiment If TRUE, Experiment is plotted on x-axis, x_axis_from values as panels.
#' @param separate_figure If TRUE, creates individual figures for each panel value.
#' @param invert_panel If TRUE, uses split_by for panels and Experiment for separation.
#' @param description_as_title If TRUE, uses "Description" column for titles instead of "Variable".
#' @param output_dir Optional path to save plots. If NULL, plots are only returned.
#' @param panel_rows Optional number of panel rows. Auto-calculated if NULL.
#' @param panel_cols Optional number of panel columns. Auto-calculated if NULL.
#' @param color_tone Optional base color for the plot. NULL uses default ggplot2 colors.
#' @param legend_position Position of legend: "none" (default), "bottom", "top", "left", "right".
#' @param width Optional width in inches. Auto-calculated if NULL.
#' @param height Optional height in inches. Auto-calculated if NULL.
#'
#' @return A list of ggplot objects or a single ggplot object depending on settings
#' @examples
#' \dontrun{
#' comparison_plot(sl4.plot.data$REG,
#'                 x_axis_from = "Region",
#'                 plot_var = sl4plot,
#'                 title_prefix = "Impact on",
#'                 output_dir = output.folder,
#'                 compare_by_experiment = FALSE,
#'                 description_as_title = TRUE,
#'                 separate_figure = FALSE,
#'                 color_tone = "grey",
#'                 width = 20,
#'                 height = 12,
#'                 legend_position = "bottom")
#' }
#' @export
comparison_plot <- function(data, plot_var = NULL,
                            x_axis_from,
                            split_by,
                            title_prefix = "", title_suffix = "",
                            compare_by_experiment = FALSE,
                            separate_figure = FALSE,
                            invert_panel = FALSE,
                            description_as_title = FALSE,
                            output_dir = NULL,
                            panel_rows = NULL, panel_cols = NULL,
                            color_tone = NULL,
                            legend_position = "none",
                            width = NULL, height = NULL) {

  if (!"Unit" %in% names(data)) stop('Missing "Unit" column, see "add_unit_col" function')
  if (!"Variable" %in% names(data)) stop('Missing "Variable" column in the data frame')

  # Filter by variables in plot_var if provided
  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) {
      # If plot_var is a character vector of variables
      plot_var <- data.frame(Variable = plot_var, stringsAsFactors = FALSE)
    }

    if (!"Variable" %in% names(plot_var))
      stop('plot_var data frame must contain "Variable" column')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")
  }

  # Validate split_by column exists
  if (!split_by %in% names(data)) {
    stop(paste0('Missing "', split_by, '" column in the data frame'))
  }

  # Get title mapping
  title_mapping <- .get_title_mapping(data, description_as_title)

  # Determine panel and separation variables based on invert_panel
  if (invert_panel) {
    # When inverted, we plot each experiment with regions as panels
    panel_var <- split_by
    separate_var <- "Experiment"
  } else {
    # Normal mode: regions as separate plots, experiments as panels (or vice versa if compare_by_experiment)
    panel_var <- if(compare_by_experiment) x_axis_from else "Experiment"
    separate_var <- split_by
  }

  # Get unique values for separate plots
  separate_values <- unique(data[[separate_var]])
  plot_list <- list()

  for (sep_value in separate_values) {
    # Filter data for the current separate value
    filtered_data <- data[data[[separate_var]] == sep_value, ]

    unit_groups <- split(filtered_data, filtered_data$Unit)

    for (unit_name in names(unit_groups)) {
      unit_data <- unit_groups[[unit_name]]

      # Calculate panel layout
      if (!is.null(panel_rows) && !is.null(panel_cols)) {
        panel_layout <- list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
      } else {
        # Count the number of panels needed
        n_panels <- length(unique(unit_data[[panel_var]]))

        # Calculate layout based on the number of panels
        if (is.null(panel_rows) && is.null(panel_cols)) {
          # Auto calculate both dimensions
          if (n_panels <= 1) {
            panel_layout <- list(rows = 1, cols = 1)
          } else if (n_panels <= 3) {
            panel_layout <- list(rows = 1, cols = n_panels)
          } else if (n_panels <= 4) {
            panel_layout <- list(rows = 2, cols = 2)
          } else if (n_panels <= 6) {
            panel_layout <- list(rows = 2, cols = 3)
          } else if (n_panels <= 9) {
            panel_layout <- list(rows = 3, cols = 3)
          } else if (n_panels <= 12) {
            panel_layout <- list(rows = 3, cols = 4)
          } else {
            # For larger numbers, try to keep the grid roughly square
            cols <- ceiling(sqrt(n_panels))
            rows <- ceiling(n_panels / cols)
            panel_layout <- list(rows = rows, cols = cols)
          }
        } else if (is.null(panel_rows)) {
          # Calculate rows based on columns
          panel_layout <- list(rows = ceiling(n_panels / panel_cols), cols = panel_cols)
        } else {
          # Calculate columns based on rows
          panel_layout <- list(cols = ceiling(n_panels / panel_rows), rows = panel_rows)
        }
      }

      # Auto-calculate dimensions
      if (is.null(width) || is.null(height)) {
        dims <- .calculate_plot_dimensions(unit_data, panel_layout)
        width_val <- ifelse(is.null(width), dims$width, width)
        height_val <- ifelse(is.null(height), dims$height, height)
      } else {
        width_val <- width
        height_val <- height
      }

      # Separate figure or panel plot
      if (separate_figure) {
        panel_values <- unique(unit_data[[panel_var]])

        # Create separate figures for each panel value
        for (panel_val in panel_values) {
          panel_data <- unit_data[unit_data[[panel_var]] == panel_val, ]

          # Format title
          if (invert_panel) {
            # For inverted panels: "Experiment - Region"
            plot_title <- paste0(
              if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
              sep_value, " - ", panel_val,
              if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
            )
          } else {
            # For normal mode: "Region - Experiment" or just "Region" if not comparing by experiment
            plot_title <- paste0(
              if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
              sep_value,
              if (compare_by_experiment) paste0(" - ", panel_val) else "",
              if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
            )
          }

          # Add unit to title
          if (tolower(unit_name) == "percent") {
            plot_title_with_unit <- paste0(plot_title, " (%)")
          } else {
            plot_title_with_unit <- paste0(plot_title, " (", unit_name, ")")
          }

          p <- .create_single_comparison_plot(
            data = panel_data,
            x_axis_from = x_axis_from,
            plot_title = plot_title_with_unit,
            unit = unit_name,
            compare_by_experiment = compare_by_experiment,
            color_tone = color_tone,
            panel_rows = 1,
            panel_cols = 1,
            legend_position = legend_position
          )

          # Save plot
          if (!is.null(output_dir)) {
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

            clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
            clean_panel <- gsub("[^[:alnum:]]", "_", panel_val)
            clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)

            filename <- file.path(output_dir, paste0(clean_sep, "_", clean_panel, "_", clean_unit, ".png"))

            ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                            dpi = 300, bg = "white")
            message("Saved plot: ", filename)
          }

          plot_list[[paste(sep_value, panel_val, unit_name, sep = "_")]] <- p
        }
      } else {
        # Format title for panel plots
        plot_title <- paste0(
          if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
          sep_value,
          if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
        )

        # Add unit to title
        if (tolower(unit_name) == "percent") {
          plot_title_with_unit <- paste0(plot_title, " (%)")
        } else {
          plot_title_with_unit <- paste0(plot_title, " (", unit_name, ")")
        }

        p <- .create_single_comparison_plot(
          data = unit_data,
          x_axis_from = x_axis_from,
          plot_title = plot_title_with_unit,
          unit = unit_name,
          compare_by_experiment = compare_by_experiment,
          color_tone = color_tone,
          panel_rows = panel_layout$rows,
          panel_cols = panel_layout$cols,
          legend_position = legend_position,
          invert_panel = invert_panel,
          panel_var = panel_var
        )

        # Save plot
        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

          clean_sep <- gsub("[^[:alnum:]]", "_", sep_value)
          clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)

          filename <- file.path(output_dir, paste0(clean_sep, "_", clean_unit, ".png"))

          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white")
          message("Saved plot: ", filename)
        }

        plot_list[[paste(sep_value, unit_name, sep = "_")]] <- p
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



# Detail Plot -------------------------------------------------------------


#' @title Filter Top Impact Data
#'
#' @description This internal function filters the top `top_impact` observations
#' within each group in the dataset, selecting an equal number of positive
#' and negative impact values when possible.
#'
#' @details
#' - The function works on both data frames and lists of data frames.
#' - It ensures that `Value` is numeric and checks for required columns.
#' - The function groups data by `Experiment`, `Variable`, `Unit`, and `group_col`.
#' - It selects the top positive and negative values separately, maintaining balance.
#' - If an insufficient number of negative (or positive) values exist, the remaining
#'   quota is filled with the other category.
#' - The final dataset is sorted by `Experiment`, `Variable`, `Unit`, `group_col`,
#'   and `Value`.
#'
#' @param data A data frame or a list of data frames containing the dataset to be filtered.
#' @param top_impact A numeric value indicating the number of top impact factors to retain.
#'   If `NULL`, all data is returned.
#' @param group_col A character string specifying the column to group by when filtering top impacts.
#'
#' @return A filtered data frame (or a list of data frames if `data` is a list),
#'   containing only the top `top_impact` observations for each group.
#'
#' @keywords internal
#'
.filter_top_impact <- function(data, top_impact, group_col) {
  if (inherits(data, "list")) {
    return(lapply(data, function(df) .filter_top_impact(df, top_impact, group_col)))
  }

  if (!inherits(data, "data.frame")) stop("Input must be a data frame or a list of data frames")
  if (!("Value" %in% names(data))) stop("Missing 'Value' column in data frame")
  if (!(group_col %in% names(data))) stop("Grouping column not found in data frame")
  if (!("Experiment" %in% names(data))) stop("Experiment column not found in data frame")
  if (!("Variable" %in% names(data))) stop("Missing 'Variable' column in data frame")
  if (!("Unit" %in% names(data))) stop("Missing 'Unit' column in data frame")

  data$Value <- as.numeric(data$Value)

  if (is.null(top_impact) || nrow(data) <= top_impact) {
    return(data)
  }

  data_grouped <- split(data, interaction(data$Experiment, data$Variable, data$Unit, data[[group_col]], drop = TRUE))

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
      df_pos[order(-df_pos$Value), , drop = FALSE][seq_len(pos_count), , drop = FALSE],
      df_neg[order(df_neg$Value), , drop = FALSE][seq_len(neg_count), , drop = FALSE]
    )
  })

  filtered_data <- do.call(rbind, filtered_list)

  filtered_data$Experiment <- factor(filtered_data$Experiment, levels = unique(filtered_data$Experiment))

  avg_values <- stats::aggregate(Value ~ get(group_col), data = filtered_data, mean, na.rm = TRUE)
  sorted_groups <- avg_values[order(avg_values$Value), 1]
  filtered_data[[group_col]] <- factor(filtered_data[[group_col]], levels = sorted_groups)

  filtered_data <- filtered_data[order(
    filtered_data$Experiment,
    filtered_data$Variable,
    filtered_data$Unit,
    filtered_data[[group_col]],
    filtered_data$Value
  ), ]

  rownames(filtered_data) <- NULL

  return(filtered_data)
}


#' @title Generate Detailed Plot
#'
#' @description Creates a detailed plot reporting all regions or sectors depending on the variable selection.
#' Uses a separate dataframe to specify variables to plot and their titles.
#'
#' @param data A data frame containing the full dataset.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables in the data will be plotted with default titles.
#' @param y_axis_from Character vector. Column names to use for the y-axis.
#' @param split_by Character. Column used to separate different plots.
#' @param title_prefix Optional character string to prepend to the title.
#' @param title_suffix Optional character string to append to the title.
#' @param description_as_title Logical. If TRUE, uses the "Description" column from plot_var as the plot title.
#' @param panel_rows Optional numeric. Number of panel rows. If left blank, it will be automatically adjusted.
#' @param panel_cols Optional numeric. Number of panel columns. If left blank, it will be automatically adjusted.
#' @param positive_color Character. Color for positive values.
#' @param negative_color Character. Color for negative values.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param top_impact Optional numeric. Number of top impact factors to include. If left blank, all factors will be included.
#' @param width Optional numeric. Width of the output plot. If left blank, it will be automatically adjusted.
#' @param height Optional numeric. Height of the output plot. If left blank, it will be automatically adjusted.
#' @param separate_figure Logical. If TRUE, creates separate figures for each combination of variable and category.
#' @param invert_panel Logical. If TRUE, swaps the roles of Experiment and split_by in faceting and figure separation.
#'
#' @return A `ggplot` object or NULL if no plots could be generated.
#'
#' @export
detail_plot <- function(data, plot_var = NULL,
                        y_axis_from, split_by,
                        title_prefix = "", title_suffix = "",
                        description_as_title = TRUE,
                        panel_rows = NULL, panel_cols = NULL,
                        positive_color = "#2E8B57", negative_color = "#CD5C5C",
                        output_dir = NULL, top_impact = NULL,
                        width = NULL, height = NULL,
                        separate_figure = FALSE,
                        invert_panel = FALSE,
                        legend_position = "none",
                        width_dodge = 0.5,
                        width_bar = 0.4) {

  if (!"Unit" %in% names(data)) stop('Missing "Unit" column')
  if (!"Variable" %in% names(data)) stop('Missing "Variable" column in the data frame')

  if (!is.null(top_impact)) {
    data <- .filter_top_impact(data, top_impact, split_by)
  }

  # Filter by variables in plot_var if provided
  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) {
      # If plot_var is a character vector of variables
      plot_var <- data.frame(Variable = plot_var, stringsAsFactors = FALSE)
    }

    if (!"Variable" %in% names(plot_var))
      stop('plot_var data frame must contain "Variable" column')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) return(NULL)
  }

  # Get title mapping directly from the data
  title_mapping <- .get_title_mapping(data)

  available_y_axis <- intersect(y_axis_from, names(data))
  if (length(available_y_axis) == 0) {
    return(NULL)
  }

  y_axis_col <- available_y_axis[1]

  if (invert_panel) {
    panel_var <- split_by
    separate_var <- "Experiment"
  } else {
    panel_var <- "Experiment"
    separate_var <- split_by
  }

  if (separate_figure) {
    if (invert_panel) {
      plot_combinations <- unique(data[c("Experiment", "Variable", split_by)])
    } else {
      plot_combinations <- unique(data[c(split_by, "Variable", "Experiment")])
    }
  } else {
    plot_combinations <- unique(data[c(separate_var, "Variable")])
  }

  if (separate_figure) {
    panel_rows <- 1
    panel_cols <- 1
  } else {
    num_panels <- length(unique(data[[panel_var]]))
    layout <- .calculate_panel_layout(data, panel_rows, panel_cols, FALSE, panel_var)
    panel_rows <- layout$rows
    panel_cols <- layout$cols
  }

  panel_layout <- list(rows = panel_rows, cols = panel_cols)

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

  # Plot Setup
  if (!is.null(top_impact) && !separate_figure) {
    value.label.font.size = 7
    axis.text.y = ggplot2::element_text(size = 26, face = "plain", hjust = 1)
    strip.text = ggplot2::element_text(size = 25, face = "bold", margin = ggplot2::margin(10, 0, 10, 0))
    top_n_name = paste0("_top", top_impact)
  } else {
    value.label.font.size <- 9
    axis.text.y = ggplot2::element_text(size = 32, face = "bold", hjust = 1)
    strip.text = ggplot2::element_text(size = 25, face = "bold", margin = ggplot2::margin(10, 0, 10, 0))
    top_n_name = NULL
  }

  for (i in seq_len(nrow(plot_combinations))) {
    var_name <- plot_combinations$Variable[i]

    # Get separation value based on invert_panel setting
    if (invert_panel) {
      sep_value <- plot_combinations$Experiment[i]
      panel_value <- if (separate_figure && split_by %in% names(plot_combinations)) {
        plot_combinations[[split_by]][i]
      } else {
        NULL
      }
    } else {
      sep_value <- plot_combinations[[split_by]][i]
      panel_value <- if (separate_figure && "Experiment" %in% names(plot_combinations)) {
        plot_combinations$Experiment[i]
      } else {
        NULL
      }
    }

    # Filter data based on invert_panel setting
    if (separate_figure) {
      if (invert_panel) {
        # When panels are switched, filter by Experiment and panel value
        if (!is.null(panel_value)) {
          filtered_data <- data[
            data$Experiment == sep_value &
              data$Variable == var_name &
              data[[split_by]] == panel_value,
          ]
        } else {
          filtered_data <- data[
            data$Experiment == sep_value &
              data$Variable == var_name,
          ]
        }
      } else {
        # Default - filter by split_by and panel value
        if (!is.null(panel_value)) {
          filtered_data <- data[
            data[[split_by]] == sep_value &
              data$Variable == var_name &
              data$Experiment == panel_value,
          ]
        } else {
          filtered_data <- data[
            data[[split_by]] == sep_value &
              data$Variable == var_name,
          ]
        }
      }
    } else {
      if (invert_panel) {
        filtered_data <- data[
          data$Experiment == sep_value &
            data$Variable == var_name,
        ]
      } else {
        filtered_data <- data[
          data[[split_by]] == sep_value &
            data$Variable == var_name,
        ]
      }
    }

    if (nrow(filtered_data) == 0) {
      next
    }

    filtered_data <- dplyr::mutate(
      filtered_data,
      value_category = dplyr::case_when(
        Value > 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_positive",
        Value < 0 & abs(Value) >= 0.7 * max(abs(Value)) ~ "extreme_negative",
        Value > 0 ~ "normal_positive",
        Value < 0 ~ "normal_negative",
        TRUE ~ "neutral"
      )
    )

    max_abs_value <- max(abs(filtered_data$Value))
    filtered_data$Label <- sprintf("%.2f", filtered_data$Value)

    n_vars <- length(unique(filtered_data[[y_axis_col]]))
    y_limits <- c(-max_abs_value * 1.2, max_abs_value * 1.2)

    unit_value <- filtered_data$Unit[1]
    unit_clean <- ifelse(tolower(unit_value) == "percent", "(%)", paste0("(", unit_value, ")"))
    y_axis_label <- ifelse(tolower(unit_value) == "percent", "Percentage (%)", unit_value)

    plot_title <- title_mapping[var_name]
    plot_title <- paste0(
      if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
      plot_title,
      if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
    )

    # Construct plot title based on invert_panel setting
    if (separate_figure) {
      if (invert_panel) {
        if (!is.null(panel_value)) {
          plot_title <- paste(plot_title, ":", sep_value, "-", panel_value, unit_clean)
        } else {
          plot_title <- paste(plot_title, ":", sep_value, unit_clean)
        }
      } else {
        if (!is.null(panel_value)) {
          plot_title <- paste(plot_title, ":", sep_value, "-", panel_value, unit_clean)
        } else {
          plot_title <- paste(plot_title, ":", sep_value, unit_clean)
        }
      }
    } else {
      plot_title <- paste(plot_title, ":", sep_value, unit_clean)
    }

    if (!is.null(top_impact) && !separate_figure) {
      panel_list <- split(filtered_data, filtered_data[[panel_var]])

      processed_data <- do.call(rbind, lapply(panel_list, function(panel_df) {
        panel_df_sorted <- panel_df[order(panel_df$Value), ]
        panel_df_sorted[[paste0(y_axis_col, "_factor")]] <- factor(
          panel_df_sorted[[y_axis_col]],
          levels = panel_df_sorted[[y_axis_col]]
        )

        return(panel_df_sorted)
      }))

      filtered_data <- processed_data
    } else {
      y_axis_levels <- unique(filtered_data[[y_axis_col]])
      filtered_data[[paste0(y_axis_col, "_factor")]] <- factor(
        filtered_data[[y_axis_col]],
        levels = y_axis_levels
      )
    }

    y_factor_col <- paste0(y_axis_col, "_factor")

    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 1:n_vars + 0.5, color = "gray70", linewidth = 0.4) +
      ggplot2::geom_col(
        data = filtered_data,
        mapping = ggplot2::aes(
          x = !!rlang::sym(y_factor_col),
          y = Value,
          fill = value_category
        ),
        width = width_bar,
        position = ggplot2::position_dodge(width = width_dodge)
      ) +
      ggplot2::geom_text(
        data = filtered_data,
        mapping = ggplot2::aes(
          x = !!rlang::sym(y_factor_col),
          y = Value,
          label = Label
        ),
        hjust = ifelse(filtered_data$Value >= 0, -0.2, 1.2),
        position = ggplot2::position_dodge(width = width_dodge),
        size = value.label.font.size
      ) +
      ggplot2::scale_fill_manual(values = color_palette, guide = "none") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = plot_title,
                    y = y_axis_label,
                    x = "")

    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = axis.text.y,
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 32, margin = ggplot2::margin(t = 50), hjust = 0.5, face = "bold"),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 48, face = "bold", margin = ggplot2::margin(b = 30)),
        panel.grid = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "lightgrey"),
        strip.text = strip.text,
        panel.spacing.x = ggplot2::unit(1, "cm"),
        legend.position = legend_position,
        plot.margin = ggplot2::margin(t = 10, r = 5, b = 10, l = 5, unit = "pt")
      )

    p <- p +
      ggplot2::scale_y_continuous(limits = y_limits, expand = ggplot2::expansion(mult = c(0.2, 0.2))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

    if (!separate_figure) {
      if (!is.null(top_impact)) {
        p <- p + ggplot2::facet_wrap(as.formula(paste("~", panel_var)),
                                     nrow = panel_rows, ncol = panel_cols, scales = "free_y")
      } else {
        p <- p + ggplot2::facet_wrap(as.formula(paste("~", panel_var)),
                                     nrow = panel_rows, ncol = panel_cols)
      }
    }

    # Generate plot key based on invert_panel setting
    if (separate_figure) {
      if (invert_panel) {
        plot_key <- if (!is.null(panel_value)) {
          paste(sep_value, var_name, panel_value, sep = "_")
        } else {
          paste(sep_value, var_name, sep = "_")
        }
      } else {
        plot_key <- if (!is.null(panel_value)) {
          paste(sep_value, var_name, panel_value, sep = "_")
        } else {
          paste(sep_value, var_name, sep = "_")
        }
      }
    } else {
      plot_key <- paste(sep_value, var_name, sep = "_")
    }

    plots_generated[[plot_key]] <- p

    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

      # Generate filename based on invert_panel setting
      plot_name <- if (separate_figure) {
        if (invert_panel) {
          if (!is.null(panel_value)) {
            paste0(sep_value, "_", var_name, "_", panel_value, top_n_name, ".png")
          } else {
            paste0(sep_value, "_", var_name, top_n_name, ".png")
          }
        } else {
          if (!is.null(panel_value)) {
            paste0(sep_value, "_", var_name, "_", panel_value, top_n_name, ".png")
          } else {
            paste0(sep_value, "_", var_name, top_n_name, ".png")
          }
        }
      } else {
        paste0(sep_value, "_", var_name, top_n_name, ".png")
      }

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


#' @title Generate Macro Plot with Flexible Title Options
#'
#' @description Creates figures of macro variables from the Macros header in the SL4.
#' Uses a separate dataframe to specify variables to plot with options for using either variable names,
#' PlotTitle, or Description columns for plot titles.
#'
#' @param data A data frame containing the full dataset with macroeconomic variables.
#' @param plot_var A data frame with "Variable" column and optionally "PlotTitle" or "Description" columns
#'        specifying which variables to plot and their titles. If NULL, all variables in the data will be
#'        plotted with default titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param title_suffix Optional character string to append to the title.
#' @param compare_by_experiment Logical. If `TRUE`, compares experiments within x-axis categories.
#' @param description_as_title Logical. If TRUE, uses the "Description" column for plot titles instead
#'        of "PlotTitle". Default is FALSE.
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
#' @examples
#' \dontrun{
#' macro_plot(Macros,
#'            plot_var = macro.map,
#'            compare_by_experiment = FALSE,
#'            color_tone = "grey",
#'            output_dir = output.folder,
#'            description_as_title = FALSE,
#'            panel_rows = 2,
#'            width = 45,
#'            separate_figure = FALSE,
#'            legend_position = "bottom")
#' }
#' @export
macro_plot <- function(data, plot_var = NULL,
                       title_prefix = "", title_suffix = "",
                       compare_by_experiment = FALSE,
                       description_as_title = FALSE,
                       output_dir = NULL,
                       panel_rows = NULL, panel_cols = NULL,
                       color_tone = NULL, separate_figure = FALSE,
                       width = NULL, height = NULL, legend_position = "none") {

  if (!all(c("Experiment", "Variable", "Value") %in% names(data))) {
    stop('Required columns missing. Data must contain "Experiment", "Variable", and "Value" columns')
  }

  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) stop("plot_var must be a data frame")
    if (!all("Variable" %in% names(plot_var)))
      stop('plot_var data frame must contain "Variable" column')

    if (description_as_title && !"Description" %in% names(plot_var))
      stop('plot_var data frame must contain "Description" column when description_as_title=TRUE')

    if (!description_as_title && !"PlotTitle" %in% names(plot_var) && !"Description" %in% names(plot_var))
      stop('plot_var data frame must contain either "PlotTitle" or "Description" column')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) stop("No matching variables found in the data")

    title_col <- if (description_as_title && "Description" %in% names(plot_var)) {
      "Description"
    } else if (!description_as_title && "PlotTitle" %in% names(plot_var)) {
      "PlotTitle"
    } else if ("Description" %in% names(plot_var)) {
      "Description"
    } else {
      NULL
    }

    if (!is.null(title_col)) {
      title_mapping <- setNames(plot_var[[title_col]], plot_var$Variable)
    } else {
      title_mapping <- setNames(plot_var$Variable, plot_var$Variable)
    }
  } else {
    variables_in_data <- unique(data$Variable)
    title_mapping <- setNames(variables_in_data, variables_in_data)
  }

  n_panels <- if (compare_by_experiment) {
    length(unique(data$Variable))
  } else {
    length(unique(data$Experiment))
  }

  if (!is.null(panel_rows) && !is.null(panel_cols)) {
    panel_layout <- list(rows = as.numeric(panel_rows), cols = as.numeric(panel_cols))
  } else {
    panel_layout <- .calculate_panel_layout(data, panel_rows, panel_cols, compare_by_experiment, "Experiment")
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
      color_palette <- .generate_comparison_colors(plot_data, color_tone, compare_by_experiment, "Variable")
    }

    if (compare_by_experiment) {
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
#' @param compare_by_experiment Logical. If TRUE, compares x-axis values within experiments
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
                                      compare_by_experiment, color_tone,
                                      panel_rows, panel_cols,
                                      show_total = TRUE,
                                      legend_position = "bottom",
                                      y_limit = NULL) {

  n_panels <- if (compare_by_experiment) {
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
        x = if(compare_by_experiment) "Experiment" else x_axis_from,
        y = "Value",
        fill = stack_value_from
      ),
      position = "stack",
      width = 0.7
    )

  # Add total labels above each stack
  if (show_total) {
    p <- p + ggplot2::geom_text(
      data = total_data,
      ggplot2::aes(
        x = if(compare_by_experiment) Experiment else !!rlang::sym(x_axis_from),
        y = ifelse(Total >= 0,
                   PositiveTotal + abs(Total) * 0.05,
                   NegativeTotal - abs(Total) * 0.05),
        label = sprintf("Total\n%.2f", Total)
      ),
      vjust = ifelse(total_data$Total >= 0, 0, 1.5),
      size = 5,
      fontface = "bold"
    )
  }

  # Add faceting if needed
  if (n_panels > 1) {
    p <- p + ggplot2::facet_wrap(
      as.formula(if(compare_by_experiment)
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
#' @param compare_by_experiment Logical. If TRUE, compares x-axis values within experiments
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
                                   compare_by_experiment, color_tone,
                                   panel_rows, panel_cols,
                                   legend_position = "bottom",
                                   y_limit = NULL) {

  n_panels <- if (compare_by_experiment) {
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
      size = 7
    )

  # Add faceting
  facet_formula <- if (compare_by_experiment) {
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
      plot.title = ggplot2::element_text(hjust = 0.5, size = 32, face = "bold",
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
#' @param compare_by_experiment Logical. If TRUE, compares x-axis values within experiments
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
                       compare_by_experiment = FALSE,
                       separate_figure = FALSE,
                       unstack_plot = FALSE,
                       output_dir = NULL,
                       panel_rows = NULL,
                       panel_cols = NULL,
                       color_tone = NULL,
                       show_total = TRUE,
                       description_as_title = TRUE,
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

  title_mapping <- .get_title_mapping(data, description_as_title)

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
      total_data <- dplyr::group_by_at(unit_data, c("Experiment", x_axis_from))
      total_data <- dplyr::summarise(
        total_data,
        Total = sum(Value, na.rm = TRUE),
        PositiveTotal = sum(pmax(Value, 0), na.rm = TRUE),
        NegativeTotal = sum(pmin(Value, 0), na.rm = TRUE),
        .groups = "drop"
      )
      total_data <- dplyr::mutate(total_data, TotalLabel = sprintf("Total\n%.2f", Total))

      if (separate_figure) {
        split_column <- if (compare_by_experiment) x_axis_from else "Experiment"
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
                compare_by_experiment = compare_by_experiment,
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
              compare_by_experiment = compare_by_experiment,
              color_tone = color_tone,
              panel_rows = 1,
              panel_cols = 1,
              legend_position = legend_position,
              show_total = show_total,
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
                                            compare_by_experiment, x_axis_from)
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
              compare_by_experiment = compare_by_experiment,
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
            compare_by_experiment = compare_by_experiment,
            color_tone = color_tone,
            panel_rows = panel_rows,
            panel_cols = panel_cols,
            legend_position = legend_position,
            show_total = show_total,
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



# Multi Variable Plot -----------------------------------------------------

#' @title Create Multi-Variable Plot
#' @description A helper function to generate a faceted bar plot comparing multiple variables across experiments.
#'
#' @param data A data frame containing variables, values, and experiment labels.
#' @param plot_title A character string specifying the plot title.
#' @param y_axis_label A character string specifying the y-axis label.
#' @param color_tone A character string specifying the color scheme (default: NULL).
#' @param panel_rows An integer specifying the number of rows in facet wrapping (default: 1).
#' @param panel_cols An integer specifying the number of columns in facet wrapping (default: 1).
#' @param same_scale A logical value indicating whether to use the same y-axis scale across facets (default: FALSE).
#' @param legend_position A character string specifying the legend position (default: "bottom").
#'
#' @return A ggplot object representing the multi-variable comparison plot.
#' @keywords internal
#'
.create_multi_variable_plot <- function(data, plot_title, y_axis_label,
                                        color_tone = NULL,
                                        panel_rows = 1,
                                        panel_cols = 1,
                                        same_scale = FALSE,
                                        legend_position = "bottom") {
  # Generate colors based on variables
  if (!is.null(color_tone)) {
    color_palette <- .generate_comparison_colors(
      data, color_tone, compare_by_experiment = FALSE, x_axis_from = "Variable"
    )
  } else {
    n_colors <- length(unique(data$Variable))
    color_palette <- scales::hue_pal()(n_colors)
    names(color_palette) <- unique(data$Variable)
  }

  # Create plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = VariableLabel, y = Value, fill = VariableLabel)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::facet_wrap(~ Experiment, scales = ifelse(same_scale, "fixed", "free_y"),
                        nrow = panel_rows, ncol = panel_cols) +
    ggplot2::labs(title = plot_title, x = NULL, y = y_axis_label) +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines"),
      legend.position = legend_position,
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

  # Add value labels
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", Value), y = Value + ifelse(Value >= 0, 0.1, -0.1)),
    position = ggplot2::position_dodge(width = 0.7),
    angle = 90,
    hjust = ifelse(data$Value >= 0, 0, 1),
    size = 3
  )

  return(p)
}


#' @title Generate Multi-Variable Plot for Regions
#'
#' @description Creates plots showing multiple variables for a specific region across experiments.
#' Designed to work with data from get_data_by_dims for the REG dimension.
#'
#' @param data A data frame containing GTAP output data with Region, Variable, and Value columns.
#' @param region Character. The specific region/country to plot.
#' @param plot_var A data frame with "Variable" and "PlotTitle" columns specifying which variables to plot and their titles.
#'        If NULL, all variables for the given region will be plotted with default titles.
#' @param title_prefix Optional character string to prepend to the title.
#' @param title_suffix Optional character string to append to the title.
#' @param sort_by Character. How to sort the variables: "name" (alphabetical), "value" (by magnitude),
#'        or "none" (as provided). Default is "value".
#' @param limit_vars Numeric. Maximum number of variables to include. If NULL, all variables are included.
#' @param output_dir Optional character. Directory to save the output plot.
#' @param color_tone Optional character. Base color for the plot.
#' @param same_scale Logical. If TRUE, all variables use the same y-axis scale. Default is FALSE.
#' @param panel_rows Optional numeric. Number of panel rows for the plot.
#' @param panel_cols Optional numeric. Number of panel columns for the plot.
#' @param width Optional numeric. Width of the output plot.
#' @param height Optional numeric. Height of the output plot.
#' @param legend_position Character. Position of the legend: "none", "bottom", "top", "left", or "right".
#'
#' @return A ggplot object or a list of ggplot objects (one per unit type).
#'
#' @export
multi_variable_plot <- function(data,
                                x_axis_from,
                                plot_var = NULL,
                                title_prefix = "", title_suffix = "",
                                separate_figure = FALSE,
                                sort_by = "value",
                                limit_vars = NULL,
                                output_dir = NULL,
                                color_tone = NULL,
                                same_scale = FALSE,
                                panel_rows = NULL,
                                panel_cols = NULL,
                                width = NULL,
                                height = NULL,
                                legend_position = "bottom") {

  if (!x_axis_from %in% names(data)) stop(paste0('Missing "', x_axis_from, '" column in the data frame'))
  if (!"Variable" %in% names(data)) stop('Missing "Variable" column in the data frame')
  if (!"Value" %in% names(data)) stop('Missing "Value" column in the data frame')
  if (!"Experiment" %in% names(data)) stop('Missing "Experiment" column in the data frame')

  # Filter by variables in plot_var if provided
  if (!is.null(plot_var)) {
    if (!is.data.frame(plot_var)) {
      # If plot_var is a character vector of variables
      plot_var <- data.frame(Variable = plot_var, stringsAsFactors = FALSE)
    }

    if (!"Variable" %in% names(plot_var))
      stop('plot_var data frame must contain "Variable" column')

    data <- data[data$Variable %in% plot_var$Variable, ]
    if (nrow(data) == 0) return(NULL)
  }

  # Get title mapping
  title_mapping <- .get_title_mapping(data)

  # Add Unit column if it doesn't exist
  if (!"Unit" %in% names(data)) {
    data$Unit <- "Value"
  }

  # Sort variables based on the sort_by parameter
  if (sort_by == "value") {
    # Calculate mean absolute value for each variable
    var_means <- aggregate(abs(Value) ~ Variable, data = data, FUN = mean)
    sorted_vars <- var_means$Variable[order(var_means$`abs(Value)`, decreasing = TRUE)]
    data$Variable <- factor(data$Variable, levels = sorted_vars)
  } else if (sort_by == "name") {
    # Sort alphabetically
    sorted_vars <- sort(unique(data$Variable))
    data$Variable <- factor(data$Variable, levels = sorted_vars)
  }

  # Limit number of variables if specified
  if (!is.null(limit_vars) && limit_vars > 0) {
    var_levels <- levels(data$Variable)
    if (length(var_levels) > limit_vars) {
      data <- data[data$Variable %in% var_levels[1:limit_vars], ]
      data$Variable <- factor(data$Variable)  # Drop unused levels
    }
  }

  # Split by x_axis_from and unit for multiple plots
  x_axis_groups <- split(data, data[[x_axis_from]])
  plot_list <- list()

  for (x_axis_value in names(x_axis_groups)) {
    x_axis_data <- x_axis_groups[[x_axis_value]]
    unit_groups <- split(x_axis_data, x_axis_data$Unit)

    for (unit_name in names(unit_groups)) {
      unit_data <- unit_groups[[unit_name]]

      # Create VariableLabel column using title mapping
      unit_data$VariableLabel <- sapply(unit_data$Variable, function(v) {
        title_mapping[v]
      })

      # Ensure VariableLabel is a factor with original Variable order
      unit_data$VariableLabel <- factor(
        unit_data$VariableLabel,
        levels = title_mapping[levels(unit_data$Variable)]
      )

      if (separate_figure) {
        # Single plot per figure
        panel_layout <- list(rows = 1, cols = 1)

        # Auto-calculate dimensions
        if (is.null(width) || is.null(height)) {
          dims <- .calculate_plot_dimensions(unit_data, panel_layout)
          width_val <- ifelse(is.null(width), dims$width, width)
          height_val <- ifelse(is.null(height), dims$height, height)
        } else {
          width_val <- width
          height_val <- height
        }

        # Create plot title
        plot_title <- paste0(
          if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
          x_axis_value, " Variables",
          if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
        )

        if (!is.null(unit_name) && nchar(unit_name) > 0) {
          if (tolower(unit_name) == "percent") {
            plot_title <- paste0(plot_title, " (%)")
          } else {
            plot_title <- paste0(plot_title, " (", unit_name, ")")
          }
        }

        p <- .create_multi_variable_plot(
          data = unit_data,
          plot_title = plot_title,
          y_axis_label = unit_name,
          color_tone = color_tone,
          panel_rows = 1,
          panel_cols = 1,
          same_scale = same_scale,
          legend_position = legend_position
        )

        # Save plot
        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }

          clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
          clean_x_axis <- gsub("[^[:alnum:]]", "_", x_axis_value)
          filename <- file.path(output_dir, paste0("multi_var_", clean_x_axis, "_", clean_unit, ".png"))

          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white")
          message("Saved plot: ", filename)
        }

        plot_list[[paste(x_axis_value, unit_name, sep="_")]] <- p
      } else {
        # Calculate panel layout for non-separate figure
        n_experiments <- length(unique(unit_data$Experiment))

        if (!is.null(panel_rows) && !is.null(panel_cols)) {
          # Use specified dimensions
        } else if (!is.null(panel_rows)) {
          panel_cols <- ceiling(n_experiments / panel_rows)
        } else if (!is.null(panel_cols)) {
          panel_rows <- ceiling(n_experiments / panel_cols)
        } else {
          # Auto-calculate layout
          layout <- .calculate_panel_layout(unit_data)
          panel_rows <- layout$rows
          panel_cols <- layout$cols
        }

        panel_layout <- list(rows = panel_rows, cols = panel_cols)

        # Auto-calculate dimensions
        if (is.null(width) || is.null(height)) {
          n_vars <- length(unique(unit_data$Variable))
          auto_width <- max(12, 8 + n_vars * 0.4)
          auto_height <- max(8, 6 + n_experiments * 0.8)

          width_val <- ifelse(is.null(width), auto_width, width)
          height_val <- ifelse(is.null(height), auto_height, height)
        } else {
          width_val <- width
          height_val <- height
        }

        # Create plot title
        plot_title <- paste0(
          if (nchar(title_prefix) > 0) paste0(title_prefix, " ") else "",
          x_axis_value, " Variables",
          if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""
        )

        if (!is.null(unit_name) && nchar(unit_name) > 0) {
          if (tolower(unit_name) == "percent") {
            plot_title <- paste0(plot_title, " (%)")
          } else {
            plot_title <- paste0(plot_title, " (", unit_name, ")")
          }
        }

        p <- .create_multi_variable_plot(
          data = unit_data,
          plot_title = plot_title,
          y_axis_label = unit_name,
          color_tone = color_tone,
          panel_rows = panel_rows,
          panel_cols = panel_cols,
          same_scale = same_scale,
          legend_position = legend_position
        )

        # Save plot
        if (!is.null(output_dir)) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }

          clean_unit <- gsub("[^[:alnum:]]", "_", unit_name)
          clean_x_axis <- gsub("[^[:alnum:]]", "_", x_axis_value)
          filename <- file.path(output_dir, paste0("multi_var_", clean_x_axis, "_", clean_unit, ".png"))

          ggplot2::ggsave(filename, p, width = width_val, height = height_val,
                          dpi = 300, bg = "white")
          message("Saved plot: ", filename)
        }

        plot_list[[paste(x_axis_value, unit_name, sep="_")]] <- p
      }
    }
  }

  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}

# Reuse the helper function from previous implementation
.create_multi_variable_plot <- function(data, plot_title, y_axis_label,
                                        color_tone = NULL,
                                        panel_rows = 1,
                                        panel_cols = 1,
                                        same_scale = FALSE,
                                        legend_position = "bottom") {
  # Generate colors based on variables
  if (!is.null(color_tone)) {
    color_palette <- .generate_comparison_colors(
      data, color_tone, compare_by_experiment = FALSE, x_axis_from = "Variable"
    )
  } else {
    n_colors <- length(unique(data$Variable))
    color_palette <- scales::hue_pal()(n_colors)
    names(color_palette) <- unique(data$Variable)
  }

  # Prepare y-axis label
  y_axis_label <- if (tolower(y_axis_label) == "percent") {
    "Percentage (%)"
  } else {
    y_axis_label
  }

  # Create plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = VariableLabel, y = Value, fill = VariableLabel)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::facet_wrap(~ Experiment, scales = ifelse(same_scale, "fixed", "free_y"),
                        nrow = panel_rows, ncol = panel_cols) +
    ggplot2::labs(title = plot_title, x = NULL, y = y_axis_label) +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines"),
      legend.position = legend_position,
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")

  # Add value labels
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", Value), y = Value + ifelse(Value >= 0, 0.1, -0.1)),
    position = ggplot2::position_dodge(width = 0.7),
    angle = 90,
    hjust = ifelse(data$Value >= 0, 0, 1),
    size = 3
  )

  return(p)
}
