# SIMPLIFIED COMMON HELPERS -------------------------------------------------

#' @title Preprocess Data for Plotting
#' @description
#' Unified function to validate columns, handle filtering, process unit and split-by logic,
#' and format variable names prior to plotting. Used across all GTAPViz plot types.
#'
#' @param data A data frame or list of data frames containing GTAP results.
#' @param x_axis_from Character. X-axis category column (e.g., "REG", "Sector").
#' @param split_by Character or NULL. Column(s) for splitting plots.
#' @param panel_var Character. Facet variable (typically "Experiment").
#' @param variable_col Character. Column identifying variables.
#' @param unit_col Character. Column identifying unit information.
#' @param desc_col Character. Column identifying variable descriptions.
#' @param filter_var Vector or data frame to filter variable values.
#' @param var_name_by_description Logical. Whether to use descriptions in titles.
#' @param add_var_info Logical. Whether to append variable codes in titles.
#' @param stack_value_from Character or NULL. Used only in stack plots.
#'
#' @return A list with preprocessed `data` and `is_macro_mode` flag.
#' @keywords internal
#' @noRd
.preprocess_data <- function(data, x_axis_from, split_by, panel_var, variable_col,
                             unit_col, desc_col, filter_var, var_name_by_description,
                             add_var_info, stack_value_from = NULL) {
  # Validate columns with better error handling
  if (!is.null(stack_value_from)) {
    data <- .prepare_data_source(data, x_axis_from, stack_value_from, variable_col)
  } else {
    data <- .prepare_data_source(data, x_axis_from, variable_col = variable_col)
  }

  # Handle unit column
  unit_result <- .check_unit_column(data, unit_col)
  data <- unit_result$data
  unit_col <- unit_result$unit_col

  # Process split_by
  is_macro_mode <- is.null(split_by) || (is.logical(split_by) && !split_by)

  if (!is_macro_mode) {
    # Validate split_by columns
    if (length(split_by) > 1) {
      # Check if all split_by columns exist
      missing_cols <- setdiff(split_by, names(data))
      if (length(missing_cols) > 0) {
        stop(paste0("Split-by column(s) not found in data: ", paste(missing_cols, collapse=", "),
                    ". Available columns are: ", paste(names(data), collapse=", ")))
      }

      # Create a split_display for multiple columns
      data$split_display <- apply(data[, split_by, drop = FALSE], 1, paste, collapse = "-")
    } else {
      # Check if split_by column exists
      if (!split_by %in% names(data)) {
        stop(paste0("Split-by column '", split_by, "' not found in data. ",
                    "Available columns are: ", paste(names(data), collapse=", ")))
      }
    }
  }

  # Filter data using enhanced filter function
  filtered_data <- .process_filter_var(data, filter_var, variable_col)

  # Early return if filtering resulted in empty data
  if (nrow(filtered_data) == 0) {
    warning("No data remains after filtering. Please check your filter criteria.")
    return(NULL)
  }

  # Format variable names
  if (variable_col %in% names(filtered_data) && desc_col %in% names(filtered_data)) {
    filtered_data <- .format_variable_names(
      filtered_data, variable_col, desc_col, var_name_by_description, add_var_info
    )
  }

  # Ensure panel_var is a factor with original order
  if (panel_var %in% names(filtered_data)) {
    panel_levels <- unique(filtered_data[[panel_var]])
    filtered_data[[panel_var]] <- factor(filtered_data[[panel_var]], levels = panel_levels)
  } else {
    warning(paste0("Panel variable '", panel_var, "' not found in data. ",
                   "This might affect facet layouts. Available columns are: ",
                   paste(names(filtered_data), collapse=", ")))
  }

  return(list(
    data = filtered_data,
    is_macro_mode = is_macro_mode
  ))
}

#' @title Generate All Plots from GTAP Data
#' @description
#' Creates plots for each unit and split-by group using the specified plot type.
#' Supports top-impact filtering, macro modes, and dispatching to comparison/detail/stack variants.
#'
#' @param data A data frame containing GTAP results.
#' @param unit_col Character. Unit column name.
#' @param panel_var Character. Facet variable column.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param separate_figure Logical. Whether to separate plots by panel.
#' @param style_config List. Plot styling configuration.
#' @param is_macro_mode Logical. Whether macro-level plots are requested.
#' @param plot_type Character. One of "comparison", "detail", "stack", "unstack".
#' @param invert_axis Logical. Whether to flip bar orientation.
#' @param variable_col Character or NULL. Column for variable names.
#' @param split_by Character or NULL. Column(s) used for plot splitting.
#' @param top_impact Numeric or NULL. Number of top values to keep.
#' @param stack_value_from Character or NULL. Column for stack component categories.
#' @param show_total Logical. Whether to show total values in stacked bars.
#' @param unstack_plot Logical. Whether to unstack bars (only for stack type).
#'
#' @return A named list of ggplot2 plot objects.
#' @keywords internal
#' @noRd
.generate_plots <- function(data, unit_col, panel_var, x_axis_from, separate_figure,
                            style_config, is_macro_mode, plot_type, invert_axis,
                            variable_col = NULL, split_by = NULL, top_impact = NULL,
                            stack_value_from = NULL, show_total = FALSE, unstack_plot = FALSE) {

  plot_list <- list()
  unit_groups <- split(data, data[[unit_col]])

  for (unit_name in names(unit_groups)) {
    unit_data <- unit_groups[[unit_name]]

    # Apply top impact filtering if needed
    # Apply top impact filtering if needed
    if (!is.null(top_impact)) {
      if (plot_type == "detail") {
        # Determine the proper group column
        if (!is_macro_mode && length(split_by) > 1) {
          unit_data$._split_group_ <- apply(unit_data[, split_by, drop = FALSE], 1, paste, collapse = "-")
          top_impact_filter_col <- "._split_group_"
        } else if (!is_macro_mode) {
          top_impact_filter_col <- split_by
        } else {
          top_impact_filter_col <- x_axis_from
        }

        unit_data <- .filter_top_impact_values_detail(
          data = unit_data,
          top_impact = top_impact,
          group_col = top_impact_filter_col,
          panel_var = panel_var,
          x_axis_from = x_axis_from,
          variable_col = variable_col,
          unit_col = unit_col
        )

        # Clean up temporary column if it exists
        if ("._split_group_" %in% names(unit_data)) {
          unit_data$._split_group_ <- NULL
        }
      } else if (plot_type == "stack") {
        total_data <- .calculate_stack_totals(unit_data, x_axis_from, panel_var)
        unit_data <- .filter_top_impact_values_stack(unit_data, total_data, top_impact, is_macro_mode,
                                                     split_by, x_axis_from, panel_var, variable_col,
                                                     unit_col)
      }
    }

    # FIX: Handle macro mode properly (split_by = NULL)
    if (is_macro_mode) {
      # In macro mode, we process a single dataset with all data
      if (plot_type == "detail") {
        # For detail plots, we need to process by variable
        var_combinations <- unique(unit_data[[variable_col]])

        for (var_name in var_combinations) {
          var_data <- unit_data[unit_data[[variable_col]] == var_name, ]

          if (separate_figure) {
            panel_values <- unique(var_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_data <- var_data[var_data[[panel_var]] == panel_val, ]

              title_info <- .handle_plot_title_and_export(
                var_name = var_name,
                sep_value = NULL,
                plot_type = "detail",
                is_macro_mode = TRUE,
                variable_col = variable_col,
                unit_name = unit_name,
                style_config = style_config,
                data = panel_data,
                separate_figure = TRUE,
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
                invert_axis = invert_axis,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
            title_info <- .handle_plot_title_and_export(
              var_name = var_name,
              plot_type = "detail",
              is_macro_mode = TRUE,
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
              invert_axis = invert_axis,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      } else if (plot_type == "stack") {
        # For stack plots in macro mode
        total_data <- .calculate_stack_totals(unit_data, x_axis_from, panel_var)

        if (unstack_plot) {
          # Handle unstacked plots in macro mode
          x_axis_values <- unique(unit_data[[x_axis_from]])

          for (x_val in x_axis_values) {
            x_data <- unit_data[unit_data[[x_axis_from]] == x_val, ]
            x_totals <- total_data[total_data[[x_axis_from]] == x_val, ]

            if (separate_figure) {
              panel_values <- unique(x_data[[panel_var]])

              for (panel_val in panel_values) {
                panel_x_data <- x_data[x_data[[panel_var]] == panel_val, ]
                panel_x_totals <- x_totals[x_totals[[panel_var]] == panel_val, ]

                title_info <- .handle_plot_title_and_export(
                  var_name = NULL,
                  sep_value = NULL,
                  x_value = x_val,
                  plot_type = "unstack",
                  is_macro_mode = TRUE,
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
                  unit = unit_name,
                  panel_rows = style_config$panel_rows,
                  panel_cols = style_config$panel_cols,
                  panel_var = panel_var,
                  invert_axis = invert_axis,
                  top_impact = top_impact,
                  plot_style_config = style_config
                )

                plot_list[[title_info$export_name]] <- p
              }
            } else {
              title_info <- .handle_plot_title_and_export(
                var_name = NULL,
                sep_value = NULL,
                x_value = x_val,
                plot_type = "unstack",
                is_macro_mode = TRUE,
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
                unit = unit_name,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                invert_axis = invert_axis,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          }
        } else {
          # Regular stacked plots in macro mode
          if (separate_figure) {
            panel_values <- unique(unit_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_data <- unit_data[unit_data[[panel_var]] == panel_val, ]
              panel_totals <- total_data[total_data[[panel_var]] == panel_val, ]

              title_info <- .handle_plot_title_and_export(
                var_name = NULL,
                sep_value = NULL,
                plot_type = "stack",
                is_macro_mode = TRUE,
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
                unit = unit_name,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                show_total = show_total,
                invert_axis = invert_axis,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
            title_info <- .handle_plot_title_and_export(
              var_name = NULL,
              sep_value = NULL,
              plot_type = "stack",
              is_macro_mode = TRUE,
              variable_col = variable_col,
              unit_name = unit_name,
              style_config = style_config,
              data = unit_data
            )

            p <- .create_single_stacked_plot(
              data = unit_data,
              total_data = total_data,
              x_axis_from = x_axis_from,
              stack_value_from = stack_value_from,
              plot_title = title_info$title,
              unit = unit_name,
              panel_rows = style_config$panel_rows,
              panel_cols = style_config$panel_cols,
              panel_var = panel_var,
              show_total = show_total,
              invert_axis = invert_axis,
              top_impact = top_impact,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      } else {
        # For comparison plots in macro mode
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
              separate_figure = TRUE,
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
              invert_axis = invert_axis,
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
            invert_axis = invert_axis,
            plot_style_config = style_config
          )

          plot_list[[title_info$export_name]] <- p
        }
      }
    } else {
      # Not in macro mode - process data by split values
      # Get split column name
      split_col <- if (length(split_by) > 1) "split_display" else split_by

      # Get unique values for splitting
      separate_values_names <- unique(unit_data[[split_col]])

      # Process each separate value
      for (sep_value in separate_values_names) {
        filtered_data <- unit_data[unit_data[[split_col]] == sep_value, ]

        # For stack plots, calculate totals
        if (plot_type == "stack") {
          total_data <- .calculate_stack_totals(filtered_data, x_axis_from, panel_var)

          if (unstack_plot) {
            # Handle unstacked plots
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
                    is_macro_mode = FALSE,
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
                    unit = unit_name,
                    panel_rows = style_config$panel_rows,
                    panel_cols = style_config$panel_cols,
                    panel_var = panel_var,
                    invert_axis = invert_axis,
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
                  is_macro_mode = FALSE,
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
                  unit = unit_name,
                  panel_rows = style_config$panel_rows,
                  panel_cols = style_config$panel_cols,
                  panel_var = panel_var,
                  invert_axis = invert_axis,
                  top_impact = top_impact,
                  plot_style_config = style_config
                )

                plot_list[[title_info$export_name]] <- p
              }
            }
          } else {
            # Regular stacked plots
            if (separate_figure) {
              panel_values <- unique(filtered_data[[panel_var]])

              for (panel_val in panel_values) {
                panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]
                panel_totals <- total_data[total_data[[panel_var]] == panel_val, ]

                title_info <- .handle_plot_title_and_export(
                  var_name = NULL,
                  sep_value = sep_value,
                  plot_type = "stack",
                  is_macro_mode = FALSE,
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
                  unit = unit_name,
                  panel_rows = style_config$panel_rows,
                  panel_cols = style_config$panel_cols,
                  panel_var = panel_var,
                  show_total = show_total,
                  invert_axis = invert_axis,
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
                is_macro_mode = FALSE,
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
                unit = unit_name,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                show_total = show_total,
                invert_axis = invert_axis,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          }
        } else if (plot_type == "detail") {
          variable_already_in_split <- variable_col %in% split_by

          if (variable_already_in_split) {
            if (separate_figure) {
              panel_values <- unique(filtered_data[[panel_var]])

              for (panel_val in panel_values) {
                panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]

                title_info <- .handle_plot_title_and_export(
                  var_name = NULL,
                  sep_value = sep_value,
                  plot_type = "detail",
                  is_macro_mode = FALSE,
                  split_by = split_by,
                  x_axis_from = x_axis_from,
                  variable_col = variable_col,
                  unit_name = unit_name,
                  style_config = style_config,
                  data = panel_data,
                  separate_figure = TRUE,
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
                  invert_axis = invert_axis,
                  top_impact = top_impact,
                  plot_style_config = style_config
                )

                plot_list[[title_info$export_name]] <- p
              }
            } else {
              title_info <- .handle_plot_title_and_export(
                var_name = NULL,
                sep_value = sep_value,
                plot_type = "detail",
                is_macro_mode = FALSE,
                split_by = split_by,
                x_axis_from = x_axis_from,
                variable_col = variable_col,
                unit_name = unit_name,
                style_config = style_config,
                data = filtered_data
              )

              p <- .create_single_detail_plot(
                data = filtered_data,
                x_axis_from = x_axis_from,
                plot_title = title_info$title,
                unit = unit_name,
                panel_rows = style_config$panel_rows,
                panel_cols = style_config$panel_cols,
                panel_var = panel_var,
                invert_axis = invert_axis,
                top_impact = top_impact,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
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
                    plot_type = "detail",
                    is_macro_mode = FALSE,
                    split_by = split_by,
                    x_axis_from = x_axis_from,
                    variable_col = variable_col,
                    unit_name = unit_name,
                    style_config = style_config,
                    data = panel_data,
                    separate_figure = TRUE,
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
                    invert_axis = invert_axis,
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
                  is_macro_mode = FALSE,
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
                  invert_axis = invert_axis,
                  top_impact = top_impact,
                  plot_style_config = style_config
                )

                plot_list[[title_info$export_name]] <- p
              }
            }
          }
        } else {
          # Comparison plots
          if (separate_figure) {
            panel_values <- unique(filtered_data[[panel_var]])

            for (panel_val in panel_values) {
              panel_data <- filtered_data[filtered_data[[panel_var]] == panel_val, ]

              title_info <- .handle_plot_title_and_export(
                var_name = NULL,
                sep_value = sep_value,
                plot_type = "comparison",
                is_macro_mode = FALSE,
                split_by = split_by,
                x_axis_from = x_axis_from,
                variable_col = variable_col,
                unit_name = unit_name,
                style_config = style_config,
                data = panel_data,
                separate_figure = TRUE,
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
                invert_axis = invert_axis,
                plot_style_config = style_config
              )

              plot_list[[title_info$export_name]] <- p
            }
          } else {
            title_info <- .handle_plot_title_and_export(
              var_name = NULL,
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
              invert_axis = invert_axis,
              plot_style_config = style_config
            )

            plot_list[[title_info$export_name]] <- p
          }
        }
      }
    }
  }

  return(plot_list)
}

#' @title Create Plots with Panel Separation
#' @description
#' Handles the logic for creating single or multiple plots depending on `separate_figure`.
#' Dispatches to specific plot creators based on plot type.
#'
#' @param data A data frame for the plot.
#' @param total_data Optional totals (for stack/unstack).
#' @param unit_name Character. Unit label.
#' @param x_axis_from Character. X-axis column.
#' @param stack_value_from Character or NULL. Stack component column.
#' @param panel_var Character. Facet column.
#' @param separate_figure Logical. Whether to separate plots by panel.
#' @param style_config List. Style configuration.
#' @param plot_type Character. Plot type ("comparison", "detail", "stack", "unstack").
#' @param is_macro_mode Logical. Whether macro plot mode is on.
#' @param invert_axis Logical. Whether to flip bar orientation.
#' @param var_name Character or NULL. Variable name used in title.
#' @param sep_value Character or NULL. Split value.
#' @param split_by Character or NULL. Columns used to split plots.
#' @param variable_col Character or NULL. Column for variable labels.
#' @param top_impact Numeric or NULL. Top-N filtering.
#' @param show_total Logical. Whether to show totals in stacked bars.
#' @param x_val Character or NULL. Value of x-axis when in unstack mode.
#'
#' @return A list of ggplot2 objects keyed by export title.
#' @keywords internal
#' @noRd
.create_plots_with_panels <- function(data, total_data, unit_name, x_axis_from, stack_value_from,
                                      panel_var, separate_figure, style_config, plot_type,
                                      is_macro_mode, invert_axis, var_name = NULL, sep_value = NULL,
                                      split_by = NULL, variable_col = NULL, top_impact = NULL,
                                      show_total = FALSE, x_val = NULL) {
  plot_list <- list()

  # Get appropriate plot function
  plot_function <- switch(plot_type,
                          "comparison" = .create_single_comparison_plot,
                          "detail" = .create_single_detail_plot,
                          "stack" = .create_single_stacked_plot,
                          "unstack" = .create_single_unstacked_plot
  )

  if (separate_figure) {
    panel_values <- unique(data[[panel_var]])

    for (panel_val in panel_values) {
      panel_data <- data[data[[panel_var]] == panel_val, ]

      # Handle total data for stack/unstack plots
      panel_total_data <- if (!is.null(total_data)) {
        total_data[total_data[[panel_var]] == panel_val, ]
      } else {
        NULL
      }

      # Get title info
      title_info <- .handle_plot_title_and_export(
        var_name = var_name,
        sep_value = sep_value,
        x_value = x_val,
        plot_type = plot_type,
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

      # Create plot
      if (plot_type %in% c("stack", "unstack")) {
        p <- plot_function(
          data = panel_data,
          total_data = panel_total_data,
          x_axis_from = x_axis_from,
          stack_value_from = stack_value_from,
          plot_title = title_info$title,
          unit = unit_name,
          panel_rows = style_config$panel_rows,
          panel_cols = style_config$panel_cols,
          panel_var = panel_var,
          show_total = show_total,
          invert_axis = invert_axis,
          top_impact = top_impact,
          plot_style_config = style_config
        )
      } else if (plot_type == "detail") {
        p <- plot_function(
          data = panel_data,
          x_axis_from = x_axis_from,
          plot_title = title_info$title,
          unit = unit_name,
          panel_rows = style_config$panel_rows,
          panel_cols = style_config$panel_cols,
          panel_var = panel_var,
          invert_axis = invert_axis,
          top_impact = top_impact,
          plot_style_config = style_config
        )
      } else if (plot_type == "comparison") {
        p <- plot_function(
          data = panel_data,
          x_axis_from = x_axis_from,
          plot_title = title_info$title,
          unit = unit_name,
          panel_rows = style_config$panel_rows,
          panel_cols = style_config$panel_cols,
          panel_var = panel_var,
          invert_axis = invert_axis,
          plot_style_config = style_config
        )
      }

      plot_list[[title_info$export_name]] <- p
    }
  } else {
    # Get title info
    title_info <- .handle_plot_title_and_export(
      var_name = var_name,
      sep_value = sep_value,
      x_value = x_val,
      plot_type = plot_type,
      is_macro_mode = is_macro_mode,
      split_by = split_by,
      x_axis_from = x_axis_from,
      variable_col = variable_col,
      unit_name = unit_name,
      style_config = style_config,
      data = data
    )

    # Create plot
    if (plot_type %in% c("stack", "unstack")) {
      p <- plot_function(
        data = data,
        total_data = total_data,
        x_axis_from = x_axis_from,
        stack_value_from = stack_value_from,
        plot_title = title_info$title,
        unit = unit_name,
        panel_rows = style_config$panel_rows,
        panel_cols = style_config$panel_cols,
        panel_var = panel_var,
        show_total = show_total,
        invert_axis = invert_axis,
        top_impact = top_impact,
        plot_style_config = style_config
      )
    } else if (plot_type == "detail") {
      p <- plot_function(
        data = data,
        x_axis_from = x_axis_from,
        plot_title = title_info$title,
        unit = unit_name,
        panel_rows = style_config$panel_rows,
        panel_cols = style_config$panel_cols,
        panel_var = panel_var,
        invert_axis = invert_axis,
        top_impact = top_impact,
        plot_style_config = style_config
      )
    } else if (plot_type == "comparison") {
      p <- plot_function(
        data = data,
        x_axis_from = x_axis_from,
        plot_title = title_info$title,
        unit = unit_name,
        panel_rows = style_config$panel_rows,
        panel_cols = style_config$panel_cols,
        panel_var = panel_var,
        invert_axis = invert_axis,
        plot_style_config = style_config
      )
    }

    plot_list[[title_info$export_name]] <- p
  }

  return(plot_list)
}

#' @title Finalize Plot Export Process
#' @description
#' Prepares and calls the unified export function for GTAPViz plots.
#' Adds defaults for filename and merges user-specified configurations.
#'
#' @param plot_list List of ggplot2 objects to export.
#' @param data Original data used for plotting.
#' @param panel_layout Panel layout specification (rows/cols).
#' @param output_path Character. Path to output directory.
#' @param export_picture Logical. Whether to export images.
#' @param export_as_pdf Logical or "merged". Whether to export as PDF.
#' @param export_config List. Export parameters (width, height, dpi, etc).
#' @param default_filename Character. Default export name if not specified.
#'
#' @return Invisibly returns the list of plots.
#' @keywords internal
#' @noRd
.finalize_plot_export <- function(plot_list, data, panel_layout, output_path, export_picture,
                                  export_as_pdf, export_config, default_filename) {

  # Only create export_config if it's NULL
  if (is.null(export_config)) {
    export_config <- list()
  }

  # Calculate dimensions if not already specified
  dimensions <- if (!is.null(export_config$width) && !is.null(export_config$height)) {
    list(width = export_config$width, height = export_config$height)
  } else {
    .calculate_plot_dimensions(data, panel_layout)
  }

  # Add calculated dimensions to export_config
  export_config$width <- dimensions$width
  export_config$height <- dimensions$height

  # Handle NULL output_path - use tempdir() if needed
  if (is.null(output_path) && (export_picture || export_as_pdf)) {
    output_path <- tempdir()
    message("No output path specified. Using temporary directory: ", output_path)
  }

  # Export plots
  if (export_picture || export_as_pdf) {
    .export_plot_output(
      plots = plot_list,
      output_path = output_path,
      export_picture = export_picture,
      export_as_pdf = export_as_pdf,
      export_config = export_config,
      data = data,
      panel_layout = panel_layout,
      default_filename = default_filename
    )
  }

  # Return plots
  return(invisible(plot_list))
}

# UNIFIED STYLING AND AXIS HELPERS ---------------------------------

#' @title Apply Axis Scale Configuration
#' @description
#' Applies a `scale_x_continuous` or `scale_y_continuous` scale to a ggplot axis
#' using the plot style configuration. Supports expansion and tick mark increment.
#'
#' @param limits Numeric vector of length 2 specifying axis limits.
#' @param style_config List. Style configuration containing expansion and increments.
#' @param axis Character. Either `"x"` or `"y"` (default `"y"`).
#'
#' @return A ggplot2 scale object.
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

#' @title Create Facet Wrap for Panels
#' @description
#' Generates a `facet_wrap` layout using consistent configuration based on the style config.
#'
#' @param panel_var Character. Column name used for faceting.
#' @param panel_rows Integer. Number of rows in the panel layout.
#' @param panel_cols Integer. Number of columns in the panel layout.
#' @param style_config List. Style configuration with axis facet controls.
#' @param free_scales Logical. Whether to use `"free"` scales (default: FALSE).
#'
#' @return A ggplot2 `facet_wrap` object.
#' @keywords internal
#' @noRd
.create_facet_wrap <- function(panel_var, panel_rows = NULL, panel_cols = NULL,
                               style_config = NULL, free_scales = FALSE) {
  facet_args <- list(
    as.formula(paste("~", panel_var)),
    scales = if (free_scales || (!is.null(style_config) && style_config$show_axis_titles_on_all_facets))
      "free" else "fixed"
  )

  if (!is.null(panel_rows)) {
    facet_args$nrow <- panel_rows
  }

  if (!is.null(panel_cols)) {
    facet_args$ncol <- panel_cols
  }

  return(do.call(ggplot2::facet_wrap, facet_args))
}

#' @title Add Zero Reference Line to Plot
#' @description
#' Adds a horizontal or vertical line at the zero position to a plot based on the configuration.
#'
#' @param plot A ggplot2 object.
#' @param invert_axis Logical. Whether the axis is inverted (horizontal bars).
#' @param style_config List. Contains zero line settings (position, color, type).
#'
#' @return A ggplot2 object with the zero line added.
#' @keywords internal
#' @noRd
.add_zero_line <- function(plot, invert_axis, style_config) {
  if (invert_axis) {
    return(plot + ggplot2::geom_vline(
      xintercept = style_config$zero_line_position,
      linetype = style_config$zero_line_type,
      color = style_config$zero_line_color,
      linewidth = style_config$zero_line_size
    ))
  } else {
    return(plot + ggplot2::geom_hline(
      yintercept = style_config$zero_line_position,
      linetype = style_config$zero_line_type,
      color = style_config$zero_line_color,
      linewidth = style_config$zero_line_size
    ))
  }
}

#' @title Format Axis Labels from Unit and Style
#' @description
#' Generates x- and y-axis labels based on unit, axis source, and style config.
#'
#' @param unit Character. Unit name.
#' @param x_axis_from Character. Column name for x-axis.
#' @param style_config List. Contains label descriptions or overrides.
#'
#' @return A list with `y_label` and `x_label` strings.
#' @keywords internal
#' @noRd
.format_axis_labels <- function(unit, x_axis_from, style_config) {
  # y-axis label shows the unit
  y_label <- if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    style_config$y_axis_description
  } else if (tolower(unit) == "percent") {
    "Percentage (%)"
  } else {
    unit
  }

  # x-axis label uses column name if no description provided
  x_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    x_axis_from
  }

  return(list(y_label = y_label, x_label = x_label))
}

#' @title Format Y-Axis Label from Unit
#' @description
#' Simplified helper to return the correct y-axis label given the unit and style config.
#'
#' @param unit_name Character. Unit string.
#' @param style_config List. Style config possibly containing `y_axis_description`.
#'
#' @return A string for y-axis label.
#' @keywords internal
#' @noRd
.format_y_axis_label <- function(unit_name, style_config) {
  if (!is.null(style_config$y_axis_description) && nzchar(style_config$y_axis_description)) {
    return(style_config$y_axis_description)
  } else if (tolower(unit_name) == "percent") {
    return("Percentage (%)")
  } else {
    return(unit_name)
  }
}

#' @title Set Axis Labels on ggplot
#' @description
#' Applies plot title, x-label, and y-label depending on orientation and visibility flags.
#'
#' @param plot A ggplot2 object.
#' @param invert_axis Logical. Whether to flip orientation.
#' @param plot_title Character. Main plot title.
#' @param x_axis_label Character. Label for x-axis.
#' @param y_axis_label Character. Label for y-axis.
#' @param style_config List. Style config containing label visibility options.
#'
#' @return A ggplot2 object with updated axis labels.
#' @keywords internal
#' @noRd
.set_axis_labels <- function(plot, invert_axis, plot_title, x_axis_label, y_axis_label, style_config) {
  if (invert_axis) {
    # For horizontal bars, x is Value axis and y is Categories axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      plot <- plot + ggplot2::labs(title = plot_title, x = y_axis_label, y = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only categories axis title visible
      plot <- plot + ggplot2::labs(title = plot_title, x = "", y = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only value axis title visible
      plot <- plot + ggplot2::labs(title = plot_title, x = y_axis_label, y = "")
    } else {
      # No axis titles
      plot <- plot + ggplot2::labs(title = plot_title, x = "", y = "")
    }
  } else {
    # For vertical bars, x is Categories axis and y is Value axis
    if (style_config$show_x_axis_title && style_config$show_y_axis_title) {
      # Both axis titles visible
      plot <- plot + ggplot2::labs(title = plot_title, y = y_axis_label, x = x_axis_label)
    } else if (style_config$show_x_axis_title) {
      # Only categories axis title visible
      plot <- plot + ggplot2::labs(title = plot_title, y = "", x = x_axis_label)
    } else if (style_config$show_y_axis_title) {
      # Only value axis title visible
      plot <- plot + ggplot2::labs(title = plot_title, y = y_axis_label, x = "")
    } else {
      # No axis titles
      plot <- plot + ggplot2::labs(title = plot_title, y = "", x = "")
    }
  }

  return(plot)
}

#' @title Calculate Y-Axis Limits
#' @description
#' Computes value axis limits using unit type and data characteristics, or returns
#' manual limits from the config.
#'
#' @param data A data frame containing a `Value` column.
#' @param unit Character. Unit label (e.g., `"percent"`).
#' @param style_config List. Contains optional `scale_limit` or guides padding behavior.
#'
#' @return A numeric vector of length 2 indicating y-axis limits.
#' @keywords internal
#' @noRd
.calculate_value_axis_limits <- function(data, unit, style_config) {
  # Use scale_limit if provided
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    return(style_config$scale_limit)
  }

  # Calculate appropriate limits based on data
  value_range <- range(data$Value, na.rm = TRUE)
  max_abs_value <- max(abs(value_range), na.rm = TRUE)

  # Different limit calculations based on data and unit
  if (tolower(unit) == "percent") {
    # For percentage values, use symmetric limits
    return(c(-max_abs_value * 1.35, max_abs_value * 1.35))
  } else {
    # For other units, base on value range
    if (all(data$Value >= 0, na.rm = TRUE)) {
      # All positive values
      return(c(0, value_range[2] * 1.3))
    } else if (all(data$Value <= 0, na.rm = TRUE)) {
      # All negative values
      return(c(value_range[1] * 1.3, 0))
    } else {
      # Mixed values
      return(c(value_range[1] * 1.3, value_range[2] * 1.3))
    }
  }
}


# COMMON DATA HANDLING HELPERS ---------------------------------

#' @title Find Column Case-Insensitive
#' @description
#' Searches for a column name in a data frame with optional case-insensitive matching and fallback options.
#'
#' @param data A data frame.
#' @param col_name Character. Desired column name.
#' @param is_required Logical. If TRUE, throw an error when not found.
#' @param default_name Character. Optional fallback if column not found.
#'
#' @return Matching column name or fallback.
#' @keywords internal
#' @noRd
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

#' @title Process Filter Variable for GTAPViz Data
#'
#' @description
#' Applies filtering to a GTAP-compatible data frame based on a specified `filter_var`.
#' This function supports filtering using a vector, list of column-value pairs, or
#' a data frame of filter values matched by `variable_col`.
#' @md
#' @param data A data frame containing GTAP result variables.
#' @param filter_var A filter object that can be:
#'   - A vector of values to filter in the column specified by `variable_col`.
#'   - A list of column-value pairs for multi-column filtering.
#'   - A data frame with a `variable_col` column to match values in `data`.
#' @param variable_col Character. Column name to match values when `filter_var` is a vector or data frame.
#'
#' @return A filtered data frame with rows matching the specified criteria.
#' If no matches are found, a warning is issued and an empty data frame is returned.
#'
#' @details
#' This function is called internally during data preprocessing for GTAPViz plots.
#' If `filter_var` is NULL, the original `data` is returned unmodified.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @noRd
.process_filter_var <- function(data, filter_var, variable_col) {
  # If filter_var is NULL, return data as is
  if (is.null(filter_var)) {
    return(data)
  }

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame. Current data structure is ", class(data)[1], ".")
  }

  # Case 1: filter_var is a list of column/value pairs
  if (is.list(filter_var) && !is.data.frame(filter_var)) {
    filtered_data <- data

    for (col_name in names(filter_var)) {
      filter_values <- filter_var[[col_name]]

      # Check if column exists in data
      if (!col_name %in% names(filtered_data)) {
        warning(paste0("Filter column '", col_name, "' not found in data. Skipping this filter."))
        next
      }

      # Apply filter for this column
      filtered_data <- filtered_data[filtered_data[[col_name]] %in% filter_values, ]

      # Stop if no data left after filtering
      if (nrow(filtered_data) == 0) {
        warning(paste0("No data remains after filtering by column '", col_name,
                       "' with values: ", paste(filter_values, collapse=", "), "."))
        return(filtered_data)
      }
    }

    return(filtered_data)
  }

  # Case 2: filter_var is a data frame
  if (is.data.frame(filter_var)) {
    if (!variable_col %in% names(filter_var)) {
      stop("Variable column '", variable_col, "' not found in filter_var data frame.")
    }

    if (!variable_col %in% names(data)) {
      stop("Variable column '", variable_col, "' not found in input data.")
    }

    filtered_data <- data[data[[variable_col]] %in% filter_var[[variable_col]], ]

    if (nrow(filtered_data) == 0) {
      warning("No matching data found for the specified filter_var values.")
    }

    return(filtered_data)
  }

  # Case 3: filter_var is a vector (original behavior)
  if (is.vector(filter_var) && !is.list(filter_var)) {
    if (!variable_col %in% names(data)) {
      stop("Variable column '", variable_col, "' not found in input data.")
    }

    filtered_data <- data[data[[variable_col]] %in% filter_var, ]

    if (nrow(filtered_data) == 0) {
      warning(paste0("No matching data found for filter values: ", paste(filter_var, collapse=", ")))
    }

    return(filtered_data)
  }

  # If filter_var is of an unexpected type
  stop("filter_var must be a vector, list, or data frame. Current type is ", class(filter_var)[1], ".")
}

#' @title Prepare Data Source for Plotting
#' @description
#' Validates and retrieves a data frame containing required columns from either a data frame or list of frames.
#'
#' @param data A data frame or list of data frames.
#' @param x_axis_from Character. Required column for x-axis.
#' @param stack_value_from Character. Optional column for stacked bars.
#' @param variable_col Character. Optional column for variable codes.
#'
#' @return A validated data frame.
#' @keywords internal
#' @noRd
.prepare_data_source <- function(data, x_axis_from, stack_value_from = NULL, variable_col = NULL) {
  # Check if data is completely missing
  if (is.null(data)) {
    stop("Input data is NULL. Please provide a valid data frame or list of data frames.")
  }

  # Check data type and provide helpful message
  if (!is.data.frame(data) && !is.list(data)) {
    stop(paste0("Invalid data format. Expected a data frame or list of data frames, but got: ",
                class(data)[1], ". Please check your input data."))
  }

  # If already a data frame, validate columns
  if (is.data.frame(data)) {
    # Check x_axis_from column
    if (!(x_axis_from %in% names(data))) {
      stop(paste0("Required column '", x_axis_from, "' not found in the data frame. ",
                  "Available columns are: ", paste(names(data), collapse=", ")))
    }

    # Check stack_value_from if provided (for stack_plot)
    if (!is.null(stack_value_from) && !(stack_value_from %in% names(data))) {
      stop(paste0("Required column '", stack_value_from, "' not found in the data frame. ",
                  "Available columns are: ", paste(names(data), collapse=", ")))
    }

    # Check variable_col if provided
    if (!is.null(variable_col) && !(variable_col %in% names(data))) {
      stop(paste0("Required column '", variable_col, "' not found in the data frame. ",
                  "Available columns are: ", paste(names(data), collapse=", ")))
    }

    return(data)
  }

  # If a list of data frames, find first matching data frame
  if (is.list(data)) {
    if (length(data) == 0) {
      stop("Empty list provided. The list must contain at least one data frame.")
    }

    valid_dfs <- 0
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (is.data.frame(df)) {
        valid_dfs <- valid_dfs + 1
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

    # Helpful error messages based on what was found
    if (valid_dfs == 0) {
      stop("No valid data frames found in the input list. Please check your data structure.")
    } else {
      stop(paste0("No suitable data frame found with required column '", x_axis_from, "'. ",
                  "Checked ", valid_dfs, " data frames but none contained all required columns."))
    }
  }

  # This should never be reached due to the initial type check, but including for completeness
  stop("Input must be a data frame or a list of data frames.")
}

#' @title Check and Standardize Unit Column
#' @description
#' Validates and resolves the unit column in a data frame.
#'
#' @param data A data frame.
#' @param unit_col Character. Column name to use as unit (default: "Unit").
#'
#' @return A list containing the possibly modified data and resolved `unit_col`.
#' @keywords internal
#' @noRd
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

#' @title Format Variable Names Using Descriptions
#' @description
#' Formats `Variable` column using corresponding `Description` values, with optional concatenation.
#'
#' @param data A data frame.
#' @param variable_col Character. Variable name column.
#' @param desc_col Character. Description column.
#' @param var_name_by_description Logical. Use description as primary label.
#' @param add_var_info Logical. Append variable code to description.
#'
#' @return Modified data frame with formatted variable labels.
#' @keywords internal
#' @noRd
.format_variable_names <- function(data, variable_col, desc_col, var_name_by_description = TRUE,
                                   add_var_info = FALSE) {
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

#' @title Calculate Panel Layout Automatically
#' @description
#' Calculates rows and columns for facet panels based on user-specified or auto-detected layout.
#'
#' @param data A data frame.
#' @param panel_rows Integer. Desired number of rows.
#' @param panel_cols Integer. Desired number of columns.
#' @param panel_var Character. Facet column (default: "Experiment").
#'
#' @return A list with `rows` and `cols` for panel layout.
#' @keywords internal
#' @noRd
.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL, panel_var = "Experiment") {
  # Determine number of panels
  num_panels <- if (is.data.frame(data) && panel_var %in% names(data)) {
    length(unique(data[[panel_var]]))
  } else {
    1
  }

  # Case 1: Only panel_rows is specified (panel_cols is NULL)
  if (!is.null(panel_rows) && is.null(panel_cols)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  }
  # Case 2: Only panel_cols is specified (panel_rows is NULL)
  else if (is.null(panel_rows) && !is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  }
  # Case 3: Both panel_rows and panel_cols are specified (neither is NULL)
  else if (!is.null(panel_rows) && !is.null(panel_cols)) {
    # Check if there are enough panels and adjust if needed
    if (panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
      # Preferentially adjust columns to fit all panels
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  }

  # Case 4: AUTO CALCULATE LAYOUT WHEN NEITHER DIMENSION IS SPECIFIED
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

#' @title Calculate Plot Dimensions for Export
#' @description
#' Determines width and height based on number of panels to ensure readable layout.
#'
#' @param data A data frame.
#' @param panel_layout A list with `rows` and `cols` values.
#'
#' @return A list with `width` and `height` in inches.
#' @keywords internal
#' @noRd
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

#' @title Filter Top Impact Values (Detail Plot)
#' @description
#' Filters the most impactful positive and negative values within each group for detailed plots.
#'
#' @param data A data frame or list.
#' @param top_impact Integer. Number of top observations to retain per group.
#' @param group_col Character. Grouping variable.
#' @param panel_var Character. Facet variable.
#' @param x_axis_from Character. X-axis variable.
#' @param variable_col Character. Variable identifier.
#' @param unit_col Character. Unit column.
#'
#' @return Filtered data frame or list.
#' @keywords internal
#' @noRd
.filter_top_impact_values_detail <- function(data, top_impact, group_col, panel_var,
                                             x_axis_from, variable_col, unit_col) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .filter_top_impact_values_detail, top_impact,
                                group_col, panel_var, x_axis_from, variable_col, unit_col))
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

#' @title Calculate Stack Totals
#' @description
#' Aggregates total, positive, and negative values by `x_axis_from` and `panel_var`.
#'
#' @param data A data frame or list.
#' @param x_axis_from Character. X-axis variable.
#' @param panel_var Character. Facet variable.
#'
#' @return A data frame with totals and formatted labels.
#' @keywords internal
#' @noRd
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

#' @title Filter Top Impact Values (Stack Plot)
#' @description
#' Filters stack components to retain only top impactful bars based on total impact.
#'
#' @param data A data frame or list.
#' @param total_data Data frame of precomputed totals.
#' @param top_impact Integer. Number of top values to retain.
#' @param group_col Character. Grouping column.
#' @param panel_var Character. Facet variable.
#' @param x_axis_from Character. X-axis column.
#' @param variable_col Character. Variable code column.
#' @param unit_col Character. Unit column.
#' @param stack_value_from Character. Column representing stack components.
#'
#' @return Filtered data frame.
#' @keywords internal
#' @noRd
.filter_top_impact_values_stack <- function(data, total_data, top_impact, is_macro_mode, split_by,
                                            x_axis_from, panel_var, variable_col, unit_col) {
  if (inherits(data, "list") && !is.data.frame(data)) {
    return(.apply_to_dataframes(data, .filter_top_impact_values_stack, total_data, top_impact,
                                is_macro_mode, split_by, x_axis_from, panel_var, variable_col, unit_col))
  }

  # INPUT VALIDATION
  if (!is.data.frame(data)) return(data)
  if (!("Value" %in% names(data))) return(data)
  if (is.null(top_impact) || nrow(total_data) <= top_impact) return(data)

  # GET GROUP COLUMNS
  group_cols <- c(variable_col, unit_col, panel_var, split_by)
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

#' @title Export and Save GTAP Plots
#' @description
#' Handles exporting plots to PNG or PDF format using specified export settings.
#' Supports merged PDF export, individual export, and dynamic file naming.
#'
#' @param plots A ggplot2 object or a named list of ggplot2 objects to export.
#' @param output_path Character. Directory to save plots. If NULL, uses working directory.
#' @param export_picture Logical. If TRUE, exports PNG files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports individual PDFs. If "merged", combines all into one PDF.
#' @param export_config List. Export settings such as `width`, `height`, `dpi`, `bg`, `limitsize`, `file_name`.
#' @param data Data used for calculating fallback plot dimensions (optional).
#' @param panel_layout A list with `rows` and `cols` used for auto-scaling plot size (optional).
#' @param default_filename Character. Default base filename if none specified.
#'
#' @return Invisibly returns NULL. Exports plots to disk.
#' @keywords internal
#' @noRd
.export_plot_output <- function(plots, output_path = NULL, export_picture = TRUE,
                                export_as_pdf = FALSE, export_config = NULL,
                                data = NULL, panel_layout = NULL, default_filename = NULL) {
  # Prepare export configuration
  if (is.null(export_config)) {
    export_config <- list()
  }

  # Default export settings if not specified
  if (is.null(export_config$dpi)) export_config$dpi <- 300
  if (is.null(export_config$bg)) export_config$bg <- "white"
  if (is.null(export_config$limitsize)) export_config$limitsize <- FALSE

  # Handle custom dimensions if provided
  if (is.null(export_config$width) || is.null(export_config$height)) {
    dimensions <- .calculate_plot_dimensions(data, panel_layout)
    export_config$width <- dimensions$width
    export_config$height <- dimensions$height
  } else {
    dimensions <- list(width = export_config$width, height = export_config$height)
  }

  # Display dimensions at the start of export process
  .display_export_dimensions(dimensions, plots, "start", export_config$dpi)

  # Process output_path
  if (is.null(output_path)) {
    output_path <- tempdir()
    message("No output path specified. Using temporary directory: ", output_path)
  }

  # Normalize the path to handle spaces and special characters
  output_path <- normalizePath(output_path, mustWork = FALSE)

  # Create output directory first, before any file operations
  if (!dir.exists(output_path)) {
    tryCatch({
      dir.create(output_path, recursive = TRUE)
      if (!dir.exists(output_path)) {
        warning("Failed to create output directory: ", output_path)
        output_path <- tempdir()
        message("Using temporary directory instead: ", output_path)
      }
    }, error = function(e) {
      warning("Error creating output directory: ", conditionMessage(e))
      output_path <- tempdir()
      message("Using temporary directory instead: ", output_path)
    })
  }

  # Check if directory is writable
  if (file.access(output_path, 2) != 0) {
    warning("Output directory is not writable: ", output_path)
    output_path <- tempdir()
    message("Using temporary directory instead: ", output_path)

    # Ensure this directory exists
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
  }

  # Normalize plot names to be valid filenames
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

  # Make sure all plot names are valid filenames
  plots_with_clean_names <- list()
  for (i in seq_along(plots)) {
    original_name <- names(plots)[i]
    # Keep the original name but ensure no invalid characters
    # LEAVE % signs intact here
    clean_name <- gsub("[^a-zA-Z0-9_\\-\\. ()%]", "_", original_name)
    clean_name <- gsub("\\s+", " ", clean_name)
    clean_name <- trimws(clean_name)

    plots_with_clean_names[[clean_name]] <- plots[[i]]
  }
  plots <- plots_with_clean_names
  n_plots <- length(plots)

  is_merge_pdf <- FALSE
  if (is.character(export_as_pdf)) {
    if (tolower(export_as_pdf) == "merged") {
      is_merge_pdf <- TRUE
      export_as_pdf <- TRUE
    }
  }

  if (export_as_pdf) {
    if (is_merge_pdf && n_plots >= 1) {
      # For merged PDFs, determine the filename
      pdf_file_name <- NULL

      # Use user-provided file name if specified
      if (!is.null(export_config$file_name) && nzchar(export_config$file_name)) {
        pdf_file_name <- export_config$file_name
      } else {
        # Use default_filename passed from the calling function
        # This should be the plot type (comparison, detail, stack)
        pdf_file_name <- default_filename
      }

      # Add number of plots suffix if more than 1 plot
      if (n_plots > 1) {
        pdf_file_name <- paste0(pdf_file_name, "_", n_plots, "plots")
      }

      # Make sure the filename is safe
      pdf_file_name <- gsub("[^a-zA-Z0-9_\\-\\. ]", "_", pdf_file_name)
      pdf_file_name <- gsub("\\s+", " ", pdf_file_name)
      pdf_file_name <- trimws(pdf_file_name)

      # Create the full path
      pdf_path <- file.path(output_path, paste0(pdf_file_name, ".pdf"))

      # Use tryCatch to handle PDF creation errors
      tryCatch({
        grDevices::pdf(
          file = pdf_path,
          width = export_config$width,
          height = export_config$height,
          useDingbats = FALSE,
          title = pdf_file_name
        )

        for (i in seq_along(plots)) {
          tryCatch({
            grid::grid.draw(plots[[i]])
          }, error = function(e) {
            warning("Error drawing plot: ", conditionMessage(e))
          })
        }

        # Always make sure to close the device
        grDevices::dev.off()

        if (file.exists(pdf_path)) {
          message("Combined PDF exported to: ", pdf_path)
        } else {
          warning("PDF creation failed. The output file was not created.")
        }
      }, error = function(e) {
        # Make sure to close any open devices on error
        if (grDevices::dev.cur() > 1) {
          try(grDevices::dev.off(), silent = TRUE)
        }
        warning("Error exporting merged PDF: ", conditionMessage(e))
      })
    } else {
      # Individual PDF export - use the cleaned plot names
      for (i in seq_along(plots)) {
        p <- plots[[i]]
        plot_name <- names(plots)[[i]]

        # CRITICAL FIX: Escape % with %% for ggsave
        safe_name <- gsub("%", "%%", plot_name)

        # Create the full path
        pdf_path <- file.path(output_path, paste0(plot_name, ".pdf"))

        # Use tryCatch to handle ggsave errors
        tryCatch({
          # Use the escaped name for ggsave
          ggplot2::ggsave(
            filename = pdf_path,  # Use original path with %
            plot = p,
            device = "pdf",
            width = export_config$width,
            height = export_config$height,
            dpi = export_config$dpi,
            bg = export_config$bg,
            limitsize = export_config$limitsize
          )

          if (file.exists(pdf_path)) {
            message("PDF figure exported to: ", pdf_path)
          } else {
            warning("PDF creation failed for plot name: ", plot_name)
          }
        }, error = function(e) {
          warning("Error exporting PDF for plot '", plot_name, "': ", conditionMessage(e))
        })
      }
    }
  }

  if (export_picture) {
    # PNG export - use the cleaned plot names
    for (i in seq_along(plots)) {
      p <- plots[[i]]
      plot_name <- names(plots)[[i]]

      # CRITICAL FIX: Escape % with %% for ggsave
      safe_name <- gsub("%", "%%", plot_name)

      # Create the full path for final filepath
      png_path <- file.path(output_path, paste0(plot_name, ".png"))

      # Create the full path with escaped % for ggsave
      png_path_safe <- file.path(output_path, paste0(safe_name, ".png"))

      # Use tryCatch to handle ggsave errors
      tryCatch({
        # The key fix - use the path with escaped % signs
        ggplot2::ggsave(
          filename = png_path_safe,  # Use safe path with %% for ggsave
          plot = p,
          device = "png",
          width = export_config$width,
          height = export_config$height,
          dpi = export_config$dpi,
          bg = export_config$bg,
          limitsize = export_config$limitsize
        )

        # Check if file exists and report success
        if (file.exists(png_path)) {
          message("PNG figure exported to: ", png_path)
        } else {
          warning("PNG creation failed for plot name: ", plot_name)
        }
      }, error = function(e) {
        warning("Error exporting PNG for plot '", plot_name, "': ", conditionMessage(e))
      })
    }
  }

  # Display dimensions at the end of export process
  .display_export_dimensions(dimensions, plots, "end", export_config$dpi)

  # Always return NULL invisibly to suppress output
  return(invisible(NULL))
}

# COMPARISON PLOT SPECIFIC FUNCTIONS --------------------------------------

#' @title Create Single Comparison Plot
#' @description
#' Generates a single bar chart for comparison plots, using consistent axis, theme, and label formatting.
#'
#' @param data A data frame containing values to plot.
#' @param x_axis_from Character. Column used for x-axis categories.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measure.
#' @param panel_rows Integer. Number of rows in the facet layout.
#' @param panel_cols Integer. Number of columns in the facet layout.
#' @param panel_var Character. Facet column name (default: "Experiment").
#' @param invert_axis Logical. Whether to flip the axis for horizontal layout.
#' @param plot_style_config List. Plot styling configuration.
#'
#' @return A ggplot2 object representing the comparison plot.
#' @keywords internal
#' @noRd
.create_single_comparison_plot <- function(data, x_axis_from, plot_title, unit,
                                           panel_rows, panel_cols,
                                           panel_var = "Experiment",
                                           invert_axis = FALSE,
                                           plot_style_config = NULL) {

  # Get style configuration
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # Set up variables for plotting
  x_var <- x_axis_from
  facet_var <- panel_var

  # Sort the Data of X_AXIS_FROM
  data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))

  n_panels <- length(unique(data[[facet_var]]))

  # Calculate Y-Axis Limits using common function
  y_limits <- .calculate_value_axis_limits(data, unit, style_config)

  # Format axis labels using common function
  axis_labels <- .format_axis_labels(unit, x_axis_from, style_config)
  y_axis_label <- axis_labels$y_label
  x_axis_label <- axis_labels$x_label

  # Calculate label positions
  if (invert_axis) {
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

  # Generate colors if provided
  if (!is.null(style_config$color_tone)) {
    palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
    color_palette <- .generate_comparison_colors(data, style_config$color_tone, x_var, palette_type)
  }

  # Get bar styling from config
  bar_width <- style_config$bar_width
  bar_spacing <- style_config$bar_spacing

  # Create the basic plot
  if (invert_axis) {
    # For horizontal bars (flipped coordinates)
    p <- ggplot2::ggplot(data, ggplot2::aes(
      y = .data[[x_var]],
      x = .data[["Value"]],
      fill = .data[[x_var]])) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = bar_spacing),
                        width = bar_width)

    # Add value labels if configured
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

    # Apply scale to value axis (X-axis) using common function
    p <- p + .apply_axis_scale(y_limits, style_config, "x")

    # Add zero line if configured using common function
    if (style_config$show_zero_line) {
      p <- .add_zero_line(p, invert_axis, style_config)
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

    # Add value labels if configured
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

    # Apply scale to value axis (Y-axis) using common function
    p <- p + .apply_axis_scale(y_limits, style_config, "y")

    # Add zero line if configured using common function
    if (style_config$show_zero_line) {
      p <- .add_zero_line(p, invert_axis, style_config)
    }
  }

  # Apply colors if provided
  if (!is.null(style_config$color_tone)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
  }

  # Add facet wrap if we have multiple panels using common function
  if (n_panels > 1) {
    p <- p + .create_facet_wrap(panel_var = facet_var,
                                panel_rows = panel_rows,
                                panel_cols = panel_cols,
                                style_config = style_config)
  }

  # Apply theme styling
  p <- p + ggplot2::theme_minimal()

  # Set axis labels based on orientation using common function
  p <- .set_axis_labels(p, invert_axis, plot_title, x_axis_label, y_axis_label, style_config)

  # Apply style config
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

#' @title Generate Comparison Colors
#' @description
#' Creates a custom palette for bar plots based on number of categories and color tone.
#'
#' @param data A data frame.
#' @param color_tone Character. Palette base color or theme name.
#' @param axis_col Character. Column representing categories.
#' @param palette_type Character. Palette style (e.g., "qualitative").
#'
#' @return Named vector of hex color codes.
#' @keywords internal
#' @noRd
.generate_comparison_colors <- function(data, color_tone = NULL, axis_col, palette_type = "qualitative") {
  if(is.null(color_tone)) return(NULL)

  n_colors <- length(unique(data[[axis_col]]))

  # Try to get a themed palette first
  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_colors,
                                          palette_type = palette_type)

  if (!is.null(themed_palette)) {
    return(themed_palette)
  }

  # Fallback to custom color generation if needed
  base_color <- if(startsWith(color_tone, "#")) {
    color_tone
  } else {
    try(colorspace::hex(colorspace::sRGB(t(col2rgb(color_tone) / 255))), silent = TRUE)
  }

  if (inherits(base_color, "try-error")) {
    return(NULL)
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

# DETAIL PLOT SPECIFIC FUNCTIONS -----------------------------------------

#' @title Create Single Detail Plot
#' @description
#' Generates a bar chart with color-coded bars based on value magnitude and polarity.
#' Used for detail-level visualization with top-impact filtering support.
#'
#' @param data A data frame containing plotting data.
#' @param x_axis_from Character. X-axis grouping variable.
#' @param plot_title Character. Title of the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Integer. Number of rows in the facet layout.
#' @param panel_cols Integer. Number of columns in the facet layout.
#' @param panel_var Character. Facet variable (default: "Experiment").
#' @param invert_axis Logical. Flip the plot orientation.
#' @param top_impact Numeric. Number of top-impact bars to display (optional).
#' @param plot_style_config List. Visual configuration.
#'
#' @return A ggplot2 object.
#' @keywords internal
#' @noRd
.create_single_detail_plot <- function(data, x_axis_from, plot_title, unit,
                                       panel_rows, panel_cols,
                                       panel_var = "Experiment",
                                       invert_axis = FALSE,
                                       top_impact = NULL,
                                       plot_style_config = NULL) {

  # Get style configuration
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # Setup color palette
  positive_color <- style_config$positive_color
  negative_color <- style_config$negative_color
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_color_palette(positive_color, negative_color, style_config$color_tone, palette_type)

  # Prepare data
  max_abs_value <- max(abs(data$Value))
  decimal_places <- style_config$value_label_decimal_places
  data$Label <- sprintf(paste0("%.", decimal_places, "f"), data$Value)

  n_vars <- length(unique(data[[x_axis_from]]))
  n_panels <- length(unique(data[[panel_var]]))

  # Categorize values for color coding
  data <- .categorize_values_by_panel(data, panel_var)

  # Calculate Y-Axis Limits using common function
  y_limits <- .calculate_value_axis_limits(data, unit, style_config)

  # Format axis labels using common function
  axis_labels <- .format_axis_labels(unit, x_axis_from, style_config)
  y_axis_label <- axis_labels$y_label
  x_axis_label <- axis_labels$x_label

  # Prepare data for plotting
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

  # Create base plot with appropriate ordering
  if (invert_axis) {
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

    # Apply scale to x-axis using common function
    p <- p + .apply_axis_scale(y_limits, style_config, "x")

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

    # Apply scale to y-axis using common function
    p <- p + .apply_axis_scale(y_limits, style_config, "y")
  }

  # Setup plot appearance
  p <- p +
    ggplot2::scale_fill_manual(values = color_palette, guide = "none") +
    ggplot2::theme_minimal()

  # Add facets if needed using common function
  free_scales <- !is.null(top_impact) || style_config$show_axis_titles_on_all_facets
  if (n_panels > 1) {
    p <- p + .create_facet_wrap(panel_var = panel_var,
                                panel_rows = panel_rows,
                                panel_cols = panel_cols,
                                style_config = style_config,
                                free_scales = free_scales)
  }

  # Set axis labels based on orientation using common function
  p <- .set_axis_labels(p, invert_axis, plot_title, x_axis_label, y_axis_label, style_config)

  # Add zero line if configured using common function
  if (style_config$show_zero_line) {
    p <- .add_zero_line(p, invert_axis, style_config)
  }

  # Apply style config
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

#' @title Categorize Bar Colors by Value Intensity
#' @description
#' Assigns value categories for use in color-coding bars in detail plots.
#'
#' @param data A data frame.
#' @param panel_var Character. Column for facet panels.
#'
#' @return Modified data frame with a `value_category` column.
#' @keywords internal
#' @noRd
.categorize_values_by_panel <- function(data, panel_var) {
  n_panels <- length(unique(data[[panel_var]]))

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
    max_abs_value <- max(abs(data$Value), na.rm = TRUE)
    data$value_category <- dplyr::case_when(
      data$Value > 0 & abs(data$Value) >= 0.7 * max_abs_value ~ "extreme_positive",
      data$Value < 0 & abs(data$Value) >= 0.7 * max_abs_value ~ "extreme_negative",
      data$Value > 0 ~ "normal_positive",
      data$Value < 0 ~ "normal_negative",
      TRUE ~ "neutral"
    )
  }

  return(data)
}

#' @title Generate Positive/Negative Color Palette
#' @description
#' Generates a color palette for value categories in detail plots,
#' including fallbacks for custom tones and RGB lightening/darkening.
#'
#' @param positive_color Character. Color for positive values (default "#2E8B57").
#' @param negative_color Character. Color for negative values (default "#CD5C5C").
#' @param color_tone Character. Optional color tone theme.
#' @param palette_type Character. Type of palette to use ("qualitative", etc.).
#'
#' @return Named vector of hex colors for value categories.
#' @keywords internal
#' @noRd
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

# STACK PLOT SPECIFIC FUNCTIONS ------------------------------------------

#' @title Create Single Stacked Plot
#' @description
#' Generates a stacked bar chart for decomposition analysis with optional total labels.
#'
#' @param data A data frame for the stacked bars.
#' @param total_data A data frame containing totals to annotate.
#' @param x_axis_from Character. X-axis grouping variable.
#' @param stack_value_from Character. Column representing stacked components.
#' @param plot_title Character. Title for the plot.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Integer. Number of panel rows for faceting.
#' @param panel_cols Integer. Number of panel columns for faceting.
#' @param panel_var Character. Facet variable (default: "Experiment").
#' @param show_total Logical. Whether to display total value labels.
#' @param invert_axis Logical. Flip bar orientation.
#' @param top_impact Numeric. Number of top impactful values (optional).
#' @param plot_style_config List. Visual style configuration.
#'
#' @return A ggplot2 object.
#' @keywords internal
#' @noRd
.create_single_stacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                        plot_title, unit,
                                        panel_rows, panel_cols,
                                        panel_var = "Experiment",
                                        show_total = TRUE,
                                        invert_axis = FALSE,
                                        top_impact = NULL,
                                        plot_style_config = NULL) {

  # Get style configuration
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # Setup variables
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # Respect existing factor levels for consistent sorting
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

  # Generate color palette
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone, palette_type)

  # Calculate Y Limits
  y_limit <- .calculate_stack_limits(total_data, style_config)

  # Format axis labels
  y_axis_label <- .format_y_axis_label(unit, style_config)
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    x_axis_from
  }

  # Create base plot based on orientation
  if (invert_axis) {
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

    # Add total labels if configured
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

    # Apply scale to value axis (X-axis)
    p <- p + .apply_axis_scale(y_limit, style_config, "x")

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

    # Add total labels if configured
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

    # Apply scale to value axis (Y-axis)
    p <- p + .apply_axis_scale(y_limit, style_config, "y")
  }

  # Add zero line if configured
  if (style_config$show_zero_line) {
    p <- .add_zero_line(p, invert_axis, style_config)
  }

  # Add facets if needed
  if (n_panels > 1) {
    free_scales <- !is.null(top_impact) || style_config$show_axis_titles_on_all_facets
    p <- p + .create_facet_wrap(panel_var = panel_var,
                                panel_rows = panel_rows,
                                panel_cols = panel_cols,
                                style_config = style_config,
                                free_scales = free_scales)
  }

  # Setup appearance
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

  # Set up legend
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

  # Set axis labels based on orientation
  p <- .set_axis_labels(p, invert_axis, plot_title, x_axis_label, y_axis_label, style_config)

  # Apply style config
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

#' @title Create Single Unstacked Plot
#' @description
#' Generates a bar plot showing disaggregated components from stack plots.
#' Each `x_axis_from` value is plotted as a separate unstacked bar.
#'
#' @param data A data frame of values to plot.
#' @param total_data A data frame containing totals (optional).
#' @param x_axis_from Character. The grouping variable for bars.
#' @param stack_value_from Character. Component column (used for fill).
#' @param plot_title Character. Plot title.
#' @param unit Character. Unit of measurement.
#' @param panel_rows Integer. Facet layout rows.
#' @param panel_cols Integer. Facet layout columns.
#' @param panel_var Character. Facet grouping variable.
#' @param invert_axis Logical. Flip orientation.
#' @param top_impact Numeric. Number of top values to show (optional).
#' @param plot_style_config List. Style configuration.
#'
#' @return A ggplot2 object.
#' @keywords internal
#' @noRd
.create_single_unstacked_plot <- function(data, total_data, x_axis_from, stack_value_from,
                                          plot_title, unit,
                                          panel_rows, panel_cols,
                                          panel_var = "Experiment",
                                          invert_axis = FALSE,
                                          top_impact = NULL,
                                          plot_style_config = NULL) {

  # Get style configuration
  style_config <- if (!is.null(plot_style_config)) {
    plot_style_config
  } else {
    .calculate_plot_style_config(NULL, "default")
  }

  # Setup variables
  color_tone <- style_config$color_tone
  n_panels <- length(unique(data[[panel_var]]))

  # Respect existing factor levels for consistent sorting
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

  # Generate color palette
  palette_type <- if (!is.null(style_config$color_palette_type)) style_config$color_palette_type else "qualitative"
  color_palette <- .generate_stack_colors(data, stack_value_from, color_tone, palette_type)

  # Calculate Y Limits - for unstacked, use value range directly
  value_range <- range(data$Value, na.rm = TRUE)
  max_abs <- max(abs(value_range), na.rm = TRUE)

  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    y_limit <- style_config$scale_limit
  } else {
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

  # Format axis labels
  y_axis_label <- .format_y_axis_label(unit, style_config)
  x_axis_label <- if (!is.null(style_config$x_axis_description) && nzchar(style_config$x_axis_description)) {
    style_config$x_axis_description
  } else {
    stack_value_from
  }

  # Format value labels
  decimal_places <- style_config$value_label_decimal_places
  data$Label <- sprintf(paste0("%.", decimal_places, "f"), data$Value)

  # CREATE BASE PLOT BASED ON ORIENTATION
  if (invert_axis) {
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
    p <- p + .apply_axis_scale(y_limit, style_config, "x")

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
    p <- p + .apply_axis_scale(y_limit, style_config, "y")
  }

  # ADD ZERO LINE IF CONFIGURED
  if (style_config$show_zero_line) {
    p <- .add_zero_line(p, invert_axis, style_config)
  }

  # ADD FACETS IF NEEDED
  if (n_panels > 1) {
    free_scales <- !is.null(top_impact) || style_config$show_axis_titles_on_all_facets
    p <- p + .create_facet_wrap(panel_var = panel_var,
                                panel_rows = panel_rows,
                                panel_cols = panel_cols,
                                style_config = style_config,
                                free_scales = free_scales)
  }

  # SETUP APPEARANCE
  p <- p + ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::theme_minimal()

  # HANDLE AXIS LABELS BASED ON ORIENTATION
  p <- .set_axis_labels(p, invert_axis, plot_title, x_axis_label, y_axis_label, style_config)

  # Apply style config
  p <- .apply_plot_style_config(p, style_config)

  return(p)
}

#' @title Calculate Stack Plot Y-Axis Limits
#' @description
#' Determines appropriate y-axis limits for stacked plots using the total data,
#' or returns fallback if totals are unavailable.
#'
#' @param total_data Data frame with precomputed totals.
#' @param style_config List. Contains user-specified `scale_limit` or default fallback.
#'
#' @return Numeric vector of length 2 indicating axis limits.
#' @keywords internal
#' @noRd
.calculate_stack_limits <- function(total_data, style_config) {
  # Use scale_limit if provided
  if (!is.null(style_config$scale_limit) && length(style_config$scale_limit) == 2) {
    return(style_config$scale_limit)
  }

  # Check if total_data exists and has the Total column
  if (is.data.frame(total_data) && nrow(total_data) > 0 && "Total" %in% names(total_data)) {
    # Calculate appropriate limits based on actual values
    max_abs_total <- max(abs(total_data$Total), na.rm = TRUE)

    if (is.finite(max_abs_total) && max_abs_total > 0) {
      if (all(total_data$Total >= 0, na.rm = TRUE)) {
        # All positive values
        return(c(0, max(total_data$Total, na.rm = TRUE) * 1.4))
      } else if (all(total_data$Total <= 0, na.rm = TRUE)) {
        # All negative values
        return(c(min(total_data$Total, na.rm = TRUE) * 1.4, 0))
      } else {
        # Mixed values
        return(c(min(total_data$Total, na.rm = TRUE) * 1.4,
                 max(total_data$Total, na.rm = TRUE) * 1.4))
      }
    } else {
      # Handle invalid data within total_data
      return(c(-10, 10))  # Fallback
    }
  }

  # Fallback if total_data is invalid
  return(c(-10, 10))
}

#' @title Generate Stack Plot Colors
#' @description
#' Generates a color palette for stacked bar components using tone, palette type,
#' and fallback logic for high contrast.
#'
#' @param data A data frame.
#' @param stack_value_from Character. Column name for stack components.
#' @param color_tone Character. Optional color theme.
#' @param palette_type Character. Palette type (e.g., "qualitative").
#'
#' @return Named vector of hex color codes for each stack category.
#' @keywords internal
#' @noRd
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
    adjusted_colors <- try(colorspace::lighten(themed_palette, amount = seq(0.1, 0.5, length.out = n_components)), silent = TRUE)
    if (!inherits(adjusted_colors, "try-error")) {
      return(setNames(adjusted_colors, components))
    }
    return(setNames(themed_palette, components))
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

# COMMON COLOR AND STYLING FUNCTIONS --------------------------------------

#' @title Create Themed or Monochrome Color Palette
#' @description
#' Generates a vector of colors using predefined themes or a specified base color.
#' Supports palette types like "qualitative", "sequential", and "diverging".
#'
#' @param color_tone Character. Theme name or base color (e.g., "gtap", "blue").
#' @param n_colors Integer. Number of colors required.
#' @param palette_type Character. Type of palette ("qualitative", "sequential", "diverging", "mono").
#'
#' @return A character vector of hex color codes.
#' @keywords internal
#' @noRd
.create_color_palette <- function(color_tone = NULL, n_colors = 5, palette_type = "qualitative") {
  # Special case for "mono" palette type - return exact same color for all items
  if (!is.null(palette_type) && tolower(palette_type) == "mono") {
    if (!is.null(color_tone)) {
      # Try to validate if it's a valid R color
      tryCatch({
        # Confirm it's a valid color
        base_col <- grDevices::col2rgb(color_tone)
        # Convert to hex for consistency
        hex_color <- grDevices::rgb(base_col[1,1], base_col[2,1], base_col[3,1], maxColorValue = 255)
        # Return the exact same color n_colors times
        return(rep(hex_color, n_colors))
      }, error = function(e) {
        warning(paste("Invalid color:", color_tone, "- using black instead"))
        return(rep("#000000", n_colors))
      })
    } else {
      # If no color_tone provided with mono, default to black
      return(rep("#000000", n_colors))
    }
  }

  # Rest of the original function for themed palettes
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
    )
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
  if (!is.null(color_tone) && grepl("_mono$", color_tone)) {
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
    else if (base_color %in% names(themed_palettes)) {
      color_hex <- themed_palettes[[base_color]]$qualitative[1]
    }
    # Default fallback
    else {
      color_hex <- "#000000"  # Default to black if color not recognized
    }

    # For _mono suffix, return the exact same color for all items
    return(rep(color_hex, n_colors))
  }

  # Try to interpret any standard R color
  if (!is.null(color_tone)) {
    tryCatch({
      # Try to validate if it's a valid R color
      base_col <- grDevices::col2rgb(color_tone)

      # If we get here, it's a valid color - create a palette of shades
      if (palette_type == "diverging") {
        darken_factor <- seq(0.4, 1.3, length.out = n_colors)
      } else {
        darken_factor <- seq(0.5, 1.5, length.out = n_colors)
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

#' @title Calculate Plot Style Configuration
#' @description
#' Merges user-supplied style settings with GTAPViz defaults. Adjusts font sizes if `all_font_size` is used.
#'
#' @param config Optional list of user-defined style settings.
#' @param plot_type Character. Type of plot (default = "default").
#'
#' @return A complete list of style configuration parameters.
#' @keywords internal
#' @noRd
.calculate_plot_style_config <- function(config = NULL, plot_type = "default") {
  # Get default configuration from create_plot_style
  default_config <- list(
    # Title settings
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

    # Data sorting
    sort_data_by_value = FALSE,

    # Plot margin
    plot.margin = c(10, 25, 10, 10)
  )

  # If no config is provided, return the default
  if (is.null(config)) {
    return(default_config)
  }

  # Merge user config with defaults (user settings take precedence)
  final_config <- utils::modifyList(default_config, config)

  # Override font sizes with all_font_size if provided
  if (!is.null(final_config$all_font_size)) {
    font_sizes <- .calculate_font_sizes(NULL, NULL, final_config$all_font_size)

    # Only override font sizes if not explicitly set in user config
    font_size_fields <- c(
      "title_size", "x_axis_title_size", "y_axis_title_size",
      "strip_text_size", "x_axis_text_size", "y_axis_text_size",
      "legend_text_size", "value_label_size"
    )

    for (field in font_size_fields) {
      if (is.null(config) || is.null(config[[field]])) {
        final_config[[field]] <- font_sizes[[field]]
      }
    }
  }

  return(final_config)
}

#' @title Calculate Font Sizes for Plot Text
#' @description
#' Computes proportional font sizes based on a global multiplier `all_font_size`.
#'
#' @param width Numeric or NULL. Plot width (optional).
#' @param height Numeric or NULL. Plot height (optional).
#' @param all_font_size Numeric. Scaling factor for font size (default = 1).
#'
#' @return A list of named font sizes.
#' @keywords internal
#' @noRd
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

#' @title Apply Plot Style Configuration
#' @description
#' Applies margins, labels, fonts, themes, and grid visibility to a ggplot object
#' based on a given style configuration list.
#'
#' @param p A ggplot2 object.
#' @param config A list of style parameters as generated by `.calculate_plot_style_config`.
#'
#' @return A ggplot2 object with styles applied.
#' @keywords internal
#' @noRd
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

  # Apply custom theme if provided
  if (!is.null(config$theme)) {
    p <- p + config$theme
  }

  return(p)
}

#' @title Display Export Dimensions in Console
#' @description
#' Outputs width, height, DPI, and number of plots before and after the export process.
#'
#' @param dimensions List with `width` and `height` values.
#' @param plots ggplot object or list of ggplots.
#' @param phase Character. "start" or "end" to indicate export phase.
#' @param dpi Numeric. Resolution in dots per inch (default = 300).
#'
#' @return Invisible NULL. Only used for messaging.
#' @keywords internal
#' @noRd
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

# TITLE HANDLING FUNCTIONS ------------------------------------------

#' @title Handle Plot Title and Export Name Generation
#' @description
#' Constructs a dynamic or static plot title and a clean file-safe export name
#' using variable labels, split/grouping values, unit, panel ID, and style config.
#'
#' @param var_name Character. Name of the variable being plotted.
#' @param sep_value Character. Split/grouping value for plot title.
#' @param x_value Character. Value used for unstacked plots.
#' @param plot_type Character. Plot type ("comparison", "detail", "stack", "unstack").
#' @param is_macro_mode Logical. Indicates macro-level aggregation (TRUE = one plot).
#' @param split_by Character or NULL. Column(s) used to split plots.
#' @param x_axis_from Character. Column name used on x-axis.
#' @param variable_col Character. Name of the variable column.
#' @param unit_name Character. Label for unit (e.g., "percent", "USD").
#' @param style_config List. List of plot title and style options.
#' @param data A data frame to optionally use for dynamic title rendering.
#' @param separate_figure Logical. If TRUE, individual plot per panel value.
#' @param panel_val Character. Name of panel to be added as suffix (if applicable).
#'
#' @return A list with `title` (plot title) and `export_name` (safe filename).
#' @keywords internal
#' @noRd
.handle_plot_title_and_export <- function(var_name = NULL, sep_value = NULL, x_value = NULL,
                                          plot_type = NULL, is_macro_mode = FALSE, split_by = NULL,
                                          x_axis_from = NULL, variable_col = NULL, unit_name = NULL,
                                          style_config = NULL, data = NULL, separate_figure = FALSE,
                                          panel_val = NULL, panel_var = "Experiment") {

  safe_collapse <- function(x) {
    if (is.null(x)) return(NULL)
    x <- as.character(x)
    if (length(x) > 1) {
      return(paste(x, collapse = "-"))
    }
    return(x)
  }

  var_name <- safe_collapse(var_name)
  sep_value <- safe_collapse(sep_value)
  x_value <- safe_collapse(x_value)
  panel_val <- safe_collapse(panel_val)
  unit_name <- safe_collapse(unit_name)

  if (is_macro_mode) {
    plot_title <- .coalesce(
      var_name,
      .coalesce(
        if (!is.null(data) && !is.null(variable_col) && variable_col %in% names(data))
          unique(data[[variable_col]])[1],
        "Global Economic Impacts"
      )
    )
  } else {
    if (!is.null(sep_value)) {
      plot_title <- sep_value
      if (!is.null(var_name) && !identical(as.character(sep_value), as.character(var_name))) {
        if (!grepl(var_name, sep_value, fixed = TRUE)) {
          plot_title <- paste0(sep_value, " - ", var_name)
        }
      }
    } else if (!is.null(var_name)) {
      plot_title <- var_name
    } else {
      plot_title <- "GTAP Analysis"
    }

    if (!is.null(x_value) && plot_type %in% c("unstack", "stack")) {
      plot_title <- paste0(plot_title, " - ", x_value)
    }
  }

  plot_title <- safe_collapse(plot_title)

  dynamic_title_has_unit <- FALSE
  if (!is.null(style_config) && !is.null(style_config$title_format)) {
    title_format <- style_config$title_format

    if (title_format$type == "dynamic") {
      if (!is.null(data) && !is.null(title_format$text) && nrow(data) > 0) {
        if (is.vector(title_format$text) && length(title_format$text) > 1) {
          warning("title_format$text should be a single string with {placeholders}, not a vector. Ignoring malformed dynamic title.")
        } else if (!requireNamespace("glue", quietly = TRUE)) {
          warning("The 'glue' package is required for dynamic titles but is not installed. Using standard title format.")
        } else {
          title_text <- if (is.vector(title_format$text)) title_format$text[1] else title_format$text

          referenced_cols <- regmatches(
            title_text,
            gregexpr("\\{([^}]+)\\}", title_text)
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
              plot_title <- as.character(glue::glue_data(data[1, ], title_text))
            }
          }
        }
      }
    }
    else if (title_format$type == "full") {
      if (is.vector(title_format$text)) {
        plot_title <- title_format$text[1]
      } else {
        plot_title <- title_format$text
      }
    }
    else if (title_format$type == "prefix") {
      separator <- if (!is.null(title_format$sep)) title_format$sep else " "
      prefix_text <- if (is.vector(title_format$text)) title_format$text[1] else title_format$text
      plot_title <- paste0(prefix_text, separator, plot_title)
    }
    else if (title_format$type == "suffix") {
      separator <- if (!is.null(title_format$sep)) title_format$sep else " "
      suffix_text <- if (is.vector(title_format$text)) title_format$text[1] else title_format$text
      plot_title <- paste0(plot_title, separator, suffix_text)
    }
  }

  if (!is.null(style_config) &&
      (!is.null(style_config$title_format) &&
       (style_config$title_format$type != "dynamic" ||
        (style_config$title_format$type == "dynamic" && !dynamic_title_has_unit))) &&
      style_config$add_unit_to_title && !is.null(unit_name)) {
    if (tolower(unit_name) == "percent") {
      plot_title <- paste0(plot_title, " (%)")
    } else {
      plot_title <- paste0(plot_title, " (", unit_name, ")")
    }
  }

  panel_is_unique <- FALSE
  if (!separate_figure && !is.null(data) && !is.null(panel_val)) {
    if (!is.null(data) && panel_var %in% names(data)) {
      unique_panel_vals <- unique(data[[panel_var]])
      panel_is_unique <- length(unique_panel_vals) == 1
    }
  }

  if (separate_figure && !is.null(panel_val)) {
    plot_title <- paste0(plot_title, " - ", panel_val)
  } else if (!panel_is_unique && !is.null(panel_val)) {
    plot_title <- paste0(plot_title, " - ", panel_val)
  }

  export_name_parts <- c()
  if (!is.null(sep_value)) {
    export_name_parts <- c(export_name_parts, safe_collapse(sep_value))
  }
  if (!is.null(var_name)) {
    var_str <- safe_collapse(var_name)
    if (is.null(sep_value) || !grepl(var_str, safe_collapse(sep_value), fixed = TRUE)) {
      export_name_parts <- c(export_name_parts, var_str)
    }
  }
  if (!is.null(x_value)) {
    export_name_parts <- c(export_name_parts, safe_collapse(x_value))
  }
  if (!is.null(panel_val)) {
    export_name_parts <- c(export_name_parts, safe_collapse(panel_val))
  }

  if (length(export_name_parts) > 0) {
    export_name <- paste(export_name_parts, collapse = "-")
  } else {
    export_name <- "plot"
  }

  if (!is.null(unit_name)) {
    safe_unit <- tolower(unit_name)
    safe_unit <- gsub("percent|percentage|%", "pct", safe_unit)
    safe_unit <- gsub("\\$|usd|dollar", "usd", safe_unit)
    safe_unit <- gsub("million", "mil", safe_unit)
    safe_unit <- gsub("billion", "bil", safe_unit)
    safe_unit <- gsub("[^a-zA-Z0-9_\\-]", "_", safe_unit)
    safe_unit <- gsub("_{2,}", "_", safe_unit)
    safe_unit <- gsub("^_|_$", "", safe_unit)

    if (nchar(safe_unit) > 0) {
      export_name <- paste0(export_name, "_", safe_unit)
    }
  }

  export_name <- gsub("[^a-zA-Z0-9_\\-]", "_", export_name)
  export_name <- gsub("_{2,}", "_", export_name)
  export_name <- gsub("^_|_$", "", export_name)
  export_name <- trimws(export_name)

  if (!is.null(plot_type)) {
    if (plot_type == "stack") {
      export_name <- paste0(export_name, "_stack")
    } else if (plot_type == "unstack") {
      export_name <- paste0(export_name, "_unstack")
    }
  }

  if (!is.null(export_name) && length(export_name) == 1 && nchar(export_name) > 200) {
    export_name <- paste0(substr(export_name, 1, 197), "...")
  }

  return(list(
    title = plot_title,
    export_name = export_name
  ))
}

# Ge Plot Styles Help -----------------------------------------------------
#' @title Get Export Configuration Options
#'
#' @description
#' Returns documentation and default values for export configuration options used in plotting functions.
#'
#' @keywords internal
#' @noRd
#'
.get_export_config <- function() {
  # Export config parameters with default values
  export_config_params <- list(
    file_name = "gtap_plots",
    width = NULL,
    height = NULL,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )

  # Build message string
  msg <- "my_export_config <- list(\n"

  # Print file_name
  msg <- paste0(msg, "  file_name = \"", export_config_params$file_name, "\",\n")

  # Print width
  msg <- paste0(msg, "  width = ", if(is.null(export_config_params$width)) "NULL" else export_config_params$width, ",\n")

  # Print height
  msg <- paste0(msg, "  height = ", if(is.null(export_config_params$height)) "NULL" else export_config_params$height, ",\n")

  # Print dpi
  msg <- paste0(msg, "  dpi = ", export_config_params$dpi, ",\n")

  # Print bg
  msg <- paste0(msg, "  bg = \"", export_config_params$bg, "\",\n")

  # Print limitsize (last item, no comma)
  msg <- paste0(msg, "  limitsize = ", ifelse(export_config_params$limitsize, "TRUE", "FALSE"), "\n")

  msg <- paste0(msg, ")\n\n")
  msg <- paste0(msg, "# Example usage:\n")
  msg <- paste0(msg, "# comparison_plot(data, x_axis_from = \"REG\", export_config = my_export_config)\n")

  # Output the message
  message(msg)

  return(invisible(export_config_params))
}

#' @title Get Plot Style Configuration
#'
#' @description
#' Returns configuration settings for plot styles, with options to view as a structured dataframe
#' or to look up specific parameters. Also provides parameter validation for custom configurations.
#'
#' @param plot_type Character. Type of plot: "default" (default).
#' @param validate_custom List or NULL. Custom configuration settings to validate.
#'
#' @keywords internal
#' @noRd
.get_plot_style_config <- function(plot_type = "default",
                                   validate_custom = NULL) {
  config <- .calculate_plot_style_config(NULL, plot_type)

  # Start building the message string
  msg <- "my_style_config <- list(\n"

  # Title settings
  msg <- paste0(msg, "\n  # Title settings\n")
  msg <- paste0(msg, "  show_title = ", ifelse(config$show_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  title_face = \"", config$title_face, "\",\n")
  msg <- paste0(msg, "  title_size = ", config$title_size, ",\n")
  msg <- paste0(msg, "  title_hjust = ", config$title_hjust, ",\n")
  msg <- paste0(msg, "  add_unit_to_title = ", ifelse(config$add_unit_to_title, "TRUE", "FALSE"), ",\n")

  # Format margin objects as simple vectors with description
  margin_values <- as.numeric(config$title_margin)
  msg <- paste0(msg, "  title_margin = c(", margin_values[1], ", ", margin_values[2],
                ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n")

  # Format title_format as a properly structured list
  tf <- config$title_format
  msg <- paste0(msg, "  title_format = create_title_format(\n")
  msg <- paste0(msg, "    type = \"", .coalesce(tf$type, "standard"), "\", #option: prefix, suffix, full, dynamic\n")
  msg <- paste0(msg, "    text = \"", .coalesce(tf$text, ""), "\",\n")
  msg <- paste0(msg, "    sep = \"", .coalesce(tf$sep, ""), "\"\n")
  msg <- paste0(msg, "  ),\n")

  # X-Axis settings
  msg <- paste0(msg, "\n  # X-Axis settings\n")
  msg <- paste0(msg, "  show_x_axis_title = ", ifelse(config$show_x_axis_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  x_axis_title_face = \"", config$x_axis_title_face, "\",\n")
  msg <- paste0(msg, "  x_axis_title_size = ", config$x_axis_title_size, ",\n")

  margin_values <- as.numeric(config$x_axis_title_margin)
  msg <- paste0(msg, "  x_axis_title_margin = c(", margin_values[1], ", ", margin_values[2],
                ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n")

  msg <- paste0(msg, "  show_x_axis_labels = ", ifelse(config$show_x_axis_labels, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  x_axis_text_face = \"", config$x_axis_text_face, "\",\n")
  msg <- paste0(msg, "  x_axis_text_size = ", config$x_axis_text_size, ",\n")
  msg <- paste0(msg, "  x_axis_text_angle = ", config$x_axis_text_angle, ",\n")
  msg <- paste0(msg, "  x_axis_text_hjust = ", config$x_axis_text_hjust, ",\n")
  msg <- paste0(msg, "  x_axis_description = \"", config$x_axis_description, "\",\n")

  # Y-Axis settings
  msg <- paste0(msg, "\n  # Y-Axis settings\n")
  msg <- paste0(msg, "  show_y_axis_title = ", ifelse(config$show_y_axis_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  y_axis_title_face = \"", config$y_axis_title_face, "\",\n")
  msg <- paste0(msg, "  y_axis_title_size = ", config$y_axis_title_size, ",\n")

  margin_values <- as.numeric(config$y_axis_title_margin)
  msg <- paste0(msg, "  y_axis_title_margin = c(", margin_values[1], ", ", margin_values[2],
                ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n")

  msg <- paste0(msg, "  show_y_axis_labels = ", ifelse(config$show_y_axis_labels, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  y_axis_text_face = \"", config$y_axis_text_face, "\",\n")
  msg <- paste0(msg, "  y_axis_text_size = ", config$y_axis_text_size, ",\n")
  msg <- paste0(msg, "  y_axis_text_angle = ", config$y_axis_text_angle, ",\n")
  msg <- paste0(msg, "  y_axis_text_hjust = ", config$y_axis_text_hjust, ",\n")
  msg <- paste0(msg, "  y_axis_description = \"", config$y_axis_description, "\",\n")
  msg <- paste0(msg, "  show_axis_titles_on_all_facets = ", ifelse(config$show_axis_titles_on_all_facets, "TRUE", "FALSE"), ",\n")

  # Value Labels
  msg <- paste0(msg, "\n  # Value Labels\n")
  msg <- paste0(msg, "  show_value_labels = ", ifelse(config$show_value_labels, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  value_label_face = \"", config$value_label_face, "\",\n")
  msg <- paste0(msg, "  value_label_size = ", config$value_label_size, ",\n")
  msg <- paste0(msg, "  value_label_position = \"", config$value_label_position, "\",\n")
  msg <- paste0(msg, "  value_label_decimal_places = ", config$value_label_decimal_places, ",\n")

  # Legend
  msg <- paste0(msg, "\n  # Legend\n")
  msg <- paste0(msg, "  show_legend = ", ifelse(config$show_legend, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_legend_title = ", ifelse(config$show_legend_title, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  legend_position = \"", config$legend_position, "\",\n")
  msg <- paste0(msg, "  legend_title_face = \"", config$legend_title_face, "\",\n")
  msg <- paste0(msg, "  legend_text_face = \"", config$legend_text_face, "\",\n")
  msg <- paste0(msg, "  legend_text_size = ", config$legend_text_size, ",\n")

  # Panel Strip
  msg <- paste0(msg, "\n  # Panel Strip\n")
  msg <- paste0(msg, "  strip_face = \"", config$strip_face, "\",\n")
  msg <- paste0(msg, "  strip_text_size = ", config$strip_text_size, ",\n")
  msg <- paste0(msg, "  strip_background = \"", config$strip_background, "\",\n")

  margin_values <- as.numeric(config$strip_text_margin)
  msg <- paste0(msg, "  strip_text_margin = c(", margin_values[1], ", ", margin_values[2],
                ", ", margin_values[3], ", ", margin_values[4], "), #c(top, right, bottom, left)\n")

  # Panel Layout
  msg <- paste0(msg, "\n  # Panel Layout\n")
  msg <- paste0(msg, "  panel_spacing = ", config$panel_spacing, ",\n")
  msg <- paste0(msg, "  panel_rows = ", if(is.null(config$panel_rows)) "NULL" else config$panel_rows, ",\n")
  msg <- paste0(msg, "  panel_cols = ", if(is.null(config$panel_cols)) "NULL" else config$panel_cols, ",\n")
  msg <- paste0(msg, "  theme = ", if(is.null(config$theme)) "NULL" else "custom_theme", ",\n")

  # Color
  msg <- paste0(msg, "\n  # Colors and Grid \n")
  msg <- paste0(msg, "  color_tone = ", if(is.null(config$color_tone)) "NULL" else paste0("\"", config$color_tone, "\""), ",\n")
  msg <- paste0(msg, "  color_palette_type = \"", config$color_palette_type, "\", #option: qualitative, sequential, diverging\n")
  msg <- paste0(msg, "  positive_color = \"", config$positive_color, "\",\n")
  msg <- paste0(msg, "  negative_color = \"", config$negative_color, "\",\n")
  msg <- paste0(msg, "  background_color = \"", config$background_color, "\",\n")
  msg <- paste0(msg, "  grid_color = \"", config$grid_color, "\",\n")
  msg <- paste0(msg, "  show_grid_major_x = ", ifelse(config$show_grid_major_x, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_grid_major_y = ", ifelse(config$show_grid_major_y, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_grid_minor_x = ", ifelse(config$show_grid_minor_x, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  show_grid_minor_y = ", ifelse(config$show_grid_minor_y, "TRUE", "FALSE"), ",\n")

  # Zero Line
  msg <- paste0(msg, "\n  # Zero Line\n")
  msg <- paste0(msg, "  show_zero_line = ", ifelse(config$show_zero_line, "TRUE", "FALSE"), ",\n")
  msg <- paste0(msg, "  zero_line_type = \"", config$zero_line_type, "\",\n")
  msg <- paste0(msg, "  zero_line_color = \"", config$zero_line_color, "\",\n")
  msg <- paste0(msg, "  zero_line_size = ", config$zero_line_size, ",\n")
  msg <- paste0(msg, "  zero_line_position = ", config$zero_line_position, ",\n")

  # Bar Chart
  msg <- paste0(msg, "\n  # Bar Chart\n")
  msg <- paste0(msg, "  bar_width = ", config$bar_width, ",\n")
  msg <- paste0(msg, "  bar_spacing = ", config$bar_spacing, ",\n")

  # Scale Settings
  msg <- paste0(msg, "\n  # Scale Settings\n")
  if (is.null(config$scale_limit)) {
    msg <- paste0(msg, "  scale_limit = NULL,\n")
  } else {
    msg <- paste0(msg, "  scale_limit = c(", paste(config$scale_limit, collapse=", "), "),\n")
  }
  msg <- paste0(msg, "  scale_increment = ", if(is.null(config$scale_increment)) "NULL" else config$scale_increment, ",\n")

  # Scale Expansion
  msg <- paste0(msg, "\n  # Scale Expansion\n")
  msg <- paste0(msg, "  expansion_y_mult = c(", paste(config$expansion_y_mult, collapse=", "), "),\n")
  msg <- paste0(msg, "  expansion_x_mult = c(", paste(config$expansion_x_mult, collapse=", "), "),\n")

  # Font Size Control
  msg <- paste0(msg, "\n  # Font Size Control\n")
  msg <- paste0(msg, "  all_font_size = ", config$all_font_size, ",\n")

  # Data Sorting
  msg <- paste0(msg, "\n  # Data Sorting\n")
  msg <- paste0(msg, "  sort_data_by_value = ", ifelse(config$sort_data_by_value, "TRUE", "FALSE"), ",\n")

  # Plot Margin Settings
  msg <- paste0(msg, "\n  # Plot Margin\n")
  margin_values <- as.numeric(config$plot.margin)
  msg <- paste0(msg, "  plot.margin = c(", margin_values[1], ", ", margin_values[2],
                ", ", margin_values[3], ", ", margin_values[4], ") #c(top, right, bottom, left)\n")

  msg <- paste0(msg, ")\n\n")
  msg <- paste0(msg, "# Example usage:\n")
  msg <- paste0(msg, "# comparison_plot(data, x_axis_from = \"REG\", plot_style_config = my_style_config)\n")

  # Output the message
  message(msg)

  return(invisible(config))
}
