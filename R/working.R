# In a utils.R or misc.R file in your package
#' .coalesce two values, returning the first non-NULL value
#'
#' @param x First value to check
#' @param y Fallback value
#' @return First non-NULL value, or y if x is NULL
.coalesce <- function(x, y) {
  if (is.null(x)) y else x
}


# Simple fix for .calculate_panel_layout

.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL,
                                    panel_var = "Experiment") {
  # DETERMINE NUMBER OF PANELS
  num_panels <- length(unique(data[[panel_var]]))
  
  # HANDLE WHEN ONLY ONE DIMENSION IS SPECIFIED
  if (!is.null(panel_rows) && is.null(panel_cols)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    
    # Ensure we have enough panels to fit all data
    if (panel_rows * panel_cols < num_panels) {
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (is.null(panel_rows) && !is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    
    # Ensure we have enough panels to fit all data
    if (panel_rows * panel_cols < num_panels) {
      panel_rows <- ceiling(num_panels / panel_cols)
    }
    
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_rows) && !is.null(panel_cols)) {
    # Check if there are enough panels and adjust if needed
    if (panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting layout to fit all panels.")
      # Preferentially adjust columns to fit all panels
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  }
  
  # AUTO CALCULATE LAYOUT WHEN NEITHER DIMENSION IS SPECIFIED
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
      new_rows <- ceiling(num_panels / new_cols)
      rows <- new_rows
      cols <- new_cols
    }
    
    return(list(rows = rows, cols = cols))
  }
}


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

.handle_plot_title_and_export <- function(
    var_name = NULL,
    sep_value = NULL,
    x_value = NULL,
    plot_type = NULL,  # "comparison", "detail", "stack", "unstack"
    is_macro_mode = FALSE,
    split_by = NULL,
    x_axis_from = NULL,
    variable_col = NULL,
    unit_name = NULL,
    style_config = NULL,
    data = NULL) {
  
  # DETERMINE BASE TITLE
  if (is_macro_mode) {
    plot_title <- coalesce(
      var_name, 
      coalesce(
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
  
  # APPLY TITLE FORMAT
  if (!is.null(style_config$title_format)) {
    title_format <- style_config$title_format
    switch(title_format$type,
           "prefix" = {
             plot_title <- paste0(title_format$text, " - ", plot_title)
           },
           "suffix" = {
             plot_title <- paste0(plot_title, " - ", title_format$text)
           },
           "full" = {
             plot_title <- title_format$text
           },
           "dynamic" = {
             if (!is.null(data) && !is.null(title_format$text)) {
               separator <- if (!is.null(title_format$sep)) title_format$sep else " - "
               
               cols_to_use <- title_format$text
               valid_cols <- cols_to_use[cols_to_use %in% names(data)]
               
               if (length(valid_cols) > 0) {
                 unique_values <- list()
                 for (col in valid_cols) {
                   vals <- unique(as.character(data[[col]]))
                   unique_values[[col]] <- vals
                 }
                 
                 all_values <- unlist(unique_values)
                 deduped_values <- unique(all_values)
                 
                 plot_title <- paste(deduped_values, collapse = separator)
               }
             }
           }
    )
  }
  
  # ADD UNIT IF CONFIGURED
  if (style_config$add_unit_to_title && !is.null(unit_name)) {
    if (tolower(unit_name) == "percent") {
      plot_title <- paste0(plot_title, " (%)")
    } else {
      plot_title <- paste0(plot_title, " (", unit_name, ")")
    }
  }
  
  # CLEAN TITLE FOR EXPORT NAME
  clean_title <- gsub("[^a-zA-Z0-9\\s]", "", plot_title)
  export_name <- gsub("\\s+", "_", clean_title)
  export_name <- gsub("_+", "_", export_name)
  export_name <- gsub("^_|_$", "", export_name)
  
  # ADD PLOT TYPE SUFFIX
  if (!is.null(plot_type)) {
    if (plot_type == "stack") {
      export_name <- paste0(export_name, "_stack")
    } else if (plot_type == "unstack") {
      export_name <- paste0(export_name, "_unstack")
    }
  }
  
  return(list(
    title = plot_title,
    export_name = export_name
  ))
}

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
      data <- data[data[[x_axis_from]] %in% filter_var, ]
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
            data = panel_data
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
              data = panel_data
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
    export_config <- coalesce(export_config, list())
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
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}


detail_plot <- function(data, filter_var = NULL,
                        x_axis_from,
                        split_by = NULL,
                        panel_var = "Experiment",
                        variable_col = "Variable",
                        unit_col = "Unit",
                        desc_col = "Description",
                        var_name_by_description = FALSE,
                        add_var_info = FALSE,
                        output_path = NULL,
                        export_picture = TRUE,
                        export_as_pdf = FALSE,
                        export_config = NULL,
                        top_impact = NULL,
                        separate_figure = FALSE,
                        invert_pane = FALSE,
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
      data <- data[data[[x_axis_from]] %in% filter_var, ]
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
    all_font_size <- coalesce(plot_style_config$all_font_size, 1)
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
              data = panel_data
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
                data = panel_data
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
    export_config <- coalesce(export_config, list())
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
  
  # RETURN SINGLE PLOT OR LIST OF PLOTS
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}


stack_plot <- function(data, filter_var = NULL,
                       x_axis_from,
                       stack_value_from,
                       split_by = NULL,
                       panel_var = "Experiment",
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       invert_pane = FALSE,
                       separate_figure = FALSE,
                       unstack_plot = FALSE,
                       output_path = NULL,
                       export_picture = TRUE,
                       export_as_pdf = FALSE,
                       export_config = NULL,
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       show_total = TRUE,
                       top_impact = NULL,
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
      data <- data[data[[x_axis_from]] %in% filter_var, ]
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
    all_font_size <- coalesce(plot_style_config$all_font_size, 1)
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
  
  # SET DEFAULT FILE_NAME IN EXPORT_CONFIG
  if (is.null(export_config) || is.null(export_config$file_name)) {
    export_config <- coalesce(export_config, list())
    
    plot_type_name <- if (unstack_plot) "Unstacked_plots" else "Stacked_plots"
    n_plots <- length(plot_list)
    export_config$file_name <- paste0(plot_type_name, "_", n_plots)
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
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}