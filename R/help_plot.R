# PLOT STYLE CONFIG HELPERS -----------------------------------------

#' @title Calculate Plot Style Configuration
#'
#' @description Merges user-defined plot style configurations with defaults for different plot types.
#' This function is internal and used by plotting functions to define visual styling.
#'
#' @param config Optional list with custom style configuration parameters.
#' @param plot_type Type of plot: `"comparison"`, `"detail"`, or `"stack"`.
#'
#' @return A list with complete style configuration for the specified plot type.
#' @importFrom utils modifyList
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_style_config <- function(config = NULL, plot_type = "comparison") {

  # Default configurations specific to each plot type
  comparison_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 20,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(10, 0, 10, 0),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 16,
    x_axis_title_margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "bold",
    x_axis_text_size = 14,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 16,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 14,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 0,
    y_axis_description = "",

    # Axis Label across panel
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
    legend_position = "none",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 14,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 16,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 2,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = FALSE,
    show_grid_major_y = TRUE,
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

    # Scale expansion settings
    expansion_y_mult = c(0.05, 0.1),
    expansion_x_mult = c(0.05, 0.05)
  )

  detail_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 48,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(b = 30),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 32,
    x_axis_title_margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "plain",
    x_axis_text_size = 32,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 32,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 50, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 32,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 1,
    y_axis_description = "",

    # Axis Label across panel
    show_axis_titles_on_all_facets = TRUE,

    # Value label settings
    show_value_labels = TRUE,
    value_label_face = "plain",
    value_label_size = 7,
    value_label_position = "outside",
    value_label_decimal_places = 2,

    # Legend settings
    show_legend = FALSE,
    show_legend_title = FALSE,
    legend_position = "none",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 14,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 25,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 1,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
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
    bar_width = 0.4,
    bar_spacing = 0.5,

    # Scale expansion settings
    expansion_y_mult = c(0.2, 0.2),
    expansion_x_mult = c(0.05, 0.05)
  )

  stack_defaults <- list(
    # Title settings
    show_title = TRUE,
    title_face = "bold",
    title_size = 32,
    title_hjust = 0.5,
    add_unit_to_title = TRUE,
    title_margin = ggplot2::margin(b = 15),
    title_format = list(type = "standard", text = ""),

    # X-Axis settings
    show_x_axis_title = TRUE,
    x_axis_title_face = "bold",
    x_axis_title_size = 20,
    x_axis_title_margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    show_x_axis_labels = TRUE,
    x_axis_text_face = "bold",
    x_axis_text_size = 18,
    x_axis_text_angle = 45,
    x_axis_text_hjust = 1,
    x_axis_description = "",

    # Y-Axis settings
    show_y_axis_title = TRUE,
    y_axis_title_face = "bold",
    y_axis_title_size = 20,
    y_axis_title_margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    show_y_axis_labels = TRUE,
    y_axis_text_face = "plain",
    y_axis_text_size = 18,
    y_axis_text_angle = 0,
    y_axis_text_hjust = 0,
    y_axis_description = "",

    # Axis Label across panel
    show_axis_titles_on_all_facets = TRUE,

    # Value label settings
    show_value_labels = TRUE,
    value_label_face = "plain",
    value_label_size = 5,
    value_label_position = "top",
    value_label_decimal_places = 2,

    # Legend settings
    show_legend = TRUE,
    show_legend_title = FALSE,
    legend_position = "bottom",
    legend_title_face = "bold",
    legend_text_face = "plain",
    legend_text_size = 18,

    # Panel strip settings
    strip_face = "bold",
    strip_text_size = 18,
    strip_background = "lightgrey",
    strip_text_margin = ggplot2::margin(10, 0, 10, 0),

    # Panel layout
    panel_spacing = 1,
    panel_rows = NULL,
    panel_cols = NULL,
    theme = NULL,

    # Color settings
    color_tone = NULL,
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    background_color = "white",
    grid_color = "grey90",
    show_grid_major_x = FALSE,
    show_grid_major_y = TRUE,
    show_grid_minor_x = FALSE,
    show_grid_minor_y = FALSE,

    # Zero line settings
    show_zero_line = TRUE,
    zero_line_type = "dashed",
    zero_line_color = "black",
    zero_line_size = 0.5,
    zero_line_position = 0,

    # Bar chart settings
    bar_width = 0.7,
    bar_spacing = 0,

    # Scale expansion settings
    expansion_y_mult = c(0.1, 0.1),
    expansion_x_mult = c(0.05, 0.05)
  )

  # Select the appropriate default based on plot type
  default_config <- switch(plot_type,
                           "comparison" = comparison_defaults,
                           "detail" = detail_defaults,
                           "stack" = stack_defaults,
                           comparison_defaults)  # Default to comparison if unspecified

  # If no config is provided, return the default
  if (is.null(config)) {
    return(default_config)
  }

  # Merge user config with defaults (user settings take precedence)
  final_config <- modifyList(default_config, config)
  return(final_config)
}


#' @title Apply Plot Style Configuration to a ggplot Object
#'
#' @description Applies a style configuration to a ggplot object. This is an internal function
#' used by the plotting functions.
#'
#' @param p A ggplot object to modify.
#' @param config A list containing style configuration parameters.
#'
#' @return A ggplot object with applied styling.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.apply_plot_style_config <- function(p, config) {

  # Apply theme modifications
  p <- p + ggplot2::theme(
    # Title settings
    plot.title = if (config$show_title) {
      ggplot2::element_text(
        hjust = config$title_hjust,
        size = config$title_size,
        face = config$title_face,
        margin = config$title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # X axis title settings
    axis.title.x = if (config$show_x_axis_title) {
      ggplot2::element_text(
        size = config$x_axis_title_size,
        face = config$x_axis_title_face,
        margin = config$x_axis_title_margin
      )
    } else {
      ggplot2::element_blank()
    },

    # Y axis title settings
    axis.title.y = if (config$show_y_axis_title) {
      ggplot2::element_text(
        size = config$y_axis_title_size,
        face = config$y_axis_title_face,
        margin = config$y_axis_title_margin
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
      margin = config$strip_text_margin
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

  # Apply zero line if configured
  if (config$show_zero_line) {
    # Remove any existing zero line (geom_hline with yintercept=0)
    p$layers <- p$layers[!sapply(p$layers, function(l) {
      if ("geom_hline" %in% class(l$geom) && !is.null(l$data)) {
        if (!is.null(l$data$yintercept) && any(l$data$yintercept == 0)) {
          return(TRUE)
        }
      }
      return(FALSE)
    })]

    # Add the new zero line with configured properties
    p <- p + ggplot2::geom_hline(
      yintercept = config$zero_line_position,
      linetype = config$zero_line_type,
      color = config$zero_line_color,
      size = config$zero_line_size
    )
  }

  # Apply custom theme if provided
  if (!is.null(config$theme)) {
    p <- p + config$theme
  }

  return(p)
}


# COLUMN HANDLING HELPERS -----------------------------------------

#' @title Find Column in Data Frame
#'
#' @description Finds a column in a data frame using case-insensitive matching.
#'
#' @param data A data frame to search for the column.
#' @param col_name Name of the column to find.
#' @param is_required Logical. If TRUE, stops with error if column not found.
#' @param default_name Optional default name to return if column not found.
#'
#' @return The actual column name if found, default_name if provided, or NULL.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
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


#' @title Prepare Data Source from List or Data Frame
#'
#' @description Finds a suitable data frame from a list or returns the input data frame.
#'
#' @param data A data frame or list of data frames.
#' @param required_columns Optional vector of column names that must be present.
#'
#' @return A data frame that contains the required columns.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.prepare_data_source <- function(data, required_columns = NULL) {
  # Handle the case where data is a list of data frames
  if (is.list(data) && !is.data.frame(data)) {
    for (df_name in names(data)) {
      df <- data[[df_name]]
      if (!is.data.frame(df)) next

      # If no required columns, return the first dataframe
      if (is.null(required_columns)) return(df)

      # Check if df has the required columns (case-insensitive)
      has_required <- sapply(required_columns, function(col) {
        any(tolower(names(df)) == tolower(col))
      })

      if (all(has_required)) {
        return(df)
      }
    }

    if (!is.null(required_columns)) {
      stop(paste("No suitable dataframe found with required columns:",
                 paste(required_columns, collapse=", ")))
    } else {
      stop("No suitable dataframe found")
    }
  }

  # If data is already a data frame, return it directly
  if (is.data.frame(data)) {
    return(data)
  }

  stop("Input must be a data frame or a list of data frames")
}


#' @title Check and Standardize Unit Column
#'
#' @description Checks for the presence of a unit column and adds a default if missing.
#'
#' @param data A data frame to check.
#' @param unit_col Name of the unit column to check for (default: "Unit").
#'
#' @return A list with two elements: data (the possibly modified data frame) and unit_col (the actual unit column name).
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
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


#' @title Process Split-By Parameter
#'
#' @description Processes the split_by parameter to handle different cases consistently.
#'
#' @param data A data frame to process.
#' @param split_by NULL, a logical value, or a character vector of column names.
#'
#' @return A list with three elements: data (the possibly modified data frame), is_macro_mode (logical), and split_by (the processed split-by column names).
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.process_split_by <- function(data, split_by) {
  # Process split_by parameter
  if (is.null(split_by) || (is.logical(split_by) && !split_by)) {
    return(list(data = data, is_macro_mode = TRUE, split_by = NULL))
  }

  # For single column split_by
  if (length(split_by) == 1) {
    actual_split_by <- .find_column(data, split_by)

    if (is.null(actual_split_by)) {
      warning(paste("Split-by column", split_by, "not found. Using default values."))
      data[[split_by]] <- "Default"
      actual_split_by <- split_by
    }

    return(list(data = data, is_macro_mode = FALSE, split_by = actual_split_by))
  }

  # For multiple split_by columns
  actual_split_by <- character(length(split_by))
  for (i in seq_along(split_by)) {
    found_col <- .find_column(data, split_by[i])

    if (is.null(found_col)) {
      warning(paste("Split-by column", split_by[i], "not found. Using default values."))
      data[[split_by[i]]] <- "Default"
      actual_split_by[i] <- split_by[i]
    } else {
      actual_split_by[i] <- found_col
    }
  }

  return(list(data = data, is_macro_mode = FALSE, split_by = actual_split_by))
}


#' @title Format Variable Names Using Descriptions
#'
#' @description Formats variable names using available descriptions for better readability.
#'
#' @param data A data frame containing variable and description columns.
#' @param variable_col Name of the column containing variable identifiers.
#' @param desc_col Name of the column containing descriptions.
#' @param var_name_by_description Logical. If TRUE, replaces variable names with descriptions.
#' @param add_var_info Logical. If TRUE, adds additional information in parentheses.
#'
#' @return A data frame with formatted variable names.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.format_variable_names <- function(data, variable_col, desc_col,
                                   var_name_by_description = TRUE, add_var_info = FALSE) {
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


#' @title Get Title Mapping From Variable Names and Descriptions
#'
#' @description Creates a mapping from original variable names to formatted display names.
#'
#' @param data A data frame or list of data frames containing variable and description columns.
#' @param variable_col Name of the column containing variable identifiers.
#' @param desc_col Name of the column containing descriptions.
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable names.
#' @param add_var_info Logical. If TRUE, adds additional information in parentheses.
#'
#' @return A named list mapping original variable names to formatted display names.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.get_title_mapping <- function(data, variable_col, desc_col,
                               var_name_by_description = TRUE, add_var_info = FALSE) {
  extract_title_mapping <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    if (!variable_col %in% names(df)) return(NULL)

    # Apply the variable name formatting
    formatted_df <- .format_variable_names(df, variable_col, desc_col, var_name_by_description, add_var_info)

    # Create mapping from original Variable to formatted Variable
    unique_vars <- unique(data.frame(
      OrigVar = df[[variable_col]],
      DisplayVar = formatted_df[[variable_col]],
      stringsAsFactors = FALSE
    ))

    setNames(unique_vars$DisplayVar, unique_vars$OrigVar)
  }

  if (is.data.frame(data)) {
    return(extract_title_mapping(data))
  } else if (is.list(data)) {
    result <- lapply(data, extract_title_mapping)
    result <- result[!sapply(result, is.null)]
    return(do.call(c, result))
  } else {
    stop("Unsupported data type. Input should be a data frame or a list of data frames.")
  }
}


# COLOR PALETTE  -----------------------------------

#' @title Create Academic and Themed Color Palettes
#'
#' @description Creates professionally designed color palettes for various plot types
#' based on the specified theme or color tone.
#'
#' @param color_tone Character. Base color, palette name, or theme ("academic", "purdue", etc.).
#' @param n_colors Numeric. Number of colors needed in the palette.
#' @param palette_type Character. Type of palette to generate: "sequential", "diverging", or "qualitative".
#'
#' @return A character vector of colors in hexadecimal format, or NULL if color_tone isn't a recognized theme.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.create_color_palette <- function(color_tone = NULL, n_colors = 5,
                                  palette_type = "qualitative") {

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
    )
  )

  if (!is.null(color_tone) && tolower(color_tone) %in% names(themed_palettes)) {
    palette <- themed_palettes[[tolower(color_tone)]][[palette_type]]

    if (length(palette) < n_colors) {
      palette <- grDevices::colorRampPalette(palette)(n_colors)
    } else if (length(palette) > n_colors) {
      palette <- palette[1:n_colors]
    }

    return(palette)
  }

  return(NULL)
}


#' @title Generate Comparison Colors for Bar Charts
#'
#' @description Generates a color palette for comparison plots based on a base color tone.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param color_tone Character. Base color for generating shades, or a theme name.
#' @param axis_col Column name used for determining the number of unique colors needed.
#'
#' @return A vector of colors in hexadecimal format, or NULL if color_tone is NULL.
#'
#' @importFrom colorspace hex2RGB hex polarLUV
#' @importFrom grDevices col2rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}
#'
.generate_comparison_colors <- function(data, color_tone = NULL, axis_col) {
  if(is.null(color_tone)) return(NULL)

  n_colors <- length(unique(data[[axis_col]]))

  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_colors,
                                          palette_type = "qualitative")

  if (!is.null(themed_palette)) {
    return(themed_palette)
  }

  base_color <- if(startsWith(color_tone, "#")) {
    color_tone
  } else {
    colorspace::hex(colorspace::sRGB(t(col2rgb(color_tone) / 255)))
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


#' @title Generate Color Palette for Positive/Negative Comparison
#'
#' @description Generates a color palette with shades for positive, negative, and neutral values.
#'
#' @param positive_color Character. Hex code or color name for the positive color (default: "#2E8B57", sea green).
#' @param negative_color Character. Hex code or color name for the negative color (default: "#CD5C5C", indian red).
#'
#' @return A named vector containing hex codes for different value categories.
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{detail_plot}}
#'
.generate_color_palette <- function(positive_color = "#2E8B57", negative_color = "#CD5C5C") {
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


#' @title Generate Colors for Stacked Bar Components
#'
#' @description Generates a color palette for stacked bar components, ensuring good contrast between items.
#'
#' @param data A data frame containing the stack value column.
#' @param stack_value_from Column name containing the stack categories.
#' @param color_tone Optional base color to influence the palette.
#'
#' @return A named vector of colors for each stack component.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{stack_plot}}
#'
.generate_stack_colors <- function(data, stack_value_from, color_tone = NULL) {
  components <- unique(data[[stack_value_from]])
  n_components <- length(components)

  if (n_components <= 1) {
    return(setNames(c("#4477AA"), components))
  }

  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_components,
                                          palette_type = "qualitative")

  if (!is.null(themed_palette)) {
    return(setNames(themed_palette, components))
  }

  default_palette <- c("#4C78A8", "#F58518", "#E45756", "#72B7B2", "#54A24B", "#EECA3B", "#B279A2", "#FF9DA6")

  if (n_components > length(default_palette)) {
    default_palette <- rep(default_palette, ceiling(n_components / length(default_palette)))
  }

  default_palette <- default_palette[1:n_components]
  return(setNames(default_palette, components))
}


# LAYOUT AND DIMENSIONS HELPERS ---------------------------------------

#' @title Calculate Optimal Panel Layout
#'
#' @description Determines the optimal panel layout (rows and columns) for plotting based on the number of panels needed.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param panel_rows Optional. Number of rows for the panel layout.
#' @param panel_cols Optional. Number of columns for the panel layout.
#' @param panel_var Column name to use for determining unique panels.
#'
#' @return A list with 'rows' and 'cols' specifying the calculated panel layout.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL,
                                    panel_var = "Experiment") {
  # DETERMINE NUMBER OF PANELS
  num_panels <- length(unique(data[[panel_var]]))

  # HANDLE WHEN ONLY ONE DIMENSION IS SPECIFIED
  if (!is.null(panel_rows) && is.null(panel_cols)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (is.null(panel_rows) && !is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_rows) && !is.null(panel_cols)) {
    if (panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
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
      cols <- ceiling(sqrt(num_panels))
      rows <- ceiling(num_panels / cols)
    }

    if (cols > 2 * rows) {
      new_cols <- ceiling(sqrt(num_panels))
      new_rows <- ceiling(num_panels / new_cols)
      rows <- new_rows
      cols <- new_cols
    }

    return(list(rows = rows, cols = cols))
  }
}

#' @title Calculate Plot Dimensions
#'
#' @description Determines appropriate width and height for a plot based on the panel layout.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param panel_layout A list containing 'rows' and 'cols' specifying the panel layout.
#'
#' @return A list with 'width' and 'height' specifying the calculated plot dimensions in inches.
#'
#' @author Pattawee Puangchit
#' @keywords internal
#' @seealso \code{\link{comparison_plot}}, \code{\link{detail_plot}}, \code{\link{stack_plot}}
#'
.calculate_plot_dimensions <- function(data, panel_layout) {
  num_panels <- panel_layout$rows * panel_layout$cols
  base_width <- 20
  base_height <- 30

  width <- if(num_panels <= 4) {
    base_width
  } else {
    min(base_width + (num_panels - 4) * 3.5, 50)
  }

  height <- base_height * 0.75

  return(list(width = width, height = height))
}
