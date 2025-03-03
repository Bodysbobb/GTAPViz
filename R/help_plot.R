# Color Tone --------------------------------------------------------------


#' Generate Academic and Themed Color Palettes
#'
#' @description Creates professionally designed color palettes for various plot types
#' based on the specified theme or color tone, while preserving the variation logic.
#'
#' @param color_tone Character. Base color, palette name, or theme ("academic", "purdue", etc.)
#' @param n_colors Numeric. Number of colors needed in the palette
#' @param palette_type Character. Type of palette to generate: "sequential", "diverging", or "qualitative"
#'
#' @return A character vector of colors in hexadecimal format
#' @keywords internal
#'
.create_color_palette <- function(color_tone = NULL, n_colors = 5,
                                  palette_type = "qualitative") {

  # Pre-defined academic and themed palettes
  themed_palettes <- list(
    # Academic palettes (journal publication friendly)
    academic = list(
      qualitative = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB"),
      sequential = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
      diverging = c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
    ),

    # Purdue University official colors
    purdue = list(
      qualitative = c("#9D9E9E", "#DAAA00", "#C28E0E", "#000000", "#7A6E0B", "#98700D", "#4D4038"),
      sequential = c("#FFFFFF", "#F6F0D8", "#EBE1B2", "#E2D48E", "#DAAA00", "#C28E0E", "#98700D", "#7A6E0B", "#000000"),
      diverging = c("#000000", "#4D4038", "#98700D", "#C28E0E", "#DAAA00", "#E2D48E", "#EBE1B2", "#F6F0D8", "#FFFFFF")
    ),

    # Color-blind friendly palette
    colorblind = list(
      qualitative = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
      sequential = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"),
      diverging = c("#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F")
    ),

    # Economics journal palette
    economic = list(
      qualitative = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
      sequential = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"),
      diverging = c("#1A1A40", "#306BAC", "#84B5D5", "#DEF5F7", "#FEFEFE", "#FEDEBE", "#DB7352", "#A6313A", "#67001F")
    ),

    # Trade analysis palette
    trade = list(
      qualitative = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F"),
      sequential = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
      diverging = c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
    )
  )

  # Check if color_tone is a theme name
  if (!is.null(color_tone) && tolower(color_tone) %in% names(themed_palettes)) {
    palette <- themed_palettes[[tolower(color_tone)]][[palette_type]]

    # Ensure we have enough colors
    if (length(palette) < n_colors) {
      palette <- grDevices::colorRampPalette(palette)(n_colors)
    } else if (length(palette) > n_colors) {
      palette <- palette[1:n_colors]
    }

    return(palette)
  }

  # If not a theme, return NULL to indicate we should use the existing color generation logic
  return(NULL)
}

#' @title Generate Comparison Colors (Internal)
#'
#' @description Generates a color palette for comparison plots based on a base color tone.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param color_tone Character. Base color for generating shades.
#' @param compare_by_x_axis Logical. If `TRUE`, colors are generated based on `Experiment` values.
#' @param x_axis_from Character. Column name used when `compare_by_x_axis = TRUE`.
#'
#' @return A vector of colors in hexadecimal format.
#' @author Pattawee Puangchit
#' @importFrom colorspace hex2RGB hex polarLUV
#' @importFrom grDevices col2rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
#'
.generate_comparison_colors <- function(data, color_tone = NULL, compare_by_x_axis = FALSE, x_axis_from = NULL) {
  if(is.null(color_tone)) return(NULL)

  n_colors <- if(compare_by_x_axis) {
    length(unique(data$Experiment))
  } else {
    length(unique(data[[x_axis_from]]))
  }

  # Check if we have a themed palette
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


#' @title Generate Color Palette for Positive/Negative Comparison (Internal)
#'
#' @description Generates a color palette with shades for positive, negative, and neutral values.
#'
#' @param positive_color Character. Hex code for the positive color.
#' @param negative_color Character. Hex code for the negative color.
#'
#' @return A named vector containing hex codes for different shades.
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @author Pattawee Puangchit
#' @keywords internal
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


#' @title Generate Stack Colors (Internal)
#'
#' @description Generates a diverse color palette for stacked bar components, ensuring good contrast between items.
#'
#' @param data Data frame containing the stack value column
#' @param stack_value_from Column name containing the stack categories
#' @param color_tone Optional base color to influence the palette
#'
#' @return A named vector of colors for each stack component.
#' @author Pattawee Puangchit
#' @keywords internal
#'
.generate_stack_colors <- function(data, stack_value_from, color_tone = NULL) {
  # Get unique stack components
  components <- unique(data[[stack_value_from]])
  n_components <- length(components)

  if (n_components <= 1) {
    return(setNames(c("#4477AA"), components))
  }

  # Check if we can use the themed color palette
  themed_palette <- .create_color_palette(color_tone = color_tone, n_colors = n_components,
                                          palette_type = "qualitative")

  if (!is.null(themed_palette)) {
    # Use the themed palette generated by .create_color_palette
    return(setNames(themed_palette, components))
  }

  # Fallback to the original logic if themed palette is not available
  # (This shouldn't happen since .create_color_palette should always return a palette)
  default_palette <- c("#4C78A8", "#F58518", "#E45756", "#72B7B2", "#54A24B", "#EECA3B", "#B279A2", "#FF9DA6")

  # Ensure we have enough colors by recycling if needed
  if (n_components > length(default_palette)) {
    default_palette <- rep(default_palette, ceiling(n_components / length(default_palette)))
  }

  # Trim to the number needed and assign names
  default_palette <- default_palette[1:n_components]
  return(setNames(default_palette, components))
}
# Panel Layout ------------------------------------------------------------

#' @title Calculate Panel Layout (Internal)
#'
#' @description Determines the optimal panel layout for plotting based on the number of panels needed.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param panel_rows Optional. Number of rows for the panel layout.
#' @param panel_cols Optional. Number of columns for the panel layout.
#' @param compare_by_x_axis Logical. If `TRUE`, layout is based on unique values of `x_axis_from` instead of `Experiment`.
#' @param x_axis_from Character. Column name to use when `compare_by_x_axis = TRUE`.
#'
#' @return A list with `rows` and `cols` specifying the calculated panel layout.
#' @author Pattawee Puangchit
#' @keywords internal
#'
.calculate_panel_layout <- function(data, panel_rows = NULL, panel_cols = NULL,
                                    compare_by_x_axis = FALSE, x_axis_from = NULL) {
  # Determine the number of panels needed
  num_panels <- if(compare_by_x_axis && !is.null(x_axis_from)) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  # Return early if both panel_rows and panel_cols are specified
  if (!is.null(panel_rows) && !is.null(panel_cols)) {
    # Check if provided dimensions are sufficient
    if(panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  }

  # Handle when only one dimension is specified
  if (!is.null(panel_rows)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  }

  # Auto calculate layout when neither dimension is specified
  if(num_panels <= 1) {
    return(list(rows = 1, cols = 1))
  } else if(num_panels <= 3) {
    return(list(rows = 1, cols = num_panels))
  } else if(num_panels <= 4) {
    return(list(rows = 2, cols = 2))
  } else if(num_panels <= 6) {
    return(list(rows = 2, cols = 3))
  } else if(num_panels <= 9) {
    return(list(rows = 3, cols = 3))
  } else if(num_panels <= 12) {
    return(list(rows = 3, cols = 4))
  } else {
    # For larger numbers, find the best grid layout
    factors <- c()
    for(i in 1:sqrt(num_panels)) {
      if(num_panels %% i == 0) {
        factors <- c(factors, i)
      }
    }

    if(length(factors) > 0) {
      best_factor <- factors[length(factors)]
      rows <- best_factor
      cols <- num_panels / best_factor
    } else {
      cols <- ceiling(sqrt(num_panels))
      rows <- ceiling(num_panels / cols)
    }

    # Avoid very wide, short layouts
    if(cols > 2 * rows) {
      new_cols <- ceiling(sqrt(num_panels))
      new_rows <- ceiling(num_panels / new_cols)
      rows <- new_rows
      cols <- new_cols
    }

    return(list(rows = rows, cols = cols))
  }
}

#' @title Calculate Plot Dimensions (Internal)
#'
#' @description Determines the appropriate width and height for a plot based on the panel layout.
#'
#' @param data A data frame containing the relevant plotting data.
#' @param panel_layout A list containing `rows` and `cols` specifying the panel layout.
#' @param compare_by_x_axis Logical. If `TRUE`, adjusts dimensions based on comparison type.
#'
#' @return A list with `width` and `height` specifying the calculated plot dimensions.
#' @author Pattawee Puangchit
#' @keywords internal
#'
.calculate_plot_dimensions <- function(data, panel_layout, compare_by_x_axis = FALSE) {
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


#' @title Calculate Dynamic Plot Style Parameters (Internal)
#'
#' @description Automatically calculates appropriate dimensions and font sizes
#' for plots based on the number of elements and layout.
#'
#' @param data A data frame containing the plot data
#' @param x_axis_var Name of the column used for x-axis
#' @param panel_var Name of the column used for panel faceting
#' @param panel_rows Number of panel rows (optional)
#' @param panel_cols Number of panel columns (optional)
#' @param plot_type Type of plot: "comparison", "detail", "macro", or "stack"
#' @param base_size Base font size to scale from (default: 12)
#'
#' @return A list with calculated style parameters including font sizes and chart dimensions
#' @author Pattawee Puangchit
#' @keywords internal
#'
.calculate_plot_style <- function(data, x_axis_var, panel_var = NULL,
                                  panel_rows = NULL, panel_cols = NULL,
                                  plot_type = "comparison", base_size = 12) {

  # Count items to determine scaling factors
  n_x_items <- length(unique(data[[x_axis_var]]))
  n_panels <- if (!is.null(panel_var) && panel_var %in% names(data)) {
    length(unique(data[[panel_var]]))
  } else {
    1
  }

  # Calculate panel layout if not provided
  if (is.null(panel_rows) || is.null(panel_cols)) {
    # Use existing panel calculation or simple fallback
    if (exists(".calculate_panel_layout")) {
      layout <- .calculate_panel_layout(data, panel_rows, panel_cols, FALSE, panel_var)
      panel_rows <- layout$rows
      panel_cols <- layout$cols
    } else {
      # Simple fallback
      if (n_panels <= 1) {
        panel_rows <- 1
        panel_cols <- 1
      } else if (n_panels <= 4) {
        panel_rows <- 2
        panel_cols <- 2
      } else {
        panel_cols <- ceiling(sqrt(n_panels))
        panel_rows <- ceiling(n_panels / panel_cols)
      }
    }
  }

  # Scaling factors based on plot elements
  complexity_factor <- log10(max(2, n_x_items)) * log10(max(2, n_panels))
  complexity_factor <- min(2, max(0.5, complexity_factor / 3))

  # Adjust font sizes based on complexity and plot type
  font_scaling <- switch(plot_type,
                         "detail" = list(
                           label = 0.8,  # Value labels tend to be smaller in detail plots
                           axis = 1.0,   # Axis text is standard
                           title = 1.2   # Titles slightly larger
                         ),
                         "stack" = list(
                           label = 0.9,  # Labels in stack plots need to be visible
                           axis = 1.0,   # Standard axis text
                           title = 1.1   # Standard titles
                         ),
                         "macro" = list(
                           label = 1.0,  # Macro plots often highlight the values
                           axis = 1.1,   # Slightly larger axis text
                           title = 1.2   # Slightly larger titles
                         ),
                         # Default/comparison
                         list(
                           label = 1.0,  # Standard label size
                           axis = 1.0,   # Standard axis text
                           title = 1.1   # Standard titles
                         )
  )

  # Calculate bar dimensions based on number of items
  bar_width <- max(0.05, min(0.9, 10 / (n_x_items + 5)))
  bar_spacing <- max(0.05, min(0.8, 8 / (n_x_items + 5)))

  # Calculate relative font sizes
  base_adjustment <- 1 / complexity_factor
  label_fontsize <- base_size * font_scaling$label * base_adjustment
  axis_title_fontsize <- base_size * 1.5 * font_scaling$title * base_adjustment
  axis_text_fontsize <- base_size * 1.2 * font_scaling$axis * base_adjustment
  plot_title_fontsize <- base_size * 2.0 * font_scaling$title * base_adjustment
  strip_text_fontsize <- base_size * 1.3 * font_scaling$title * base_adjustment

  # Absolute minimum sizes
  label_fontsize <- max(7, label_fontsize)
  axis_title_fontsize <- max(10, axis_title_fontsize)
  axis_text_fontsize <- max(8, axis_text_fontsize)
  plot_title_fontsize <- max(12, plot_title_fontsize)
  strip_text_fontsize <- max(10, strip_text_fontsize)

  # Calculate panel spacing (using a numeric value, not a string)
  panel_spacing_value <- max(0.5, 3 / complexity_factor)

  # Return a list of style parameters
  return(list(
    # Font sizes
    label_size = label_fontsize,
    axis_title_size = axis_title_fontsize,
    axis_text_size = axis_text_fontsize,
    plot_title_size = plot_title_fontsize,
    strip_text_size = strip_text_fontsize,
    legend_text_size = axis_text_fontsize,

    # Bar dimensions
    bar_width = bar_width,
    bar_spacing = bar_spacing,

    # Panel dimensions
    panel_rows = panel_rows,
    panel_cols = panel_cols,

    # Margins - as numeric value, not string
    panel_spacing = panel_spacing_value
  ))
}

# Misc. -------------------------------------------------------------------


#' Get Title Mapping for Plot Labels (Internal)
#'
#' @description Determines appropriate title names for plotting based on available columns
#' in the dataset. Uses the .format_variable_names logic for consistency with table generation.
#'
#' @param data A data frame or a list of data frames containing the dataset
#' @param var_name_by_description Logical. If TRUE, uses Description instead of Variable names
#' @param add_var_info Logical. If TRUE, includes original name in parentheses
#'
#' @return A named vector where variable names map to their display names
#' @author Pattawee Puangchit
#' @keywords internal
#'
.get_title_mapping <- function(data, var_name_by_description = TRUE, add_var_info = FALSE) {
  extract_title_mapping <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    if (!"Variable" %in% names(df)) return(NULL)

    # Apply the variable name formatting
    formatted_df <- .format_variable_names(df, var_name_by_description, add_var_info)

    # Create mapping from original Variable to formatted Variable
    unique_vars <- unique(data.frame(
      OrigVar = df$Variable,
      DisplayVar = formatted_df$Variable,
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


#' Format Variable Names for Display (Internal)
#'
#' Standardizes how variables are displayed in plots, with options to use description
#' text instead of variable names and optionally include both.
#'
#' @param data A data frame containing "Variable" and optionally "Description" columns
#' @param var_name_by_description Logical. If TRUE, replaces Variable names with Description
#' @param add_var_info Logical. If TRUE, appends the other part in parentheses
#'
#' @return A data frame with formatted Variable column
#' @author Pattawee Puangchit
#' @keywords internal
#'
.format_variable_names <- function(data, var_name_by_description = TRUE, add_var_info = FALSE) {
  if (!is.data.frame(data) || !"Variable" %in% names(data))
    return(data)

  # If no Description column, return data unchanged
  if (!"Description" %in% names(data))
    return(data)

  result <- data

  for (i in seq_len(nrow(result))) {
    var_ <- result$Variable[i]
    des_ <- result$Description[i]

    # Handle missing or empty description
    if (is.na(des_) || !nzchar(des_))
      des_ <- var_

    if (var_name_by_description && add_var_info) {
      # Both: Description (Variable)
      result$Variable[i] <- paste0(des_, " (", var_, ")")
    } else if (var_name_by_description && !add_var_info) {
      # Description only
      result$Variable[i] <- des_
    } else if (!var_name_by_description && add_var_info) {
      # Variable (Description), but only if different
      if (des_ != var_) {
        result$Variable[i] <- paste0(var_, " (", des_, ")")
      }
    } else {
      # Variable only (default, do nothing)
    }
  }

  return(result)
}
