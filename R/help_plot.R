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


# Misc. -------------------------------------------------------------------


#' @title Generate Title Mapping for Plot Labels (Internal)
#'
#' @description Determines appropriate title names for plotting based on available columns in the dataset.
#' If a `Description` column exists and `description_as_title` is `TRUE`, it uses `Description` as titles.
#' Otherwise, it checks for `PlotTitle` or falls back to using `Variable` names.
#'
#' @param data A data frame or a list of data frames containing the dataset.
#' @param description_as_title Logical. If `TRUE`, prioritizes the `Description` column for titles.
#' If `FALSE`, using `Variable` as title name.
#'
#' @return A named vector where variable names map to their corresponding titles.
#' @author Pattawee Puangchit
#' @keywords internal
#'
.get_title_mapping <- function(data, description_as_title = TRUE) {
  extract_title_mapping <- function(df) {
    if (!is.data.frame(df)) return(NULL)

    title_col <- if (description_as_title && "Description" %in% names(df)) {
      "Description"
    } else {
      "Variable"
    }

    setNames(unique(df[[title_col]]), unique(df$Variable))
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

