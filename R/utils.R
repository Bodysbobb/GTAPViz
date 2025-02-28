#' @title Calculate Panel Layout (Internal)
#'
#' @description Determines the optimal panel layout for plotting based on the number of experiments
#' or unique values of an x-axis variable.
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
  num_panels <- if(compare_by_x_axis && !is.null(x_axis_from)) {
    length(unique(data[[x_axis_from]]))
  } else {
    length(unique(data$Experiment))
  }

  if (!is.null(panel_rows) && !is.null(panel_cols)) {
    if(panel_rows * panel_cols < num_panels) {
      warning("Provided dimensions insufficient. Adjusting columns to fit all panels.")
      panel_cols <- ceiling(num_panels / panel_rows)
    }
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_rows)) {
    panel_cols <- ceiling(num_panels / panel_rows)
    return(list(rows = panel_rows, cols = panel_cols))
  } else if (!is.null(panel_cols)) {
    panel_rows <- ceiling(num_panels / panel_cols)
    return(list(rows = panel_rows, cols = panel_cols))
  } else {
    if(num_panels <= 1) {
      return(list(rows = 1, cols = 1))
    } else if(num_panels <= 3) {
      return(list(rows = 1, cols = num_panels))
    } else {
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

      if(cols > 2 * rows) {
        new_cols <- ceiling(sqrt(num_panels))
        new_rows <- ceiling(num_panels / new_cols)
        rows <- new_rows
        cols <- new_cols
      }

      return(list(rows = rows, cols = cols))
    }
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
#' @keywords internal
#'
.generate_comparison_colors <- function(data, color_tone = NULL, compare_by_x_axis = FALSE, x_axis_from = NULL) {
  if(is.null(color_tone)) return(NULL)

  n_colors <- if(compare_by_x_axis) {
    length(unique(data$Experiment))
  } else {
    length(unique(data[[x_axis_from]]))
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

#' @title Generate Color Palette (Internal)
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

