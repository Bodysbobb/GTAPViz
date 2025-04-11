# SIMPLIFIED MAIN PLOT FUNCTIONS ------------------------------------------

#' @title Create Comparative Bar Charts from HAR and SL4 Data
#'
#' @description
#' Generates comparative bar charts using GTAP data.
#' Supports panel facets, split-by grouping, and fully customizable styling and export options.
#'
#' @md
#'
#' @param data A data frame or list of data frames containing GTAP results.
#' @param filter_var Character vector or data frame.
#' If a vector, filters values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name used for the x-axis (e.g., `"REG"`, `"Sector"`).
#' @param split_by Character or vector. Column(s) used to split plots by group (e.g., `"COMM"`, `"REG"`).
#' If `NULL`, a single aggregated plot is produced.
#' @param panel_var Character. Column for panel facets. Default is `"Experiment"`.
#' @param variable_col Character. Column name for variable codes. Default is `"Variable"`.
#' @param unit_col Character. Column name for units. Default is `"Unit"`.
#' @param desc_col Character. Column name for variable descriptions. Default is `"Description"`.
#' @param invert_axis Logical. If `TRUE`, flips panel layout to horizontal (default: `FALSE`).
#' @param separate_figure Logical. If `TRUE`, generates a separate figure for each panel value (default: `FALSE`).
#' @param var_name_by_description Logical. If `TRUE`, uses descriptions instead of variable codes in titles (default: `FALSE`).
#' @param add_var_info Logical. If `TRUE`, appends variable codes in parentheses after the description (default: `FALSE`).
#' @param output_path Character. Path to output folder for saving plots. If `NULL`, plots are returned only in R.
#' @param export_picture Logical. If `TRUE`, exports plots as images (default: `TRUE`).
#' @param export_as_pdf Logical or `"merged"`.
#' If `TRUE`, saves separate PDFs. If `"merged"`, combines all into a single PDF. If `FALSE`, disables PDF export (default: `FALSE`).
#' @param export_config List. Export settings.
#' See \code{\link{create_export_config}} or \code{\link{get_export_config}}.
#' @param plot_style_config List. Plot styling settings.
#' See \code{\link{create_plot_style_config}} or \code{\link{get_plot_style_config}}.
#'
#' @return A single ggplot2 object or a named list of ggplot2 objects, depending on the configuration.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_all_config}}, \code{\link{detail_plot}}, \code{\link{stack_plot}},
#' \code{\link{create_title_format}}
#' @export
#'
#' @examples
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Basic usage with data frame
#' p1 <- comparison_plot(
#'   data = sl4.plot.data[["REG"]],
#'   x_axis_from = "Region",
#'   panel_var = "Experiment",
#'   filter_var = c("qgdp", "EV"),
#'   output_path = NULL,
#'   export_picture = FALSE,
#'   export_as_pdf = FALSE,
#' )
#'
#' # Split by commodity with custom styling and export options
#' p2 <- comparison_plot(
#'   data = sl4.plot.data[["REG"]],
#'   x_axis_from = "Region",
#'   split_by = "Variable",
#'   panel_var = "Experiment",
#'   filter_var = c("qgdp", "EV"),
#'   var_name_by_description = TRUE,
#'   output_path = NULL,
#'   export_picture = FALSE,
#'   export_as_pdf = FALSE,
#'   export_config = list(
#'     width = 12,
#'     height = 8
#'   ),
#'   plot_style_config = list(
#'     color_tone = "gtap",
#'     title_format = create_title_format(
#'     type = "prefix",
#'     text = "Impact on",
#'     sep = "")
#'   )
#' )
#' }
comparison_plot <- function(data, filter_var = NULL,
                            x_axis_from,
                            split_by = NULL,
                            panel_var = "Experiment",
                            variable_col = "Variable",
                            unit_col = "Unit",
                            desc_col = "Description",
                            invert_axis = FALSE,
                            separate_figure = FALSE,
                            var_name_by_description = FALSE,
                            add_var_info = FALSE,
                            output_path = NULL,
                            export_picture = TRUE,
                            export_as_pdf = FALSE,
                            export_config = NULL,
                            plot_style_config = NULL) {

  # Calculate panel layout and style config
  panel_layout <- .calculate_panel_layout(
    data,
    panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
    panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
    panel_var = panel_var
  )

  # Prepare style configuration
  style_config <- .calculate_plot_style_config(
    config = modifyList(
      list(
        panel_rows = panel_layout$rows,
        panel_cols = panel_layout$cols,
        all_font_size = if(!is.null(plot_style_config)) plot_style_config$all_font_size else 1
      ),
      if (!is.null(plot_style_config)) plot_style_config else list()
    ),
    plot_type = "default"
  )

  # Process data with common function
  preprocessed <- .preprocess_data(
    data = data,
    x_axis_from = x_axis_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    filter_var = filter_var,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info
  )

  # Validate data
  if (is.null(preprocessed)) {
    return(NULL)
  }

  data <- preprocessed$data
  is_macro_mode <- preprocessed$is_macro_mode

  # Generate plots using the common function
  plot_list <- .generate_plots(
    data = data,
    unit_col = unit_col,
    panel_var = panel_var,
    x_axis_from = x_axis_from,
    separate_figure = separate_figure,
    style_config = style_config,
    is_macro_mode = is_macro_mode,
    plot_type = "comparison",
    invert_axis = invert_axis,
    variable_col = variable_col,
    split_by = split_by
  )

  # Handle export with common function
  return(.finalize_plot_export(
    plot_list = plot_list,
    data = data,
    panel_layout = panel_layout,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    default_filename = "comparison_plots"
  ))
}

#' @title Create Comprehensive Bar Charts from HAR and SL4 Data
#'
#' @description
#' Generates detailed bar charts to visualize the distribution of impacts across multiple dimensions.
#' The function supports top impact filtering, color coding, and flexible visualization settings.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param split_by Character or vector. Column name(s) to generate separate plots for each unique value (e.g., "COMM", "REG", "Variable").
#' NULL creates a single aggregated plot, appropriate for macro-level analysis.
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param top_impact Numeric or NULL. If specified, shows only the top N impactful values; NULL shows all values.
#' @param invert_axis Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned in R.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF; if FALSE, skips PDF (default: FALSE).
#' @param export_config List. Export settings, including:
#' \itemize{
#'   \item `width`: Output file width (in inches).
#'   \item `height`: Output file height (in inches).
#'   \item Additional settings—see `?get_export_config`.
#' }
#' @param plot_style_config List. Custom plot styles—see `?get_plot_style_config`.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{comparison_plot}}, \code{\link{stack_plot}}
#' @export
#'
#' @examples
#'
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Basic usage with data frame
#' plotA <- detail_plot(sl4.plot.data[["2D"]],
#'             x_axis_from = "Sector",
#'             split_by = "Region",
#'             filter_var = "qo",
#'
#'             top_impact = NULL,
#'             var_name_by_description = TRUE,
#'
#'             invert_axis = TRUE,
#'             separate_figure = FALSE,
#'
#'             export_config = list(
#'               width = 45,
#'               height = 20
#'             ),
#'
#'             export_picture = FALSE,
#'             export_as_pdf = FALSE,
#'             output_path = NULL,
#'
#'             plot_style_config = list(
#'               positive_color = "#2E8B57",
#'               negative_color = "#CD5C5C",
#'               panel_rows = 1,
#'               panel_cols = NULL,
#'               show_axis_titles_on_all_facets = FALSE,
#'               y_axis_text_size = 25,
#'               bar_width = 0.6,
#'               all_font_size = 1.1
#'             ))
#' }
#'
detail_plot <- function(data, filter_var = NULL,
                        x_axis_from,
                        split_by = NULL,
                        panel_var = "Experiment",
                        variable_col = "Variable",
                        unit_col = "Unit",
                        desc_col = "Description",
                        var_name_by_description = FALSE,
                        add_var_info = FALSE,
                        top_impact = NULL,
                        invert_axis = FALSE,
                        separate_figure = FALSE,
                        output_path = NULL,
                        export_picture = TRUE,
                        export_as_pdf = FALSE,
                        export_config = NULL,
                        plot_style_config = NULL) {

  # Calculate panel layout and style config
  panel_layout <- .calculate_panel_layout(
    data,
    panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
    panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
    panel_var = panel_var
  )

  # Prepare style configuration
  style_config <- .calculate_plot_style_config(
    config = modifyList(
      list(
        panel_rows = panel_layout$rows,
        panel_cols = panel_layout$cols,
        all_font_size = if(!is.null(plot_style_config)) plot_style_config$all_font_size else 1
      ),
      if (!is.null(plot_style_config)) plot_style_config else list()
    ),
    plot_type = "default"
  )

  # Process data with common function
  preprocessed <- .preprocess_data(
    data = data,
    x_axis_from = x_axis_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    filter_var = filter_var,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info
  )

  # Validate data
  if (is.null(preprocessed)) {
    return(NULL)
  }

  data <- preprocessed$data
  is_macro_mode <- preprocessed$is_macro_mode

  # Generate plots using the common function
  plot_list <- .generate_plots(
    data = data,
    unit_col = unit_col,
    panel_var = panel_var,
    x_axis_from = x_axis_from,
    separate_figure = separate_figure,
    style_config = style_config,
    is_macro_mode = is_macro_mode,
    plot_type = "detail",
    invert_axis = invert_axis,
    variable_col = variable_col,
    split_by = split_by,
    top_impact = top_impact
  )

  # Define export base name
  export_name_base <- if (!is.null(top_impact)) {
    paste0("detail_plots_top", top_impact)
  } else {
    "detail_plots"
  }

  # Handle export with common function
  return(.finalize_plot_export(
    plot_list = plot_list,
    data = data,
    panel_layout = panel_layout,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    default_filename = export_name_base
  ))
}

#' @title Create Stacked Bar Charts for Decomposition Analysis
#'
#' @description
#' Generates stacked bar charts to visualize value compositions across multiple dimensions.
#' The function supports stacked and unstacked presentations for decomposition analysis.
#'
#' @param data A data frame or a list of data frames containing GTAP results.
#' @param filter_var Vector or data frame. If a vector, filters the values in `x_axis_from`.
#' If a data frame, filters `value_col` based on matching `variable_col` values.
#' @param x_axis_from Character. Column name for x-axis categories (e.g., "REG", "Sector").
#' @param stack_value_from Character. Column containing stack component categories (e.g., "COMM" for commodities).
#' @param split_by Character or vector. Column name(s) to generate separate plots for each unique value (e.g., "COMM", "REG", "Variable").
#' NULL creates a single aggregated plot, appropriate for macro-level analysis.
#' @param panel_var Character. Column for panel facets (default: "Experiment").
#' @param variable_col Character. Column containing variable identifiers (default: "Variable").
#' @param unit_col Character. Column containing unit information (default: "Unit").
#' @param desc_col Character. Column containing variable descriptions (default: "Description").
#' @param var_name_by_description Logical. If TRUE, uses descriptions instead of variable codes in titles (default: FALSE).
#' @param add_var_info Logical. If TRUE, adds the variable code in parentheses after the description (default: FALSE).
#' @param show_total Logical. If TRUE, displays total values above stacked bars (default: TRUE).
#' @param unstack_plot Logical. If TRUE, creates separate bar plots for each `x_axis_from` value instead of stacked bars (default: FALSE).
#' @param top_impact Numeric or NULL. If specified, shows only the top N impactful values; NULL shows all values.
#' @param invert_axis Logical. If TRUE, creates horizontal bars instead of vertical ones (default: FALSE).
#' @param separate_figure Logical. If TRUE, generates separate figures per panel value (default: FALSE).
#' @param output_path Character. Directory path for saving output files. If NULL, plots are only returned in R.
#' @param export_picture Logical. If TRUE, exports plots as image files (default: TRUE).
#' @param export_as_pdf Logical or "merged". If TRUE, exports separate PDFs; if "merged", creates a multi-page PDF; if FALSE, skips PDF (default: FALSE).
#' @param export_config List. Export settings, including:
#' \itemize{
#'   \item `width`: Output file width (in inches).
#'   \item `height`: Output file height (in inches).
#'   \item Additional settings—see `?get_export_config`.
#' }
#' @param plot_style_config List. Custom plot styles—see `?get_plot_style_config`.
#'
#' @return A ggplot2 object for a single plot or a list of ggplot2 objects for multiple plots.
#'
#' @author Pattawee Puangchit
#' @seealso \code{\link{get_plot_style_config}}, \code{\link{get_export_config}}, \code{\link{comparison_plot}}, \code{\link{detail_plot}}
#' @export
#'
#' @examples
#' \donttest{
#' # Load Data:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))
#'
#' plotC <- stack_plot(data = har.plot.data[["A"]],
#'            x_axis_from = "Region",
#'            stack_value_from = "COLUMN",
#'            split_by = FALSE,
#'
#'            show_total = TRUE,
#'            unstack_plot = FALSE,
#'
#'            var_name_by_description = TRUE,
#'
#'            invert_axis = FALSE,
#'            separate_figure = FALSE,
#'
#'            export_picture = FALSE,
#'            export_as_pdf = FALSE,
#'            export_config = list(
#'              width = 28,
#'              height = 15
#'            ),
#'            output_path = NULL,
#'
#'            plot_style_config = list(
#'              color_tone = "gtap",
#'              panel_rows = 2,
#'              show_legend = TRUE,
#'            ))
#' }
stack_plot <- function(data, filter_var = NULL,
                       x_axis_from,
                       stack_value_from,
                       split_by = NULL,
                       panel_var = "Experiment",
                       variable_col = "Variable",
                       unit_col = "Unit",
                       desc_col = "Description",
                       var_name_by_description = FALSE,
                       add_var_info = FALSE,
                       show_total = TRUE,
                       unstack_plot = FALSE,
                       top_impact = NULL,
                       invert_axis = FALSE,
                       separate_figure = FALSE,
                       output_path = NULL,
                       export_picture = TRUE,
                       export_as_pdf = FALSE,
                       export_config = NULL,
                       plot_style_config = NULL) {

  # Calculate panel layout and style config
  panel_layout <- .calculate_panel_layout(
    data,
    panel_rows = if(!is.null(plot_style_config)) plot_style_config$panel_rows else NULL,
    panel_cols = if(!is.null(plot_style_config)) plot_style_config$panel_cols else NULL,
    panel_var = panel_var
  )

  # Prepare style configuration
  style_config <- .calculate_plot_style_config(
    config = modifyList(
      list(
        panel_rows = panel_layout$rows,
        panel_cols = panel_layout$cols,
        all_font_size = if(!is.null(plot_style_config)) plot_style_config$all_font_size else 1
      ),
      if (!is.null(plot_style_config)) plot_style_config else list()
    ),
    plot_type = "default"
  )

  # Process data with common function
  preprocessed <- .preprocess_data(
    data = data,
    x_axis_from = x_axis_from,
    split_by = split_by,
    panel_var = panel_var,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    filter_var = filter_var,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info,
    stack_value_from = stack_value_from
  )

  # Validate data
  if (is.null(preprocessed)) {
    return(NULL)
  }

  data <- preprocessed$data
  is_macro_mode <- preprocessed$is_macro_mode

  # Generate plots using the common function
  plot_list <- .generate_plots(
    data = data,
    unit_col = unit_col,
    panel_var = panel_var,
    x_axis_from = x_axis_from,
    separate_figure = separate_figure,
    style_config = style_config,
    is_macro_mode = is_macro_mode,
    plot_type = "stack",
    invert_axis = invert_axis,
    variable_col = variable_col,
    split_by = split_by,
    top_impact = top_impact,
    stack_value_from = stack_value_from,
    show_total = show_total,
    unstack_plot = unstack_plot
  )

  # Define export base name
  plot_type_name <- if (unstack_plot) "Unstacked_plots" else "Stacked_plots"
  data_source_info <- ""

  if (variable_col %in% names(data) && length(unique(data[[variable_col]])) == 1) {
    data_source_info <- paste0("_", unique(data[[variable_col]])[1])
  } else if (!is.null(split_by) && split_by %in% names(data) && length(unique(data[[split_by]])) == 1) {
    data_source_info <- paste0("_", unique(data[[split_by]])[1])
  }

  data_source_info <- gsub("[^a-zA-Z0-9_]", "", data_source_info)
  export_name_base <- paste0(plot_type_name, data_source_info, "_", length(plot_list))

  # Handle export with common function
  return(.finalize_plot_export(
    plot_list = plot_list,
    data = data,
    panel_layout = panel_layout,
    output_path = output_path,
    export_picture = export_picture,
    export_as_pdf = export_as_pdf,
    export_config = export_config,
    default_filename = export_name_base
  ))
}


# All Configurations ------------------------------------------------------

#' @title Print Plot and Export Configuration Snippets
#'
#' @description
#' Retrieve full configuration code as a list for applying in the plot styling and export settings.
#'
#' @param plot_style Character. Plot style to use (currently only `"default"` is supported).
#' @param plot_config Logical. If `TRUE`, prints the plot style configuration.
#' @param export_config Logical. If `TRUE`, prints the export configuration.
#'
#' @details Onece printing into the console, users can simply copy and paste
#' the entire list of configurations, rename it (if needed), and use it in your plot functions directly.
#'
#' @return List of configuration snippets to the console.
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' \donttest{
#' # Input Path:
#' input_path <- system.file("extdata/in", package = "GTAPViz")
#' sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
#'
#' # Retrive configurations
#' get_all_config()
#'
#' # Basic usage with data frame
#' p1 <- comparison_plot(
#'   data = sl4.plot.data[["REG"]],
#'   x_axis_from = "Region",
#'   panel_var = "Experiment",
#'   filter_var = c("qgdp", "EV"),
#'   output_path = NULL,
#'   export_picture = FALSE,
#'   export_as_pdf = FALSE,
#'   export_config = my_export_config,
#'   plot_style_config = my_style_config
#' )
#'}
#'
get_all_config <- function(plot_style = "default", plot_config = TRUE,
                           export_config = TRUE) {

  valid_styles <- c("default")
  if (!plot_style %in% valid_styles) {
    stop("Plot style must be one of: 'default'")
  }

  if (isTRUE(plot_config)) {
    # Print the plot style configuration
    .get_plot_style_config(plot_type = plot_style, validate_custom = config)

    # Add some separation between the two sections
    cat("\n\n")
  }

  if (isTRUE(export_config)) {
    # Print the export configuration
    .get_export_config()
  }

  # Return invisibly to avoid printing any values
  return(invisible(NULL))
}

#' @title Print and Visualize Themed Color Palettes
#'
#' @description
#' Prints and visualizes predefined color palettes used in GTAPViz.
#' Use `color_tone = "all"` to return a list of callable palette functions.
#'
#' @param color_tone Character. Name of the color theme to display
#' (e.g., `"gtap"`, `"winter"`, `"fall"`, or `"all"`).
#' @param palette_type Character. Palette type: `"qualitative"` (default), `"sequential"`, or `"diverging"`.
#'
#' @author Pattawee Puangchit
#' @export
#' @examples
#' # Get all palettes as callable functions
#' all_palettes <- get_color_palette("all")
#' all_palettes$winter()
#' all_palettes$gtap()
#'
#' # Visualize specific palettes
#' get_color_palette("fall", "sequential")
#' get_color_palette("academic", "diverging")
#'
get_color_palette <- function(color_tone = NULL,
                              palette_type = "qualitative") {
  # Define available themes
  available_palettes <- c(
    "academic", "purdue", "colorblind", "economic", "trade", "gtap", "gtap2",
    "earth", "vibrant", "bright", "minimal", "energetic", "pastel", "spring",
    "summer", "winter", "fall"
  )

  # When color_tone is NULL, print all available themes first
  if (is.null(color_tone)) {
    cat("\nAvailable color themes:\n")
    cat(paste(" -", available_palettes), sep = "\n")
    cat("\nUse color_tone = \"all\" to get a list of all palette visualizations.\n")
    cat("Example: get_color_palette(\"winter\") or get_color_palette(\"gtap\", \"sequential\")\n\n")

    # Continue with default ("academic") to demonstrate a color palette
    color_tone <- "academic"
    cat("Showing default palette (academic) as an example:\n")
  }

  # If color_tone is "all", return a list of functions (lazy evaluation)
  if (!is.null(color_tone) && color_tone == "all") {
    plot_list <- list()
    for (palette in available_palettes) {
      plot_list[[palette]] <- local({
        pal <- palette  # Store the palette name (to prevent overwriting issues)
        function() { get_color_palette(pal, palette_type) }
      })
    }
    return(plot_list)  # Returns a list of callable functions
  }

  # Option to show all theme names only without visualization
  if (!is.null(color_tone) && color_tone == "list") {
    cat("\nAvailable color themes:\n")
    cat(paste(" -", available_palettes), sep = "\n")
    return(invisible(available_palettes))
  }

  # Generate the color palette using the existing function
  colors <- .create_color_palette(color_tone = color_tone, n_colors = 10, palette_type = palette_type)

  # Validate output
  if (is.null(colors) || length(colors) == 0) {
    stop("Invalid color tone or empty palette. Please choose a valid color_tone from .create_color_palette().")
  }

  # Print colors in console
  cat("\nPalette:", color_tone, "-", palette_type, "\n")
  cat(" Colors: ", paste(colors, collapse = ", "), "\n")

  # Base R visualization
  n_colors <- length(colors)
  bar_x <- seq_len(n_colors)
  bar_y <- rep(1, n_colors)
  graphics::par(mar = c(2, 2, 2, 2))  # Adjust margins for visualization
  graphics::plot(bar_x, bar_y, type = "n", xlab = "", ylab = "", axes = FALSE,
                 main = paste("Palette:", color_tone, "-", palette_type))
  graphics::rect(bar_x - 0.5, 0, bar_x + 0.5, 1, col = colors, border = "black")

  # Add labels
  graphics::text(bar_x, rep(-0.2, n_colors), labels = seq_along(colors), cex = 0.8, col = "black")
}


#' @title Create a Plot Style Configuration
#'
#' @description
#' Creates a configuration list for plot styling that can be used with GTAPViz plotting functions.
#' This function provides auto-completion for style options while maintaining compatibility
#' with direct list specification.
#'
#' @param show_title Logical. Show or hide the plot title. Default: TRUE
#' @param title_face Character. Font face ("bold", "plain", "italic"). Default: "bold"
#' @param title_size Numeric. Font size of title. Default: 20
#' @param title_hjust Numeric. Horizontal alignment (0 = left, 1 = right). Default: 0.5
#' @param add_unit_to_title Logical. Append unit to title if applicable. Default: TRUE
#' @param title_margin Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)
#' @param title_format List or function output. Title formatting options. Can be created with
#'        \code{create_title_format()}. Default: list(type = "standard", text = "", sep = "")
#'
#' @param show_x_axis_title Logical. Show or hide x-axis title. Default: TRUE
#' @param x_axis_title_face Character. Font face for x-axis title. Default: "bold"
#' @param x_axis_title_size Numeric. Font size of x-axis title. Default: 16
#' @param x_axis_title_margin Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)
#' @param show_x_axis_labels Logical. Show or hide x-axis labels. Default: TRUE
#' @param x_axis_text_face Character. Font face for x-axis labels. Default: "plain"
#' @param x_axis_text_size Numeric. Font size of x-axis labels. Default: 14
#' @param x_axis_text_angle Numeric. Angle of x-axis labels. Default: 0
#' @param x_axis_text_hjust Numeric. Horizontal justification of x-axis labels. Default: 0
#' @param x_axis_description Character. Optional description for the x-axis. Default: ""
#'
#' @param show_y_axis_title Logical. Show or hide y-axis title. Default: TRUE
#' @param y_axis_title_face Character. Font face for y-axis title. Default: "bold"
#' @param y_axis_title_size Numeric. Font size of y-axis title. Default: 16
#' @param y_axis_title_margin Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)
#' @param show_y_axis_labels Logical. Show or hide y-axis labels. Default: TRUE
#' @param y_axis_text_face Character. Font face for y-axis labels. Default: "plain"
#' @param y_axis_text_size Numeric. Font size of y-axis labels. Default: 14
#' @param y_axis_text_angle Numeric. Angle of y-axis labels. Default: 0
#' @param y_axis_text_hjust Numeric. Horizontal justification of y-axis labels. Default: 0
#' @param y_axis_description Character. Optional description for the y-axis. Default: ""
#' @param show_axis_titles_on_all_facets Logical. Show axis titles on all facets. Default: TRUE
#'
#' @param show_value_labels Logical. Show or hide value labels. Default: TRUE
#' @param value_label_face Character. Font face for value labels. Default: "plain"
#' @param value_label_size Numeric. Font size of value labels. Default: 5
#' @param value_label_position Character. Position of value labels ("above", "outside", "top"). Default: "above"
#' @param value_label_decimal_places Numeric. Number of decimal places in value labels. Default: 2
#'
#' @param show_legend Logical. Show or hide legend. Default: FALSE
#' @param show_legend_title Logical. Show or hide legend title. Default: FALSE
#' @param legend_position Character. Legend position ("none", "bottom", "right"). Default: "bottom"
#' @param legend_title_face Character. Font face for legend title. Default: "bold"
#' @param legend_text_face Character. Font face for legend text. Default: "plain"
#' @param legend_text_size Numeric. Font size of legend text. Default: 14
#'
#' @param strip_face Character. Font face for panel strip. Default: "bold"
#' @param strip_text_size Numeric. Font size for panel strip. Default: 16
#' @param strip_background Character. Background color of strip. Default: "lightgrey"
#' @param strip_text_margin Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)
#'
#' @param panel_spacing Numeric. Spacing between panels. Default: 2
#' @param panel_rows Numeric or NULL. Number of rows in panel layout. Default: NULL
#' @param panel_cols Numeric or NULL. Number of columns in panel layout. Default: NULL
#' @param theme ggplot2 theme or NULL. Custom ggplot theme. Default: NULL
#'
#' @param color_tone Character or NULL. Base color theme. Default: NULL
#' @param color_palette_type Character. Type of color palette ('qualitative', 'sequential', 'diverging'). Default: "qualitative"
#' @param positive_color Character. Color for positive values. Default: "#2E8B57"
#' @param negative_color Character. Color for negative values. Default: "#CD5C5C"
#' @param background_color Character. Background color of plot. Default: "white"
#' @param grid_color Character. Color of grid lines. Default: "grey90"
#' @param show_grid_major_x Logical. Show major grid lines on x-axis. Default: FALSE
#' @param show_grid_major_y Logical. Show major grid lines on y-axis. Default: FALSE
#' @param show_grid_minor_x Logical. Show minor grid lines on x-axis. Default: FALSE
#' @param show_grid_minor_y Logical. Show minor grid lines on y-axis. Default: FALSE
#'
#' @param show_zero_line Logical. Show or hide zero line. Default: TRUE
#' @param zero_line_type Character. Line type ("solid", "dashed", "dotted"). Default: "dashed"
#' @param zero_line_color Character. Color of zero line. Default: "black"
#' @param zero_line_size Numeric. Line thickness of zero line. Default: 0.5
#' @param zero_line_position Numeric. Position of the zero line. Default: 0
#'
#' @param bar_width Numeric. Width of bars. Default: 0.9
#' @param bar_spacing Numeric. Spacing between groups of bars. Default: 0.9
#'
#' @param scale_limit Numeric vector of length 2 or NULL. Manual limits for value axis. Default: NULL
#' @param scale_increment Numeric or NULL. Step size for axis tick marks. Default: NULL
#'
#' @param expansion_y_mult Numeric vector. Y-axis expansion. Default: c(0.05, 0.1)
#' @param expansion_x_mult Numeric vector. X-axis expansion. Default: c(0.05, 0.05)
#'
#' @param all_font_size Numeric. Master control for all font sizes. Default: 1
#'
#' @param sort_data_by_value Logical. Whether to sort data by value. Default: FALSE
#'
#' @param plot.margin Numeric vector c(top, right, bottom, left). Margins around the entire plot. Default: c(10, 25, 10, 10)
#'
#' @return A list containing all style configuration parameters
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Create customized style with title formatting
#' custom_style <- create_plot_style(
#'   color_tone = "gtap",
#'   title_size = 24,
#'   title_format = create_title_format(
#'     type = "prefix",
#'     text = "Impact on",
#'     sep = " "
#'   ),
#'   bar_width = 0.5,
#'   x_axis_text_angle = 45
#' )
#'
create_plot_style <- function(
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
) {
  # Process title_format if it was created using create_title_format()
  if (is.function(title_format)) {
    title_format <- title_format()
  }

  # Collect all arguments into a list
  style_config <- as.list(environment())

  # Return the style configuration list
  return(style_config)
}

#' Create an Export Configuration
#'
#' @description
#' Creates a configuration list for controlling plot export settings.
#' This function provides auto-completion for export options.
#'
#' @param file_name Character. Base name for exported files. Default: "gtap_plots".
#' @param width Numeric. Width of output in inches. Default: NULL (auto-calculated).
#' @param height Numeric. Height of output in inches. Default: NULL (auto-calculated).
#' @param dpi Numeric. Resolution for PNG export. Default: 300.
#' @param bg Character. Background color. Default: "white".
#' @param limitsize Logical. Whether to limit size. Default: FALSE.
#'
#' @return A list with export configuration parameters.
#'
#' @examples
#' # Default export configuration
#' default_export <- create_export_config()
#'
#' # Custom export configuration
#' custom_export <- create_export_config(
#'   file_name = "regional_impacts",
#'   width = 12,
#'   height = 8,
#'   dpi = 600
#' )
#' @author Pattawee Puangchit
#' @export
create_export_config <- function(
    file_name = "gtap_plots",
    width = NULL,
    height = NULL,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
) {
  # Return the export configuration
  list(
    file_name = file_name,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg,
    limitsize = limitsize
  )
}

#' @title Create a Title Format Configuration
#'
#' @description
#' Creates a configuration list for controlling plot title formatting.
#' Supports auto-completion for common title format types.
#' @md
#' @param type Character. Title format type:
#' - `"standard"`: Default (variable + description + unit)
#' - `"prefix"`: Adds text before the automatic title
#' - `"suffix"`: Adds text after the automatic title
#' - `"full"`: Uses only the specified text as the title
#' - `"dynamic"`: Builds a title using column values
#'
#' @param text Character. Text content used for `prefix`, `suffix`, `full`, or a template for `dynamic`.
#'
#' @return A list with title format configuration parameters.
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Standard auto-generated title
#' standard_title <- create_title_format()
#'
#' # Prefix title
#' prefix_title <- create_title_format(
#'   type = "prefix",
#'   text = "Impact on",
#'   sep = " "
#' )
#'
#' # Dynamic title using column values
#' dynamic_title <- create_title_format(
#'   type = "dynamic",
#'   text = "Impact on {Variable} in {Region}"
#' )
#'
create_title_format <- function(
    type = "standard",
    text = "",
    sep = ""
) {
  # Validate type parameter
  valid_types <- c("standard", "prefix", "suffix", "full", "dynamic")
  if (!type %in% valid_types) {
    warning(sprintf(
      "Invalid title format type '%s'. Valid options are: '%s'. Using 'standard' instead.",
      type, paste(valid_types, collapse = "', '")
    ))
    type <- "standard"
  }

  # Return the title format configuration
  list(
    type = type,
    text = text,
    sep = sep
  )
}
