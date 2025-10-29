# MAP HEATMAP FUNCTION ========================================================

#' @title Create Label Box Variables Configuration
#' @description
#' Defines which variables to display in label boxes next to country flags.
#'
#' @param variable Character vector. Variable codes to display
#' @param label Character vector or NULL. Custom labels. If NULL, uses variable codes
#' @param symbol Character vector or NULL. Symbols to prefix each value. Auto-assigned if NULL
#' @param decimal_places Numeric vector. Decimal places for each variable. Default: 1
#'
#' @return Data frame with label box configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' label_vars <- create_label_box_vars(
#'   variable = c("EV", "qgdp", "qpriv"),
#'   label = c("Welfare", "GDP", "Consumption"),
#'   decimal_places = c(2, 1, 1)
#' )
create_label_box_vars <- function(variable,
                                  label = NULL,
                                  symbol = NULL,
                                  decimal_places = 1) {

  if (is.null(label)) label <- variable
  if (length(label) != length(variable)) label <- rep_len(label, length(variable))

  if (is.null(symbol)) {
    symbols_list <- c("\u25CF", "\u25A0", "\u25B2", "\u25C6", "\u2605", "\u25BA", "\u2666", "\u2663", "\u2665", "\u2660")
    symbol <- symbols_list[1:length(variable)]
  }

  if (length(decimal_places) == 1) {
    decimal_places <- rep(decimal_places, length(variable))
  }

  data.frame(
    variable = variable,
    label = label,
    symbol = symbol,
    decimal_places = decimal_places,
    stringsAsFactors = FALSE
  )
}


#' @title Create Map Style Configuration
#' @description
#' Creates styling options for map appearance, following the same pattern as create_plot_style.
#'
#' @param show_title Logical. Show plot title. Default: TRUE
#' @param title_face Character. Title font face. Default: "bold"
#' @param title_size Numeric. Title size. Default: 20
#' @param title_hjust Numeric. Title horizontal justification. Default: 0.5
#' @param add_unit_to_title Logical. Append unit to title. Default: TRUE
#' @param title_margin Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)
#' @param title_format List. Title formatting. See \code{\link{create_title_format}}
#'
#' @param show_caption Logical. Show caption. Default: TRUE
#' @param caption_text Character or NULL. Custom caption text. Default: NULL
#' @param caption_size Numeric. Caption text size. Default: 13
#' @param caption_hjust Numeric. Caption horizontal justification. Default: 0.5
#' @param caption_margin Numeric vector. Caption margins. Default: c(12, 0, 0, 0)
#'
#' @param legend_position Character. Legend position ("bottom", "top", "left", "right", "none"). Default: "bottom"
#' @param legend_direction Character. Legend direction. Default: "horizontal"
#' @param legend_title Character or NULL. Custom legend title. Default: NULL
#' @param show_legend_title Logical. Show legend title. Default: TRUE
#' @param legend_title_face Character. Legend title font face. Default: "bold"
#' @param legend_title_size Numeric. Legend title size. Default: 15
#' @param legend_text_face Character. Legend text font face. Default: "plain"
#' @param legend_text_size Numeric. Legend text size. Default: 13
#' @param legend_barwidth Numeric. Legend bar width. Default: 16
#' @param legend_barheight Numeric. Legend bar height. Default: 0.7
#'
#' @param color_low Character. Low value color. Default: "#D9B17D"
#' @param color_high Character. High value color. Default: "#1C3E5D"
#' @param color_mid Character or NULL. Mid value color for diverging scales. Default: NULL
#' @param color_palette Character or NULL. Named palette: "viridis", "magma", etc. Default: NULL
#' @param color_na Character. Color for NA values. Default: "grey90"
#'
#' @param border_color Character. Country border color. Default: "white"
#' @param border_size Numeric. Border thickness. Default: 0.4
#'
#' @param background_color Character. Background color of plot. Default: "white"
#'
#' @param country_name_bold Logical. Bold country names. Default: TRUE
#' @param country_name_face Character. Country name font face. Default: "bold"
#' @param country_name_size Numeric. Country name text size. Default: 3.3
#'
#' @param show_stat_labels Logical. Show statistics labels in boxes. Default: TRUE
#' @param stat_label_face Character. Statistics label font face. Default: "plain"
#' @param stat_label_size Numeric. Statistics label text size. Default: 4
#' @param stat_label_bold Logical. Bold statistics labels. Default: FALSE
#' @param label_spacing Numeric. Line height spacing for multi-line labels. Default: 1.05
#'
#' @param line_color Character. Connecting line color. Default: "grey40"
#' @param line_size Numeric. Line thickness. Default: 0.25
#' @param line_type Character. Line type ("solid", "dashed", "dotted"). Default: "solid"
#'
#' @param flag_size Numeric. Flag image size. Default: 0.032
#' @param flag_aspect Numeric. Flag aspect ratio. Default: 1.3
#'
#' @param plot.margin Numeric vector c(top, right, bottom, left). Margins around plot in mm. Default: c(8, 8, 12, 8)
#'
#' @param all_font_size Numeric. Master control for all font sizes. Default: 1
#'
#' @return List with style configuration
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' asean_style <- create_map_style(
#'   color_low = "#D9B17D",
#'   color_high = "#1C3E5D",
#'   country_name_size = 4,
#'   stat_label_size = 3.5,
#'   plot.margin = c(15, 50, 15, 50)
#' )
create_map_style <- function(
    # Title settings
  show_title = TRUE,
  title_face = "bold",
  title_size = 20,
  title_hjust = 0.5,
  add_unit_to_title = TRUE,
  title_margin = c(10, 0, 10, 0),
  title_format = list(type = "standard", text = "", sep = ""),

  # Caption settings
  show_caption = TRUE,
  caption_text = NULL,
  caption_size = 13,
  caption_hjust = 0.5,
  caption_margin = c(12, 0, 0, 0),

  # Legend settings
  legend_position = "bottom",
  legend_direction = "horizontal",
  legend_title = NULL,
  show_legend_title = TRUE,
  legend_title_face = "bold",
  legend_title_size = 15,
  legend_text_face = "plain",
  legend_text_size = 13,
  legend_barwidth = 16,
  legend_barheight = 0.7,

  # Color settings
  color_low = "#D9B17D",
  color_high = "#1C3E5D",
  color_mid = NULL,
  color_palette = NULL,
  color_na = "grey90",

  # Border settings
  border_color = "white",
  border_size = 0.4,

  # Background
  background_color = "white",

  # Country name settings
  country_name_bold = TRUE,
  country_name_face = "bold",
  country_name_size = 3.3,

  # Statistics label settings
  show_stat_labels = TRUE,
  stat_label_face = "plain",
  stat_label_size = 4,
  stat_label_bold = FALSE,
  label_spacing = 1.05,

  # Line settings
  line_color = "grey40",
  line_size = 0.25,
  line_type = "solid",

  # Flag settings
  flag_size = 0.032,
  flag_aspect = 1.3,

  # Plot margin (in mm)
  plot.margin = c(8, 8, 12, 8),

  # Font size control
  all_font_size = 1
) {

  # Process title_format if it was created using create_title_format()
  if (is.function(title_format)) {
    title_format <- title_format()
  }

  # Collect all arguments into a list
  style_config <- as.list(environment())

  return(style_config)
}


# UNIT CONVERSION HELPERS -----------------------------------------------------

#' @keywords internal
#' @noRd
.convert_unit_for_display <- function(unit) {
  if (is.null(unit) || is.na(unit) || unit == "") return("")

  unit_lower <- tolower(trimws(unit))

  conversions <- c(
    "percent" = "%",
    "percentage" = "%",
    "million usd" = "M USD",
    "million us$" = "M USD",
    "billion usd" = "B USD",
    "billion us$" = "B USD",
    "thousand usd" = "K USD",
    "thousand us$" = "K USD",
    "usd million" = "M USD",
    "usd billion" = "B USD",
    "us$ million" = "M USD",
    "us$ billion" = "B USD"
  )

  if (unit_lower %in% names(conversions)) {
    return(conversions[[unit_lower]])
  }

  return(unit)
}


#' @keywords internal
#' @noRd
.format_value_with_unit <- function(value, unit, decimal_places = 1) {
  if (is.na(value) || is.null(value)) return("NA")

  formatted_val <- sprintf(paste0("%.", decimal_places, "f"), value)
  converted_unit <- .convert_unit_for_display(unit)

  if (converted_unit == "") {
    return(formatted_val)
  } else {
    return(paste(formatted_val, converted_unit))
  }
}


#' @title Create Country/Region Heatmaps
#' @md
#' @description
#' Creates geographical heatmaps for GTAP data with optional label boxes.
#'
#' **Input Data**
#' @param data Data frame or list. GTAP data with country codes and values
#' @param filter_var NULL, vector, data frame, or named list specifying filtering conditions
#' @param iso_col Character. Column with ISO3 country codes. Default: `"REG"`
#' @param value_col Character. Column with numeric values. Default: `"Value"`
#' @param split_by Character or vector. Column(s) to create separate maps. Default: `NULL`
#' @param variable_col Character. Column with variable codes. Default: `"Variable"`
#' @param unit_col Character. Column with units. Default: `"Unit"`
#' @param desc_col Character. Column with descriptions. Default: `"Description"`
#'
#' **Map Region Settings**
#' @param map_region Character or vector. Region to display. Default: `"world"`
#'
#' **Supported regions:**
#' - **Continents:** `"world"` (default), `"asia"`, `"europe"`, `"africa"`, `"americas"`, `"north_america"`, `"south_america"`, `"oceania"`
#' - **Sub-regions:** `"asean"` (or `"southeast_asia"`, `"seasia"`), `"east_asia"`, `"south_asia"`, `"central_asia"`
#' - **Middle East/Africa:** `"mena"` (or `"middle_east"`), `"gcc"` (Gulf Cooperation Council), `"north_africa"`, `"sub_saharan_africa"`
#' - **Europe:** `"eu"` (or `"european_union"`), `"western_europe"`, `"eastern_europe"`
#' - **Americas:** `"latin_america"`, `"caribbean"`, `"nafta"` (or `"usmca"`), `"mercosur"`
#' - **Pacific:** `"pacific"`
#' - **Custom:** Vector of ISO3 codes (e.g., `c("USA", "CAN", "MEX")`)
#'
#' @param xlim Numeric vector. Longitude limits `c(min, max)`. Default: `NULL` (auto from region)
#' @param ylim Numeric vector. Latitude limits `c(min, max)`. Default: `NULL`
#'
#' **Label Box Settings**
#' @param show_flags Logical. Display country flags. Default: `FALSE`
#' @param label_box_vars Data frame or NULL. Variables to show in boxes. Create with
#' \code{\link{create_label_box_vars}}. If `NULL`, no label boxes shown. Default: `NULL`
#' @param map_value_var Character or NULL. Variable for map coloring. If `NULL`, uses first
#' variable or all data. Default: `NULL`
#'
#' **Aggregation**
#' @param aggregate_by Character or vector. Columns to aggregate by. Default: `NULL`
#' @param aggregate_fun Character. Aggregation function: `"mean"`, `"sum"`, `"median"`. Default: `"mean"`
#'
#' **Variable Display**
#' @param var_name_by_description Logical. Use descriptions in titles. Default: `FALSE`
#' @param add_var_info Logical. Append variable codes. Default: `FALSE`
#' @param common_scale Logical. Use unified color scale across split maps. When `TRUE` and
#' `split_by` is used, all maps share the same color scale based on global min/max values,
#' making colors comparable across maps. When `FALSE`, each map uses its own min/max for
#' color scaling. Default: `FALSE`
#'
#' **Export Settings**
#' @param output_path Character. Directory to save maps. Default: `NULL`
#' @param export_picture Logical. Export as PNG. Default: `TRUE`
#' @param export_as_pdf Logical or `"merged"`. Export as PDF. Default: `FALSE`
#' @param export_config List. Export options. See \code{\link{create_export_config}}
#'
#' **Styling**
#' @param map_style_config List. Map appearance. See \code{\link{create_map_style}}
#' @param title_format List or NULL. Title formatting. See \code{\link{create_title_format}}
#' @param show_unmatched_iso Logical. Warn about unmatched ISO codes. Default: `TRUE`
#'
#' @return List with `plots` (ggplot objects) and `unmatched_iso` (character vector)
#' @author Pattawee Puangchit
#'
#' @importFrom sf st_as_sf
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_gradient scale_fill_gradient2
#' scale_fill_gradientn coord_sf labs theme_void theme element_text element_rect
#' margin guides guide_colorbar geom_segment geom_text
#' @importFrom dplyr filter group_by summarise across all_of left_join group_split mutate
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggimage geom_image
#' @importFrom countrycode countrycode
#' @importFrom rlang sym
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple map without labels
#' map_heatmap(
#'   data = gtap_data,
#'   iso_col = "Region",
#'   value_col = "Value",
#'   split_by = "Case"
#' )
#'
#' # Map with label boxes showing multiple variables
#' label_vars <- create_label_box_vars(
#'   variable = c("EV", "qgdp", "qpriv"),
#'   label = c("Welfare", "GDP", "Consumption")
#' )
#'
#' map_heatmap(
#'   data = gtap_data,
#'   iso_col = "Region",
#'   map_value_var = "qgdp",
#'   label_box_vars = label_vars,
#'   show_flags = TRUE,
#'   split_by = "Case"
#' )
#'
#' # Use unified color scale across all maps for comparison
#' map_heatmap(
#'   data = gtap_data,
#'   iso_col = "Region",
#'   value_col = "Value",
#'   split_by = "Case",
#'   common_scale = TRUE  # All maps use same color scale
#' )
#' }
map_heatmap <- function(data,
                        filter_var = NULL,
                        iso_col = "REG",
                        value_col = "Value",
                        split_by = NULL,
                        variable_col = "Variable",
                        unit_col = "Unit",
                        desc_col = "Description",
                        map_region = "world",
                        xlim = NULL,
                        ylim = NULL,
                        show_flags = FALSE,
                        label_box_vars = NULL,
                        map_value_var = NULL,
                        aggregate_by = NULL,
                        aggregate_fun = "mean",
                        var_name_by_description = FALSE,
                        add_var_info = FALSE,
                        common_scale = FALSE,
                        output_path = NULL,
                        export_picture = TRUE,
                        export_as_pdf = FALSE,
                        export_config = NULL,
                        map_style_config = NULL,
                        title_format = NULL,
                        show_unmatched_iso = TRUE) {

  if (!is.data.frame(data) && !is.list(data)) {
    stop("'data' must be a data frame or a list of data frames.")
  }

  if (is.data.frame(data)) data <- list(data)

  if (!iso_col %in% colnames(data[[1]])) {
    stop(paste0("ISO column '", iso_col, "' not found in data."))
  }

  if (!all(value_col %in% colnames(data[[1]]))) {
    stop(paste0("Value column '", paste(value_col, collapse = ", "), "' not found in data."))
  }

  style_config <- if (!is.null(map_style_config)) {
    map_style_config
  } else {
    create_map_style()
  }

  if (!is.null(title_format)) {
    style_config$title_format <- title_format
  }

  export_config <- if (!is.null(export_config)) {
    export_config
  } else {
    create_export_config()
  }

  map_list <- .create_map_heatmaps(
    data = data,
    filter_var = filter_var,
    iso_col = iso_col,
    value_col = value_col,
    split_by = split_by,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    map_region = map_region,
    xlim = xlim,
    ylim = ylim,
    show_flags = show_flags,
    label_box_vars = label_box_vars,
    map_value_var = map_value_var,
    aggregate_by = aggregate_by,
    aggregate_fun = aggregate_fun,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info,
    common_scale = common_scale,
    style_config = style_config,
    show_unmatched_iso = show_unmatched_iso
  )

  if (!is.null(output_path) && (export_picture || export_as_pdf)) {
    if (is.null(export_config$width)) export_config$width <- 13
    if (is.null(export_config$height)) export_config$height <- 9

    .export_plot_output(
      plots = map_list$plots,
      export_picture = export_picture,
      export_as_pdf = export_as_pdf,
      output_path = output_path,
      export_config = export_config,
      data = NULL,
      panel_layout = list(rows = 1, cols = 1),
      default_filename = "map_heatmap"
    )
  }

  return(invisible(list(
    plots = map_list$plots,
    unmatched_iso = map_list$unmatched_iso
  )))
}


# INTERNAL FUNCTIONS ===========================================================

#' @keywords internal
#' @noRd
.create_map_heatmaps <- function(data, filter_var, iso_col, value_col, split_by,
                                 variable_col, unit_col, desc_col, map_region,
                                 xlim, ylim, show_flags, label_box_vars,
                                 map_value_var, aggregate_by,
                                 aggregate_fun, var_name_by_description,
                                 add_var_info, common_scale, style_config, show_unmatched_iso) {

  plot_list <- list()
  all_unmatched <- character()

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- .get_map_region(world, map_region)

  country_positions <- if (map_region == "asean" ||
                           all(map_region %in% c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM"))) {
    .get_asean_positions()
  } else {
    NULL
  }

  # Calculate global scale limits if common_scale is TRUE
  global_scale_limits <- NULL
  if (common_scale && !is.null(split_by)) {
    all_values <- numeric()
    for (i in seq_along(data)) {
      df <- data[[i]]
      if (!is.null(filter_var)) {
        df <- .apply_filter(df, filter_var)
      }
      if (!is.null(aggregate_by)) {
        df <- .aggregate_map_data(df, aggregate_by, value_col, aggregate_fun)
      }
      # Get values for the map_value_var if specified
      if (!is.null(map_value_var)) {
        df <- df %>% dplyr::filter(!!rlang::sym(variable_col) == map_value_var)
      }
      all_values <- c(all_values, df[[value_col]])
    }
    # Remove NA values and calculate limits
    all_values <- all_values[!is.na(all_values)]
    if (length(all_values) > 0) {
      global_scale_limits <- c(min(all_values, na.rm = TRUE), max(all_values, na.rm = TRUE))
    }
  }

  for (i in seq_along(data)) {
    df <- data[[i]]

    if (!is.null(filter_var)) {
      df <- .apply_filter(df, filter_var)
    }

    if (!is.null(aggregate_by)) {
      df <- .aggregate_map_data(df, aggregate_by, value_col, aggregate_fun)
    }

    data_isos <- unique(df[[iso_col]])
    map_isos <- unique(map_data$iso_a3)
    unmatched <- setdiff(data_isos, map_isos)

    if (length(unmatched) > 0 && show_unmatched_iso) {
      warning(paste0("ISO codes not found: ", paste(unmatched, collapse = ", ")))
      all_unmatched <- c(all_unmatched, unmatched)
    }

    if (is.null(split_by)) {
      plot_obj <- .create_single_map(
        data = df,
        map_data = map_data,
        iso_col = iso_col,
        value_col = value_col,
        variable_col = variable_col,
        unit_col = unit_col,
        desc_col = desc_col,
        xlim = xlim,
        ylim = ylim,
        show_flags = show_flags,
        label_box_vars = label_box_vars,
        map_value_var = map_value_var,
        country_positions = country_positions,
        var_name_by_description = var_name_by_description,
        add_var_info = add_var_info,
        style_config = style_config,
        map_region = map_region,
        global_scale_limits = global_scale_limits
      )
      plot_list[["map_1"]] <- plot_obj
    } else {
      split_groups <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(split_by))) %>%
        dplyr::group_split()

      for (group in split_groups) {
        plot_name <- .generate_map_name(group, split_by, map_value_var)

        plot_obj <- .create_single_map(
          data = group,
          map_data = map_data,
          iso_col = iso_col,
          value_col = value_col,
          variable_col = variable_col,
          unit_col = unit_col,
          desc_col = desc_col,
          xlim = xlim,
          ylim = ylim,
          show_flags = show_flags,
          label_box_vars = label_box_vars,
          map_value_var = map_value_var,
          country_positions = country_positions,
          var_name_by_description = var_name_by_description,
          add_var_info = add_var_info,
          style_config = style_config,
          map_region = map_region,
          global_scale_limits = global_scale_limits
        )
        plot_list[[plot_name]] <- plot_obj
      }
    }
  }

  return(list(
    plots = plot_list,
    unmatched_iso = unique(all_unmatched)
  ))
}


#' @keywords internal
#' @noRd
.create_single_map <- function(data, map_data, iso_col, value_col, variable_col,
                               unit_col, desc_col, xlim, ylim, show_flags,
                               label_box_vars, map_value_var, country_positions,
                               var_name_by_description, add_var_info,
                               style_config, map_region, global_scale_limits = NULL) {

  if (!is.null(map_value_var)) {
    map_color_data <- data %>% dplyr::filter(!!rlang::sym(variable_col) == map_value_var)
  } else {
    map_color_data <- data
  }

  plot_data <- map_data %>%
    dplyr::left_join(map_color_data, by = c("iso_a3" = iso_col))

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = !!rlang::sym(value_col)),
      color = style_config$border_color,
      linewidth = style_config$border_size
    )

  legend_var_name <- if (!is.null(map_value_var)) {
    map_value_var
  } else if (variable_col %in% colnames(map_color_data) && nrow(map_color_data) > 0) {
    unique(map_color_data[[variable_col]])[1]
  } else {
    NULL
  }

  p <- .add_map_color_scale(p, style_config, value_col, legend_var_name, global_scale_limits)

  if ((show_flags || !is.null(label_box_vars)) && !is.null(country_positions)) {
    label_data <- .prepare_label_data(
      data = data,
      iso_col = iso_col,
      value_col = value_col,
      variable_col = variable_col,
      unit_col = unit_col,
      label_box_vars = label_box_vars,
      country_positions = country_positions
    )

    if (!is.null(label_data) && nrow(label_data) > 0) {
      p <- .add_labels_flags(p, label_data, show_flags, style_config)
    }
  }

  if (!is.null(xlim) && !is.null(ylim)) {
    # If labels are present, expand limits to accommodate them
    if ((show_flags || !is.null(label_box_vars)) && !is.null(country_positions)) {
      # Calculate how much to expand based on label positions
      label_x_range <- range(country_positions$flag_x, na.rm = TRUE)
      label_y_range <- range(country_positions$flag_y, na.rm = TRUE)

      # Expand xlim and ylim to include label positions
      expanded_xlim <- c(
        min(xlim[1], label_x_range[1] - 5),
        max(xlim[2], label_x_range[2] + 5)
      )
      expanded_ylim <- c(
        min(ylim[1], label_y_range[1] - 3),
        max(ylim[2], label_y_range[2] + 3)
      )

      p <- p + ggplot2::coord_sf(xlim = expanded_xlim, ylim = expanded_ylim, expand = FALSE, clip = "off")
    } else {
      p <- p + ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  } else {
    limits <- .get_region_limits(map_region)
    if (!is.null(limits)) {
      # Same expansion logic for auto limits
      if ((show_flags || !is.null(label_box_vars)) && !is.null(country_positions)) {
        label_x_range <- range(country_positions$flag_x, na.rm = TRUE)
        label_y_range <- range(country_positions$flag_y, na.rm = TRUE)

        expanded_xlim <- c(
          min(limits$xlim[1], label_x_range[1] - 5),
          max(limits$xlim[2], label_x_range[2] + 5)
        )
        expanded_ylim <- c(
          min(limits$ylim[1], label_y_range[1] - 3),
          max(limits$ylim[2], label_y_range[2] + 3)
        )

        p <- p + ggplot2::coord_sf(xlim = expanded_xlim, ylim = expanded_ylim, expand = FALSE, clip = "off")
      } else {
        p <- p + ggplot2::coord_sf(xlim = limits$xlim, ylim = limits$ylim, expand = FALSE)
      }
    } else {
      if ((show_flags || !is.null(label_box_vars)) && !is.null(country_positions)) {
        p <- p + ggplot2::coord_sf(expand = FALSE, clip = "off")
      } else {
        p <- p + ggplot2::coord_sf(expand = FALSE)
      }
    }
  }

  title_text <- .generate_map_title(
    data = map_color_data,
    variable_col = variable_col,
    unit_col = unit_col,
    desc_col = desc_col,
    var_name_by_description = var_name_by_description,
    add_var_info = add_var_info,
    style_config = style_config,
    value_col = value_col
  )

  caption_text <- if (!is.null(label_box_vars) && style_config$show_caption) {
    .generate_label_box_caption(label_box_vars)
  } else if (style_config$show_caption && !is.null(style_config$caption_text)) {
    style_config$caption_text
  } else {
    NULL
  }

  p <- p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = if (style_config$show_title) title_text else NULL,
      caption = if (style_config$show_caption) caption_text else NULL
    ) +
    ggplot2::theme_void(base_size = 13 * style_config$all_font_size) +
    ggplot2::theme(
      legend.position = style_config$legend_position,
      legend.title = ggplot2::element_text(
        face = style_config$legend_title_face,
        size = style_config$legend_title_size * style_config$all_font_size
      ),
      legend.text = ggplot2::element_text(
        face = style_config$legend_text_face,
        size = style_config$legend_text_size * style_config$all_font_size
      ),
      plot.title = ggplot2::element_text(
        size = style_config$title_size * style_config$all_font_size,
        face = style_config$title_face,
        hjust = style_config$title_hjust,
        margin = ggplot2::margin(
          t = style_config$title_margin[1],
          r = style_config$title_margin[2],
          b = style_config$title_margin[3],
          l = style_config$title_margin[4]
        )
      ),
      plot.caption = ggplot2::element_text(
        hjust = style_config$caption_hjust,
        size = style_config$caption_size * style_config$all_font_size,
        margin = ggplot2::margin(
          t = style_config$caption_margin[1],
          r = style_config$caption_margin[2],
          b = style_config$caption_margin[3],
          l = style_config$caption_margin[4]
        )
      ),
      plot.background = ggplot2::element_rect(
        fill = style_config$background_color,
        color = NA
      ),
      plot.margin = ggplot2::margin(
        t = style_config$plot.margin[1],
        r = style_config$plot.margin[2],
        b = style_config$plot.margin[3],
        l = style_config$plot.margin[4]
      )
    )

  if (style_config$legend_position != "none") {
    p <- p + ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = style_config$legend_barwidth,
        barheight = style_config$legend_barheight,
        title.position = "top",
        title.hjust = 0.5
      )
    )
  }

  return(p)
}


#' @keywords internal
#' @noRd
.prepare_label_data <- function(data, iso_col, value_col, variable_col,
                                unit_col, label_box_vars, country_positions) {

  label_data <- country_positions

  countries <- unique(data[[iso_col]])
  label_data <- label_data %>% dplyr::filter(iso_a3 %in% countries)

  label_data$iso_a2 <- countrycode::countrycode(label_data$iso_a3, "iso3c", "iso2c")
  label_data$flag <- paste0("https://flagcdn.com/w80/", tolower(label_data$iso_a2), ".png")

  # Left side labels: right-aligned (flows leftward from flag)
  # Right side labels: left-aligned (flows rightward from flag)
  label_data$text_x <- ifelse(label_data$flag_x < 100,
                              label_data$flag_x - 3.5,  # Left side: text to left of flag
                              label_data$flag_x + 3)     # Right side: text to right of flag
  label_data$text_hjust <- ifelse(label_data$flag_x < 100, 1, 0)  # Left: right-align, Right: left-align
  label_data$text_y <- label_data$flag_y - 0.3
  label_data$name_x <- label_data$flag_x
  label_data$name_y <- label_data$flag_y - 1.5

  if (!is.null(label_box_vars)) {
    label_data$text_label <- sapply(label_data$iso_a3, function(country_iso) {
      country_data <- data %>% dplyr::filter(!!rlang::sym(iso_col) == country_iso)

      label_lines <- character()
      for (i in 1:nrow(label_box_vars)) {
        var_code <- label_box_vars$variable[i]
        var_label <- label_box_vars$label[i]
        var_symbol <- label_box_vars$symbol[i]
        var_decimals <- label_box_vars$decimal_places[i]

        var_data <- country_data %>%
          dplyr::filter(!!rlang::sym(variable_col) == var_code)

        if (nrow(var_data) > 0) {
          val <- var_data[[value_col]][1]
          unit <- if (unit_col %in% colnames(var_data)) var_data[[unit_col]][1] else ""

          formatted_value <- .format_value_with_unit(val, unit, var_decimals)
          line <- paste0(var_symbol, " ", var_label, ": ", formatted_value)
          label_lines <- c(label_lines, line)
        }
      }

      paste(label_lines, collapse = "\n")
    })
  } else {
    label_data$text_label <- ""
  }

  return(label_data)
}


#' @keywords internal
#' @noRd
.add_labels_flags <- function(p, label_data, show_flags, style_config) {

  p <- p + ggplot2::geom_segment(
    data = label_data,
    ggplot2::aes(
      x = point_x, y = point_y,
      xend = ifelse(flag_x < 100, flag_x + 0.4, flag_x - 0.4),
      yend = flag_y
    ),
    color = style_config$line_color,
    linewidth = style_config$line_size,
    linetype = style_config$line_type
  )

  if (show_flags) {
    p <- p + ggimage::geom_image(
      data = label_data,
      ggplot2::aes(x = flag_x, y = flag_y, image = flag),
      size = style_config$flag_size,
      asp = style_config$flag_aspect
    )
  }

  p <- p + ggplot2::geom_text(
    data = label_data,
    ggplot2::aes(x = name_x, y = name_y, label = name),
    size = style_config$country_name_size,
    fontface = style_config$country_name_face,
    hjust = 0.5
  )

  if (style_config$show_stat_labels) {
    p <- p + ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = text_x, y = text_y, label = text_label, hjust = text_hjust),
      size = style_config$stat_label_size,
      fontface = style_config$stat_label_face,
      vjust = 0.5,
      lineheight = style_config$label_spacing
    )
  }

  return(p)
}


#' @keywords internal
#' @noRd
.get_asean_positions <- function() {
  data.frame(
    iso_a3 = c("MMR", "THA", "KHM", "MYS", "SGP", "LAO", "VNM", "PHL", "BRN", "IDN"),
    name = c("MYANMAR", "THAILAND", "CAMBODIA", "MALAYSIA", "SINGAPORE", "LAOS",
             "VIETNAM", "PHILIPPINES", "BRUNEI", "INDONESIA"),
    point_x = c(96.5, 100.5, 105, 102, 103.85, 102.5, 106.5, 122, 114.5, 120),
    point_y = c(20, 13.5, 11.5, 3.5, 1.35, 19, 14, 13, 4.5, -1),
    flag_x = c(86, 86, 86, 86, 86, 145, 145, 145, 145, 145),
    flag_y = c(27, 19.5, 11.5, 3, -6, 27, 19.5, 11.5, 3, -7),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
#' @noRd
.get_map_region <- function(world, map_region) {
  if (length(map_region) == 1) {
    # World
    if (map_region == "world") return(world)

    # ASEAN
    if (map_region %in% c("asean", "southeast_asia", "seasia")) {
      return(world %>% dplyr::filter(iso_a3 %in% c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM")))
    }

    # Continents
    if (map_region == "asia") return(world %>% dplyr::filter(continent == "Asia"))
    if (map_region == "europe") return(world %>% dplyr::filter(continent == "Europe"))
    if (map_region == "africa") return(world %>% dplyr::filter(continent == "Africa"))
    if (map_region == "oceania") return(world %>% dplyr::filter(continent == "Oceania"))
    if (map_region == "americas") return(world %>% dplyr::filter(continent %in% c("North America", "South America")))
    if (map_region == "north_america") return(world %>% dplyr::filter(continent == "North America"))
    if (map_region == "south_america") return(world %>% dplyr::filter(continent == "South America"))

    # Sub-regions
    if (map_region == "east_asia") {
      return(world %>% dplyr::filter(iso_a3 %in% c("CHN", "JPN", "KOR", "PRK", "MNG", "TWN", "HKG", "MAC")))
    }
    if (map_region == "south_asia") {
      return(world %>% dplyr::filter(iso_a3 %in% c("IND", "PAK", "BGD", "LKA", "NPL", "BTN", "MDV", "AFG")))
    }
    if (map_region %in% c("mena", "middle_east")) {
      return(world %>% dplyr::filter(iso_a3 %in% c("SAU", "ARE", "QAT", "KWT", "BHR", "OMN", "YEM", "IRQ", "IRN", "JOR", "LBN", "SYR", "ISR", "PSE", "TUR", "EGY", "LBY", "TUN", "DZA", "MAR", "SDN", "SSD")))
    }
    if (map_region == "central_asia") {
      return(world %>% dplyr::filter(iso_a3 %in% c("KAZ", "UZB", "TKM", "TJK", "KGZ")))
    }
    if (map_region %in% c("eu", "european_union")) {
      return(world %>% dplyr::filter(iso_a3 %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")))
    }
    if (map_region == "western_europe") {
      return(world %>% dplyr::filter(iso_a3 %in% c("GBR", "FRA", "DEU", "ITA", "ESP", "PRT", "BEL", "NLD", "LUX", "CHE", "AUT", "IRL")))
    }
    if (map_region == "eastern_europe") {
      return(world %>% dplyr::filter(iso_a3 %in% c("POL", "CZE", "SVK", "HUN", "ROU", "BGR", "UKR", "BLR", "MDA", "RUS")))
    }
    if (map_region == "sub_saharan_africa") {
      return(world %>% dplyr::filter(continent == "Africa", !iso_a3 %in% c("EGY", "LBY", "TUN", "DZA", "MAR", "SDN")))
    }
    if (map_region == "north_africa") {
      return(world %>% dplyr::filter(iso_a3 %in% c("EGY", "LBY", "TUN", "DZA", "MAR", "SDN", "ESH")))
    }
    if (map_region == "latin_america") {
      return(world %>% dplyr::filter(continent == "South America" | iso_a3 %in% c("MEX", "GTM", "BLZ", "SLV", "HND", "NIC", "CRI", "PAN")))
    }
    if (map_region == "caribbean") {
      return(world %>% dplyr::filter(iso_a3 %in% c("CUB", "JAM", "HTI", "DOM", "PRI", "TTO", "BHS", "BRB", "GRD", "LCA", "VCT", "ATG", "DMA", "KNA")))
    }
    if (map_region == "pacific") {
      return(world %>% dplyr::filter(iso_a3 %in% c("AUS", "NZL", "PNG", "FJI", "SLB", "VUT", "NCL", "WSM", "TON", "KIR", "FSM", "MHL", "PLW")))
    }
    if (map_region %in% c("gcc", "gulf_cooperation_council")) {
      return(world %>% dplyr::filter(iso_a3 %in% c("SAU", "ARE", "QAT", "KWT", "BHR", "OMN")))
    }
    if (map_region %in% c("nafta", "usmca")) {
      return(world %>% dplyr::filter(iso_a3 %in% c("USA", "CAN", "MEX")))
    }
    if (map_region == "mercosur") {
      return(world %>% dplyr::filter(iso_a3 %in% c("BRA", "ARG", "URY", "PRY")))
    }
  }

  # If map_region is a vector of ISO codes
  return(world %>% dplyr::filter(iso_a3 %in% map_region))
}


#' @keywords internal
#' @noRd
.aggregate_map_data <- function(df, aggregate_by, value_col, aggregate_fun) {
  agg_function <- switch(aggregate_fun, "mean" = mean, "sum" = sum, "median" = median, mean)
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(aggregate_by))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_col), ~ agg_function(., na.rm = TRUE)), .groups = "drop")
}


#' @keywords internal
#' @noRd
.apply_filter <- function(data, filter_var) {
  for (col_name in names(filter_var)) {
    if (col_name %in% colnames(data)) {
      data <- data[data[[col_name]] %in% filter_var[[col_name]], ]
    }
  }
  return(data)
}


#' @keywords internal
#' @noRd
.get_region_limits <- function(map_region) {
  limits <- list(
    # World
    world = list(xlim = c(-180, 180), ylim = c(-60, 85)),

    # Continents
    asia = list(xlim = c(25, 150), ylim = c(-15, 60)),
    europe = list(xlim = c(-25, 50), ylim = c(35, 72)),
    africa = list(xlim = c(-20, 55), ylim = c(-40, 40)),
    americas = list(xlim = c(-170, -30), ylim = c(-60, 75)),
    "north_america" = list(xlim = c(-170, -50), ylim = c(15, 75)),
    "south_america" = list(xlim = c(-85, -30), ylim = c(-60, 15)),
    oceania = list(xlim = c(110, 180), ylim = c(-50, 5)),

    # Regional groupings
    asean = list(xlim = c(75, 150), ylim = c(-15, 30)),  # Expanded for labels
    "southeast_asia" = list(xlim = c(75, 150), ylim = c(-15, 30)),
    seasia = list(xlim = c(75, 150), ylim = c(-15, 30)),

    "east_asia" = list(xlim = c(100, 150), ylim = c(20, 55)),
    "south_asia" = list(xlim = c(60, 100), ylim = c(5, 40)),

    mena = list(xlim = c(-20, 65), ylim = c(10, 45)),
    "middle_east" = list(xlim = c(25, 65), ylim = c(10, 45)),

    "central_asia" = list(xlim = c(45, 90), ylim = c(35, 55)),

    eu = list(xlim = c(-15, 35), ylim = c(35, 72)),
    "european_union" = list(xlim = c(-15, 35), ylim = c(35, 72)),
    "western_europe" = list(xlim = c(-15, 20), ylim = c(40, 60)),
    "eastern_europe" = list(xlim = c(15, 45), ylim = c(40, 60)),

    "sub_saharan_africa" = list(xlim = c(-20, 55), ylim = c(-40, 20)),
    "north_africa" = list(xlim = c(-20, 40), ylim = c(10, 40)),

    "latin_america" = list(xlim = c(-120, -30), ylim = c(-60, 35)),
    caribbean = list(xlim = c(-90, -55), ylim = c(10, 30)),

    pacific = list(xlim = c(110, 180), ylim = c(-50, 30)),

    # Economic regions
    gcc = list(xlim = c(35, 60), ylim = c(15, 35)),
    "gulf_cooperation_council" = list(xlim = c(35, 60), ylim = c(15, 35)),

    nafta = list(xlim = c(-170, -50), ylim = c(15, 75)),
    usmca = list(xlim = c(-170, -50), ylim = c(15, 75)),

    mercosur = list(xlim = c(-80, -35), ylim = c(-55, 15))
  )

  if (length(map_region) == 1 && map_region %in% names(limits)) {
    return(limits[[map_region]])
  }
  return(NULL)
}


#' @keywords internal
#' @noRd
.generate_map_name <- function(group, split_by, map_value_var = NULL) {
  name_parts <- sapply(split_by, function(col) as.character(unique(group[[col]])[1]))
  base_name <- paste0("map_", paste(name_parts, collapse = "_"))

  # Add map_value_var to filename if specified
  if (!is.null(map_value_var) && map_value_var != "") {
    # Clean the variable name for use in filename (replace special chars)
    clean_var <- gsub("[^A-Za-z0-9_]", "_", map_value_var)
    base_name <- paste0(base_name, "_", clean_var)
  }

  return(base_name)
}


#' @keywords internal
#' @noRd
.add_map_color_scale <- function(p, style_config, value_col, legend_var_name = NULL, global_scale_limits = NULL) {
  if (!is.null(style_config$legend_title)) {
    legend_title <- style_config$legend_title
  } else if (!is.null(legend_var_name)) {
    legend_title <- paste("Value of", legend_var_name)
  } else {
    legend_title <- value_col
  }

  if (!is.null(style_config$color_mid)) {
    p + ggplot2::scale_fill_gradient2(
      low = style_config$color_low,
      mid = style_config$color_mid,
      high = style_config$color_high,
      name = legend_title,
      na.value = style_config$color_na,
      midpoint = 0,
      limits = global_scale_limits
    )
  } else {
    p + ggplot2::scale_fill_gradient(
      low = style_config$color_low,
      high = style_config$color_high,
      name = legend_title,
      na.value = style_config$color_na,
      limits = global_scale_limits
    )
  }
}


#' @keywords internal
#' @noRd
.generate_map_title <- function(data, variable_col, unit_col, desc_col,
                                var_name_by_description, add_var_info,
                                style_config, value_col) {

  if (variable_col %in% colnames(data) && nrow(data) > 0) {
    var_code <- unique(data[[variable_col]])[1]
    var_desc <- if (desc_col %in% colnames(data)) unique(data[[desc_col]])[1] else var_code
    var_unit <- if (unit_col %in% colnames(data)) unique(data[[unit_col]])[1] else NULL

    title <- if (var_name_by_description) {
      if (add_var_info) paste0(var_desc, " (", var_code, ")") else var_desc
    } else {
      var_code
    }

    if (style_config$add_unit_to_title && !is.null(var_unit) && !is.na(var_unit) && var_unit != "") {
      converted_unit <- .convert_unit_for_display(var_unit)
      title <- paste0(title, " (", converted_unit, ")")
    }
  } else {
    title <- value_col
  }

  if (!is.null(style_config$title_format)) {
    title <- .apply_title_format(title, style_config$title_format, data)
  }

  return(title)
}


#' @keywords internal
#' @noRd
.apply_title_format <- function(title, title_format, data) {
  if (is.null(title_format$type)) return(title)

  switch(title_format$type,
         "standard" = title,
         "prefix" = paste0(title_format$text, title_format$sep, title),
         "suffix" = paste0(title, title_format$sep, title_format$text),
         "full" = title_format$text,
         "dynamic" = {
           result <- title_format$text
           if (grepl("\\{Description\\}", result)) {
             desc <- if ("Description" %in% colnames(data)) unique(data$Description)[1] else title
             result <- gsub("\\{Description\\}", desc, result)
           }
           if (grepl("\\{Variable\\}", result)) {
             var <- if ("Variable" %in% colnames(data)) unique(data$Variable)[1] else title
             result <- gsub("\\{Variable\\}", var, result)
           }
           result
         },
         title
  )
}


#' @keywords internal
#' @noRd
.generate_label_box_caption <- function(label_box_vars) {
  caption_lines <- sapply(1:nrow(label_box_vars), function(i) {
    paste(label_box_vars$symbol[i], label_box_vars$label[i])
  })
  paste(caption_lines, collapse = "    ")
}
