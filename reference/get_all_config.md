# Print Plot and Export Configuration Snippets

Retrieve full configuration code as a list for applying in the plot
styling and export settings.

## Usage

``` r
get_all_config(
  plot_style = "default",
  plot_config = TRUE,
  export_config = TRUE
)
```

## Arguments

- plot_style:

  Character. Plot style to use (currently only \`"default"\` is
  supported).

- plot_config:

  Logical. If \`TRUE\`, prints the plot style configuration.

- export_config:

  Logical. If \`TRUE\`, prints the export configuration.

## Value

A named list containing the current default values for all GTAPViz
configuration options, including plot styles, table formats, and export
parameters.

## Details

Onece printing into the console, users can simply copy and paste the
entire list of configurations, rename it (if needed), and use it in your
plot functions directly.

## Author

Pattawee Puangchit

## Examples

``` r
# Input Path:
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

# Retrive configurations
get_all_config()
#> my_style_config <- list(
#> 
#>   # Title settings
#>   show_title = TRUE,
#>   title_face = "bold",
#>   title_size = 20,
#>   title_hjust = 0.5,
#>   add_unit_to_title = TRUE,
#>   title_margin = c(10, 0, 10, 0), #c(top, right, bottom, left)
#>   title_format = create_title_format(
#>     type = "standard", #option: prefix, suffix, full, dynamic
#>     text = "",
#>     sep = ""
#>   ),
#> 
#>   # X-Axis settings
#>   show_x_axis_title = TRUE,
#>   x_axis_title_face = "bold",
#>   x_axis_title_size = 16,
#>   x_axis_title_margin = c(25, 25, 0, 0), #c(top, right, bottom, left)
#>   show_x_axis_labels = TRUE,
#>   x_axis_text_face = "plain",
#>   x_axis_text_size = 14,
#>   x_axis_text_angle = 0,
#>   x_axis_text_hjust = 0,
#>   x_axis_description = "",
#> 
#>   # Y-Axis settings
#>   show_y_axis_title = TRUE,
#>   y_axis_title_face = "bold",
#>   y_axis_title_size = 16,
#>   y_axis_title_margin = c(25, 25, 0, 0), #c(top, right, bottom, left)
#>   show_y_axis_labels = TRUE,
#>   y_axis_text_face = "plain",
#>   y_axis_text_size = 14,
#>   y_axis_text_angle = 0,
#>   y_axis_text_hjust = 0,
#>   y_axis_description = "",
#>   show_axis_titles_on_all_facets = TRUE,
#> 
#>   # Value Labels
#>   show_value_labels = TRUE,
#>   value_label_face = "plain",
#>   value_label_size = 5,
#>   value_label_position = "above",
#>   value_label_decimal_places = 2,
#> 
#>   # Legend
#>   show_legend = FALSE,
#>   show_legend_title = FALSE,
#>   legend_position = "bottom",
#>   legend_title_face = "bold",
#>   legend_text_face = "plain",
#>   legend_text_size = 14,
#> 
#>   # Panel Strip
#>   strip_face = "bold",
#>   strip_text_size = 16,
#>   strip_background = "lightgrey",
#>   strip_text_margin = c(10, 0, 10, 0), #c(top, right, bottom, left)
#> 
#>   # Panel Layout
#>   panel_spacing = 2,
#>   panel_rows = NULL,
#>   panel_cols = NULL,
#>   theme = NULL,
#> 
#>   # Colors and Grid 
#>   color_tone = NULL,
#>   color_palette_type = "qualitative", #option: qualitative, sequential, diverging
#>   positive_color = "#2E8B57",
#>   negative_color = "#CD5C5C",
#>   background_color = "white",
#>   grid_color = "grey90",
#>   show_grid_major_x = FALSE,
#>   show_grid_major_y = FALSE,
#>   show_grid_minor_x = FALSE,
#>   show_grid_minor_y = FALSE,
#> 
#>   # Zero Line
#>   show_zero_line = TRUE,
#>   zero_line_type = "dashed",
#>   zero_line_color = "black",
#>   zero_line_size = 0.5,
#>   zero_line_position = 0,
#> 
#>   # Bar Chart
#>   bar_width = 0.9,
#>   bar_spacing = 0.9,
#> 
#>   # Scale Settings
#>   scale_limit = NULL,
#>   scale_increment = NULL,
#> 
#>   # Scale Expansion
#>   expansion_y_mult = c(0.05, 0.1),
#>   expansion_x_mult = c(0.05, 0.05),
#> 
#>   # Font Size Control
#>   all_font_size = 1,
#> 
#>   # Data Sorting
#>   sort_data_by_value = FALSE,
#> 
#>   # Plot Margin
#>   plot.margin = c(10, 25, 10, 10) #c(top, right, bottom, left)
#> )
#> 
#> # Example usage:
#> # comparison_plot(data, x_axis_from = "REG", plot_style_config = my_style_config)
#> 
#> 
#> my_export_config <- list(
#>   file_name = "gtap_plots",
#>   width = NULL,
#>   height = NULL,
#>   dpi = 300,
#>   bg = "white",
#>   limitsize = FALSE
#> )
#> 
#> # Example usage:
#> # comparison_plot(data, x_axis_from = "REG", export_config = my_export_config)
```
