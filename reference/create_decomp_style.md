# Create Decomposition Area Plot Style Configuration

Creates a configuration list for controlling decomposition area plot
appearance and behavior.

## Usage

``` r
create_decomp_style(
  area_alpha = 0.7,
  area_position = "stack",
  show_area_lines = FALSE,
  area_line_color = "white",
  area_line_size = 0.3,
  color_palette = NULL,
  reverse_stack_order = FALSE,
  show_total_line = TRUE,
  total_line_color = "black",
  total_line_size = 1.2,
  total_line_type = "solid",
  show_points = FALSE,
  point_size = 2,
  legend_title = NULL,
  vertical_lines = NULL,
  vline_label_size = 3.5,
  vline_label_angle = 90,
  vline_label_vjust = -0.2,
  period_breaks = "auto",
  show_title = TRUE,
  title_face = "bold",
  title_size = 20,
  title_hjust = 0.5,
  add_unit_to_title = TRUE,
  title_margin = c(10, 0, 10, 0),
  title_format = list(type = "standard", text = "", sep = ""),
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
  show_legend = TRUE,
  show_legend_title = FALSE,
  legend_position = "right",
  legend_title_face = "bold",
  legend_text_face = "plain",
  legend_text_size = 14,
  strip_face = "bold",
  strip_text_size = 16,
  strip_background = "lightgrey",
  strip_text_margin = c(10, 0, 10, 0),
  panel_spacing = 2,
  panel_rows = NULL,
  panel_cols = NULL,
  show_axis_titles_on_all_facets = FALSE,
  background_color = "white",
  grid_color = "grey90",
  show_grid_major_x = TRUE,
  show_grid_major_y = TRUE,
  show_grid_minor_x = FALSE,
  show_grid_minor_y = FALSE,
  show_zero_line = FALSE,
  zero_line_type = "dashed",
  zero_line_color = "black",
  zero_line_size = 0.5,
  zero_line_position = 0,
  scale_limit = NULL,
  scale_increment = NULL,
  expansion_y_mult = c(0, 0.1),
  expansion_x_mult = c(0.05, 0.05),
  plot.margin = c(10, 25, 10, 10),
  all_font_size = 1
)
```

## Arguments

- area_alpha:

  Numeric. Transparency of area fills

- area_position:

  Character. Stacking method

- show_area_lines:

  Logical. Whether to show lines at area boundaries

- area_line_color:

  Character. Color of area boundary lines

- area_line_size:

  Numeric. Thickness of area boundary lines

- color_palette:

  Character or vector. Color palette for areas

- reverse_stack_order:

  Logical. Reverse the stacking order

- show_total_line:

  Logical. Overlay a line showing total

- total_line_color:

  Character. Color of the total line

- total_line_size:

  Numeric. Thickness of the total line

- total_line_type:

  Character. Line type

- show_points:

  Logical. Show points on the total line

- point_size:

  Numeric. Size of points

- legend_title:

  Character or NULL. Custom legend title

- vertical_lines:

  List or NULL. List of vertical line specs

- vline_label_size:

  Numeric. Font size for vertical line labels

- vline_label_angle:

  Numeric. Angle for vertical line labels

- vline_label_vjust:

  Numeric. Vertical adjustment for labels

- period_breaks:

  Character or numeric. Period axis breaks

- show_title:

  Logical. Show plot title

- title_face:

  Character. Title font face

- title_size:

  Numeric. Title font size

- title_hjust:

  Numeric. Title horizontal alignment

- add_unit_to_title:

  Logical. Append unit to title

- title_margin:

  Numeric vector

- title_format:

  List. Title format configuration

- show_x_axis_title:

  Logical. Show x-axis title

- x_axis_title_face:

  Character. X-axis title font face

- x_axis_title_size:

  Numeric. X-axis title size

- x_axis_title_margin:

  Numeric vector

- show_x_axis_labels:

  Logical. Show x-axis labels

- x_axis_text_face:

  Character. X-axis text face

- x_axis_text_size:

  Numeric. X-axis text size

- x_axis_text_angle:

  Numeric. X-axis text rotation

- x_axis_text_hjust:

  Numeric. X-axis text horizontal alignment

- x_axis_description:

  Character. Custom x-axis label

- show_y_axis_title:

  Logical. Show y-axis title

- y_axis_title_face:

  Character. Y-axis title font face

- y_axis_title_size:

  Numeric. Y-axis title size

- y_axis_title_margin:

  Numeric vector

- show_y_axis_labels:

  Logical. Show y-axis labels

- y_axis_text_face:

  Character. Y-axis text face

- y_axis_text_size:

  Numeric. Y-axis text size

- y_axis_text_angle:

  Numeric. Y-axis text rotation

- y_axis_text_hjust:

  Numeric. Y-axis text horizontal alignment

- y_axis_description:

  Character. Custom y-axis label

- show_legend:

  Logical. Show legend

- show_legend_title:

  Logical. Show legend title

- legend_position:

  Character. Legend position

- legend_title_face:

  Character. Legend title font face

- legend_text_face:

  Character. Legend text face

- legend_text_size:

  Numeric. Legend text size

- strip_face:

  Character. Facet strip text face

- strip_text_size:

  Numeric. Facet strip text size

- strip_background:

  Character. Facet strip background color

- strip_text_margin:

  Numeric vector

- panel_spacing:

  Numeric. Spacing between facet panels

- panel_rows:

  Integer or NULL. Number of facet rows

- panel_cols:

  Integer or NULL. Number of facet columns

- show_axis_titles_on_all_facets:

  Logical. Show axis titles on all panels

- background_color:

  Character. Plot background color

- grid_color:

  Character. Grid line color

- show_grid_major_x:

  Logical. Show major x-grid lines

- show_grid_major_y:

  Logical. Show major y-grid lines

- show_grid_minor_x:

  Logical. Show minor x-grid lines

- show_grid_minor_y:

  Logical. Show minor y-grid lines

- show_zero_line:

  Logical. Show horizontal zero line

- zero_line_type:

  Character. Zero line type

- zero_line_color:

  Character. Zero line color

- zero_line_size:

  Numeric. Zero line thickness

- zero_line_position:

  Numeric. Y-position of zero line

- scale_limit:

  Numeric vector or NULL. Y-axis limits

- scale_increment:

  Numeric or NULL. Y-axis tick increment

- expansion_y_mult:

  Numeric vector. Y-axis expansion

- expansion_x_mult:

  Numeric vector. X-axis expansion

- plot.margin:

  Numeric vector. Margins in mm

- all_font_size:

  Numeric. Master font size multiplier

## Value

List with decomposition area plot style configuration

## Author

Pattawee Puangchit

## Examples

``` r
basic_decomp_style <- create_decomp_style()
```
