# Create Trend Plot Style Configuration

Creates a configuration list for controlling trend plot appearance and
behavior. Extends the base plot style with line-specific options.

## Usage

``` r
create_trend_style(
  show_points = TRUE,
  point_size = 2,
  point_shape = 19,
  point_alpha = 1,
  line_size = 1,
  line_type = "solid",
  line_alpha = 1,
  smooth_line = FALSE,
  smooth_se = FALSE,
  smooth_span = 0.75,
  color_palette = NULL,
  legend_title = NULL,
  period_breaks = "auto",
  vertical_lines = NULL,
  vline_label_size = 3.5,
  vline_label_angle = 90,
  vline_label_vjust = -0.2,
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
  show_legend_title = TRUE,
  legend_position = "right",
  legend_text_face = "plain",
  legend_text_size = 14,
  legend_key_width = 3,
  legend_key_height = 1,
  legend_line_size = 2,
  legend_point_size = 3,
  strip_face = "bold",
  strip_text_size = 16,
  strip_background = "lightgrey",
  strip_text_margin = c(10, 0, 10, 0),
  panel_spacing = 2,
  panel_rows = NULL,
  panel_cols = NULL,
  background_color = "white",
  grid_color = "grey90",
  show_grid_major_x = TRUE,
  show_grid_major_y = TRUE,
  show_grid_minor_x = FALSE,
  show_grid_minor_y = FALSE,
  show_zero_line = TRUE,
  zero_line_type = "dashed",
  zero_line_color = "black",
  zero_line_size = 0.5,
  zero_line_position = 0,
  scale_limit = NULL,
  scale_increment = NULL,
  expansion_y_mult = c(0.05, 0.1),
  expansion_x_mult = c(0.05, 0.05),
  plot.margin = c(10, 25, 10, 10),
  all_font_size = 1
)
```

## Arguments

- show_points:

  Logical. Whether to show points at each period. Default: TRUE

- point_size:

  Numeric. Size of points on lines. Default: 2

- point_shape:

  Numeric or character. Shape of points (0-25 or "circle", "square",
  etc.). Default: 19 (solid circle)

- point_alpha:

  Numeric. Transparency of points (0-1). Default: 1

- line_size:

  Numeric. Thickness of trend lines. Default: 1

- line_type:

  Character. Line type ("solid", "dashed", "dotted", etc.). Default:
  "solid"

- line_alpha:

  Numeric. Transparency of lines (0-1). Default: 1

- smooth_line:

  Logical. Use smoothed lines (loess) instead of straight lines.
  Default: FALSE

- smooth_se:

  Logical. Show confidence interval around smooth lines. Default: FALSE

- smooth_span:

  Numeric. Smoothing parameter for loess (0-1). Default: 0.75

- color_palette:

  Character or vector. Color palette for lines. Options include:
  "default", "gtap", "viridis", "magma", "plasma", "inferno", "cividis",
  RColorBrewer palettes, or custom color vector. Default: NULL

- legend_title:

  Character. Custom title for the legend. Default: NULL

- period_breaks:

  Character or numeric vector. X-axis breaks: "all", "auto", or custom
  numeric vector. Default: "auto"

- vertical_lines:

  List or NULL. List of vertical line specifications created with
  create_vline(). Default: NULL

- vline_label_size:

  Numeric. Font size for vertical line labels. Default: 3.5

- vline_label_angle:

  Numeric. Angle for vertical line labels. Default: 90

- vline_label_vjust:

  Numeric. Vertical adjustment for vline labels. Default: -0.2

- show_title:

  Logical. Show plot title. Default: TRUE

- title_face:

  Character. Title font face. Default: "bold"

- title_size:

  Numeric. Title size. Default: 20

- title_hjust:

  Numeric. Title horizontal justification. Default: 0.5

- add_unit_to_title:

  Logical. Append unit to title. Default: TRUE

- title_margin:

  Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)

- title_format:

  List. Title formatting. See
  [`create_title_format`](https://www.pattawee-pp.com/GTAPViz/reference/create_title_format.md)

- show_x_axis_title:

  Logical. Show x-axis title. Default: TRUE

- x_axis_title_face:

  Character. X-axis title font face. Default: "bold"

- x_axis_title_size:

  Numeric. X-axis title size. Default: 16

- x_axis_title_margin:

  Numeric vector. X-axis title margins. Default: c(25, 25, 0, 0)

- show_x_axis_labels:

  Logical. Show x-axis labels. Default: TRUE

- x_axis_text_face:

  Character. X-axis text font face. Default: "plain"

- x_axis_text_size:

  Numeric. X-axis text size. Default: 14

- x_axis_text_angle:

  Numeric. X-axis text angle. Default: 0

- x_axis_text_hjust:

  Numeric. X-axis text horizontal justification. Default: 0

- x_axis_description:

  Character. Custom x-axis title. Default: ""

- show_y_axis_title:

  Logical. Show y-axis title. Default: TRUE

- y_axis_title_face:

  Character. Y-axis title font face. Default: "bold"

- y_axis_title_size:

  Numeric. Y-axis title size. Default: 16

- y_axis_title_margin:

  Numeric vector. Y-axis title margins. Default: c(25, 25, 0, 0)

- show_y_axis_labels:

  Logical. Show y-axis labels. Default: TRUE

- y_axis_text_face:

  Character. Y-axis text font face. Default: "plain"

- y_axis_text_size:

  Numeric. Y-axis text size. Default: 14

- y_axis_text_angle:

  Numeric. Y-axis text angle. Default: 0

- y_axis_text_hjust:

  Numeric. Y-axis text horizontal justification. Default: 0

- y_axis_description:

  Character. Custom y-axis title. Default: ""

- show_legend:

  Logical. Show legend. Default: TRUE

- show_legend_title:

  Logical. Show legend title (group name). Default: TRUE

- legend_position:

  Character. Legend position ("right", "left", "top", "bottom").
  Default: "right"

- legend_text_face:

  Character. Legend text font face. Default: "plain"

- legend_text_size:

  Numeric. Legend text size. Default: 14

- legend_key_width:

  Numeric. Width of legend key symbols (lines/points). Default: 3

- legend_key_height:

  Numeric. Height of legend key symbols (lines/points). Default: 1

- legend_line_size:

  Numeric. Line thickness in legend. Default: 2

- legend_point_size:

  Numeric. Point size in legend. Default: 3

- strip_face:

  Character. Facet strip font face. Default: "bold"

- strip_text_size:

  Numeric. Facet strip text size. Default: 16

- strip_background:

  Character. Facet strip background color. Default: "lightgrey"

- strip_text_margin:

  Numeric vector. Strip text margins. Default: c(10, 0, 10, 0)

- panel_spacing:

  Numeric. Spacing between facet panels. Default: 2

- panel_rows:

  Numeric or NULL. Number of panel rows. Default: NULL

- panel_cols:

  Numeric or NULL. Number of panel columns. Default: NULL

- background_color:

  Character. Plot background color. Default: "white"

- grid_color:

  Character. Grid line color. Default: "grey90"

- show_grid_major_x:

  Logical. Show major x-axis grid lines. Default: TRUE

- show_grid_major_y:

  Logical. Show major y-axis grid lines. Default: TRUE

- show_grid_minor_x:

  Logical. Show minor x-axis grid lines. Default: FALSE

- show_grid_minor_y:

  Logical. Show minor y-axis grid lines. Default: FALSE

- show_zero_line:

  Logical. Show zero reference line. Default: TRUE

- zero_line_type:

  Character. Zero line type. Default: "dashed"

- zero_line_color:

  Character. Zero line color. Default: "black"

- zero_line_size:

  Numeric. Zero line thickness. Default: 0.5

- zero_line_position:

  Numeric. Position of zero line. Default: 0

- scale_limit:

  Numeric vector or NULL. Y-axis limits c(min, max). Default: NULL

- scale_increment:

  Numeric or NULL. Y-axis scale increment. Default: NULL

- expansion_y_mult:

  Numeric vector. Y-axis expansion multipliers c(bottom, top). Default:
  c(0.05, 0.1)

- expansion_x_mult:

  Numeric vector. X-axis expansion multipliers c(left, right). Default:
  c(0.05, 0.05)

- plot.margin:

  Numeric vector c(top, right, bottom, left). Margins around plot in mm.
  Default: c(10, 25, 10, 10)

- all_font_size:

  Numeric. Master control for all font sizes. Default: 1

## Value

List with trend plot style configuration

## Author

Pattawee Puangchit

## Examples

``` r
basic_trend_style <- create_trend_style()

custom_trend_style <- create_trend_style(
  show_points = TRUE,
  point_size = 3,
  line_size = 1.5,
  color_palette = "viridis",
  smooth_line = TRUE,
  vertical_lines = list(
    create_vline(2019, "Shock 1", "red", "dashed"),
    create_vline(2023, "Shock 2", "blue", "dotted")
  )
)
```
