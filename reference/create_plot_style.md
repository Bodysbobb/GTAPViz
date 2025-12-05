# Create a Plot Style Configuration

Creates a configuration list for plot styling that can be used with
GTAPViz plotting functions. This function provides auto-completion for
style options while maintaining compatibility with direct list
specification.

## Usage

``` r
create_plot_style(
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
  show_axis_titles_on_all_facets = TRUE,
  show_value_labels = TRUE,
  value_label_face = "plain",
  value_label_size = 5,
  value_label_position = "above",
  value_label_decimal_places = 2,
  show_legend = FALSE,
  show_legend_title = FALSE,
  legend_position = "bottom",
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
  theme = NULL,
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
  show_zero_line = TRUE,
  zero_line_type = "dashed",
  zero_line_color = "black",
  zero_line_size = 0.5,
  zero_line_position = 0,
  bar_width = 0.9,
  bar_spacing = 0.9,
  scale_limit = NULL,
  scale_increment = NULL,
  expansion_y_mult = c(0.05, 0.1),
  expansion_x_mult = c(0.05, 0.05),
  all_font_size = 1,
  sort_data_by_value = FALSE,
  plot.margin = c(10, 25, 10, 10)
)
```

## Arguments

- show_title:

  Logical. Show or hide the plot title. Default: TRUE

- title_face:

  Character. Font face ("bold", "plain", "italic"). Default: "bold"

- title_size:

  Numeric. Font size of title. Default: 20

- title_hjust:

  Numeric. Horizontal alignment (0 = left, 1 = right). Default: 0.5

- add_unit_to_title:

  Logical. Append unit to title if applicable. Default: TRUE

- title_margin:

  Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)

- title_format:

  List or function output. Title formatting options. Can be created with
  [`create_title_format()`](https://pattawee-pp.com/GTAPViz/reference/create_title_format.md).
  Default: list(type = "standard", text = "", sep = "")

- show_x_axis_title:

  Logical. Show or hide x-axis title. Default: TRUE

- x_axis_title_face:

  Character. Font face for x-axis title. Default: "bold"

- x_axis_title_size:

  Numeric. Font size of x-axis title. Default: 16

- x_axis_title_margin:

  Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)

- show_x_axis_labels:

  Logical. Show or hide x-axis labels. Default: TRUE

- x_axis_text_face:

  Character. Font face for x-axis labels. Default: "plain"

- x_axis_text_size:

  Numeric. Font size of x-axis labels. Default: 14

- x_axis_text_angle:

  Numeric. Angle of x-axis labels. Default: 0

- x_axis_text_hjust:

  Numeric. Horizontal justification of x-axis labels. Default: 0

- x_axis_description:

  Character. Optional description for the x-axis. Default: ""

- show_y_axis_title:

  Logical. Show or hide y-axis title. Default: TRUE

- y_axis_title_face:

  Character. Font face for y-axis title. Default: "bold"

- y_axis_title_size:

  Numeric. Font size of y-axis title. Default: 16

- y_axis_title_margin:

  Numeric vector c(top, right, bottom, left). Default: c(25, 25, 0, 0)

- show_y_axis_labels:

  Logical. Show or hide y-axis labels. Default: TRUE

- y_axis_text_face:

  Character. Font face for y-axis labels. Default: "plain"

- y_axis_text_size:

  Numeric. Font size of y-axis labels. Default: 14

- y_axis_text_angle:

  Numeric. Angle of y-axis labels. Default: 0

- y_axis_text_hjust:

  Numeric. Horizontal justification of y-axis labels. Default: 0

- y_axis_description:

  Character. Optional description for the y-axis. Default: ""

- show_axis_titles_on_all_facets:

  Logical. Show axis titles on all facets. Default: TRUE

- show_value_labels:

  Logical. Show or hide value labels. Default: TRUE

- value_label_face:

  Character. Font face for value labels. Default: "plain"

- value_label_size:

  Numeric. Font size of value labels. Default: 5

- value_label_position:

  Character. Position of value labels ("above", "outside", "top").
  Default: "above"

- value_label_decimal_places:

  Numeric. Number of decimal places in value labels. Default: 2

- show_legend:

  Logical. Show or hide legend. Default: FALSE

- show_legend_title:

  Logical. Show or hide legend title. Default: FALSE

- legend_position:

  Character. Legend position ("none", "bottom", "right"). Default:
  "bottom"

- legend_title_face:

  Character. Font face for legend title. Default: "bold"

- legend_text_face:

  Character. Font face for legend text. Default: "plain"

- legend_text_size:

  Numeric. Font size of legend text. Default: 14

- strip_face:

  Character. Font face for panel strip. Default: "bold"

- strip_text_size:

  Numeric. Font size for panel strip. Default: 16

- strip_background:

  Character. Background color of strip. Default: "lightgrey"

- strip_text_margin:

  Numeric vector c(top, right, bottom, left). Default: c(10, 0, 10, 0)

- panel_spacing:

  Numeric. Spacing between panels. Default: 2

- panel_rows:

  Numeric or NULL. Number of rows in panel layout. Default: NULL

- panel_cols:

  Numeric or NULL. Number of columns in panel layout. Default: NULL

- theme:

  ggplot2 theme or NULL. Custom ggplot theme. Default: NULL

- color_tone:

  Character or NULL. Base color theme. Default: NULL

- color_palette_type:

  Character. Type of color palette ('qualitative', 'sequential',
  'diverging'). Default: "qualitative"

- positive_color:

  Character. Color for positive values. Default: "#2E8B57"

- negative_color:

  Character. Color for negative values. Default: "#CD5C5C"

- background_color:

  Character. Background color of plot. Default: "white"

- grid_color:

  Character. Color of grid lines. Default: "grey90"

- show_grid_major_x:

  Logical. Show major grid lines on x-axis. Default: FALSE

- show_grid_major_y:

  Logical. Show major grid lines on y-axis. Default: FALSE

- show_grid_minor_x:

  Logical. Show minor grid lines on x-axis. Default: FALSE

- show_grid_minor_y:

  Logical. Show minor grid lines on y-axis. Default: FALSE

- show_zero_line:

  Logical. Show or hide zero line. Default: TRUE

- zero_line_type:

  Character. Line type ("solid", "dashed", "dotted"). Default: "dashed"

- zero_line_color:

  Character. Color of zero line. Default: "black"

- zero_line_size:

  Numeric. Line thickness of zero line. Default: 0.5

- zero_line_position:

  Numeric. Position of the zero line. Default: 0

- bar_width:

  Numeric. Width of bars. Default: 0.9

- bar_spacing:

  Numeric. Spacing between groups of bars. Default: 0.9

- scale_limit:

  Numeric vector of length 2 or NULL. Manual limits for value axis.
  Default: NULL

- scale_increment:

  Numeric or NULL. Step size for axis tick marks. Default: NULL

- expansion_y_mult:

  Numeric vector. Y-axis expansion. Default: c(0.05, 0.1)

- expansion_x_mult:

  Numeric vector. X-axis expansion. Default: c(0.05, 0.05)

- all_font_size:

  Numeric. Master control for all font sizes. Default: 1

- sort_data_by_value:

  Logical. Whether to sort data by value. Default: FALSE

- plot.margin:

  Numeric vector c(top, right, bottom, left). Margins around the entire
  plot. Default: c(10, 25, 10, 10)

## Value

A list containing all plot style configuration parameters

## Author

Pattawee Puangchit

## Examples

``` r
# Create customized style with title formatting
custom_style <- create_plot_style(
  color_tone = "gtap",
  title_size = 24,
  title_format = create_title_format(
    type = "prefix",
    text = "Impact on",
    sep = "-"
  ),
  bar_width = 0.5,
  x_axis_text_angle = 45
)
```
