# Create Map Style Configuration

Creates styling options for map appearance, following the same pattern
as create_plot_style.

## Usage

``` r
create_map_style(
  show_title = TRUE,
  title_face = "bold",
  title_size = 20,
  title_hjust = 0.5,
  add_unit_to_title = TRUE,
  title_margin = c(10, 0, 10, 0),
  title_format = list(type = "standard", text = "", sep = ""),
  show_caption = TRUE,
  caption_text = NULL,
  caption_size = 13,
  caption_hjust = 0.5,
  caption_margin = c(12, 0, 0, 0),
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
  color_low = "#D9B17D",
  color_high = "#1C3E5D",
  color_mid = NULL,
  color_palette = NULL,
  color_na = "grey90",
  border_color = "white",
  border_size = 0.4,
  background_color = "white",
  country_name_bold = TRUE,
  country_name_face = "bold",
  country_name_size = 3.3,
  show_stat_labels = TRUE,
  stat_label_face = "plain",
  stat_label_size = 4,
  stat_label_bold = FALSE,
  label_spacing = 1.05,
  line_color = "grey40",
  line_size = 0.25,
  line_type = "solid",
  flag_size = 0.032,
  flag_aspect = 1.3,
  plot.margin = c(8, 8, 12, 8),
  all_font_size = 1
)
```

## Arguments

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

- show_caption:

  Logical. Show caption. Default: TRUE

- caption_text:

  Character or NULL. Custom caption text. Default: NULL

- caption_size:

  Numeric. Caption text size. Default: 13

- caption_hjust:

  Numeric. Caption horizontal justification. Default: 0.5

- caption_margin:

  Numeric vector. Caption margins. Default: c(12, 0, 0, 0)

- legend_position:

  Character. Legend position ("bottom", "top", "left", "right", "none").
  Default: "bottom"

- legend_direction:

  Character. Legend direction. Default: "horizontal"

- legend_title:

  Character or NULL. Custom legend title. Default: NULL

- show_legend_title:

  Logical. Show legend title. Default: TRUE

- legend_title_face:

  Character. Legend title font face. Default: "bold"

- legend_title_size:

  Numeric. Legend title size. Default: 15

- legend_text_face:

  Character. Legend text font face. Default: "plain"

- legend_text_size:

  Numeric. Legend text size. Default: 13

- legend_barwidth:

  Numeric. Legend bar width. Default: 16

- legend_barheight:

  Numeric. Legend bar height. Default: 0.7

- color_low:

  Character. Low value color. Default: "#D9B17D"

- color_high:

  Character. High value color. Default: "#1C3E5D"

- color_mid:

  Character or NULL. Mid value color for diverging scales. Default: NULL

- color_palette:

  Character or NULL. Named palette: "viridis", "magma", etc. Default:
  NULL

- color_na:

  Character. Color for NA values. Default: "grey90"

- border_color:

  Character. Country border color. Default: "white"

- border_size:

  Numeric. Border thickness. Default: 0.4

- background_color:

  Character. Background color of plot. Default: "white"

- country_name_bold:

  Logical. Bold country names. Default: TRUE

- country_name_face:

  Character. Country name font face. Default: "bold"

- country_name_size:

  Numeric. Country name text size. Default: 3.3

- show_stat_labels:

  Logical. Show statistics labels in boxes. Default: TRUE

- stat_label_face:

  Character. Statistics label font face. Default: "plain"

- stat_label_size:

  Numeric. Statistics label text size. Default: 4

- stat_label_bold:

  Logical. Bold statistics labels. Default: FALSE

- label_spacing:

  Numeric. Line height spacing for multi-line labels. Default: 1.05

- line_color:

  Character. Connecting line color. Default: "grey40"

- line_size:

  Numeric. Line thickness. Default: 0.25

- line_type:

  Character. Line type ("solid", "dashed", "dotted"). Default: "solid"

- flag_size:

  Numeric. Flag image size. Default: 0.032

- flag_aspect:

  Numeric. Flag aspect ratio. Default: 1.3

- plot.margin:

  Numeric vector c(top, right, bottom, left). Margins around plot in mm.
  Default: c(8, 8, 12, 8)

- all_font_size:

  Numeric. Master control for all font sizes. Default: 1

## Value

List with style configuration

## Author

Pattawee Puangchit

## Examples

``` r
asean_style <- create_map_style(
  color_low = "#D9B17D",
  color_high = "#1C3E5D",
  country_name_size = 4,
  stat_label_size = 3.5,
  plot.margin = c(15, 50, 15, 50)
)
```
