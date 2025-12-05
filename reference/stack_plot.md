# Create Stacked Bar Charts for Decomposition Analysis

Generates stacked bar charts to visualize value compositions across
multiple dimensions. Supports both stacked and unstacked layouts for
decomposition analysis, with full control over grouping, faceting,
top-impact filtering, and export styling.

**Input Data**

## Usage

``` r
stack_plot(
  data,
  filter_var = NULL,
  x_axis_from,
  stack_value_from,
  split_by = NULL,
  panel_var = "Experiment",
  variable_col = "Variable",
  unit_col = "Unit",
  desc_col = "Description",
  invert_axis = FALSE,
  separate_figure = FALSE,
  show_total = TRUE,
  unstack_plot = FALSE,
  top_impact = NULL,
  var_name_by_description = FALSE,
  add_var_info = FALSE,
  output_path = NULL,
  export_picture = TRUE,
  export_as_pdf = FALSE,
  export_config = NULL,
  plot_style_config = NULL
)
```

## Arguments

- data:

  A data frame or list of data frames containing GTAP results.

- filter_var:

  NULL, a vector, a data frame, or a named list specifying filtering
  conditions. For example:
  `list(Variable = c("EV", "qgdp"), REG = c("USA", "THA"))`.

- x_axis_from:

  Character. Column name used for the x-axis.

- stack_value_from:

  Character. Column containing stack component categories (e.g.,
  `"COMM"` for commodities).

- split_by:

  Character or vector.

  - Column(s) used to split plots by (e.g., `"REG"` or
    `c("COMM", "REG")`).

  - If `NULL`, a single aggregated plot is generated.

- panel_var:

  Character. Column for panel facets. Default is `"Experiment"`.

- variable_col:

  Character. Column name for variable codes. Default is `"Variable"`.

- unit_col:

  Character. Column name for units. Default is `"Unit"`.

- desc_col:

  Character. Column name for variable descriptions. Default is
  `"Description"`.

  **Plot Behavior**

- invert_axis:

  Logical. If `TRUE`, flips the plot orientation (horizontal bars).
  Default is `FALSE`.

- separate_figure:

  Logical. If `TRUE`, generates a separate plot for each value in
  `panel_var`. Default is `FALSE`.

- show_total:

  Logical. If `TRUE`, displays total values above stacked bars. Default
  is `TRUE`.

- unstack_plot:

  Logical. If `TRUE`, creates separate bar plots for each `x_axis_from`
  value instead of stacking. Default is `FALSE`.

- top_impact:

  Numeric or `NULL`. If specified, shows only the top N impactful
  values; `NULL` shows all.

  **Variable Display**

- var_name_by_description:

  Logical. If `TRUE`, uses descriptions instead of variable codes in
  titles. Default is `FALSE`.

- add_var_info:

  Logical. If `TRUE`, appends variable codes in parentheses after the
  description. Default is `FALSE`.

  **Export Settings**

- output_path:

  Character. Directory to save plots. If `NULL`, plots are returned but
  not saved.

- export_picture:

  Logical. If `TRUE`, exports plots as PNG images. Default is `TRUE`.

- export_as_pdf:

  Logical or `"merged"`.

  - `FALSE` (default): disables PDF export.

  - `TRUE`: exports each plot as a separate PDF file.

  - `"merged"`: combines all plots into a single PDF file.

- export_config:

  List. Export options including dimensions, DPI, and background. See
  [`create_export_config`](https://pattawee-pp.com/GTAPViz/reference/create_export_config.md)
  or
  [`get_all_config`](https://pattawee-pp.com/GTAPViz/reference/get_all_config.md).

  **Styling**

- plot_style_config:

  List. Custom plot appearance settings. See
  [`create_plot_style`](https://pattawee-pp.com/GTAPViz/reference/create_plot_style.md)
  or
  [`get_all_config`](https://pattawee-pp.com/GTAPViz/reference/get_all_config.md).

## Value

A ggplot object or a named list of ggplot objects depending on the
`separate_figure` setting. If `export_picture` or `export_as_pdf` is
enabled, the plots are also saved to `output_path`.

## See also

[`comparison_plot`](https://pattawee-pp.com/GTAPViz/reference/comparison_plot.md),
[`detail_plot`](https://pattawee-pp.com/GTAPViz/reference/detail_plot.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))

# Prepare Dataframe
welfare.decomp <- har.plot.data[["A"]]

# Plot
plotC <- stack_plot(
  # === Input Data ===
  data              = welfare.decomp,
  filter_var        = list(Region = "Oceania"),
  x_axis_from       = "Region",
  stack_value_from  = "COLUMN",
  split_by          = FALSE,
  panel_var         = "Experiment",
  variable_col      = "Variable",
  unit_col          = "Unit",
  desc_col          = "Description",

  # === Plot Behavior ===
  invert_axis     = FALSE,
  separate_figure = FALSE,
  show_total      = TRUE,
  unstack_plot    = FALSE,
  top_impact      = NULL,

  # === Variable Display ===
  var_name_by_description = TRUE,
  add_var_info            = FALSE,

  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(width = 28, height = 15),

  # === Styling ===
  plot_style_config = create_plot_style(
    color_tone                   = "gtap",
    panel_rows                   = 2,
    panel_cols                   = NULL,
    show_legend                  = TRUE,
    show_axis_titles_on_all_facets = FALSE
  )
)
```
