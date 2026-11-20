# Create Time Trend Line Plots from GTAP Data

Generates time trend line plots for GTAP data, showing changes over time
periods. Supports panel facets, grouping by multiple variables, and
customizable line styling.

**Input Data**

## Usage

``` r
trend_plot(
  data,
  filter_var = NULL,
  period_col = "Period",
  split_by = NULL,
  line_group_by = "Case",
  panel_var = NULL,
  variable_col = "Variable",
  unit_col = "Unit",
  desc_col = "Description",
  separate_figure = FALSE,
  var_name_by_description = FALSE,
  add_var_info = FALSE,
  output_path = NULL,
  export_picture = TRUE,
  export_as_pdf = FALSE,
  export_config = NULL,
  trend_style_config = NULL,
  title_format = NULL
)
```

## Arguments

- data:

  A data frame or list of data frames containing GTAP results with time
  series data.

- filter_var:

  NULL, a vector, a data frame, or a named list specifying filtering
  conditions. For example:
  `list(Variable = c("EV", "qgdp"), REG = c("USA", "THA"))`.

- period_col:

  Character. Column name for time periods (x-axis). Default is
  `"Period"`.

- split_by:

  Character or vector. Column(s) used to create separate plots. If
  `NULL`, a single aggregated plot is generated. Example: `"Variable"`
  or `c("Variable", "AREG")`.

- line_group_by:

  Character or vector. Column(s) that define line grouping and coloring.
  For example, `"Case"` will create one line per case. Default is
  `"Case"`.

- panel_var:

  Character. Column for panel facets (e.g., `"Case"`). When specified,
  creates separate figures by default for each unique value. Set
  `separate_figure = FALSE` to use facet panels instead. Default is
  `NULL` (no faceting).

- variable_col:

  Character. Column name for variable codes. Default is `"Variable"`.

- unit_col:

  Character. Column name for units. Default is `"Unit"`.

- desc_col:

  Character. Column name for variable descriptions. Default is
  `"Description"`.

  **Plot Behavior**

- separate_figure:

  Logical. When `panel_var` is specified, controls whether to create
  separate plots (TRUE) or use facet panels (FALSE). Automatically set
  to `TRUE` when `panel_var` is specified. Default is `FALSE`.

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
  [`create_export_config`](https://www.pattawee-pp.com/GTAPViz/reference/create_export_config.md)
  or
  [`get_all_config`](https://www.pattawee-pp.com/GTAPViz/reference/get_all_config.md).

  **Styling**

- trend_style_config:

  List. Custom plot appearance settings for trend plots. See
  [`create_trend_style`](https://www.pattawee-pp.com/GTAPViz/reference/create_trend_style.md)
  for available options.

- title_format:

  List or NULL. Title format configuration created with
  [`create_title_format`](https://www.pattawee-pp.com/GTAPViz/reference/create_title_format.md).
  Supports "standard", "prefix", "suffix", "full", and "dynamic" types.
  If `NULL`, uses standard formatting. Example:
  `create_title_format(type = "dynamic", text = "Impacts on {Description}", sep = "")`.

## Value

A ggplot object or a named list of ggplot objects depending on input
settings. If `export_picture` or `export_as_pdf` is enabled, plots are
also saved to `output_path`.

## Author

Pattawee Puangchit

## Examples

``` r
# Create simple example dataset
data <- data.frame(
  Period = rep(2017:2025, each = 3),
  Case = rep(c("Uniform Country", "Uniform Sector", "Heterogeneous"), times = 9),
  Variable = "GDP",
  AREG = "ASEAN",
  Value = cumsum(rnorm(27, 0, 0.2))
)

# Define vertical lines for key shock years
vlines <- list(
  create_vline(2019, "Shock 1", "black", "dashed"),
  create_vline(2023, "Shock 2", "blue", "dotted"),
  create_vline(2025, "Shock 3", "darkgreen", "dashed")
)

# Define trend style configuration
trend_style <- create_trend_style(
  show_points = TRUE,
  smooth_line = FALSE,
  line_size = 1,
  vertical_lines = vlines,
  title_format = create_title_format(type = "prefix", text = "Impact on")
)

# Generate example trend plot
trend_plot(
  data = data,
  period_col = "Period",
  line_group_by = "Case",
  split_by = "Variable",
  trend_style_config = trend_style
)
```
