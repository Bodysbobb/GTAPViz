# Create Decomposition Area Charts Over Time

Generates stacked area charts showing how different components
contribute to a total value over time periods. Ideal for visualizing GDP
decomposition, welfare contributions, or any time-series decomposition
analysis where you want to see both the evolution of individual
components and their cumulative effect.

**Input Data**

## Usage

``` r
decomp_area_plot(
  data,
  filter_var = NULL,
  period_col = "Period",
  component_col,
  split_by = NULL,
  panel_var = "Experiment",
  variable_col = "Variable",
  unit_col = "Unit",
  desc_col = "Description",
  separate_figure = FALSE,
  show_total_line = TRUE,
  normalize = FALSE,
  var_name_by_description = FALSE,
  add_var_info = FALSE,
  output_path = NULL,
  export_picture = TRUE,
  export_as_pdf = FALSE,
  export_config = NULL,
  decomp_style_config = NULL,
  title_format = NULL
)
```

## Arguments

- data:

  A data frame or list of data frames containing GTAP time-series
  results.

- filter_var:

  NULL, a vector, a data frame, or a named list specifying filtering
  conditions. For example:
  `list(Variable = c("GDP"), Country = c("USA", "CHN"))`.

- period_col:

  Character. Column name for time periods (x-axis). Default is
  `"Period"`.

- component_col:

  Character. Column containing decomposition components (e.g.,
  "Component", "Sector"). This defines the areas that will be stacked.

- split_by:

  Character or vector.

  - Column(s) used to create separate plots (e.g., `"Country"` or
    `c("Country", "Variable")`).

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

- separate_figure:

  Logical. If `TRUE`, generates a separate plot for each value in
  `panel_var`. Default is `FALSE`.

- show_total_line:

  Logical. If `TRUE`, overlays a line showing the total across all
  components. Default is `TRUE`.

- normalize:

  Logical. If `TRUE`, shows percentage contribution (0-100\\

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

- decomp_style_config:

  List. Custom plot appearance settings for decomposition area plots.
  See
  [`create_decomp_style`](https://www.pattawee-pp.com/GTAPViz/reference/create_decomp_style.md)
  for available options.

- title_format:

  List or NULL. Title format configuration created with
  [`create_title_format`](https://www.pattawee-pp.com/GTAPViz/reference/create_title_format.md).
  Supports "standard", "prefix", "suffix", "full", and "dynamic" types.
  If `NULL`, uses standard formatting. Example:
  `create_title_format(type = "dynamic", text = "Decomposition: {Description}", sep = "")`.

## Value

A ggplot object or a named list of ggplot objects depending on input
settings. If `export_picture` or `export_as_pdf` is enabled, plots are
also saved to `output_path`.

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic GDP decomposition by country
decomp_area_plot(
  data = gdp_data,
  period_col = "Year",
  component_col = "Component", # Consumption, Investment, Net Exports, etc.
  split_by = "Country"
)

# Example 2: Normalized view with custom styling
my_decomp_style <- create_decomp_style(
  color_palette = "economic",
  show_total_line = TRUE,
  total_line_size = 1.5,
  area_alpha = 0.7,
  vertical_lines = list(
    create_vline(2008, "Financial Crisis", "red", "dashed"),
    create_vline(2020, "COVID-19", "darkred", "dashed")
  )
)

decomp_area_plot(
  data = welfare_data,
  period_col = "Year",
  component_col = "Source",
  split_by = "Region",
  normalize = TRUE,
  decomp_style_config = my_decomp_style,
  title_format = create_title_format(
    type = "dynamic",
    text = "Welfare Decomposition: {Description}",
    sep = " - "
  )
)
} # }
```
