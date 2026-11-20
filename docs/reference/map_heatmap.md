# Create Country/Region Heatmaps

Creates geographical heatmaps for GTAP data with optional label boxes.

**Input Data**

## Usage

``` r
map_heatmap(
  data,
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
  show_unmatched_iso = TRUE
)
```

## Arguments

- data:

  Data frame or list. GTAP data with country codes and values

- filter_var:

  NULL, vector, data frame, or named list specifying filtering
  conditions

- iso_col:

  Character. Column with ISO3 country codes. Default: `"REG"`

- value_col:

  Character. Column with numeric values. Default: `"Value"`

- split_by:

  Character or vector. Column(s) to create separate maps. Default:
  `NULL`

- variable_col:

  Character. Column with variable codes. Default: `"Variable"`

- unit_col:

  Character. Column with units. Default: `"Unit"`

- desc_col:

  Character. Column with descriptions. Default: `"Description"`

  **Map Region Settings**

- map_region:

  Character or vector. Region to display. Default: `"world"`

  **Supported regions:**

  - **Continents:** `"world"` (default), `"asia"`, `"europe"`,
    `"africa"`, `"americas"`, `"north_america"`, `"south_america"`,
    `"oceania"`

  - **Sub-regions:** `"asean"` (or `"southeast_asia"`, `"seasia"`),
    `"east_asia"`, `"south_asia"`, `"central_asia"`

  - **Middle East/Africa:** `"mena"` (or `"middle_east"`), `"gcc"` (Gulf
    Cooperation Council), `"north_africa"`, `"sub_saharan_africa"`

  - **Europe:** `"eu"` (or `"european_union"`), `"western_europe"`,
    `"eastern_europe"`

  - **Americas:** `"latin_america"`, `"caribbean"`, `"nafta"` (or
    `"usmca"`), `"mercosur"`

  - **Pacific:** `"pacific"`

  - **Custom:** Vector of ISO3 codes (e.g., `c("USA", "CAN", "MEX")`)

- xlim:

  Numeric vector. Longitude limits `c(min, max)`. Default: `NULL` (auto
  from region)

- ylim:

  Numeric vector. Latitude limits `c(min, max)`. Default: `NULL`

  **Label Box Settings**

- show_flags:

  Logical. Display country flags. Default: `FALSE`

- label_box_vars:

  Data frame or NULL. Variables to show in boxes. Create with
  [`create_label_box_vars`](https://www.pattawee-pp.com/GTAPViz/reference/create_label_box_vars.md).
  If `NULL`, no label boxes shown. Default: `NULL`

- map_value_var:

  Character or NULL. Variable for map coloring. If `NULL`, uses first
  variable or all data. Default: `NULL`

  **Aggregation**

- aggregate_by:

  Character or vector. Columns to aggregate by. Default: `NULL`

- aggregate_fun:

  Character. Aggregation function: `"mean"`, `"sum"`, `"median"`.
  Default: `"mean"`

  **Variable Display**

- var_name_by_description:

  Logical. Use descriptions in titles. Default: `FALSE`

- add_var_info:

  Logical. Append variable codes. Default: `FALSE`

- common_scale:

  Logical. Use unified color scale across split maps. When `TRUE` and
  `split_by` is used, all maps share the same color scale based on
  global min/max values, making colors comparable across maps. When
  `FALSE`, each map uses its own min/max for color scaling. Default:
  `FALSE`

  **Export Settings**

- output_path:

  Character. Directory to save maps. Default: `NULL`

- export_picture:

  Logical. Export as PNG. Default: `TRUE`

- export_as_pdf:

  Logical or `"merged"`. Export as PDF. Default: `FALSE`

- export_config:

  List. Export options. See
  [`create_export_config`](https://www.pattawee-pp.com/GTAPViz/reference/create_export_config.md)

  **Styling**

- map_style_config:

  List. Map appearance. See
  [`create_map_style`](https://www.pattawee-pp.com/GTAPViz/reference/create_map_style.md)

- title_format:

  List or NULL. Title formatting. See
  [`create_title_format`](https://www.pattawee-pp.com/GTAPViz/reference/create_title_format.md)

- show_unmatched_iso:

  Logical. Warn about unmatched ISO codes. Default: `TRUE`

## Value

List with `plots` (ggplot objects) and `unmatched_iso` (character
vector)

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple map without labels
map_heatmap(
  data = gtap_data,
  iso_col = "Region",
  value_col = "Value",
  split_by = "Case"
)

# Map with label boxes showing multiple variables
label_vars <- create_label_box_vars(
  variable = c("EV", "qgdp", "qpriv"),
  label = c("Welfare", "GDP", "Consumption")
)

map_heatmap(
  data = gtap_data,
  iso_col = "Region",
  map_value_var = "qgdp",
  label_box_vars = label_vars,
  show_flags = TRUE,
  split_by = "Case"
)

# Use unified color scale across all maps for comparison
map_heatmap(
  data = gtap_data,
  iso_col = "Region",
  value_col = "Value",
  split_by = "Case",
  common_scale = TRUE  # All maps use same color scale
)
} # }
```
