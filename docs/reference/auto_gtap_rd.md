# Process GTAP Recursive Dynamic Data with Multi-Case Support

Processes GTAP recursive dynamic data from `.sl4` or `.har` files with
support for multiple case scenarios, comparison analysis, and cumulative
effects. Data is always assigned to specified variable names in the
parent environment.

## Usage

``` r
auto_gtap_rd(
  input_detail,
  input_path = NULL,
  input_format = ".sl4",
  input_suffix = "",
  process_vars = NULL,
  comparison = TRUE,
  result_format = "cumulative",
  keep_raw_data = TRUE,
  mapping_info = "GTAPv7",
  external_mapping_info = NULL,
  extract_method = "get_data_by_var",
  priority_list = NULL,
  convert_unit = NULL,
  decimals = 4,
  rename_columns = TRUE,
  region_select = NULL,
  sector_select = NULL,
  subtotal_level = FALSE,
  main_output_name = "comparison.data",
  macro_output_name = "GTAPMacro"
)
```

## Arguments

- input_detail:

  Data frame. Output from
  [`dynamic_input_name`](https://www.pattawee-pp.com/GTAPViz/reference/dynamic_input_name.md)
  containing columns: `"Input"`, `"Case"`, `"Period"`, `"PeriodRange"`,
  `"SimType"`, and optionally `"ScenarioRank"`.

- input_path:

  Character. Path to the input folder containing GTAP data files.

- input_format:

  Character. File format to process. Options:

  - `".sl4"`: Solution files (default)

  - `".har"`: Header array files

- input_suffix:

  Character. Custom suffix for input files (e.g., `""`, `"-custom"`,
  `"-WEL"`). Default: `""`.

- process_vars:

  Character vector, `NULL`, or `FALSE`. Variables to extract:

  - Character vector: Specific variable names to extract

  - `NULL`: Extract all available variables (default)

  - `FALSE`: Skip variable processing

- comparison:

  Character or `NULL`. Comparison format for calculating differences.
  Format: `"minuend-subtrahend"` where result = minuend - subtrahend.
  Examples: `"policy-base_rerun"`, `"base_rerun-base"`. If `NULL`, no
  comparison is performed and raw data is returned. Default: `NULL`.

- result_format:

  Character. Format for comparison results:

  - `"single"`: Period-specific differences (default)

  - `"cumulative"`: Accumulated effects from initial period

- keep_raw_data:

  Logical. If `TRUE` and `comparison` is specified, retains original
  minuend and subtrahend values as separate columns alongside the
  difference. Default: `FALSE`.

- mapping_info:

  Character. Metadata mode for variable descriptions and units. Options:
  `"GTAPv7"` (default), `"Yes"`, `"No"`, `"Mix"`. See
  [`add_mapping_info`](https://www.pattawee-pp.com/GTAPViz/reference/add_mapping_info.md)
  for details.

- external_mapping_info:

  Data frame or `NULL`. Custom mapping table with columns: `"Variable"`,
  `"Description"`, `"Unit"`. Overrides default mappings. Default:
  `NULL`.

- extract_method:

  Character. Method for extracting data from files:

  - `"get_data_by_var"`: Extract by variable name (default)

  - `"get_data_by_dims"`: Extract by dimension structure

  - `"group_data_by_dims"`: Group and extract by dimension with priority
    rules

- priority_list:

  Optional list. Priority rules required when
  `extract_method = "group_data_by_dims"`. Specifies dimension hierarchy
  for grouping. Example:
  `list("Sector" = c("COMM", "ACTS"), "Region" = c("REG"))`. Default:
  `NULL`.

- convert_unit:

  Character or `NULL`. Unit conversion to apply:

  - `"mil2bil"`: Convert millions to billions

  - `"bil2mil"`: Convert billions to millions

  - `"pct2frac"`: Convert percentage to fraction

  - `"frac2pct"`: Convert fraction to percentage

  Default: `NULL` (no conversion).

- decimals:

  Integer or `NULL`. Number of decimal places for rounding numeric
  values. Set to `NULL` to disable rounding. Default: `4`.

- rename_columns:

  Logical. If `TRUE`, renames GTAP dimension codes to readable names
  (e.g., `"REG"` to `"Region"`, `"COMM"` to `"Commodity"`, `"ACTS"` to
  `"Activity"`). Default: `TRUE`.

- region_select:

  Character vector or `NULL`. Filter data to selected regions. Applies
  only to the `"REG"` (or `"Region"` if renamed) column. Default: `NULL`
  (all regions).

- sector_select:

  Character vector or `NULL`. Filter data to selected sectors. Applies
  to `"ACTS"` and `"COMM"` (or `"Activity"` and `"Commodity"` if
  renamed) columns. Default: `NULL` (all sectors).

- subtotal_level:

  Logical. If `TRUE`, includes subtotal rows in the output for
  aggregated dimensions. Default: `FALSE`.

- main_output_name:

  Character. Variable name to assign main/primary data output in parent
  environment. Default: `"plot.data"`.

- macro_output_name:

  Character. Variable name to assign macro economic data output in
  parent environment. Default: `"GTAPMacro"`.

## Value

Invisibly returns a list containing processed data:

- GTAPMacros:

  Macro economic indicators data frame (if processed)

- main_data:

  Primary variable data frame or list (if processed)

- bilateral_data:

  QXS bilateral trade data (if processed)

All processed data is also assigned to specified variable names in the
parent environment.

## Details

**Multi-Case Processing:**

The function processes multiple GTAP recursive dynamic cases
simultaneously. Cases are distinguished by the `"Case"` column in the
output. This allows comparative analysis across different policy
scenarios or model configurations.

**Scenario Ranking:**

If `input_detail` contains a `"ScenarioRank"` column (created by
[`dynamic_input_name`](https://www.pattawee-pp.com/GTAPViz/reference/dynamic_input_name.md)
with `add_scenario_ranking = TRUE`), this ranking is automatically
preserved throughout all processing steps. No additional parameters are
needed.

**Comparison Calculations:**

When `comparison` is specified, the function calculates differences
between simulation types (e.g., policy effects relative to baseline) for
each period and case:

- `result_format = "single"`: Calculates period-by-period differences

- `result_format = "cumulative"`: Accumulates differences over time
  within each group

Cumulative calculations group by all columns except `"Period"`, ensuring
correct accumulation for each unique combination of Case, Region,
Sector, Variable, etc.

**File Discovery:**

The function automatically discovers available input files and reports
missing files per case. Files must match the naming pattern specified in
`input_detail$Input` with the appropriate suffix and format extension.

**Output Structure:**

All outputs include:

- `Case`: Case identifier from input mapping

- `Period`: Time period (numeric year)

- `Value`: Main data values or calculated differences

- `Unit`: Variable units (required for GTAPViz compatibility)

- `ScenarioRank`: Scenario ordering (if present in `input_detail`)

- Additional columns: SimType (if no comparison), minuend/subtrahend
  values (if `keep_raw_data = TRUE`)

**Special Variable Processing:**

- **Macro variables**: Automatically processed from `.sl4` files when
  `process_vars = NULL` or includes `"macro"`, `"macros"`, or
  `"gtapmacro"`

- **QXS (bilateral trade)**: Automatically processed from `.sl4` files
  when `process_vars = NULL` or includes variables with `"qxs"` in the
  name

## See also

[`dynamic_input_name`](https://www.pattawee-pp.com/GTAPViz/reference/dynamic_input_name.md)
for generating input mappings,
[`add_mapping_info`](https://www.pattawee-pp.com/GTAPViz/reference/add_mapping_info.md)
for variable metadata,
[`convert_units`](https://www.pattawee-pp.com/GTAPViz/reference/convert_units.md)
for unit conversions

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Generate input mapping for multiple cases with ranking
dynamic_input_name(
  case_name = c("US_All", "US_Retail", "US_ReduceTar"),
  type = "prefix",
  input_format = list(
    US_All = list(
      base_rerun = "bwrb-rwrr-",
      policy = "bwrb-rwrr-pwrp-"
    ),
    US_Retail = list(
      base_rerun = "brtb-rtrr-",
      policy = "brtb-rtrr-prtp-"
    ),
    US_ReduceTar = list(
      base_rerun = "bfrb-rfrr-",
      policy = "bfrb-rfrr-pfrp-"
    )
  ),
  pattern = "2018:2035",
  increment = 1,
  add_scenario_ranking = TRUE,
  output = "Input_map"
)

# Example 2: Process with policy-baseline comparison (cumulative)
auto_gtap_rd(
  input_detail = Input_map,
  input_path = "path/to/data",
  input_format = ".sl4",
  comparison = "policy-base_rerun",
  result_format = "cumulative",
  keep_raw_data = TRUE,
  convert_unit = "mil2bil",
  main_output_name = "sl4.data"
)

# Example 3: Process specific variables without comparison
auto_gtap_rd(
  input_detail = Input_map,
  input_path = "path/to/data",
  input_format = ".sl4",
  process_vars = c("qgdp", "pop", "qo", "qxs"),
  comparison = NULL,
  extract_method = "get_data_by_var",
  main_output_name = "raw.data"
)

# Example 4: Process with custom mapping and filters
# Define custom variable mapping
custom_map <- data.frame(
  Variable = c("qgdp", "pop"),
  Description = c("Real GDP", "Population"),
  Unit = c("Billion USD", "Million persons")
)

auto_gtap_rd(
  input_detail = Input_map,
  input_path = "path/to/data",
  input_format = ".sl4",
  process_vars = custom_map,
  comparison = "policy-base_rerun",
  result_format = "single",
  mapping_info = "Mix",
  external_mapping_info = custom_map,
  region_select = c("USA", "CHN", "JPN"),
  decimals = 2,
  main_output_name = "filtered.data"
)

# Example 5: Process HAR decomposition files
auto_gtap_rd(
  input_detail = Input_map,
  input_path = "path/to/data",
  input_format = ".har",
  input_suffix = "-WEL",
  comparison = "policy-base_rerun",
  result_format = "cumulative",
  extract_method = "get_data_by_dims",
  main_output_name = "decomp.data"
)

# Example 6: Process with dimension grouping
auto_gtap_rd(
  input_detail = Input_map,
  input_path = "path/to/data",
  input_format = ".sl4",
  comparison = "policy-base_rerun",
  result_format = "cumulative",
  extract_method = "group_data_by_dims",
  priority_list = list(
    "Sector" = c("COMM", "ACTS"),
    "Region" = c("REG", "SRCREG", "DSTREG")
  ),
  main_output_name = "grouped.data"
)
} # }
```
