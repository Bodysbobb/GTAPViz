# Process Static GTAP Data with Simplified Parameter Structure

Processes static GTAP data from `.sl4` or `.har` files with a simplified
parameter structure similar to
[`auto_gtap_rd`](https://www.pattawee-pp.com/GTAPViz/reference/auto_gtap_rd.md).
Supports automatic processing of macro variables, bilateral trade data,
and regular variables with flexible filtering and output options.

## Usage

``` r
auto_gtap(
  experiment,
  input_path = NULL,
  input_format = ".sl4",
  input_suffix = "",
  process_vars = NULL,
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
  main_output_name = "plot.data",
  macro_output_name = "GTAPMacro",
  keep_raw_data = TRUE
)
```

## Arguments

- experiment:

  Character vector. Case names to process (without file extension).
  Example: `c("EXP1", "EXP2", "EXP3")`.

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

  Character vector, data frame, `NULL`, or `FALSE`. Variables to
  extract:

  - Character vector: Specific variable names to extract

  - Data frame: Must contain a `"Variable"` column with variable names

  - `NULL`: Extract all available variables (default)

  - `FALSE`: Skip variable processing

  Special keywords: `"macro"`, `"macros"`, `"gtapmacro"` trigger macro
  processing.

- mapping_info:

  Character. Metadata mode for variable descriptions and units. Options:

  - `"GTAPv7"`: Use default GTAPv7 definitions (default)

  - `"Yes"`: Use external mapping only

  - `"No"`: No mapping information

  - `"Mix"`: Combine GTAPv7 defaults with external mapping (external
    takes precedence)

  See
  [`add_mapping_info`](https://www.pattawee-pp.com/GTAPViz/reference/add_mapping_info.md)
  for details.

- external_mapping_info:

  Data frame or `NULL`. Custom mapping table with columns: `"Variable"`,
  `"Description"`, `"Unit"`. Overrides default mappings when
  `mapping_info = "Yes"` or `"Mix"`. Default: `NULL`.

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

  Default: `NULL` (no conversion). See
  [`convert_units`](https://www.pattawee-pp.com/GTAPViz/reference/convert_units.md)
  for details.

- decimals:

  Integer or `NULL`. Number of decimal places for rounding numeric
  values. Set to `NULL` to disable rounding. Default: `4`.

- rename_columns:

  Logical. If `TRUE`, renames GTAP dimension codes to readable names:

  - `"REG"` → `"Region"`

  - `"COMM"` → `"Commodity"`

  - `"ACTS"` → `"Activity"`

  - `"Experiment"` → `"Scenario"`

  Default: `TRUE`.

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

- keep_raw_data:

  Logical. Reserved parameter for future compatibility with comparison
  features. Currently not used. Default: `TRUE`.

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

**Automatic Processing:**

The function automatically processes different data types based on file
format and parameters:

- **Macro variables**: Automatically processed from `.sl4` files when
  `process_vars = NULL` or includes `"macro"`, `"macros"`, or
  `"gtapmacro"`

- **QXS (bilateral trade)**: Automatically processed from `.sl4` files
  when `process_vars = NULL` or includes variables with `"qxs"` in the
  name

- **Regular variables**: Processed based on `extract_method` and
  `process_vars`

**File Discovery:**

The function automatically discovers available input files and reports
missing files. Files must match the pattern:
`experiment_name + input_suffix + input_format`

**Output Structure:**

All outputs include standardized columns:

- `Variable`: Variable identifier

- `Value`: Numeric data values

- `Experiment` or `Scenario`: Case identifier (depends on
  `rename_columns`)

- `Unit`: Variable units (required for GTAPViz plotting functions)

- `Description`: Variable description (if mapping is enabled)

- Additional dimension columns (REG, COMM, ACTS, etc.)

**Unit Conversion:**

When `convert_unit` is specified, the conversion is applied to all
processed data (main data, macro data, and bilateral trade data). The
`Unit` column is automatically updated to reflect the conversion.

**Differences from auto_gtap_data:**

- Processes single format per call (not simultaneous SL4 + HAR)

- Simplified parameter structure matching `auto_gtap_rd`

- Uses `experiment` vector instead of complex experiment lists

- Automatic output assignment to parent environment

## See also

[`auto_gtap_data`](https://www.pattawee-pp.com/GTAPViz/reference/auto_gtap_data.md)
for multi-format processing,
[`auto_gtap_rd`](https://www.pattawee-pp.com/GTAPViz/reference/auto_gtap_rd.md)
for recursive dynamic data processing,
[`add_mapping_info`](https://www.pattawee-pp.com/GTAPViz/reference/add_mapping_info.md)
for variable metadata,
[`convert_units`](https://www.pattawee-pp.com/GTAPViz/reference/convert_units.md)
for unit conversions

## Author

Pattawee Puangchit

## Examples

``` r
# Input Path:
input_path <- system.file("extdata/in", package = "GTAPViz")

# Example 1: Process all variables from SL4 files (macros + regular + QXS)
# Files: EXP1.sl4, EXP2.sl4
gtap_data <- auto_gtap(
  experiment = c("EXP1", "EXP2"),
  input_path = input_path,
  input_format = ".sl4",
  process_vars = NULL,
  mapping_info = "GTAPv7",
  subtotal_level = FALSE,
  main_output_name = "plot.data"
)
#> Processing GTAP Macro Data
#> Processing SL4 Data
#> Processing QXS Bilateral Data
#> GTAP Macro Data processed successfully
#> SL4 Data processed successfully
#> QXS Bilateral Data processed successfully
#> GTAP static data processing completed successfully!
```
