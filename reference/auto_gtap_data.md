# Process GTAP Data Automation with Flexible Output Options

Processes GTAP data from `sl4` and `har` files with options for
exporting and preparing plot-ready data.

## Usage

``` r
auto_gtap_data(
  experiment,
  input_path = NULL,
  output_path = NULL,
  sl4_suffix = "",
  har_suffix = "",
  process_sl4_vars = NULL,
  process_har_vars = NULL,
  mapping_info = "GTAPv7",
  sl4_mapping_info = NULL,
  har_mapping_info = NULL,
  sl4_extract_method = "get_data_by_dims",
  har_extract_method = "get_data_by_var",
  sl4_priority = NULL,
  har_priority = NULL,
  sl4_convert_unit = NULL,
  har_convert_unit = NULL,
  decimals = 4,
  rename_columns = TRUE,
  region_select = NULL,
  sector_select = NULL,
  subtotal_level = FALSE,
  plot_data = TRUE,
  output_formats = NULL,
  sl4_output_name = "sl4.plot.data",
  har_output_name = "har.plot.data",
  macro_output_name = "GTAPMacro",
  add_scenario_ranking = FALSE,
  rank_column = "ScenarioRank"
)
```

## Arguments

- experiment:

  Character vector. Case names to process.

- input_path:

  Character. Path to the input folder.

- output_path:

  Character. Path to the output folder.

- sl4_suffix:

  Character. Custom suffix for SL4 files (e.g., `""`, `"-custom"`).

- har_suffix:

  Character. Custom suffix for HAR files (e.g., `"-WEL"`).

- process_sl4_vars:

  Character, `NULL`, or `FALSE`. Variables to extract from SL4 data:

  - Character vector: Specific variable names.

  - `NULL`: Extract all.

  - `FALSE`: Skip SL4 processing.

- process_har_vars:

  Character, `NULL`, or `FALSE`. Variables to extract from HAR data:

  - Character vector: Specific variable names.

  - `NULL`: Extract all.

  - `FALSE`: Skip HAR processing.

- mapping_info:

  Character. Metadata mode for variable descriptions and units. Options:
  `"GTAPv7"` (default), `"Yes"`, `"No"`, `"Mix"`. See
  [`add_mapping_info()`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md)
  for full details.

- sl4_mapping_info:

  Data frame or `NULL`. Mapping for SL4 variables. Must include columns
  `"Variable"`, `"Description"`, and `"Unit"`.

- har_mapping_info:

  Data frame or `NULL`. Mapping for HAR variables, structured as above.

- sl4_extract_method:

  Character. SL4 extraction method: `"get_data_by_dims"`,
  `"get_data_by_var"`, or `"group_data_by_dims"`.

- har_extract_method:

  Character. HAR extraction method. Same options as above.

- sl4_priority:

  Optional list. Required only when `sl4_extract_method` is
  `"group_data_by_dims"`. Specifies priority rules for SL4 data
  grouping.

- har_priority:

  Optional list. Required only when `har_extract_method` is
  `"group_data_by_dims"`. Specifies priority rules for HAR data
  grouping.

- sl4_convert_unit:

  Character or `NULL`. Optional SL4 unit conversion. Valid options:
  `"mil2bil"`, `"bil2mil"`, `"pct2frac"`, `"frac2pct"`. See
  [`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md)
  for details.

- har_convert_unit:

  Character or `NULL`. Optional HAR unit conversion. Same options as
  above.

- decimals:

  Integer or `NULL`. Number of decimal places to round numeric values.
  Set to `NULL` to disable rounding.

- rename_columns:

  Logical. If `TRUE` (default), renames GTAP codes (e.g., `"REG"` →
  `"Region"`, `"COMM"` → `"Commodity"`).

- region_select:

  Optional character vector. Filters data to selected regions. Applies
  only to the `"REG"` column, which is fixed and cannot be modified.

- sector_select:

  Optional character vector. Filters data to selected sectors. Applies
  only to the `"ACTS"` and `"COMM"` columns, which are fixed and cannot
  be modified.

- subtotal_level:

  Logical. If `TRUE`, includes subtotal rows. Default is `FALSE`.

- plot_data:

  Logical. If `TRUE`, generates plot-ready data and assigns to specified
  variable names.

- output_formats:

  Character vector or list. Output formats for export. Valid values:
  `"csv"`, `"stata"`, `"rds"`, `"txt"`.

- sl4_output_name:

  Character. Variable name to assign SL4 output. Default:
  `"sl4.plot.data"`.

- har_output_name:

  Character. Variable name to assign HAR output. Default:
  `"har.plot.data"`.

- macro_output_name:

  Character. Variable name to assign macro output. Default:
  `"GTAPMacro"`.

- add_scenario_ranking:

  Logical or `"merged"`. Adds a numeric index for each scenario:

  - `TRUE`: Adds a ranking column.

  - `"merged"`: Also prefixes experiment names with the rank.

- rank_column:

  Character. Name of the ranking column. Default is `"ScenarioRank"`.

## Value

A processed GTAP-formatted dataset with standardized structure and
metadata, ready for analysis or visualization.

## Details

- To prepare data for plotting and generating tables within the GTAPViz
  package, the `"Unit"` column must be included in the output.

- When using the extraction method `"group_data_by_dims"`, the
  corresponding priority list must be defined via the `sl4_priority` or
  `har_priority` argument. See
  [`group_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/group_data_by_dims.html)
  for more details.

## See also

[`add_mapping_info`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md),
[`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md),
[`rename_value`](https://pattawee-pp.com/GTAPViz/reference/rename_value.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Input Path:
input_path <- system.file("extdata/in", package = "GTAPViz")

# GTAP Macro Variables from 2 .sl4 Files named (EXP1, EXP2)
# Note: No need to add .sl4 to the experiment name
gtap_data <- auto_gtap_data(experiment = c("EXP1", "EXP2"),
                            har_suffix = "-WEL",
                            input_path = input_path, subtotal_level = FALSE,
                            process_sl4_vars = NULL, process_har_vars = NULL,
                            mapping_info = "GTAPv7", plot_data = TRUE)
#> All 2 requested experiment files are found with both SL4 and HAR data.
#> Mapping method used: GTAPv7
#> Processing GTAP Macro Data
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP1.sl4: subscript out of bounds
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP2.sl4: subscript out of bounds
#> Processing SL4 Data
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP1.sl4: subscript out of bounds
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP2.sl4: subscript out of bounds
#> Processing QXS Bilateral Trade Data
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP1.sl4: subscript out of bounds
#> Error processing /home/runner/work/_temp/Library/GTAPViz/extdata/in/EXP2.sl4: subscript out of bounds
#> Processing HAR Data
#> 
#> Summary of Processing:
#> HAR Data processed successfully
#> GTAP data processing completed successfully!
#> 
```
