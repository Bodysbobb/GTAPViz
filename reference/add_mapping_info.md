# Add Mapping Information to GTAP Data

Adds descriptions and unit information to GTAP data based on a specified
mapping mode. Supports external mappings or default GTAPv7 mappings,
allowing users to enrich datasets with standardized metadata.

Adds **description** and **unit** information to GTAP data structures
based on a specified mapping mode. This function supports internal
GTAPv7 mappings, external mappings, or a combination of both.

## Usage

``` r
add_mapping_info(
  data_list,
  external_map = NULL,
  mapping = "GTAPv7",
  description_info = TRUE,
  unit_info = TRUE
)
```

## Arguments

- data_list:

  A list or nested data structure containing GTAP output data frames.

- external_map:

  Optional data frame. External mapping must include columns:
  `"Variable"`, `"Description"`, and `"Unit"`.

- mapping:

  Character. Mapping mode for assigning metadata to variables. Options:

  - `"GTAPv7"`: Use GTAPv7 internal definitions (default).

  - `"Yes"`: Use only the provided `external_map`.

  - `"Mix"`: Use external definitions first, then fallback to GTAPv7 for
    missing values.

  - `"No"`: Skip mapping entirely.

- description_info:

  Logical. If `TRUE`, adds or updates variable descriptions. Default:
  `TRUE`.

- unit_info:

  Logical. If `TRUE`, adds or updates unit information. Default: `TRUE`.

## Value

The same data structure as input with added `"Description"` and `"Unit"`
columns, if applicable.

## Details

The `mapping` argument supports:

## See also

[`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md),
[`rename_value`](https://pattawee-pp.com/GTAPViz/reference/rename_value.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Load GTAP SL4 data
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

# Add mapping using GTAPv7 defaults
gtap_data <- add_mapping_info(sl4.plot.data, mapping = "GTAPv7")

# Use a custom mapping file
my_map <- data.frame(
  Variable = c("qgdp", "EV"),
  Description = c("Real GDP", "Welfare"),
  Unit = c("percent", "million USD")
)
gtap_data <- add_mapping_info(sl4.plot.data, external_map = my_map, mapping = "Mix")
```
