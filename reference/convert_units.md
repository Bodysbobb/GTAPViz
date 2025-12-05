# Convert Units in GTAP Data

Converts values in a dataset to different units based on predefined
transformations or custom scaling. Supports manual and automatic
conversions for economic and trade-related metrics.

## Usage

``` r
convert_units(
  data,
  change_unit_from = NULL,
  change_unit_to = NULL,
  adjustment = NULL,
  value_col = "Value",
  unit_col = "Unit",
  variable_select = NULL,
  variable_col = "Variable",
  scale_auto = NULL
)
```

## Arguments

- data:

  A data structure (list, data frame, or nested combination).

- change_unit_from:

  Character vector. Units to be converted (case-insensitive).

- change_unit_to:

  Character vector. Target units corresponding to `change_unit_from`.

- adjustment:

  Character or numeric vector. Specifies conversion operations (e.g.,
  `"/1000"` to convert million to billion).

- value_col:

  Character. Column name containing values to adjust (default:
  `"Value"`).

- unit_col:

  Character. Column name containing unit information (default:
  `"Unit"`).

- variable_select:

  Optional character vector. If provided, only these variables are
  converted.

- variable_col:

  Character. Column name containing variable identifiers (default:
  `"Variable"`).

- scale_auto:

  Optional character vector of predefined conversion rules:

  - `"mil2bil"`: Converts million USD to billion USD (divides by 1000).

  - `"bil2mil"`: Converts billion USD to million USD (multiplies by
    1000).

  - `"pct2frac"`: Converts percent to fraction (divides by 100).

  - `"frac2pct"`: Converts fraction to percent (multiplies by 100).

## Value

A data structure with values converted to the specified units.

## Details

If both `change_unit_from` and `scale_auto` are provided, the function
prompts the user to choose between manual and automatic conversion.

## See also

[`add_mapping_info`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md),
[`rename_value`](https://pattawee-pp.com/GTAPViz/reference/rename_value.md),
[`sort_plot_data`](https://pattawee-pp.com/GTAPViz/reference/sort_plot_data.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

# Convert million USD to billion USD
gtap_data <- convert_units(sl4.plot.data,
  change_unit_from = "million USD",
  change_unit_to = "billion USD",
  adjustment = "/1000"
)

# Automatic conversion from percent to fraction
gtap_data <- convert_units(sl4.plot.data, scale_auto = "pct2frac")
#> 60 observations converted to new unit
#> 200 observations converted to new unit
```
