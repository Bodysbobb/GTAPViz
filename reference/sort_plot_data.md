# Sort GTAP Plot Data

Sorts data frames in a GTAP plot list structure based on specified
column orders. Works with data frames, lists of data frames, or nested
data structures.

## Usage

``` r
sort_plot_data(
  data,
  sort_columns = NULL,
  sort_by_value_desc = NULL,
  convert_to_factor = TRUE
)
```

## Arguments

- data:

  A data frame or list structure containing data to be sorted.

- sort_columns:

  Named list. Specifies columns to sort by and their ordering. Each
  element should be a character vector of values in desired order. For
  example, `list(Region = c("USA", "EU", "CHN")`,
  `Experiment = c("Base", "Shock1", "Shock2"))`.

- sort_by_value_desc:

  Logical or NULL. Controls sorting by the "Value" column: - NULL
  (default): Don't sort by value, only use column-based sorting. - TRUE:
  After column-based sorting, sort by value in descending order. -
  FALSE: After column-based sorting, sort by value in ascending order.

- convert_to_factor:

  Logical. Whether to convert sorted columns to factors with custom
  ordering. Default is TRUE, which preserves ordering in GTAP plotting
  functions.

## Value

A data structure with the same form as the input, with all contained
data frames sorted.

## See also

[`add_mapping_info`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md),
[`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md),
[`rename_value`](https://pattawee-pp.com/GTAPViz/reference/rename_value.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

# Creating Sorting Rule
sorting_specs <- list(
  Experiment = c("EXP2", "EXP1"),     # Show EXP2 first, then EXP1
  Region = c("EastAsia", "SEAsia", "Oceania")  # Custom region order
)

# Sorting
sort_data <- sort_plot_data(sl4.plot.data, sort_columns = sorting_specs,
                            sort_by_value_desc = FALSE)
```
