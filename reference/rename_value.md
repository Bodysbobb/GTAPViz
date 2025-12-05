# Rename Values in a Column

Replaces specific values in a column based on a provided mapping file.
Supports renaming across nested data structures and preserves factor
levels.

## Usage

``` r
rename_value(data, column_name = NULL, mapping.file)
```

## Arguments

- data:

  Data structure (data frame, list, or nested combination).

- column_name:

  Character. Column to modify. If \`NULL\`, the function extracts it
  from \`mapping.file\`.

- mapping.file:

  Data frame with \`"OldName"\` and \`"NewName"\` columns for renaming.

## Value

The same data structure with specified values replaced.

## See also

[`add_mapping_info`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md),
[`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md),
[`sort_plot_data`](https://pattawee-pp.com/GTAPViz/reference/sort_plot_data.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))

# Rename variables in a dataset
mapping_welfare <- data.frame(
  ColumnName = "COLUMN",
  OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
  NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
  stringsAsFactors = FALSE
)

har.plot.data <- rename_value(har.plot.data, mapping.file = mapping_welfare)
```
