# Generate a Structured Report Table

Transforms multiple datasets into wide-format tables based on defined
pivot columns, hierarchical grouping, and renaming rules. Supports
optional subtotal filtering and exporting to Excel.

## Usage

``` r
report_table(
  data_list,
  pivot_col,
  total_column = FALSE,
  export_table = FALSE,
  separate_file = FALSE,
  output_path = NULL,
  sheet_names = NULL,
  include_units = FALSE,
  component_exclude = NULL,
  group_by = NULL,
  rename_cols = NULL,
  var_name_by_description = TRUE,
  add_var_info = FALSE,
  decimal = 2,
  unit_select = NULL,
  separate_sheet_by = NULL,
  subtotal_level = FALSE,
  repeat_label = FALSE,
  workbook_name = "detail_results",
  add_group_line = FALSE
)
```

## Arguments

- data_list:

  A named list of data frames to process.

- pivot_col:

  A named list specifying the column to pivot into a wide format for
  each dataset. Each dataset can have only one pivot column. Example:
  `pivot_col = list(A = "COLUMN", E1 = "PRICES")`

- total_column:

  Logical. If `TRUE`, adds a "Total" column summing numeric values.

- export_table:

  Logical. If `TRUE`, saves the output as an Excel file.

- separate_file:

  Logical. If `TRUE`, saves each dataset as a separate Excel file.

- output_path:

  Character. Directory for saving Excel files when
  `export_table = TRUE`.

- sheet_names:

  Optional named list for custom sheet names.

- include_units:

  Logical. If `TRUE`, includes "Unit" as a grouping column if
  applicable.

- component_exclude:

  Optional character vector specifying pivoted values to exclude.

- group_by:

  A named list defining hierarchical grouping for each dataset. The
  order of columns in each list determines the priority. Example:
  `group_by = list(A = list("Experiment", "REG"), E1 = list("Experiment", "REG", "COMM"))`

- rename_cols:

  A named list for renaming columns across **all** datasets. Example:
  `rename_cols = list("REG" = "Region", "COMM" = "Commodities", "Experiment" = "Scenario")`

- var_name_by_description:

  Logical. If `TRUE`, replaces variable codes with descriptions when
  available.

- add_var_info:

  Logical. If `TRUE`, appends variable codes in parentheses after
  descriptions.

- decimal:

  Numeric. Number of decimal places for rounding values.

- unit_select:

  Optional character. Specifies a unit to filter the dataset.

- separate_sheet_by:

  Optional column name to split sheets in Excel. If defined, each unique
  value in the specified column gets its own sheet. Example:
  `separate_sheet_by = "Scenario"`.

- subtotal_level:

  Logical. If `TRUE`, includes all subtotal values; otherwise, keeps
  only `TOTAL` rows.

- repeat_label:

  Logical. If `TRUE`, repeats the first group column in exports for
  clarity.

- workbook_name:

  Character. Name of the Excel workbook (without extension).

- add_group_line:

  Logical. If `TRUE`, adds a thin line after each group in the exported
  table.

## Value

If `export_table = TRUE`, tables are saved as Excel files.

## Details

This function requires a data list and can generate multiple output
tables in a single setup. That is, all data frames within the list can
be processed simultaneously. See the example for how to generate two
data frames at once from the data list `sl4.plot.data`, which is
obtained via `auto_gtap_data(plot_data = TRUE)`.

## See also

[`add_mapping_info`](https://pattawee-pp.com/GTAPViz/reference/add_mapping_info.md),
[`convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md),
[`rename_value`](https://pattawee-pp.com/GTAPViz/reference/rename_value.md),
[`pivot_table_with_filter`](https://pattawee-pp.com/GTAPViz/reference/pivot_table_with_filter.md)

## Author

Pattawee Puangchit

## Examples

``` r
# \donttest{
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

report_table(
  data_list = sl4.plot.data,

  # === Table Structure ===
  pivot_col = list(
    REG = "Variable",
    "COMM*REG" = "Commodity"
  ),
  group_by = list(
    REG = list("Experiment", "Region"),
    "COMM*REG" = list("Experiment", "Variable", "Region")
  ),
  rename_cols = list("Experiment" = "Scenario"),

  # === Table Layout & Labels ===
  total_column = FALSE,
  decimal = 4,
  subtotal_level = FALSE,
  repeat_label = FALSE,
  include_units = TRUE,
  var_name_by_description = TRUE,
  add_var_info = TRUE,
  add_group_line = FALSE,

  # === Export Options ===
  separate_sheet_by = "Unit",
  export_table = FALSE,
  output_path = NULL,
  separate_file = FALSE,
  workbook_name = "Comparison Table Default"
  )
# }
```
