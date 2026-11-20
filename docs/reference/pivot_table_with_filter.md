# Export Data as an Excel Pivot Table

Exports a dataset to an Excel file with both raw data and a generated
pivot table.

## Usage

``` r
pivot_table_with_filter(
  data,
  filter = NULL,
  rows = NULL,
  cols = NULL,
  data_fields = "Value",
  raw_sheet_name = "RawData",
  pivot_sheet_name = "PivotTable",
  dims = "A4",
  export_table = FALSE,
  output_path = NULL,
  workbook_name = "GTAP_PivotTable.xlsx"
)
```

## Arguments

- data:

  Data frame. The dataset to be exported.

- filter:

  Character vector (optional). Columns to be used as filter fields in
  the pivot table.

- rows:

  Character vector (optional). Columns to be used as row fields in the
  pivot table.

- cols:

  Character vector (optional). Columns to be used as column fields in
  the pivot table.

- data_fields:

  Character. The data field(s) to be summarized in the pivot table
  (default: `"Value"`).

- raw_sheet_name:

  Character. Name of the sheet containing raw data (default:
  `"RawData"`).

- pivot_sheet_name:

  Character. Name of the sheet containing the pivot table (default:
  `"PivotTable"`).

- dims:

  Character. Cell reference where the pivot table starts (default:
  `"A3"`).

- export_table:

  Logical. Whether to save the Excel file (default: `TRUE`).

- output_path:

  Character. Directory where the file should be saved (default: current
  working directory).

- workbook_name:

  Character. Name of the output Excel file (default:
  `"GTAP_PivotTable.xlsx"`).

## Value

An excel workbook object containing both raw data and the pivot table.

## Details

This function creates an Excel workbook with:

- A raw data sheet (`raw_sheet_name`) containing the provided dataset.

- A pivot table sheet (`pivot_sheet_name`) generated based on specified
  row, column, and data fields.

If `export = TRUE`, the function saves the workbook to the specified
`output_path`.

## Author

Pattawee Puangchit

## Examples

``` r
# \donttest{
# Load Data:
input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))

data_pivot_table <- sl4.plot.data[["REG"]]

# Generate Pivot Table with Filter
# Only use columns that exist in the data
pivot_table_with_filter(

  # === Input & Filter Settings ===
  data = data_pivot_table,
  filter = c("Variable", "Unit"),  # Allow filtering by variable type and unit

  # === Pivot Structure ===
  rows = c("Region"),             # Rows: Regions (removed "Sector" which doesn't exist)
  cols = c("Experiment"),         # Columns: Experiments
  data_fields = "Value",          # Values to be aggregated

  # === Sheet & Layout ===
  raw_sheet_name = "Raw_Data",         # Sheet name for raw data
  pivot_sheet_name = "Sector_Pivot",   # Sheet name for pivot table
  dims = "A3",                         # Starting cell for pivot table

  # === Export Options ===
  export_table = FALSE,
  output_path = NULL,
  workbook_name = "Sectoral_Impact_Analysis.xlsx"
)
#> A Workbook object.
#>  
#> Worksheets:
#>  Sheets: Raw_Data, Sector_Pivot 
#>  Write order: 1, 2
# }
```
