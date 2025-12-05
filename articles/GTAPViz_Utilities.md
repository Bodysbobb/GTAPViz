# GTAPViz: Utilities

This vignette provides other useful functions in the `GTAPViz` package.

## Adding Unit and Description Columns

If you manually import data or need to adjust the `"Description"` and
`"Unit"` columns, you can use the following command:

``` r
# Create mapping file
mapping_df <- data.frame(
  Variable = c("qgdp", "EV", "ppriv"),
  Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
  Unit = c("Percent", "million USD", "percent"),
  stringsAsFactors = FALSE
)

datasets <- add_mapping_info(mapping_df, 
                             external_map = mapping_df, 
                             mapping = "Yes")
```

## Convert Different Units

You can convert all units across all data frames within the data list by
using the following code:

``` r
your_data <- sl4.plot.data   # Change this to your data list name

your_data <- convert_units(change_unit_from = c("BAHT", "million USD"),
                           change_unit_to = c("USD", "billion USD"),
                           adjustment = c("*34.5", "/1000"))
```

Explanation

**What does this code do?**

- Converts `BAHT` to `USD` by multiplying the values by 34.5.

- Converts `million USD` to `billion USD` by dividing the values by
  1,000.

**Parameters:**

- `change_unit_from`: The original unit name to be changed in the
  `"Unit"` column.  
  If your dataset does not yet include a `"Unit"` column, please refer
  to [adding unit and description columns](#sec:add-info).

- `change_unit_to`: The new unit name to be recorded in the `"Unit"`
  column.

- `adjustment`: The mathematical operation used to transform the
  values.  
  Accepts operators such as `/`, `*`, `+`, or `-`.

## Renaming Columns and Data List Names

To modify column names across all datasets and/or rename data list names
automatically—such as changing `REG` to `Region` or `COMM` to
`Commodity`—use the following command:

``` r
# Creating Mapping File
rename_col <- data.frame(
  old = c("REG", "COMM", "ACTS"),
  new = c("Region", "Commodity", "Activity")
  )

har.plot.data <- HARplus::rename_dims(har.plot.data, 
                                      mapping_df = rename_col,
                                      rename_list_names = FALSE)
```

💡 Tip

You can create this mapping dataframe in Excel and import it into R. The
column names must be **“old”** and **“new”**.

## Sorting Output

This function allows you to sort data before generating plots or
tables.  
For example, you can display `EXP2` before `EXP1`, or `SEAsia` before
`Oceania`.

It supports:

- Sorting by multiple columns simultaneously for precise output control

- Applying consistent sorting across all data frames within a data list

See
[`?sort_plot_data`](https://pattawee-pp.com/GTAPViz/reference/sort_plot_data.md)
for full details.

Key arguments:

- `cols` specifies the column(s) to sort based on a predefined order
  (e.g., in `sl4.plot.data`, `Experiment` and `Region` will follow the
  defined order).

- `sort_by_value_desc` controls whether sorting is in descending order
  (`TRUE/FALSE/NULL`).

To apply sorting, use the following command:

``` r
# Using the function
sort_sl4_plot <- sort_plot_data(
  sl4.plot.data,
  sort_columns = list(
    Experiment = c("EXP2", "EXP1"), # Column Name = Sorting Order
    Region = c("SEAsia", "Oceania")
    ),
  sort_by_value_desc = NULL
)
```

## Renaming Values

To automatically modify values in any column before plotting—such as
renaming country names in the `REG` column (e.g., changing `"USA"` to
`"United States"` or `"CHN"` to `"China"`)—use the following command:

``` r
# Creating Mapping File
rename.region <- data.frame(
  ColumnName = "REG",
  OldName = c("USA", "CHN"),
  NewName = c("United States", "China"),
  stringsAsFactors = FALSE
)

sl4.plot.data_rename <- rename_value(sl4.plot.data, mapping.file = rename.region)
```

  
Another useful example is renaming the components of welfare
decomposition before plotting.

``` r
# Rename Value if needed
wefare.decomp.rename <- data.frame(
  ColumnName = "COLUMN",
  OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
  NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
  stringsAsFactors = FALSE
)

welfare_decomp <- rename_value(har.plot.data, mapping.file = wefare.decomp.rename)
```

## (HARplus) Manual Data Processing

This section is for non-GTAP models or manual data extraction for
plotting. Follow these steps:

1.  Use `"HARplus::load_sl4x"` or `"HARplus::load_harx"`, then
    `"HARplus::group_data_by_dims"` to structure the data in the
    required format.

2.  Filter each dataframe using standard R `data.frame` operations.

3.  Add the “Unit” and “Description” columns using `"add_mapping_info"`
    (the *“Unit”* column is required for plotting).

``` r
# Step 1: Filtering Data using Dataframe and lapply
manual.data <- lapply(sl4data, function(x) {
  if (is.data.frame(x)) {
    x[x$REG %in% selected_regions & x$COMM %in% selected_sector & x$ACTS %in% selected_sector
      & x$Experiment %in% selected_exp , ]
  } else {
    x  
  }
})

# Step 2: Adding Unit and Description Column
manual.data <- add_mapping_info(manual.data, external_map = "/your/mapping.xlsx",
                                description_info = TRUE,
                                unit_info = TRUE)
```

## Sample Data

Sample data used in this vignette is obtained from the GTAPv7 model and
utilizes data from the GTAP 11 database. For more details, refer to the
[GTAP Database
Archive](https://www.gtap.agecon.purdue.edu/databases/archives.asp).
