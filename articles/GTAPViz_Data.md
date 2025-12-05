# Project Setup and Data Extraction

This vignette outlines the complete project setup process and
demonstrates the use of `auto_gtap_data`.

## Package Overview

This package streamlines the creation of figures and tables from
**.HAR** and **.sl4** results, making academic presentations effortless.
Some key features are:

- **Effortless Multi-Plot Generation** – Automatically adjusts
  dimensions, facets, and layout with minimal input.

- **Smart Plot Adjustments** – Fine-tune visuals easily without manual
  sizing or layout tweaks.

- **Dual Export Plot Formats** – Instantly save high-resolution PNG and
  PDF outputs for slides, papers, and LaTeX.

- **Publication-Ready Pivot Tables** – Generate clean tables alongside
  figures—ideal for academic papers.

- **Streamlined Styling** – Customize colors, fonts, and legends through
  simple, flexible options.

- **Powerful Yet Simple** – Built on `ggplot2`, with intuitive
  `TRUE/FALSE` switches—no advanced coding needed.

- **Self-Contained Help** – Includes a detailed vignette and internal
  help—no need to search online.

💡 Tip for Non-GTAP Users

If you’re an advanced R user or not working directly with GTAP data,
this package still works for **you**.

However, data preparation may require custom manipulation before using
`GTAPViz` plotting and table functions.

For working with `.sl4` and `.har` files, refer to my companion package:
[**HARplus**](https://github.com/Bodysbobb/HARplus).

  
Before proceeding, ensure that `GTAPViz` is installed and loaded:

``` r
if (!requireNamespace("GTAPViz", quietly = TRUE)) {
  install.packages("GTAPViz")
}

library(GTAPViz)
```

### Plot Types

`GTAPViz` provides four main plotting functions, generating over 10 plot
types:

1.  [`comparison_plot`](#sec:comparisonplot): Compares variables across
    multiple experiments for selected observations from a specific
    dimension (e.g., region, sector).  
    Examples: `qgdp`, `ppriv`, `EV`, `GTAP Macros`, and more.

2.  [`detail_plot`](#sec:detailplot): Shows a variable across one or two
    dimensions for each experiment.  
    Examples: `qo`, `qxw`, `qmw`, etc.

3.  [`stack_plot`](#sec:stackplot): Visualizes the composition of a
    variable for decomposition analysis—e.g., `EV` decomposition.

💡 Tip: Plot Catalog

Not sure which plot to use or what it looks like?  
Browse the full [Plot
Catalog](https://pattawee.shinyapps.io/gtapviz-advanced-plot-configs/)
for examples and use cases.

------------------------------------------------------------------------

### Tables

`GTAPViz` includes powerful functions for generating structured pivot
tables and Excel-ready tables with **interactive filters**, ideal for
academic presentations and reports.

Explore the full [Table
Catalog](https://pattawee.shinyapps.io/gtapviz-advanced-table-configs/)
for examples.

## Project Directory

To use `GTAPViz` efficiently—with little to no manual adjustment—I
highly recommend setting up your project directory as follows:

``` r
project.folder <- "your/folder/path"

# Optional: You might not need to adjust this
input.folder <- paste0(project.folder, "/in") 
map.folder <- paste0(project.folder, "/map")      
output.folder <- paste0(project.folder, "/out")   
```

``` monospace
📂 project.folder/ 
├── 📂 in/  # Stores all input files
├── 📂 map/ # Stores the mapping file
├── 📂 out/
```

Example of Project Folder

### Input Folder

All `.sl4` and `.har` input files must be stored in the same folder—by
default, `<input.folder>`, which refers to the `/in` directory within
your project folder.

Below is an example of the expected input folder structure:

``` monospace
📂 in/ 
├── 📄 TAR10.sl4 
├── 📄 TAR10-WEL.har 
├── 📄 SUBT10.sl4 
├── 📄 SUBT10-WEL.har 
```

Example of Input Folder

### Mapping Folder

The mapping XLSX template is available here:
[OutputMapping.xlsx](https://github.com/Bodysbobb/GTAPViz/tree/main/inst/extdata/map).
It contains three main sheets:

- [SL4File](#sec:main-sheets)

- [HARFile](#sec:main-sheets)

- [FilterData](#sec:filterdata-sheets)

#### SL4File and HARFile Sheets

• **“Variable”** specifies the required variable from each file.

• **“Description”** is optional for defining variables and plot titles.

• **“Unit”** is required for all figure commands.

Below is an example of mapping file:

| Variable | Description          | Unit        |
|----------|----------------------|-------------|
| qgdp     | Real GDP Index       | percent     |
| EV       | Welfare Equivalents  | million USD |
| ppriv    | Consumer Price Index | percent     |
| qo       | Output               |             |
| qxw      |                      |             |

**Note:** The **Description** and **Unit** columns can be left empty if
using [GTAP defaults](#sec:mapping_info).

Example of “SL4File” and “HARFile” Sheet

**Important:** You must manually define both the description and unit
for all **additional variables — non-GTAPv7 variables**.

#### FilterData Sheet

The `FilterData` sheet is **optional** and can alternatively be defined
directly in R. It contains two columns used to filter data from all
loaded data frames:

- **“Region”** – Excludes specified regions by filtering the `"REG"`
  column.

- **“Sector”** – Excludes specified sectors by filtering the `"COMM"`
  and `"ACTS"` columns.

**Caveat:** This option only works with the default column names
`"REG"`, `"COMM"`, and `"ACTS"`.  
For filtering by other columns, you must manually apply filters in R
after importing the data.

Below is an example of the filter data sheet:

| Region   | Sector |
|----------|--------|
| EastAsia |        |
| SEAsia   |        |
| Oceania  |        |

**Note:** You may leave it empty to include all entries or redefine the
order for sorting the output format.

Example of “FilterData” Sheet

## R Environment Configuration

This section configures experiment names, description and unit handling,
and output formats for processing GTAP model results.

### Experiment Names

Define `<experiment>` to specify **input file names**. The experiment
name:

• Appears in plots and is added to the **“Experiment”** column.

• Sorts figures and tables based on the order of `<experiment>`.

The following command processes the files and sorts the output with EXP1
before EXP2:

``` r
experiment <- c("TAR10", "SUBT10")

# Automatically Processing These Inputs in the Input Folder 
# - TAR10.sl4 and TAR10-WEL.har  
# - SUBT10.sl4 and SUBT10-WEL.har  
```

Note

You can include as many experiments (inputs) as needed, but a higher
number will increase processing time.

### Description and Unit

`<mapping_info>` controls how the **“Description”** and **“Unit”**
columns are included in the output:

- `"Yes"` → Uses descriptions and units from the mapping file.

- `"No"` → Excludes `"Description"` and `"Unit"`.

- `"GTAPv7"`→ Applies default definitions and units from *GTAP Model
  Version 7*.

- `"Mix"` → Uses manual values when available; otherwise, applies GTAP
  defaults.

``` r
mapping_info <- "Mix"
```

**Note:** GTAP defaults apply only to variables included in the GTAPv7
model. Any additional variables must be manually defined.

### Output Formats

Select the required output formats (`"Yes"` = export, `"No"` = skip):

- **CSV (`csv.output`)** → `"No"`

- **STATA (`stata.output`)** → `"No"`

- **R (`r.output`)** → `"Yes"`

- **Text (`txt.output`)** → `"Yes"`

You can also choose to export only the organized raw data, with or
without visualization, by setting `plot_data` to `TRUE` or `FALSE`.

The following command exports all formats and generates data for
plotting:

``` r
csv.output <- "YES"    
stata.output <- "YES"  
r.output <- "YES"
txt.output <- "YES"   

plot_data = TRUE

# Convert units (optional)
# Options: "mil2bil", "bil2mil", "pct2frac", "frac2pct" — see details in `?convert_units`
sl4_convert_unit <- c("mil2bil")
har_convert_unit <- c("mil2bil")
```

💡 Tip: Unit Conversion

You can convert result units for ‘sl4’ and ‘har’ independently by using
the following automatic options, see
[`?convert_units`](https://pattawee-pp.com/GTAPViz/reference/convert_units.md):

- `"mil2bil"`: converts million USD to billion USD.
- `"bil2mil"`: converts billion USD to million USD.
- `"pct2frac"`: converts percentages to fractions.
- `"frac2pct"`: converts fractions to percentages.
- `NULL`: No conversion

## R Configuration Summary

In summary, the entire R setup is captured in the following chunk:

``` r
# 1. Project Directory
project.folder <- "your/project/folder"

# 2. Define the Input Names 
experiment <- c("TAR10", "SUBT10")

# 3. Adding Description / Unit (Yes/No/GTAPv7/Mix)
mapping_info <- "Mix"

# 4. Choosing Output: (CSV, STATA, R, TEXT) 
csv.output <- "No"    
stata.output <- "No"  
r.output <- "No"
txt.output <- "No"   

# 5. For Plotting: (TRUE/FALSE)
plot_data = TRUE
```

💡 Tip

Once this process is complete, you can use the same format for future
documents—saving you time and ensuring consistency.

You can simply run the following code to setup subdirectories without
modification if you followed all the previous instructions:

``` r
# Default Subdirectories:
input.folder <- paste0(project.folder, "/in")
map.folder <- paste0(project.folder, "/map")
output.folder <- paste0(project.folder, "/out")

# Default Mapping File:
sl4map <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "SL4File")
harmap <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "HARFile")
filter.map <- readxl::read_xlsx(paste0(map.folder, "/OutputMapping.xlsx"), sheet = "FilterData")

# Filtering Data:
selected_regions <- if(length(filter.map$Region) > 0) filter.map$Region else NULL
selected_sector  <- if(length(filter.map$Sector) > 0) filter.map$Sector else NULL
```

**Note:** If you’re familiar with R and prefer a more flexible directory
structure, you can customize any of these. However, you must also define
them in the function.

## Automated Data Extraction

To streamline figure generation from GTAP results, I developed an
automated data extraction method using the following command:

``` r
auto_gtap_data(
  experiment = experiment,
  process_sl4_vars = sl4map, 
  process_har_vars = harmap,
  mapping_info = mapping_info,
  sl4_mapping_info = sl4map,
  har_mapping_info = harmap,
  sl4_convert_unit ="mil2bil",
  har_convert_unit = "mil2bil",
  region_select = selected_regions,
  sector_select = selected_sector,
  subtotal_level = FALSE,
  rename_columns = TRUE,
  decimals = 4,
  project_path = project.folder,
  plot_data = plot_data,
  output_formats = list(
    "csv" = csv.output,
    "stata" = stata.output,
    "rds" = r.output,
    "txt" = txt.output))
```

W \# Tips

### Manual Mapping Files

You can easily create a filter file (`Filter`) using the following code:

``` r
selected_regions <- c("EastAsia", "SEAsia", "Oceania")
selected_sector  <- NULL
```

You can also manually create a mapping file that replicates the
structure of the `SL4File` and `HARFile` sheets using the following
code:

``` r
mapping_df <- data.frame(
  Variable = c("qgdp", "EV", "ppriv"),
  Description = c("Real GDP Index", "Welfare Equivalents", "Consumer Price Index"),
  Unit = c("Percent", "million USD", "percent"),
  stringsAsFactors = FALSE
)
```

### Sorting Rules

These predefined lists determine the display order of outputs, i.e.,
figures and tables:

- `<experiment>` sorts the column `"Experiment"`, i.e., your input
  files.

- `<selected_region>` sorts the column for defined countries by GTAP;
  the default is `"REG"`.

- `<selected_sectors>` sorts the column for defined sectors by GTAP; the
  default is `"COMM"` and `"ACTS"`.

💡 Tip

To customize sorting for additional columns, see `Sorting Data` in the
`Utilities` manuscript.

For example, this setup with will display the figure as shown below:

- `experiment <- c("TAR10", "SUBT10")`

- `selected_regions <- c("EastAsia", "SEAsia", "Oceania")`

## Sample Data

Sample data used in this vignette is obtained from the GTAPv7 model and
utilizes data from the GTAP 11 database. For more details, refer to the
[GTAP Database
Archive](https://www.gtap.agecon.purdue.edu/databases/archives.asp).
