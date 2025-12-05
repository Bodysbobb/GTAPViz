# Create an Export Configuration

Creates a configuration list for controlling plot export settings. This
function provides auto-completion for export options.

## Usage

``` r
create_export_config(
  file_name = NULL,
  width = NULL,
  height = NULL,
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)
```

## Arguments

- file_name:

  Character. Base name for exported files. Default: "gtap_plots".

- width:

  Numeric. Width of output in inches. Default: NULL (auto-calculated).

- height:

  Numeric. Height of output in inches. Default: NULL (auto-calculated).

- dpi:

  Numeric. Resolution for PNG export. Default: 300.

- bg:

  Character. Background color. Default: "white".

- limitsize:

  Logical. Whether to limit size. Default: FALSE.

## Value

A list with export configuration parameters.

## Author

Pattawee Puangchit

## Examples

``` r
# Default export configuration
default_export <- create_export_config()

# Custom export configuration
custom_export <- create_export_config(
  file_name = "regional_impacts",
  width = 12,
  height = 8,
  dpi = 600
)
```
