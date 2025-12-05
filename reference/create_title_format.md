# Create a Title Format Configuration

Creates a configuration list for controlling plot title formatting.
Supports auto-completion for common title format types.

## Usage

``` r
create_title_format(type = "standard", text = "", sep = NULL)
```

## Arguments

- type:

  Character. Title format type:

  - `"standard"`: Default (variable + description + unit)

  - `"prefix"`: Adds text before the automatic title

  - `"suffix"`: Adds text after the automatic title

  - `"full"`: Uses only the specified text as the title

  - `"dynamic"`: Builds a title using column values

- text:

  Character. Text content used for `prefix`, `suffix`, `full`, or a
  template for `dynamic`.

- sep:

  Character. The separator between components (only used in `"prefix"`
  or `"suffix"` mode). Default is `": "`.

## Value

A list with title format configuration parameters.

## Author

Pattawee Puangchit

## Examples

``` r
# Standard auto-generated title
standard_title <- create_title_format()

# Prefix title
prefix_title <- create_title_format(
  type = "prefix",
  text = "Impact on",
  sep = " "
)

# Dynamic title using column values
dynamic_title <- create_title_format(
  type = "dynamic",
  text = "Impact on {Variable} in {Region}"
)
```
