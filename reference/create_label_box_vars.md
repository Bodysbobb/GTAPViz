# Create Label Box Variables Configuration

Defines which variables to display in label boxes next to country flags.

## Usage

``` r
create_label_box_vars(
  variable,
  label = NULL,
  symbol = NULL,
  decimal_places = 1
)
```

## Arguments

- variable:

  Character vector. Variable codes to display

- label:

  Character vector or NULL. Custom labels. If NULL, uses variable codes

- symbol:

  Character vector or NULL. Symbols to prefix each value. Auto-assigned
  if NULL

- decimal_places:

  Numeric vector. Decimal places for each variable. Default: 1

## Value

Data frame with label box configuration

## Author

Pattawee Puangchit

## Examples

``` r
label_vars <- create_label_box_vars(
  variable = c("EV", "qgdp", "qpriv"),
  label = c("Welfare", "GDP", "Consumption"),
  decimal_places = c(2, 1, 1)
)
```
