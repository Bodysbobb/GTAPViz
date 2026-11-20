# Create Vertical Line Configuration

Helper function to create vertical line specifications for trend plots.
Makes it easier to add reference lines at specific periods.

## Usage

``` r
create_vline(
  period,
  label = "",
  color = "black",
  linetype = "dashed",
  size = 0.5
)
```

## Arguments

- period:

  Numeric. The period (x-axis value) where the line should appear

- label:

  Character. Text label for the line. Default: ""

- color:

  Character. Color of the line. Default: "black"

- linetype:

  Character. Line type ("solid", "dashed", "dotted", etc.). Default:
  "dashed"

- size:

  Numeric. Line thickness. Default: 0.5

## Value

A list with vertical line configuration

## Author

Pattawee Puangchit

## Examples

``` r
vline1 <- create_vline(period = 2019, label = "Shock 1", color = "red")

vlines <- list(
  create_vline(2019, "Shock 1", "black", "dashed"),
  create_vline(2023, "Shock 2", "blue", "dotted"),
  create_vline(2025, "Shock 3", "darkgreen", "dashed")
)
```
