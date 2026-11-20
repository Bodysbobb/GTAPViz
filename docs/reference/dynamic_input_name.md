# Generate Dynamic Input Mapping for GTAP Experiments

Creates a structured mapping between experiment names, simulation types,
and time periods for dynamic GTAP analysis. Supports flexible naming
with base, base_rerun, and policy scenarios for single or multiple cases
simultaneously.

## Usage

``` r
dynamic_input_name(
  case_name = NULL,
  type = "prefix",
  input_format = NULL,
  pattern = NULL,
  increment = 1,
  separator = "-",
  period_pattern = FALSE,
  period_prefix = "yr_",
  add_scenario_ranking = FALSE,
  rank_column = "ScenarioRank",
  output = "Input_map"
)
```

## Arguments

- case_name:

  Character vector or list. Case names to process. Can be:

  - Single character: `"US_All"`

  - Character vector: `c("US_All", "US_Retail")`

  - Named list: Each element defines a separate case with its own input
    format

- type:

  Character. Position of identifier: `"prefix"` or `"suffix"`.

- input_format:

  Named list or list of named lists. Simulation type definitions:

  - Single format (applies to all cases):  
    `list(base = "base-", base_rerun = "rerun-", policy = "pol-")`

  - Multiple formats (one per case):  
    `list( US_All = list(base = "bwrb-rwrr-", policy = "brtb-rrtr-"), US_Retail = list(base = "base-", policy = "pol-") )`

  When using multiple formats, names must match `case_name` values.

- pattern:

  Character or numeric vector. Sequence pattern for periods:

  - Range format: `"2020:2030"`

  - Numeric vector: `c(2020, 2025, 2030)`

- increment:

  Numeric. Step size for sequence generation. Default: `1`.

- separator:

  Character. Separator between elements. Default: `"-"`.

- period_pattern:

  Logical. If `TRUE`, creates period ranges (e.g., `"2020-2025"`).
  Default: `FALSE`.

- period_prefix:

  Character. Prefix for period labels. Default: `"yr_"`.

- add_scenario_ranking:

  Logical. If `TRUE`, adds `ScenarioRank` column based on case order in
  `input_format`. Default: `FALSE`.

- rank_column:

  Character. Name of the ranking column. Default: `"ScenarioRank"`.

- output:

  Character. Name of output variable to assign in parent environment.
  Default: `"Input_map"`.

## Value

A data frame with columns:

- Input:

  Generated input file names

- Case:

  Case identifier

- Period:

  Period label with prefix

- PeriodRange:

  Numeric period range

- SimType:

  Simulation type (base, base_rerun, policy, etc.)

- ScenarioRank:

  Numeric rank (only if `add_scenario_ranking = TRUE`)

Invisibly returns the data frame while assigning it to the specified
output variable.

## Details

The function validates that all required simulation types are defined
for each case. Missing simulation types trigger informative error
messages specifying which types are missing for which cases.

When `input_format` is a list of lists, each case can have unique
simulation type definitions, allowing flexible configuration across
multiple scenarios.

When `add_scenario_ranking = TRUE`, the `ScenarioRank` column is added
based on the order of cases in `input_format`. This ranking propagates
through
[`auto_gtap_rd`](https://www.pattawee-pp.com/GTAPViz/reference/auto_gtap_rd.md)
processing.

## Author

Pattawee Puangchit

## Examples

``` r
# Single case with uniform format
dynamic_input_name(
  case_name = "US_All",
  type = "prefix",
  input_format = list(
    base = "bwrb-rwrr-",
    base_rerun = "brtb-rwrr-",
    policy = "brtb-rrtr-"
  ),
  pattern = "2018:2035",
  output = "Input_map"
)

# Multiple cases with scenario ranking
dynamic_input_name(
  case_name = c("US_All", "US_Retail", "US_ReduceTar"),
  type = "prefix",
  input_format = list(
    US_All = list(base_rerun = "bwrb-rwrr-", policy = "bwrb-rwrr-pwrp-"),
    US_Retail = list(base_rerun = "brtb-rtrr-", policy = "brtb-rtrr-prtp-"),
    US_ReduceTar = list(base_rerun = "bfrb-rfrr-", policy = "bfrb-rfrr-pfrp-")
  ),
  pattern = "2020:2030",
  add_scenario_ranking = TRUE,
  output = "Input_map"
)
```
