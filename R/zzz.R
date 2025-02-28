#' @import ggplot2
#' @importFrom stats as.formula setNames
#' @importFrom methods as
#' @importFrom dplyr where
#' @importFrom utils globalVariables
# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".data", "Experiment", "Group", "Label", "NegativeTotal",
  "PositiveTotal", "REG", "Total", "Unit", "Value", "Variable",
  "csv.output", "r.output", "stata.output", "txt.output",
  "value_category"
))
