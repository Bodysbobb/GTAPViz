#' @import ggplot2
#' @importFrom stats as.formula setNames
#' @importFrom methods as
#' @importFrom dplyr where
#' @importFrom utils globalVariables capture.output str tail
#' @importFrom stringdist stringdist
#' @importFrom grDevices col2rgb rgb
#' @importFrom utils flush.console modifyList
# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".data", "Experiment", "Group", "Label", "NegativeTotal",
  "PositiveTotal", "REG", "Total", "Unit", "Value", "Variable",
  "csv.output", "r.output", "stata.output", "txt.output",
  "value_category", "data", "tail"
))


#' @title Coalesce Two Values
#'
#' @description
#' Returns the first non-NULL value from the given inputs.
#'
#' @param x First value to check.
#' @param y Fallback value if `x` is NULL.
#'
#' @return The first non-NULL value. If `x` is NULL, returns `y`.
#'
#' @keywords internal
#' @noMd
.coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

