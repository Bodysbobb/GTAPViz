#' @import ggplot2
#' @importFrom stats as.formula setNames median
#' @importFrom methods as
#' @importFrom dplyr where
#' @importFrom utils globalVariables capture.output str tail
#' @importFrom stringdist stringdist
#' @importFrom grDevices col2rgb rgb
#' @importFrom utils flush.console modifyList
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom viridisLite viridis
utils::globalVariables(c(
  ".data", "Experiment", "Group", "Label", "NegativeTotal",
  "PositiveTotal", "REG", "Total", "Unit", "Value", "Variable",
  "csv.output", "r.output", "stata.output", "txt.output",
  "value_category", "data", "tail",
  "point_x", "point_y", "flag_x", "flag_y", "flag",
  "name_x", "name_y", "name", "text_x", "text_y",
  "text_label", "text_hjust", "iso_a3", "continent"
))

#' @keywords internal
#' @noRd
.coalesce <- function(x, y) {
  if (is.null(x)) y else x
}
