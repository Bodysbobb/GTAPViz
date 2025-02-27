% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_manipulation.R
\name{add_mapping_info}
\alias{add_mapping_info}
\title{Add Mapping Information}
\usage{
add_mapping_info(
  data_list,
  external_map = NULL,
  mapping = "GTAPv7",
  description_info = TRUE,
  unit_info = TRUE
)
}
\arguments{
\item{data_list}{A list or data frame containing GTAP variables.}

\item{external_map}{\strong{Optional}. A data frame containing external mapping
information with columns \code{"Variable"}, \code{"Description"}, and \code{"Unit"}. If \code{NULL},
the default GTAPv7 mapping is used.}

\item{mapping}{\strong{Optional}. Either \code{"GTAPv7"}, \code{"No"}, \code{"Yes"}, or \code{"Mix"}.
Determines the mapping approach. Default is \code{"GTAPv7"}.}

\item{description_info}{\strong{Optional}. Logical. If \code{TRUE}, adds description information. Default is \code{TRUE}.}

\item{unit_info}{\strong{Optional}. Logical. If \code{TRUE}, adds unit information. Default is \code{TRUE}.}
}
\value{
A modified list or data frame with added description and/or unit information.
}
\description{
The description and unit are based on GTAPv7 model variables.
Full list refer to https://www.gtap.agecon.purdue.edu/models/setsvariables.asp.
If any variable is added apart from the default GTAPv7 model, the information will be left blank.
}
\details{
For the "E1" header in HAR files, if using GTAPv7 or Mix mapping mode, it renames the "FORM" column to "Unit".
When using "Yes" mapping mode with an external map, it will use provided Description and Unit values.
}
\examples{
\dontrun{
sl4_data1 <- HARplus::load_sl4x(system.file("extdata", "TAR10.sl4",
package = "HARplus"))
# Add only Description
result <- add_mapping_info(sl4_data1, mapping = "GTAPv7", unit_info = FALSE)
# Add only Unit
result <- add_mapping_info(sl4_data1, mapping = "GTAPv7",
description_info = FALSE)
custom_mapping <- data.frame(Variable = c("qgdp", "EV"),
                             Description = c("Real GDP", "Economic Value"),
                             Unit = c("Percent", "million USD"))
result <- add_mapping_info(sl4_data1, external_map = custom_mapping,
mapping = "Mix")
}
}
\author{
Pattawee Puangchit
}
