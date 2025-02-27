% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table.R
\name{report_table}
\alias{report_table}
\title{Create Report Table}
\usage{
report_table(
  data,
  vars,
  x_axis_from,
  output_dir = NULL,
  workbook_name = NULL,
  sheet_name = "Results",
  col_names = NULL
)
}
\arguments{
\item{data}{A data frame or list containing the dataset. If a list, it is converted into a
data frame before processing.}

\item{vars}{A character vector specifying the variables to include in the report.}

\item{x_axis_from}{A character string specifying the column used for grouping (e.g., \code{"REG"}, \code{"COMM"}).}

\item{output_dir}{Optional character string specifying the directory where the report
will be saved as an Excel file. If \code{NULL}, the table is not exported.}

\item{workbook_name}{Optional character string for the Excel workbook name. Default is \code{"Report_table"}.}

\item{sheet_name}{Optional character string for the worksheet name. Default is \code{"Results"}.}

\item{col_names}{Optional data frame or character vector for renaming columns. If a data
frame, it must contain \code{"Variable"} and \code{"PlotTitle"} columns to map variable names
to custom titles. If a character vector, it must match the length of \code{vars}.}
}
\value{
A formatted data frame in wide format with grouped variables as columns and
experimental groups as rows. If \code{output_dir} is provided, the function also saves
the output to an Excel file.
}
\description{
Generates a structured report table from a given dataset, allowing for flexible
formatting and customization of column names. The function reshapes the data to present
variables in a wide format with experimental groups as rows.
}
\details{
\itemize{
\item The function checks for the required columns \code{"Experiment"}, \code{"Variable"}, \code{"Value"},
and the specified \code{x_axis_from} column.
\item The table is formatted to ensure numerical values are rounded to two decimal places.
\item If \code{output_dir} is provided, the table is exported as an Excel file with enhanced formatting.
\item If column renaming (\code{col_names}) is specified, it is applied to improve readability.
}
}
\examples{
\dontrun{
# Example dataset
data <- data.frame(
  Experiment = rep(c("Baseline", "Policy"), each = 3),
  REG = rep(c("USA", "CHN", "EU"), 2),
  Variable = rep(c("GDP", "Consumption", "Investment"), 2),
  Value = c(100, 50, 30, 105, 55, 35)
)

# Generate report table
report <- report_table(data, vars = c("GDP", "Consumption"),
                       x_axis_from = "REG", output_dir = "results")

# Custom column names
col_names_df <- data.frame(
  Variable = c("GDP", "Consumption"),
  PlotTitle = c("Gross Domestic Product", "Household Consumption")
)

report_table(data, vars = c("GDP", "Consumption"), x_axis_from = "REG",
             output_dir = "results", col_names = col_names_df)
}

}
\seealso{
\code{\link{scalar_table}}, \code{\link{decomp_table}}
}
\author{
Pattawee Puangchit
}
