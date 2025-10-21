#' Generate Dynamic Input Mapping for GTAP Experiments
#'
#' Creates a structured mapping between experiment names, simulation types, and time periods
#' for dynamic GTAP analysis. Supports flexible naming with base, base_rerun, and policy scenarios
#' for single or multiple cases simultaneously.
#'
#' @param case_name Character vector or list. Case names to process. Can be:
#'   \itemize{
#'     \item Single character: \code{"US_All"}
#'     \item Character vector: \code{c("US_All", "US_Retail")}
#'     \item Named list: Each element defines a separate case with its own input format
#'   }
#' @param type Character. Position of identifier: \code{"prefix"} or \code{"suffix"}.
#' @param input_format Named list or list of named lists. Simulation type definitions:
#'   \itemize{
#'     \item Single format (applies to all cases): \cr
#'       \code{list(base = "base-", base_rerun = "rerun-", policy = "pol-")}
#'     \item Multiple formats (one per case): \cr
#'       \code{list(
#'         US_All = list(base = "bwrb-rwrr-", policy = "brtb-rrtr-"),
#'         US_Retail = list(base = "base-", policy = "pol-")
#'       )}
#'   }
#'   When using multiple formats, names must match \code{case_name} values.
#' @param pattern Character or numeric vector. Sequence pattern for periods:
#'   \itemize{
#'     \item Range format: \code{"2020:2030"}
#'     \item Numeric vector: \code{c(2020, 2025, 2030)}
#'   }
#' @param increment Numeric. Step size for sequence generation. Default: \code{1}.
#' @param separator Character. Separator between elements. Default: \code{"-"}.
#' @param period_pattern Logical. If \code{TRUE}, creates period ranges (e.g., \code{"2020-2025"}).
#'   Default: \code{FALSE}.
#' @param period_prefix Character. Prefix for period labels. Default: \code{"yr_"}.
#' @param add_scenario_ranking Logical. If \code{TRUE}, adds \code{ScenarioRank} column based on
#'   case order in \code{input_format}. Default: \code{FALSE}.
#' @param rank_column Character. Name of the ranking column. Default: \code{"ScenarioRank"}.
#' @param output Character. Name of output variable to assign in parent environment.
#'   Default: \code{"Input_map"}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Input}{Generated input file names}
#'     \item{Case}{Case identifier}
#'     \item{Period}{Period label with prefix}
#'     \item{PeriodRange}{Numeric period range}
#'     \item{SimType}{Simulation type (base, base_rerun, policy, etc.)}
#'     \item{ScenarioRank}{Numeric rank (only if \code{add_scenario_ranking = TRUE})}
#'   }
#'   Invisibly returns the data frame while assigning it to the specified output variable.
#'
#' @details
#' The function validates that all required simulation types are defined for each case.
#' Missing simulation types trigger informative error messages specifying which types
#' are missing for which cases.
#'
#' When \code{input_format} is a list of lists, each case can have unique simulation
#' type definitions, allowing flexible configuration across multiple scenarios.
#'
#' When \code{add_scenario_ranking = TRUE}, the \code{ScenarioRank} column is added
#' based on the order of cases in \code{input_format}. This ranking propagates through
#' \code{\link{auto_gtap_rd}} processing.
#'
#' @author Pattawee Puangchit
#' @export
#'
#' @examples
#' # Single case with uniform format
#' dynamic_input_name(
#'   case_name = "US_All",
#'   type = "prefix",
#'   input_format = list(
#'     base = "bwrb-rwrr-",
#'     base_rerun = "brtb-rwrr-",
#'     policy = "brtb-rrtr-"
#'   ),
#'   pattern = "2018:2035",
#'   output = "Input_map"
#' )
#'
#' # Multiple cases with scenario ranking
#' dynamic_input_name(
#'   case_name = c("US_All", "US_Retail", "US_ReduceTar"),
#'   type = "prefix",
#'   input_format = list(
#'     US_All = list(base_rerun = "bwrb-rwrr-", policy = "bwrb-rwrr-pwrp-"),
#'     US_Retail = list(base_rerun = "brtb-rtrr-", policy = "brtb-rtrr-prtp-"),
#'     US_ReduceTar = list(base_rerun = "bfrb-rfrr-", policy = "bfrb-rfrr-pfrp-")
#'   ),
#'   pattern = "2020:2030",
#'   add_scenario_ranking = TRUE,
#'   output = "Input_map"
#' )
dynamic_input_name <- function(case_name = NULL,
                               type = "prefix",
                               input_format = NULL,
                               pattern = NULL,
                               increment = 1,
                               separator = "-",
                               period_pattern = FALSE,
                               period_prefix = "yr_",
                               add_scenario_ranking = FALSE,
                               rank_column = "ScenarioRank",
                               output = "Input_map") {

  if (is.null(case_name)) {
    stop("'case_name' must be provided")
  }

  if (is.null(input_format) || !is.list(input_format) || length(input_format) == 0) {
    stop("'input_format' must be a named list with at least one element")
  }

  if (is.null(pattern)) {
    stop("'pattern' must be provided")
  }

  if (!type %in% c("prefix", "suffix")) {
    stop("'type' must be either 'prefix' or 'suffix'")
  }

  case_names <- if (is.list(case_name) && !is.null(names(case_name))) {
    names(case_name)
  } else if (is.character(case_name)) {
    case_name
  } else {
    stop("'case_name' must be a character vector or named list")
  }

  is_nested_format <- any(sapply(input_format, is.list))

  if (is_nested_format) {
    if (is.null(names(input_format)) || any(names(input_format) == "")) {
      stop("When 'input_format' is a list of lists, all elements must be named with case names")
    }

    missing_cases <- setdiff(case_names, names(input_format))
    if (length(missing_cases) > 0) {
      stop("Missing input_format definitions for cases: ", paste(missing_cases, collapse = ", "))
    }

    for (case in case_names) {
      fmt <- input_format[[case]]
      if (!is.list(fmt) || is.null(names(fmt)) || any(names(fmt) == "")) {
        stop("input_format for case '", case, "' must be a named list")
      }
    }

    case_order <- names(input_format)
  } else {
    if (is.null(names(input_format)) || any(names(input_format) == "")) {
      stop("All elements in 'input_format' must be named")
    }
    single_format <- input_format
    input_format <- setNames(
      lapply(case_names, function(x) single_format),
      case_names
    )
    case_order <- case_names
  }

  if (is.character(pattern) && grepl(":", pattern)) {
    pattern_parts <- strsplit(pattern, ":")[[1]]
    if (length(pattern_parts) == 2) {
      start_val <- as.numeric(pattern_parts[1])
      end_val <- as.numeric(pattern_parts[2])
      if (!is.na(start_val) && !is.na(end_val)) {
        pattern_values <- if (start_val <= end_val) {
          seq(start_val, end_val, by = increment)
        } else {
          seq(start_val, end_val, by = -increment)
        }
      } else {
        stop("Invalid pattern format. Pattern should be 'start:end' with numeric values")
      }
    } else {
      stop("Invalid pattern format. Pattern should be 'start:end'")
    }
  } else if (is.numeric(pattern) && length(pattern) == 2) {
    start_val <- pattern[1]
    end_val <- pattern[2]
    pattern_values <- if (start_val <= end_val) {
      seq(start_val, end_val, by = increment)
    } else {
      seq(start_val, end_val, by = -increment)
    }
  } else {
    pattern_values <- pattern
  }

  if (period_pattern && length(pattern_values) > 1) {
    period_values <- character(length(pattern_values) - 1)
    period_ranges <- character(length(pattern_values) - 1)
    for (i in 1:(length(pattern_values) - 1)) {
      period_values[i] <- paste0(period_prefix, pattern_values[i], separator, pattern_values[i + 1])
      period_ranges[i] <- paste(as.character(pattern_values[i]), as.character(pattern_values[i + 1]), sep = separator)
    }
    pattern_values <- period_ranges
  } else {
    period_values <- paste0(period_prefix, pattern_values)
    period_ranges <- as.character(pattern_values)
  }

  case_rank_map <- setNames(seq_along(case_order), case_order)

  all_inputs <- character(0)
  all_cases <- character(0)
  all_periods <- character(0)
  all_period_ranges <- character(0)
  all_sim_types <- character(0)
  all_ranks <- integer(0)

  for (case in case_names) {
    case_format <- input_format[[case]]
    case_rank <- case_rank_map[case]

    for (sim_type in names(case_format)) {
      type_prefixes <- case_format[[sim_type]]

      for (prefix_val in type_prefixes) {
        for (i in seq_along(pattern_values)) {
          if (type == "prefix") {
            input_name <- paste0(prefix_val, pattern_values[i])
          } else {
            input_name <- paste0(pattern_values[i], separator, prefix_val)
          }

          all_inputs <- c(all_inputs, input_name)
          all_cases <- c(all_cases, case)
          all_periods <- c(all_periods, period_values[i])
          all_period_ranges <- c(all_period_ranges, period_ranges[i])
          all_sim_types <- c(all_sim_types, sim_type)
          all_ranks <- c(all_ranks, case_rank)
        }
      }
    }
  }

  result_df <- data.frame(
    Input = all_inputs,
    Case = all_cases,
    Period = all_periods,
    PeriodRange = all_period_ranges,
    SimType = all_sim_types,
    stringsAsFactors = FALSE
  )

  if (add_scenario_ranking) {
    result_df[[rank_column]] <- all_ranks
  }

  assign(output, result_df, envir = parent.frame())
  invisible(result_df)
}


#' Process GTAP Recursive Dynamic Data with Multi-Case Support
#'
#' Processes GTAP recursive dynamic data from \code{.sl4} or \code{.har} files with support
#' for multiple case scenarios, comparison analysis, and cumulative effects. Data is always
#' assigned to specified variable names in the parent environment.
#'
#' @param input_detail Data frame. Output from \code{\link{dynamic_input_name}} containing
#'   columns: \code{"Input"}, \code{"Case"}, \code{"Period"}, \code{"PeriodRange"}, \code{"SimType"},
#'   and optionally \code{"ScenarioRank"}.
#' @param input_path Character. Path to the input folder containing GTAP data files.
#' @param input_format Character. File format to process. Options:
#'   \itemize{
#'     \item \code{".sl4"}: Solution files (default)
#'     \item \code{".har"}: Header array files
#'   }
#' @param input_suffix Character. Custom suffix for input files (e.g., \code{""}, \code{"-custom"},
#'   \code{"-WEL"}). Default: \code{""}.
#' @param process_vars Character vector, \code{NULL}, or \code{FALSE}. Variables to extract:
#'   \itemize{
#'     \item Character vector: Specific variable names to extract
#'     \item \code{NULL}: Extract all available variables (default)
#'     \item \code{FALSE}: Skip variable processing
#'   }
#' @param comparison Character or \code{NULL}. Comparison format for calculating differences.
#'   Format: \code{"minuend-subtrahend"} where result = minuend - subtrahend.
#'   Examples: \code{"policy-base_rerun"}, \code{"base_rerun-base"}.
#'   If \code{NULL}, no comparison is performed and raw data is returned. Default: \code{NULL}.
#' @param result_format Character. Format for comparison results:
#'   \itemize{
#'     \item \code{"single"}: Period-specific differences (default)
#'     \item \code{"cumulative"}: Accumulated effects from initial period
#'   }
#' @param keep_raw_data Logical. If \code{TRUE} and \code{comparison} is specified, retains
#'   original minuend and subtrahend values as separate columns alongside the difference.
#'   Default: \code{FALSE}.
#' @param mapping_info Character. Metadata mode for variable descriptions and units.
#'   Options: \code{"GTAPv7"} (default), \code{"Yes"}, \code{"No"}, \code{"Mix"}.
#'   See \code{\link{add_mapping_info}} for details.
#' @param external_mapping_info Data frame or \code{NULL}. Custom mapping table with columns:
#'   \code{"Variable"}, \code{"Description"}, \code{"Unit"}. Overrides default mappings.
#'   Default: \code{NULL}.
#' @param extract_method Character. Method for extracting data from files:
#'   \itemize{
#'     \item \code{"get_data_by_var"}: Extract by variable name (default)
#'     \item \code{"get_data_by_dims"}: Extract by dimension structure
#'     \item \code{"group_data_by_dims"}: Group and extract by dimension with priority rules
#'   }
#' @param priority_list Optional list. Priority rules required when \code{extract_method = "group_data_by_dims"}.
#'   Specifies dimension hierarchy for grouping.
#'   Example: \code{list("Sector" = c("COMM", "ACTS"), "Region" = c("REG"))}.
#'   Default: \code{NULL}.
#' @param convert_unit Character or \code{NULL}. Unit conversion to apply:
#'   \itemize{
#'     \item \code{"mil2bil"}: Convert millions to billions
#'     \item \code{"bil2mil"}: Convert billions to millions
#'     \item \code{"pct2frac"}: Convert percentage to fraction
#'     \item \code{"frac2pct"}: Convert fraction to percentage
#'   }
#'   Default: \code{NULL} (no conversion).
#' @param decimals Integer or \code{NULL}. Number of decimal places for rounding numeric values.
#'   Set to \code{NULL} to disable rounding. Default: \code{4}.
#' @param rename_columns Logical. If \code{TRUE}, renames GTAP dimension codes to readable names
#'   (e.g., \code{"REG"} to \code{"Region"}, \code{"COMM"} to \code{"Commodity"},
#'   \code{"ACTS"} to \code{"Activity"}). Default: \code{TRUE}.
#' @param region_select Character vector or \code{NULL}. Filter data to selected regions.
#'   Applies only to the \code{"REG"} (or \code{"Region"} if renamed) column.
#'   Default: \code{NULL} (all regions).
#' @param sector_select Character vector or \code{NULL}. Filter data to selected sectors.
#'   Applies to \code{"ACTS"} and \code{"COMM"} (or \code{"Activity"} and \code{"Commodity"} if renamed) columns.
#'   Default: \code{NULL} (all sectors).
#' @param subtotal_level Logical. If \code{TRUE}, includes subtotal rows in the output for
#'   aggregated dimensions. Default: \code{FALSE}.
#' @param main_output_name Character. Variable name to assign main/primary data output in parent
#'   environment. Default: \code{"plot.data"}.
#' @param macro_output_name Character. Variable name to assign macro economic data output in parent
#'   environment. Default: \code{"GTAPMacro"}.
#'
#' @return Invisibly returns a list containing processed data:
#'   \describe{
#'     \item{GTAPMacros}{Macro economic indicators data frame (if processed)}
#'     \item{main_data}{Primary variable data frame or list (if processed)}
#'     \item{bilateral_data}{QXS bilateral trade data (if processed)}
#'   }
#'   All processed data is also assigned to specified variable names in the parent environment.
#'
#' @details
#' \strong{Multi-Case Processing:}
#'
#' The function processes multiple GTAP recursive dynamic cases simultaneously. Cases are
#' distinguished by the \code{"Case"} column in the output. This allows comparative analysis
#' across different policy scenarios or model configurations.
#'
#' \strong{Scenario Ranking:}
#'
#' If \code{input_detail} contains a \code{"ScenarioRank"} column (created by
#' \code{\link{dynamic_input_name}} with \code{add_scenario_ranking = TRUE}), this ranking
#' is automatically preserved throughout all processing steps. No additional parameters are needed.
#'
#' \strong{Comparison Calculations:}
#'
#' When \code{comparison} is specified, the function calculates differences between simulation
#' types (e.g., policy effects relative to baseline) for each period and case:
#' \itemize{
#'   \item \code{result_format = "single"}: Calculates period-by-period differences
#'   \item \code{result_format = "cumulative"}: Accumulates differences over time within each group
#' }
#'
#' Cumulative calculations group by all columns except \code{"Period"}, ensuring correct
#' accumulation for each unique combination of Case, Region, Sector, Variable, etc.
#'
#' \strong{File Discovery:}
#'
#' The function automatically discovers available input files and reports missing files per case.
#' Files must match the naming pattern specified in \code{input_detail$Input} with the
#' appropriate suffix and format extension.
#'
#' \strong{Output Structure:}
#'
#' All outputs include:
#' \itemize{
#'   \item \code{Case}: Case identifier from input mapping
#'   \item \code{Period}: Time period (numeric year)
#'   \item \code{Value}: Main data values or calculated differences
#'   \item \code{Unit}: Variable units (required for GTAPViz compatibility)
#'   \item \code{ScenarioRank}: Scenario ordering (if present in \code{input_detail})
#'   \item Additional columns: SimType (if no comparison), minuend/subtrahend values (if \code{keep_raw_data = TRUE})
#' }
#'
#' \strong{Special Variable Processing:}
#'
#' \itemize{
#'   \item \strong{Macro variables}: Automatically processed from \code{.sl4} files when
#'     \code{process_vars = NULL} or includes \code{"macro"}, \code{"macros"}, or \code{"gtapmacro"}
#'   \item \strong{QXS (bilateral trade)}: Automatically processed from \code{.sl4} files when
#'     \code{process_vars = NULL} or includes variables with \code{"qxs"} in the name
#' }
#'
#' @author Pattawee Puangchit
#' @export
#' @seealso
#' \code{\link{dynamic_input_name}} for generating input mappings,
#' \code{\link{add_mapping_info}} for variable metadata,
#' \code{\link{convert_units}} for unit conversions
#'
#' @examples
#' \dontrun{
#' # Example 1: Generate input mapping for multiple cases with ranking
#' dynamic_input_name(
#'   case_name = c("US_All", "US_Retail", "US_ReduceTar"),
#'   type = "prefix",
#'   input_format = list(
#'     US_All = list(
#'       base_rerun = "bwrb-rwrr-",
#'       policy = "bwrb-rwrr-pwrp-"
#'     ),
#'     US_Retail = list(
#'       base_rerun = "brtb-rtrr-",
#'       policy = "brtb-rtrr-prtp-"
#'     ),
#'     US_ReduceTar = list(
#'       base_rerun = "bfrb-rfrr-",
#'       policy = "bfrb-rfrr-pfrp-"
#'     )
#'   ),
#'   pattern = "2018:2035",
#'   increment = 1,
#'   add_scenario_ranking = TRUE,
#'   output = "Input_map"
#' )
#'
#' # Example 2: Process with policy-baseline comparison (cumulative)
#' auto_gtap_rd(
#'   input_detail = Input_map,
#'   input_path = "path/to/data",
#'   input_format = ".sl4",
#'   comparison = "policy-base_rerun",
#'   result_format = "cumulative",
#'   keep_raw_data = TRUE,
#'   convert_unit = "mil2bil",
#'   main_output_name = "sl4.data"
#' )
#'
#' # Example 3: Process specific variables without comparison
#' auto_gtap_rd(
#'   input_detail = Input_map,
#'   input_path = "path/to/data",
#'   input_format = ".sl4",
#'   process_vars = c("qgdp", "pop", "qo", "qxs"),
#'   comparison = NULL,
#'   extract_method = "get_data_by_var",
#'   main_output_name = "raw.data"
#' )
#'
#' # Example 4: Process with custom mapping and filters
#' # Define custom variable mapping
#' custom_map <- data.frame(
#'   Variable = c("qgdp", "pop"),
#'   Description = c("Real GDP", "Population"),
#'   Unit = c("Billion USD", "Million persons")
#' )
#'
#' auto_gtap_rd(
#'   input_detail = Input_map,
#'   input_path = "path/to/data",
#'   input_format = ".sl4",
#'   process_vars = custom_map,
#'   comparison = "policy-base_rerun",
#'   result_format = "single",
#'   mapping_info = "Mix",
#'   external_mapping_info = custom_map,
#'   region_select = c("USA", "CHN", "JPN"),
#'   decimals = 2,
#'   main_output_name = "filtered.data"
#' )
#'
#' # Example 5: Process HAR decomposition files
#' auto_gtap_rd(
#'   input_detail = Input_map,
#'   input_path = "path/to/data",
#'   input_format = ".har",
#'   input_suffix = "-WEL",
#'   comparison = "policy-base_rerun",
#'   result_format = "cumulative",
#'   extract_method = "get_data_by_dims",
#'   main_output_name = "decomp.data"
#' )
#'
#' # Example 6: Process with dimension grouping
#' auto_gtap_rd(
#'   input_detail = Input_map,
#'   input_path = "path/to/data",
#'   input_format = ".sl4",
#'   comparison = "policy-base_rerun",
#'   result_format = "cumulative",
#'   extract_method = "group_data_by_dims",
#'   priority_list = list(
#'     "Sector" = c("COMM", "ACTS"),
#'     "Region" = c("REG", "SRCREG", "DSTREG")
#'   ),
#'   main_output_name = "grouped.data"
#' )
#' }
auto_gtap_rd <- function(input_detail,
                         input_path = NULL,
                         input_format = ".sl4",
                         input_suffix = "",
                         process_vars = NULL,
                         comparison = TRUE,
                         result_format = "cumulative",
                         keep_raw_data = TRUE,
                         mapping_info = "GTAPv7",
                         external_mapping_info = NULL,
                         extract_method = "get_data_by_var",
                         priority_list = NULL,
                         convert_unit = NULL,
                         decimals = 4,
                         rename_columns = TRUE,
                         region_select = NULL,
                         sector_select = NULL,
                         subtotal_level = FALSE,
                         main_output_name = "comparison.data",
                         macro_output_name = "GTAPMacro") {

  if (!is.data.frame(input_detail)) {
    stop("'input_detail' must be a data frame")
  }

  required_cols <- c("Input", "Case", "Period", "SimType")
  missing_cols <- setdiff(required_cols, names(input_detail))
  if (length(missing_cols) > 0) {
    stop("'input_detail' is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!input_format %in% c(".sl4", ".har")) {
    stop("'input_format' must be '.sl4' or '.har'")
  }

  if (!result_format %in% c("single", "cumulative")) {
    stop("'result_format' must be 'single' or 'cumulative'")
  }

  has_scenario_rank <- "ScenarioRank" %in% names(input_detail)

  do_comparison <- !is.null(comparison)
  minuend_type <- NULL
  subtrahend_type <- NULL
  input_detail_filtered <- input_detail

  if (do_comparison) {
    comparison_parts <- strsplit(comparison, "-")[[1]]
    if (length(comparison_parts) != 2) {
      stop("'comparison' must be in format 'type1-type2' (e.g., 'policy-base_rerun', 'base_rerun-base')")
    }
    minuend_type <- comparison_parts[1]
    subtrahend_type <- comparison_parts[2]

    available_types <- unique(input_detail$SimType)
    if (!minuend_type %in% available_types) {
      stop("'", minuend_type, "' not found in input_detail$SimType. Available: ",
           paste(available_types, collapse = ", "))
    }
    if (!subtrahend_type %in% available_types) {
      stop("'", subtrahend_type, "' not found in input_detail$SimType. Available: ",
           paste(available_types, collapse = ", "))
    }

    input_detail_filtered <- input_detail[input_detail$SimType %in% c(minuend_type, subtrahend_type), ]
  }

  all_cases <- unique(input_detail_filtered$Case)
  message("Processing cases: ", paste(all_cases, collapse = ", "))

  process_log <- list()
  all_data <- list()

  extract_var_names <- function(var_def) {
    if (is.data.frame(var_def) && "Variable" %in% names(var_def)) {
      return(var_def$Variable)
    } else {
      return(var_def)
    }
  }

  var_list <- extract_var_names(process_vars)
  process_data_flag <- !identical(process_vars, FALSE)

  process_macro <- input_format == ".sl4" && (is.null(process_vars) ||
                                                any(tolower(var_list) %in% c("macro", "macros", "gtapmacro")))

  process_qxs <- input_format == ".sl4" && (is.null(process_vars) ||
                                              any(grepl("qxs", var_list, ignore.case = TRUE)))

  if (process_qxs && is.character(var_list) && !is.null(var_list)) {
    qxs_vars <- var_list[grepl("qxs", var_list, ignore.case = TRUE)]
    var_list <- var_list[!grepl("qxs", var_list, ignore.case = TRUE)]
    if (length(var_list) == 0) var_list <- NULL
  }

  file_suffix <- if (nzchar(input_suffix)) paste0(input_suffix, input_format) else input_format

  files <- list.files(input_path, full.names = FALSE, ignore.case = TRUE)

  find_valid_cases <- function(file_suffix, input_names) {
    pattern_str <- paste0(file_suffix, "$")
    files_matching <- files[grepl(pattern_str, files, ignore.case = TRUE)]

    bases <- character(0)
    for (file in files_matching) {
      base <- substr(file, 1, nchar(file) - nchar(file_suffix))
      bases <- c(bases, tolower(trimws(base)))
    }

    valid_cases <- input_names[tolower(input_names) %in% bases]
    return(valid_cases)
  }

  valid_inputs <- find_valid_cases(file_suffix, input_detail_filtered$Input)

  missing_inputs <- setdiff(input_detail_filtered$Input, valid_inputs)
  if (length(missing_inputs) > 0) {
    message("Warning: The following inputs were not found in the directory:")
    for (case in all_cases) {
      case_missing <- missing_inputs[input_detail_filtered$Input[input_detail_filtered$Case == case] %in% missing_inputs]
      if (length(case_missing) > 0) {
        message("  Case '", case, "': ", paste(case_missing, collapse = ", "))
      }
    }
  }

  load_func <- if (input_format == ".sl4") HARplus::load_sl4x else HARplus::load_harx

  transform_data <- function(data, apply_filters = TRUE) {
    if (is.null(data)) return(NULL)

    data <- .apply_to_dataframes(data, rename_GTAP_bilateral)
    data <- add_mapping_info(data, mapping = mapping_info, external_map = external_mapping_info)

    if (apply_filters && (!is.null(region_select) || !is.null(sector_select))) {
      data <- .apply_filters(
        data,
        region_select = region_select,
        experiment_select = unique(input_detail_filtered$Input),
        sector_select = sector_select
      )
    }

    if (!is.null(decimals)) {
      data <- .format_decimal_places(data, decimals)
    }

    if (length(data) == 1 && is.list(data) && !is.data.frame(data)) {
      data <- data[[1]]
    }

    return(data)
  }

  process_data <- function(valid_cases, select_vars, method_name, priority = NULL) {
    if (length(valid_cases) == 0) return(NULL)

    data_raw <- setNames(
      lapply(valid_cases, function(scenario) {
        file_path <- file.path(input_path, paste0(scenario, file_suffix))
        if (file.exists(file_path)) {
          tryCatch({
            load_func(file_path, select_header = select_vars)
          }, error = function(e) {
            message(sprintf("Error processing %s: %s", file_path, e$message))
            return(NULL)
          })
        } else {
          message(sprintf("Skipping %s (file not found)", file_path))
          return(NULL)
        }
      }),
      valid_cases
    )

    data_raw <- data_raw[!sapply(data_raw, is.null)]
    if (length(data_raw) == 0) return(NULL)

    method_map <- list(
      "get_data_by_dims" = HARplus::get_data_by_dims,
      "get_data_by_var" = HARplus::get_data_by_var,
      "group_data_by_dims" = HARplus::group_data_by_dims
    )

    if (!is.character(method_name) || !method_name %in% names(method_map)) {
      stop(paste0("Invalid method provided. Choose from: '",
                  paste(names(method_map), collapse = "', '"), "'."))
    }

    keep_unique_flag <- length(data_raw) > 1

    if (method_name == "group_data_by_dims") {
      params <- list(
        experiment_names = names(data_raw),
        auto_rename = TRUE,
        priority = priority %||% list("Sector" = c("COMM", "ACTS"), "Region" = c("REG")),
        subtotal_level = subtotal_level
      )

      if (rename_columns) {
        params$rename_cols = c(REG = "Region", COMM = "Commodity", ACTS = "Activity")
      }
    } else {
      params <- list(
        experiment_names = names(data_raw),
        subtotal_level = subtotal_level,
        merge_data = keep_unique_flag
      )

      if (rename_columns) {
        params$rename_cols = c(REG = "Region", COMM = "Commodity", ACTS = "Activity")
      }
    }

    grouped_data <- tryCatch({
      do.call(method_map[[method_name]], c(params, data_raw))
    }, error = function(e) {
      message(sprintf("Error in grouping data: %s", e$message))
      return(NULL)
    })

    if (!keep_unique_flag && is.list(grouped_data) && length(grouped_data) == 1 && !is.data.frame(grouped_data)) {
      grouped_data <- grouped_data[[1]]
    }

    return(grouped_data)
  }

  add_dynamic_info <- function(data, input_map) {
    process_df <- function(df) {
      if (!is.data.frame(df) || nrow(df) == 0 || !"Experiment" %in% names(df)) {
        return(df)
      }

      if (!"Case" %in% names(df)) df$Case <- NA_character_
      if (!"Period" %in% names(df)) df$Period <- NA_integer_
      if (!"SimType" %in% names(df)) df$SimType <- NA_character_
      if (has_scenario_rank && !"ScenarioRank" %in% names(df)) df$ScenarioRank <- NA_integer_

      for (i in 1:nrow(input_map)) {
        input_val <- input_map$Input[i]
        case_val <- input_map$Case[i]
        period_str <- input_map$Period[i]
        sim_type_val <- input_map$SimType[i]

        period_val <- as.numeric(substr(period_str, nchar(period_str) - 3, nchar(period_str)))

        matching_rows <- df$Experiment == input_val
        if (any(matching_rows)) {
          df$Case[matching_rows] <- case_val
          df$Period[matching_rows] <- period_val
          df$SimType[matching_rows] <- sim_type_val

          if (has_scenario_rank) {
            df$ScenarioRank[matching_rows] <- input_map$ScenarioRank[i]
          }
        }
      }

      df$Experiment <- NULL

      return(df)
    }

    if (is.data.frame(data)) {
      return(process_df(data))
    } else if (is.list(data)) {
      return(lapply(data, function(x) {
        if (is.data.frame(x)) {
          process_df(x)
        } else if (is.list(x)) {
          lapply(x, process_df)
        } else {
          x
        }
      }))
    } else {
      return(data)
    }
  }

  calculate_comparison <- function(data, minuend, subtrahend, cumulative = FALSE, keep_raw = FALSE) {

    process_single_df <- function(df) {
      if (!is.data.frame(df) || !"SimType" %in% names(df) || !"Value" %in% names(df)) {
        return(NULL)
      }

      group_cols <- setdiff(names(df), c("SimType", "Value"))

      df_minuend <- df[df$SimType == minuend, ]
      df_subtrahend <- df[df$SimType == subtrahend, ]

      if (nrow(df_minuend) == 0 || nrow(df_subtrahend) == 0) {
        return(NULL)
      }

      merged <- merge(df_minuend, df_subtrahend,
                      by = group_cols,
                      suffixes = c("_minuend", "_subtrahend"),
                      all = FALSE)

      if (nrow(merged) == 0) {
        return(NULL)
      }

      merged$Value <- merged$Value_minuend - merged$Value_subtrahend

      if (keep_raw) {
        merged[[minuend]] <- merged$Value_minuend
        merged[[subtrahend]] <- merged$Value_subtrahend
        keep_cols <- c(group_cols, "Value", minuend, subtrahend)
      } else {
        keep_cols <- c(group_cols, "Value")
      }

      result <- merged[, keep_cols, drop = FALSE]

      if (cumulative && "Period" %in% names(result)) {
        group_by_cols <- setdiff(group_cols, "Period")

        result <- result[order(result$Period), ]

        if (length(group_by_cols) > 0) {
          split_factor <- do.call(paste, c(result[group_by_cols], sep = "|||"))
          result_list <- split(result, split_factor)

          result_list <- lapply(result_list, function(sub_df) {
            sub_df <- sub_df[order(sub_df$Period), ]
            sub_df$Value <- cumsum(sub_df$Value)
            return(sub_df)
          })

          result <- do.call(rbind, result_list)
        } else {
          result <- result[order(result$Period), ]
          result$Value <- cumsum(result$Value)
        }
      }

      rownames(result) <- NULL
      return(result)
    }

    if (is.data.frame(data)) {
      return(process_single_df(data))
    } else if (is.list(data)) {
      result_list <- lapply(data, function(element) {
        if (is.data.frame(element)) {
          process_single_df(element)
        } else if (is.list(element)) {
          lapply(element, process_single_df)
        } else {
          NULL
        }
      })

      result_list <- result_list[!sapply(result_list, is.null)]

      if (length(result_list) == 0) return(NULL)

      return(result_list)
    } else {
      return(NULL)
    }
  }

  if (process_macro && length(valid_inputs) > 0) {
    message("Processing GTAP Macro Data")
    macro_data <- tryCatch({
      macro_raw <- setNames(
        lapply(valid_inputs, function(scenario) {
          file_path <- file.path(input_path, paste0(scenario, file_suffix))
          if (file.exists(file_path)) {
            tryCatch({
              HARplus::load_sl4x(file_path, select_header = macro_info$Variable)
            }, error = function(e) {
              message(sprintf("Error processing %s: %s", file_path, e$message))
              return(NULL)
            })
          } else {
            message(sprintf("Skipping %s (file not found)", file_path))
            return(NULL)
          }
        }),
        valid_inputs
      )

      macro_raw <- macro_raw[!sapply(macro_raw, is.null)]

      GTAPMacros <- do.call(
        HARplus::get_data_by_var,
        c(
          list(
            experiment_names = names(macro_raw),
            subtotal_level = subtotal_level,
            merge_data = length(macro_raw) > 1
          ),
          macro_raw
        )
      )

      GTAPMacros <- add_mapping_info(GTAPMacros, mapping = "GTAPv7")

      GTAPMacros_filtered <- .apply_to_dataframes(GTAPMacros, function(df) {
        df[, c("Variable", "Value", "Subtotal", "Experiment", "Description", "Unit"), drop = FALSE]
      })

      if (length(valid_inputs) == 1) {
        if (is.list(GTAPMacros_filtered) && length(GTAPMacros_filtered) == 1 && !is.data.frame(GTAPMacros_filtered)) {
          GTAPMacros_final <- do.call(rbind, unlist(GTAPMacros_filtered, recursive = FALSE))
        } else {
          GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
        }
      } else {
        GTAPMacros_final <- do.call(rbind, GTAPMacros_filtered)
      }
      rownames(GTAPMacros_final) <- NULL

      GTAPMacros_final <- GTAPMacros_final[order(GTAPMacros_final$Experiment,
                                                 GTAPMacros_final$Variable,
                                                 GTAPMacros_final$Unit), ]

      GTAPMacros_final <- add_dynamic_info(GTAPMacros_final, input_detail_filtered)

      if (do_comparison) {
        GTAPMacros_final <- calculate_comparison(GTAPMacros_final, minuend_type, subtrahend_type,
                                                 cumulative = (result_format == "cumulative"),
                                                 keep_raw = keep_raw_data)
      }

      if (!is.null(GTAPMacros_final)) {
        rename_GTAP_bilateral(GTAPMacros_final)
      } else {
        NULL
      }
    }, error = function(e) {
      process_log$macro <- sprintf("Error processing GTAP Macro Data: %s", e$message)
      return(NULL)
    })

    if (!is.null(macro_data)) {
      process_log$macro <- "GTAP Macro Data processed successfully"
      all_data$GTAPMacros <- macro_data

      if (!is.null(convert_unit) && !is.null(macro_data)) {
        message("Applying unit conversion to macro data: ", convert_unit)
        all_data$GTAPMacros <- convert_units(macro_data, scale_auto = convert_unit)
        all_data$GTAPMacros <- .format_decimal_places(all_data$GTAPMacros, decimals)
      }

      assign(macro_output_name, list(macros = all_data$GTAPMacros), envir = parent.frame())
    }
  }

  if (process_data_flag && length(valid_inputs) > 0) {
    message(paste("Processing", toupper(gsub("\\.", "", input_format)), "Data"))
    process_regular <- TRUE
    if (process_macro && !is.null(var_list) && is.character(var_list)) {
      if (length(var_list) == 1 && tolower(var_list) == "macros") {
        process_regular <- FALSE
      }
    }

    if (process_regular) {
      vars_to_use <- var_list
      if (process_qxs && !is.null(vars_to_use) && is.character(vars_to_use)) {
        vars_to_use <- vars_to_use[!grepl("qxs", vars_to_use, ignore.case = TRUE)]
        if (length(vars_to_use) == 0) vars_to_use <- NULL
      }

      grouped_data <- tryCatch({
        process_data(valid_inputs, vars_to_use, extract_method, priority_list)
      }, error = function(e) {
        process_log$main <- sprintf("Error processing data: %s", e$message)
        return(NULL)
      })

      if (!is.null(grouped_data)) {
        process_log$main <- paste(toupper(gsub("\\.", "", input_format)), "Data processed successfully")
        grouped_data <- transform_data(grouped_data)

        grouped_data <- add_dynamic_info(grouped_data, input_detail_filtered)

        if (do_comparison) {
          grouped_data <- calculate_comparison(grouped_data, minuend_type, subtrahend_type,
                                               cumulative = (result_format == "cumulative"),
                                               keep_raw = keep_raw_data)
        }

        if (!is.null(grouped_data)) {
          all_data$main_data <- grouped_data

          if (!is.null(convert_unit) && !is.null(grouped_data)) {
            message("Applying unit conversion: ", convert_unit)
            all_data$main_data <- convert_units(grouped_data, scale_auto = convert_unit)
            all_data$main_data <- .format_decimal_places(all_data$main_data, decimals)
          }

          assign(main_output_name, all_data$main_data, envir = parent.frame())
        }
      }
    }
  }

  if (process_qxs && length(valid_inputs) > 0) {
    message("Processing QXS Bilateral Trade Data")

    invisible(capture.output({
      bilateral_data <- tryCatch({
        process_data(valid_inputs, "qxs", "get_data_by_var", NULL)
      }, error = function(e) {
        process_log$qxs <- sprintf("Error processing QXS Data: %s", e$message)
        return(NULL)
      })
    }))

    if (!is.null(bilateral_data)) {
      process_log$qxs <- "QXS Bilateral Data processed successfully"
      bilateral_data <- transform_data(bilateral_data)

      bilateral_data <- add_dynamic_info(bilateral_data, input_detail_filtered)

      if (do_comparison) {
        bilateral_data <- calculate_comparison(bilateral_data, minuend_type, subtrahend_type,
                                               cumulative = (result_format == "cumulative"),
                                               keep_raw = keep_raw_data)
      }

      if (!is.null(bilateral_data)) {
        all_data$bilateral_data <- bilateral_data

        if (!is.null(convert_unit) && !is.null(bilateral_data)) {
          message("Applying unit conversion to bilateral data: ", convert_unit)
          all_data$bilateral_data <- convert_units(bilateral_data, scale_auto = convert_unit)
          all_data$bilateral_data <- .format_decimal_places(all_data$bilateral_data, decimals)
        }

        assign("bilateral_data", list(qxs = all_data$bilateral_data), envir = parent.frame())
      }
    }
  }

  message("\nSummary of Processing:")
  message("Cases processed: ", paste(all_cases, collapse = ", "))
  if (!is.null(process_log$macro)) message(process_log$macro)
  if (!is.null(process_log$main)) message(process_log$main)
  if (!is.null(process_log$qxs)) message(process_log$qxs)

  if (all(vapply(process_log, function(x) grepl("successfully", x), logical(1)))) {
    message("GTAP recursive dynamic data processing completed successfully!")
  } else {
    failed_processes <- names(process_log)[!vapply(process_log, function(x) grepl("successfully", x), logical(1))]
    message(sprintf("Processing completed with errors in: %s", paste(failed_processes, collapse = ", ")))
  }

  on.exit({
    message("\r", appendLF = FALSE)
    flush.console()
  }, add = TRUE)

  return(invisible(all_data))
}
