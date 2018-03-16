#' metrics shared statistics
#'
#' helper for sfn_metrics
#'
#' This helper pipes the different chain of commands to perform the period
#' summaries, shared by general, predawn and midday metrics
#'
#' @param sfn_data an sfn_data object
#' @param period tibbletime::collapse_by period
#' @param .funs summarise_all funs
#' @param ... optional arguments for tibbletime::collapse_by function

period_summaries <- function(sfn_data, period, .funs, ...) {

  # we will need the extra arguments (...) if any, just in case
  dots <- list(...)
  dots_collapse_by <- dots[names(dots) %in% methods::formalArgs(tibbletime::collapse_by)]
  dots_summarise_all <- dots[!(names(dots) %in% methods::formalArgs(tibbletime::collapse_by))]

  # if we need to pass unknown arguments to collapse_by and summarise_all, then
  # we use the quasiquotation system (!!!args_list), that way we don't need to
  # worry about which arguments are supplied
  sfn_data %>%
    tibbletime::as_tbl_time(index = TIMESTAMP) %>%
    tibbletime::collpase_by(period = period, !!! dots_collapse_by) %>%
    dplyr::group_by(TIMESTAMP) %>%
    dplyr::summarise_all(.funs = .funs, !!! dots_summarise_all) -> res

  return(res)
}

#' data coverage
#'
#' helper for sfn_metrics
#'
#' This helper function calculates the coverage percentage in a vector, and is
#' designed to be used inside a dplyr summarise statement.
#'
#' @param x a vector, usually a variable in the sapflow or environmental data.
#'
#' @return a single value (numeric) with the percentage of coverage for that
#'   variable
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   group_by(Species)
#'   summarise_all(data_coverage) # 100 for all variables
#'
#' @export

data_coverage <- function(x) {
  (sum(!is.na(x)) / length(x)) * 100
}

#' time at maximum/minimum
#'
#' helper for sfn_metrics
#'
#' This helper functions return the TIMESTAMP value at which the maximum value
#' occurs. It is designed to be used inside a dplyr summarise statement.
#'
#' @param x a numeric vector, usually a variable in the sapflow or environmental
#'   data.
#'
#' @param time a POSIXct or character vector with the TIMESTAMP
#'
#' @return a single value (character) with the TIMESTAMP value.
#'
#' @examples
#' library(dplyr)
#' storms %>%
#'   group_by(year) %>%
#'   summarise(wind_max = max(wind),
#'             hour_at_max = max_time(wind, time = hour),
#'             wind_min = min(wind),
#'             hour_at_min = min_time(wind, time = hour))
#'
#' @export

max_time <- function(x, time) {
  time[which.max(x)]
}

#' @describeIn max_time helper for sfn_metrics
#'
#' @export

min_time <- function(x, time) {
  time[which.min(x)]
}
