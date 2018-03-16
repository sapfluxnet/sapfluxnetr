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

#' getting the formals (arguments and their default value)
