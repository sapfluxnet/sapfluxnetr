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
  sfn_data %>%
    tibbletime::as_tbl_time(index = TIMESTAMP) %>%
    tibbletime::collpase_by(period = period, ...) %>%
    dplyr::group_by(TIMESTAMP) %>%
    dplyr::summarise_all(.funs = .funs) -> res
  
  return(res)
}