#' Summaries by period
#'
#' This function collapse the TIMESTAMP to the desired period (day, month...)
#' by setting the same value to all timestamps within the period. This modified
#' TIMESTAMP is used to group by and summarise the data.
#'
#' This function uses internally \code{\link[tibbletime]{collapse_index}} and
#' \code{\link[dplyr]{summarise_all}} and arguments to control these functions
#' can be passed as `...`. Arguments for each function are spliced and applied
#' when needed. Be advised that all arguments passed to the summarise_all function
#' will be applied to all the summarising functions used, so it will fail if any
#' of that functions does not accept that argument. To complex function-argument
#' relationships, indicate each summary function call within the \code{.funs}
#' argument with \code{\link[dplyr]{funs}}:
#' \preformatted{
#' # This will fail beacuse na.rm argument will be also passed to the n function,
#' # which does not accept any argument:
#' summarise_by_period(
#'   data = get_sapf(FOO),
#'   period = '7 days',
#'   .funs = funs(mean, sd, n),
#'   na.rm = TRUE
#' )
#'
#' # to solve this is better to use the .funs argument:
#' summarise_by_period(
#'   data = get_sapf(FOO),
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
#' )
#' }
#'
#' @section TIMESTAMP_coll:
#'   Previously to the collapsing step, a temporal variable called \code{TIMESTAMP_coll}
#'   is created to be able to catch the real timestamp when some events happens, for
#'   example to use the \code{min_time} function. If your custom summarise function
#'   needs to get the time at which some event happens, use TIMESTAMP_coll instead of
#'   TIMESTAMP for that:
#'   \preformatted{
#'     min_time <- function(x, time) {
#'       time[which.min(x)]
#'     }
#'
#'     summarise_by_period(
#'       data = get_sapf(FOO),
#'       period = 'daily',
#'       .funs = funs(min_time, time = TIMESTAMP_coll) # Not TIMESTAMP
#'     )
#'   }
#'
#' @param data sapflow or environmental data as obtained by \code{\link{get_sapf}}
#'   and \code{\link{get_env}}
#' @param period tibbletime::collapse_index period
#' @param .funs dplyr::summarise_all funs
#' @param ... optional arguments for tibbletime::collapse_index function and
#'   dplyr::summarise_all function
#'
#' @return A `tbl_time` object with the metrics results. The names of the columns
#'   indicate the original variable (tree or environmental variable) and the
#'   metric calculated (i.e. `vpd_mean`)
#'
#' @examples
#' # data
#' load('FOO', package = 'sapfluxnetr')
#'
#' # simple summary
#' summarise_by_period(
#'   data = get_sapf(FOO),
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
#' )
#'
#' @importFrom rlang .data
#'
#' @export

summarise_by_period <- function(data, period, .funs, ...) {

  # we will need the extra arguments (...) if any, just in case
  dots <- list(...)
  dots_collapse_index <- dots[names(dots) %in% methods::formalArgs(tibbletime::collapse_index)]
  dots_summarise_all <- dots[!(names(dots) %in% methods::formalArgs(tibbletime::collapse_index))]

  # if we need to pass unknown arguments to collapse_index and summarise_all, then
  # we use the quasiquotation system (!!!args_list), that way we don't need to
  # worry about which arguments are supplied
  # But there is a problem, if no extra arguments provided, !!! fails, so we need
  # to cover all possible scenarios
  if (length(dots_collapse_index) > 0) {
    if (length(dots_summarise_all) > 0) {
      data %>%
        tibbletime::as_tbl_time(index = TIMESTAMP) %>%
        # tibbletime::collapse_index(period = period, !!! dots_collapse_index) %>%
        dplyr::mutate(
          TIMESTAMP_coll = .data$TIMESTAMP,
          TIMESTAMP = tibbletime::collapse_index(
            index = .data$TIMESTAMP,
            period = period,
            !!! dots_collapse_index
          )
        ) %>%
        dplyr::group_by(.data$TIMESTAMP) %>%
        dplyr::summarise_all(.funs = .funs, !!! dots_summarise_all) %>%
        dplyr::select(-dplyr::contains('_coll_')) -> res

      return(res)
    } else {
      data %>%
        tibbletime::as_tbl_time(index = TIMESTAMP) %>%
        # tibbletime::collapse_index(period = period, !!! dots_collapse_index) %>%
        dplyr::mutate(
          TIMESTAMP_coll = .data$TIMESTAMP,
          TIMESTAMP = tibbletime::collapse_index(
            index = .data$TIMESTAMP,
            period = period,
            !!! dots_collapse_index
          )
        ) %>%
        dplyr::group_by(.data$TIMESTAMP) %>%
        dplyr::summarise_all(.funs = .funs) %>%
        dplyr::select(-dplyr::contains('_coll_')) -> res

      return(res)
    }
  } else {
    if (length(dots_summarise_all) > 0) {
      data %>%
        tibbletime::as_tbl_time(index = TIMESTAMP) %>%
        # tibbletime::collapse_index(period = period, !!! dots_collapse_index) %>%
        dplyr::mutate(
          TIMESTAMP_coll = .data$TIMESTAMP,
          TIMESTAMP = tibbletime::collapse_index(
            index = .data$TIMESTAMP,
            period = period
          )
        ) %>%
        dplyr::group_by(.data$TIMESTAMP) %>%
        dplyr::summarise_all(.funs = .funs, !!! dots_summarise_all) %>%
        dplyr::select(-dplyr::contains('_coll_')) -> res

      return(res)
    } else {
      data %>%
        tibbletime::as_tbl_time(index = TIMESTAMP) %>%
        # tibbletime::collapse_index(period = period, !!! dots_collapse_index) %>%
        dplyr::mutate(
          TIMESTAMP_coll = .data$TIMESTAMP,
          TIMESTAMP = tibbletime::collapse_index(
            index = .data$TIMESTAMP,
            period = period
          )
        ) %>%
        dplyr::group_by(.data$TIMESTAMP) %>%
        dplyr::summarise_all(.funs = .funs) %>%
        dplyr::select(-dplyr::contains('_coll_')) -> res

      return(res)
    }
  }
}


#' Metrics summary function
#'
#' Generate daily or above metrics from a site data for the period indicated
#'
#' @section period:
#' \code{period} argument is piped to \code{tibbletime::collapse_index} function.
#' See \code{\link[tibbletime]{collapse_index}} for a detailed explanation but in
#' short:
#' \itemize{
#'   \item{\emph{frequency period} format: "1 day", "7 day", "1 month", "1 year"}
#'   \item{\emph{shorthand} format: "hourly", "daily", "monthly", "yearly"}
#'   \item{\emph{custom} format: a vector of dates to use as custom and more flexible boundaries}
#' }
#'
#' @section .funs:
#' \code{.funs} argument uses the same method as the \code{.funs} argument in the
#' \code{\link[dplyr]{summarise_all}} function of \code{dplyr} package. Basically
#' it accepts a list of function calls generated by funs(), or a character vector
#' of function names, or simply a function. If you want to pass on a custom
#' function you can specify it here. See details in \code{\link{summarise_by_period}}
#' for more complex summarising functions declaration.
#'
#' @param sfn_data \code{\link{sfn_data}} or \code{\link{sfn_data_multi}} object
#'   to obtain the metrics from
#'
#' @param period Time period to aggregate data by. See period section for an explanation
#'   about the periods ('daily', 'monthly', 'yearly', ...)
#'
#' @param .funs List of function calls generated by \code{\link[dplyr]{funs}}, or
#'   a character vector of functions means, or simply a function. See .funs section
#'   for more info about supplying custom functions.
#'
#' @param solar Logical indicating if the solarTIMESTAMP must be used instead of
#'   the site local TIMESTAMP. Default to TRUE (use solarTIMESTAMP).
#'
#' @param predawn Logical indicating if metrics for predawn interval must be
#'   also returned.
#'
#' @param pd_start Hour to start the predawn interval
#'
#' @param pd_end Hour to end the predawn interval
#'
#' @param midday Logical indicating if metrics for midday interval must be also
#'   returned.
#'
#' @param md_start Hour to start the midday interval
#'
#' @param md_end Hour to end the midday interval
#'
#' @param nighttime Experimental, not implemented (stats for night time)
#'
#' @param ... optional arguments to pass to methods used
#'   (i.e. tibbletime::collapse_index or summarise funs extra arguments)
#'
#' @family metrics
#'
#' @return For \code{\link{sfn_data}} objects, a list of tbl_time objects
#'   with the following structure:
#'   \itemize{
#'     \item{$sapf: metrics for the sapflow data
#'           \itemize{
#'             \item{$sapf: general metrics}
#'             \item{$sapf_pd: metrics for predawn interval (if
#'                   \code{predawn = FALSE} the slot will be empty)}
#'             \item{$sapf_md: metrics for midday interval (if
#'                   \code{midday = FALSE} the slot will be empty)}
#'           }}
#'     \item{$env: metrics for the environmental data
#'           \itemize{
#'             \item{$env: general metrics}
#'             \item{$env_pd: metrics for predawn interval (if
#'                   \code{predawn = FALSE} the slot will be empty)}
#'             \item{$env_md: metrics for midday interval (if
#'                   \code{midday = FALSE} the slot will be empty)}
#'           }}
#'   }
#'
#'   For \code{\link{sfn_data_multi}} objects, a list of lists of tbl_time objects
#'   with the metrics for each site:
#'   \itemize{
#'     \item{$SITE_CODE
#'       \itemize{
#'         \item{$sapf: metrics for the sapflow data
#'           \itemize{
#'             \item{$sapf: general metrics}
#'             \item{$sapf_pd: metrics for predawn interval (if
#'                   \code{predawn = FALSE} the slot will be empty)}
#'             \item{$sapf_md: metrics for midday interval (if
#'                   \code{midday = FALSE} the slot will be empty)}
#'           }}
#'       }
#'       \itemize{
#'         \item{$env: metrics for the environmental data
#'           \itemize{
#'             \item{$env: general metrics}
#'             \item{$env_pd: metrics for predawn interval (if
#'                   \code{predawn = FALSE} the slot will be empty)}
#'             \item{$env_md: metrics for midday interval (if
#'                   \code{midday = FALSE} the slot will be empty)}
#'           }}
#'       }
#'     }
#'     \item{$NEXT_SITE_CODE...}
#'   }
#'
#' @examples
#' ## sfn_data
#' data('FOO', pkg = 'sapfluxnetr')
#' FOO_metrics <- sfn_metrics(
#'   FOO,
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = FALSE,
#'   predawn = TRUE,
#'   pd_start = 4,
#'   pd_end = 6,
#'   midday = TRUE,
#'   md_start = 12,
#'   md_end = 14,
#'   side = 'start'
#' )
#'
#' str(FOO_metrics)
#' FOO_metrics[['sapf']][['sapf_pd']]
#'
#' ## sfn_data_multi
#' data('BAR', pkg = 'sapfluxnetr')
#' data('BAZ', pkg = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(FOO, BAR, BAZ)
#'
#' multi_metrics <- sfn_metrics(
#'   multi_sfn,
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = FALSE,
#'   predawn = TRUE,
#'   pd_start = 4,
#'   pd_end = 6,
#'   midday = TRUE,
#'   md_start = 12,
#'   md_end = 14,
#'   side = 'start'
#' )
#'
#' str(multi_metrics)
#'
#' multi_metrics[['FOO']][['sapf']][['sapf_pd']]
#'
#' @export

sfn_metrics <- function(
  sfn_data,
  period,
  .funs,
  solar,
  predawn,
  pd_start,
  pd_end,
  midday,
  md_start,
  md_end,
  # nighttime,
  ...
) {

  # argument checks
  if (!(class(sfn_data) %in% c('sfn_data', 'sfn_data_multi'))) {
    stop(
      'sfn_metrics only works with sfn_data and sfn_data_multi object classes'
    )
  }

  # we need to check if multi and then repeat the function for each element
  if (is(sfn_data, 'sfn_data_multi')) {
    res_multi <- sfn_data %>%
      purrr::map(sfn_metrics,
        period = period,
        .funs = .funs,
        solar = solar,
        predawn = predawn,
        pd_start = pd_start,
        pd_end = pd_end,
        midday = midday,
        md_start = md_start,
        md_end = md_end,
        # nighttime,
        ...
      )

    return(res_multi)
  }

  # if sfn_data then we have to calculate the desired metrics from the data
  sapf_data <- get_sapf(sfn_data, solar = solar)
  env_data <- get_env(sfn_data, solar = solar)

  whole_data <- list(sapf = sapf_data, env = env_data)

  # period summaries: we want to know period means, maximum, minimum, quantiles...
  whole_data %>%
    purrr::map(summarise_by_period, period, .funs, ...) -> period_summary

  # filtering summaries we want to know filtered period (predawn, midday) means,
  # maximum, minimum, quantiles...
  # `summarise_by_period` is a helper function documented in helpers.R

  # predawn
  if (predawn) {
    whole_data %>%
      purrr::map(
        dplyr::filter,
        dplyr::between(lubridate::hour(.data$TIMESTAMP), pd_start, pd_end)
      ) %>%
      purrr::map(summarise_by_period, period, .funs, ...) -> predawn_summary

    names(predawn_summary) <- paste0(names(predawn_summary), '_pd')
    names(predawn_summary[['sapf_pd']]) <- paste0(names(predawn_summary[['sapf_pd']]), '_pd')
    names(predawn_summary[['env_pd']]) <- paste0(names(predawn_summary[['env_pd']]), '_pd')

  } else {
    predawn_summary <- NULL
  }

  # midday
  if (midday) {
    whole_data %>%
      purrr::map(
        dplyr::filter,
        dplyr::between(lubridate::hour(.data$TIMESTAMP), md_start, md_end)
      ) %>%
      purrr::map(summarise_by_period, period, .funs, ...) -> midday_summary

    names(midday_summary) <- paste0(names(midday_summary), '_md')
    names(midday_summary[['sapf_md']]) <- paste0(names(midday_summary[['sapf_md']]), '_md')
    names(midday_summary[['env_md']]) <- paste0(names(midday_summary[['env_md']]), '_md')

  } else {
    midday_summary <- NULL
  }

  # we create the result object:
  # res
  #   $sapf
  #     $sapf
  #     $sapf_pd
  #     $spf_md
  #   $env
  #     $env
  #     $env_pd
  #     $env_md
  # this way all is modular and after that they can be combined by bind_cols
  res <- list(
    sapf = list(
      sapf = period_summary[['sapf']],
      sapf_pd = predawn_summary[['sapf_pd']],
      sapf_md = midday_summary[['sapf_md']]
    ),
    env = list(
      env = period_summary[['env']],
      env_pd = predawn_summary[['env_pd']],
      env_md = midday_summary[['env_md']]
    )
  )

  return(res)
}


####### shorthand functions for sfn_metrics ####################################

#' Complete daily metrics for a site (or multi-site)
#'
#' This function returns a complete daily summary for the site/s
#'
#' @inheritParams sfn_metrics
#'
#' @param probs numeric vector of probabilities for \code{\link[stats]{quantile}}
#'
#' @param ... optional arguments passed to \code{\link{sfn_metrics}}
#'
#' @family metrics
#'
#' @examples
#' # data load
#' data('FOO', package = 'sapfluxnetr')
#'
#' # default complete daily metrics
#' FOO_daily <- daily_metrics(FOO)
#'
#' str(FOO_daily)
#' FOO_daily[['env']][['env']]
#'
#' # change the predawn and midday interval
#' FOO_int_daily <- daily_metrics(
#'   FOO,
#'   pd_start = 5, pd_end = 7, # predawn starting and ending hour
#'   md_start = 13, md_end = 15 # midday starting and ending hour
#' )
#'
#' str(FOO_int_daily)
#'
#' # get only the general metrics
#' FOO_gen <- daily_metrics(FOO, predawn = FALSE, midday = FALSE)
#'
#' str(FOO_gen)
#' # no predawn or midday
#' FOO_gen[['sapf']][['sapf_pd']] # NULL
#' FOO_gen[['sapf']][['sapf']] # data
#'
#' @return For \code{\link{sfn_data}} objects, a tibble with the metrics. For
#'   \code{\link{sfn_data_multi}} objects, a list of tibbles with the metrics
#'   for each site.
#'
#' @importFrom dplyr n
#'
#' @export

daily_metrics <- function(
  sfn_data,
  solar = TRUE,
  predawn = TRUE,
  pd_start = 3,
  pd_end = 5,
  midday = TRUE,
  md_start = 11,
  md_end = 13,
  probs = c(0.95, 0.99),
  ...
) {

  # hardcoded values
  period <- 'daily'

  # check if user supplied custom funs (.funs), if not, harcoded values
  dots <- list(...)
  if ('.funs' %in% names(dots)) {
    .funs <- dots[['.funs']]
    dots <- dots[names(dots) != '.funs']
  } else {

    # we need magic to add the quantiles as they return more than one value
    # (usually). So lets play with quasiquotation
    quantile_args <- probs %>%
      purrr::map(function(x) {dplyr::quo(quantile(., probs = x, na.rm = TRUE))})
    names(quantile_args) <- paste0('q_', round(probs*100, 0))

    .funs <- dplyr::funs(
      mean = mean(., na.rm = TRUE),
      n = n(),
      coverage = data_coverage(.),
      !!! quantile_args,
      max = max(., na.rm = TRUE),
      max_time = max_time(., .data$TIMESTAMP_coll),
      min = min(., na.rm = TRUE),
      min_time = min_time(., .data$TIMESTAMP_coll)
    )
  }

  # just input all in the sfn_function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    predawn = predawn,
    pd_start = pd_start,
    pd_end = pd_end,
    midday = midday,
    md_start = md_start,
    md_end = md_end,
    ...
  )
}

#' Complete monthly metrics for a site (or multi-site)
#'
#' This function returns a complete monthly summary for the site/s
#'
#' @inheritParams daily_metrics
#'
#' @family metrics
#'
#' @return For \code{\link{sfn_data}} objects, a tibble with the metrics. For
#'   \code{\link{sfn_data_multi}} objects, a list of tibbles with the metrics
#'   for each site.
#'
#' @examples
#' # data load
#' data('FOO', package = 'sapfluxnetr')
#'
#' # default complete daily metrics
#' FOO_monthly <- monthly_metrics(FOO)
#'
#' str(FOO_monthly)
#' FOO_monthly[['env']][['env']]
#'
#' # change the predawn and midday interval
#' FOO_int_monthly <- monthly_metrics(
#'   FOO,
#'   pd_start = 5, pd_end = 7, # predawn starting and ending hour
#'   md_start = 13, md_end = 15 # midday starting and ending hour
#' )
#'
#' str(FOO_int_monthly)
#'
#' # get only the general metrics
#' FOO_gen <- monthly_metrics(FOO, predawn = FALSE, midday = FALSE)
#'
#' str(FOO_gen)
#' # no predawn or midday
#' FOO_gen[['sapf']][['sapf_pd']] # NULL
#' FOO_gen[['sapf']][['sapf']] # data
#'
#' @export

monthly_metrics <- function(
  sfn_data,
  solar = TRUE,
  predawn = TRUE,
  pd_start = 3,
  pd_end = 5,
  midday = TRUE,
  md_start = 11,
  md_end = 13,
  probs = c(0.95, 0.99),
  na.rm = FALSE,
  ...
) {

  # hardcoded values
  period <- 'monthly'

  # check if user supplied custom funs (.funs), if not, harcoded values
  dots <- list(...)
  if ('.funs' %in% names(dots)) {
    .funs <- dots[['.funs']]
    dots <- dots[names(dots) != '.funs']
  } else {

    # we need magic to add the quantiles as they return more than one value
    # (usually). So lets play with quasiquotation
    quantile_args <- probs %>%
      purrr::map(function(x) {dplyr::quo(quantile(., probs = x, na.rm = TRUE))})
    names(quantile_args) <- paste0('q_', round(probs*100, 0))

    .funs <- dplyr::funs(
      mean = mean(., na.rm = TRUE),
      n = n(),
      coverage = data_coverage(.),
      !!! quantile_args,
      max = max(., na.rm = TRUE),
      max_time = max_time(., .data$TIMESTAMP_coll),
      min = min(., na.rm = TRUE),
      min_time = min_time(., .data$TIMESTAMP_coll)
    )
  }

  # just input all in the sfn_metrics function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    predawn = predawn,
    pd_start = pd_start,
    pd_end = pd_end,
    midday = midday,
    md_start = md_start,
    md_end = md_end,
    ...
  )
}
