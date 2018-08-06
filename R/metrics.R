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
#'   data = get_sapf_data(ARG_TRE),
#'   period = '7 days',
#'   .funs = funs(mean, sd, n),
#'   na.rm = TRUE
#' )
#'
#' # to solve this is better to use the .funs argument:
#' summarise_by_period(
#'   data = get_sapf_data(ARG_TRE),
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
#'       data = get_sapf_data(ARG_TRE),
#'       period = 'daily',
#'       .funs = funs(min_time, time = TIMESTAMP_coll) # Not TIMESTAMP
#'     )
#'   }
#'
#' @param data sapflow or environmental data as obtained by \code{\link{get_sapf_data}}
#'   and \code{\link{get_env_data}}. Must have a column named TIMESTAMP
#' @param period tibbletime::collapse_index period
#' @param .funs dplyr::summarise_all funs
#' @param ... optional arguments for \code{link{summarise_by_period}}
#'
#' @return A `tbl_time` object with the metrics results. The names of the columns
#'   indicate the original variable (tree or environmental variable) and the
#'   metric calculated (i.e. `vpd_mean`)
#'
#' @examples
#' library(dplyr)
#'
#' # data
#' data('ARG_TRE', package = 'sapfluxnetr')
#'
#' # simple summary
#' summarise_by_period(
#'   data = get_sapf_data(ARG_TRE),
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
#' )
#'
#' @importFrom rlang .data
#'
#' @export

summarise_by_period <- function(data, period, .funs, ...) {

  # modificate .funs if data is environmental (no centroids).
  env_vars_names <- .env_vars_names()

  if (any(names(data) %in% env_vars_names)) {

    # only in daily, but as daily can be stated in many ways, we check if
    # .funs[['centroid']] is NULL, and if not, we make it.
    if (!is.null(.funs[['centroid']])) {
      .funs[['centroid']] <- NULL
    }

  }

  # we will need the extra arguments (...) if any, just in case
  dots <- rlang::quos(...)
  dots_collapse_index <- dots[names(dots) %in%
                                methods::formalArgs(tibbletime::collapse_index)]
  dots_summarise_all <- dots[!(names(dots) %in%
                                 methods::formalArgs(tibbletime::collapse_index))]

  # TODO set clean = TRUE and side "start" for the collapse, except if they are
  # setted by the user.
  if (is.null(dots_collapse_index[['side']])) {
    dots_collapse_index[['side']] <- rlang::quo('start')
  }

  if (is.null(dots_collapse_index[['clean']])) {
    dots_collapse_index[['clean']] <- rlang::quo(TRUE)
  }

  data %>%
    # tibbletime::as_tbl_time(index = TIMESTAMP) %>%
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

}


#' Metrics summary function
#'
#' Generate daily or above metrics from a site data for the period indicated
#'
#' @section Period:
#' \code{period} argument is piped to \code{tibbletime::collapse_index} function
#' with \code{side = 'start', clean = TRUE} options. See
#' \code{\link[tibbletime]{collapse_index}} for a detailed explanation but in
#' short:
#' \itemize{
#'   \item{\emph{frequency period} format: "1 day", "7 day", "1 month",
#'         "1 year"}
#'   \item{\emph{shorthand} format: "hourly", "daily", "monthly", "yearly"}
#'   \item{\emph{custom} format: a vector of dates to use as custom and more
#'         flexible boundaries}
#' }
#' Also, you can override the default behaviour of \code{sfn_metrics}, providing
#' \code{side = 'end'} or \code{clean = FALSE}.
#'
#' @section .funs:
#' \code{.funs} argument uses the same method as the \code{.funs} argument in the
#' \code{\link[dplyr]{summarise_all}} function of \code{dplyr} package. Basically
#' it accepts a list of function calls generated by funs(), or a character vector
#' of function names, or simply a function. If you want to pass on a custom
#' function you can specify it here. See details in
#' \code{\link{summarise_by_period}} for more complex summarising functions
#' declaration.
#'
#' @section Interval:
#' Previously to the metrics summary, data can be filtered by an special
#' interval (predawn for example). With the \code{interval} argument this
#' filtering can be specified:
#' \itemize{
#'   \item{\code{general} (default). No special interval is used, and metrics
#'         are performed with all the data}.
#'   \item{\code{predawn}. Data is filtered for predawn interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{midday}. Data is filtered for midday interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{night}. Data is filtered for night interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{daylight}. Data is filtered for daylight interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#' }
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
#' @param interval Character vector indicating if the metrics must be filtered
#'   by an special hour interval. See Interval section in details.
#'
#' @param int_start Numeric value indicating the starting hour of the special
#'   interval in 24h format. See Interval section in details.
#'
#' @param int_end Numeric value indicating the ending hour of the special
#'   interval in 24h format. See Interval section in details.
#'
#' @param ... optional arguments to pass to methods used
#'   (i.e. tibbletime::collapse_index or summarise funs extra arguments)
#'
#' @family metrics
#'
#' @return For \code{\link{sfn_data}} objects, a list of tbl_time objects
#'   with the following structure:
#'   \itemize{
#'     \item{$sapf: metrics for the sapflow data}
#'     \item{$env: metrics for the environmental data}
#'   }
#'
#'   For \code{\link{sfn_data_multi}} objects, a list of lists of tbl_time objects
#'   with the metrics for each site:
#'   \itemize{
#'     \item{$SITE_CODE
#'       \itemize{
#'         \item{$sapf: metrics for the sapflow data}
#'         \item{$env: metrics for the environmental data}
#'       }
#'     }
#'     \item{$NEXT_SITE_CODE...}
#'   }
#'
#' @examples
#' library(dplyr)
#'
#' ### general metrics
#' ## sfn_data
#' data('ARG_TRE', package = 'sapfluxnetr')
#' ARG_TRE_metrics <- sfn_metrics(
#'   ARG_TRE,
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = FALSE,
#'   interval = 'general'
#' )
#'
#' str(ARG_TRE_metrics)
#' ARG_TRE_metrics[['sapf']]
#' ARG_TRE_metrics[['env']]
#'
#' ## sfn_data_multi
#' \dontrun{
#' data('ARG_MAZ', package = 'sapfluxnetr')
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
#'
#' multi_metrics <- sfn_metrics(
#'   multi_sfn,
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = FALSE,
#'   interval = 'general'
#' )
#'
#' str(multi_metrics)
#'
#' multi_metrics[['ARG_TRE']][['sapf']]
#' }
#'
#' ### midday metrics
#' ARG_TRE_midday <- sfn_metrics(
#'   ARG_TRE,
#'   period = 'daily',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = TRUE,
#'   interval = 'midday', int_start = 11, int_end = 13
#' )
#'
#' str(ARG_TRE_midday)
#' ARG_TRE_midday[['sapf']]
#'
#' @export

sfn_metrics <- function(
  sfn_data,
  period,
  .funs,
  solar,
  interval = c('general', 'predawn', 'midday', 'night', 'daylight'),
  int_start = NULL,
  int_end = NULL,
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
                 interval = interval,
                 int_start = int_start,
                 int_end = int_end,
                 ...
      )

    return(res_multi)
  }

  # if sfn_data then we have to calculate the desired metrics from the data

  print(paste0(
    'Crunching data for ', get_si_code(sfn_data), '. In large datasets this ',
    'could take a while'
  ))

  sapf_data <- get_sapf_data(sfn_data, solar = solar) %>%
    dplyr::mutate(
      timestep = get_plant_md(sfn_data)[['pl_sens_timestep']][1],
      period_minutes = .period_to_minutes(
        period, .data$TIMESTAMP, unique(.data$timestep)
      )
    )
  env_data <- get_env_data(sfn_data, solar = solar) %>%
    dplyr::mutate(
      timestep = get_env_md(sfn_data)[['env_timestep']][1],
      period_minutes = .period_to_minutes(
        period, .data$TIMESTAMP, unique(.data$timestep)
      )
    )

  whole_data <- list(sapf = sapf_data, env = env_data)

  # if interval is night, is somewhat complicated. We need to uniformize the
  # interval for nights as they span two days.
  if (interval == 'night') {

    # progress to not scare seeming like freezed in large datasets
    print(paste0('Nighttime data for ', get_si_code(sfn_data)))

    night_data <- whole_data %>%
      purrr::map(
        dplyr::filter,
        dplyr::between(
          lubridate::hour(.data$TIMESTAMP), int_start, 24
        ) |
          dplyr::between(
            lubridate::hour(.data$TIMESTAMP), 0, int_end - 1
          )
      )

    if (period == 'daily') {

      night_boundaries <- night_data[['sapf']] %>%
        dplyr::mutate(
          coll = tibbletime::collapse_index(
            index = .data$TIMESTAMP,
            period = period,
            side = 'start'
          )
        ) %>%
        dplyr::group_by(.data$coll) %>%
        # closest to night start timestamp
        dplyr::summarise(
          custom_dates = .data$TIMESTAMP[which.min(
            abs(lubridate::hour(.data$TIMESTAMP) - int_start)
          )]
        ) %>%
        dplyr::pull(.data$custom_dates)

      night_boundaries <- night_boundaries %>%
        lubridate::floor_date(unit = 'hours')

      # workaround the two nights in one day problem (sites that start at
      # 00:00:00 have two nights in the same day, the first one corresponds to
      # the previous day and we have to fix it)
      margins <- 60*60*24
      extra_start_boundary <- night_boundaries[[1]] - margins
      extra_end_boundary <- night_boundaries[[length(night_boundaries)]] + margins
      night_boundaries <- as.POSIXct(
        c(
          as.character(extra_start_boundary),
          as.character(night_boundaries),
          as.character(extra_end_boundary)
        ),
        tz = attr(extra_start_boundary, 'tz')
      )

      period_summary <- night_data %>%
        purrr::map(summarise_by_period, night_boundaries, .funs, ...)

    } else {

     period_summary <- night_data %>%
        purrr::map(summarise_by_period, period, .funs, ...)

    }

    names(period_summary[['sapf']]) <- paste0(
      names(period_summary[['sapf']]), '_night'
    )
    names(period_summary[['env']]) <- paste0(
      names(period_summary[['env']]), '_night'
    )

  } else {

    # if interval is general, we don't need to filter by hours so is
    # straightforward:
    if (interval == 'general') {

      # progress to not scare seeming like freezed
      print(paste0('General data for ', get_si_code(sfn_data)))

      # period summaries: we want to know period means, maximum, minimum, quantiles...
      whole_data %>%
        purrr::map(summarise_by_period, period, .funs, ...) -> period_summary

    } else {

      # if interval is not general and not night, then we need to filter by hours
      # and then calculate the summaries

      # progress to not scare seeming like freezed
      print(paste0(interval, ' data for ', get_si_code(sfn_data)))

      whole_data %>%
        purrr::map(
          dplyr::filter,
          dplyr::between(lubridate::hour(.data$TIMESTAMP), int_start, int_end)
        ) %>%
        ### map2(., list(ts_sapf, ts_env), ~ mutate(.x, ts = .y))
        purrr::map(summarise_by_period, period, .funs, ...) -> period_summary
        ### map(mutate eliminar ^timestep_*)

      # now we need to create the names with the interval
      # predawn
      if (interval == 'predawn') {
        names(period_summary[['sapf']]) <- paste0(
          names(period_summary[['sapf']]), '_pd'
        )
        names(period_summary[['env']]) <- paste0(
          names(period_summary[['env']]), '_pd'
        )
      }

      # midday
      if (interval == 'midday') {
        names(period_summary[['sapf']]) <- paste0(
          names(period_summary[['sapf']]), '_md'
        )
        names(period_summary[['env']]) <- paste0(
          names(period_summary[['env']]), '_md'
        )
      }

      # daylight
      if (interval == 'daylight') {
        names(period_summary[['sapf']]) <- paste0(
          names(period_summary[['sapf']]), '_daylight'
        )
        names(period_summary[['env']]) <- paste0(
          names(period_summary[['env']]), '_daylight'
        )
      }
    }
  }

  # remove the timestep and period_minutes columns created
  res <- period_summary %>%
    purrr::modify_depth(
      1, ~ dplyr::select(
        .x, -dplyr::starts_with('timestep'),
        -dplyr::starts_with('period_minutes')
      )
    )

  return(res)
}




####### shorthand functions for sfn_metrics ####################################

#' helper function to generate the fixed metrics
#'
#' generates a call to dplyr::funs to capture the fixed metrics
#'
#' @param probs probs vector for quantile
#'
#' @param centroid logical indicating if centroid calculation must be made.
#'   (i.e. in monthly metrics, the centroid calculation is not needed)

.fixed_metrics_funs <- function(probs, centroid) {

  # hack to avoid R CMD CHECKS to complain about . not being a global variable
  . <- NULL

  # we need magic to add the quantiles as they return more than one value
  # (usually). So lets play with quasiquotation
  quantile_args <- probs %>%
    purrr::map(function(x) {dplyr::quo(stats::quantile(., probs = x, na.rm = TRUE))})
  names(quantile_args) <- paste0('q_', round(probs*100, 0))

  .funs <- dplyr::funs(
    mean = mean(., na.rm = TRUE),
    sd = stats::sd(., na.rm = TRUE),
    n = n(),
    # coverage = data_coverage(.),
    coverage = data_coverage(., .data$timestep, .data$period_minutes),
    !!! quantile_args,
    max = max(., na.rm = TRUE),
    max_time = max_time(., .data$TIMESTAMP_coll),
    min = min(., na.rm = TRUE),
    min_time = min_time(., .data$TIMESTAMP_coll),
    centroid = diurnal_centroid(.)
  )

  if (!centroid) {
    .funs[['centroid']] <- NULL
    }

  return(.funs)

}

#' Complete metrics wrappers
#'
#' This set of functions returns a complete set of statistics for a site (using
#' \code{\link{sfn_data}}) or several sites (using \code{\link{sfn_data_multi}})
#'
#' @details
#' \code{*_metrics} functions are wrappers for \code{\link{sfn_metrics}} with a
#' set of fixed arguments.
#'
#' \code{*_metrics} functions return all or some of the following statistics:
#' \itemize{
#'   \item{mean: mean of variable (tree or environmental variable) for the
#'         given period. NAs are removed}
#'   \item{n: Number of measures in the give period used for the metrics
#'         (NAs included)}
#'   \item{coverage: Data coverage percentage (percentage of measures without
#'         NAs)}
#'   \item{q_XX: XX quantile value for the period}
#'   \item{max: Maximum value for the period}
#'   \item{max_time: Time when maximum value was reached}
#'   \item{min: Minimum value for the period}
#'   \item{min_time: Time when minimum value was reached}
#'   \item{centroid: Diurnal centroid value (hours passed until the half of
#'         the summed daily value was reached). Only returned for sapflow
#'         measures when period is 'daily'}
#' }
#'
#' @param probs numeric vector of probabilities for
#'   \code{\link[stats]{quantile}}
#'
#' @param tidy Logical indicating if the metrics must be returned in a tidy
#'   format (a long tibble, each observation in its own row)
#'
#' @param metadata metadata object, usually the result of
#'   \code{\link{read_sfn_metadata}}. Only used if tidy is TRUE.
#'
#' @family metrics
#'
#' @return If \code{tidy} is TRUE, a tibble with the metrics for
#'   sapflow and environmental data, with all the metadata included. If
#'   \code{tidy} is FALSE (default), a list of tibbles with the calculated
#'   metrics.
#'
#' @importFrom dplyr n
#'
#' @name metrics
NULL

#' @rdname metrics
#'
#' @section daily_metrics:
#' \code{daily_metrics} summarise daily data for all hours in the day
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## daily_metrics
#' # data load
#' data('ARG_TRE', package = 'sapfluxnetr')
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#'
#' # non tidy raw metrics (default)
#' ARG_TRE_raw_daily <- daily_metrics(ARG_TRE)
#' str(ARG_TRE_raw_daily)
#'
#' # tidy daily metrics
#' ARG_TRE_daily <- daily_metrics(
#'   ARG_TRE, tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' ARG_TRE_daily
#'
#' @export

daily_metrics <- function(
  sfn_data,
  solar = TRUE,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  # hardcoded values
  period <- 'daily'

  # default funs
  .funs <- .fixed_metrics_funs(probs, TRUE)

  # just input all in the sfn_function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'general',
    ...
  )

  # tidy?
  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'general')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section monthly_metrics:
#' \code{monthly_metrics} summarise monthly data for all hours in the day.
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## monthly_metrics
#' # data load
#' data('ARG_TRE', package = 'sapfluxnetr')
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#'
#' # non tidy raw metrics (default)
#' ARG_TRE_raw_monthly <- monthly_metrics(ARG_TRE)
#' str(ARG_TRE_raw_monthly)
#'
#' # tidy monthly metrics
#' ARG_TRE_monthly <- monthly_metrics(
#'   ARG_TRE, tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' ARG_TRE_monthly
#'
#' @export

monthly_metrics <- function(
  sfn_data,
  solar = TRUE,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  # hardcoded values
  period <- 'monthly'

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'general',
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'general')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section nightly_metrics:
#' \code{nightly_metrics} will return the metrics for night
#' periods, summarised daily or monthly
#'
#' Night for daily period starts in DOY x and ends in DOY x+1 (i.e. if
#' \code{night_start = 20, night_end = 6} values for the night starting at
#' 2018-03-28 20:00:00 and ending at 2018-03-29 06:00:00 are summarised).
#'
#' Night for monthly period summarises all night periods in the month, that
#' includes from 00:00:00 of the first month night to 23:59:59 of the last
#' month night.
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## nightly_metrics
#' # data load
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#'
#' # non tidy daily night metrics (default)
#' AUS_CAN_ST2_MIX_night <- nightly_metrics(AUS_CAN_ST2_MIX)
#'
#' str(AUS_CAN_ST2_MIX_night)
#' AUS_CAN_ST2_MIX_night[['sapf']]
#' AUS_CAN_ST2_MIX_night[['env']]
#'
#' # change the night interval
#' AUS_CAN_ST2_MIX_night_short <- nightly_metrics(
#'   AUS_CAN_ST2_MIX, int_start = 21, int_end = 4 # night starting and ending hour
#' )
#' AUS_CAN_ST2_MIX_night_short[['env']]
#'
#' # tidy nightly metrics
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#' AUS_CAN_ST2_MIX_night_tidy <- nightly_metrics(
#'   AUS_CAN_ST2_MIX,
#'   tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' AUS_CAN_ST2_MIX_night_tidy
#'
#' @export

nightly_metrics <- function(
  sfn_data,
  period = c('daily', 'monthly'),
  solar = TRUE,
  int_start = 20,
  int_end = 6,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  period <- match.arg(period)

  # default funs
  if (period == 'daily') {
    .funs <- .fixed_metrics_funs(probs, TRUE)
  } else {
    .funs <- .fixed_metrics_funs(probs, FALSE)
  }

  # just input all in the sfn_metrics function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'night',
    int_start = int_start,
    int_end = int_end,
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'night')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section daylight_metrics:
#' \code{daylight_metrics} will return the metrics for daylight
#' periods, summarised daily or monthly. Daylight interval is selected by start
#' and end hours.
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## daylight_metrics
#' # data load
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#'
#' # non tidy daily daylight metrics (default)
#' AUS_CAN_ST2_MIX_daylight <- daylight_metrics(AUS_CAN_ST2_MIX)
#'
#' str(AUS_CAN_ST2_MIX_daylight)
#' AUS_CAN_ST2_MIX_daylight[['sapf']]
#' AUS_CAN_ST2_MIX_daylight[['env']]
#'
#' # change the daylight interval
#' AUS_CAN_ST2_MIX_daylight_short <- daylight_metrics(
#'   AUS_CAN_ST2_MIX, int_start = 8, int_end = 18 # night starting and ending hour
#' )
#' AUS_CAN_ST2_MIX_daylight_short[['env']]
#'
#' # tidy daylight metrics
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#' AUS_CAN_ST2_MIX_daylight_tidy <- daylight_metrics(
#'   AUS_CAN_ST2_MIX,
#'   tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' AUS_CAN_ST2_MIX_daylight_tidy
#'
#' @export

daylight_metrics <- function(
  sfn_data,
  period = c('daily', 'monthly'),
  solar = TRUE,
  int_start = 6,
  int_end = 20,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  period <- match.arg(period)

  # default funs
  if (period == 'daily') {
    .funs <- .fixed_metrics_funs(probs, TRUE)
  } else {
    .funs <- .fixed_metrics_funs(probs, FALSE)
  }

  # just input all in the sfn_metrics function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'daylight',
    int_start = int_start,
    int_end = int_end,
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'daylight')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section predawn_metrics:
#' \code{predawn_metrics} will always return the metrics for predawn
#' period, summarised daily or monthly. Predawn interval is selected by start and
#' end hours.
#'
#' Predawn metrics did not return the centroid metric.
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## predawn_metrics
#' # data load
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#'
#' # non tidy daily predawn metrics (default)
#' AUS_CAN_ST2_MIX_predawn <- predawn_metrics(AUS_CAN_ST2_MIX)
#'
#' str(AUS_CAN_ST2_MIX_predawn)
#' AUS_CAN_ST2_MIX_predawn[['sapf']]
#' AUS_CAN_ST2_MIX_predawn[['env']]
#'
#' # change the predawn interval
#' AUS_CAN_ST2_MIX_predawn_short <- predawn_metrics(
#'   AUS_CAN_ST2_MIX, int_start = 8, int_end = 18 # night starting and ending hour
#' )
#' AUS_CAN_ST2_MIX_predawn_short[['env']]
#'
#' # tidy daylight metrics
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#' AUS_CAN_ST2_MIX_predawn_tidy <- predawn_metrics(
#'   AUS_CAN_ST2_MIX,
#'   tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' AUS_CAN_ST2_MIX_predawn_tidy
#'
#' @export

predawn_metrics <- function(
  sfn_data,
  period = c('daily', 'monthly'),
  solar = TRUE,
  int_start = 4,
  int_end = 6,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  period <- match.arg(period)

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_metrics function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'predawn',
    int_start = int_start,
    int_end = int_end,
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'predawn')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section midday_metrics:
#' \code{midday_metrics} will always return the metrics for midday
#' period, summarised daily or monthly. midday interval is selected by start and
#' end hours.
#'
#' Midday metrics did not return the centroid metric.
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## midday_metrics
#' # data load
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#'
#' # non tidy daily midday metrics (default)
#' AUS_CAN_ST2_MIX_midday <- midday_metrics(AUS_CAN_ST2_MIX)
#'
#' str(AUS_CAN_ST2_MIX_midday)
#' AUS_CAN_ST2_MIX_midday[['sapf']]
#' AUS_CAN_ST2_MIX_midday[['env']]
#'
#' # change the midday interval
#' AUS_CAN_ST2_MIX_midday_short <- midday_metrics(
#'   AUS_CAN_ST2_MIX, int_start = 8, int_end = 18 # night starting and ending hour
#' )
#' AUS_CAN_ST2_MIX_midday_short[['env']]
#'
#' # tidy daylight metrics
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#' AUS_CAN_ST2_MIX_midday_tidy <- midday_metrics(
#'   AUS_CAN_ST2_MIX,
#'   tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' AUS_CAN_ST2_MIX_midday_tidy
#'
#' @export

midday_metrics <- function(
  sfn_data,
  period = c('daily', 'monthly'),
  solar = TRUE,
  int_start = 11,
  int_end = 13,
  probs = c(0.95, 0.99),
  tidy = FALSE,
  metadata = NULL,
  ...
) {

  period <- match.arg(period)

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_metrics function
  res_raw <- sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'midday',
    int_start = int_start,
    int_end = int_end,
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'midday')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' Build a tidy data frame from the metrics results nested list
#'
#' Transform the nested list of metrics in a tidy tibble where each observation
#' has its own row
#'
#' @param metrics_res Nested list containing the metrics results as obtained
#'   from \code{\link{metrics}}
#'
#' @param metadata List containing the metadata nested list, as obtained from
#'   \code{\link{read_sfn_metadata}}
#'
#' @param interval Interval to return, it depends on the \code{metrics_res} and
#'   can be \code{"gen"} for the general metrics, \code{"md"} for midday metrics,
#'   \code{"pd"} for predawn metrics, \code{"night"} for night metrics or
#'   \code{"day"} for diurnal metrics.
#'
#' @return a tibble with the following columns:
#'   \itemize{
#'     \item{TIMESTAMP: POSIXct vector with the date-time of the observation}
#'     \item{si_code: Character vector with the site codes}
#'     \item{pl_code: Character vector with the plant codes}
#'     \item{sapflow_*: Variables containing the different metrics for the
#'           sapflow measurements (i.e. sapflow_mean, sapflow_q_95)}
#'     \item{ta_*; rh_*; vpd_*; ...: Variables containing the different metrics
#'           for environmental variables (i.e. ta_mean, ta_q_95)}
#'     \item{pl_*: plant metadata variables (i.e. pl_sapw_area, pl_sens_meth)}
#'     \item{si_*: site metadata variables (i.e. si_biome, si_contact_firstname)}
#'     \item{st_*: stand metadata variables (i.e. st_aspect, st_lai)}
#'     \item{sp_*: species metadata variables (i.e. sp_basal_area_perc)}
#'     \item{env_*: environmental metadata variables (i.e. env_timezone)}
#'   }
#'
#' @examples
#' # data
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
#' data('sfn_metadata_ex', package = 'sapfluxnetr')
#'
#' # metrics
#' multi_metrics <- daily_metrics(multi_sfn)
#'
#' # tidyfing
#' multi_tidy <- metrics_tidyfier(
#'   multi_metrics, sfn_metadata_ex, interval = 'general'
#' )
#' multi_tidy
#'
#' # A really easier way of doing the same
#' multi_tidy_easy <- daily_metrics(multi_sfn, tidy = TRUE, metadata = sfn_metadata_ex)
#'
#' @export

metrics_tidyfier <- function(
  metrics_res,
  metadata,
  interval = c('general', 'predawn', 'midday', 'night', 'daylight')
) {

  # hack to avoid CRAN NOTE with the use of "." in the last step
  . <- NULL

  # which timestamp var we use (depends on the interval)
  timestamp_var <- switch(
    interval,
    'general' = 'TIMESTAMP',
    'midday' = 'TIMESTAMP_md',
    'predawn' = 'TIMESTAMP_pd',
    'night' = 'TIMESTAMP_night',
    'daylight' = 'TIMESTAMP_daylight'
  )

  # individual data objects
  # In the case of sapf and env data, if the metrics corresponds to only one
  # site the extraction must be done without sapf/env level
  if (all(names(metrics_res) %in% c('sapf', 'env'))) {

    sapf_data <- metrics_res['sapf']

    env_data <- metrics_res['env']

    raw_codes <- metadata[['site_md']][['si_code']]

    raw_index <- stringr::str_detect(names(sapf_data[['sapf']])[2], raw_codes)

    sites_codes <- raw_codes[raw_index]

    names(sapf_data) <- sites_codes
    names(env_data) <- sites_codes

  } else {

    sapf_data <- metrics_res %>%
      purrr::map(c('sapf'))

    env_data <- metrics_res %>%
      purrr::map(c('env'))

    sites_codes <- names(metrics_res)

  }

  plant_md <- metadata[['plant_md']] %>%
    dplyr::filter(.data$si_code %in% sites_codes)

  site_md <- metadata[['site_md']] %>%
    dplyr::filter(.data$si_code %in% sites_codes)

  stand_md <- metadata[['stand_md']] %>%
    dplyr::filter(.data$si_code %in% sites_codes)

  species_md <- metadata[['species_md']] %>%
    dplyr::filter(.data$si_code %in% sites_codes) %>%
    # dplyr::group_by(.data$si_code) %>%
    # dplyr::summarise_all(function(x) { list(x) })
    dplyr::mutate(pl_species = .data$sp_name)

  env_md <- metadata[['env_md']] %>%
    dplyr::filter(.data$si_code %in% sites_codes)

  # whole data object
  env_vars_names <- .env_vars_names()

  # In this step we need to avoid gathering environmental metrics as well as the
  # timestamp. If we do it this way we don't have to worry which env vars are
  # present at the current site
  env_vars_to_exclude_from_gather <- dplyr::vars(
    -dplyr::starts_with('TIMESTAMP'),
    -dplyr::starts_with(env_vars_names[1]),
    -dplyr::starts_with(env_vars_names[2]),
    -dplyr::starts_with(env_vars_names[3]),
    -dplyr::starts_with(env_vars_names[4]),
    -dplyr::starts_with(env_vars_names[5]),
    -dplyr::starts_with(env_vars_names[6]),
    -dplyr::starts_with(env_vars_names[7]),
    -dplyr::starts_with(env_vars_names[8]),
    -dplyr::starts_with(env_vars_names[9]),
    -dplyr::starts_with(env_vars_names[10]),
    -dplyr::starts_with(env_vars_names[11])
  )

  tmp_data <- sapf_data %>%

    # join sapf and env for all sites
    purrr::map2(env_data, ~ dplyr::full_join(.x, .y, by = timestamp_var)) %>%

    # converting to tibble to get rid of tibbletime corrupted index after gather
    purrr::map(tibble::as.tibble) %>%

    # wide to long for sapflow measures (we use here the excluded vars from
    # before)
    purrr::map(
      ~ suppressWarnings(tidyr::gather(
        .x, tree, value, !!! env_vars_to_exclude_from_gather
      )
      )) %>%

    # union all sites, creating the variables needed as NA for those sites that
    # lack some env variable
    .multi_union() %>%

    # arrange by timestamp
    dplyr::arrange(eval(as.name(timestamp_var))) %>%

    # mutate to get the plant code. Is tricky as we have to separate the metric
    # and interval labels at the end of the tree column
    dplyr::mutate(
      sapflow = stringr::str_sub(
        .data$tree, stringr::str_locate(.data$tree, "(Js|Jt)_[0-9]*")[,2] + 2, -1
      ),
      tree = stringr::str_sub(
        .data$tree, 1, stringr::str_locate(.data$tree, "(Js|Jt)_[0-9]*")[,2]
      )
    ) %>%
    dplyr::rename(pl_code = .data$tree) %>%

    # from long to wide, as each sapflow metric is a variable
    tidyr::spread(.data$sapflow, .data$value, sep = '_') %>%

    # join all the metadata, always first the plant_md to join by pl_code
    # and after that by site code as the rest of metadata is one row only,
    # except for species_md, as it has a row by species and we need to join
    # not only by site, also by pl_species
    dplyr::left_join(plant_md, by = 'pl_code') %>%
    dplyr::left_join(site_md, by = 'si_code') %>%
    dplyr::left_join(stand_md, by = 'si_code') %>%
    dplyr::left_join(species_md, by = c('si_code', 'pl_species')) %>%
    dplyr::left_join(env_md, by = 'si_code') %>%

    # order the columns
    dplyr::select(
      dplyr::starts_with('TIMESTAMP'), .data$si_code, .data$pl_code,
      dplyr::starts_with('sapflow_'), dplyr::everything()
    )

  # reconvert the sapflow times (min and max) to POSIX, if calculated
  whole_data <- tmp_data %>%
    dplyr::mutate_at(
      dplyr::vars(
        dplyr::contains('sapflow_min_time'),
        dplyr::contains('sapflow_max_time')
      ),
      dplyr::funs(
        as.POSIXct(
          ., tz = attr(.data[[timestamp_var]], 'tz'),
          origin = lubridate::origin
        )
      )
    )

  return(whole_data)

}
