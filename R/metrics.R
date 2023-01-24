#' Summaries by period
#'
#' This function collapse the TIMESTAMP to the desired period (day, month...)
#' by setting the same value to all timestamps within the period. This modified
#' TIMESTAMP is used to group by and summarise the data.
#' 
#' This function uses internally \code{\link{.collapse_timestamp}} and
#' \code{\link[dplyr]{summarise_all}}. Arguments to control these functions
#' can be passed as `...`. Arguments for each function are spliced and applied
#' when needed. Be advised that all arguments passed to the summarise_all function
#' will be applied to all the summarising functions used, so it will fail if any
#' of that functions does not accept that argument. To complex function-argument
#' relationships, indicate each summary function call within the \code{.funs}
#' argument as explained here \code{\link[dplyr]{summarise_all}}:
#' \preformatted{
#' # This will fail beacuse na.rm argument will be also passed to the n function,
#' # which does not accept any argument:
#' summarise_by_period(
#'   data = get_sapf_data(ARG_TRE),
#'   period = '7 days',
#'   .funs = list(mean, sd, n()),
#'   na.rm = TRUE
#' )
#'
#' # to solve this is better to use the .funs argument:
#' summarise_by_period(
#'   data = get_sapf_data(ARG_TRE),
#'   period = '7 days',
#'   .funs = list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE), ~ n())
#' )
#' }
#'
#' @section TIMESTAMP_coll:
#'   Previously to the collapsing step, a temporal variable called
#'   \code{TIMESTAMP_coll} is created to be able to catch the real timestamp when
#'   some events happens, for example to use the \code{min_time} function. If
#'   your custom summarise function needs to get the time at which some event
#'   happens, use TIMESTAMP_coll instead of TIMESTAMP for that:
#'   \preformatted{
#'     min_time <- function(x, time) {
#'       time[which.min(x)]
#'     }
#'
#'     summarise_by_period(
#'       data = get_sapf_data(ARG_TRE),
#'       period = '1 day',
#'       .funs = list(~ min_time(., time = TIMESTAMP_coll)) # Not TIMESTAMP
#'     )
#'   }
#'
#' @param data sapflow or environmental data as obtained by \code{\link{get_sapf_data}}
#'   and \code{\link{get_env_data}}. Must have a column named TIMESTAMP
#' @param period period to collapse by. See \code{\link{sfn_metrics}} for details.
#' @param .funs funs to summarise the data. See details.
#' @param ... optional arguments. See details
#'
#' @return A `tbl_df` object with the metrics results. The names of the columns
#'   indicate the original variable (tree or environmental variable) and the
#'   metric calculated (i.e. `vpd_mean`), separated by underscore
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
#'   .funs = list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE), ~ n())
#' )
#'
#' @importFrom rlang .data
#'
#' @export

summarise_by_period <- function(data, period, .funs, ...) {
  
  # modificate .funs if data is environmental (no centroids).
  if (any(names(data) %in% .env_vars_names())) {

    # only in daily, but as daily can be stated in many ways, we check if
    # .funs[['centroid']] is NULL, and if not, we make it.
    if (!is.null(.funs[['centroid']])) {
      .funs[['centroid']] <- NULL
    }

  }

  # we will need the extra arguments (...) if any, just in case
  dots <- rlang::quos(...)
  
  if (rlang::is_function(period)) {
    # if period is a custom function, remove side argument if it exists
    # and dispatch each argument in dots to the corresponding function
    # (period custom function or summarise_all)
    dots['side'] <- NULL
    side_val <- 'start'
    dots_collapse_timestamp <- dots[names(dots) %in%
                                      methods::formalArgs(period)]
    dots_summarise_all <- dots[!(names(dots) %in%
                                   methods::formalArgs(period))]
  } else {
    # if period is a character vector, then the collapse timestamp arguments
    # are for *_date from lubridate
    args_date_funs <- c(
      methods::formalArgs(lubridate::floor_date),
      methods::formalArgs(lubridate::ceiling_date)
    )
    
    side_val <- rlang::eval_tidy(dots[['side']])
    if (is.null(side_val)) {side_val <- 'start'}
    dots['side'] <- NULL
    dots_collapse_timestamp <- dots[names(dots) %in% args_date_funs]
    dots_summarise_all <- dots[!(names(dots) %in% args_date_funs)]
  }
  
  # data processing
  data %>%
    dplyr::mutate(
      TIMESTAMP_coll = .data$TIMESTAMP,
      TIMESTAMP = .collapse_timestamp(
        .data$TIMESTAMP, !!! dots_collapse_timestamp,
        period = period, side = side_val
      )
    ) %>%
    dplyr::group_by(.data$TIMESTAMP) %>%
    dplyr::summarise_all(.funs = .funs, !!! dots_summarise_all) %>%
    dplyr::select(
      -dplyr::contains('_coll_'),
      -dplyr::contains('accumulated'),
      dplyr::contains('precip_accumulated')
    ) -> res

  return(res)

}


#' Metrics summary function
#'
#' Generate metrics from a site/s data for the period indicated
#'
#' @section Period:
#' \code{period} argument is used by internal function
#' \code{\link{.collapse_timestamp}} and it can be stated in two ways:
#' \itemize{
#'   \item{\emph{frequency period} format: "3 hours", "1 day", "7 days", "1 month"}
#'   \item{As a \emph{custom function}. This will be the name of a function,
#'   without quotes, that accepts as the first argument the timestamp to collapse.
#'   The result of the function must be a vector of collapsed TIMESTAMPs of the
#'   same length than the original TIMESTAMP which will be used to group by and
#'   summarise the data. Additional arguments to this function, if needed, can
#'   be passed in the \code{...} argument.}
#' }
#' \code{\link{.collapse_timestamp}} also accepts the \code{side} argument to
#' collapse by the starting timestamp or the ending timestamp of each group. This
#' can be supplied in the \code{...} argument.
#'
#' @section .funs:
#' \code{.funs} argument uses the same method as the \code{.funs} argument in the
#' \code{\link[dplyr]{summarise_all}} function of \code{dplyr} package. Basically
#' it accepts a list of function calls generated by list(). If you want to pass
#' on a custom function you can specify it here. See details in
#' \code{\link{summarise_by_period}} for more complex summarising functions
#' declaration.
#'
#' @section Interval:
#' Previously to the metrics summary, data can be filtered by an special
#' interval (i.e. predawn or nightly). This filtering can be specified with the
#'  \code{interval} argument as this:
#' \itemize{
#'   \item{\code{"general"} (default). No special interval is used, and metrics
#'         are performed with all the data}.
#'   \item{\code{"predawn"}. Data is filtered for predawn interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{"midday"}. Data is filtered for midday interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{"night"}. Data is filtered for night interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#'   \item{\code{"daylight"}. Data is filtered for daylight interval. In this case
#'         \code{int_start} and \code{int_end} must be specified as 24h value}
#' }
#'
#' @param sfn_data \code{\link{sfn_data}} or \code{\link{sfn_data_multi}} object
#'   to obtain the metrics from
#'
#' @param period Time period to aggregate data by. See period section for an
#'   explanation about the periods ('3 hours', '1 day', '1 month', '1 year', ...)
#'
#' @param .funs List of function calls to summarise the data by, see .funs
#'   section for more details.
#'
#' @param solar Logical indicating if the solarTIMESTAMP must be used instead of
#'   the site local TIMESTAMP. Default to TRUE (use solarTIMESTAMP).
#'
#' @param interval Character vector indicating if the metrics must be filtered
#'   by an special hour interval. See Interval section in details.
#'
#' @param int_start Integer value indicating the starting hour of the special
#'   interval in 24h format. See Interval section in details.
#'
#' @param int_end Integer value indicating the ending hour of the special
#'   interval in 24h format. See Interval section in details.
#'
#' @param ... optional arguments to pass to methods used
#'   (i.e. .collapse_timestamp or summarise funs extra arguments)
#'
#' @family metrics
#'
#' @return For \code{\link{sfn_data}} objects, a list of tbl_df objects
#'   with the following structure:
#'   \itemize{
#'     \item{$sapf: metrics for the sapflow data}
#'     \item{$env: metrics for the environmental data}
#'   }
#'
#'   For \code{\link{sfn_data_multi}} objects, a list of lists of tbl_df objects
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
#'   .funs = list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE), ~ n()),
#'   solar = FALSE,
#'   interval = 'general'
#' )
#'
#' str(ARG_TRE_metrics)
#' ARG_TRE_metrics[['sapf']]
#' ARG_TRE_metrics[['env']]
#'
#' ## sfn_data_multi
#' \donttest{
#' data('ARG_MAZ', package = 'sapfluxnetr')
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
#'
#' multi_metrics <- sfn_metrics(
#'   multi_sfn,
#'   period = '7 days',
#'   .funs = list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE), ~ n()),
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
#'   period = '1 day',
#'   .funs = list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE), ~ n()),
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
  assertthat::assert_that(
    inherits(sfn_data, c('sfn_data', 'sfn_data_multi')),
    msg = 'sfn_data must be a sfn_Data or sfn_data_multi object'
  )
  
  ## period style deprecated conversion ####
  if (is.character(period) && period %in% c('daily', 'hourly', 'monthly', 'yearly', 'weekly')) {
    
    warning("especifying period as '***ly' format (i.e. 'daily', 'monthly')
            is soft-deprecated since 0.0.6.9000 version. Please use the
            'frequency period' format instead: '1 day', '1 month'")
    
    period <- switch(
      period,
      'daily' = '1 day',
      'weekly' = '1 week',
      'monthly' = '1 month',
      'yearly' = '1 year',
      'hourly' = '1 hour',
      period
    )
  }

  # we need to check if multi and then repeat the function for each element
  if (inherits(sfn_data, 'sfn_data_multi')) {
    
    res_multi <- sfn_data %>%
      furrr::future_map(
      # purrr::map(
        sfn_metrics,
        period = period,
        .funs = .funs,
        solar = solar,
        interval = interval,
        int_start = int_start,
        int_end = int_end,
        ...,
        .progress = TRUE
      )

    return(res_multi)
  }

  # if sfn_data then we have to calculate the desired metrics from the data

  print(paste0(
    'Crunching data for ', get_si_code(sfn_data), '. In large datasets this ',
    'could take a while'
  ))

  sapf_data <- get_sapf_data(sfn_data, solar = solar) %>%
    # mutate to create the temporal columns timestep and period_minutes, to use
    # them in the data_coverage summarise function
    dplyr::mutate(
      timestep = get_plant_md(sfn_data)[['pl_sens_timestep']][1],
      period_minutes = .period_to_minutes(
        period, .data$TIMESTAMP, unique(.data$timestep)
      )
    )
  env_data <- get_env_data(sfn_data, solar = solar) %>%
    # mutate to create the temporal columns timestep and period_minutes, to use
    # them in the data_coverage summarise function
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

    # period_minutes modification specific for night intervals calculations
    new_period_minutes <- lubridate::as.duration(
      lubridate::hours(24 - (int_start - int_end))
    )@.Data / 60
    
    night_data <- whole_data %>%
      purrr::map(
        dplyr::filter,
        dplyr::between(
          lubridate::hour(.data$TIMESTAMP), int_start, 24
        ) |
          dplyr::between(
            lubridate::hour(.data$TIMESTAMP), 0, int_end - 1
          )
      ) %>% 
      purrr::map(
        dplyr::mutate,
        period_minutes = new_period_minutes
      )

    if (period == '1 day') {

      # TODO nightly_daily_fun
      period_summary <- night_data %>%
        purrr::map(
          summarise_by_period,
          .nightly_daily_cf,
          .funs,
          # .nightly_daily_cf args
          night_data = night_data, int_start = int_start,
          # dots
          ...
        )

    } else {

     period_summary <- night_data %>%
       purrr::map(
         dplyr::mutate,
         period_minutes = new_period_minutes * 30
       ) %>%
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
      
      # period_minutes modification specific for daytime intervals calculations
      new_period_minutes <- lubridate::as.duration(
        lubridate::hours(int_end - int_start)
      )@.Data / 60
      
      if (period != '1 day') {
        new_period_minutes <- new_period_minutes * 30
      }

      whole_data %>%
        purrr::map(
          dplyr::filter,
          dplyr::between(lubridate::hour(.data$TIMESTAMP), int_start, int_end - 1)
        ) %>%
        purrr::map(
          dplyr::mutate,
          period_minutes = new_period_minutes
        ) %>%
        purrr::map(summarise_by_period, period, .funs, ...) -> period_summary

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
#' generates a call to list to capture the fixed metrics in a quosure lambda
#' style
#' 
#' @section Metrics:
#' Calculated metrics are as follow:
#' 
#' \itemize{
#'   \item{mean: Mean value for the selected period}
#'   \item{sd: Standard deviation for the selected period}
#'   \item{coverage: Percentage of coverage as the percentage of no NAs in the
#'   expected length of the period, stated by the site timestep}
#'   \item{q*: q95 by default. Quantile value for the selected period. Quantiles to
#'   calculate are stated in the probs argument}
#'   \item{accumulated: Accumulated value on the selected period}
#'   \item{centroid: daily centroid value in the selected period, calculated
#'   only if centroid argument is TRUE}
#' }
#'
#' @param probs probs vector for quantile
#'
#' @param centroid logical indicating if centroid calculation must be made.
#'   (i.e. in monthly metrics, the centroid calculation is not needed)
#'
#' @importFrom stats sd
#' @importFrom stats quantile
#' 
#' @examples 
#' sapfluxnetr:::.fixed_metrics_funs(0.95, FALSE)
#' sapfluxnetr:::.fixed_metrics_funs(c(0.05, 0.95), TRUE)
#' 
#' @keywords internal

.fixed_metrics_funs <- function(probs, centroid) {

  # hack to avoid R CMD CHECKS to complain about . not being a global variable
  . <- NULL

  # we need magic to add the quantiles as they return more than one value
  # (usually). So lets play with quasiquotation
  quantile_args <- probs %>%
    purrr::map(
      function(x) {
        quantile_quo <- dplyr::quo(quantile(., probs = !!x, na.rm = TRUE))
        # dplyr .funs argument expect a list of formulas, not quosures:
        rlang::new_formula(
          NULL,
          rlang::quo_squash(quantile_quo),
          rlang::quo_get_env(quantile_quo)
        )
      }
    )
  names(quantile_args) <- paste0('q_', round(probs*100, 0))

  # we use rlang::list2 as we need the quantile spliced and evaluated with !!!
  .funs <- rlang::list2(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    coverage = ~ data_coverage(., .data$timestep, .data$period_minutes),
    !!! quantile_args,
    accumulated = ~ .accumulated_posix_aware(., na.rm = TRUE),
    centroid = ~ diurnal_centroid(.)
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
#'   \item{sd: standard deviation of the variable for the given period. NAs are
#'         removed}
#'   \item{coverage: Data coverage percentage (percentage of measures without
#'         NAs)}
#'   \item{q_XX: 0.XX quantile value for the period}
#'   \item{centroid: Diurnal centroid value (hours passed until the half of
#'         the summed daily value was reached). Only returned for sapflow
#'         measures when period is '1 day'}
#'   \item{accumulated: Accumulated values for precipitation only}
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
#' @param ... additional arguments to be passed to \code{\link{.collapse_timestamp}}
#'   or \code{\link[lubridate]{floor_date}} or \code{\link[lubridate]{ceiling_date}}.
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
#' \donttest{
#' # tidy daily metrics
#' ARG_TRE_daily <- daily_metrics(
#'   ARG_TRE, tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' ARG_TRE_daily
#' }
#'
#' @export

daily_metrics <- function(
  sfn_data,
  solar = TRUE,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  # hack for cran tests
  . <- NULL

  # hardcoded values
  period <- '1 day'

  # default funs
  .funs <- .fixed_metrics_funs(probs, TRUE)
  
  # pipe for avoid creating intermediate big objects
  # 
  # just input all in the sfn_function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'general',
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'general')
    } else {
      .
    }
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
#' \donttest{
#' # tidy monthly metrics
#' ARG_TRE_monthly <- monthly_metrics(
#'   ARG_TRE, tidy = TRUE, metadata = sfn_metadata_ex
#' )
#' ARG_TRE_monthly
#' }
#'
#' @export

monthly_metrics <- function(
  sfn_data,
  solar = TRUE,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  . <- NULL

  # hardcoded values
  period <- '1 month'

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'general',
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'general')
    } else {
      .
    }
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
#' \donttest{
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
#' }
#'
#' @export

nightly_metrics <- function(
  sfn_data,
  period = c('1 day', '1 month'),
  solar = TRUE,
  int_start = 20,
  int_end = 6,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  . <- NULL

  period <- match.arg(period)

  # default funs
  if (period == '1 day') {
    .funs <- .fixed_metrics_funs(probs, TRUE)
  } else {
    .funs <- .fixed_metrics_funs(probs, FALSE)
  }

  # just input all in the sfn_metrics function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'night',
    int_start = int_start,
    int_end = int_end,
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'night')
    } else {
      .
    }
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
#' \donttest{
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
#' }
#'
#' @export

daylight_metrics <- function(
  sfn_data,
  period = c('1 day', '1 month'),
  solar = TRUE,
  int_start = 6,
  int_end = 20,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  . <- NULL

  period <- match.arg(period)

  # default funs
  if (period == '1 day') {
    .funs <- .fixed_metrics_funs(probs, TRUE)
  } else {
    .funs <- .fixed_metrics_funs(probs, FALSE)
  }

  # just input all in the sfn_metrics function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'daylight',
    int_start = int_start,
    int_end = int_end,
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'daylight')
    } else {
      .
    }
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
#' \donttest{
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
#' }
#'
#' @export

predawn_metrics <- function(
  sfn_data,
  period = c('1 day', '1 month'),
  solar = TRUE,
  int_start = 4,
  int_end = 6,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  . <- NULL

  period <- match.arg(period)

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_metrics function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'predawn',
    int_start = int_start,
    int_end = int_end,
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'predawn')
    } else {
      .
    }
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
#' \donttest{
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
#' }
#'
#' @export

midday_metrics <- function(
  sfn_data,
  period = c('1 day', '1 month'),
  solar = TRUE,
  int_start = 11,
  int_end = 13,
  probs = c(0.95),
  tidy = FALSE,
  metadata = NULL,
  ...
) {
  
  . <- NULL

  period <- match.arg(period)

  # default funs
  .funs <- .fixed_metrics_funs(probs, FALSE)

  # just input all in the sfn_metrics function
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    interval = 'midday',
    int_start = int_start,
    int_end = int_end,
    ...
  ) %>% {
    # and tidyfy it if tidy is true
    if (tidy) {
      metrics_tidyfier(., metadata, interval = 'midday')
    } else {
      .
    }
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
#' \donttest{
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
#' }
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
  
  # get the metadata
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
  
  
  # we start modifying the sapflow data
  whole_data <- sapf_data %>%
    purrr::map(.sapflow_tidy) %>%
  
    # join sapf and env for all sites, by timestamp. As we have two lists, of
    # equal length, one with the modified sapflow data and another with the
    # env data, we join them together with map2
    purrr::map2(env_data, ~ dplyr::full_join(.x, .y, by = timestamp_var)) %>%
    
    # and finally we join all list elements with bind rows (controls for
    # missing variables and create them with the correct class), arranging by
    # TIMESTMAP
    dplyr::bind_rows() %>%
    dplyr::arrange(!!dplyr::sym(timestamp_var)) %>%
    
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
      dplyr::starts_with('TIMESTAMP'), "si_code", "pl_code",
      dplyr::starts_with('sapflow_'), dplyr::everything()
    )
  
  return(whole_data)
  
}


#### Utils for substituting tibbletime dependencies ####
#' .collapse_timestamp helper
#' 
#' Util to collapse the TIMESTAMP using lubridate::*_date functions
#' 
#' @section Period:
#' Periods accepted are in the "number period" format, as in
#' \code{\link[lubridate]{floor_date}} or a custom function name without quotes:
#' \itemize{
#'   \item{hours (exs. "1 hour", "12 hours")}
#'   \item{days (exs. "1 day", "3 days")}
#'   \item{weeks (exs. "1 week", "4 weeks")}
#'   \item{months (exs. "1 month", "6 months")}
#'   \item{years (exs. "1 year", "7 years")}
#'   \item{Also a custom function can be supplied, one that transforms the
#'   TIMESTAMP variable to the collapsed timestamp desired. See
#'   \code{\link{sfn_metrics}} for details}
#' }
#' 
#' @section Side:
#' Side indicates if using \code{\link[lubridate]{floor_date}} (side = "start) or
#' \code{\link[lubridate]{ceiling_date}} (side = 'end'). Ceiling dates is not
#' trivial, see \code{\link[lubridate]{ceiling_date}} for information on how
#' the date will be ceiled.
#' 
#' @return A vector of the same length as TIMESTAMP, with the collapsed timestamp
#'   with the same tz as the original one.
#' 
#' @keywords internal
#' 
#' @examples
#' arg_tre_timestamp <- get_timestamp(ARG_TRE)
#' sapfluxnetr:::.collapse_timestamp(
#'   arg_tre_timestamp, period = "1 day", side = 'start'
#' )
.collapse_timestamp <- function(timestamp, ..., period, side = 'start') {
  
  if (rlang::is_function(period)) {
    # if (rlang::is_empty(dots_list)) {
    #   collapsed_timestamp <- period(timestamp)
    # } else {
    #   collapsed_timestamp <- period(timestamp, !!! dots_list)
    # }
    collapsed_timestamp <- period(timestamp, ...)
  } else {
    # checks
    .assert_that_period_is_valid(period)
    assertthat::assert_that(
      assertthat::is.string(side),
      assertthat::are_equal(length(side), 1),
      side %in% c('start', 'end'),
      msg = "'side' must be a character vector of length one with accepted values 'start' or 'end'"
    )
    
    if (side == 'start') {
      collapsed_timestamp <- lubridate::floor_date(
        timestamp, period, ...
      )
    } else {
      collapsed_timestamp <- lubridate::ceiling_date(
        timestamp, period, ...
      )
    }
  }
  
  return(collapsed_timestamp)
}

#' .parse_period
#' 
#' Util to parse the period supplied to the function call to
#' create the collapsed timestamp
#' 
#' @param period Period in the "number period" format
#' 
#' @examples 
#' sapfluxnetr:::.parse_period('1 day')
#' 
#' @return  a list, with the freq value as numeric and the period value as
#'   character
#'
#' @keywords internal
.parse_period <- function(period) {
  
  .assert_that_period_is_valid(period)
  
  # split string
  splitted_period <- period %>%
    stringr::str_split(' ') %>%
    purrr::flatten_chr()
  
  period_freq <- as.numeric(splitted_period[1])
  period_char <- splitted_period[2]
  
  list(freq = period_freq, period = period_char)
  
}


#' test for character period
#' 
#' @param period "frequency period"
#' @examples 
#' sapfluxnetr:::.assert_that_period_is_valid('1 day') # TRUE
#' # not run
#' # sapfluxnetr:::.assert_that_period_is_valid('1day') # error
#' 
#' @return TRUE if is valid, informative error if not.
#' @keywords internal
.assert_that_period_is_valid <- function(period) {
  # check for character and length
  assertthat::assert_that(
    assertthat::is.string(period),
    assertthat::are_equal(length(period), 1),
    msg = '"period" must be character string of length 1. See help for more details'
  )
  
  # split string
  splitted_period <- period %>%
    stringr::str_split(' ') %>%
    purrr::flatten_chr()
  
  ## check that we have length 2
  assertthat::assert_that(
    assertthat::are_equal(length(splitted_period), 2),
    msg = '"period" must consist of a frequency and a period in the style: "2 days"'
  )
  
  ## check that 
  assertthat::assert_that(
    # Coercing to numeric should give a number, not NA
    suppressWarnings(!is.na(as.numeric(splitted_period[1]))),
    msg = "Frequency must be coercible to numeric."
  )
}

#' Period custom function
#' 
#' nightly period custom function, to use with nightly_metrics
#' 
#' This function will be supplied as "period" argument to summarise_by_period
#' when daily nightly metrics are requested.
#' 
#' @param timestamp TIMESTAMP
#' @param night_data as it comes inside of sfn_metrics
#' @param int_start interval start as supplied to sfn_metrics
#' 
#' @return a vector of the same length as TIMESTAMP with the collapsed values.
#' @keywords internal
.nightly_daily_cf <- function(
  timestamp,
  night_data, int_start
) {
  
  night_data[['sapf']] %>%
    dplyr::mutate(
      coll = .collapse_timestamp(
        .data$TIMESTAMP,
        period = '1 day',
        side = 'start'
      )
    ) %>%
    dplyr::group_by(.data$coll) %>%
    # closest to night start timestamp
    dplyr::summarise(
      custom_dates = .data$TIMESTAMP[which.min(
        abs(lubridate::hour(.data$TIMESTAMP) - int_start)
      )],
      TIMESTAMP = .data$custom_dates
    ) %>%
    dplyr::right_join(
      night_data[['sapf']] %>% dplyr::select("TIMESTAMP"), by = 'TIMESTAMP'
    ) %>%
    tidyr::fill("custom_dates") %>%
    dplyr::pull(.data$custom_dates) %>%
    lubridate::floor_date(unit = 'hour')
}
