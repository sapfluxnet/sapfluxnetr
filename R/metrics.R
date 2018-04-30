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
  dots_collapse_index <- dots[names(dots) %in% methods::formalArgs(tibbletime::collapse_index)]
  dots_summarise_all <- dots[!(names(dots) %in% methods::formalArgs(tibbletime::collapse_index))]

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
#' @section period:
#' \code{period} argument is piped to \code{tibbletime::collapse_index} function
#' with \code{side = 'end', clean = FALSE} options. See
#' \code{\link[tibbletime]{collapse_index}} for a detailed explanation but in
#' short:
#' \itemize{
#'   \item{\emph{frequency period} format: "1 day", "7 day", "1 month", "1 year"}
#'   \item{\emph{shorthand} format: "hourly", "daily", "monthly", "yearly"}
#'   \item{\emph{custom} format: a vector of dates to use as custom and more flexible boundaries}
#' }
#' Also, you can override the default behaviour of \code{sfn_metrics}, providing
#' \code{side = 'end'} or \code{clean = FALSE}.
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
#' @param general Logical indicating if general metrics (all data) must be
#'   returned
#'
#' @param predawn Logical indicating if metrics for predawn interval must be
#'   returned.
#'
#' @param pd_start Hour to start the predawn interval
#'
#' @param pd_end Hour to end the predawn interval
#'
#' @param midday Logical indicating if metrics for midday interval must be
#'   returned.
#'
#' @param md_start Hour to start the midday interval
#'
#' @param md_end Hour to end the midday interval
#'
#' @param nighttime Logical indicating if division between night and day id
#'   done and metrics for each subset (day and night) returned.
#'
#' @param night_start Hour to start the night interval
#'
#' @param night_end Hour to end the night interval
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
#'             \item{$sapf_day: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the slot will be empty)}
#'             \item{$sapf_night: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the slot will be empty)}
#'           }}
#'     \item{$env: metrics for the environmental data
#'           \itemize{
#'             \item{$env: general metrics}
#'             \item{$env_pd: metrics for predawn interval (if
#'                   \code{predawn = FALSE} the slot will be empty)}
#'             \item{$env_md: metrics for midday interval (if
#'                   \code{midday = FALSE} the slot will be empty)}
#'             \item{$env_day: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the slot will be empty)}
#'             \item{$env_night: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the slot will be empty)}
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
#'             \item{$sapf_day: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the solot will be empty)}
#'             \item{$sapf_night: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the solot will be empty)}
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
#'             \item{$env_day: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the solot will be empty)}
#'             \item{$env_night: metrics for diurnal interval (if
#'                   \code{nighttime = FALSE} the solot will be empty)}
#'           }}
#'       }
#'     }
#'     \item{$NEXT_SITE_CODE...}
#'   }
#'
#' @examples
#' library(dplyr)
#'
#' ## sfn_data
#' data('ARG_TRE', package = 'sapfluxnetr')
#' ARG_TRE_metrics <- sfn_metrics(
#'   ARG_TRE,
#'   period = '7 days',
#'   .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
#'   solar = FALSE,
#'   general = TRUE,
#'   predawn = TRUE,
#'   pd_start = 4,
#'   pd_end = 6,
#'   midday = TRUE,
#'   md_start = 12,
#'   md_end = 14,
#'   nighttime = TRUE,
#'   night_start = 20,
#'   night_end = 6,
#'   side = 'start'
#' )
#'
#' str(ARG_TRE_metrics)
#' ARG_TRE_metrics[['sapf']][['sapf_pd']]
#' ARG_TRE_metrics[['env']][['env_night']]
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
#'   general = TRUE,
#'   predawn = TRUE,
#'   pd_start = 4,
#'   pd_end = 6,
#'   midday = TRUE,
#'   md_start = 12,
#'   md_end = 14,
#'   nighttime = FALSE,
#'   side = 'start'
#' )
#'
#' str(multi_metrics)
#'
#' multi_metrics[['ARG_TRE']][['sapf']][['sapf_pd']]
#' }
#'
#' @export

# sfn_metrics <- function(
#   sfn_data,
#   period,
#   .funs,
#   solar,
#   general,
#   predawn,
#   pd_start,
#   pd_end,
#   midday,
#   md_start,
#   md_end,
#   nighttime,
#   night_start,
#   night_end,
#   ...
# ) {
#
#   # argument checks
#   if (!(class(sfn_data) %in% c('sfn_data', 'sfn_data_multi'))) {
#     stop(
#       'sfn_metrics only works with sfn_data and sfn_data_multi object classes'
#     )
#   }
#
#   # we need to check if multi and then repeat the function for each element
#   if (is(sfn_data, 'sfn_data_multi')) {
#     res_multi <- sfn_data %>%
#       purrr::map(sfn_metrics,
#         period = period,
#         .funs = .funs,
#         solar = solar,
#         general = general,
#         predawn = predawn,
#         pd_start = pd_start,
#         pd_end = pd_end,
#         midday = midday,
#         md_start = md_start,
#         md_end = md_end,
#         nighttime = nighttime,
#         night_start = night_start,
#         night_end = night_end,
#         ...
#       )
#
#     return(res_multi)
#   }
#
#   # if sfn_data then we have to calculate the desired metrics from the data
#
#   print(paste0(
#     'Crunching data for ', get_si_code(sfn_data), '. In large datasets this ',
#     'could take a while'
#   ))
#
#   sapf_data <- get_sapf_data(sfn_data, solar = solar)
#   env_data <- get_env_data(sfn_data, solar = solar)
#
#   whole_data <- list(sapf = sapf_data, env = env_data)
#
#   if (general) {
#     # progress to not scare seeming like freezed
#     print(paste0('General data for ', get_si_code(sfn_data)))
#
#     # period summaries: we want to know period means, maximum, minimum, quantiles...
#     whole_data %>%
#       purrr::map(summarise_by_period, period, .funs, ...) -> period_summary
#   } else {
#     period_summary <- NULL
#   }
#
#   # filtering summaries we want to know filtered period (predawn, midday) means,
#   # maximum, minimum, quantiles...
#   # `summarise_by_period` is a helper function documented in helpers.R
#
#   # predawn
#   if (predawn) {
#
#     # progress to not scare seeming like freezed
#     print(paste0('Predawn data for ', get_si_code(sfn_data)))
#
#     whole_data %>%
#       purrr::map(
#         dplyr::filter,
#         dplyr::between(lubridate::hour(.data$TIMESTAMP), pd_start, pd_end)
#       ) %>%
#       purrr::map(summarise_by_period, period, .funs, ...) -> predawn_summary
#
#     names(predawn_summary) <- paste0(names(predawn_summary), '_pd')
#     names(predawn_summary[['sapf_pd']]) <- paste0(names(predawn_summary[['sapf_pd']]), '_pd')
#     names(predawn_summary[['env_pd']]) <- paste0(names(predawn_summary[['env_pd']]), '_pd')
#
#   } else {
#     predawn_summary <- NULL
#   }
#
#   # midday
#   if (midday) {
#
#     # progress to not scare seeming like freezed
#     print(paste0('Midday data for ', get_si_code(sfn_data)))
#
#     whole_data %>%
#       purrr::map(
#         dplyr::filter,
#         dplyr::between(lubridate::hour(.data$TIMESTAMP), md_start, md_end)
#       ) %>%
#       purrr::map(summarise_by_period, period, .funs, ...) -> midday_summary
#
#     names(midday_summary) <- paste0(names(midday_summary), '_md')
#     names(midday_summary[['sapf_md']]) <- paste0(names(midday_summary[['sapf_md']]), '_md')
#     names(midday_summary[['env_md']]) <- paste0(names(midday_summary[['env_md']]), '_md')
#
#   } else {
#     midday_summary <- NULL
#   }
#
#   #### night time ####
#   if (nighttime) {
#
#     # progress to not scare seeming like freezed in large datasets
#     print(paste0('Nighttime data for ', get_si_code(sfn_data)))
#
#     night_data <- whole_data %>%
#       purrr::map(
#         dplyr::filter,
#         dplyr::between(
#           lubridate::hour(.data$TIMESTAMP), night_start, 24
#         ) |
#           dplyr::between(
#             lubridate::hour(.data$TIMESTAMP), 0, night_end - 1
#           )
#       )
#
#
#
#     if (period == 'daily') {
#
#       night_boundaries <- night_data[['sapf']] %>%
#         dplyr::mutate(
#           coll = tibbletime::collapse_index(
#             index = .data$TIMESTAMP,
#             period = period,
#             side = 'start'
#           )
#         ) %>%
#         dplyr::group_by(.data$coll) %>%
#         # closest to night start timestamp
#         dplyr::summarise(
#           custom_dates = .data$TIMESTAMP[which.min(
#             abs(lubridate::hour(.data$TIMESTAMP) - night_start)
#           )]
#         ) %>%
#         dplyr::pull(.data$custom_dates)
#
#       night_boundaries <- night_boundaries %>%
#         lubridate::floor_date(unit = 'hours')
#
#       # workaround the two nights in one day problem (sites that start at
#       # 00:00:00 have two nights in the same day, the first one corresponds to
#       # the previous day and we have to fix it)
#       margins <- 60*60*24
#       extra_start_boundary <- night_boundaries[[1]] - margins
#       extra_end_boundary <- night_boundaries[[length(night_boundaries)]] + margins
#       night_boundaries <- as.POSIXct(
#         c(
#           as.character(extra_start_boundary),
#           as.character(night_boundaries),
#           as.character(extra_end_boundary)
#         ),
#         tz = attr(extra_start_boundary, 'tz')
#       )
#
#       night_sum <- night_data %>%
#         purrr::map(
#           summarise_by_period, night_boundaries, .funs,
#           # clean = FALSE, !!! dots_summ,
#           ...
#         )
#
#     } else {
#
#       # night_boundaries <- night_boundaries %>%
#       #   lubridate::floor_date(unit = 'hourly')
#       #
#       # # workaround the two nights in one month problem (sites that start at
#       # # XXXX-XX-01 00:00:00 have two nights in the same month, the first one
#       # # corresponds to the previous month and we have to fix it)
#       # margins <- 60*60*24
#       # extra_start_boundary <- night_boundaries[[1]] - margins
#       # extra_end_boundary <- night_boundaries[[length(night_boundaries)]] + margins
#       # night_boundaries <- as.POSIXct(
#       #   c(
#       #     as.character(extra_start_boundary),
#       #     as.character(night_boundaries),
#       #     as.character(extra_end_boundary)
#       #   ),
#       #   tz = attr(extra_start_boundary, 'tz')
#       # )
#
#
#       night_sum <- night_data %>%
#         purrr::map(
#           summarise_by_period, period, .funs,
#           # clean = FALSE, !!! dots_summ,
#           ...
#         )
#
#     }
#
#     names(night_sum[['sapf']]) <- paste0(names(night_sum[['sapf']]), '_night')
#     names(night_sum[['env']]) <- paste0(names(night_sum[['env']]), '_night')
#
#     day_sum <- whole_data %>%
#       purrr::map2(
#         night_data,
#         ~ dplyr::anti_join(.x, .y, by = 'TIMESTAMP')
#       ) %>%
#       purrr::map(summarise_by_period, period, .funs, ...)
#
#     names(day_sum[['sapf']]) <- paste0(names(day_sum[['sapf']]), '_day')
#     names(day_sum[['env']]) <- paste0(names(day_sum[['env']]), '_day')
#
#     nighttime_summary <- list(
#       sapf = list(
#         sapf_day = day_sum[['sapf']],
#         sapf_night = night_sum[['sapf']]
#       ),
#       env = list(
#         env_day = day_sum[['env']],
#         env_night = night_sum[['env']]
#       )
#     )
#   } else {
#     nighttime_summary <- NULL
#   }
#
#   # we create the result object:
#   # res
#   #   $sapf
#   #     $sapf_gen
#   #     $sapf_pd
#   #     $sapf_md
#   #     $sapf_day
#   #     $sapf_night
#   #   $env
#   #     $env_gen
#   #     $env_pd
#   #     $env_md
#   #     $env_day
#   #     $env_night
#   # this way all is modular and after that they can be combined by bind_cols
#   res <- list(
#     sapf = list(
#       sapf_gen = period_summary[['sapf']],
#       sapf_pd = predawn_summary[['sapf_pd']],
#       sapf_md = midday_summary[['sapf_md']],
#       sapf_day = nighttime_summary[['sapf']][['sapf_day']],
#       sapf_night = nighttime_summary[['sapf']][['sapf_night']]
#     ),
#     env = list(
#       env_gen = period_summary[['env']],
#       env_pd = predawn_summary[['env_pd']],
#       env_md = midday_summary[['env_md']],
#       env_day = nighttime_summary[['env']][['env_day']],
#       env_night = nighttime_summary[['env']][['env_night']]
#     )
#   )
#
#   # remove the NULLs
#   return(
#     res %>%
#       purrr::modify_depth(1, ~ purrr::keep(.x, ~ !is.null(.)))
#   )
# }


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

  sapf_data <- get_sapf_data(sfn_data, solar = solar)
  env_data <- get_env_data(sfn_data, solar = solar)

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
        purrr::map(summarise_by_period, period, .funs, ...) -> period_summary

      # now we need to create the names with the interval
      # predawn
      if (interval == 'predawn') {
        names(period_summary[['sapf']]) <- paste0(names(period_summary[['sapf']]), '_pd')
        names(period_summary[['env']]) <- paste0(names(period_summary[['env']]), '_pd')
      }

      # midday
      if (interval == 'midday') {
        names(period_summary[['sapf']]) <- paste0(names(period_summary[['sapf']]), '_md')
        names(period_summary[['env']]) <- paste0(names(period_summary[['env']]), '_md')
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

  # remove the NULLs
  # return(
  #   period_summary %>%
  #     purrr::modify_depth(1, ~ purrr::keep(.x, ~ !is.null(.)))
  # )

  return(period_summary)
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
    coverage = data_coverage(.),
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
#'         the summed daily value was reached). Only returned when period
#'         is 'daily' and data is from sapflow}
#' }
#'
#' @param probs numeric vector of probabilities for
#'   \code{\link[stats]{quantile}}
#'
#' @param tidy Logical indicating if the metrics must be returned in a tidy
#'   format (a long tibble, each observation in its own row)
#'
#' @param metadata metadata object. Only used if tidy is TRUE
#'
#' @family metrics
#'
#' @return If \code{tidy} is TRUE (default), a tibble with the metrics for
#'   sapflow and environmental data, with all the metadata included. If
#'   \code{tidy} is FALSE, a list of tibbles with the calculated metrics.
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
#' # default complete daily metrics
#' ARG_TRE_daily <- daily_metrics(ARG_TRE, metadata = sfn_metadata_ex)
#'
#' str(ARG_TRE_daily)
#'
#' # non tidy raw metrics
#' ARG_TRE_raw_daily <- daily_metrics(ARG_TRE, tidy = FALSE)
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
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'gen')
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
#' data('ARG_MAZ', package = 'sapfluxnetr')
#'
#' # default complete monthly metrics
#' ARG_MAZ_monthly <- monthly_metrics(ARG_MAZ)
#'
#' str(ARG_MAZ_monthly)
#' ARG_MAZ_monthly[['env']][['env_gen']]
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
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'gen')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}

#' @rdname metrics
#'
#' @section nightly_metrics:
#' \code{nightly_metrics} will always return the metrics for day and night
#' periods, summarised daily or monthly
#'
#' Night for daily period starts in DOY x and ends in DOY x+1 (i.e. if
#' \code{night_start = 20, night_end = 6} values for 2018-03-28 20:00:00 means
#' values for the night starting at 2018-03-28 20:00:00 and ending at
#' 2018-03-29 06:00:00).
#'
#' Night for monthly period summarises all night periods in the month, that
#' includes from 00:00:00 of the first month night to 23:59:59 of the last
#' month night
#'
#' @inheritParams sfn_metrics
#'
#' @examples
#' ## nightly_metrics
#' # data load
#' data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
#'
#' # nightly monthly metrics
#' AUS_CAN_ST2_MIX_monthly <- nightly_metrics(ARG_MAZ, period = 'monthly')
#'
#' str(AUS_CAN_ST2_MIX_monthly)
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_day']]
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_night']]
#'
#' # change the night interval
#' AUS_CAN_ST2_MIX_daily_short <- nightly_metrics(
#'   ARG_MAZ,
#'   night_start = 21, night_end = 4 # night starting and ending hour
#' )
#'
#' AUS_CAN_ST2_MIX_daily_short[['env']][['env_night']]
#'
#' @export

nightly_metrics <- function(
  sfn_data,
  period = c('daily', 'monthly'),
  solar = TRUE,
  night_start = 20,
  night_end = 6,
  probs = c(0.95, 0.99),
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
  sfn_metrics(
    sfn_data,
    period = period,
    .funs = .funs,
    solar = solar,
    general = FALSE,
    predawn = FALSE,
    midday = FALSE,
    nighttime = TRUE,
    night_start = night_start,
    night_end = night_end,
    ...
  )
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
#' # predawn monthly metrics
#' AUS_CAN_ST2_MIX_monthly <- predawn_metrics(ARG_MAZ, period = 'monthly')
#'
#' str(AUS_CAN_ST2_MIX_monthly)
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_day']]
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_night']]
#'
#' # change the predawn interval
#' AUS_CAN_ST2_MIX_daily_short <- predawn_metrics(
#'   ARG_MAZ,
#'   pd_start = 3, pd_end = 5 # predawn starting and ending hour
#' )
#'
#' AUS_CAN_ST2_MIX_daily_short[['env']][['env_night']]
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
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'pd')
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
#' # midday monthly metrics
#' AUS_CAN_ST2_MIX_monthly <- midday_metrics(ARG_MAZ, period = 'monthly')
#'
#' str(AUS_CAN_ST2_MIX_monthly)
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_day']]
#' AUS_CAN_ST2_MIX_monthly[['env']][['env_night']]
#'
#' # change the midday interval
#' AUS_CAN_ST2_MIX_daily_short <- midday_metrics(
#'   ARG_MAZ,
#'   pd_start = 3, pd_end = 5 # midday starting and ending hour
#' )
#'
#' AUS_CAN_ST2_MIX_daily_short[['env']][['env_night']]
#'
#' @export

midday_metrics <- function(
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
    interval = 'midday',
    int_start = int_start,
    int_end = int_end,
    ...
  )

  if (tidy) {
    res_tidy <- metrics_tidyfier(res_raw, metadata, interval = 'md')
    return(res_tidy)
  } else {
    return(res_raw)
  }
}
