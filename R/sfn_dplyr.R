#' Filter sfn_data by variable/s value
#'
#' Port of \code{\link[dplyr]{filter}} for \code{sfn_data} and
#' \code{sfn_data_multi} objects
#'
#' `sfn_filter` will remove the rows not matching the logical expression/s
#' provided. So, it will remove cases and will create TIMESTAMP gaps, so its use
#' is not recommended except in the case of filtering by TIMESTAMP (i.e. to
#' set several sites (sfn_data_multi) in the same time frame). For other
#' scenarios (removing extreme environmental conditions values or strange
#' sapflow measures patterns) see \code{\link{sfn_mutate}} and
#' \code{\link{sfn_mutate_at}}
#'
#' @param sfn_data \code{sfn_data} or \code{sfn_data_multi} object to subset
#'
#' @param ... expressions to pass to the \code{\link[dplyr]{filter}} function
#'
#' @param solar Logical indicating if solar timestamp must used to subset
#'
#' @return For \code{sfn_data} objects, a filtered \code{sfn_data} or NULL if
#'   no data meet the criteria. For \code{sfn_data_multi} another
#'   \code{sfn_data_multi} with the sites filtered, and an empty
#'   \code{sfn_data_multi} if any sites met the criteria
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # data
#' data('FOO', package = 'sapfluxnetr')
#'
#' # by timestamp
#' foo_timestamp <- get_timestamp(FOO)
#'
#' foo_timestamp_trimmed <- foo_timestamp[1:100]
#'
#' sfn_filter(
#'   FOO,
#'   TIMESTAMP %in% foo_timestamp_trimmed
#' )
#'
#' # by wind speed value
#' ws_threshold <- 25
#'
#' sfn_filter(
#'   FOO,
#'   ws <= ws_threshold
#' )
#'
#' ## multi
#' data('BAR', package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(FOO, BAR)
#'
#' # by timestamp
#' sfn_filter(
#'   multi_sfn,
#'   between(day(TIMESTAMP), 18, 22)
#' )
#'
#' # by wind speed value
#' sfn_filter(
#'   multi_sfn,
#'   ws <= ws_threshold
#' )
#'
#' @export

sfn_filter <- function(sfn_data, ..., solar = FALSE) {

  if (is(sfn_data, 'sfn_data_multi')) {
    res_multi <- purrr::map(sfn_data, sfn_filter, ..., solar = solar) %>%
      purrr::discard(is.null)

    if (length(res_multi) < 1) {
      warning('Any sites met the criteria, returning empty results')
      return(sfn_data_multi())
    }

    return(as_sfn_data_multi(res_multi))
  }

  sapf_data <- get_sapf_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()
  env_data <- get_env_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()

  whole_data <- dplyr::inner_join(sapf_data, env_data, by = 'TIMESTAMP')

  filtered_data <- dplyr::filter(whole_data, ...)

  # if filter throws no results, warning and NULL (to remove it in multi)
  if (nrow(filtered_data) < 1) {
    warning(get_si_code(sfn_data), " didn't met the subset criteria, removing",
            " it from results and returning NULL.")
    return(NULL)
  }

  sapf_data_mod <- dplyr::select(filtered_data, names(sapf_data))
  env_data_mod <- dplyr::select(filtered_data, names(env_data))

  sapf_flags_mod <- dplyr::semi_join(
    get_sapf_flags(sfn_data, solar = solar), sapf_data_mod, by = 'TIMESTAMP'
  ) %>%
    tibble::as_tibble()

  env_flags_mod <- dplyr::semi_join(
    get_env_flags(sfn_data, solar = solar), env_data_mod, by = 'TIMESTAMP'
  ) %>%
    tibble::as_tibble()

  if (solar) {
    solar_timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_solar_timestamp(sfn_data) %in% solar_timestamp_mod, arr.ind = TRUE
    )
    timestamp_mod <- get_timestamp(sfn_data)[index_timestamp]
  } else {
    timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_timestamp(sfn_data) %in% timestamp_mod, arr.ind = TRUE
    )
    solar_timestamp_mod <- get_solar_timestamp(sfn_data)[index_timestamp]
  }

  # build the trimmed sfn_data
  res <- sfn_data(
    sapf_data = sapf_data_mod[,-1],
    env_data = env_data_mod[,-1],
    sapf_flags = sapf_flags_mod[,-1],
    env_flags = env_flags_mod[,-1],
    si_code = get_si_code(sfn_data),
    timestamp = timestamp_mod,
    solar_timestamp = solar_timestamp_mod,
    site_md = get_site_md(sfn_data),
    stand_md = get_stand_md(sfn_data),
    species_md = get_species_md(sfn_data),
    plant_md = get_plant_md(sfn_data),
    env_md = get_env_md(sfn_data)
  )

  return(res)

}

#' Mutate variables by function
#'
#' Port of \code{\link[dplyr]{mutate}} for \code{sfn_data} and
#' \code{sfn_data_multi} objects
#'
#' `sfn_mutate` function will maintain the same number of rows before and after
#' the modification, so it is well suited to modify variables without creating
#' TIMESTAMP gaps (i.e. to change variable units). For mutating groups of
#' variables at the same time see \code{\link{sfn_mutate_at}}.
#'
#' @section Sapflow and environmental variables:
#' `sfn_mutate` internally joins the sapflow and environmental datasets by the
#' TIMESTAMP, so it is possible to mutate variables conditionally between
#' sapflow and environmental measures (i.e. mutate sapflow when wind is high or
#' radiation is zero). Due to this, at the moment any new variable is dropped
#' when building the final results, so this is ONLY intended to mutate
#' exisiting variables without changing the names.
#'
#' @param sfn_data \code{sfn_data} or \code{sfn_data_multi} object to subset
#'
#' @param ... Name-value pairs of expressions to pass to the
#'   \code{\link[dplyr]{mutate}} function.
#'
#' @param solar Logical indicating if solar timestamp must used to subset
#'
#' @return For \code{sfn_data} objects, a mutated \code{sfn_data}. For
#'   \code{sfn_data_multi} another \code{sfn_data_multi} with the sites mutated
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # data
#' data('FOO', package = 'sapfluxnetr')
#'
#' # transform to NAs any wind value above 25
#' ws_threshold <- 25
#' sfn_mutate(FOO, ws = if_else(ws > 25, NA_real_, ws))
#'
#' ## multi
#' data(BAR, package = 'sapfluxnetr')
#' data(BAZ, package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(FOO, BAR, BAZ)
#'
#' multi_sfn_mutated <- sfn_mutate(
#'   multi_sfn, ws = if_else(ws > 25, NA_real_, ws)
#' )
#'
#' multi_sfn_mutated[['FOO']]
#'
#' @export

sfn_mutate <- function(sfn_data, ..., solar = FALSE) {

  # if multi iterate along the multi elements
  if (is(sfn_data, 'sfn_data_multi')) {
    res_multi <- purrr::map(sfn_data, sfn_mutate, ..., solar = solar) %>%
      as_sfn_data_multi()
    return(res_multi)
  }

  # get sapf and env and join them to be able to use only one mutate statement
  sapf_data <- get_sapf_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()
  env_data <- get_env_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()

  whole_data <- dplyr::inner_join(sapf_data, env_data, by = 'TIMESTAMP')

  # mutate whole data
  mutated_data <- dplyr::mutate(whole_data, ...)

  # separate in sapf and env again
  # TODO separation is based in old names, but mutate can generate new names
  # that we need to distribute in the sapf or env data
  sapf_data_mod <- dplyr::select(mutated_data, names(sapf_data))
  env_data_mod <- dplyr::select(mutated_data, names(env_data))

  # get the differences between old and new and flag the variables mutated
  sapf_data_vars_mod <- names(base::setdiff(
    as.data.frame(sapf_data_mod),
    as.data.frame(sapf_data)
  ))
  env_data_vars_mod <- names(base::setdiff(
    as.data.frame(env_data_mod),
    as.data.frame(env_data)
  ))

  if (length(sapf_data_vars_mod) > 0) {
    sapf_flags_mod <- get_sapf_flags(sfn_data) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(sapf_data_vars_mod, .flag)
  } else {
    sapf_flags_mod <- get_sapf_flags(sfn_data) %>%
      tibble::as_tibble()
  }

  if (length(env_data_vars_mod) > 0) {
    env_flags_mod <- get_env_flags(sfn_data) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(env_data_vars_mod, .flag)
  } else {
    env_flags_mod <- get_env_flags(sfn_data)
  }

  # just in case the TIMESTAMP was mutated, update it
  if (solar) {
    solar_timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_solar_timestamp(sfn_data) %in% solar_timestamp_mod, arr.ind = TRUE
    )
    timestamp_mod <- get_timestamp(sfn_data)[index_timestamp]
  } else {
    timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_timestamp(sfn_data) %in% timestamp_mod, arr.ind = TRUE
    )
    solar_timestamp_mod <- get_solar_timestamp(sfn_data)[index_timestamp]
  }

  # build the mutated sfn_data
  res <- sfn_data(
    sapf_data = sapf_data_mod[,-1],
    env_data = env_data_mod[,-1],
    sapf_flags = sapf_flags_mod[,-1],
    env_flags = env_flags_mod[,-1],
    si_code = get_si_code(sfn_data),
    timestamp = timestamp_mod,
    solar_timestamp = solar_timestamp_mod,
    site_md = get_site_md(sfn_data),
    stand_md = get_stand_md(sfn_data),
    species_md = get_species_md(sfn_data),
    plant_md = get_plant_md(sfn_data),
    env_md = get_env_md(sfn_data)
  )

  return(res)

}

#' Mutate selected columns by function
#'
#' Port of \code{\link[dplyr]{mutate_at}} for \code{sfn_data} and
#' \code{sfn_data_multi} objects
#'
#' `sfn_mutate_at` function will maintain the same number of rows before and
#' after the modification, so it is well suited to modify variables without
#' creating TIMESTAMP gaps (i.e. to change variable units). For mutating
#' indiviudal variables see \code{\link{sfn_mutate}}.
#'
#' @param sfn_data \code{sfn_data} or \code{sfn_data_multi} object to subset
#'
#' @param .vars Variables to mutate. Passed to \code{\link[dplyr]{mutate_at}}
#'
#' @param .funs Function/s for mutate, passed to \code{\link[dplyr]{mutate_at}}
#'
#' @param ... Extra arguments to pass to the functions in \code{.funs}, passed
#'   to \code{\link[dplyr]{mutate_at}}.
#'
#' @param solar Logical indicating if solar timestamp must used to subset
#'
#' @return For \code{sfn_data} objects, a mutated \code{sfn_data}. For
#'   \code{sfn_data_multi} another \code{sfn_data_multi} with the sites mutated
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # data
#' data('FOO', package = 'sapfluxnetr')
#'
#' # transform to NAs any sapflow value occured with wind speed above 25
#' ws_threshold <- 25
#' # get the names of the variables to mutate (tree names)
#' vars_to_mutate <- names(get_sapf_data(FOO)[,-1]) # no TIMESTAMP
#'
#' sfn_mutate_at(
#'   FOO,
#'   .vars = vars(one_of(vars_to_mutate)),
#'   .funs = funs(
#'     case_when(
#'       ws > ws_threshold ~ NA_real_,
#'       TRUE ~ .
#'     )
#'   )
#' )
#'
#' ## multi
#' data(BAR, package = 'sapfluxnetr')
#' data(BAZ, package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(FOO, BAR, BAZ)
#'
#' ## in multi it's better to discard the variables to not mutate:
#' vars_to_not_mutate <- names(get_env_data(FOO)) # all the environmental
#'
#' multi_sfn_mutated <- sfn_mutate_at(
#'   multi_sfn,
#'   .vars = vars(-one_of(vars_to_not_mutate)), # we use -
#'   .funs = funs(
#'     case_when(
#'       ws > ws_threshold ~ NA_real_,
#'       TRUE ~ .
#'     )
#'   )
#' )
#'
#' multi_sfn_mutated[['FOO']]
#'
#' @export

sfn_mutate_at <- function(sfn_data, .vars, .funs, ..., solar = FALSE) {

  # if multi, iterate along the multi elements
  if (is(sfn_data, 'sfn_data_multi')) {
    res_multi <- purrr::map(
      sfn_data, sfn_mutate_at,
      .vars, .funs, ..., solar = solar
    ) %>%
      as_sfn_data_multi()
    return(res_multi)
  }

  # get sapf and env data
  sapf_data <- get_sapf_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()
  env_data <- get_env_data(sfn_data, solar = solar) %>%
    tibble::as_tibble()
  # create the whole data to be able to mutate sap based on env and viceversa
  whole_data <- dplyr::inner_join(sapf_data, env_data, by = 'TIMESTAMP')

  # mutate data at will
  mutated_data <- dplyr::mutate_at(whole_data, .vars, .funs, ...)

  # separate data
  sapf_data_mod <- dplyr::select(mutated_data, names(sapf_data))
  env_data_mod <- dplyr::select(mutated_data, names(env_data))

  # look for differences between old and new to flag the variables changed
  sapf_data_vars_mod <- names(base::setdiff(
    as.data.frame(sapf_data_mod),
    as.data.frame(sapf_data)
  ))
  env_data_vars_mod <- names(base::setdiff(
    as.data.frame(env_data_mod),
    as.data.frame(env_data)
  ))

  # flag the variables mutated (all values) (.flag is documented in helpers.R)
  if (length(sapf_data_vars_mod) > 0) {
    sapf_flags_mod <- get_sapf_flags(sfn_data) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(sapf_data_vars_mod, .flag)
  } else {
    sapf_flags_mod <- get_sapf_flags(sfn_data) %>%
      tibble::as_tibble()
  }

  if (length(env_data_vars_mod) > 0) {
    env_flags_mod <- get_env_flags(sfn_data) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(env_data_vars_mod, .flag)
  } else {
    env_flags_mod <- get_env_flags(sfn_data)
  }

  # in case TIMESTAMP was mutated, update it
  if (solar) {
    solar_timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_solar_timestamp(sfn_data) %in% solar_timestamp_mod, arr.ind = TRUE
    )
    timestamp_mod <- get_timestamp(sfn_data)[index_timestamp]
  } else {
    timestamp_mod <- dplyr::pull(sapf_data_mod, .data$TIMESTAMP)
    index_timestamp <- which(
      get_timestamp(sfn_data) %in% timestamp_mod, arr.ind = TRUE
    )
    solar_timestamp_mod <- get_solar_timestamp(sfn_data)[index_timestamp]
  }

  # build the mutated sfn_data
  res <- sfn_data(
    sapf_data = sapf_data_mod[,-1],
    env_data = env_data_mod[,-1],
    sapf_flags = sapf_flags_mod[,-1],
    env_flags = env_flags_mod[,-1],
    si_code = get_si_code(sfn_data),
    timestamp = timestamp_mod,
    solar_timestamp = solar_timestamp_mod,
    site_md = get_site_md(sfn_data),
    stand_md = get_stand_md(sfn_data),
    species_md = get_species_md(sfn_data),
    plant_md = get_plant_md(sfn_data),
    env_md = get_env_md(sfn_data)
  )

  return(res)

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
#'     \item{Tree}
#'   }
#'
#' @examples
#' # not yet
#'
#' @export

metrics_tidyfier <- function(metrics_res, metadata, interval = 'gen') {

  # individual data objects
  sapf_data <- metrics_res %>%
    purrr::map(c('sapf', paste0('sapf_', interval)))

  env_data <- metrics_res %>%
    purrr::map(c('env', paste0('env_', interval)))

  plant_md <- metadata[['plant_md']] %>%
    filter(si_code %in% names(metrics_res))

  site_md <- metadata[['site_md']] %>%
    filter(si_code %in% names(metrics_res))

  stand_md <- metadata[['stand_md']] %>%
    filter(si_code %in% names(metrics_res))

  species_md <- metadata[['species_md']] %>%
    filter(si_code %in% names(metrics_res))

  env_md <- metadata[['env_md']] %>%
    filter(si_code %in% names(metrics_res))

  # whole data object

  env_vars_names <- .env_vars_names()

  env_vars_to_exclude_from_gather <- dplyr::vars(
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

  sapf_data %>%
    purrr::map2(env_data, ~ full_join(.x, .y, by = 'TIMESTAMP')) %>%
    purrr::map(
      ~ tidyr::gather(
        .x, Tree, Sapflow, -TIMESTAMP, !!!env_vars_to_exclude_from_gather
      )
    ) %>%
    purrr::map(tibble::as.tibble) %>%
    .multi_union() %>%
    dplyr::arrange(.data$TIMESTAMP) %>%
    # mutate to get the plant code. Is tricky as we have to separate the metric
    # and interval labels at the end of the Tree column
    dplyr::mutate(
      Metric = stringr::str_sub(
        .data$Tree, stringr::str_locate(.data$Tree, "(Js|Jt)_[0-9]*")[,2] + 2, -1
      ),
      pl_code = stringr::str_sub(
        .data$Tree, 1, stringr::str_locate(.data$Tree, "(Js|Jt)_[0-9]*")[,2]
      )
    ) %>%
    dplyr::left_join(plant_md, by = 'pl_code') %>%
    dplyr::left_join(site_md, by = 'si_code') %>%
    dplyr::left_join(stand_md, by = 'si_code') %>%
    dplyr::left_join(plant_md, by = 'si_code') %>%
    dplyr::left_join(env_md, by = 'si_code') %>%

}
