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
#' data('ARG_TRE', package = 'sapfluxnetr')
#'
#' # by timestamp
#' foo_timestamp <- get_timestamp(ARG_TRE)
#'
#' foo_timestamp_trimmed <- foo_timestamp[1:100]
#'
#' sfn_filter(
#'   ARG_TRE,
#'   TIMESTAMP %in% foo_timestamp_trimmed
#' )
#'
#' # by wind speed value
#' ws_threshold <- 25
#'
#' sfn_filter(
#'   ARG_TRE,
#'   ws <= ws_threshold
#' )
#'
#' ## multi
#' data('ARG_MAZ', package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ)
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
#' data('ARG_TRE', package = 'sapfluxnetr')
#'
#' # transform to NAs any wind value above 25
#' ws_threshold <- 25
#' sfn_mutate(ARG_TRE, ws = if_else(ws > 25, NA_real_, ws))
#'
#' ## multi
#' data(ARG_MAZ, package = 'sapfluxnetr')
#' data(AUS_CAN_ST2_MIX, package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
#'
#' multi_sfn_mutated <- sfn_mutate(
#'   multi_sfn, ws = if_else(ws > 25, NA_real_, ws)
#' )
#'
#' multi_sfn_mutated[['ARG_TRE']]
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
  sapf_data_vars_mod <- names(sapf_data_mod[!(sapf_data_mod %in% sapf_data)])
  env_data_vars_mod <- names(env_data_mod[!(env_data_mod %in% env_data)])

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
#' data('ARG_TRE', package = 'sapfluxnetr')
#'
#' # transform to NAs any sapflow value occured with wind speed above 25
#' ws_threshold <- 25
#' # get the names of the variables to mutate (tree names)
#' vars_to_mutate <- names(get_sapf_data(ARG_TRE)[,-1]) # no TIMESTAMP
#'
#' sfn_mutate_at(
#'   ARG_TRE,
#'   .vars = vars(one_of(vars_to_mutate)),
#'   .funs = list(
#'     ~ case_when(
#'       ws > ws_threshold ~ NA_real_,
#'       TRUE ~ .
#'     )
#'   )
#' )
#'
#' ## multi
#' data(ARG_MAZ, package = 'sapfluxnetr')
#' data(AUS_CAN_ST2_MIX, package = 'sapfluxnetr')
#' multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
#'
#' ## in multi it's better to discard the variables to not mutate:
#' vars_to_not_mutate <- names(get_env_data(ARG_TRE)) # all the environmental
#'
#' multi_sfn_mutated <- sfn_mutate_at(
#'   multi_sfn,
#'   .vars = vars(-one_of(vars_to_not_mutate)), # we use -
#'   .funs = list(
#'     ~ case_when(
#'       ws > ws_threshold ~ NA_real_,
#'       TRUE ~ .
#'     )
#'   )
#' )
#'
#' multi_sfn_mutated[['ARG_TRE']]
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
  sapf_data_vars_mod <- names(sapf_data_mod[!(sapf_data_mod %in% sapf_data)])
  env_data_vars_mod <- names(env_data_mod[!(env_data_mod %in% env_data)])

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
