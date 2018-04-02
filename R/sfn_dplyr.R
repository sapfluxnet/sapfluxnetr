# Functions to emulate dplyr verbs (filter, mutate)
# summarise is not included because is already in sfn_metrics ;)

#' dplyr_like functions to work with sfn_data objects
#' 
#' Family of functions to simulate dplyr verbs in sfn_data objects
#' 
#' This functions accepts also sfn_data_multi objects. In this case the
#' function arguments (filters or mutations) are applied to each sfn_data (site)
#' present in the object provided.
#' 
#' @param sfn_data \code{sfn_data} or \code{sfn_data_multi} object to subset
#' 
#' @param ... expressions to pass to the relevant dplyr function,
#'   \code{\link[dplyr]{filter}} or \code{\link[dplyr]{mutate}}
#' 
#' @param solar Logical indicating if solar timestamp must used to subset
#' 
#' @name sfn_dplyr_functions
NULL

#' @describeIn sfn_dplyr_functions \code{sfn_filter}: filtering rows by value
#'   (i.e. filtering by TIMESTAMP).
#' 
#' @inheritParams sfn_dplyr_functions
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
#' foo_timestamp <- get_timestmap(FOO)
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
  
  sapf_data <- get_sapf_data(sfn_data, solar = solar)
  env_data <- get_env_data(sfn_data, solar = solar)
  
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
  )
  
  env_flags_mod <- dplyr::semi_join(
    get_env_flags(sfn_data, solar = solar), env_data_mod, by = 'TIMESTAMP'
  )
  
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

#' @describeIn sfn_dplyr_functions \code{sfn_mutate}: transform columns by function.
#' 
#' @inheritParams sfn_dplyr_functions
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
  
  if (is(sfn_data, 'sfn_data_multi')) {
    res_multi <- purrr::map(sfn_data, sfn_mutate, ..., solar = solar) %>%
      as_sfn_data_multi()
    return(res_multi)
  }
  
  sapf_data <- get_sapf_data(sfn_data, solar = solar)
  env_data <- get_env_data(sfn_data, solar = solar)
  
  whole_data <- dplyr::inner_join(sapf_data, env_data, by = 'TIMESTAMP')
  
  mutated_data <- dplyr::mutate(whole_data, ...)
  
  sapf_data_mod <- dplyr::select(mutated_data, names(sapf_data))
  env_data_mod <- dplyr::select(mutated_data, names(env_data))
  
  sapf_data_vars_mod <- names(base::setdiff(
    as.data.frame(sapf_data_mod),
    as.data.frame(sapf_data)
  ))
  env_data_vars_mod <- names(base::setdiff(
    as.data.frame(env_data_mod),
    as.data.frame(env_data)
  ))
  
  .flag <- function(x) {
    dplyr::case_when(
      x == '' ~ 'USER_MODF',
      TRUE ~ paste0(x, '; USER_MODF')
    )
  }
  
  if (length(sapf_data_vars_mod) > 0) {
    sapf_flags_mod <- get_sapf_flags(sfn_data) %>%
      dplyr::mutate_at(sapf_data_vars_mod, .flag)
  } else {
    sapf_flags_mod <- get_sapf_flags(sfn_data)
  }
  
  if (length(env_data_vars_mod) > 0) {
    env_flags_mod <- get_env_flags(sfn_data) %>%
      dplyr::mutate_at(env_data_vars_mod, .flag)
  } else {
    env_flags_mod <- get_env_flags(sfn_data)
  }
  
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