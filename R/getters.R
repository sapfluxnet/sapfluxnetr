## This functions are related to get the info about the different datasets
## and list/copy/load the filtered datasets

#' Read sfn_data from disk
#'
#' Given a site code and a route, \code{read_sfn_data} will return the selected
#' sfn_data object
#'
#' @param site_code A character vector with the site code/s
#'
#' @param folder Route to the folder containing the \code{.RData} file. Default
#'   to working directory.
#'
#' @return If \code{site_code} is a vector of length 1, an sfn_data object with
#'   the selected site data. If \code{site_code} is a vector of length > 1, then
#'   a sfn_data_multi object containing all selected sites.
#'
#' @examples
#' # load one single site
#' FOO <- read_sfn_data('FOO', 'Data')
#'
#' # load several sites
#' multi_sfn <- read_sfn_data(c('FOO', 'BAR', 'BAZ'), 'Data')
#'
#' @export

read_sfn_data <- function(site_code, folder = '.') {

  # if more than one site we need to map the call
  if (length(site_code) > 1) {
    sites_multi <- purrr::map(site_code, read_sfn_data, folder) %>%
      as_sfn_data_multi()

    return(sites_multi)
  }

  # one site, we need to find it and load it
  file_name <- file.path(folder, paste0(site_code, '.RData'))

  if (!file.exists(file_name)) {
    stop(folder, ' folder does not contain any file called ', site_code, '.RData')
  } else {
    load(file = file_name)

    # load will load in the function environment a SITE_CODE object,
    # we need to access to it to return it
    return(eval(as.name(site_code)))
  }

}

#' Filter the sites by metadata variable values
#'
#' \code{filter_by_var} function takes a vector of variables and a list of
#' possible values and list the sites that comply with the request
#'
#' If \code{variables} length > 1, then only sites that match the values for
#' ALL variables declared are showed.
#'
#' \code{variables} and \code{values} must be of the same length.
#'
#' @param variables A character vector indicating the variables to check
#'
#' @param values A named list. Each element is named as the variable and contains
#'   a character vector with the desired values for that variable
#'
#' @param folder Route to the folder containing the data files (*.RData)
#'
#' @param .use_cache Experimental, not implemented yet. Searches can be time
#'   and resources consuming. Using a cache speed up the searches storing the
#'   results in a cache folder and serving directly from the file.
#'
#' @examples
#' # simple, want to know which sites are using the Heat Ratio method to measure
#' # the sap flow
#' filter_by_var('pl_sens_meth', 'HR', folder = 'Data')
#'
#' # Both, Heat Ratio and Heat Dissipation
#' filter_by_var('pl_sens_meth',
#'               list(pl_sens_meth = c('HR', 'HD')),
#'               folder = 'Data')
#'
#' # more complex, Heat Ratio method AND Mediterranean biome
#' filter_by_var(
#'   variables = c('pl_sens_meth', 'env_biome'),
#'   values = list(pl_sens_meth = 'HR',
#'                 env_biome = 'Mediterranean'),
#'   folder = 'Data'
#' )
#'
#' @return A character vector with the sites fullfilling the premises
#'
#' @export

filter_by_var <- function(variables, values, folder = '.', .use_cache = FALSE) {

  # a custom function to read the metadata directly. This way when used inside
  # a purrr::map statement, not all the objects are stored (memory problem),
  # only the metadata is stored
  read_metadata <- function(site, metadata, folder) {

    switch(
      metadata,
      'si_' = get_site_md(read_sfn_data(site, folder)),
      'st_' = get_stand_md(read_sfn_data(site, folder)),
      'sp_' = get_species_md(read_sfn_data(site, folder)),
      'pl_' = get_plant_md(read_sfn_data(site, folder)),
      'env' = get_env_md(read_sfn_data(site, folder))
    )

  }

  # list of sites for search
  sites_codes <- list.files(folder, recursive = TRUE, pattern = '.RData') %>%
    stringr::str_remove('.RData')

  # quo for future filters.
  # This takes the variable character, transforms it to
  # a name (symbol) and creates a quosure with the filtering expression,
  # some in the form variable %in% y, see rlang::get_expr(filters[[1]])
  # After that, all will be evaluated when called inside of filter (see below in
  # the purrr pipeline step)
  filters <- purrr::map(
    variables, as.name
  ) %>%
    purrr::map2(
      values,
      function(var_name, accepted_values) {
        dplyr::quo(!!var_name %in% accepted_values)
      }
    )

  # purrr pipeline
  variables %>%
    # we select the metadata (by the code in the variable name)
    purrr::map(stringr::str_sub, start = 1, end = 3) %>%
    # remove duplicate metadatas (to avoid load the same twice or more times)
    unique() %>%
    # read the corresponding metadata for all sites stored in folder
    # (remember this is a list with elements for each metadata kind)
    purrr::map(
      ~ purrr::map(sites_codes, read_metadata, metadata = .x, folder = folder)
    ) %>%
    # join all the sites by rows for each metadata loaded
    purrr::map(~ dplyr::bind_rows(.x)) %>%
    # get a unique tbl by joining the columns of the metadatas. This way we will
    # be able to filter by variable and values provided
    dplyr::bind_cols() %>%
    # we use the quo made before as filter args
    dplyr::filter(
      !!! filters
    ) %>%
    # pull the si_code variable
    dplyr::pull(.data$si_code)


}
