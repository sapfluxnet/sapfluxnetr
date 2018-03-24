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

#' Read and combine all metadata
#' 
#' Read metadata from all sites in folder and write it to disk to cache the
#' info for easy and fast access
#' 
#' Load all data in memory to collect metadata info can be resource limiting.
#' For easy and quick access to metadata, this function stores an .RData file
#' in the specified folder along the data with all the metadata preloaded. Also
#' it return it as an object to use in filtering and selecting sites.
#' 
#' @param folder Route to the folder containing the data. Default to working
#'   directory
#' 
#' @param .write_cache Logical indicating if a cached copy of the metadata must
#'   be written in \code{folder}.
#'
#' @examples
#' # load the metadata for the first time, it can be a minute ;)
#' sites_metadata <- read_metadata(folder = 'Data')
#' 
#' # a cached copy must have been written to "folder"
#' file.exists('Data/.metadata_cache.RData') # TRUE
#' 
#' # inspect the metadata
#' sites_metadata
#' 
#' @return A list of tibbles with the five metadata classes (site, stand,
#'   species, plant and environmental)
#'
#' @export

read_sfn_metadata <- function(folder = '.', .write_cache = FALSE) {
  
  # In order to avoid loading all data objects at one time in memory (it could be
  # too much in a normal system we think), lets only store the metadata. To do
  # that, we load one site and store the metadata before to pass to the next.
  # If we simply map read_sfn_data %>% get_plant_md for example, that will
  # result in all objects in memory, so NO GOOD, we nned to circumvent that.
  sites_codes <- list.files(folder, recursive = TRUE, pattern = '.RData') %>%
    stringr::str_remove('.RData')
  
  sfn_metadata <- list(
    site_md = tibble::tibble(),
    stand_md = tibble::tibble(),
    species_md = tibble::tibble(),
    plant_md = tibble::tibble(),
    env_md = tibble::tibble()
  )
  
  for (i in 1:length(sites_codes)) {
    sfn_data <- read_sfn_data(sites_codes[i], folder)
    
    sfn_metadata[['site_md']] <- dplyr::bind_rows(
      sfn_metadata[['site_md']], get_site_md(sfn_data)
    )
    sfn_metadata[['stand_md']] <- dplyr::bind_rows(
      sfn_metadata[['stand_md']], get_stand_md(sfn_data)
    )
    sfn_metadata[['species_md']] <- dplyr::bind_rows(
      sfn_metadata[['species_md']], get_species_md(sfn_data)
    )
    sfn_metadata[['plant_md']] <- dplyr::bind_rows(
      sfn_metadata[['plant_md']], get_plant_md(sfn_data)
    )
    sfn_metadata[['env_md']] <- dplyr::bind_rows(
      sfn_metadata[['env_md']], get_env_md(sfn_data)
    )
  }
  
  # cache thing
  if (.write_cache) {
    save(sfn_metadata, file = file.path(folder, '.metadata_cache.RData'))
  }
  
  return(sfn_metadata)
  
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
      'sp_' = get_species_md(read_sfn_data(site, folder)) %>%
        dplyr::summarise_all(stringr::str_flatten, collapse = '-') %>%
        dplyr::mutate_all(stringr::str_split, pattern = '-'),
      'pl_' = get_plant_md(read_sfn_data(site, folder)) %>%
        dplyr::summarise_all(stringr::str_flatten, collapse = '-') %>%
        dplyr::mutate_all(stringr::str_split, pattern = '-'),
      'env' = get_env_md(read_sfn_data(site, folder))
    )

  }

  # list of sites for search
  sites_codes <- list.files(folder, recursive = TRUE, pattern = '.RData') %>%
    stringr::str_remove('.RData')
  
  if (length(sites_codes) < 1) {
    stop(folder, ' seems to contain no sapfluxnet data files (.RData)')
  }

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
        # dplyr::quo(!!var_name %in% accepted_values)
        dplyr::quo(stringr::str_detect(!!var_name, stringr::str_flatten(accepted_values, '|')))
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
    dplyr::pull(.data$si_code) %>%
    purrr::flatten_chr() %>%
    unique()

  # TODO transform with help quo and ... in an equivalent of filter (you put
  # a logical expression that is evaluated a posteriori. To do that maybe
  # we need to load all the metadatas, do the filtering and collapse to
  # lists)
  # > quo(pl_sens_meth %in% c('tururu', 'tarara'))
  # <quosure>
  #   expr: ^pl_sens_meth %in% c("tururu", "tarara")
  # env:  global
  # > quo(pl_sens_meth %in% c('tururu', 'tarara')) -> quo_foo
  # > get_expr(quo_foo)
  # Error in get_expr(quo_foo) : no se pudo encontrar la función "get_expr"
  # > rlang::get_expr(quo_foo)
  # pl_sens_meth %in% c("tururu", "tarara")
  # > class(rlang::get_expr(quo_foo))
  # [1] "call"
  # > as.character(rlang::get_expr(quo_foo))
  # [1] "%in%"                      "pl_sens_meth"              "c(\"tururu\", \"tarara\")"
  # > length(as.character(rlang::get_expr(quo_foo)))
  # [1] 3
  # El caso es dividir las calls en metadatas y aplicarlas a la hora de cargar
  # los datos (de esa manera)
  
}
