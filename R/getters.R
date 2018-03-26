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
    
    print(paste0(
      'processing site ', sites_codes[i], ' (', i, ' of ', length(sites_codes), ')' 
    ), width = 80)
    
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
      sfn_metadata[['plant_md']], get_plant_md(sfn_data) %>%
        dplyr::mutate(pl_name = as.character(pl_name)) # TODO remove this when the sites are corrected
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
#' \code{filter_by_var} function takes logical expressions for the metadata
#' variables (i.e. \code{pl_sens_meth == 'HR'}), and list the sites that meet
#' the expressions.
#' 
#' \code{join} argument indicates how sites must be filtered between metadata
#' classes. \code{'and'} indicates only sites meeting all conditions for all
#' metadata classes are returned. \code{'or'}cindicates all sites meeting any
#' condition between classes are returned. For two or more filtes of the same
#' metadata class, they are combined as 'and'.
#'
#' @param ... Logical expressions for the metadata variables, as in
#'   \code{\link[dplyr]{filter}}.
#'
#' @param folder Route to the folder containing the data files (*.RData)
#' 
#' @param join Character indicating how to filter the sites, see details.
#'
#' @param .use_cache Experimental, not implemented yet. Searches can be time
#'   and resources consuming. Using a cache speed up the searches storing the
#'   results in a cache folder and serving directly from the file.
#'
#' @examples
#' # simple, want to know which sites are using the Heat Ratio method to measure
#' # the sap flow
#' filter_by_var(pl_sens_meth == 'HR', folder = 'Data')
#'
#' # Both, Heat Ratio and Heat Dissipation
#' filter_by_var(pl_sens_meth %in% c('HR', 'HD'),
#'               folder = 'Data')
#'
#' # more complex, Heat Ratio method AND Mediterranean biome
#' filter_by_var(
#'   pl_sens_meth == 'HR',
#'   si_biome == 'Mediterranean',
#'   folder = 'Data',
#'   join = 'and' # default
#' )
#' 
#' # join = 'or' returns sites that meet any condition
#' filter_by_var(
#'   pl_sens_meth == 'HR',
#'   si_biome == 'Mediterranean',
#'   folder = 'Data',
#'   join = 'or'
#' )
#'
#' @return A character vector with the sites fullfilling the premises
#'
#' @export

filter_by_var <- function(..., folder = '.', join = c('and', 'or'), .use_cache = FALSE) {
  
  # Don't waste resources, if cache, read metadata from disk directly
  if (.use_cache) {
    
    cache_file <- file.path(folder, '.metadata_cache.RData')
    
    if (file.exists(cache_file)) {
      load(cache_file) # loads and object called sfn_metadata
    } else {
      warning('.use_cache is TRUE but no cache file could be found in ', folder,
              '\n', 'Running read_sfn_metadata with .write_cache = TRUE to ',
              'create the sapfluxnet metadata db. This can take a while...')
      sfn_metadata <- read_sfn_metadata(folder, .write_cache = TRUE)
    }
  } else {
    message('.use_cache is set to FALSE, creating a temporal metadata db. ',
            'This can take a while...')
    sfn_metadata <- read_sfn_metadata(folder, .write_cache = FALSE)
  }
  
  # if we accept ... (expressions with logical result) we need to enquo them
  dots <- dplyr::quos(...)
  
  metadata <- c(site_md = 'si_', stand_md = 'st_',
                species_md = 'sp_', plant_md = 'pl_', env_md = 'env_')
  res_list <- vector(mode = 'list')
  
  # loop along all metadata classes to check if there is filters and apply them
  for (md in 1:5) {
    
    # dot dispatcher, distribute the dots in the corresponding metadata
    md_dots <- dots %>%
      purrr::map(rlang::quo_get_expr) %>%
      purrr::map(as.character) %>%
      purrr::map(stringr::str_detect, pattern = metadata[[md]]) %>%
      purrr::map_lgl(any) %>%
      dots[.]
    
    # if there is filters, filter the corresponding metadata, pull the codes
    # and get the unique (in case of plant and species md, that can be repeated)
    if (length(md_dots) > 0) {
      md_sites_selected <- sfn_metadata[[names(metadata)[md]]] %>%
        dplyr::filter(
          !!! md_dots
        ) %>%
        # pull the si_code variable
        dplyr::pull(.data$si_code) %>%
        unique()
      
      res_list[[md]] <- md_sites_selected
      names(res_list)[md] <- names(metadata)[md]
      
    } else {
      # if there is no filter, return NULL to the list, we will remove it after
      res_list[[md]] <- NULL
    }
  }
  
  # remove the NULL elements, this way we can check for values in all elements
  # We do it with purrr::keep (compact removes also empty vectors, which is not
  # desirable in this situation)
  names_sites <- res_list %>%
    purrr::keep(~ !is.null(.x))
  
  # get the join argument
  join <- match.arg(join)
  if (join == 'or') {
    
    # 'or' indicates any site that meet any condition
    res_names <- purrr::flatten_chr(names_sites) %>%
      unique()
    return(res_names)
    
  } else {
    
    # 'and' indicates only sites that meet all conditions, we will do that with
    # Reduce and intersect
    res_names <- Reduce(intersect, names_sites)
    return(res_names)
    
  }
}
