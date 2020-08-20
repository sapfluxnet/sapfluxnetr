## This functions are related to get the info about the different datasets
## and list/copy/load the filtered datasets

#' Read sfn_data from disk
#'
#' Given a site code and a route, \code{read_sfn_data} will return the selected
#' sfn_data object
#'
#' @param site_codes A character vector with the site code/s
#'
#' @param folder Route to the folder containing the \code{.RData} file. Default
#'   to working directory.
#'
#' @return If \code{site_codes} is a vector of length 1, an sfn_data object with
#'   the selected site data. If \code{site_codes} is a vector of length > 1, then
#'   a sfn_data_multi object containing all selected sites.
#'
#' @examples
#' # Let's access the data in "folder". This typically is the folder where the
#' # sapflow data at the desired unit level is (i.e. "RData/plant"), but in this
#' # example we will create a temporal folder with some data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#' 
#' # now we read a single site
#' ARG_TRE_test <- read_sfn_data('ARG_TRE', folder)
#' ARG_TRE_test
#' 
#' # or we can read multiple sites at once
#' multi_sfn <- read_sfn_data(
#'   c('ARG_TRE', 'ARG_MAZ'), folder
#' )
#' multi_sfn
#'
#' @export

read_sfn_data <- function(site_codes, folder = '.') {
  
  # if more than one site we need to map the call
  if (length(site_codes) > 1) {
    sites_multi <- purrr::map(site_codes, read_sfn_data, folder) %>%
      as_sfn_data_multi()
    
    return(sites_multi)
  }
  
  # one site, we need to find it and load it
  file_name <- file.path(folder, paste0(site_codes, '.RData'))
  
  if (!file.exists(file_name)) {
    stop(folder, ' folder does not contain any file called ', site_codes, '.RData')
  } else {
    load(file = file_name)
    
    # load will load in the function environment a SITE_CODE object,
    # we need to access to it to return it
    return(eval(as.name(site_codes)))
  }
  
}

#' Write metadata cache file to disk
#'
#' Load all sites, read the metadata and write it to disk to cache the
#' info for easy and fast access
#'
#' Load all data in memory to collect metadata info can be resource limiting.
#' For easy and quick access to metadata, this function stores an .RData file
#' in the specified folder along the data with all the metadata preloaded.
#'
#' @param folder Route to the folder containing the data. Default to working
#'   directory
#'
#' @param .dry Dry run. Metadata is loaded and readed, but no cache is written
#' 
#' @examples
#' # Let's access the data in "folder". This typically is the folder where the
#' # sapflow data at the desired unit level is (i.e. "RData/plant"), but in this
#' # example we will create a temporal folder with some data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#' 
#' # lets create the metadata cache file
#' sapfluxnetr:::.write_metadata_cache(folder, .dry = FALSE)#' 
#' file.exists(file.path(folder, '.metadata_cache.RData')) # TRUE
#'
#' @return A list of tibbles with the five metadata classes (site, stand,
#'   species, plant and environmental)
#' @keywords internal

.write_metadata_cache <- function(folder, .dry = FALSE) {
  
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
  
  # we do a simple for loop instead of loading all sites and getting the
  # metadata because in the benchmarking it not presents any time adventage and
  # in this way the memory is liberated in each loop, so it's suitable for
  # low memory systems (more or less)
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
        # TODO remove this when the sites are corrected
        dplyr::mutate(pl_name = as.character(.data$pl_name))
    )
    sfn_metadata[['env_md']] <- dplyr::bind_rows(
      sfn_metadata[['env_md']], get_env_md(sfn_data)
    )
  }
  
  # cache thing
  if (!.dry) {
    save(sfn_metadata, file = file.path(folder, '.metadata_cache.RData'))
  }
  
  return(sfn_metadata)
  
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
#' # Let's access the data in "folder". This typically is the folder where the
#' # sapflow data at the desired unit level is (i.e. "RData/plant"), but in this
#' # example we will create a temporal folder with some data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#' 
#' # create and load the metadata. The first time we use .write_cache = TRUE,
#' # to ensure creating a file containing the metadata for speed the process
#' # for the next times
#' read_sfn_metadata(
#'   folder = folder, .write_cache = TRUE
#' )
#' # a cached copy must have been written to "folder"
#' file.exists(paste0(folder, '.metadata_cache.RData')) # TRUE
#' 
#' # after that, we only need to especify the folder
#' sites_metadata <- read_sfn_metadata(folder = folder) # quicker than before
#' sites_metadata
#'
#' @return A list of tibbles with the five metadata classes (site, stand,
#'   species, plant and environmental)
#'
#' @export

read_sfn_metadata <- function(folder = '.', .write_cache = FALSE) {
  
  if (.write_cache) {
    sfn_metadata <- .write_metadata_cache(folder = folder, .dry = FALSE)
  } else {
    file_name <- file.path(folder, '.metadata_cache.RData')
    
    if (!file.exists(file_name)) {
      stop('metadata cache file not found at ', folder,
           ' If you want to create one, please set .write_cache to TRUE')
    } else {
      load(file_name)
    }
  }
  
  return(sfn_metadata)
  
}

#' Filter the sites by metadata variable values
#'
#' \code{filter_sites_by_md} function takes logical expressions for the metadata
#' variables (i.e. \code{pl_sens_meth == 'HR'}), and list the sites that met
#' the criteria from thos supplied
#'
#' \code{.join} argument indicates how sites must be filtered between metadata
#' classes. \code{'and'} indicates only sites meeting all conditions for all
#' metadata classes are returned. \code{'or'} indicates all sites meeting any
#' condition between classes are returned. For two or more filtes of the same
#' metadata class, they are combined as 'and'.
#' 
#' @param sites character vector with the sites codes to filter, generally the
#'   result of \code{\link{sfn_sites_in_folder}}
#'
#' @param metadata metadata tbl object, usually the result of
#'   \code{\link{read_sfn_metadata}}
#'
#' @param ... Logical expressions for the metadata variables, as in
#'   \code{\link[dplyr]{filter}}.
#'
#' @param .join Character indicating how to filter the sites, see details.
#'
#' @examples
#' # Let's access the data in "folder". This typically is the folder where the
#' # sapflow data at the desired unit level is (i.e. "RData/plant"), but in this
#' # example we will create a temporal folder with some data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#' save(AUS_CAN_ST2_MIX, file = file.path(folder, 'AUS_CAN_ST2_MIX.RData'))
#' 
#' # we need the metadata and the site names
#' metadata <- read_sfn_metadata(folder = folder, .write_cache = TRUE)
#' sites <- sfn_sites_in_folder(folder)
#' 
#' # Filter by Heat Ratio method
#' filter_sites_by_md(
#'   pl_sens_meth == 'HR', sites = sites, metadata = metadata
#' )
#'
#' # Both, Heat Ratio and Heat Dissipation
#' filter_sites_by_md(
#'   pl_sens_meth %in% c('HR', 'HD'),
#'   sites = sites, metadata = metadata
#' )
#'
#' # more complex, Heat Ratio method AND Woodland/Shrubland biome
#' filter_sites_by_md(
#'   pl_sens_meth == 'HR',
#'   si_biome == 'Woodland/Shrubland',
#'   sites = sites, metadata = metadata,
#'   .join = 'and' # default
#' )
#'
#' # join = 'or' returns sites that meet any condition
#' filter_sites_by_md(
#'   pl_sens_meth == 'HR',
#'   si_biome == 'Woodland/Shrubland',
#'   sites = sites, metadata = metadata,
#'   .join = 'or'
#' )
#'
#' @return A character vector with the sites fullfilling the premises
#'
#' @export

filter_sites_by_md <- function(
  sites, metadata,
  ...,
  .join = c('and', 'or')
) {
  
  # if we accept ... (expressions with logical result) we need to enquo them
  dots <- dplyr::quos(...)
  # metadata dic to distribute the dots arguments to their respective md
  metadata_dic <- c(site_md = 'si_', stand_md = 'st_',
                species_md = 'sp_', plant_md = 'pl_', env_md = 'env_')
  # empty res
  res_list <- vector(mode = 'list')
  
  # loop along all metadata classes to check if there is filters and apply them
  for (md in 1:5) {
    
    # dot dispatcher, distribute the dots in the corresponding metadata
    sel_dots <- dots %>%
      purrr::map(rlang::quo_get_expr) %>%
      purrr::map(as.character) %>%
      purrr::map(stringr::str_detect, pattern = metadata_dic[[md]]) %>%
      purrr::map_lgl(any)
    md_dots <- dots[sel_dots]
    
    # if there is filters, filter the corresponding metadata, pull the codes
    # and get the unique (in case of plant and species md, that can be repeated)
    if (length(md_dots) > 0) {
      md_sites_selected <- metadata[[names(metadata_dic)[md]]] %>%
        dplyr::filter(
          !!! md_dots
        ) %>%
        # pull the si_code variable
        dplyr::pull(.data$si_code) %>%
        unique()
      
      res_list[[md]] <- md_sites_selected
      names(res_list)[md] <- names(metadata_dic)[md]
      
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
  .join <- match.arg(.join)
  if (.join == 'or') {
    
    # 'or' indicates any site that meet any condition
    res_names <- purrr::flatten_chr(names_sites) %>%
      unique()
    # we return only those which fulfill the criteria but also are in the sites
    # provided
    return(res_names[res_names %in% sites])
    
  } else {
    
    # 'and' indicates only sites that meet all conditions, we will do that with
    # Reduce and intersect
    res_names <- Reduce(intersect, names_sites)
    # we return only those which fulfill the criteria but also are in the sites
    # provided
    return(res_names[res_names %in% sites])
    
  }
  
}

#' list available sites in a db folder
#' 
#' Retrieves the site codes in the specified folder
#' 
#' If folder 
#' 
#' @param folder Character vector of length 1 indicating the route to the
#'   db folder
#' 
#' @examples
#' # Let's access the data in "folder". This typically is the folder where the
#' # sapflow data at the desired unit level is (i.e. "RData/plant"), but in this
#' # example we will create a temporal folder with some data to test the function
#' folder <- tempdir()
#' save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
#' save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
#' save(AUS_CAN_ST2_MIX, file = file.path(folder, 'AUS_EUC_ST2_MIX.RData'))
#' 
#' # lets see the sites
#' sites <- sfn_sites_in_folder(folder)
#' 
#' @return A character vector with the site codes present in the folder, an
#'   error if the folder is not valid or does not contain any site data file.
#' 
#' @export
sfn_sites_in_folder <- function(folder = '.') {
  
  # checks
  stopifnot(
    is.character(folder), # folder must be character
    dir.exists(folder) # folder must be a valid directory
  )
  
  # get the files, if any
  res <- list.files(folder, pattern = '.RData') %>%
    stringr::str_remove('.RData')
  
  if (length(res) < 1) {
    # if no files were found, report it and stop
    stop(folder, ' does not contain any site data file')
  }
  
  # return res
  return(res)
}
