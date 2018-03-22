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
    sites_multi <- purrr::map(site_code, read_sfn_data, folder)
    # TODO convert the list obtained in a sfn_data_multi
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
