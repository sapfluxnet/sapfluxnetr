#### sfn_data classes definitions ####

##### sfn_data #################################################################
# set old class for S$ to recognize tbl as data.frames
setOldClass(c("tbl_df", "tbl", "data.frame"))


#' S4 class for sapfluxnet site data
#'
#' Main class for storing sapfluxnet project site data and metadata
#'
#' This class allows to store all the data and metadata for a sapfluxnet site
#' in one single object, to easily work with it. See
#' \code{vignette('sfn-data-classes', package = 'sapfluxnetr')} for more info.
#'
#' @slot sapf_data A data frame with the sapf data
#'
#' @slot env_data A data frame with the env data
#'
#' @slot sapf_flags A data frame with the same dimensions of \code{sapf_data}
#'   with the flag info for each tree/TIMESTAMP combination
#'
#' @slot env_flags A data frame with the same dimensions of \code{env_data} with
#'   the flag info for each env_var/TIMESTAMP combination
#'
#' @slot si_code A character vector of length one indicating
#'   the site code
#'
#' @slot timestamp A POSIXct vector of length \code{nrow(sapf_data)} with the
#'   timestamp
#'
#' @slot solar_timestamp A POSIXct vector of length \code{nrow(sapf_data)} with
#'   the solar timestamp
#'
#' @slot site_md A data frame containing the site metadata
#'
#' @slot stand_md A data frame containing the stand metadata
#'
#' @slot species_md A data frame containing the species metadata
#'
#' @slot plant_md A data frame containing the plant metadata
#'
#' @slot env_md A data frame containing the env metadata
#'
#' @import methods
#' @export sfn_data
#' @exportClass sfn_data

sfn_data <- setClass(
  'sfn_data',
  slots = list(
    sapf_data = "data.frame",
    env_data = "data.frame",
    sapf_flags = "data.frame",
    env_flags = "data.frame",
    si_code = "character",
    timestamp = "POSIXt",
    solar_timestamp = "POSIXt",
    site_md = "data.frame",
    stand_md = "data.frame",
    species_md = "data.frame",
    plant_md = "data.frame",
    env_md = "data.frame"
  )
)

##### sfn_data_multi ###########################################################
#' S4 class for sapfluxnet multi-site data
#'
#' Multi sfn data class, derived from list
#'
#' This class inherits from \code{list}, but modified to contain sfn_data objects
#' as elements. This will allow to work with several sites at the same time
#' obtaining results for all of them combined or individually as elements of
#' the resulting list (with \code{lapply} or \code{purrr::map})
#'
#' @export sfn_data_multi
#' @exportClass sfn_data_multi

sfn_data_multi <- setClass(
  'sfn_data_multi',
  contains = 'list'
)
