#### sfn_data methods definitions ####

#### sfn_data_validity #########################################################
#' Validity method for sfn_data class
#'
#' Validation checks for generating sfn_data class objects
#'
#' This method is used internally when creating and/or modifing sfn_data class
#' objects to ensure that the object returned is correct in terms of content
#' classes and dimensions (i.e. sapflow data and environmental data has the
#' same lenght)
#'
#' @name sfn_data_validity

setValidity(
  "sfn_data",
  function(object) {

    # initial values
    info <- NULL
    valid <- TRUE

    # check dimensions
    if (any(
      nrow(slot(object, "sapf_data")) != nrow(slot(object, "env_data"))
    )) {
      valid <- FALSE
      info <- c(info, 'nrow(sapf_data) != nrow(env_data)')
    }

    if (any(
      nrow(slot(object, "sapf_data")) != length(slot(object, "timestamp")),
      nrow(slot(object, "env_data")) != length(slot(object, "timestamp"))
    )) {
      valid <- FALSE
      info <- c(info, 'nrow(sapf_data) != length(timestamp) | nrow(env_data) != length(timestamp)')
    }

    if (any(
      length(slot(object, "timestamp")) != length(slot(object, "solar_timestamp"))
    )) {
      valid <- FALSE
      info <- c(info, 'length(timestamp) != length(solar_timestamp)')
    }

    if (any(
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "sapf_data")),
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "env_data")),
      nrow(slot(object, "env_flags")) != nrow(slot(object, "sapf_data")),
      nrow(slot(object, "env_flags")) != nrow(slot(object, "env_data")),
      nrow(slot(object, "sapf_flags")) != nrow(slot(object, "env_flags"))
    )) {
      valid <- FALSE
      info <- c(info, 'nrow(*_flags) =! nrow(*_flags) | nrow(*_flags) =! nrow(*_data)')
    }

    if (any(
      nrow(slot(object, "sapf_flags")) != length(slot(object, "timestamp")),
      nrow(slot(object, "env_flags")) != length(slot(object, "timestamp"))
    )) {
      valid <- FALSE
      info <- c(info, 'nrow(sapf_flags) != length(timestamp) | nrow(env_flags) != length(timestamp)')
    }

    # check if si_code is empty
    if (any(slot(object, "si_code") == '')) {
      valid <- FALSE
      info <- c(info, 'si_code slot can not be an empty string')
    }

    # check for metadata presence
    if (any(nrow(slot(object, "site_md")) < 1, nrow(slot(object, "stand_md")) < 1,
            nrow(slot(object, "species_md")) < 1, nrow(slot(object, "plant_md")) < 1,
            nrow(slot(object, "env_md")) < 1)) {
      valid <- FALSE
      info <- c(info, 'metadata slots can not be 0-row data frames')
    }

    # check for timestamp presence
    if (length(slot(object, "timestamp")) < 1) {
      valid <- FALSE
      info <- c(info, 'TIMESTAMP must be of length >= 1')
    }

    # check for si_code presence
    if (length(slot(object, "si_code")) != 1) {
      valid <- FALSE
      info <- c(info, 'si_code must be of length = 1')
    }

    # insert more checks here

    # return TRUE or info
    if (valid) {
      return(TRUE)
    } else {return(info)}
  }
)

#### sfn_data_multi_validity #########################################################
#' Validity method for sfn_data_multi class
#'
#' Validation checks for generating sfn_data_multi class objects
#'
#' This method is used internally to ensure the correctnes of the sfn_data_multi
#' object. Basically ensures that the object returned is a list of sfn_data
#' class objects
#'
#' @name sfn_data_multi_validity

setValidity(
  "sfn_data_multi",
  function(object) {

    # initial values
    info <- NULL
    valid <- TRUE

    # check if all are sfn_data
    if (
      object %>%
        purrr::map_lgl(is(.x, 'sfn_data')) %>%
        all()
    ) {
      valid <- FALSE
      info <- c(info, 'Some element is NOT an sfn_data class object')
    }

    # insert more checks here

    # return TRUE or info
    if (valid) {
      return(TRUE)
    } else {return(info)}
  }
)
