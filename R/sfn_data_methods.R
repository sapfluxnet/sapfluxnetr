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

    if (length(slot(object, "solar_timestamp")) < 1) {
      valid <- FALSE
      info <- c(info, 'solar TIMESTAMP must be of length >= 1')
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
      slot(object, ".Data") %>%
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

#### sfn_data show #############################################################
#' Show method for sfn_data
#'
#' print a summary for sfn_data objects
#'
#' @param object sfn_data object to show
#'
#' @name sfn_data_show
#'
#' @importFrom dplyr %>%
#'
#' @export

setMethod(
  "show", "sfn_data",
  definition = function(object) {
    # object class
    cat(class(object), " object\n", sep = "")
    # site code
    cat("Data from ", unique(slot(object, 'si_code')), " site/s\n\n", sep = "")
    # number of trees
    cat("Sapflow data: ", nrow(slot(object, "sapf_data")), " observations of ",
        length(names(slot(object, "sapf_data"))), " trees/plants\n\n")
    # env_vars
    cat("Environmental data: ", nrow(slot(object, "env_data")), " observations.\n",
        "Env vars: ", paste(names(slot(object, "env_data"))), "\n\n")
    # timestamp span
    cat("TIMESTAMP span, from ", as.character(head(slot(object, 'timestamp'), 1)),
        "to ", as.character(tail(slot(object, 'timestamp'), 1)), "\n\n")

    # solar_timestamp
    cat("Solar TIMESTAMP available: ", !all(is.na(slot(object, 'solar_timestamp'))),
        "\n\n")

    # sapf_flags
    unique_sapf_flags <- slot(object, 'sapf_flags') %>%
      purrr::map(~ stringr::str_split(.x, '; ')) %>%
      purrr::map(flatten_chr) %>%
      purrr::flatten_chr() %>%
      unique()

    sapf_flags_table <- unique_sapf_flags[unique_sapf_flags != ''] %>%
      purrr::map(~ stringr::str_count(as.matrix(slot(object, "sapf_flags")), .x)) %>%
      purrr::map(sum) %>%
      purrr::flatten_int()
    names(sapf_flags_table) <- unique_sapf_flags[unique_sapf_flags != '']

    cat("Sapflow data flags:\n")
    if (length(sapf_flags_table)) {
      print(sort(sapf_flags_table))
    } else {cat("No flags present")}
    cat("\n")

    # env_flags
    unique_env_flags <- slot(object, 'env_flags') %>%
      purrr::map(~ stringr::str_split(.x, '; ')) %>%
      purrr::map(flatten_chr) %>%
      purrr::flatten_chr() %>%
      unique()

    env_flags_table <- unique_env_flags[unique_env_flags != ''] %>%
      purrr::map(~ stringr::str_count(as.matrix(slot(object, "env_flags")), .x)) %>%
      purrr::map(sum) %>%
      purrr::flatten_int()
    names(env_flags_table) <- unique_env_flags[unique_env_flags != '']

    cat("Sapflow data flags:\n")
    if (length(env_flags_table)) {
      print(sort(env_flags_table))
    } else {cat("No flags present")}
    cat("\n")

  }
)

#### sfn_data_multi show #############################################################
#' Show method for sfn_data_multi
#'
#' print a summary for sfn_data_multi objects
#'
#' @param object sfn_data_multi object to show
#'
#' @name sfn_data_multi_show
#'
#' @export

setMethod(
  'show', 'sfn_data_multi',
  definition = function(object) {

    # class
    cat(class(object), " object\n", sep = "")

    # so what, what can be here?
    # 1. a list with max 6 site codes
    if (length(object) > 6) {
      cat(length(object), " sites: ", names(object)[1:6], " ...\n", sep = '')
    } else {
      cat(length(object), " sites: ", names(object)[1:6], "\n", sep = '')
    }
  }
)

