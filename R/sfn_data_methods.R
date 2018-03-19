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
      !(slot(object, ".Data") %>%
        purrr::map_lgl(~ is(.x, 'sfn_data')) %>%
        all())
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

#### sfn_data_multi initialize #################################################
#' Initialize method for sfn_data multi
#'
#' Initialize an sfn_data_multi object
#'
#' @param .Object sfn_data_multi object to create
#'
#' @param ... sfn_data elements
#'
#' @export

setMethod(
  "initialize", "sfn_data_multi",
  definition = function(.Object, ...) {
    .Data <- list(...)

    site_codes <- .Data %>%
      purrr::map_chr(get_si_code)

    names(.Data) <- site_codes

    .Object <- callNextMethod(.Object, .Data = .Data)
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
    timestamp_minmax <- min_max(slot(object, 'timestamp'))
    timestamp_span <- lubridate::interval(timestamp_minmax[1],
                                          timestamp_minmax[2],
                                          tzone = get_timezone(object)) %>%
      as.character()
    cat("TIMESTAMP span: ", timestamp_span, "\n\n")
    # solar_timestamp
    solar_timestamp_minmax <- min_max(slot(object, 'timestamp'))
    solar_timestamp_span <- lubridate::interval(solar_timestamp_minmax[1],
                                                solar_timestamp_minmax[2],
                                                tzone = get_timezone(object)) %>%
      as.character()
    cat("Solar TIMESTAMP span: ", solar_timestamp_span, "\n\n")

    # sapf_flags
    unique_sapf_flags <- slot(object, 'sapf_flags') %>%
      purrr::map(~ stringr::str_split(.x, '; ')) %>%
      purrr::map(purrr::flatten_chr) %>%
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

    cat("Environmental data flags:\n")
    if (length(env_flags_table)) {
      print(sort(env_flags_table))
    } else {cat("No flags present")}
    cat("\n")

  }
)

#### sfn_data_multi show #######################################################
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

    # 1. a list with max 6 site codes
    if (length(object) > 6) {
      cat(length(object), " sites: ",
          paste(names(object)[1:6], collapse = ' '), " ...\n", sep = '')
    } else {
      cat(length(object), " sites: ",
          paste(names(object), collapse = ' '), "\n", sep = '')
    }

    # 2. combined timespan
    timestamp_minmax <- object %>%
      purrr::map(~ slot(.x, 'timestamp')) %>%
      purrr::map(as.character) %>%
      purrr::flatten_chr() %>%
      sort()
    timestamp_span <- lubridate::interval(timestamp_minmax[1],
                                          tail(timestamp_minmax, 1),
                                          tzone = "") %>%
      as.character()
    cat('Time span (UTC) for the combined sites: ', timestamp_span, '\n', sep = '')
  }
)

#### sfn_data get methods ######################################################
#' sfn_data get methods
#'
#' Methods to get the data and metadata from the sfn_data class slots
#'
#' \code{get_sapf} and \code{get_env} methods retrieve sapflow or environmental
#' data and timestamp to create a functional dataset to work with.
#'
#' \code{get_sapf_flags} and \code{get_env_flags} methods retrieve sapflow or
#' environmental flags also with the timestamp.
#'
#' \code{get_timestamp} method retrieve only the timestamp as POSIXct vector.
#'
#' \code{get_si_code} method retrieve a character vector with length(timestamp)
#' containing the site code.
#'
#' \code{get_site_md}, \code{get_stand_md}, \code{get_species_md},
#' \code{get_plant_md} and \code{get_env_md} methods retrieve the corresponding
#' metadata.
#'
#' @param object Object of class sfn_data from which data is retrieved
#'
#' @param solar Logical indicating if the timestamp to return in the \code{get_sapf},
#'   \code{get_env}, \code{get_sapf_flags} and \code{get_env_flags} methods is
#'   the solarTIMESTAMP (TRUE) or the contributors provided TIMESTAMP (FALSE)
#'
#' @examples
#' data('FOO', pkg = 'sapfluxnetr')
#' sapf_data <- get_sapf(FOO, solar = TRUE)
#' env_data_no_solar <- get_env(FOO, solar = FALSE)
#' plant_md <- get_plant_md(FOO)
#'
#' # dplyr pipe to get the mean dbh for a site
#' FOO %>%
#'   get_plant_md() %>%
#'   summarise(dbh_mean = mean(dbh, na.rm = TRUE)) %>%
#'   pull(dbh_mean)
#'
#' @name sfn_get_methods
#' @include sfn_data_classes.R sfn_data_generics.R
NULL

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf", "sfn_data",
  function(object, solar = FALSE) {
    # data
    .sapf <- slot(object, "sapf_data")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .sapf)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env", "sfn_data",
  function(object, solar = FALSE) {
    # data
    .env <- slot(object, "env_data")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .env)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf_flags", "sfn_data",
  function(object, solar = FALSE) {
    .sapf_flags <- slot(object, "sapf_flags")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .sapf_flags)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_flags", "sfn_data",
  function(object, solar = FALSE) {
    .env_flags <- slot(object, "env_flags")

    # timestamp
    if (solar) {
      TIMESTAMP <- slot(object, "solar_timestamp")
    } else {
      TIMESTAMP <- slot(object, "timestamp")
    }

    # combining both
    res <- cbind(TIMESTAMP, .env_flags)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_timestamp", "sfn_data",
  function(object) {
    slot(object, "timestamp")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_solar_timestamp", "sfn_data",
  function(object) {
    slot(object, "solar_timestamp")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_si_code", "sfn_data",
  function(object) {
    slot(object, "si_code")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_site_md", "sfn_data",
  function(object) {
    slot(object, "site_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_stand_md", "sfn_data",
  function(object) {
    slot(object, "stand_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_species_md", "sfn_data",
  function(object) {
    slot(object, "species_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_plant_md", "sfn_data",
  function(object) {
    slot(object, "plant_md")
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_md", "sfn_data",
  function(object) {
    slot(object, "env_md")
  }
)

#### sfn_data replacement methods ######################################################
#' sfn_data replacement methods
#'
#' Methods to replace the data and metadata from the sfn_data class slots
#'
#' The replacement object must be a valid object for that slot:
#' \itemize{
#'   \item{For \code{get_sapf}, \code{get_env}, \code{get_sapf_flags} and
#'         \code{get_env_flags} a data.frame or tibble without the TIMESTAMP
#'         variable}
#'   \item{For \code{get_*_md} a data.frame or tibble}
#'   \item{For \code{get_timestamp} and \code{get_solar_timestamp} a POSIXct
#'         vector of length == nrow(sapf/env_data)}
#'   \item{For \code{get_si_code} a character vector}
#' }
#' Validity is automatically checked before modifing the sfn_data object, and
#' an error is raised if not valid
#'
#' @examples
#' data('FOO', pkg = 'sapfluxnetr')
#' sapf_data <- get_sapf(FOO, solar = TRUE)
#' sapf_data[1:10, 2] <- NA
#' get_sapf(FOO) <- sapf_data
#'
#' @name sfn_replacement_methods
NULL

#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_sapf", "sfn_data",
  function(object, value) {
    slot(object, "sapf_data") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_env", "sfn_data",
  function(object, value) {
    slot(object, "env_data") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_sapf_flags", "sfn_data",
  function(object, value) {
    slot(object, "sapf_flags") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_env_flags", "sfn_data",
  function(object, value) {
    slot(object, "env_flags") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_timestamp", "sfn_data",
  function(object, value) {
    slot(object, "timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_solar_timestamp", "sfn_data",
  function(object, value) {
    slot(object, "solar_timestamp") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_si_code", "sfn_data",
  function(object, value) {
    slot(object, "si_code") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_site_md", "sfn_data",
  function(object, value) {
    slot(object, "site_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_stand_md", "sfn_data",
  function(object, value) {
    slot(object, "stand_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_species_md", "sfn_data",
  function(object, value) {
    slot(object, "species_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_plant_md", "sfn_data",
  function(object, value) {
    slot(object, "plant_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)

#' @export
#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_env_md", "sfn_data",
  function(object, value) {
    slot(object, "env_md") <- value

    # check validity before return the object, we don't want a messy object
    validity <- try(validObject(object))
    if (is(validity, "try-error")) {
      stop('new data is not valid: ', validity[1])
    }

    return(object)
  }
)
