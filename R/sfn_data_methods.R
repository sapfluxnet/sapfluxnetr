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

#### sfn_data initialize #################################################
#' Initialize method for sfn_data
#'
#' Initialize an sfn_data object
#'
#' @param .Object sfn_data object to create
#'
#' @param sapf_data A tibble (or any object coercible to one) with the sapf_data
#'   (without the TIMESTAMP variable)
#'
#' @param env_data A tibble (or any object coercible to one) with the env_data
#'   (without the TIMESTAMP variable)
#'
#' @param sapf_flags A tibble (or any object coercible to one) with the same
#'   dimensions of \code{sapf_data} with the flag info for each tree/TIMESTAMP
#'   combination
#'
#' @param env_flags A tibble (or any object coercible to one) with the same
#'   dimensions of \code{env_data} with the flag info for each env_var/TIMESTAMP
#'   combination
#'
#' @param si_code A character vector of length one indicating
#'   the site code
#'
#' @param timestamp A POSIXct vector of length \code{nrow(sapf_data)} with the
#'   timestamp
#'
#' @param solar_timestamp A POSIXct vector of length \code{nrow(sapf_data)} with
#'   the solar timestamp
#'
#' @param site_md A tibble (or any object coercible to one) containing the site
#'   metadata
#'
#' @param stand_md A tibble (or any object coercible to one) containing the stand
#'   metadata
#'
#' @param species_md A tibble (or any object coercible to one) containing the species
#'   metadata
#'
#' @param plant_md A tibble (or any object coercible to one) containing the plant
#'   metadata
#'
#' @param env_md A tibble (or any object coercible to one) containing the env
#'   metadata
#'
#' @export

setMethod(
  "initialize", "sfn_data",
  definition = function(
    .Object,
    sapf_data, env_data, sapf_flags, env_flags,
    si_code, timestamp, solar_timestamp,
    site_md, stand_md, species_md, plant_md, env_md
  ) {

    ## Coerce to tibble, fail if not possible
    # sapf_data
    if (any(class(sapf_data) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      sapf_data_tbl <- tibble::as_tibble(sapf_data)
    } else {
      stop('sapf_data must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # env_data
    if (any(class(env_data) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      env_data_tbl <- tibble::as_tibble(env_data)
    } else {
      stop('env_data must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # sapf_flags
    if (any(class(sapf_flags) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      sapf_flags_tbl <- tibble::as_tibble(sapf_flags)
    } else {
      stop('sapf_flags must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # env_flags
    if (any(class(env_flags) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      env_flags_tbl <- tibble::as_tibble(env_flags)
    } else {
      stop('env_flags must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # site_md
    if (any(class(site_md) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      site_md_tbl <- tibble::as_tibble(site_md)
    } else {
      stop('site_md must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # stand_md
    if (any(class(stand_md) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      stand_md_tbl <- tibble::as_tibble(stand_md)
    } else {
      stop('stand_md must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # species_md
    if (any(class(species_md) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      species_md_tbl <- tibble::as_tibble(species_md)
    } else {
      stop('species_md must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # plant_md
    if (any(class(plant_md) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      plant_md_tbl <- tibble::as_tibble(plant_md)
    } else {
      stop('plant_md must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    # env_md
    if (any(class(env_md) %in% c('data.frame', 'tbl_df','tbl', 'tbl_time'))) {
      env_md_tbl <- tibble::as_tibble(env_md)
    } else {
      stop('env_md must be a tibble or an object coercible to one',
           ' (data.frame, tbl_df, tbl, tbl_time)')
    }

    .Object <- callNextMethod(
      .Object,
      sapf_data = sapf_data_tbl,
      env_data = env_data_tbl,
      sapf_flags = sapf_flags_tbl,
      env_flags = env_flags_tbl,
      si_code = si_code,
      timestamp = timestamp,
      solar_timestamp = solar_timestamp,
      site_md = site_md_tbl,
      stand_md = stand_md_tbl,
      species_md = species_md_tbl,
      plant_md = plant_md_tbl,
      env_md = env_md_tbl
    )


    # .Object <- callNextMethod(
    #   .Object,
    #   sapf_data = sapf_data,
    #   env_data = env_data,
    #   sapf_flags = sapf_flags,
    #   env_flags = env_flags,
    #   si_code = si_code,
    #   timestamp = timestamp,
    #   solar_timestamp = solar_timestamp,
    #   site_md = site_md,
    #   stand_md = stand_md,
    #   species_md = species_md,
    #   plant_md = plant_md,
    #   env_md = env_md
    # )
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

    site_codes <- try(
      .Data %>%
        purrr::map_chr(get_si_code)
    )

    if (is(site_codes, 'try-error')) {
      stop('All elements must be sfn_data objects')
    }

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
#' @export

setMethod(
  "show", "sfn_data",
  definition = function(object) {

    si_code <- slot(object, 'si_code')
    site_md <- slot(object, 'site_md')
    stand_md <- slot(object, 'stand_md')
    species_md <- slot(object, 'species_md')
    plant_md <- slot(object, 'plant_md')
    env_md <- slot(object, 'env_md')
    sapf_data <- slot(object, 'sapf_data')
    env_data <- slot(object, 'env_data')
    sapf_flags <- slot(object, 'sapf_flags')
    env_flags <- slot(object, 'env_flags')

    # object class
    cat(class(object), " object\n", sep = "")

    # site code
    cat("Data from ", si_code, " site\n\n", sep = "")

    # main contributor
    cat(
      "Data kindly provided by ",
      paste0(c(site_md[['si_contact_firstname']], site_md[['si_contact_lastname']]),
             collapse = ' '),
      " from ", site_md[['si_contact_institution']],
      fill = 80
    )

    # additional
    if (!is.na(site_md[['si_addcontr_firstname']])) {
      cat("and ",
          paste0(c(site_md[['si_addcontr_firstname']], site_md[['si_addcontr_lastname']]),
                 collapse = ' '),
          " from ", site_md[['si_addcontr_institution']], '\n',
          fill = 80)
    } else {
      cat('\n')
    }

    # paper
    paper <- site_md[['si_paper']]

    cat('Site related literature: ', paper, '\n\n')

    # number of trees
    cat("Sapflow data: ", nrow(sapf_data), " observations of ",
        length(names(sapf_data)), " trees/plants\n")

    # species
    cat("Species present: ",
        paste0(species_md[["sp_name"]], collapse = ', '),
        '\n',
        fill = TRUE)

    # env_vars
    cat("Environmental data: ", nrow(env_data), " observations.\n")

    cat(
      "Variables present:\n ", paste(names(env_data)), "\n",
      fill = 80
    )

    # biome
    cat("Biome: ", site_md[['si_biome']], '\n\n')

    # timestamp span
    # timestamp_minmax <- .min_max(slot(object, 'timestamp'))
    timestamp_span <- lubridate::interval(
      dplyr::first(slot(object, 'timestamp')),
      dplyr::last(slot(object, 'timestamp')),
      tzone = get_timezone(object)
    ) %>%
      as.character()
    cat("TIMESTAMP span: ", timestamp_span, "\n\n")
    # solar_timestamp
    # solar_timestamp_minmax <- .min_max(slot(object, 'solar_timestamp'))
    solar_timestamp_span <- lubridate::interval(
      dplyr::first(slot(object, 'solar_timestamp')),
      dplyr::last(slot(object, 'solar_timestamp'))
    ) %>%
      as.character()
    cat("Solar TIMESTAMP span: ", solar_timestamp_span, "\n\n")

    # sapf_flags
    unique_sapf_flags <- sapf_flags %>%
      purrr::map(~ stringr::str_split(.x, '; ')) %>%
      purrr::map(purrr::flatten_chr) %>%
      purrr::flatten_chr() %>%
      stringr::str_trim('both') %>%
      unique()

    sapf_flags_table <- unique_sapf_flags[unique_sapf_flags != ''] %>%
      purrr::map(~ stringr::str_count(as.matrix(sapf_flags), .x)) %>%
      purrr::map(sum) %>%
      purrr::flatten_int()
    names(sapf_flags_table) <- unique_sapf_flags[unique_sapf_flags != '']

    cat("Sapflow data flags:\n")
    if (length(sapf_flags_table)) {
      print(sort(sapf_flags_table))
    } else {cat("No flags present\n")}
    cat("\n")

    # env_flags
    unique_env_flags <- env_flags %>%
      purrr::map(~ stringr::str_split(.x, '; ')) %>%
      purrr::map(purrr::flatten_chr) %>%
      purrr::flatten_chr() %>%
      stringr::str_trim('both') %>%
      unique()

    env_flags_table <- unique_env_flags[unique_env_flags != ''] %>%
      purrr::map(~ stringr::str_count(as.matrix(env_flags), .x)) %>%
      purrr::map(sum) %>%
      purrr::flatten_int()
    names(env_flags_table) <- unique_env_flags[unique_env_flags != '']

    cat("Environmental data flags:\n")
    if (length(env_flags_table)) {
      print(sort(env_flags_table))
    } else {cat("No flags present\n")}
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
      # we take only the first timestamp value, to make it faster in
      # large multis
      purrr::map(~ slot(.x, 'solar_timestamp')[1]) %>%
      purrr::map(as.character) %>%
      purrr::flatten_chr() %>%
      sort()
    timestamp_span <- lubridate::interval(
      dplyr::first(timestamp_minmax),
      dplyr::last(timestamp_minmax),
      tzone = "UTC"
    ) %>%
      as.character()
    cat(
      'Approximate time span (UTC) for the combined sites: ', timestamp_span,
      '\n', sep = ''
    )
  }
)

#### sfn_data get methods ######################################################
#' sfn_data get methods
#'
#' Methods to get the data and metadata from the sfn_data class slots
#'
#' \code{get_sapf_data} and \code{get_env_data} methods retrieve sapflow or environmental
#' tibbletime object to create a functional dataset to work with.
#'
#' \code{get_sapf_flags} and \code{get_env_flags} methods retrieve sapflow or
#' environmental flags as tibbletime objects.
#'
#' \code{get_timestamp} and \code{get_solar_timestamp} methods retrieve only the
#' timestamp as POSIXct vector.
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
#' @param solar Logical indicating if the timestamp to return in the \code{get_sapf_data},
#'   \code{get_env_data}, \code{get_sapf_flags} and \code{get_env_flags} methods is
#'   the solarTIMESTAMP (TRUE) or the contributors provided TIMESTAMP (FALSE)
#'
#' @examples
#' library(dplyr)
#'
#' data('ARG_TRE', package = 'sapfluxnetr')
#' sapf_data <- get_sapf_data(ARG_TRE, solar = TRUE)
#' env_data_no_solar <- get_env_data(ARG_TRE, solar = FALSE)
#' plant_md <- get_plant_md(ARG_TRE)
#'
#' # dplyr pipe to get the mean dbh for a site
#' ARG_TRE %>%
#'   get_plant_md() %>%
#'   summarise(dbh_mean = mean(pl_dbh, na.rm = TRUE)) %>%
#'   pull(dbh_mean)
#'
#' @name sfn_get_methods
#' @include sfn_data_classes.R sfn_data_generics.R
NULL

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_sapf_data", "sfn_data",
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
    res <- cbind(TIMESTAMP, .sapf) %>%
      tibbletime::as_tbl_time(index = TIMESTAMP)

    # return
    return(res)
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_data", "sfn_data",
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
    res <- cbind(TIMESTAMP, .env) %>%
      tibbletime::as_tbl_time(index = TIMESTAMP)

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
    res <- cbind(TIMESTAMP, .sapf_flags) %>%
      tibbletime::as_tbl_time(index = TIMESTAMP)

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
    res <- cbind(TIMESTAMP, .env_flags) %>%
      tibbletime::as_tbl_time(index = TIMESTAMP)

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
    slot(object, "site_md") %>%
      tibble::as_tibble()
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_stand_md", "sfn_data",
  function(object) {
    slot(object, "stand_md") %>%
      tibble::as_tibble()
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_species_md", "sfn_data",
  function(object) {
    slot(object, "species_md") %>%
      tibble::as_tibble()
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_plant_md", "sfn_data",
  function(object) {
    slot(object, "plant_md") %>%
      tibble::as_tibble()
  }
)

#' @rdname sfn_get_methods
#' @export
setMethod(
  "get_env_md", "sfn_data",
  function(object) {
    slot(object, "env_md") %>%
      tibble::as_tibble()
  }
)

#### sfn_data_multi get methods ######################################################
#' sfn_data_multi get methods
#'
#' Methods to get the data and metadata from the sfn_data class slots
#'
#' \code{get_sapf_data} and \code{get_env_data} methods retrieve sapflow or
#' environmental tibbletimes from the sfn_data objects contained in the 
#' sfn_data_multi and return them in a list.
#'
#' \code{get_sapf_flags} and \code{get_env_flags} methods retrieve sapflow or
#' environmental flags tibbletimes from the sfn_data objects contained in the 
#' sfn_data_multi and return them in a list.
#'
#' \code{get_timestamp} and \code{get_solar_timestamp} methods retrieve only the
#' timestamps as POSIXct vectors and return them as a list (each element
#' corresponding to a site in the sfn_data_multi object).
#'
#' \code{get_si_code} method retrieve a character vector with length(timestamp)
#' containing the site code for each site, returning them as a list.
#'
#' \code{get_site_md}, \code{get_stand_md}, \code{get_species_md},
#' \code{get_plant_md} and \code{get_env_md} methods retrieve the corresponding
#' metadata objects for each site returning them as a list, unless collapse is
#' TRUE, then the list collapses to a tibble.
#'
#' @param object Object of class sfn_data_multi from which data or metadata is
#'   retrieved
#'
#' @param solar Logical indicating if the timestamp to return in the \code{get_sapf_data},
#'   \code{get_env_data}, \code{get_sapf_flags} and \code{get_env_flags} methods is
#'   the solarTIMESTAMP (TRUE) or the contributors provided TIMESTAMP (FALSE)
#'
#' @param collapse Logical indicating if the metadata get methods must collapse
#'   the returning list to a data frame with all sites
#'
#' @examples
#' library(dplyr)
#'
#' @name sfn_multi_get_methods
#' @include sfn_data_classes.R sfn_data_generics.R
NULL

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_sapf_data", "sfn_data_multi",
  function(object, solar = FALSE) {
    # data
    .sapf <- object %>%
      purrr::map(slot, "sapf_data")
    
    # timestamp
    if (solar) {
      TIMESTAMP <- object %>%
        purrr::map(slot, "solar_timestamp")
    } else {
      TIMESTAMP <- TIMESTAMP <- object %>%
        purrr::map(slot, "timestamp")
    }
    
    # combining both
    res <- TIMESTAMP %>%
      purrr::map2(.sapf, cbind) %>%
      purrr::map(dplyr::rename, TIMESTAMP = ".x[[i]]") %>%
      purrr::map(~ tibbletime::as_tbl_time(.x, index = TIMESTAMP))
    
    # return
    return(res)
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_env_data", "sfn_data_multi",
  function(object, solar = FALSE) {
    # data
    .env <- object %>%
      purrr::map(slot, "env_data")
    
    # timestamp
    if (solar) {
      TIMESTAMP <- object %>%
        purrr::map(slot, "solar_timestamp")
    } else {
      TIMESTAMP <- TIMESTAMP <- object %>%
        purrr::map(slot, "timestamp")
    }
    
    # combining both
    res <- TIMESTAMP %>%
      purrr::map2(.env, cbind) %>%
      purrr::map(dplyr::rename, TIMESTAMP = ".x[[i]]") %>%
      purrr::map(~ tibbletime::as_tbl_time(.x, index = TIMESTAMP))
    
    # return
    return(res)
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_sapf_flags", "sfn_data_multi",
  function(object, solar = FALSE) {
    # data
    .flags <- object %>%
      purrr::map(slot, "sapf_flags")
    
    # timestamp
    if (solar) {
      TIMESTAMP <- object %>%
        purrr::map(slot, "solar_timestamp")
    } else {
      TIMESTAMP <- TIMESTAMP <- object %>%
        purrr::map(slot, "timestamp")
    }
    
    # combining both
    res <- TIMESTAMP %>%
      purrr::map2(.flags, cbind) %>%
      purrr::map(dplyr::rename, TIMESTAMP = ".x[[i]]") %>%
      purrr::map(~ tibbletime::as_tbl_time(.x, index = TIMESTAMP))
    
    # return
    return(res)
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_env_flags", "sfn_data_multi",
  function(object, solar = FALSE) {
    # data
    .flags <- object %>%
      purrr::map(slot, "env_flags")
    
    # timestamp
    if (solar) {
      TIMESTAMP <- object %>%
        purrr::map(slot, "solar_timestamp")
    } else {
      TIMESTAMP <- TIMESTAMP <- object %>%
        purrr::map(slot, "timestamp")
    }
    
    # combining both
    res <- TIMESTAMP %>%
      purrr::map2(.flags, cbind) %>%
      purrr::map(dplyr::rename, TIMESTAMP = ".x[[i]]") %>%
      purrr::map(~ tibbletime::as_tbl_time(.x, index = TIMESTAMP))
    
    # return
    return(res)
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_timestamp", "sfn_data_multi",
  function(object) {
    object %>%
      purrr::map(slot, "timestamp")
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_solar_timestamp", "sfn_data_multi",
  function(object) {
    object %>%
      purrr::map(slot, "solar_timestamp")
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_si_code", "sfn_data_multi",
  function(object) {
    object %>%
      purrr::map(slot, "si_code")
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_site_md", "sfn_data_multi",
  function(object, collapse = FALSE) {
    res_list <- object %>%
      purrr::map(slot, 'site_md') %>%
      purrr::map(tibble::as_tibble)
    
    if (isTRUE(collapse)) {
      return(
        res_list %>%
          dplyr::bind_rows()
      )
    } else {
      return(res_list)
    }
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_stand_md", "sfn_data_multi",
  function(object, collapse = FALSE) {
    res_list <- object %>%
      purrr::map(slot, 'stand_md') %>%
      purrr::map(tibble::as_tibble)
    
    if (isTRUE(collapse)) {
      return(
        res_list %>%
          dplyr::bind_rows()
      )
    } else {
      return(res_list)
    }
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_species_md", "sfn_data_multi",
  function(object, collapse = FALSE) {
    res_list <- object %>%
      purrr::map(slot, 'species_md') %>%
      purrr::map(tibble::as_tibble)
    
    if (isTRUE(collapse)) {
      return(
        res_list %>%
          dplyr::bind_rows()
      )
    } else {
      return(res_list)
    }
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_plant_md", "sfn_data_multi",
  function(object, collapse = FALSE) {
    res_list <- object %>%
      purrr::map(slot, 'plant_md') %>%
      purrr::map(tibble::as_tibble)
    
    if (isTRUE(collapse)) {
      return(
        res_list %>%
          dplyr::bind_rows()
      )
    } else {
      return(res_list)
    }
  }
)

#' @rdname sfn_multi_get_methods
#' @export
setMethod(
  "get_env_md", "sfn_data_multi",
  function(object, collapse = FALSE) {
    res_list <- object %>%
      purrr::map(slot, 'env_md') %>%
      purrr::map(tibble::as_tibble)
    
    if (isTRUE(collapse)) {
      return(
        res_list %>%
          dplyr::bind_rows()
      )
    } else {
      return(res_list)
    }
  }
)

#### sfn_data replacement methods ##############################################
#' sfn_data replacement methods
#'
#' Methods to replace the data and metadata from the sfn_data class slots
#'
#' The replacement object must be a valid object for that slot:
#' \itemize{
#'   \item{For \code{get_sapf_data}, \code{get_env_data}, \code{get_sapf_flags} and
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
#' @param object sfn_data containing the slot to replace
#'
#' @param value object with the data to replace snf_Data slot with
#'
#' @examples
#' # preparation
#' data('ARG_TRE', package = 'sapfluxnetr')
#' sapf_data <- get_sapf_data(ARG_TRE, solar = TRUE)
#'
#' # modifying the slot data
#' sapf_data[1:10, 2] <- NA
#'
#' # replacement. Remember, the sfn_data slot does not contain a TIMESTAMP
#' # variable, it must be removed
#' get_sapf_data(ARG_TRE) <- sapf_data[,-1]
#'
#' @name sfn_replacement_methods
NULL

#' @rdname sfn_replacement_methods
setReplaceMethod(
  "get_sapf_data", "sfn_data",
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
  "get_env_data", "sfn_data",
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
