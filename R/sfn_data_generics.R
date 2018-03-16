#' SfnData custom get generics
#'
#' Generics for getting the info in the slots of SfnData
#'
#' @name sfn_get_generics
#' @include SfnData_class.R
NULL

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_sapf",
  function(object, ...) {
    standardGeneric("get_sapf")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_env",
  function(object, ...) {
    standardGeneric("get_env")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_sapf_flags",
  function(object, ...) {
    standardGeneric("get_sapf_flags")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_env_flags",
  function(object, ...) {
    standardGeneric("get_env_flags")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_timestamp",
  function(object, ...) {
    standardGeneric("get_timestamp")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_solar_timestamp",
  function(object, ...) {
    standardGeneric("get_solar_timestamp")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_si_code",
  function(object, ...) {
    standardGeneric("get_si_code")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_site_md",
  function(object, ...) {
    standardGeneric("get_site_md")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_stand_md",
  function(object, ...) {
    standardGeneric("get_stand_md")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_species_md",
  function(object, ...) {
    standardGeneric("get_species_md")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_plant_md",
  function(object, ...) {
    standardGeneric("get_plant_md")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_env_md",
  function(object, ...) {
    standardGeneric("get_env_md")
  }
)

#' Replacement generics
#'
#' Generic functions for replacement functions for SfnData
#'
#' @name sfn_replacement_generics
NULL

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_sapf<-",
  function(object, value) {
    standardGeneric("get_sapf<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_env<-",
  function(object, value) {
    standardGeneric("get_env<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_sapf_flags<-",
  function(object, value) {
    standardGeneric("get_sapf_flags<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_env_flags<-",
  function(object, value) {
    standardGeneric("get_env_flags<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_timestamp<-",
  function(object, value) {
    standardGeneric("get_timestamp<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_solar_timestamp<-",
  function(object, value) {
    standardGeneric("get_solar_timestamp<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_si_code<-",
  function(object, value) {
    standardGeneric("get_si_code<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_site_md<-",
  function(object, value) {
    standardGeneric("get_site_md<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_stand_md<-",
  function(object, value) {
    standardGeneric("get_stand_md<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_species_md<-",
  function(object, value) {
    standardGeneric("get_species_md<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_plant_md<-",
  function(object, value) {
    standardGeneric("get_plant_md<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_env_md<-",
  function(object, value) {
    standardGeneric("get_env_md<-")
  }
)
