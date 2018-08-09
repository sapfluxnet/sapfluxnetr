#' sfn_data custom get generics
#'
#' Generics for getting the info in the slots of SfnData
#'
#' see \code{\link{sfn_get_methods}} for detailed info about using the get
#' methods in \code{sfn_data} class objects and
#' \code{\link{sfn_multi_get_methods}} for detailed info about using the get
#' methods in \code{sfn_data_multi} class objects.
#' 
#' @param object Object to get data from
#' 
#' @param ... Further arguments to pass to the corresponding get method
#'
#' @name sfn_get_generics
#' @include sfn_data_classes.R
NULL

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_sapf_data",
  function(object, ...) {
    standardGeneric("get_sapf_data")
  }
)

#' @rdname sfn_get_generics
#' @export
setGeneric(
  "get_env_data",
  function(object, ...) {
    standardGeneric("get_env_data")
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

#' sfn_data replacement generics
#'
#' Generic functions for replacement functions for sfn_data
#'
#' see \code{\link{sfn_replacement_methods}} for more info about using the
#' replacement methods in sfn_data objects
#' 
#' @param object Object to replace
#' 
#' @param value Object to replace with
#'
#' @name sfn_replacement_generics
NULL

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_sapf_data<-",
  function(object, value) {
    standardGeneric("get_sapf_data<-")
  }
)

#' @rdname sfn_replacement_generics
#' @export
setGeneric(
  "get_env_data<-",
  function(object, value) {
    standardGeneric("get_env_data<-")
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
