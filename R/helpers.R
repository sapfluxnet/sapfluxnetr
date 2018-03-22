#' data coverage
#'
#' helper for sfn_metrics
#'
#' This helper function calculates the coverage percentage in a vector, and is
#' designed to be used inside a dplyr summarise statement.
#'
#' @param x a vector, usually a variable in the sapflow or environmental data.
#'
#' @return a single value (numeric) with the percentage of coverage for that
#'   variable
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   group_by(Species)
#'   summarise_all(data_coverage) # 100 for all variables
#'
#' @export

data_coverage <- function(x) {
  (sum(!is.na(x)) / length(x)) * 100
}

#' time at maximum/minimum
#'
#' helpers for sfn_metrics
#'
#' These helper functions return the TIMESTAMP value at which the maximum value
#' for other variable occurs. It is designed to be used inside a dplyr summarise
#' statement.
#'
#' @param x a numeric vector, usually a variable in the sapflow or environmental
#'   data.
#'
#' @param time a POSIXct or character vector with the TIMESTAMP values
#'
#' @return a single value (POSIXct) with the TIMESTAMP value.
#'
#' @examples
#' library(dplyr)
#' storms %>%
#'   group_by(year) %>%
#'   summarise(wind_max = max(wind),
#'             hour_at_max = max_time(wind, time = hour),
#'             wind_min = min(wind),
#'             hour_at_min = min_time(wind, time = hour))
#'
#' @name time_at_events
NULL

#' @describeIn time_at_events helper for sfn_metrics
#'
#' @export

max_time <- function(x, time) {
  time[which.max(x)]
}

#' @describeIn time_at_events helper for sfn_metrics
#'
#' @export

min_time <- function(x, time) {
  time[which.min(x)]
}

#' min max
#'
#' wrapper for quicky return the max and the min value of a vector to use in
#' a dplyr pipe
#'
#' @param x a numeric, POSIXct... (any accepted by \code{\link[base]{max}} and
#'   \code{\link[base]{min}}) vector
#'
#' @return a two-element named vector, c(min = value, max = value) with values
#'   transformed to characters
#'
#' @examples
#' library(tidiverse)
#' # pipe example, not efficient
#' iris %>%
#'   pull(Sepal.Length) %>%
#'   min_max()
#'
#' # the same, directly
#' min_max(iris$Sepal.Length)

.min_max <- function(x) {
  # c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
  c(
    min = as.character(x[which.min(x)]),
    max = as.character(x[which.max(x)])
  )
}

#' Timezones dictionary
#'
#' Tranforms timezone ISO code to character vector compatible with lubridate and
#' POSIXct
#'
#' GMT time zones are used, as they are day saving light time (DST) agnostic,
#' and in that way the DST can setted if the metadata says so. GMT are sign
#' exchanged to be compatible with ISO.
#'
#' @param tz Character vector with the ISO code of the timezone as provided in
#'   \code{env_time_zone} variable in \code{environmental_md}
#'
#' @return A character vector with the timezone code compatible with lubridate
#'   and as.POSIXct

# START
# Function declaration
.timezone_dic <- function(tz) {

  # STEP 0
  # Argument checking
  if (is.na(tz) | is.null(tz)) {
    stop('Timezone not provided in environmental metadata')
  }

  # STEP 1
  # Create the list with the codes
  timezones <- list(
    "1UTC-12:00, Y" = "Etc/GMT+12",
    "2UTC-11:00, X" = "Etc/GMT+11",
    "3UTC-10:00, W" = "Etc/GMT+10",
    "4UTC-09:30, V†" = "Pacific/Marquesas",
    "5UTC-09:00, V" = "Etc/GMT+9",
    "6UTC-08:00, U" = "Etc/GMT+8",
    "7UTC-07:00, T" = "Etc/GMT+7",
    "8UTC-06:00, S" = "Etc/GMT+6",
    "9UTC-05:00, R" = "Etc/GMT+5",
    "11UTC-04:00, Q" = "Etc/GMT+4",
    "12UTC-03:30, P†" = "Canada/Newfoundland",
    "13UTC-03:00, P" = "Etc/GMT+3",
    "14UTC-02:00, O" = "Etc/GMT+2",
    "15UTC-01:00, N" = "Etc/GMT+1",
    "16UTC±00:00, Z" = "Etc/GMT+0",
    "17UTC+01:00, A" = "Etc/GMT-1",
    "18UTC+02:00, B" = "Etc/GMT-2",
    "19UTC+03:00, C" = "Etc/GMT-3",
    "20UTC+03:30, C†" = "Asia/Tehran",
    "21UTC+04:00, D" = "Etc/GMT-4",
    "22UTC+04:30, D†" = "Asia/Kabul",
    "23UTC+05:00, E" = "Etc/GMT-5",
    "24UTC+05:30, E†" = "Asia/Kolkata",
    "25UTC+05:45, E*" = "Asia/Katmandu",
    "26UTC+06:00, F" = "Etc/GMT-6",
    "27UTC+06:30, F†" = "Indian/Cocos",
    "28UTC+07:00, G" = "Etc/GMT-7",
    "29UTC+08:00, H" = "Etc/GMT-8",
    "30UTC+08:30, H†" = "Asia/Pyongyang",
    "31UTC+08:45, H*" = "Australia/Eucla",
    "32UTC+09:00, I" = "Etc/GMT-9",
    "33UTC+09:30, I†" = "Australia/Adelaide",
    "34UTC+10:00, K" = "Etc/GMT-10",
    "35UTC+10:30, K†" = "Australia/Lord_Howe",
    "36UTC+11:00, L" = "Etc/GMT-11",
    "37UTC+12:00, M" = "Etc/GMT-12",
    "38UTC+12:45, M*" = "Pacific/Chatham",
    "39UTC+13:00, M†" = "Etc/GMT-13",
    "40UTC+14:00, M†" = "Etc/GMT-14"
  )

  # STEP 2
  # Return the timezone name compatible with lubridate
  return(timezones[[as.character(tz)]])

  # END FUNCTION
}

#' get the timezone of the site
#'
#' Obtain the site timezone from a sfn_data/sfn_data_multi object
#'
#' @param sfn_data An sfn_data or sfn_data_multi object
#'
#' @return a character with the site timezone
#'
#' @export

get_timezone <- function(
  sfn_data
) {
  # get the timezone
  .timezone_dic(get_env_md(sfn_data)[['env_time_zone']])
}
