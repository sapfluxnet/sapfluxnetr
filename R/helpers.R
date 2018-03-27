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
  
  # if all the values in x are NAs (a daily summmarise of no measures day for
  # example) this will return a length 0 POSIXct vector, which will crash
  # dplyr summarise step. So, check if all NA and if true return NA
  if(all(is.na(x))) {
    return(NA)
  } else {
    time[which.max(x)]
  }
}

#' @describeIn time_at_events helper for sfn_metrics
#'
#' @export

min_time <- function(x, time) {
  
  # if all the values in x are NAs (a daily summmarise of no measures day for
  # example) this will return a length 0 POSIXct vector, which will crash
  # dplyr summarise step. So, check if all NA and if true return NA
  if(all(is.na(x))) {
    return(NA)
  } else {
    time[which.min(x)]
  }
}

#' Diurnal centroid calculation
#' 
#' Calculate the diurnal centroid for sapflow and environmental variables
#' 
#' The code for this function has been kindly provided by Jacob Nelson in python
#' (see https://github.com/jnelson18/FluxnetTools/blob/master/FileS3.py) and has
#' been translated to a tidy data phylosophy in R to be used inside a
#' \code{\link[dplyr]{summary}} statement.
#' 
#' @section Diurnal centroid algorithm:
#' Given a continuous subdaily values at regular intervals
#' \eqn{V = {x_1, ..., x_n}} to obtain the diurnal centroid each value is
#' multiplied by its interval index and summed up and divided
#' by the sum of the values for the day and finally the value is normalized to
#' 24h:
#' 
#' \deqn{
#' \sum {x_1 * 1, x_2 * 2, ···, x_n * n} / \sum {x_1, x_2, ···, x_n} * (24/n)
#' }
#' 
#' With even values for all the intervals (i.e. 100 for all), centroid converges
#' to 12h at more than 1000 intervals per day. With only 48 (half hourly
#' measurements) centroid converges to 12.25h and with 24 intervals (hourly
#' measurements) centroid converges to 12.5h. So, using diurnal centroid value
#' in half hourly datasets or above can have a considerable error associated.
#' 
#' @param variable A numeric vector containing the values for a day at a
#'   regular intervals. Missing values are allowed but not recommended
#' 
#' @examples
#' # check convergence to 12h:
#' diurnal_centroid(rep(1, 1000)) # 12.012 h
#' diurnal_centroid(rep(10000, 1000)) # 12.012 h, variable scale not affects calculation
#' 
#' # sapflow diurnal centroid
#' data('FOO', package = 'sapfluxnetr')
#' 
#' sfn_metrics(
#'   FOO,
#'   period = 'daily',
#'   .funs = funs(diurnal_centroid(.), data_coverage(.)),
#'   solar = FALSE,
#'   predawn = FALSE,
#'   midday = FALSE
#' )
#' 
#' @return A numeric vector with the diurnal centroid value (0 to 24 h)
#' 
#' @author Jacob Nelson & Víctor Granda
#' 
#' @export

diurnal_centroid <- function(variable) {
  
  # Hack to work in POSIXct vectors which does not support * operations.
  # make sure variable is numeric (to avoid errors with POSIXct TIMESTAMP_coll)
  variable <- as.numeric(variable)
  
  steps_by_day = length(variable)
  raw_c <- sum(variable * 1:steps_by_day, na.rm = TRUE) / 
    sum(variable, na.rm = TRUE)
  res_c <- raw_c * (24 / steps_by_day)
  
  return(res_c)
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

#' as_sfn_data_multi helper
#'
#' Convert any list of sfn_data objects in a sfn_data_multi object
#'
#' @param x A list of sfn_data objects
#'
#' @return A sfn_data_multi object

as_sfn_data_multi <- function(x) {

  dplyr::quo(sfn_data_multi(!!!x)) %>%
    rlang::eval_tidy()

}

#' Metadata variables architecture
#'
#' This function returns a nested list with the metadata, the variable and the
#' values, description , units and type for it use in describe_variable,
#' sfn_vars_to_filter and sfn_values_for
#'
#' @return a list with the metadata architecture

.metadata_architecture <- function() {

  arch_list <- list(
    site_md = list(
      si_name = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Site name given by contributors'
      ),
      si_country = list(
        values = c('AFG', 'ALA', 'ALB', 'DZA', 'ASM', 'AND', 'AGO', 'AIA', 'ATA',
                   'ATG', 'ARG', 'ARM', 'ABW', 'AUS', 'AUT', 'AZE', 'BHS', 'BHR',
                   'BGD', 'BRB', 'BLR', 'BEL', 'BLZ', 'BEN', 'BMU', 'BTN', 'BOL',
                   'BES', 'BIH', 'BWA', 'BVT', 'BRA', 'IOT', 'BRN', 'BGR', 'BFA',
                   'BDI', 'CPV', 'KHM', 'CMR', 'CAN', 'CYM', 'CAF', 'TCD', 'CHL',
                   'CHN', 'CXR', 'CCK', 'COL', 'COM', 'COG', 'COD', 'COK', 'CRI',
                   'CIV', 'HRV', 'CUB', 'CUW', 'CYP', 'CZE', 'DNK', 'DJI', 'DMA',
                   'DOM', 'ECU', 'EGY', 'SLV', 'GNQ', 'ERI', 'EST', 'ETH', 'FLK',
                   'FRO', 'FJI', 'FIN', 'FRA', 'GUF', 'PYF', 'ATF', 'GAB', 'GMB',
                   'GEO', 'DEU', 'GHA', 'GIB', 'GRC', 'GRL', 'GRD', 'GLP', 'GUM',
                   'GTM', 'GGY', 'GIN', 'GNB', 'GUY', 'HTI', 'HMD', 'VAT', 'HND',
                   'HKG', 'HUN', 'ISL', 'IND', 'IDN', 'IRN', 'IRQ', 'IRL', 'IMN',
                   'ISR', 'ITA', 'JAM', 'JPN', 'JEY', 'JOR', 'KAZ', 'KEN', 'KIR',
                   'PRK', 'KOR', 'KWT', 'KGZ', 'LAO', 'LVA', 'LBN', 'LSO', 'LBR',
                   'LBY', 'LIE', 'LTU', 'LUX', 'MAC', 'MKD', 'MDG', 'MWI', 'MYS',
                   'MDV', 'MLI', 'MLT', 'MHL', 'MTQ', 'MRT', 'MUS', 'MYT', 'MEX',
                   'FSM', 'MDA', 'MCO', 'MNG', 'MNE', 'MSR', 'MAR', 'MOZ', 'MMR',
                   'NAM', 'NRU', 'NPL', 'NLD', 'NCL', 'NZL', 'NIC', 'NER', 'NGA',
                   'NIU', 'NFK', 'MNP', 'NOR', 'OMN', 'PAK', 'PLW', 'PSE', 'PAN',
                   'PNG', 'PRY', 'PER', 'PHL', 'PCN', 'POL', 'PRT', 'PRI', 'QAT',
                   'REU', 'ROU', 'RUS', 'RWA', 'BLM', 'SHN', 'KNA', 'LCA', 'MAF',
                   'SPM', 'VCT', 'WSM', 'SMR', 'STP', 'SAU', 'SEN', 'SRB', 'SYC',
                   'SLE', 'SGP', 'SXM', 'SVK', 'SVN', 'SLB', 'SOM', 'ZAF', 'SGS',
                   'SSD', 'ESP', 'LKA', 'SDN', 'SUR', 'SJM', 'SWZ', 'SWE', 'CHE',
                   'SYR', 'TWN', 'TJK', 'TZA', 'THA', 'TLS', 'TGO', 'TKL', 'TON',
                   'TTO', 'TUN', 'TUR', 'TKM', 'TCA', 'TUV', 'UGA', 'UKR', 'ARE',
                   'GBR', 'USA', 'UMI', 'URY', 'UZB', 'VUT', 'VEN', 'VNM', 'VGB',
                   'VIR', 'WLF', 'ESH', 'YEM', 'ZMB', 'ZWE'),
        type = "Character",
        units = "Fixed values",
        description = "Country code (ISO)"
      ),
      si_contact_firstname = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Contributor first name'
      ),
      si_contact_lastname = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Contributor last name'
      ),
      si_contact_email = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Contributor email'
      ),
      si_contact_institution = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Contributor affiliation'
      ),
      si_addcontr_firstname = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Additional contributor first name'
      ),
      si_addcontr_lastname = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Additional contributor last name'
      ),
      si_addcontr_email = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Additional contributor email'
      ),
      si_addcontr_institution = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Additional contributor affiliation'
      ),
      si_lat = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'Latitude, decimal format',
        description = 'Site latitude (i.e. 42.36)'
      ),
      si_long = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'Longitude, decimal format',
        description = 'Site longitude (i.e. -8.23)'
      ),
      si_elev = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'meters',
        description = 'Elevation above sea level'
      ),
      si_paper = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'DOI link',
        description = paste0(
          'Paper with relevant information to understand the site',
          ' as DOI links or DOI codes'
        )
      ),
      si_dist_mgmt = list(
        units = c('Agriculture', 'Drought', 'Fire', 'Forestry', 'Grazing',
                  'Hydrologic event', 'Land cover change', 'Pests and disease',
                  'NULL'),
        type = "Character",
        units = "Fixed values",
        description = paste0(
          "Recent and historic disturbance and management events that",
          " affected the measurement years"
        )
      ),
      si_igbp = list(
        values = c('BSV', 'CRO', 'CSH', 'CVM', 'DBF', 'DNF', 'EBF',
                   'ENF', 'MF', 'OSH', 'SAV', 'URB', 'WET', 'WSA'),
        type = 'Character',
        units = "Fixed values",
        description = "Vegetation type based on IGBP definition"
      ),
      si_flux_network = list(
        values = c(TRUE, FALSE),
        type = 'Logical',
        units = 'Fixed values',
        description = paste0(
          'Logical indicating if site is participating in the FLUXNET network'
        )
      ),
      si_dendro_network = list(
        values = c(TRUE, FALSE),
        type = 'Logical',
        units = 'Fixed values',
        description = paste0(
          'Logical indicating if site is participating in the DENDROGLOBAL',
          ' network'
        )
      ),
      si_remarks = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = paste0(
          'Remarks and commentaries useful to grasp some site-specific',
          ' peculiarities'
        )
      ),
      si_code = list(
        values = 'sapfluxnet defined',
        type = 'Character',
        units = 'Fixed value',
        description = 'sapfluxnet site code, unique for each site'
      ),
      si_mat = list(
        values = 'sapfluxnet calculated',
        type = 'Numeric',
        units = 'Celsius degrees',
        description = 'Site annual mean temperature, as obtained from WorldClim'
      ),
      si_map = list(
        values = 'sapfluxnet calculated',
        type = 'Numeric',
        units = 'mm',
        description = 'Site annual mean precipitation, as obtained from WorldClim'
      ),
      si_biome = list(
        values = '',
        type = 'Character',
        units = 'sapfluxnet calculated',
        description = paste0(
          'Biome classification as per Whitaker diagram, based on mat and',
          ' map obtained from WorldClim'
        )
      )
    ),
    stand_md = list(
      st_name = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Stand name given by contributors'
      ),
      st_growth_condition = list(
        values = c('Naturally regenerated, unmanaged',
                   'Naturally regenerated, managed',
                   'Plantation, managed', 'Plantation, unmanaged',
                   'Orchard', 'Urban'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Growth condition with respect to stand origin and management'
      ),
      st_treatment = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Treatment applied at stand level'
      ),
      st_age = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'years',
        description = 'Mean stand age at the moment of sap flow measurements'
      ),
      st_height = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'meters',
        description = 'Canopy height'
      ),
      st_density = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'stems/ha',
        description = 'Total stem density for stand'
      ),
      st_basal_area = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'm2/ha',
        description = 'Total stand basal area'
      ),
      st_lai = list(
        values = 'Coontributor defined',
        type = 'Numeric',
        units = '',
        description = 'Total maximum stand leaf area (one-sided, projected)'
      ),
      st_aspect = list(
        values = c('Flat', 'N', 'E', 'S', 'W'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Aspect the stand is facing (exposure)'
      ),
      st_terrain = list(
        values = c('Flat', 'Undulated/Variable', 'Valley', 'Gentle slope (<2 %)',
                   'Medium Slope (>2 %, <5%)', 'Significant Slope (>5%, <10%)',
                   'Strong Slope (>10%)', 'Hilltop'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Slope and/or relief of the stand'
      ),
      st_soil_depth = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Soil total depth'
      ),
      st_soil_texture = list(
        values = c('SAND', 'LOAM', 'SILT', 'CLAY'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Soil texture class, based on USDA classification'
      ),
      st_sand_perc = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = '% percentage',
        description = 'Soil sand content, % mass'
      ),
      st_silt_perc = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = '% percentage',
        description = 'Soil silt content, % mass'
      ),
      st_clay_perc = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = '% percentage',
        description = 'Soil clay content, % mass'
      ),
      st_remarks = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = paste0(
          'Remarks and commentaries useful to grasp some stand-specific',
          ' peculiarities'
        )
      ),
      st_USDA_soil_texture = list(
        values = '',
        type = 'Character',
        units = 'sapfluxnet calculated',
        description = paste0(
          'USDA soil classification based on the percentages provided by',
          ' the contributor'
        )
      )
    ),
    species_md = list(
      sp_name = list(
        values = 'Contributor defined',
        type = 'Character',
        units = paste0(
          'Scientific name without author abbreviation, as accepted by The',
          ' Plant List'
        ),
        description = 'Identity of each measured species'
      ),
      sp_ntrees = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'number of trees',
        description = 'Number of trees measured of each species'
      ),
      sp_leaf_habit = list(
        values = c('evergreen', 'cold deciduous',
                   'drought deciduous', 'marcescent'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Leaf habit of the measured species'
      ),
      sp_basal_area_perc = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = '% percentage',
        description = paste0(
          'Basal area occupied by each measured species, in percentage',
          ' over total stand basal area'
        )
      )
    ),
    plant_md = list(
      pl_name = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Plant code assigned by contributors'
      ),
      pl_species = list(
        values = 'Contributor defined',
        type = 'Character',
        units = paste0(
          'Scientific name without author abbreviation, as accepted by The',
          ' Plant List'
        ),
        description = 'Species identity of the measured plant'
      ),
      pl_treatment = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = 'Experimental treatment (if any)'
      ),
      pl_dbh = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Diameter at breast height of measured plants'
      ),
      pl_height = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'm',
        description = 'Height of measured plants'
      ),
      pl_age = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'years',
        description = 'Plant age at the moment of measure'
      ),
      pl_social = list(
        values = c('dominant', 'codominant', 'suppressed'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Plant social status'
      ),
      pl_sapw_area = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm2',
        description = 'Cross-sectional sapwood area'
      ),
      pl_sapw_depth = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Sapwood depth, measured at breast height'
      ),
      pl_bark_thick = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Plant bark thickness'
      ),
      pl_leaf_area = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'cm2',
        description = 'Leaf area of eachvvmeasured plant'
      ),
      pl_sens_meth = list(
        values = c('CAG', 'HD', 'CHP', 'CHD', 'HFD', 'HPTM',
                   'HR', 'SFPLUS', 'SHB', 'TSHB', 'Other/unknown'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Sap flow measures method'
      ),
      pl_sens_man = list(
        values = c('Lab made', 'Dynamax', 'UP GmbH', 'Ecomatik', 'PlantSensors',
                   'ICT International', 'Ems Brno', 'East30', 'Tranzflo', 'Phytech',
                   'Puech Asociados', 'Advanced Measurements and Controls',
                   'HortResearch', 'Greenspan Technology', 'Other/unknown'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Sap flow measures sensor manufacturer'
      ),
      pl_sens_cor_grad = list(
        units = c('No correction', 'NTG separately measured',
                  'NTG measured in cyclic heating deisgn','NTG modelled',
                  'Other/unknown'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Correction for natural temperature gradients method'
      ),
      pl_sens_cor_zero = list(
        values = c('Previous night zero flow', 'Long time-window zero flow',
                   'Moist nights zero flow', 'Manipulative zero flow',
                   'Not needed', 'Other/unknown'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Zero flow determination method'
      ),
      pl_sens_calib = list(
        values = '',
        type = 'Character',
        units = 'Fixed values',
        description = 'Species-specific calibration used'
      ),
      pl_sap_units = list(
        values = c('“cm3 cm-2 h-1”', '“cm3 h-1”'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Uniformized sapfluxnet units for sapwood, leaf and plant level'
      ),
      pl_sap_units_orig = list(
        values = c('“cm3 cm-2 h-1”', '“cm3 m-2 s-1”', '“dm3 dm-2 h-1”',
                   '“dm3 dm-2 s-1”', '“mm3 mm-2 s-1”', '“g m-2 s-1”',
                   '“kg m-2 h-1”', '“kg m-2 s-1”', '“cm3 s-1”',
                   '“cm3 h-1”', '“dm3 h-1”', '“g h-1”', '“kg h-1”'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Original contribution units (by sapwood or plant level)'
      ),
      pl_sens_length = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'mm',
        description = 'Length of the needles or electrodes forming the sensor'
      ),
      pl_sens_hgt = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'm',
        description = 'Sensor installation height, measured from the ground'
      ),
      pl_sens_timestep = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'minutes',
        description = 'Subdaily time step of sensor measures'
      ),
      pl_radial_int = list(
        values = c('No radial correction', 'Sensor-integrated', 'Measured',
                   'Corrected, measured radial variation',
                   'Corrected, species coefficients',
                   'Corrected, other coefficients'),
        type = 'Character',
        units = 'Fixed values',
        description = ''
      ),
      pl_azimut_int = list(
        values = c('No azimuthal correction', 'sensor_integrated', 'measured',
                   'Corrected, measured azimuthal variation'),
        type = 'Character',
        units = 'Fixed values',
        description = ''
      ),
      pl_remarks = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = paste0(
          'Remarks and commentaries useful to grasp some plant-specific',
          ' peculiarities'
        )
      ),
      pl_code = list(
        values = 'sapfluxnet defined',
        type = 'Character',
        units = 'Fixed value',
        description = 'sapfluxnet plant code, unique for each plant'
      )
    ),
    env_md = list(
      env_time_zone = list(
        values = c('1UTC-12:00, Y', '2UTC-11:00, X', '3UTC-10:00, W',
                   '4UTC-09:30, V†', '5UTC-09:00, V', '6UTC-08:00, U',
                   '7UTC-07:00, T', '8UTC-06:00, S',
                   '9UTC-05:00, R', '10UTC-04:30, Q†', '11UTC-04:00, Q',
                   '12UTC-03:30, P†', '13UTC-03:00, P', '14UTC-02:00, O',
                   '15UTC-01:00, N','16UTC±00:00, Z', '17UTC+01:00, A',
                   '18UTC+02:00, B', '19UTC+03:00, C', '20UTC+03:30, C†',
                   '21UTC+04:00, D', '22UTC+04:30, D†', '23UTC+05:00, E',
                   '24UTC+05:30, E†', '25UTC+05:45, E*', '26UTC+06:00, F',
                   '27UTC+06:30, F†', '28UTC+07:00, G', '29UTC+08:00, H',
                   '30UTC+08:30, H†', '31UTC+08:45, H*', '32UTC+09:00, I',
                   '33UTC+09:30, I†', '34UTC+10:00, K', '35UTC+10:30, K†',
                   '36UTC+11:00, L', '37UTC+12:00, M', '38UTC+12:45, M*',
                   '39UTC+13:00, M†', '40UTC+14:00, M†'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Time zone of site used in the TIMESTAMPS'
      ),
      env_time_daylight = list(
        values = c(TRUE, FALSE),
        type = 'Logical',
        units = 'Fixed values',
        description = 'Is daylight saving time applied to the original timestamp?'
      ),
      env_timestep = list(
        values = 'Contributor defined',
        type = 'Numeric',
        units = 'minutes',
        description = 'Subdaily timestep of environmental measures'
      ),
      env_ta = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of air temperature sensor'
      ),
      env_rh = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of relative humidity sensor'
      ),
      env_vpd = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of relative vapour pressure decifit sensor'
      ),
      env_sw_in = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of shortwave incoming radiation sensor'
      ),
      env_ppfd_in = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of incoming photosynthetic photon flux density sensor'
      ),
      env_netrad = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of net radiation sensor'
      ),
      env_ws = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of wind speed sensor'
      ),
      env_precip = list(
        values = c('Above canopy', 'Within canopy', 'Clearing',
                   'Off-site', 'Not provided'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Location of precipitation sensor'
      ),
      env_swc_shallow_depth = list(
        values = 'COntributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Average depth for shallow soil water content measures'
      ),
      env_swc_deep_depth = list(
        values = 'COntributor defined',
        type = 'Numeric',
        units = 'cm',
        description = 'Average depth for deep soil water content measures'
      ),
      env_plant_watpot = list(
        values = c('leaf: predawn', 'leaf: midday', 'xylem: predawn',
                   'xylem: midday', 'leaf: predawn and midday',
                   'xylem: predawn and midday', 'xylem: continuous',
                   'leaf: continuous'),
        type = 'Character',
        units = 'Fixed values',
        description = paste0(
          'Availability of water potential values for the same measured plants',
          ' during the sap flow measurements period'
        )
      ),
      env_leafarea_seasonal = list(
        values = c('stand level', 'species level', 'tree level', 'NULL'),
        type = 'Character',
        units = 'Fixed values',
        description = 'Availability of seasonal course leaf area data and level'
      ),
      env_remarks = list(
        values = 'Contributor defined',
        type = 'Character',
        units = 'None',
        description = paste0(
          'Remarks and commentaries useful to grasp some environmental-specific',
          ' peculiarities'
        )
      )
    )
  )

  return(arch_list)

  # TODO fill the metadata_arch list wth values for biome, pl_sens_calib, st_USDA*
  # and check the units for all numeric variables, as I'm sure they are wrong in
  # some cases
  
}

#' List all variables that can be used to filter sites
#'
#' \code{sfn_vars_to_filter()} returns a list with the variables for each
#' kind of metadata that can be used to select and filter sites
#'
#' @examples
#' # all variables
#' sfn_vars_to_filter()
#'
#' # by some metadata
#' sfn_vars_to_filter()$site_md
#'
#' @return A list with five elements, \code{site_md}, \code{stand_md},
#'   \code{species_md}, \code{plant_md} and \code{env_md}
#'
#' @export

sfn_vars_to_filter <- function() {

  # we get a nested list with the metadata and call names on the first level,
  # obtaining the names of the variables in each metadata
  .metadata_architecture() %>%
    purrr::modify(names)

}

#' List all possible values for a filtering metadata variable
#'
#' \code{sfn_values_for} function returns a vector with the accepted values for
#' the required metadata variable, and can be used to filter the sites with
#' \code{\link{filter_by_var}}
#'
#' @param variable Character indicating the metadata variable to look for
#'   accepted values
#'
#' @examples
#' # accepted values for plant sensor method (pl_sens_meth)
#' sfn_values_for('pl_sens_meth')
#'
#' @return A character vector with the accepted values for the desired variable
#'
#' @export

sfn_values_for <- function(variable = 'pl_sens_meth') {

  # we create a nested list with the metadata, the variable and the values and
  # we call purrr::modify_depth to extract the desired value
  .metadata_architecture() %>%
    purrr::modify(c(variable, 'values')) %>%
    purrr::flatten_chr()

}

#' Detailed description of metadata variables
#' 
#' \code{describe_md_variable} prints in console a detailed description for the
#' requested variable. Useful to know which values to filter or in which units
#' the variables are.
#' 
#' @param variable A character with the name of the variable 
#'
#' @return Nothing, prints information to console
#' @export
#'
#' @examples
#' # info avout pl_sens_meth
#' describe_md_variable('pl_sens_meth')

describe_md_variable <- function(variable) {
  
  arch_list <- .metadata_architecture()
  
  # description
  cat('Description:\n')
  arch_list %>%
    purrr::modify(c(variable, 'description')) %>%
    purrr::flatten_chr() %>%
    cat('\n', sep = '', fill = 80)
  
  # values
  cat('Values:\n')
  arch_list %>%
    purrr::modify(c(variable, 'values')) %>%
    purrr::flatten_chr() %>%
    cat('\n', sep = ' | ', fill = 80)
  
  # units
  cat('Units:\n')
  arch_list %>%
    purrr::modify(c(variable, 'units')) %>%
    purrr::flatten_chr() %>%
    cat('\n\n', sep = '')
  
  # type
  cat('Type:\n')
  arch_list %>%
    purrr::modify(c(variable, 'type')) %>%
    purrr::flatten_chr() %>%
    cat('\n', sep = '')
}
