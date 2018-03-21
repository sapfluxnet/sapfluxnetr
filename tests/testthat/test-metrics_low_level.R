context("metrics_low_level")

# data
load('FOO.RData')

FOO <- sfn_data(
  sapf_data = FOO_sapf_data,
  env_data = FOO_env_data,
  sapf_flags = FOO_sapf_flags,
  env_flags = FOO_env_flags,
  si_code = FOO_si_code,
  timestamp = FOO_timestamp,
  solar_timestamp = FOO_solar_timestamp,
  site_md = FOO_site_md,
  stand_md = FOO_stand_md,
  species_md = FOO_species_md,
  plant_md = FOO_plant_md,
  env_md = FOO_env_md
)


#### summarise_by_period tests ####
test_that('summarise_by_period function example works', {

  library(dplyr)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    )),
    regexp = '_sd', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    )),
    regexp = '_n', all = FALSE
  )

})

test_that('summarise_by_period dots work as intended', {

  # there are no tests based on values as the tests are intended to be data
  # agnostic

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start" # for collapse_index
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start" # for collapse_index
    )),
    regexp = '_sd', all = FALSE
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    )),
    regexp = '_sd', all = FALSE
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start" # for collapse_index
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start" # for collapse_index
    )),
    regexp = '_sd', all = FALSE
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    )),
    regexp = '_sd', all = FALSE
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE # for summarise
    ),
    'tbl_time'
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE # for summarise
    )),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE # for summarise
    )),
    regexp = '_sd', all = FALSE
  )

})

#### data_coverage tests ####
test_that('data_coverage works as intended', {

  data_10 <- c(rnorm(10), rep(NA, 90))
  data_20 <- c(rnorm(20), rep(NA, 80))
  data_40 <- c(rnorm(40), rep(NA, 60))
  data_80 <- c(rnorm(80), rep(NA, 20))
  data_100 <- c(rnorm(100))

  # works for doubles
  expect_equal(data_coverage(data_10), 10)
  expect_equal(data_coverage(data_20), 20)
  expect_equal(data_coverage(data_40), 40)
  expect_equal(data_coverage(data_80), 80)
  expect_equal(data_coverage(data_100), 100)

  # works for characters
  expect_equal(data_coverage(as.character(data_10)), 10)
  expect_equal(data_coverage(as.character(data_20)), 20)
  expect_equal(data_coverage(as.character(data_40)), 40)
  expect_equal(data_coverage(as.character(data_80)), 80)
  expect_equal(data_coverage(as.character(data_100)), 100)

})

#### min_time/max_time tests ####
test_that('min_time and max_time functions work as intended', {

  x <- c(1:50, 49:1)
  time <- seq.POSIXt(as.POSIXct(Sys.Date()), by = 'day', length.out = 99)

  expect_s3_class(
    max_time(x, time),
    'POSIXct'
  )

  expect_equal(
    max_time(x, time),
    time[50]
  )

  expect_s3_class(
    min_time(x, time),
    'POSIXct'
  )

  expect_equal(
    min_time(x, time),
    time[1]
  )

})
