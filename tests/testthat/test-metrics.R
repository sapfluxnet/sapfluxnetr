context("metrics")

# data
data('ARG_TRE', package = 'sapfluxnetr')
data('ARG_MAZ', package = 'sapfluxnetr')
data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')
multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)

#### summarise_by_period tests ####
test_that('summarise_by_period function example works', {

  library(dplyr)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )

  expect_s3_class(
    summarise_by_period(
      data = get_env_data(ARG_TRE),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )

  test_expr <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = '7 days',
    .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
  )

  expect_match(
    names(test_expr),
    regexp = '_mean', all = FALSE
  )

  expect_match(
    names(test_expr),
    regexp = '_sd', all = FALSE
  )

  expect_match(
    names(test_expr),
    regexp = '_n', all = FALSE
  )

})

test_that('summarise_by_period dots work as intended', {

  # there are no tests based on values as the tests are intended to be data
  # agnostic

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )

  test_expr <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    na.rm = TRUE, # for summarise
    side = "start" # for collapse_index
  )

  expect_match(names(test_expr), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )

  test_expr2 <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    na.rm = TRUE, # for summarise
    side = "start", # for collapse_index
    clean = TRUE # for collapse_index
  )

  expect_match(names(test_expr2), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr2), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )

  test_expr3 <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    side = "start" # for collapse_index
  )

  expect_match(names(test_expr3), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr3), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )

  test_expr4 <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    side = "start", # for collapse_index
    clean = TRUE # for collapse_index
  )

  expect_match(names(test_expr4), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr4), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(ARG_TRE),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE # for summarise
    ),
    'tbl_time'
  )

  test_expr5 <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    na.rm = TRUE # for summarise
  )

  expect_match(names(test_expr5), regexp = '_mean', all = FALSE)

  expect_match(names(test_expr5), regexp = '_sd', all = FALSE)

  test_expr6 <- summarise_by_period(
    data = get_env_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(
      mean(., na.rm = TRUE), sd(., na.rm = TRUE), centroid = diurnal_centroid(.)
    )
  )

  expect_failure(
    expect_match(names(test_expr6), regexp = '_centroid', all = FALSE)
  )

  test_expr7 <- summarise_by_period(
    data = get_sapf_data(ARG_TRE),
    period = 'daily',
    .funs = dplyr::funs(
      mean(., na.rm = TRUE), sd(., na.rm = TRUE), centroid = diurnal_centroid(.)
    )
  )

  expect_match(names(test_expr7), regexp = '_centroid', all = FALSE)

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

#### diurnal_centroid tests ####
test_that('diurnal_centroid function works with even data', {

  variable <- rep(1, 48)
  variable_2 <- rep(1000, 48)

  expect_true(is.numeric(diurnal_centroid(variable)))
  expect_equal(diurnal_centroid(variable), 12.25)
  expect_equal(diurnal_centroid(variable_2), 12.25)
  expect_identical(diurnal_centroid(variable), diurnal_centroid(variable_2))

  variable_3 <- rep(1, 24)
  variable_4 <- rep(1000, 24)

  expect_equal(diurnal_centroid(variable_3), 12.5)
  expect_equal(diurnal_centroid(variable_4), 12.5)
  expect_identical(diurnal_centroid(variable_3), diurnal_centroid(variable_4))

})

#### min_time/max_time tests ####
test_that('min_time and max_time functions work as intended', {

  x <- c(1:50, 49:1)
  time <- seq.POSIXt(as.POSIXct(Sys.Date()), by = 'day', length.out = 99)

  expect_s3_class(max_time(x, time), 'POSIXct')
  expect_equal(max_time(x, time), time[50])
  expect_s3_class(min_time(x, time), 'POSIXct')
  expect_equal(min_time(x, time), time[1])

})

test_that('max and min_time functions return NA when all values are NA', {

  x <- rep(NA, 99)
  time <- seq.POSIXt(as.POSIXct(Sys.Date()), by = 'day', length.out = 99)

  expect_true(is.na(min_time(x, time)))
  expect_true(is.na(max_time(x, time)))

})

#### sfn_metrics tests ####
test_that('sfn_metrics for general metrics works', {

  library(dplyr)

  test_expr <- sfn_metrics(
    ARG_TRE,
    period = '7 days',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE,
    interval = 'general'
  )

  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = '7 days',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE,
    interval = 'general'
  )

  # test sfn_data
  expect_true(is.list(test_expr))
  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_s3_class(test_expr[['sapf']], 'tbl')
  expect_s3_class(test_expr[['env']], 'tbl')

  # test sfn_data_multi
  expect_true(is.list(test_expr2))
  expect_identical(names(test_expr2), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
  expect_s3_class(test_expr2[['ARG_MAZ']][['sapf']], 'tbl')
  expect_s3_class(test_expr2[['ARG_MAZ']][['env']], 'tbl')

  # sfn_data and sfn_data_multi returns the same results for the same sites
  expect_equal(test_expr[['sapf']], test_expr2[['ARG_TRE']][['sapf']])
  expect_equal(test_expr[['env']], test_expr2[['ARG_TRE']][['env']])

})

test_that('sfn_metrics for predawn metrics works', {

  library(dplyr)

  test_expr <- sfn_metrics(
    ARG_TRE,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'predawn', int_start = 4, int_end = 6
  )

  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'predawn', int_start = 4, int_end = 6
  )

  # test sfn_data
  expect_true(is.list(test_expr))
  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_s3_class(test_expr[['sapf']], 'tbl')
  expect_s3_class(test_expr[['env']], 'tbl')
  expect_true(all(stringr::str_detect(names(test_expr[['sapf']]), '_pd')))
  expect_true(all(stringr::str_detect(names(test_expr[['env']]), '_pd')))

  # test sfn_data_multi
  expect_true(is.list(test_expr2))
  expect_identical(names(test_expr2), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
  expect_s3_class(test_expr2[['ARG_MAZ']][['sapf']], 'tbl')
  expect_s3_class(test_expr2[['ARG_MAZ']][['env']], 'tbl')
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['sapf']]), '_pd'))
  )
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['env']]), '_pd'))
  )

  # sfn_data and sfn_data_multi returns the same results for the same sites
  expect_equal(test_expr[['sapf']], test_expr2[['ARG_TRE']][['sapf']])
  expect_equal(test_expr[['env']], test_expr2[['ARG_TRE']][['env']])

})

test_that('sfn_metrics for midday metrics works', {

  library(dplyr)

  test_expr <- sfn_metrics(
    ARG_TRE,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'midday', int_start = 11, int_end = 13
  )

  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'midday', int_start = 11, int_end = 13
  )

  # test sfn_data
  expect_true(is.list(test_expr))
  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_s3_class(test_expr[['sapf']], 'tbl')
  expect_s3_class(test_expr[['env']], 'tbl')
  expect_true(all(stringr::str_detect(names(test_expr[['sapf']]), '_md')))
  expect_true(all(stringr::str_detect(names(test_expr[['env']]), '_md')))

  # test sfn_data_multi
  expect_true(is.list(test_expr2))
  expect_identical(names(test_expr2), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
  expect_s3_class(test_expr2[['ARG_MAZ']][['sapf']], 'tbl')
  expect_s3_class(test_expr2[['ARG_MAZ']][['env']], 'tbl')
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['sapf']]), '_md'))
  )
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['env']]), '_md'))
  )

  # sfn_data and sfn_data_multi returns the same results for the same sites
  expect_equal(test_expr[['sapf']], test_expr2[['ARG_TRE']][['sapf']])
  expect_equal(test_expr[['env']], test_expr2[['ARG_TRE']][['env']])

})

test_that('sfn_metrics for nightly metrics works', {

  library(dplyr)

  test_expr <- sfn_metrics(
    ARG_TRE,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'night', int_start = 20, int_end = 6
  )

  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'night', int_start = 20, int_end = 6
  )

  test_expr3 <- sfn_metrics(
    ARG_TRE,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'night', int_start = 20, int_end = 6,
    clean = FALSE
  )

  # test sfn_data
  expect_true(is.list(test_expr))
  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_s3_class(test_expr[['sapf']], 'tbl')
  expect_s3_class(test_expr[['env']], 'tbl')
  expect_true(all(stringr::str_detect(names(test_expr[['sapf']]), '_night')))
  expect_true(all(stringr::str_detect(names(test_expr[['env']]), '_night')))

  # test sfn_data_multi
  expect_true(is.list(test_expr2))
  expect_identical(names(test_expr2), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
  expect_s3_class(test_expr2[['ARG_MAZ']][['sapf']], 'tbl')
  expect_s3_class(test_expr2[['ARG_MAZ']][['env']], 'tbl')
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['sapf']]), '_night'))
  )
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['env']]), '_night'))
  )

  # sfn_data and sfn_data_multi returns the same results for the same sites
  expect_equal(test_expr[['sapf']], test_expr2[['ARG_TRE']][['sapf']])
  expect_equal(test_expr[['env']], test_expr2[['ARG_TRE']][['env']])

  # night works as expected
  sapf_night_timestamp <- test_expr[['sapf']][['TIMESTAMP_night']]
  env_night_timestamp <- test_expr[['env']][['TIMESTAMP_night']]

  good_sapf_night_first <- "2009-11-17 22:00:00"
  good_sapf_night_second <- "2009-11-18 20:00:00"
  good_sapf_night_last <- "2009-11-30 20:00:00"

  good_env_night_first <- "2009-11-17 22:00:00"
  good_env_night_second <- "2009-11-18 20:00:00"
  good_env_night_last <- "2009-11-30 20:00:00"

  expect_equal(as.character(sapf_night_timestamp[1]), good_sapf_night_first)
  expect_equal(as.character(sapf_night_timestamp[2]), good_sapf_night_second)
  expect_equal(as.character(sapf_night_timestamp[14]), good_sapf_night_last)
  expect_equal(as.character(env_night_timestamp[1]), good_env_night_first)
  expect_equal(as.character(env_night_timestamp[2]), good_env_night_second)
  expect_equal(as.character(env_night_timestamp[14]), good_env_night_last)

  # lets be sure that without clean the hours are cutted where they must be
  # cutted
  expect_identical(names(test_expr3), c('sapf', 'env'))
  expect_s3_class(test_expr3[['sapf']], 'tbl_time')
  expect_s3_class(test_expr3[['env']], 'tbl_time')

  sapf_night_timestamp_3 <- test_expr3[['sapf']][['TIMESTAMP_night']]
  env_night_timestamp_3 <- test_expr3[['env']][['TIMESTAMP_night']]

  good_sapf_night_first_3 <- "2009-11-17 22:24:58"
  good_sapf_night_second_3 <- "2009-11-18 20:24:43"
  good_sapf_night_last_3 <- "2009-11-30 20:20:48"

  good_env_night_first_3 <- "2009-11-17 22:24:58"
  good_env_night_second_3 <- "2009-11-18 20:24:43"
  good_env_night_last_3 <- "2009-11-30 20:20:48"

  expect_equal(as.character(sapf_night_timestamp_3[1]), good_sapf_night_first_3)
  expect_equal(as.character(sapf_night_timestamp_3[2]), good_sapf_night_second_3)
  expect_equal(as.character(sapf_night_timestamp_3[14]), good_sapf_night_last_3)
  expect_equal(as.character(env_night_timestamp_3[1]), good_env_night_first_3)
  expect_equal(as.character(env_night_timestamp_3[2]), good_env_night_second_3)
  expect_equal(as.character(env_night_timestamp_3[14]), good_env_night_last_3)

})

test_that('sfn_metrics for daylight metrics works', {

  library(dplyr)

  test_expr <- sfn_metrics(
    ARG_TRE,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'daylight', int_start = 6, int_end = 20
  )

  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = TRUE,
    interval = 'daylight', int_start = 6, int_end = 20
  )

  # test sfn_data
  expect_true(is.list(test_expr))
  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_s3_class(test_expr[['sapf']], 'tbl')
  expect_s3_class(test_expr[['env']], 'tbl')
  expect_true(all(stringr::str_detect(names(test_expr[['sapf']]), '_daylight')))
  expect_true(all(stringr::str_detect(names(test_expr[['env']]), '_daylight')))

  # test sfn_data_multi
  expect_true(is.list(test_expr2))
  expect_identical(names(test_expr2), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
  expect_s3_class(test_expr2[['ARG_MAZ']][['sapf']], 'tbl')
  expect_s3_class(test_expr2[['ARG_MAZ']][['env']], 'tbl')
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['sapf']]), '_daylight'))
  )
  expect_true(
    all(stringr::str_detect(names(test_expr2[['ARG_MAZ']][['env']]), '_daylight'))
  )

  # sfn_data and sfn_data_multi returns the same results for the same sites
  expect_equal(test_expr[['sapf']], test_expr2[['ARG_TRE']][['sapf']])
  expect_equal(test_expr[['env']], test_expr2[['ARG_TRE']][['env']])

  # daylight works as expected
  sapf_day_timestamp <- test_expr[['sapf']][['TIMESTAMP_daylight']]
  env_day_timestamp <- test_expr[['env']][['TIMESTAMP_daylight']]

  good_sapf_day_first <- "2009-11-18"
  good_sapf_day_second <- "2009-11-19"
  good_sapf_day_last <- "2009-11-30"

  good_env_day_first <- "2009-11-18"
  good_env_day_second <- "2009-11-19"
  good_env_day_last <- "2009-11-30"

  expect_equal(as.character(sapf_day_timestamp[1]), good_sapf_day_first)
  expect_equal(as.character(sapf_day_timestamp[2]), good_sapf_day_second)
  expect_equal(as.character(sapf_day_timestamp[13]), good_sapf_day_last)
  expect_equal(as.character(env_day_timestamp[1]), good_env_day_first)
  expect_equal(as.character(env_day_timestamp[2]), good_env_day_second)
  expect_equal(as.character(env_day_timestamp[13]), good_env_day_last)

})

#### daily_metrics ####
test_that('daily metrics examples work', {

  expect_s3_class(
    daily_metrics(ARG_TRE, solar = FALSE, metadata = sfn_metadata_ex), 'tbl'
  )
  expect_true(is.list(
    daily_metrics(ARG_TRE, solar = FALSE, tidy = FALSE)
  ))
  expect_s3_class(
    daily_metrics(ARG_TRE, solar = FALSE, tidy = FALSE)[['env']][['env_gen']],
    'tbl_time'
  )

  # expect_is(daily_metrics(
  #   ARG_TRE, solar = FALSE,
  #   pd_start = 5, pd_end = 7,
  #   md_start = 13, md_end = 15
  # ), 'list')
  # expect_s3_class(daily_metrics(
  #   ARG_TRE, solar = FALSE,
  #   pd_start = 5, pd_end = 7,
  #   md_start = 13, md_end = 15
  # )[['sapf']][['sapf_pd']], 'tbl_time')

})

test_that('daily metrics returns the variables required', {

  ARG_TRE_daily <- daily_metrics(ARG_TRE, solar = FALSE, tidy = FALSE)
  ARG_TRE_sapf_gen <- ARG_TRE_daily[['sapf']][['sapf_gen']]
  # ARG_TRE_sapf_pd <- ARG_TRE_daily[['sapf']][['sapf_pd']]
  # ARG_TRE_sapf_md <- ARG_TRE_daily[['sapf']][['sapf_md']]
  ARG_TRE_env_gen <- ARG_TRE_daily[['env']][['env_gen']]
  # ARG_TRE_env_pd <- ARG_TRE_daily[['env']][['env_pd']]
  # ARG_TRE_env_md <- ARG_TRE_daily[['env']][['env_md']]

  ARG_TRE_daily_tidy <- daily_metrics(ARG_TRE, solar = FALSE)

  expect_match(names(ARG_TRE_sapf_gen), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_min', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_max', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_centroid', all = FALSE)

  # expect_match(names(ARG_TRE_sapf_pd), '_q_95', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_q_99', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_coverage', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_min_time', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_max_time', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_min', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_max', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_pd), '_centroid', all = FALSE)
  #
  # expect_match(names(ARG_TRE_sapf_md), '_q_95', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_q_99', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_coverage', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_min_time', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_max_time', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_min', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_max', all = FALSE)
  # expect_match(names(ARG_TRE_sapf_md), '_centroid', all = FALSE)

  expect_match(names(ARG_TRE_env_gen), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_min', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_env_gen), '_centroid', all = FALSE))

  # expect_match(names(ARG_TRE_env_pd), '_q_95', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_q_99', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_coverage', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_min_time', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_max_time', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_min', all = FALSE)
  # expect_match(names(ARG_TRE_env_pd), '_max', all = FALSE)
  # expect_failure(expect_match(names(ARG_TRE_env_pd), '_centroid', all = FALSE))
  #
  # expect_match(names(ARG_TRE_env_md), '_q_95', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_q_99', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_coverage', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_min_time', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_max_time', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_min', all = FALSE)
  # expect_match(names(ARG_TRE_env_md), '_max', all = FALSE)
  # expect_failure(expect_match(names(ARG_TRE_env_md), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_daily_tidy), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_min', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_max', all = FALSE)
  expect_match(names(ARG_TRE_daily_tidy), '_centroid', all = FALSE)

})

test_that('monthly metrics returns the variables required', {

  ARG_TRE_monthly <- monthly_metrics(ARG_TRE, solar = FALSE)
  ARG_TRE_sapf_gen <- ARG_TRE_monthly[['sapf']][['sapf_gen']]
  ARG_TRE_sapf_pd <- ARG_TRE_monthly[['sapf']][['sapf_pd']]
  ARG_TRE_sapf_md <- ARG_TRE_monthly[['sapf']][['sapf_md']]
  ARG_TRE_env_gen <- ARG_TRE_monthly[['env']][['env_gen']]
  ARG_TRE_env_pd <- ARG_TRE_monthly[['env']][['env_pd']]
  ARG_TRE_env_md <- ARG_TRE_monthly[['env']][['env_md']]

  expect_match(names(ARG_TRE_sapf_gen), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_min', all = FALSE)
  expect_match(names(ARG_TRE_sapf_gen), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_sapf_gen), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_sapf_pd), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_min', all = FALSE)
  expect_match(names(ARG_TRE_sapf_pd), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_sapf_pd), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_sapf_md), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_min', all = FALSE)
  expect_match(names(ARG_TRE_sapf_md), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_sapf_md), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_env_gen), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_min', all = FALSE)
  expect_match(names(ARG_TRE_env_gen), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_env_gen), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_env_pd), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_min', all = FALSE)
  expect_match(names(ARG_TRE_env_pd), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_env_pd), '_centroid', all = FALSE))

  expect_match(names(ARG_TRE_env_md), '_q_95', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_q_99', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_coverage', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_min_time', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_max_time', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_min', all = FALSE)
  expect_match(names(ARG_TRE_env_md), '_max', all = FALSE)
  expect_failure(expect_match(names(ARG_TRE_env_md), '_centroid', all = FALSE))
})

test_that('monthly metrics examples work', {

  # we only test the main example, as the previous tests already cover almost
  # all functionality
  expect_is(monthly_metrics(ARG_TRE, solar = FALSE), 'list')
  expect_s3_class(monthly_metrics(ARG_TRE, solar = TRUE)[['sapf']][['sapf_gen']], 'tbl_df')

})

test_that('nighttime metrics work', {

  data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')

  expect_is(nightly_metrics(AUS_CAN_ST2_MIX, 'monthly'), 'list')
  expect_length(
    nightly_metrics(AUS_CAN_ST2_MIX, 'monthly')[['env']][['env_day']][['TIMESTAMP_day']], 13
  )
})

test_that('*_metrics functions with ... work', {

  expect_true(is.list(daily_metrics(ARG_TRE, clean = FALSE)))
  expect_true(is.list(monthly_metrics(ARG_TRE, clean = FALSE)))
  expect_true(is.list(nightly_metrics(ARG_TRE, clean = FALSE)))
  expect_true(is.list(daily_metrics(ARG_TRE, side = 'end')))
  expect_true(is.list(monthly_metrics(ARG_TRE, side = 'end')))
  expect_true(is.list(nightly_metrics(ARG_TRE, side = 'end')))
  # expect_error(nightly_metrics(ARG_TRE, clean = TRUE))

})

test_that('.fixed_metrics_funs works', {

  .funs <- .fixed_metrics_funs(probs = c(0.95, 0.99), TRUE)

  expect_s3_class(.funs, 'fun_list')
  expect_identical(
    names(.funs),
    c('mean', 'sd', 'n', 'coverage', 'q_95', 'q_99', 'max',
      'max_time', 'min', 'min_time', 'centroid')
  )

  .funs_no_centroid <- .fixed_metrics_funs(probs = c(0.1, 0.01), FALSE)

  expect_s3_class(.funs_no_centroid, 'fun_list')
  expect_identical(
    names(.funs_no_centroid),
    c('mean', 'sd', 'n', 'coverage', 'q_10', 'q_1', 'max',
      'max_time', 'min', 'min_time')
  )

})
