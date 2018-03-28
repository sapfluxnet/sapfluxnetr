context("metrics_low_level")

# data
FOO <- read_sfn_data('FOO', 'Data')

#### summarise_by_period tests ####
test_that('summarise_by_period function example works', {

  library(dplyr)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )
  
  expect_s3_class(
    summarise_by_period(
      data = get_env_data(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )
  
  test_expr <- summarise_by_period(
    data = get_sapf_data(FOO),
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
      data = get_sapf_data(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )
  
  test_expr <- summarise_by_period(
    data = get_sapf_data(FOO),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    na.rm = TRUE, # for summarise
    side = "start" # for collapse_index
  )

  expect_match(names(test_expr), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE, # for summarise
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )
  
  test_expr2 <- summarise_by_period(
    data = get_sapf_data(FOO),
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
      data = get_sapf_data(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start" # for collapse_index
    ),
    'tbl_time'
  )
  
  test_expr3 <- summarise_by_period(
    data = get_sapf_data(FOO),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    side = "start" # for collapse_index
  )

  expect_match(names(test_expr3), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr3), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start", # for collapse_index
      clean = TRUE # for collapse_index
    ),
    'tbl_time'
  )
  
  test_expr4 <- summarise_by_period(
    data = get_sapf_data(FOO),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    side = "start", # for collapse_index
    clean = TRUE # for collapse_index
  )

  expect_match(names(test_expr4), regexp = '_mean', all = FALSE)
  expect_match(names(test_expr4), regexp = '_sd', all = FALSE)

  expect_s3_class(
    summarise_by_period(
      data = get_sapf_data(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE # for summarise
    ),
    'tbl_time'
  )
  
  test_expr5 <- summarise_by_period(
    data = get_sapf_data(FOO),
    period = 'daily',
    .funs = dplyr::funs(mean, sd),
    na.rm = TRUE # for summarise
  )

  expect_match(names(test_expr5), regexp = '_mean', all = FALSE)

  expect_match(names(test_expr5), regexp = '_sd', all = FALSE)

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
test_that('sfn_metrics examples work', {

  library(dplyr)

  BAR <- read_sfn_data('BAR', 'Data')
  BAZ <- read_sfn_data('BAZ', 'Data')
  multi_sfn <- sfn_data_multi(FOO, BAR, BAZ)

  expect_is(
    sfn_metrics(
      FOO,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE, general = TRUE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      nighttime = FALSE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    ),
    'list'
  )
  
  test_expr <- sfn_metrics(
    FOO,
    period = '7 days',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE, general = TRUE,
    predawn = TRUE,
    pd_start = 4,
    pd_end = 6,
    midday = TRUE,
    md_start = 12,
    md_end = 14,
    nighttime = FALSE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )

  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_identical(
    names(test_expr[['sapf']]),
    c('sapf_gen', 'sapf_pd', 'sapf_md')
  )
  expect_identical(names(test_expr[['env']]), c('env_gen', 'env_pd', 'env_md'))

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE, general = TRUE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      nighttime = FALSE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    ),
    'list'
  )
  
  test_expr2 <- sfn_metrics(
    multi_sfn,
    period = '7 days',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE, general = TRUE,
    predawn = TRUE,
    pd_start = 4,
    pd_end = 6,
    midday = TRUE,
    md_start = 12,
    md_end = 14,
    nighttime = FALSE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )

  expect_identical(names(test_expr2), c('FOO', 'BAR', 'BAZ'))

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE, general = TRUE,
      predawn = TRUE,
      md_start = 12,
      md_end = 14,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      nighttime = FALSE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    )[['FOO']],
    'list'
  )
  
  test_expr3 <- sfn_metrics(
    multi_sfn,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE, general = TRUE,
    predawn = TRUE,
    md_start = 12,
    md_end = 14,
    midday = TRUE,
    pd_start = 4,
    pd_end = 6,
    nighttime = FALSE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )

  expect_is(test_expr3[['BAR']], 'list')
  expect_is(test_expr3[['BAZ']], 'list')

  expect_equal(
    test_expr[['sapf']][['sapf_pd']],
    test_expr2[['FOO']][['sapf']][['sapf_pd']]
  )

})

test_that('sfn_metrics return the expected object', {

  library(dplyr)

  load('BAR.RData')
  load('BAZ.RData')

  BAR <- sfn_data(
    sapf_data = BAR_sapf_data,
    env_data = BAR_env_data,
    sapf_flags = BAR_sapf_flags,
    env_flags = BAR_env_flags,
    si_code = BAR_si_code,
    timestamp = BAR_timestamp,
    solar_timestamp = BAR_solar_timestamp,
    site_md = BAR_site_md,
    stand_md = BAR_stand_md,
    species_md = BAR_species_md,
    plant_md = BAR_plant_md,
    env_md = BAR_env_md
  )

  BAZ <- sfn_data(
    sapf_data = BAZ_sapf_data,
    env_data = BAZ_env_data,
    sapf_flags = BAZ_sapf_flags,
    env_flags = BAZ_env_flags,
    si_code = BAZ_si_code,
    timestamp = BAZ_timestamp,
    solar_timestamp = BAZ_solar_timestamp,
    site_md = BAZ_site_md,
    stand_md = BAZ_stand_md,
    species_md = BAZ_species_md,
    plant_md = BAZ_plant_md,
    env_md = BAZ_env_md
  )

  multi_sfn <- sfn_data_multi(FOO, BAR, BAZ)

  expect_is(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      general = TRUE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      nighttime = TRUE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    ),
    'list'
  )
  
  test_expr <- sfn_metrics(
    FOO,
    period = 'daily',
    .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE,
    general = TRUE,
    predawn = TRUE,
    md_start = 13,
    md_end = 15,
    midday = TRUE,
    pd_start = 4,
    pd_end = 6,
    nighttime = TRUE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )

  expect_identical(names(test_expr), c('sapf', 'env'))
  expect_identical(
    names(test_expr[['sapf']]),
    c('sapf_gen', 'sapf_pd', 'sapf_md', 'sapf_day', 'sapf_night')
  )
  expect_s3_class(test_expr[['sapf']][['sapf_gen']], 'tbl_time')
  expect_s3_class(test_expr[['sapf']][['sapf_pd']], 'tbl_time')
  expect_s3_class(test_expr[['sapf']][['sapf_md']], 'tbl_time')
  expect_s3_class(test_expr[['sapf']][['sapf_day']], 'tbl_time')
  expect_s3_class(test_expr[['sapf']][['sapf_night']], 'tbl_time')
  expect_match(names(test_expr[['sapf']][['sapf_gen']]), '_mean', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_gen']]), '_sd', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_gen']]), '_n', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_pd']]), '_mean_pd', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_pd']]), '_sd_pd', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_pd']]), '_n_pd', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_md']]), '_mean_md', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_md']]), '_sd_md', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_md']]), '_n_md', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_day']]), '_mean_day', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_day']]), '_sd_day', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_day']]), '_n_day', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_night']]), '_mean_night', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_night']]), '_sd_night', all = FALSE)
  expect_match(names(test_expr[['sapf']][['sapf_night']]), '_n_night', all = FALSE)
  expect_identical(
    names(test_expr[['env']]),
    c('env_gen', 'env_pd', 'env_md', 'env_day', 'env_night')
  )
  expect_s3_class(test_expr[['env']][['env_gen']], 'tbl_time')
  expect_s3_class(test_expr[['env']][['env_pd']], 'tbl_time')
  expect_s3_class(test_expr[['env']][['env_md']], 'tbl_time')
  expect_s3_class(test_expr[['env']][['env_day']], 'tbl_time')
  expect_s3_class(test_expr[['env']][['env_night']], 'tbl_time')
  expect_match(names(test_expr[['env']][['env_gen']]), '_mean', all = FALSE)
  expect_match(names(test_expr[['env']][['env_gen']]), '_sd', all = FALSE)
  expect_match(names(test_expr[['env']][['env_gen']]), '_n', all = FALSE)
  expect_match(names(test_expr[['env']][['env_pd']]), '_mean_pd', all = FALSE)
  expect_match(names(test_expr[['env']][['env_pd']]), '_sd_pd', all = FALSE)
  expect_match(names(test_expr[['env']][['env_pd']]), '_n_pd', all = FALSE)
  expect_match(names(test_expr[['env']][['env_md']]), '_mean_md', all = FALSE)
  expect_match(names(test_expr[['env']][['env_md']]), '_sd_md', all = FALSE)
  expect_match(names(test_expr[['env']][['env_md']]), '_n_md', all = FALSE)
  expect_match(names(test_expr[['env']][['env_day']]), '_mean_day', all = FALSE)
  expect_match(names(test_expr[['env']][['env_day']]), '_sd_day', all = FALSE)
  expect_match(names(test_expr[['env']][['env_day']]), '_n_day', all = FALSE)
  expect_match(names(test_expr[['env']][['env_night']]), '_mean_night', all = FALSE)
  expect_match(names(test_expr[['env']][['env_night']]), '_sd_night', all = FALSE)
  expect_match(names(test_expr[['env']][['env_night']]), '_n_night', all = FALSE)

  # tests for check the structure when some interval is not selected (predawn or
  # midday or nighttime)
  expect_is(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      general = TRUE,
      predawn = FALSE,
      midday = FALSE,
      nighttime = FALSE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    ),
    'list'
  )
  
  test_expr2 <- sfn_metrics(
    FOO,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE,
    general = TRUE,
    predawn = FALSE,
    midday = FALSE,
    nighttime = FALSE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )

  expect_identical(names(test_expr2), c('sapf', 'env'))
  expect_identical(names(test_expr2[['sapf']]), c('sapf_gen'))
  expect_identical(names(test_expr2[['env']]), c('env_gen'))
  expect_s3_class(test_expr2[['sapf']][['sapf_gen']], 'tbl_time')
  expect_null(test_expr2[['sapf']][['sapf_pd']])
  expect_null(test_expr2[['sapf']][['sapf_md']])
  expect_null(test_expr2[['sapf']][['sapf_day']])
  expect_null(test_expr2[['sapf']][['sapf_night']])
  expect_s3_class(test_expr2[['env']][['env_gen']], 'tbl_time')
  expect_null(test_expr2[['env']][['env_pd']])
  expect_null(test_expr2[['env']][['env_md']])
  expect_null(test_expr2[['env']][['env_day']])
  expect_null(test_expr2[['env']][['env_night']])
  
  expect_is(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      general = FALSE,
      predawn = FALSE,
      midday = FALSE,
      nighttime = TRUE,
      night_start = 20,
      night_end = 6,
      side = 'start'
    ),
    'list'
  )
  
  test_expr3 <- sfn_metrics(
    FOO,
    period = 'daily',
    .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
    solar = FALSE,
    general = FALSE,
    predawn = FALSE,
    midday = FALSE,
    nighttime = TRUE,
    night_start = 20,
    night_end = 6,
    side = 'start'
  )
  
  expect_identical(names(test_expr3), c('sapf', 'env'))
  expect_identical(names(test_expr3[['sapf']]), c('sapf_day', 'sapf_night'))
  expect_identical(names(test_expr3[['env']]), c('env_day', 'env_night'))
  expect_s3_class(test_expr3[['sapf']][['sapf_day']], 'tbl_time')
  expect_s3_class(test_expr3[['sapf']][['sapf_night']], 'tbl_time')
  expect_s3_class(test_expr3[['env']][['env_day']], 'tbl_time')
  expect_s3_class(test_expr3[['env']][['env_night']], 'tbl_time')
  
  sapf_day_timestamp <- test_expr3[['sapf']][['sapf_day']][['TIMESTAMP_day']]
  sapf_night_timestamp <- test_expr3[['sapf']][['sapf_night']][['TIMESTAMP_night']]
  env_day_timestamp <- test_expr3[['env']][['env_day']][['TIMESTAMP_day']]
  env_night_timestamp <- test_expr3[['env']][['env_night']][['TIMESTAMP_night']]
  
  expect_true(
    all(dplyr::between(lubridate::hour(c(sapf_day_timestamp, env_day_timestamp)),
                       6, 20))
  )
  
  expect_true(
    all(
      dplyr::between(lubridate::hour(c(sapf_night_timestamp, env_night_timestamp)),
                     20, 24)) | 
      all(dplyr::between(lubridate::hour(c(sapf_night_timestamp, env_night_timestamp)),
                         0, 6))
  )
  

})

test_that('daily metrics examples work', {

  expect_is(daily_metrics(FOO, solar = FALSE), 'list')
  expect_s3_class(daily_metrics(FOO, solar = FALSE)[['env']][['env_gen']], 'tbl_time')

  expect_is(daily_metrics(
    FOO, solar = FALSE,
    pd_start = 5, pd_end = 7,
    md_start = 13, md_end = 15
  ), 'list')
  expect_s3_class(daily_metrics(
    FOO, solar = FALSE,
    pd_start = 5, pd_end = 7,
    md_start = 13, md_end = 15
  )[['sapf']][['sapf_pd']], 'tbl_time')

})

test_that('daily metrics returns the variables required', {

  FOO_daily <- daily_metrics(FOO, solar = FALSE)
  FOO_sapf_gen <- FOO_daily[['sapf']][['sapf_gen']]
  FOO_sapf_pd <- FOO_daily[['sapf']][['sapf_pd']]
  FOO_sapf_md <- FOO_daily[['sapf']][['sapf_md']]
  FOO_env_gen <- FOO_daily[['env']][['env_gen']]
  FOO_env_pd <- FOO_daily[['env']][['env_pd']]
  FOO_env_md <- FOO_daily[['env']][['env_md']]

  expect_match(names(FOO_sapf_gen), '_q_95', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_q_99', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_coverage', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_min_time', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_max_time', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_min', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_max', all = FALSE)
  expect_match(names(FOO_sapf_gen), '_centroid', all = FALSE)
  
  expect_match(names(FOO_sapf_pd), '_q_95', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_q_99', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_coverage', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_min_time', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_max_time', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_min', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_max', all = FALSE)
  expect_match(names(FOO_sapf_pd), '_centroid', all = FALSE)
  
  expect_match(names(FOO_sapf_md), '_q_95', all = FALSE)
  expect_match(names(FOO_sapf_md), '_q_99', all = FALSE)
  expect_match(names(FOO_sapf_md), '_coverage', all = FALSE)
  expect_match(names(FOO_sapf_md), '_min_time', all = FALSE)
  expect_match(names(FOO_sapf_md), '_max_time', all = FALSE)
  expect_match(names(FOO_sapf_md), '_min', all = FALSE)
  expect_match(names(FOO_sapf_md), '_max', all = FALSE)
  expect_match(names(FOO_sapf_md), '_centroid', all = FALSE)
  
  expect_match(names(FOO_env_gen), '_q_95', all = FALSE)
  expect_match(names(FOO_env_gen), '_q_99', all = FALSE)
  expect_match(names(FOO_env_gen), '_coverage', all = FALSE)
  expect_match(names(FOO_env_gen), '_min_time', all = FALSE)
  expect_match(names(FOO_env_gen), '_max_time', all = FALSE)
  expect_match(names(FOO_env_gen), '_min', all = FALSE)
  expect_match(names(FOO_env_gen), '_max', all = FALSE)
  expect_match(names(FOO_env_gen), '_centroid', all = FALSE)
  
  expect_match(names(FOO_env_pd), '_q_95', all = FALSE)
  expect_match(names(FOO_env_pd), '_q_99', all = FALSE)
  expect_match(names(FOO_env_pd), '_coverage', all = FALSE)
  expect_match(names(FOO_env_pd), '_min_time', all = FALSE)
  expect_match(names(FOO_env_pd), '_max_time', all = FALSE)
  expect_match(names(FOO_env_pd), '_min', all = FALSE)
  expect_match(names(FOO_env_pd), '_max', all = FALSE)
  expect_match(names(FOO_env_pd), '_centroid', all = FALSE)
  
  expect_match(names(FOO_env_md), '_q_95', all = FALSE)
  expect_match(names(FOO_env_md), '_q_99', all = FALSE)
  expect_match(names(FOO_env_md), '_coverage', all = FALSE)
  expect_match(names(FOO_env_md), '_min_time', all = FALSE)
  expect_match(names(FOO_env_md), '_max_time', all = FALSE)
  expect_match(names(FOO_env_md), '_min', all = FALSE)
  expect_match(names(FOO_env_md), '_max', all = FALSE)
  expect_match(names(FOO_env_md), '_centroid', all = FALSE)
})

test_that('monthly metrics examples work', {

  # we only test the main example, as the previous tests already cover almost
  # all functionality
  expect_is(monthly_metrics(FOO, solar = FALSE), 'list')
  expect_s3_class(monthly_metrics(FOO, solar = TRUE)[['sapf']][['sapf_gen']], 'tbl_df')

})

test_that('nighttime metrics work', {
  
  data('BAZ', package = 'sapfluxnetr')
  
  expect_is(nighttime_metrics(BAZ, 'monthly'), 'list')
  expect_length(
    nighttime_metrics(BAZ, 'monthly')[['env']][['env_day']][['TIMESTAMP_day']], 13
  )
})
