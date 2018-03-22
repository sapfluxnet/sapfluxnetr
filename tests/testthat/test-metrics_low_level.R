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

#### sfn_metrics tests ####
test_that('sfn_metrics examples work', {

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
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    ),
    'list'
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )),
    c('sapf', 'env')
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )[['sapf']]),
    c('sapf', 'sapf_pd', 'sapf_md')
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )[['env']]),
    c('env', 'env_pd', 'env_md')
  )

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    ),
    'list'
  )

  expect_identical(
    names(sfn_metrics(
      multi_sfn,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )),
    c('FOO', 'BAR', 'BAZ')
  )

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 12,
      md_end = 14,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['FOO']],
    'list'
  )

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 12,
      md_end = 14,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['BAR']],
    'list'
  )

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 12,
      md_end = 14,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['BAZ']],
    'list'
  )

  expect_equal(
    sfn_metrics(
      FOO,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )[['sapf']][['sapf_pd']],
    sfn_metrics(
      multi_sfn,
      period = '7 days',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 4,
      pd_end = 6,
      midday = TRUE,
      md_start = 12,
      md_end = 14,
      side = 'start'
    )[['FOO']][['sapf']][['sapf_pd']]
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
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    ),
    'list'
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )),
    c('sapf', 'env')
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']]),
    c('sapf', 'sapf_pd', 'sapf_md')
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf']],
    'tbl_time'
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_pd']],
    'tbl_time'
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_md']],
    'tbl_time'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf']]),
    '_mean', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf']]),
    '_sd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf']]),
    '_n', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_pd']]),
    '_mean_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_pd']]),
    '_sd_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_pd']]),
    '_n_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_md']]),
    '_mean_md', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_md']]),
    '_sd_md', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['sapf']][['sapf_md']]),
    '_n_md', all = FALSE
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']]),
    c('env', 'env_pd', 'env_md')
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env']],
    'tbl_time'
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_pd']],
    'tbl_time'
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_md']],
    'tbl_time'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env']]),
    '_mean', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env']]),
    '_sd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env']]),
    '_n', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_pd']]),
    '_mean_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_pd']]),
    '_sd_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_pd']]),
    '_n_pd', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_md']]),
    '_mean_md', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_md']]),
    '_sd_md', all = FALSE
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      md_start = 13,
      md_end = 15,
      midday = TRUE,
      pd_start = 4,
      pd_end = 6,
      side = 'start'
    )[['env']][['env_md']]),
    '_n_md', all = FALSE
  )

  # tests for check the structure when some interval is not selected (predawn or
  # midday)
  expect_is(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    ),
    'list'
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )),
    c('sapf', 'env')
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['sapf']]),
    c('sapf', 'sapf_pd', 'sapf_md')
  )

  expect_identical(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['env']]),
    c('env', 'env_pd', 'env_md')
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['sapf']][['sapf']],
    'tbl_time'
  )

  expect_null(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['sapf']][['sapf_pd']]
  )

  expect_null(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['sapf']][['sapf_md']]
  )

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['env']][['env']],
    'tbl_time'
  )

  expect_null(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['env']][['env_pd']]
  )

  expect_null(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = FALSE,
      midday = FALSE,
      side = 'start'
    )[['env']][['env_md']]
  )

})

test_that('daily metrics examples work', {

  expect_is(daily_metrics(FOO, solar = FALSE), 'list')
  expect_s3_class(daily_metrics(FOO, solar = FALSE)[['env']][['env']], 'tbl_time')

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

  expect_is(daily_metrics(FOO, solar = FALSE, predawn = FALSE, midday = FALSE), 'list')
  expect_null(daily_metrics(FOO, solar = FALSE, predawn = FALSE, midday = FALSE)[['env']][['env_md']])
  expect_s3_class(daily_metrics(
    FOO, solar = FALSE, predawn = FALSE, midday = FALSE
  )[['env']][['env']], 'tbl_time')

})

test_that('daily metrics returns the variables required', {

  FOO_daily <- daily_metrics(FOO, solar = FALSE)

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf']]),
    '_max', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_pd']]),
    '_max', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['sapf']][['sapf_md']]),
    '_max', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env']]),
    '_max', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_pd']]),
    '_max', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_q_95', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_q_99', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_coverage', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_min_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_max_time', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_min', all = FALSE
  )

  expect_match(
    names(FOO_daily[['env']][['env_md']]),
    '_max', all = FALSE
  )

})

