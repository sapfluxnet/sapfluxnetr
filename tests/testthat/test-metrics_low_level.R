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

  expect_s3_class(
    sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
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
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )),
    c('sapf', 'env')
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_mean'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_sd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_n'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_mean_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_sd_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_n_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_mean_md'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_sd_md'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['sapf']]),
    '_n_md'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_mean'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_sd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_n'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_mean_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_sd_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_n_pd'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_mean_md'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_sd_md'
  )

  expect_match(
    names(sfn_metrics(
      FOO,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = FALSE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    )[['env']]),
    '_n_md'
  )

  expect_is(
    sfn_metrics(
      multi_sfn,
      period = 'daily',
      .funs = funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n()),
      solar = TRUE,
      predawn = TRUE,
      pd_start = 13,
      pd_end = 15,
      midday = TRUE,
      md_start = 4,
      md_end = 6,
      side = 'start'
    ),
    'list'
  )
})
