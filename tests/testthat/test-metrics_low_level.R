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


# summarise_by_period tests
test_that('summarise_by_period function example works', {
  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = '7 days',
      .funs = dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), n())
    ),
    'tbl_time'
  )
})

test_that('dots arguments work as intended', {

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE,
      side = "start"
    ),
    'tbl_time'
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      side = "start"
    ),
    'tbl_time'
  )

  expect_s3_class(
    summarise_by_period(
      data = get_sapf(FOO),
      period = 'daily',
      .funs = dplyr::funs(mean, sd),
      na.rm = TRUE
    ),
    'tbl_time'
  )




})
