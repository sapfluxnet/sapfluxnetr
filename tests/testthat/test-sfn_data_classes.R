#### sfn_data_classes tests ####
context("sfn_data_classes")

load('FOO.RData')
load('BAR.RData')
load('BAZ.RData')

FOO <- sfn_data(
  sapf_data = FOO_sapf_data[,-1],
  env_data = FOO_env_data[,-1],
  sapf_flags = FOO_sapf_flags[,-1],
  env_flags = FOO_env_flags[, -1],
  si_code = FOO_si_code,
  timestamp = FOO_timestamp,
  solar_timestamp = FOO_solar_timestamp,
  site_md = FOO_site_md,
  stand_md = FOO_stand_md,
  species_md = FOO_species_md,
  plant_md = FOO_plant_md,
  env_md = FOO_env_md
)

BAR <- sfn_data(
  sapf_data = BAR_sapf_data[,-1],
  env_data = BAR_env_data[,-1],
  sapf_flags = BAR_sapf_flags[,-1],
  env_flags = BAR_env_flags[, -1],
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
  sapf_data = BAZ_sapf_data[,-1],
  env_data = BAZ_env_data[,-1],
  sapf_flags = BAZ_sapf_flags[,-1],
  env_flags = BAZ_env_flags[, -1],
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

# tests
test_that('constructors_work', {

  expect_s4_class(FOO, 'sfn_data')
  expect_s4_class(BAR, 'sfn_data')
  expect_s4_class(BAZ, 'sfn_data')
  expect_s4_class(multi_sfn, 'sfn_data_multi')

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data_bad[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data_bad[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags_bad[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags_bad[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data_bad[,-1],
      env_data = FOO_env_data_bad[,-1],
      sapf_flags = FOO_sapf_flags_bad[,-1],
      env_flags = FOO_env_flags_bad[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp_bad,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = 'FOO_stand_md',
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = '',
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = FOO_si_code,
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = data.frame(),
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data(
      sapf_data = FOO_sapf_data[,-1],
      env_data = FOO_env_data[,-1],
      sapf_flags = FOO_sapf_flags[,-1],
      env_flags = FOO_env_flags[, -1],
      si_code = rep(FOO_si_code, length(FOO_timestamp)),
      timestamp = FOO_timestamp,
      solar_timestamp = FOO_solar_timestamp,
      site_md = FOO_site_md,
      stand_md = FOO_stand_md,
      species_md = FOO_species_md,
      plant_md = FOO_plant_md,
      env_md = FOO_env_md
    )
  )

  expect_error(
    sfn_data_multi(FOO, BAR, BAZ_sapf_data)
  )

  expect_error(
    sfn_data_multi(FOO, 'BAR', BAZ)
  )

  expect_identical(names(multi_sfn), c('FOO', 'BAR', 'BAZ'))
})

test_that('get methods returns the correct type of object', {
  expect_true(tibble::is_tibble(get_sapf(FOO)))
  expect_true(tibble::is_tibble(get_sapf(FOO, solar = TRUE)))
  expect_true('TIMESTAMP' %in% names(get_sapf(FOO)))
  expect_true('TIMESTAMP' %in% names(get_sapf(FOO, solar = TRUE)))
  expect_true(tibble::is_tibble(get_sapf_flags(FOO)))
  expect_true(tibble::is_tibble(get_sapf_flags(FOO, solar = TRUE)))
  expect_true('TIMESTAMP' %in% names(get_sapf_flags(FOO)))
  expect_true('TIMESTAMP' %in% names(get_sapf_flags(FOO, solar = TRUE)))
  expect_true(tibble::is_tibble(get_env(FOO)))
  expect_true(tibble::is_tibble(get_env(FOO, solar = TRUE)))
  expect_true('TIMESTAMP' %in% names(get_env(FOO)))
  expect_true('TIMESTAMP' %in% names(get_env(FOO, solar = TRUE)))
  expect_true(tibble::is_tibble(get_env_flags(FOO)))
  expect_true(tibble::is_tibble(get_env_flags(FOO, solar = TRUE)))
  expect_true('TIMESTAMP' %in% names(get_env_flags(FOO)))
  expect_true('TIMESTAMP' %in% names(get_env_flags(FOO, solar = TRUE)))
  expect_true(tibble::is_tibble(get_site_md(FOO)))
  expect_true(tibble::is_tibble(get_stand_md(FOO)))
  expect_true(tibble::is_tibble(get_species_md(FOO)))
  expect_true(tibble::is_tibble(get_plant_md(FOO)))
  expect_true(tibble::is_tibble(get_env_md(FOO)))
  expect_is(get_timestamp(FOO), 'POSIXct')
  expect_is(get_solar_timestamp(FOO), 'POSIXct')
  expect_true(is.character(get_si_code(FOO)))
})
