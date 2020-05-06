#### sfn_data_classes tests ####
load('ARG_TRE.RData')
load('ARG_MAZ.RData')
load('AUS_CAN_ST2_MIX.RData')

ARG_TRE <- sfn_data(
  sapf_data = ARG_TRE_sapf_data,
  env_data = ARG_TRE_env_data,
  sapf_flags = ARG_TRE_sapf_flags,
  env_flags = ARG_TRE_env_flags,
  si_code = ARG_TRE_si_code,
  timestamp = ARG_TRE_timestamp,
  solar_timestamp = ARG_TRE_solar_timestamp,
  site_md = ARG_TRE_site_md,
  stand_md = ARG_TRE_stand_md,
  species_md = ARG_TRE_species_md,
  plant_md = ARG_TRE_plant_md,
  env_md = ARG_TRE_env_md
)

ARG_MAZ <- sfn_data(
  sapf_data = ARG_MAZ_sapf_data,
  env_data = ARG_MAZ_env_data,
  sapf_flags = ARG_MAZ_sapf_flags,
  env_flags = ARG_MAZ_env_flags,
  si_code = ARG_MAZ_si_code,
  timestamp = ARG_MAZ_timestamp,
  solar_timestamp = ARG_MAZ_solar_timestamp,
  site_md = ARG_MAZ_site_md,
  stand_md = ARG_MAZ_stand_md,
  species_md = ARG_MAZ_species_md,
  plant_md = ARG_MAZ_plant_md,
  env_md = ARG_MAZ_env_md
)

AUS_CAN_ST2_MIX <- sfn_data(
  sapf_data = AUS_CAN_ST2_MIX_sapf_data,
  env_data = AUS_CAN_ST2_MIX_env_data,
  sapf_flags = AUS_CAN_ST2_MIX_sapf_flags,
  env_flags = AUS_CAN_ST2_MIX_env_flags,
  si_code = AUS_CAN_ST2_MIX_si_code,
  timestamp = AUS_CAN_ST2_MIX_timestamp,
  solar_timestamp = AUS_CAN_ST2_MIX_solar_timestamp,
  site_md = AUS_CAN_ST2_MIX_site_md,
  stand_md = AUS_CAN_ST2_MIX_stand_md,
  species_md = AUS_CAN_ST2_MIX_species_md,
  plant_md = AUS_CAN_ST2_MIX_plant_md,
  env_md = AUS_CAN_ST2_MIX_env_md
)

multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)

# tests
test_that('constructors_work', {

  # these tests fails on R CMD CHECK I assume because a wrong assumption by me
  # about where (environment) the sfn_data build is performed, throwing an error
  # in the R CMD CHECK but not in the tests. So, we avoid them in the CRAN tests
  # but allow them in the normal tests
  testthat::skip_on_cran()

  expect_s4_class(ARG_TRE, 'sfn_data')
  expect_s4_class(ARG_MAZ, 'sfn_data')
  expect_s4_class(AUS_CAN_ST2_MIX, 'sfn_data')
  expect_s4_class(multi_sfn, 'sfn_data_multi')

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data_bad,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data_bad,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags_bad,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags_bad,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data_bad,
      env_data = ARG_TRE_env_data_bad,
      sapf_flags = ARG_TRE_sapf_flags_bad,
      env_flags = ARG_TRE_env_flags_bad,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp_bad,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = 'ARG_TRE_stand_md',
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'stand_md must be a tibble or an object coercible to one'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = '',
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = ARG_TRE_si_code,
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = data.frame(),
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data(
      sapf_data = ARG_TRE_sapf_data,
      env_data = ARG_TRE_env_data,
      sapf_flags = ARG_TRE_sapf_flags,
      env_flags = ARG_TRE_env_flags,
      si_code = rep(ARG_TRE_si_code, length(ARG_TRE_timestamp)),
      timestamp = ARG_TRE_timestamp,
      solar_timestamp = ARG_TRE_solar_timestamp,
      site_md = ARG_TRE_site_md,
      stand_md = ARG_TRE_stand_md,
      species_md = ARG_TRE_species_md,
      plant_md = ARG_TRE_plant_md,
      env_md = ARG_TRE_env_md
    ),
    'invalid class'
  )

  expect_error(
    sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX_sapf_data),
    'All elements must be sfn_data objects'
  )

  expect_error(
    sfn_data_multi(ARG_TRE, 'ARG_MAZ', AUS_CAN_ST2_MIX),
    'All elements must be sfn_data objects'
  )

  expect_identical(names(multi_sfn), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'))
})

test_that('get methods returns the correct object', {

  foo_sapf_data <- get_sapf_data(ARG_TRE)
  foo_sapf_data_solar <- get_sapf_data(ARG_TRE, solar = TRUE)
  foo_sapf_flags <- get_sapf_flags(ARG_TRE)
  foo_sapf_flags_solar <- get_sapf_flags(ARG_TRE, solar = TRUE)

  foo_env_data <- get_env_data(ARG_TRE)
  foo_env_data_solar <- get_env_data(ARG_TRE, solar = TRUE)
  foo_env_flags <- get_env_flags(ARG_TRE)
  foo_env_flags_solar <- get_env_flags(ARG_TRE, solar = TRUE)

  foo_timestamp <- get_timestamp(ARG_TRE)
  foo_solar_timestamp <- get_solar_timestamp(ARG_TRE)

  foo_si_code <- get_si_code(ARG_TRE)

  foo_site_md <- get_site_md(ARG_TRE)
  foo_stand_md <- get_stand_md(ARG_TRE)
  foo_species_md <- get_species_md(ARG_TRE)
  foo_plant_md <- get_plant_md(ARG_TRE)
  foo_env_md <- get_env_md(ARG_TRE)
  
  expect_true(tibble::is_tibble(foo_sapf_data))
  expect_true(tibble::is_tibble(foo_sapf_data_solar))
  expect_true('TIMESTAMP' %in% names(foo_sapf_data))
  expect_true('TIMESTAMP' %in% names(foo_sapf_data_solar))
  expect_true(tibble::is_tibble(foo_sapf_flags))
  expect_true(tibble::is_tibble(foo_sapf_flags_solar))
  expect_true('TIMESTAMP' %in% names(foo_sapf_flags))
  expect_true('TIMESTAMP' %in% names(foo_sapf_flags_solar))
  expect_true(tibble::is_tibble(foo_env_data))
  expect_true(tibble::is_tibble(foo_env_data_solar))
  expect_true('TIMESTAMP' %in% names(foo_env_data))
  expect_true('TIMESTAMP' %in% names(foo_env_data_solar))
  expect_true(tibble::is_tibble(foo_env_flags))
  expect_true(tibble::is_tibble(foo_env_flags_solar))
  expect_true('TIMESTAMP' %in% names(foo_env_flags))
  expect_true('TIMESTAMP' %in% names(foo_env_flags_solar))
  expect_true(tibble::is_tibble(foo_site_md))
  expect_true(tibble::is_tibble(foo_stand_md))
  expect_true(tibble::is_tibble(foo_species_md))
  expect_true(tibble::is_tibble(foo_plant_md))
  expect_true(tibble::is_tibble(foo_env_md))
  expect_is(foo_timestamp, 'POSIXct')
  expect_identical(attr(foo_timestamp, 'tz'), 'Etc/GMT+3')
  expect_is(foo_solar_timestamp, 'POSIXct')
  expect_identical(attr(foo_solar_timestamp, 'tz'), 'UTC')
  expect_true(is.character(foo_si_code))

  expect_identical(ARG_TRE_sapf_data[[2]], foo_sapf_data[[3]])
  expect_identical(ARG_TRE_sapf_flags[[2]], foo_sapf_flags[[3]])
  expect_identical(ARG_TRE_env_data[[2]], foo_env_data[[3]])
  expect_identical(ARG_TRE_env_flags[[2]], foo_env_flags[[3]])
  expect_identical(ARG_TRE_sapf_data[[2]], foo_sapf_data_solar[[3]])
  expect_identical(ARG_TRE_sapf_flags[[2]], foo_sapf_flags_solar[[3]])
  expect_identical(ARG_TRE_env_data[[2]], foo_env_data_solar[[3]])
  expect_identical(ARG_TRE_env_flags[[2]], foo_env_flags_solar[[3]])
  expect_identical(ARG_TRE_timestamp, foo_timestamp)
  expect_identical(ARG_TRE_solar_timestamp, foo_solar_timestamp)
  expect_identical(ARG_TRE_si_code, foo_si_code)
  expect_identical(tibble::as_tibble(ARG_TRE_site_md), foo_site_md)
  expect_identical(tibble::as_tibble(ARG_TRE_stand_md), foo_stand_md)
  expect_identical(tibble::as_tibble(ARG_TRE_species_md), foo_species_md)
  expect_identical(tibble::as_tibble(ARG_TRE_plant_md), foo_plant_md)
  expect_identical(tibble::as_tibble(ARG_TRE_env_md), foo_env_md)
  
  multi_sapf_data <- get_sapf_data(multi_sfn)
  multi_sapf_data_solar <- get_sapf_data(multi_sfn, solar = TRUE)
  multi_sapf_flags <- get_sapf_flags(multi_sfn)
  multi_sapf_flags_solar <- get_sapf_flags(multi_sfn, solar = TRUE)
  
  multi_env_data <- get_env_data(multi_sfn)
  multi_env_data_solar <- get_env_data(multi_sfn, solar = TRUE)
  multi_env_flags <- get_env_flags(multi_sfn)
  multi_env_flags_solar <- get_env_flags(multi_sfn, solar = TRUE)
  
  multi_timestamp <- get_timestamp(multi_sfn)
  multi_solar_timestamp <- get_solar_timestamp(multi_sfn)
  
  multi_si_code <- get_si_code(multi_sfn)
  
  multi_site_md <- get_site_md(multi_sfn)
  multi_stand_md <- get_stand_md(multi_sfn)
  multi_species_md <- get_species_md(multi_sfn)
  multi_plant_md <- get_plant_md(multi_sfn)
  multi_env_md <- get_env_md(multi_sfn)
  
  multi_site_md_collapsed <- get_site_md(multi_sfn, collapse = TRUE)
  multi_stand_md_collapsed <- get_stand_md(multi_sfn, collapse = TRUE)
  multi_species_md_collapsed <- get_species_md(multi_sfn, collapse = TRUE)
  multi_plant_md_collapsed <- get_plant_md(multi_sfn, collapse = TRUE)
  multi_env_md_collapsed <- get_env_md(multi_sfn, collapse = TRUE)
  
  expect_true(rlang::is_list(multi_sapf_data, 3))
  expect_true(rlang::is_list(multi_sapf_data_solar, 3))
  expect_true(rlang::is_list(multi_sapf_flags, 3))
  expect_true(rlang::is_list(multi_sapf_flags_solar, 3))
  expect_true(tibble::is_tibble(multi_sapf_data[[1]]))
  expect_true(tibble::is_tibble(multi_sapf_data_solar[[1]]))
  expect_true(tibble::is_tibble(multi_sapf_flags[[1]]))
  expect_true(tibble::is_tibble(multi_sapf_flags_solar[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_sapf_data[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_sapf_data_solar[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_sapf_flags[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_sapf_flags_solar[[1]]))
  expect_true(rlang::is_list(multi_env_data, 3))
  expect_true(rlang::is_list(multi_env_data_solar, 3))
  expect_true(rlang::is_list(multi_env_flags, 3))
  expect_true(rlang::is_list(multi_env_flags_solar, 3))
  expect_true(tibble::is_tibble(multi_env_data[[1]]))
  expect_true(tibble::is_tibble(multi_env_data_solar[[1]]))
  expect_true(tibble::is_tibble(multi_env_flags[[1]]))
  expect_true(tibble::is_tibble(multi_env_flags_solar[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_env_data[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_env_data_solar[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_env_flags[[1]]))
  expect_true('TIMESTAMP' %in% names(multi_env_flags_solar[[1]]))
  expect_true(rlang::is_list(multi_timestamp, 3))
  expect_true(rlang::is_list(multi_solar_timestamp, 3))
  expect_is(multi_timestamp[[1]], 'POSIXct')
  expect_is(multi_solar_timestamp[[1]], 'POSIXct')
  expect_true(rlang::is_list(multi_si_code, 3))
  expect_true(rlang::is_character(multi_si_code[[1]]))
  expect_true(rlang::is_list(multi_site_md, 3))
  expect_true(rlang::is_list(multi_stand_md, 3))
  expect_true(rlang::is_list(multi_species_md, 3))
  expect_true(rlang::is_list(multi_plant_md, 3))
  expect_true(rlang::is_list(multi_env_md, 3))
  expect_true(tibble::is_tibble(multi_site_md[[1]]))
  expect_true(tibble::is_tibble(multi_stand_md[[1]]))
  expect_true(tibble::is_tibble(multi_species_md[[1]]))
  expect_true(tibble::is_tibble(multi_plant_md[[1]]))
  expect_true(tibble::is_tibble(multi_env_md[[1]]))
  expect_true(tibble::is_tibble(multi_site_md_collapsed))
  expect_true(tibble::is_tibble(multi_stand_md_collapsed))
  expect_true(tibble::is_tibble(multi_species_md_collapsed))
  expect_true(tibble::is_tibble(multi_plant_md_collapsed))
  expect_true(tibble::is_tibble(multi_env_md_collapsed))
  expect_equal(nrow(multi_site_md_collapsed), 3)
  expect_equal(nrow(multi_stand_md_collapsed), 3)
  expect_equal(nrow(multi_species_md_collapsed), 4)
  expect_equal(nrow(multi_plant_md_collapsed), 43)
  expect_equal(nrow(multi_env_md_collapsed), 3)
})

# tests para replacement methods
test_that('replacement methods work as intended', {

  # data preparation
  foo_sapf_data <- get_sapf_data(ARG_TRE)
  foo_sapf_data_solar <- get_sapf_data(ARG_TRE, solar = TRUE)
  foo_sapf_flags <- get_sapf_flags(ARG_TRE)
  foo_sapf_flags_solar <- get_sapf_flags(ARG_TRE, solar = TRUE)

  foo_env_data <- get_env_data(ARG_TRE)
  foo_env_data_solar <- get_env_data(ARG_TRE, solar = TRUE)
  foo_env_flags <- get_env_flags(ARG_TRE)
  foo_env_flags_solar <- get_env_flags(ARG_TRE, solar = TRUE)

  foo_timestamp <- get_timestamp(ARG_TRE)
  foo_solar_timestamp <- get_solar_timestamp(ARG_TRE)

  foo_si_code <- get_si_code(ARG_TRE)

  foo_site_md <- get_site_md(ARG_TRE)
  foo_stand_md <- get_stand_md(ARG_TRE)
  foo_species_md <- get_species_md(ARG_TRE)
  foo_plant_md <- get_plant_md(ARG_TRE)
  foo_env_md <- get_env_md(ARG_TRE)

  foo_sapf_data_NA <- foo_sapf_data
  foo_sapf_data_NA[,2] <- NA
  foo_sapf_data_row <- foo_sapf_data[-25,]

  foo_sapf_data_solar_NA <- foo_sapf_data_solar
  foo_sapf_data_solar_NA[,2] <- NA
  foo_sapf_data_solar_row <- foo_sapf_data_solar[-25,]

  foo_env_data_NA <- foo_env_data
  foo_env_data_NA[,2] <- NA
  foo_env_data_row <- foo_env_data[-25,]

  foo_env_data_solar_NA <- foo_env_data_solar
  foo_env_data_solar_NA[,2] <- NA
  foo_env_data_solar_row <- foo_env_data_solar[-25,]

  foo_sapf_flags_NA <- foo_sapf_flags
  foo_sapf_flags_NA[,2] <- NA
  foo_sapf_flags_row <- foo_sapf_flags[-25,]

  foo_sapf_flags_solar_NA <- foo_sapf_flags_solar
  foo_sapf_flags_solar_NA[,2] <- NA
  foo_sapf_flags_solar_row <- foo_sapf_flags_solar[-25,]

  foo_env_flags_NA <- foo_env_flags
  foo_env_flags_NA[,2] <- NA
  foo_env_flags_row <- foo_env_flags[-25,]

  foo_env_flags_solar_NA <- foo_env_flags_solar
  foo_env_flags_solar_NA[,2] <- NA
  foo_env_flags_solar_row <- foo_env_flags_solar[-25,]

  foo_timestamp_NA <- foo_timestamp
  foo_timestamp_NA[25] <- NA
  foo_timestamp_row <- foo_timestamp[-25]

  foo_solar_timestamp_NA <- foo_solar_timestamp
  foo_solar_timestamp_NA[25] <- NA
  foo_solar_timestamp_row <- foo_solar_timestamp[-25]

  foo_si_code_NA <- ''

  foo_site_md_NA <- foo_site_md
  foo_site_md_NA[['si_lat']] <- NA

  foo_stand_md_NA <- foo_stand_md
  foo_stand_md_NA[['st_name']] <- NA

  foo_species_md_NA <- foo_species_md
  foo_species_md_NA[['sp_ntrees']] <- NA

  foo_plant_md_NA <- foo_plant_md
  foo_plant_md_NA[['pl_dbh']] <- NA

  foo_env_md_NA <- foo_env_md
  foo_env_md_NA[['env_precip']] <- NA

  # tests
  # errors due to dimensions
  expect_error(
    get_sapf_data(ARG_TRE) <- foo_sapf_data_row[,-1],
    'new data is not valid'
  )

  expect_error(
    get_env_data(ARG_TRE) <- foo_env_data_row[,-1],
    'new data is not valid'
  )

  expect_error(
    get_sapf_flags(ARG_TRE) <- foo_sapf_flags_row[,-1],
    'new data is not valid'
  )

  expect_error(
    get_env_flags(ARG_TRE) <- foo_env_flags_row[,-1],
    'new data is not valid'
  )

  expect_error(
    get_timestamp(ARG_TRE) <- foo_timestamp_row,
    'new data is not valid'
  )

  expect_error(
    get_solar_timestamp(ARG_TRE) <- foo_solar_timestamp_row,
    'new data is not valid'
  )

  expect_error(
    get_si_code(ARG_TRE) <- foo_si_code_NA
  )

  expect_error(
    get_si_code(ARG_TRE) <- c(rep('ARG_TRE', 25))
  )

  expect_error(
    get_site_md(ARG_TRE) <- data.frame()
  )

  # works when substituting with correct data
  get_sapf_data(ARG_TRE) <- foo_sapf_data_NA[,-1]
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_sapf_flags(ARG_TRE) <- foo_sapf_flags_NA[,-1]
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_env_data(ARG_TRE) <- foo_env_data_NA[,-1]
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_env_flags(ARG_TRE) <- foo_env_flags_NA[,-1]
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_timestamp(ARG_TRE) <- foo_timestamp_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_solar_timestamp(ARG_TRE) <- foo_solar_timestamp_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_si_code(ARG_TRE) <- 'ARG_MAZ'
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_site_md(ARG_TRE) <- foo_site_md_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_stand_md(ARG_TRE) <- foo_stand_md_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_species_md(ARG_TRE) <- foo_species_md_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_plant_md(ARG_TRE) <- foo_plant_md_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
  get_env_md(ARG_TRE) <- foo_env_md_NA
  expect_s4_class(ARG_TRE, 'sfn_data')
})


# tests para show methods
test_that('show methods works', {
  expect_output(print(ARG_TRE), 'Data from ARG_TRE site')
  expect_output(print(ARG_TRE), 'Environmental data flags:')
  expect_output(print(ARG_MAZ), 'Data from ARG_MAZ site')
  expect_output(print(ARG_MAZ), 'Environmental data flags:')
  expect_output(print(AUS_CAN_ST2_MIX), 'Data from AUS_CAN_ST2_MIX site')
  expect_output(print(AUS_CAN_ST2_MIX), 'Environmental data flags:')
  expect_output(print(multi_sfn), 'ARG_TRE ARG_MAZ AUS_CAN_ST2_MIX')
  expect_output(print(multi_sfn), 'for the combined sites:')
})
