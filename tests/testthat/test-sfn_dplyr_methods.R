context("sfn_dplyr_methods")

data('ARG_TRE', package = 'sapfluxnetr')
data('ARG_MAZ', package = 'sapfluxnetr')
data('AUS_CAN_ST2_MIX', package = 'sapfluxnetr')

#### filter #####
test_that("sfn_filter returns correct results", {
  foo_timestamp <- get_timestamp(ARG_TRE)
  foo_timestamp_trimmed <- foo_timestamp[1:100]
  foo_solar_timestamp <- get_solar_timestamp(ARG_TRE)
  foo_solar_timestamp_trimmed <- foo_solar_timestamp[1:100]
  ws_threshold <- 25
  foo_subset <- sfn_filter(ARG_TRE, TIMESTAMP %in% foo_timestamp_trimmed)
  foo_subset_2 <- sfn_filter(ARG_TRE, ws <= ws_threshold)
  foo_subset_3 <- sfn_filter(
    ARG_TRE, TIMESTAMP %in% foo_solar_timestamp_trimmed,
    solar = TRUE
  )
  multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ)
  multi_sfn_filter <- sfn_filter(
    multi_sfn, dplyr::between(lubridate::day(TIMESTAMP), 19, 23)
  )
  multi_sfn_filter_2 <- sfn_filter(multi_sfn, ws <= ws_threshold)
  multi_sfn_2 <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
  suppressWarnings(multi_sfn_filter_3 <- sfn_filter(
    multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 1998, 1999)
  ))
  suppressWarnings(multi_sfn_filter_4 <- sfn_filter(
    multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 2008, 2009)
  ))
  suppressWarnings(multi_sfn_filter_5 <- sfn_filter(
    multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 2006, 2007)
  ))


  expect_s4_class(foo_subset, 'sfn_data')
  # only check one because the constructor checks the others
  expect_length(get_solar_timestamp(foo_subset), 100)

  expect_s4_class(foo_subset_2, 'sfn_data')
  expect_true(all(get_env_data(foo_subset_2)[['ws']] <= 25))
  # only check one because the constructor checks the others
  expect_length(get_solar_timestamp(foo_subset_2), 212)

  expect_s4_class(foo_subset_3, 'sfn_data')
  # only check one because the constructor checks the others
  expect_length(get_solar_timestamp(foo_subset_3), 100)

  expect_identical(foo_subset, foo_subset_3)

  expect_s4_class(multi_sfn_filter, 'sfn_data_multi')
  expect_length(multi_sfn_filter, 2)
  expect_s4_class(multi_sfn_filter[[1]], 'sfn_data')
  expect_s4_class(multi_sfn_filter[[2]], 'sfn_data')
  expect_length(get_timestamp(multi_sfn_filter[[1]]), 24*5)
  expect_length(get_timestamp(multi_sfn_filter[[2]]), 24*5)

  expect_s4_class(multi_sfn_filter_2, 'sfn_data_multi')
  expect_length(multi_sfn_filter_2, 2)
  expect_s4_class(multi_sfn_filter_2[[1]], 'sfn_data')
  expect_s4_class(multi_sfn_filter_2[[2]], 'sfn_data')
  expect_length(get_solar_timestamp(multi_sfn_filter_2[[1]]), 212)
  expect_length(get_solar_timestamp(multi_sfn_filter_2[[2]]), 210)

  expect_s4_class(multi_sfn_filter_3, 'sfn_data_multi')
  expect_length(multi_sfn_filter_3, 0)
  expect_warning(
    sfn_filter(
      multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 1998, 1999)
    ), 'Any sites met the criteria, returning empty results'
  )

  expect_s4_class(multi_sfn_filter_4, 'sfn_data_multi')
  expect_length(multi_sfn_filter_4, 2)
  expect_warning(
    sfn_filter(
      multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 2008, 2009)
    ), 'AUS_CAN_ST2_MIX'
  )

  expect_s4_class(multi_sfn_filter_5, 'sfn_data_multi')
  expect_length(multi_sfn_filter_5, 1)
  expect_warning(
    sfn_filter(
      multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 2006, 2007)
    ), 'ARG_TRE'
  )

})

#### mutate ####
test_that('sfn_mutate returns correct results', {

  foo_mutated <- sfn_mutate(ARG_TRE, ws = dplyr::if_else(ws > 25, NA_real_, ws))
  multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
  multi_mutated <- sfn_mutate(
    multi_sfn, ws = dplyr::if_else(ws > 25, NA_real_, ws)
  )

  expect_s4_class(foo_mutated, 'sfn_data')
  expect_equal(sum(is.na(get_env_data(foo_mutated)[['ws']])), 100)
  expect_match(get_env_flags(foo_mutated)[['ws']], 'USER_MODF', all = TRUE)
  expect_match(get_env_flags(foo_mutated)[['ws']], 'RANGE_WARN', all = FALSE)
  expect_equal(sum(is.na(get_env_data(foo_mutated)[['ta']])), 0)
  expect_failure(
    expect_match(get_env_flags(foo_mutated)[['ta']], 'USER_MODF', all = TRUE)
  )
  expect_identical(attr(get_timestamp(foo_mutated), 'tz'), 'Etc/GMT+3')
  expect_identical(attr(get_solar_timestamp(foo_mutated), 'tz'), 'UTC')

  expect_s4_class(multi_mutated, 'sfn_data_multi')
  expect_length(multi_mutated, 3)
  expect_s4_class(multi_mutated[[1]], 'sfn_data')
  expect_s4_class(multi_mutated[[2]], 'sfn_data')
  expect_s4_class(multi_mutated[[3]], 'sfn_data')
  expect_identical(attr(get_timestamp(multi_mutated[[1]]), 'tz'), 'Etc/GMT+3')
  expect_identical(attr(get_solar_timestamp(multi_mutated[[1]]), 'tz'), 'UTC')
  expect_identical(attr(get_timestamp(multi_mutated[[2]]), 'tz'), 'Etc/GMT+3')
  expect_identical(attr(get_solar_timestamp(multi_mutated[[2]]), 'tz'), 'UTC')
  expect_identical(attr(get_timestamp(multi_mutated[[3]]), 'tz'), 'Etc/GMT-10')
  expect_identical(attr(get_solar_timestamp(multi_mutated[[3]]), 'tz'), 'UTC')
  expect_equal(sum(is.na(get_env_data(multi_mutated[[1]])[['ws']])), 100)
  expect_match(get_env_flags(multi_mutated[[1]])[['ws']], 'USER_MODF', all = TRUE)
  expect_match(get_env_flags(multi_mutated[[1]])[['ws']], 'RANGE_WARN', all = FALSE)
  expect_equal(sum(is.na(get_env_data(multi_mutated[[2]])[['ws']])), 78)
  expect_match(get_env_flags(multi_mutated[[2]])[['ws']], 'USER_MODF', all = TRUE)
  expect_match(get_env_flags(multi_mutated[[2]])[['ws']], 'RANGE_WARN', all = FALSE)
  expect_failure(
    expect_match(get_env_flags(multi_mutated[[3]])[['ws']], 'USER_MODF', all = TRUE)
  )
  expect_match(get_env_flags(multi_mutated[[3]])[['ws']], 'OUT_WARN', all = FALSE)
  expect_identical(multi_sfn[['AUS_CAN_ST2_MIX']], multi_mutated[['AUS_CAN_ST2_MIX']])
})

#### mutate_at ####
test_that('sfn_mutate_at returns correct results', {

  vars_to_mutate <- names(get_sapf_data(ARG_TRE)[,-1])
  foo_mutated <- sfn_mutate_at(
    ARG_TRE,
    .vars = dplyr::vars(dplyr::one_of(vars_to_mutate)),
    .funs = dplyr::funs(
      dplyr::case_when(
        ws > 25 ~ NA_real_,
        TRUE ~ .
      )
    )
  )

  vars_to_not_mutate <- names(get_env_data(ARG_TRE))
  multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
  multi_mutated <- suppressWarnings(sfn_mutate_at(
    multi_sfn,
    .vars = dplyr::vars(-dplyr::one_of(vars_to_not_mutate)), # we use -
    .funs = dplyr::funs(
      dplyr::case_when(
        ws > 25 ~ NA_real_,
        TRUE ~ .
      )
    )
  ))

  expect_s4_class(foo_mutated, 'sfn_data')
  expect_equal(sum(is.na(get_sapf_data(foo_mutated)[[2]])), 100)
  expect_equal(sum(is.na(get_sapf_data(foo_mutated)[[3]])), 100)
  expect_equal(sum(is.na(get_sapf_data(foo_mutated)[[4]])), 100)
  expect_equal(sum(is.na(get_sapf_data(foo_mutated)[[5]])), 100)
  expect_match(get_sapf_flags(foo_mutated)[[2]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(foo_mutated)[[3]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(foo_mutated)[[4]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(foo_mutated)[[5]], 'USER_MODF', all = TRUE)

  expect_s4_class(multi_mutated, 'sfn_data_multi')
  expect_length(multi_mutated, 3)
  expect_s4_class(multi_mutated[[1]], 'sfn_data')
  expect_s4_class(multi_mutated[[2]], 'sfn_data')
  expect_s4_class(multi_mutated[[3]], 'sfn_data')
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[1]])[[2]])), 100)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[1]])[[3]])), 100)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[1]])[[4]])), 100)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[1]])[[5]])), 100)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[2]])[[2]])), 78)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[2]])[[3]])), 78)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[2]])[[4]])), 78)
  expect_equal(sum(is.na(get_sapf_data(multi_mutated[[2]])[[5]])), 78)
  expect_match(get_sapf_flags(multi_mutated[[1]])[[2]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[1]])[[3]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[1]])[[4]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[1]])[[5]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[2]])[[2]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[2]])[[3]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[2]])[[4]], 'USER_MODF', all = TRUE)
  expect_match(get_sapf_flags(multi_mutated[[2]])[[5]], 'USER_MODF', all = TRUE)
  expect_failure(
    expect_match(get_sapf_flags(multi_mutated[[3]])[[2]], 'USER_MODF', all = TRUE)
  )
  expect_failure(
    expect_match(get_sapf_flags(multi_mutated[[3]])[[3]], 'USER_MODF', all = TRUE)
  )
  expect_failure(
    expect_match(get_sapf_flags(multi_mutated[[3]])[[4]], 'USER_MODF', all = TRUE)
  )
  expect_failure(
    expect_match(get_sapf_flags(multi_mutated[[3]])[[5]], 'USER_MODF', all = TRUE)
  )
  expect_identical(multi_sfn[['AUS_CAN_ST2_MIX']], multi_mutated[['AUS_CAN_ST2_MIX']])

})

#### metrics_tidyfier #####
test_that('metrics_tidyfier returns the expected object for single metrics', {

  foo_daily <- daily_metrics(ARG_TRE, TRUE)
  sfn_metadata <- sapfluxnetr:::.write_metadata_cache('Data', .dry = TRUE)

  foo_gen_tidy <- metrics_tidyfier(foo_daily, sfn_metadata, 'gen')
  foo_md_tidy <- metrics_tidyfier(foo_daily, sfn_metadata, 'md')
  foo_pd_tidy <- metrics_tidyfier(foo_daily, sfn_metadata, 'pd')

  # class
  expect_s3_class(foo_gen_tidy, 'tbl')
  expect_s3_class(foo_md_tidy, 'tbl')
  expect_s3_class(foo_pd_tidy, 'tbl')

  # is the metadata there?
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(foo_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(foo_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(foo_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(foo_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(foo_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(foo_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(foo_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(foo_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(foo_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(foo_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(foo_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(foo_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(foo_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(foo_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(foo_pd_tidy)))

  # is the data there
  sapflow_vars <- paste0(
    'sapflow_', names(sapfluxnetr:::.fixed_metrics_funs(c(0.95, 0.99), TRUE))
  )

  env_vars <- sapfluxnetr:::.env_vars_names() %>%
    purrr::map(
      ~ paste0(
        .x, '_', names(sapfluxnetr:::.fixed_metrics_funs(c(0.95, 0.99), FALSE))
      )
    ) %>%
    purrr::flatten_chr()

  expect_true(all(sapflow_vars %in% names(foo_gen_tidy)))
  expect_true(any(env_vars %in% names(foo_gen_tidy)))
  expect_true(all(paste0(sapflow_vars, '_md') %in% names(foo_md_tidy)))
  expect_true(any(paste0(env_vars, '_md') %in% names(foo_md_tidy)))
  expect_true(all(paste0(sapflow_vars, '_pd') %in% names(foo_pd_tidy)))
  expect_true(any(paste0(env_vars, '_pd') %in% names(foo_pd_tidy)))

  # has it the rows it should?
  expect_equal(nrow(foo_gen_tidy), 4*14) # trees * days
  expect_equal(nrow(foo_md_tidy), 4*13)
  expect_equal(nrow(foo_pd_tidy), 4*13)

})

test_that('metrics_tidyfier returns the expected object for multi metrics', {

  multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)
  multi_daily <- daily_metrics(multi_sfn, TRUE)
  sfn_metadata <- sapfluxnetr:::.write_metadata_cache('Data', .dry = TRUE)

  multi_gen_tidy <- metrics_tidyfier(multi_daily, sfn_metadata, 'gen')
  multi_md_tidy <- metrics_tidyfier(multi_daily, sfn_metadata, 'md')
  multi_pd_tidy <- metrics_tidyfier(multi_daily, sfn_metadata, 'pd')

  # class
  expect_s3_class(multi_gen_tidy, 'tbl')
  expect_s3_class(multi_md_tidy, 'tbl')
  expect_s3_class(multi_pd_tidy, 'tbl')

  # is the metadata there?
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(multi_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(multi_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(multi_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(multi_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(multi_gen_tidy)))
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(multi_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(multi_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(multi_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(multi_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(multi_md_tidy)))
  expect_true(all(sfn_vars_to_filter()[['site_md']] %in% names(multi_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['stand_md']] %in% names(multi_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['species_md']] %in% names(multi_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['plant_md']] %in% names(multi_pd_tidy)))
  expect_true(all(sfn_vars_to_filter()[['env_md']] %in% names(multi_pd_tidy)))

  # is the data there
  sapflow_vars <- paste0(
    'sapflow_', names(sapfluxnetr:::.fixed_metrics_funs(c(0.95, 0.99), TRUE))
  )

  env_vars <- sapfluxnetr:::.env_vars_names() %>%
    purrr::map(
      ~ paste0(
        .x, '_', names(sapfluxnetr:::.fixed_metrics_funs(c(0.95, 0.99), FALSE))
      )
    ) %>%
    purrr::flatten_chr()

  expect_true(all(sapflow_vars %in% names(multi_gen_tidy)))
  expect_true(any(env_vars %in% names(multi_gen_tidy)))
  expect_true(all(paste0(sapflow_vars, '_md') %in% names(multi_md_tidy)))
  expect_true(any(paste0(env_vars, '_md') %in% names(multi_md_tidy)))
  expect_true(all(paste0(sapflow_vars, '_pd') %in% names(multi_pd_tidy)))
  expect_true(any(paste0(env_vars, '_pd') %in% names(multi_pd_tidy)))

  # has it the rows it should?
  expect_equal(nrow(multi_gen_tidy), (5*13) + (4*14) + (34*372))  # trees * days
  expect_equal(nrow(multi_md_tidy), (5*12) + (4*13) + (34*371))
  expect_equal(nrow(multi_pd_tidy), (5*12) + (4*13) + (34*371))

})
