context("sfn_dplyr_methods")

data('FOO', package = 'sapfluxnetr')
data('BAR', package = 'sapfluxnetr')
data('BAZ', package = 'sapfluxnetr')

#### filter #####
test_that("sfn_filter returns correct results", {
  foo_timestamp <- get_timestamp(FOO)
  foo_timestamp_trimmed <- foo_timestamp[1:100]
  foo_solar_timestamp <- get_solar_timestamp(FOO)
  foo_solar_timestamp_trimmed <- foo_solar_timestamp[1:100]
  ws_threshold <- 25
  foo_subset <- sfn_filter(FOO, TIMESTAMP %in% foo_timestamp_trimmed)
  foo_subset_2 <- sfn_filter(FOO, ws <= ws_threshold)
  foo_subset_3 <- sfn_filter(
    FOO, TIMESTAMP %in% foo_solar_timestamp_trimmed,
    solar = TRUE
  )
  multi_sfn <- sfn_data_multi(FOO, BAR)
  multi_sfn_filter <- sfn_filter(
    multi_sfn, dplyr::between(lubridate::day(TIMESTAMP), 19, 23)
  )
  multi_sfn_filter_2 <- sfn_filter(multi_sfn, ws <= ws_threshold)
  multi_sfn_2 <- sfn_data_multi(FOO, BAR, BAZ)
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
    ), 'BAZ'
  )
  
  expect_s4_class(multi_sfn_filter_5, 'sfn_data_multi')
  expect_length(multi_sfn_filter_5, 1)
  expect_warning(
    sfn_filter(
      multi_sfn_2, dplyr::between(lubridate::year(TIMESTAMP), 2006, 2007)
    ), 'FOO'
  )
  
})

#### mutate ####
test_that('sfn_mutate returns correct results', {
  
  foo_mutated <- sfn_mutate(FOO, ws = dplyr::if_else(ws > 25, NA_real_, ws))
  
  expect_s4_class(foo_mutated, 'sfn_data')
  expect_equal(sum(is.na(get_env_data(foo_mutated)[['ws']])), 100)
  expect_match(get_env_flags(foo_mutated)[['ws']], 'USER_MODF', all = TRUE)
  expect_match(get_env_flags(foo_mutated)[['ws']], 'RANGE_WARN', all = FALSE)
  expect_equal(sum(is.na(get_env_data(foo_mutated)[['ta']])), 0)
  expect_failure(
    expect_match(get_env_flags(foo_mutated)[['ta']], 'USER_MODF', all = TRUE)
  )
})