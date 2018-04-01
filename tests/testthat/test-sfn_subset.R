context("sfn_subset")

test_that("sfn_subset examples work", {
  data('FOO', package = 'sapfluxnetr')
  data('BAR', package = 'sapfluxnetr')
  foo_timestamp <- get_timestamp(FOO)
  foo_timestamp_trimmed <- foo_timestamp[1:100]
  foo_solar_timestamp <- get_solar_timestamp(FOO)
  foo_solar_timestamp_trimmed <- foo_solar_timestamp[1:100]
  ws_threshold <- 25
  foo_subset <- sfn_subset(FOO, TIMESTAMP %in% foo_timestamp_trimmed)
  foo_subset_2 <- sfn_subset(FOO, ws <= ws_threshold)
  foo_subset_3 <- sfn_subset(
    FOO, TIMESTAMP %in% foo_solar_timestamp_trimmed,
    solar = TRUE
  )
  multi_sfn <- sfn_data_multi(FOO, BAR)
  multi_sfn_subset <- sfn_subset(
    multi_sfn, dplyr::between(lubridate::day(TIMESTAMP), 19, 23)
  )
  multi_sfn_subset_2 <- sfn_subset(multi_sfn, ws <= ws_threshold)
  
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
  
  expect_s4_class(multi_sfn_subset, 'sfn_data_multi')
  expect_s4_class(multi_sfn_subset[[1]], 'sfn_data')
  expect_s4_class(multi_sfn_subset[[2]], 'sfn_data')
  expect_length(get_timestamp(multi_sfn_subset[[1]]), 24*5)
  expect_length(get_timestamp(multi_sfn_subset[[2]]), 24*5)
  
  expect_s4_class(multi_sfn_subset_2, 'sfn_data_multi')
  expect_s4_class(multi_sfn_subset_2[[1]], 'sfn_data')
  expect_s4_class(multi_sfn_subset_2[[2]], 'sfn_data')
  expect_length(get_solar_timestamp(multi_sfn_subset_2[[1]]), 212)
  expect_length(get_solar_timestamp(multi_sfn_subset_2[[2]]), 210)
})
