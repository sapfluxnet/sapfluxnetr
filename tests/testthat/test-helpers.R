test_that("describe_md_variable works", {
  expect_output(describe_md_variable('pl_sens_meth'), 'Type:')
  expect_output(describe_md_variable('st_age'), 'Values:')
  expect_output(describe_md_variable('si_lat'), 'Units:')
  expect_output(describe_md_variable('sp_leaf_habit'), 'Description:')
  expect_output(describe_md_variable('env_vpd'), 'Type:')
})

test_that("accumulated metric helper function works", {
  
  # skip_on_cran()
  
  data <- get_env_data(ARG_TRE) %>% dplyr::select(TIMESTAMP, precip)
  
  expect_identical(
    sapfluxnetr:::.accumulated_posix_aware(data$TIMESTAMP), data$TIMESTAMP[1]
  )
  expect_equal(
    sapfluxnetr:::.accumulated_posix_aware(data$precip), 38.5
  )
  expect_identical(
    sapfluxnetr:::.accumulated_posix_aware(data$precip), sum(data$precip)
  )
  
  data_summ <- data %>% dplyr::summarise_all(sapfluxnetr:::.accumulated_posix_aware)
  
  expect_s3_class(data_summ, 'tbl')
  expect_identical(names(data_summ), c('TIMESTAMP', 'precip'))
  expect_identical(data_summ[['TIMESTAMP']][1], data$TIMESTAMP[1])
  expect_equal(data_summ[['precip']][1], 38.5)
})
