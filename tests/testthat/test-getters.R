context("getters")

#### read_sfn_data tests ####
test_that('read_sfn_data load the objects correctly', {

  expect_s4_class(
    read_sfn_data('FOO', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data('BAR', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data('BAZ', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data(c('FOO', 'BAR', 'BAZ'), 'Data'), 'sfn_data_multi'
  )

})

#### as_sfn_data_multi tests ####
test_that('as_sfn_data_multi helper works as intended', {

  FOO <- read_sfn_data('FOO', 'Data')
  BAR <- read_sfn_data('BAR', 'Data')
  BAZ <- read_sfn_data('BAZ', 'Data')

  multi_list <- list(FOO, BAR, BAZ)

  expect_s4_class(
    sapfluxnetr:::as_sfn_data_multi(multi_list), 'sfn_data_multi'
  )

  multi_sfn <- sapfluxnetr:::as_sfn_data_multi(multi_list)

  expect_identical(
    names(multi_sfn), c('FOO', 'BAR', 'BAZ')
  )

  expect_s4_class(
    multi_sfn[[1]], 'sfn_data'
  )

  expect_s4_class(
    multi_sfn[[2]], 'sfn_data'
  )

  expect_s4_class(
    multi_sfn[['BAZ']], 'sfn_data'
  )
})

#### filter_by_var tests ####
test_that('filter_by_var combines all metadata correctly', {
  
  filters <- list(dplyr::quo(pl_sens_meth == 'HR'))
  
  expect_true(
    is.character(filter_by_var(!!!filters, folder = 'Data'))
  )
  
  expect_length(
    filter_by_var(!!!filters, folder = 'Data'), 3
  )
  
  filters <- list(
    dplyr::quo(pl_sens_meth == 'HD'),
    dplyr::quo(env_ta == 'Clearing')
  )
  
  expect_length(
    filter_by_var(!!!filters, folder = 'Data'), 0
  )
  
  filters <- list(
    dplyr::quo(pl_sens_meth == 'HR'),
    dplyr::quo(env_ta == 'Above canopy')
  )
  
  expect_length(
    filter_by_var(!!!filters, folder = 'Data'), 0
  )
  
  expect_error(
    filter_by_var(!!!filters, folder = 'tururu'),
    'tururu'
  )
  
  filters <- list(
    dplyr::quo(pl_sens_meth == 'HR'),
    dplyr::quo(env_nonexistentname == 'Above canopy')
  )
  
  expect_error(
    filter_by_var(!!!filters, folder = 'Data'),
    'env_nonexistentname'
  )
  
  # TODO tests con los diferentes metadatas por separado, tests con combinaciones
  
})