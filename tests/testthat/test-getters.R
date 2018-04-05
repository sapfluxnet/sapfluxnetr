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

#### read_sfn_metadata tests ####
test_that('.write_sfn_metadata writes correctly the file', {

  skip_on_cran()

  folder <- 'Data'
  sfn_metadata <- sapfluxnetr:::.write_metadata_cache(folder, .dry = TRUE)

  expect_false(file.exists(file.path(folder, '.metadata_cache.RData')))
  expect_true(is.list(sfn_metadata))
  expect_length(sfn_metadata, 5)
  expect_equal(nrow(sfn_metadata[['site_md']]), 3)
  expect_s3_class(sfn_metadata[['site_md']], 'tbl')
  expect_s3_class(sfn_metadata[['stand_md']], 'tbl')
  expect_s3_class(sfn_metadata[['species_md']], 'tbl')
  expect_s3_class(sfn_metadata[['plant_md']], 'tbl')
  expect_s3_class(sfn_metadata[['env_md']], 'tbl')

  sfn_metadata_2 <- sapfluxnetr::.write_metadata_cache(folder)
  expect_true(file.exists(file.path(folder, '.metadata_cache.RData')))
  expect_identical(sfn_metadata, sfn_metadata_2)

  unlink(file.path(folder, '.metadata_cache.RData'))

})

test_that('read_sfn_metadata works as intended', {

  folder <- 'Data'

  expect_error(
    read_sfn_metadata(folder = folder), 'metadata cache file not found at'
  )

  sfn_metadata <- read_sfn_metadata(folder, .write_cache = TRUE)

  expect_true(file.exists(file.path(folder, '.metadata_cache.RData')))
  expect_true(is.list(sfn_metadata))
  expect_length(sfn_metadata, 5)
  expect_equal(nrow(sfn_metadata[['site_md']]), 3)

  sfn_metadata_2 <- read_sfn_metadata(folder)
  expect_true(is.list(sfn_metadata))
  expect_length(sfn_metadata, 5)
  expect_equal(nrow(sfn_metadata[['site_md']]), 3)
  expect_identical(sfn_metadata, sfn_metadata_2)

  unlink(file.path(folder, '.metadata_cache.RData'))

})

#### filter_by_var tests ####
test_that('filter_by_var combines all metadata correctly', {

  filters <- list(dplyr::quo(pl_sens_meth == 'HR'))

  expect_true(
    is.character(filter_by_var(!!!filters, folder = 'Data'))
  )

  expect_length(
    filter_by_var(!!!filters, folder = 'Data'), 2
  )

  expect_identical(
    filter_by_var(!!!filters, folder = 'Data'), c('BAR', 'FOO')
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


#### teardown
# teardown(unlink(file.path('Data', '.metadata_cache.RData')))
