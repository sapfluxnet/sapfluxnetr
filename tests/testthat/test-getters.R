#### read_sfn_data tests ####
test_that('read_sfn_data load the objects correctly', {

  expect_s4_class(
    read_sfn_data('ARG_TRE', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data('ARG_MAZ', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data('AUS_CAN_ST2_MIX', 'Data'), 'sfn_data'
  )

  expect_s4_class(
    read_sfn_data(c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX'), 'Data'), 'sfn_data_multi'
  )

})

#### as_sfn_data_multi tests ####
test_that('as_sfn_data_multi helper works as intended', {

  ARG_TRE <- read_sfn_data('ARG_TRE', 'Data')
  ARG_MAZ <- read_sfn_data('ARG_MAZ', 'Data')
  AUS_CAN_ST2_MIX <- read_sfn_data('AUS_CAN_ST2_MIX', 'Data')

  multi_list <- list(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)

  expect_s4_class(
    sapfluxnetr:::as_sfn_data_multi(multi_list), 'sfn_data_multi'
  )

  multi_sfn <- sapfluxnetr:::as_sfn_data_multi(multi_list)

  expect_identical(
    names(multi_sfn), c('ARG_TRE', 'ARG_MAZ', 'AUS_CAN_ST2_MIX')
  )

  expect_s4_class(
    multi_sfn[[1]], 'sfn_data'
  )

  expect_s4_class(
    multi_sfn[[2]], 'sfn_data'
  )

  expect_s4_class(
    multi_sfn[['AUS_CAN_ST2_MIX']], 'sfn_data'
  )
})

#### read_sfn_metadata tests ####
test_that('.write_sfn_metadata writes correctly the file', {

  #testthat::skip_on_cran()
  
  folder <- tempdir()
  save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
  save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
  save(AUS_CAN_ST2_MIX, file = file.path(folder, 'AUS_CAN_ST2_MIX.RData'))
  
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

  sfn_metadata_2 <- sapfluxnetr:::.write_metadata_cache(folder)
  expect_true(file.exists(file.path(folder, '.metadata_cache.RData')))
  expect_identical(sfn_metadata, sfn_metadata_2)

  unlink(file.path(folder, '.metadata_cache.RData'))

})

test_that('read_sfn_metadata works as intended', {

  folder <- tempdir()
  save(ARG_TRE, file = file.path(folder, 'ARG_TRE.RData'))
  save(ARG_MAZ, file = file.path(folder, 'ARG_MAZ.RData'))
  save(AUS_CAN_ST2_MIX, file = file.path(folder, 'AUS_CAN_ST2_MIX.RData'))

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

#### filter_sites_by_md tests ####
test_that('filter_sites_by_md combines all metadata correctly', {

  data('sfn_metadata_ex', package = 'sapfluxnetr')
  sites <- sfn_sites_in_folder('Data')
  filters <- list(dplyr::quo(pl_sens_meth == 'HR'))

  # expect_true(
  #   is.character(filter_by_var(!!!filters, folder = 'Data'))
  # )
  expect_true(is.character(filter_sites_by_md(
    sites, sfn_metadata_ex, !!!filters
  )))

  # expect_length(
  #   filter_by_var(!!!filters, folder = 'Data'), 2
  # )
  expect_length(
    filter_sites_by_md(sites, sfn_metadata_ex, !!!filters), 2
  )

  # expect_identical(
  #   filter_by_var(!!!filters, folder = 'Data'), c('ARG_MAZ', 'ARG_TRE')
  # )
  expect_identical(
    filter_sites_by_md(sites, sfn_metadata_ex, !!!filters),
    c('ARG_MAZ', 'ARG_TRE')
  )

  filters <- list(
    dplyr::quo(pl_sens_meth == 'HD'),
    dplyr::quo(env_ta == 'Clearing')
  )

  # expect_length(
  #   filter_by_var(!!!filters, folder = 'Data'), 0
  # )
  expect_length(
    filter_sites_by_md(sites, sfn_metadata_ex, !!!filters), 0
  )

  filters <- list(
    dplyr::quo(pl_sens_meth == 'HR'),
    dplyr::quo(env_ta == 'Above canopy')
  )

  # expect_length(
  #   filter_by_var(!!!filters, folder = 'Data'), 0
  # )
  expect_length(
    filter_sites_by_md(sites, sfn_metadata_ex, !!!filters), 0
  )

  # expect_error(
  #   filter_by_var(!!!filters, folder = 'tururu'),
  #   'tururu'
  # )

  filters <- list(
    dplyr::quo(pl_sens_meth == 'HR'),
    dplyr::quo(env_nonexistentname == 'Above canopy')
  )

  # expect_error(
  #   filter_by_var(!!!filters, folder = 'Data'),
  #   'env_nonexistentname'
  # )
  expect_error(
    filter_sites_by_md(sites, sfn_metadata_ex, !!!filters),
    'env_nonexistentname'
  )

  # TODO tests con los diferentes metadatas por separado, tests con combinaciones

})

#### sfn_sites_in_folder ####
test_that('sfn_sites_in_folder returns the expected results', {
  
  expect_true(is.character(sfn_sites_in_folder('Data')))
  expect_length(sfn_sites_in_folder('Data'), 3)
  expect_identical(
    sfn_sites_in_folder('Data'), c('ARG_MAZ', 'ARG_TRE', 'AUS_CAN_ST2_MIX')
  )
  
  # errors
  expect_error(sfn_sites_in_folder('NonExistentFolder'))
  expect_error(sfn_sites_in_folder(53))
  
})

#### teardown
# teardown(unlink(file.path('Data', '.metadata_cache.RData')))
