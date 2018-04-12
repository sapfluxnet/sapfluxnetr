context("visualizations")

data(ARG_TRE, package = 'sapfluxnetr')

test_that('sfn_plot returns the object correctly', {

  expect_s3_class(sfn_plot(ARG_TRE), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, solar = FALSE), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'env'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'ta'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'vpd'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, formula = ~vpd), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, formula = ~ext_rad), 'gg')

})
