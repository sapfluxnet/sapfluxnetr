context("visualizations")

data(FOO, package = 'sapfluxnetr')

test_that('sfn_plot returns the object correctly', {
  
  expect_s3_class(sfn_plot(FOO), 'gg')
  expect_s3_class(sfn_plot(FOO, solar = FALSE), 'gg')
  expect_s3_class(sfn_plot(FOO, type = 'env'), 'gg')
  expect_s3_class(sfn_plot(FOO, type = 'ta'), 'gg')
  expect_s3_class(sfn_plot(FOO, type = 'vpd'), 'gg')
  expect_s3_class(sfn_plot(FOO, formula = ~vpd), 'gg')
  expect_s3_class(sfn_plot(FOO, formula = ~ext_rad), 'gg')
  
})