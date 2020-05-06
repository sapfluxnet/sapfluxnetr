data("ARG_TRE", package = 'sapfluxnetr')
data("ARG_MAZ", package = 'sapfluxnetr')
data("AUS_CAN_ST2_MIX", package = 'sapfluxnetr')
multi_sfn <- sfn_data_multi(ARG_TRE, ARG_MAZ, AUS_CAN_ST2_MIX)

test_that('sfn_plot returns the object correctly', {

  expect_s3_class(sfn_plot(ARG_TRE), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, solar = FALSE), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'env'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'ta'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, type = 'vpd'), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, formula_env = ~vpd), 'gg')
  expect_s3_class(sfn_plot(ARG_TRE, formula_env = ~ext_rad), 'gg')

})

test_that('sfn_plot returns the object correctly when multi', {

  multi_plot <- sfn_plot(multi_sfn)
  expect_true(is.list(multi_plot))
  expect_s3_class(multi_plot[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_2 <- sfn_plot(multi_sfn, solar = FALSE)
  expect_true(is.list(multi_plot_2))
  expect_s3_class(multi_plot_2[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_2[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_2[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_3 <- sfn_plot(multi_sfn, type = 'env')
  expect_true(is.list(multi_plot_3))
  expect_s3_class(multi_plot_3[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_3[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_3[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_4 <- sfn_plot(multi_sfn, type = 'ta')
  expect_true(is.list(multi_plot_4))
  expect_s3_class(multi_plot_4[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_4[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_4[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_5 <- sfn_plot(multi_sfn, type = 'vpd')
  expect_true(is.list(multi_plot_5))
  expect_s3_class(multi_plot_5[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_5[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_5[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_6 <- sfn_plot(multi_sfn, formula_env = ~vpd)
  expect_true(is.list(multi_plot_6))
  expect_s3_class(multi_plot_6[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_6[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_6[['AUS_CAN_ST2_MIX']], 'gg')

  multi_plot_7 <- sfn_plot(multi_sfn, formula_env = ~ext_rad)
  expect_true(is.list(multi_plot_7))
  expect_s3_class(multi_plot_7[['ARG_TRE']], 'gg')
  expect_s3_class(multi_plot_7[['ARG_MAZ']], 'gg')
  expect_s3_class(multi_plot_7[['AUS_CAN_ST2_MIX']], 'gg')

})
