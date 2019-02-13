context("helpers")

test_that("describe_md_variable works", {
  expect_output(describe_md_variable('pl_sens_meth'), 'Type:')
  expect_output(describe_md_variable('st_age'), 'Values:')
  expect_output(describe_md_variable('si_lat'), 'Units:')
  expect_output(describe_md_variable('sp_leaf_habit'), 'Description:')
  expect_output(describe_md_variable('env_vpd'), 'Type:')
})
