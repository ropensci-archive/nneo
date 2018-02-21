context("nneo_wrangle")

test_that("nneo_data works as expected", {
  skip_on_cran()

  aa <- nneo_wrangle(site_code="STER",time_start="2017-03-04",
                     data_var="temperature",time_agr=30)

  expect_is(aa, 'tbl_df')
  # expect_is(aa$difRadMean.000.060, 'numeric')
  expect_is(aa$tempTripleMean.000.040, 'numeric')
  expect_is(aa$startDateTime, 'character')

})
