context("nneo_wrangle")

test_that("nneo_data works as expected", {
  skip_on_cran()

  aa <- nneo_wrangle(site_code="BART", time_start="2016-06-20",
                     time_end="2016-09-21", data_var="radiation")

  expect_is(aa, 'data.frame')
  expect_is(aa[,grep(".*Mean.*",names(aa))[1]], 'numeric')
  expect_is(aa[,1], 'character')

})
