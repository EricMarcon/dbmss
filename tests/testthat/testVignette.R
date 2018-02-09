testthat::context("Vignette")

testthat::test_that("The vignette code is run without errors.", {
  testthat::skip_on_cran()
  testthat::expect_true(packagedocs::build_vignettes())
})