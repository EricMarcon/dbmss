testthat::context("M")

# Load Paracou data (a tropical forest tree point pattern)
data(paracou16)

# Check M ppp equals M Dtable
testthat::test_that("M is unchanged with point pattern or distance table", {
  testthat::skip_on_cran()
  testthat::expect_equal(as.numeric(Mhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana")$M),
               as.numeric(Mhat(as.Dtable(paracou16), r=c(0, 100) , "Q. Rosea", "V. Americana")$M),
               tolerance = 1e-6)
})

# Check m ppp equals m Dtable
testthat::test_that("m is unchanged with point pattern or distance table", {
  testthat::skip_on_cran()
  testthat::expect_equal(as.numeric(mhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana")$m),
               as.numeric(mhat(as.Dtable(paracou16), r=c(0, 100) , "Q. Rosea", "V. Americana")$m),
               tolerance = 1e-6)
})
