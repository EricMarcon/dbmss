context("Kd")

# Load Paracou data (a tropical forest tree point pattern)
data(paracou16)

# Check Kd ppp equals Kd Dtable
testthat::test_that("Kd is unchanged with point pattern or distance table", {
  testthat::skip_on_cran()
  testthat::expect_equal(as.numeric(Kdhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana")$Kd),
               as.numeric(Kdhat(as.Dtable(paracou16), r=c(0, 100) , "Q. Rosea", "V. Americana")$Kd),
               tolerance = 1e-6)
})


# Check Kemp ppp equals Kemp Dtable
testthat::test_that("Kemp is unchanged with point pattern or distance table", {
  testthat::skip_on_cran()
  testthat::expect_equal(as.numeric(Kdhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana", Weighted = T)$Kd),
               as.numeric(Kdhat(as.Dtable(paracou16), r=c(0, 100) , "Q. Rosea", "V. Americana", Weighted = T)$Kd),
               tolerance = 1e-6)
})

# Check Kd and Kemp
testthat::test_that("Kd is different from Kemp", {
  testthat::skip_on_cran()
  testthat::expect_equal(as.numeric(Kdhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana", Weighted = T)$Kd + c(1.411929e-04, 7.733467e-04)),
               as.numeric(Kdhat(paracou16, r=c(0, 100) , "Q. Rosea", "V. Americana", Weighted = F)$Kd),
               tolerance = 1e-6)
})

# Check Kd sums to 1
testthat::test_that("Kd sums to 1 when all distances are kept", {
  testthat::skip_on_cran()
  testthat::expect_equal(sum(Kdhat(paracou16, r=0:300 , "")$Kd),
               1, 
               tolerance = 1e-2)
})
