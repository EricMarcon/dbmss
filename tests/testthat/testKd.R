testthat::context("Kd")

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


# Check Kd sums to 1
testthat::test_that("Kd sums to 1 when all distances are kept", {
  testthat::skip_on_cran()
  testthat::expect_equal(sum(Kdhat(paracou16, r=0:300 , "")$Kd),
               1, 
               tolerance = 1e-2)
})

# Check Kd ppp equals Kd approximated when only two values of r
testthat::test_that("Kd approximated is close to Kd (10% error allowed)", {
  testthat::skip_on_cran()
  # Kd exact
  kdExact <- Kdhat(paracou16, r=0:300, "Q. Rosea", "V. Americana")$Kd
  # Kd approximated
  kdApprox <- Kdhat(paracou16, r=0:300, "Q. Rosea", "V. Americana", Approximate=4)$Kd
  # Difference
  kdDiff <- sum(abs(kdExact-kdApprox))
  testthat::expect_lt(kdDiff, sum(kdExact)/10)
})
