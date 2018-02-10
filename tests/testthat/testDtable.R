testthat::context("Dtable")

# Three points at distance 1 from each other
Dmatrix <- matrix(c(0,1,1, 1,0,1, 1,1,0), nrow=3)
PointType <- c("Type1", rep("Type2", 2))
PointWeight <- 1:3
Dtable(Dmatrix, PointType, PointWeight) -> dt

# Check rRandomLocation and rPopulationIndependenceM 
testthat::test_that("The average distance between points is not changed by randomization", {
  testthat::skip_on_cran()
  rRL <- rRandomLocation(dt)
  rPI <- rPopulationIndependenceM(dt, "Type1")
  testthat::expect_equal(mean(rRL$Dmatrix), mean(rPI$Dmatrix))
})
