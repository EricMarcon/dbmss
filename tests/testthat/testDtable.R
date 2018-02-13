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


# Check envelope.Dtable and conversion from data.frame
testthat::test_that("The envelope calculated for a Dtable is the same as that of a wmppp", {
  testthat::skip_on_cran()
  
  data("paracou16")
  # Make a data.frame for tests
  df <- data.frame(x=paracou16$x, y=paracou16$y, 
                   PointWeight=paracou16$marks$PointWeight, 
                   PointType=paracou16$marks$PointType)
  # Test as.wmppp.data.frame 
  testthat::expect_true(is.wmppp(as.wmppp(df)))
  # Test as.Dtable.data.frame
  DTparacou <- as.Dtable(df)
  r <- c(0, 30)
  # Menvelope calculated from DTable and wmppp
  MDT <- MEnvelope(DTparacou, r, ReferenceType = "V. Americana")
  Mwmppp <- MEnvelope(paracou16, r, ReferenceType = "V. Americana")
  MDelta <- (MDT-Mwmppp)/MDT
  # Check that the difference is less than 10%
  testthat::expect_lt(max(MDelta[2,]), .1)
})
