testthat::context("wmppp")

testthat::test_that("A Weighted, Marked Point Pattern can be created with a cropped window and labels.", {
  testthat::skip_on_cran()
  # Create 100 coordinates of points between 0 and 2
  x <- runif(100)*2
  y <- runif(100)*2
  df <- data.frame(x, y)
  # Label the points starting from 101
  rownames(df)  <-  101:200
  # Create a point pattern retaining point in the unit square window
  wp <- wmppp(df, window = owin())
  # The label of the first point must be greater than 100
  testthat::expect_gt(as.numeric(row.names(wp$marks)[1]), 100)
})