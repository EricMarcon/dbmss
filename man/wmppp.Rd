\name{wmppp}
\alias{wmppp}
\title{
  Create a Weighted, Marked, Planar Point Pattern
}
\description{
  Creates an object of class \code{"wmppp"} representing a two-dimensional point pattern with weights and labels.
}
\usage{
wmppp(df, window = NULL, unitname = NULL)
}
\arguments{
  \item{df}{
  A dataframe with at least two columns containing point coordinates.  
  }
  \item{window}{
  An object of calls "owin" (\code{\link{owin.object}}).  
  }
  \item{unitname}{
  Name of unit of length. Either a single character string, or a vector of two character strings giving the singular and plural forms, respectively. Ignored if \code{window} is not \code{NULL}.
  }
}
\details{
  Columns named "X", "Y", "PointType", "PointWeight" (capitalization is ignored) are searched to build the \code{"wmppp"} object and set the point coordinates, type and weight. If they are not found, columns are used in this order. If columns are missing, \code{PointType} is set to "All" and \code{PointWeight} to 1.
  If a "PointName" column is found, it is used to set the row names of the marks, else the original row names are used.
  
  If the window is not specified, a rectangle containing all points is used, and \code{unitname} is used.
}
\value{
  An object of class \code{"wmppp"}.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{wmppp.object}},
}
\examples{
# Draw the coordinates of 10 points
X <- runif(10)
Y <- runif(10)
# Draw the point types.
PointType   <- sample(c("A", "B"), 10, replace=TRUE)
# Plot the point pattern. Weights are set to 1 ant the window is adjusted.
plot(wmppp(data.frame(X, Y, PointType)), , which.marks=2)
}
