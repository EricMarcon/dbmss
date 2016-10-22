\name{CheckdbmssArguments}
\alias{CheckdbmssArguments}
\title{
  Checks the arguments of a function of the package dbmss
}
\description{
  This function is used internally to verify that arguments passed to dbmss functions such as \code{\link{Mhat}} are correct.
}
\usage{
CheckdbmssArguments()
}
\value{
  Returns \code{TRUE} or stops if a problem is detected.
}
\details{
  The function compares the arguments passed to its parent function to the type they should be and performs some extra tests (\emph{e.g.} risk threshold must be between 0 and 1). It stops if an argument is not correct.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}


\keyword{internal}
