\name{paracou16}
\docType{data}
\alias{paracou16}
\title{
  Paracou field station plot 16, partial map
}
\description{
  This point pattern is from Paracou field station, French Guiana, managed by \href{https://www.cirad.fr}{Cirad}.
}
\usage{
data(paracou16)
}
\format{
  An object of class \code{\link{ppp.object}} representing the point pattern of tree locations in a 250 x 300 meter sampling region. Each tree is marked with its species ("Q. Rosea", "V. Americana" or "Other"), and basal area (square centimeters).
}

\source{
  Permanent data census of Paracou and Marcon et al. (2012).
}
\references{
  Gourlet-Fleury, S., Guehl, J. M. and Laroussinie, O., Eds. (2004). \emph{Ecology & management of a neotropical rainforest. Lessons drawn from Paracou, a long-term experimental research site in French Guiana}. Paris, Elsevier.
  
  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. \emph{International Journal of Ecology} 2012(Article ID 619281): 11.
}
\examples{
data(paracou16)
# Plot (second column of marks is Point Types) 
plot(paracou16, which.marks=2, leg.side="right")

}
\keyword{datasets}
