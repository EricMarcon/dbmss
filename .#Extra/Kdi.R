data(paracou16)
X <- paracou16
ReferenceType <- "V. Americana"
NeighborType <- "V. Americana"
plot(X[X$marks$PointType=="Q. Rosea"])

# Vectors to recognize point types
IsReferenceType <- X$marks$PointType==ReferenceType
IsNeighborType <- X$marks$PointType==NeighborType
# Eliminate useless points
X <- X[IsReferenceType | IsNeighborType]
# Compute the matrix of distances
Dist <- pairdist.ppp(X)


# Reduce the matrix to pairs of interest
IsReferenceType <- X$marks$PointType==ReferenceType
IsNeighborType <- X$marks$PointType==NeighborType
Dist <- Dist[IsReferenceType, IsNeighborType]

Weights <- matrix(rep(X$marks$PointWeight, each=X$n), nrow=X$n)
Weights <- Weights[IsReferenceType, IsNeighborType]
# Set self point pair weight equal to 0 or density will be erroneous
if (NeighborType == ReferenceType) {
  diag(Weights) <- 0
}

# Choose r
r <- seq(0, 100, 10)
# Prepare a matrix for the results: each line is a for a point
Kdi <- matrix(nrow=dim(Dist)[1], ncol=length(r))
for (i in 1:dim(Dist)[1]) {
  # Get neighbor weights
  w <- Weights[i,]
  # Calculate the density of neighbors. bw="nrd0" is D&O’s choice in their paper.
  Density <- density(Dist[i,], weights=w/sum(w), bw="nrd0", from=0)
  # Interpolate results at the chosen r
  Kdi[i,] <- stats::approx(Density$x, Density$y, xout=r)$y
}


# Check
plot(Kdhat(as.wmppp(X), r, ReferenceType, NeighborType, Weighted=TRUE))
points(x=r, y=apply(Kdi, 2, mean))
