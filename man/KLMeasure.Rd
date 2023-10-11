\name{KLMeasure}
\alias{KLMeasure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank-based smoothed precision/recall measure for projection.
}
\description{
Computes rank-based smoothed precision/recall, with cost function based on Kullback-Leibler-divergence (see [Venna2010]).
}
\usage{
KLMeasure(Data, pData, NeighborhoodSize = 20L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
numerical matrix of data: n cases in rows, d variables in columns
}
  \item{pData}{
numerical matrix of projected data: n cases in rows, k variables in columns, where k is the projection output dimension
}
  \item{NeighborhoodSize}{
Number of points in neighborhood to be considered. Default is 20
}
}

\value{
  \item{SmoothedPrecision}{
Scalar, smoothed precision value
}

  \item{SmoothedRecall}{
Scalar, smoothed recall value
}

}
\references{
[Venna2010]: Jarkko Venna, Jaakko Peltonen, Kristian Nybo, Helena Aidos, and Samuel Kaski. Information Retrieval Perspective to Nonlinear Dimensionality Reduction for Data Visualization. Journal of Machine Learning Research, 11:451-490, 2010.
}
\author{
Michael Thrun
}

\seealso{
An alternative measure is the \link{ContTrustMeasure}
}

\note{
C++ source code comes from \url{https://research.cs.aalto.fi/pml/software/dredviz/}
}

\examples{
data('Hepta')
Data=Hepta$Data
res=MDS(Data)
Proj = res$ProjectedPoints

kl_m = KLMeasure(Hepta$Data, Proj)
# Smoothed precision
print(kl_m[[1]])
# Smoothed recall
print(kl_m[[2]])

}
\keyword{Measure}
\keyword{Projection}
\keyword{Kullback-Leibler-divergence}
\keyword{Precision}
\keyword{Recall}
