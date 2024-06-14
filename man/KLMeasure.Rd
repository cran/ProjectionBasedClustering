\name{KLMeasure}
\alias{KLMeasure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smoothed Precision and Recall
}
\description{
Computes the quality measurement of rank-based smoothed precision an recall, with cost function based on Kullback-Leibler-divergence (see [Venna2010]) used to evaluated dimensionality reduction methods.
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

[Thrun et al, 2023] Thrun, M.C, Märte, J., Stier, Q.: Analyzing Quality Measurements for Dimensionality Reduction, Machine Learning and Knowledge Extraction (MAKE), Vol 5., accepted, 2023.

}
\author{
Luca Brinkmann, Felix Pape
}

\seealso{
An alternative measure is the \link{ContTrustMeasure}, see also \link[DRquality]{GabrielClassificationError}
}

\details{
This is a wrapper that is used in the \pkg{DRquality} to
investigate varius quality measurements [Thrun et al, 2023]. The paper indicates, that the Gabriel classification error seems to be a good alternative. [Thrun et al, 2023].
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
