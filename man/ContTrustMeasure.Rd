\name{ContTrustMeasure}
\alias{ContTrustMeasure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
continuity and trustworthiness}
\description{
Computes trustworthiness and continuity for projected data (see [Kaski2003]).
}
\usage{
ContTrustMeasure(datamat, projmat, lastNeighbor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datamat}{
numerical matrix of data: n cases in rows, d variables in columns
}
  \item{projmat}{
numerical matrix of projected data: n cases in rows, k variables in columns, where k is the projection output dimension
}
  \item{lastNeighbor}{
scalar, maximal size of neighborhood to be considered}
}
\details{
This is a wrapper that is used in the \pkg{DRquality} to
investigate varius quality measurements [Thrun et al, 2023]. The paper indicates, that the Gabriel classification error seems to be a good alternative. [Thrun et al, 2023].
}
\note{
C++ source code comes from \url{https://research.cs.aalto.fi/pml/software/dredviz/}
}

\value{
numerical [k,7] matrix, where k is the lastNeighbor value.
The matrix contains the columns:

Neighborhood size; worst-case trustworthiness; average trustworthiness; best-case trustworthiness; worst-case continuity; average continuity; best-case continuity

where neighborhood size is the size of the neighberhood considered, which ranges from 1:lastNeighbor
}
\references{
[Kaski2003]: Samuel Kaski, Janne Nikkilä, Merja Oja, Jarkko Venna, Petri Törönen, and Eero Castren. Trustworthiness and metrics in visualizing similarity of gene expression. BMC Bioinformatics, 4:48, 2003.
}
\author{
Luca Brinkmann, Felix Pape
}


\seealso{
For plotting see \code{plotMeasureTundD} in the package \pkg{DRquality}. An alternative measure is the \link{KLMeasure}, see also \link[DRquality]{GabrielClassificationError}
}

\examples{
data('Hepta')
Data=Hepta$Data
res=MDS(Data)
Proj = res$ProjectedPoints
ContTrustMeasure(Hepta$Data, Proj, 10)
}

\keyword{Measure}
\keyword{Projection}
\keyword{Trustworthiness}
\keyword{Continuity}
