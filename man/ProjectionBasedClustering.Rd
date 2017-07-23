\name{ProjectionBasedClustering}
\alias{ProjectionBasedClustering}
\title{
automated Clustering approach of the Databonic swarm with abstact U distances
}
\description{
automated Clustering approach of the Databonic swarm with abstact U distances, which
are the geodesic distances based on high-dimensional distances combined with low dimensional graph paths by using \code{ShortestGraphPathsC}.
}
\usage{
ProjectionBasedClustering(k, Data, BestMatches, LC, StructureType = TRUE, PlotIt = FALSE,
                 method = "euclidean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{number of clusters, how many to you see in the 3d landscape?}
  \item{Data}{[1:n,1:d] Matrix of Data (n cases, d dimensions) that will be used. One DataPoint per row}
  \item{BestMatches}{[1:n,1:2] Matrix with positions of Bestmatches=ProjectedPoints, one matrix line per data point}
  \item{LC}{grid size c(Lines,Columns)}
  \item{StructureType}{Optional, bool; =TRUE: compact structure of clusters assumed, =FALSE: connected structure of clusters assumed. For the two options vor Clusters, see [Thrun, 2017] or Handl et al. 2006}
  \item{PlotIt}{Optional, bool, Plots Dendrogramm}
  \item{method}{Optional, distance method,  do not change}
}
\details{
ProjectionBasedClustering is a flexible and robust clustering framework based on a chose projection method and
 projection method a parameter-free high-dimensional data visualization technique,
 which generates projected points on a topographic map with hypsometric colors, see package GeneralizedUmatrix function GeneralizedUmatrix, 
 called the generalized U-matrix. The clustering method with no 
 sensitive parameters is done by this function. The clustering can be verified by the visualization and vice versa. 
}
\value{
Cls                 [1:n] vector with selected classes of the bestmatches. You can use \code{plotTopographicMap(Umatrix,Bestmatches,Cls)} for verification.
}
\examples{
data('Hepta')
#2d projection
\dontrun{projectionpoints=NeRV(Hepta$Data)} 
#Computation of Generalized Umatrix
\dontrun{visualization=GeneralizedUmatrix(Data = Hepta$Data,projectionpoints)}
# Visualizuation of GenerelizedUmatrix
\dontrun{plotTopographicMap(visualization$Umatrix,visualization$Bestmatches)}
# Automatic Clustering
\dontrun{LC=c(visualization$Lines,visualization$Columns)}
# number of Cluster from dendrogram or visualization (PlotIt=T)
\dontrun{#Cls=ProjectionBasedClustering(k=7, Hepta$Data, 

visualization$Bestmatches, LC,PlotIt=T)}
# Verification
\dontrun{plotTopographicMap(visualization$Umatrix,visualization$Bestmatches,Cls)}
}
\references{
[Thrun/Ultsch, 2017] Thrun, M.C., Ultsch, A.: Projection based Clustering, accepted for publication at Conf. Int. Federation of Classification Societies, Tokyo, 2017.

[Thrun, 2017]   Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2017.
}
\author{
Michael Thrun
}
\note{
If you used pswarm with distance matrix instead of a data matrix you can mds transform your distances into data. The correct dimension can be found through the Sheppard diagram or kruskals stress.

Often it is better to mark the outliers  manually after the prozess of clustering; use in this case the visualization \code{plotTopographicMap} of the package GeneralizedUmatrix. If you would like to mark the outliers interactivly in the visualization use the Umatrix package in \url{https://www.uni-marburg.de/fb12/datenbionik/software-en}
}
\keyword{swarm}
\keyword{Databonic swarm}
\keyword{clustering}% __ONLY ONE__ keyword per line
\keyword{cluster analysis}% __ONLY ONE__ keyword per line
\keyword{cluster}% __ONLY ONE__ keyword per line