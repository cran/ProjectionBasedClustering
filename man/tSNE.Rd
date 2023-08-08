\name{tSNE}
\alias{tSNE}
\title{T-distributed Stochastic Neighbor Embedding (t-SNE)}
\usage{
tSNE(DataOrDistances,k,OutputDimension=2,Algorithm='tsne_cpp',

method="euclidean",Whitening=FALSE, Iterations=1000,PlotIt=FALSE,Cls,num_threads=1,\dots)
}
\arguments{
\item{DataOrDistances}{
Numerical matrix defined as either 

\code{Data}, i.e., [1:n,1:d], nonsymmetric, and consists of n cases of d-dimensional data points with every case having d attributes, variables or features,

or

\code{Distances}, i.e.,[1:n,1:n], symmetric and consists of n cases, e.g., \code{as.matrix(dist(Data,method))}
}
\item{k}{number of k nearest neighbors=number of effective nearest neighbors("perplexity"); Important parameter. If not given, settings of packages of t-SNE will be used depending \code{Algorithm}}
  \item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
	\item{Algorithm}{
	'tsne_cpp': T-Distributed Stochastic Neighbor Embedding using a Barnes-HutImplementation in C++ of \pkg{Rtsne}. Requires Version >= 0.15 of \pkg{Rtsne} for multicore parallelisation.
	
	'tsne_opt_cpp': T-Distributed Stochastic Neighbor Embedding with automated optimized parameters using a Barnes-HutImplementation in C++.
	
  'tsne_r': pure R implementation of the t-SNE algorithm of of \pkg{tsne}
}
\item{method}{	                    method specified by distance string: 
                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
}
\item{Whitening}{A boolean value indicating whether the matrix data should be whitened (tsne_r) or if pca should be used priorly 
(tsne_cpp)}
	
	\item{Iterations}{ maximum number of iterations to perform.}

	\item{PlotIt}{
		Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
		OutputDimension>2: only the first two dimensions will be shown
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
\item{num_threads}{
Number of threads for parallel computation, only usable for Algorithm='tsne_cpp' or 'tsne_opt_cpp'
}

\item{\dots}{
Further arguments passed on to either 'Rtsne' or 'tsne' 
}
}
\value{
List of 
 \item{ProjectedPoints}{[1:n,OutputDimension], n by OutputDimension matrix containing coordinates of the Projection}
  \item{ModelObject}{NULL for tsne_r, further information if tsne_cpp is selected }
}
\description{
  T-distributed Stochastic Neighbor Embedding   res = tSNE(Data, KNN=30,OutputDimension=2) 
	}
 
 \note{
A wrapper for \code{\link[Rtsne]{Rtsne}} (Algorithm='tsne_cpp'),

\href{https://github.com/omiq-ai/Multicore-opt-SNE}{Multicore-opt-tSNE} (Algorithm='tsne_opt_cpp'),

or  for \code{\link[tsne]{tsne}} (Algorithm='tsne_r')

 You can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}

\references{
Anna C. Belkina, Christopher O. Ciccolella, Rina Anno, Josef Spidlen, Richard Halpert, Jennifer Snyder-Cappione. Automated optimal parameters for T-distributed stochastic neighbor embedding improve visualization and allow analysis of large datasets (2018) bioRxiv 451690; doi: https://doi.org/10.1101/451690

L.J.P van der Maaten. Accelerating t-SNE using tree-based algorithms. Journal of Machine Learning Research 15.1:3221-3245, 2014.
}

 \details{
 An short overview of different types of projection methods can be found in [Thrun, 2018, p.42, Fig. 4.1] (\doi{10.1007/978-3-658-20540-9}).
 }
\examples{
data('Hepta')
Data=Hepta$Data

\dontrun{
Proj=tSNE(Data,k=7)

PlotProjectedPoints(Proj$ProjectedPoints,Hepta$Cls)
}
\dontshow{
Proj=tSNE(Data[1:4,],k = 1,Iterations=1,Method="tsne_r")
}
}
\author{Michael Thrun}

\keyword{tSNE}
\concept{t-SNE}
\concept{T-distributed Stochastic Neighbor Embedding}
 \concept{Projection Method}
\concept{Dimensionality Reduction}
\keyword{DR}