\name{tSNE}
\alias{tSNE}
\title{  T-distributed Stochastic Neighbor Embedding}
\usage{
tSNE(DataOrDists,k,OutputDimension=2,method="euclidean",Whitening=TRUE,

InitialDimensions=NULL, Iterations=1000,PlotIt=FALSE,Cls)
}
\arguments{
\item{DataOrDists}{array of data: n cases in rows, d variables in columns, matrix is not symmetric or distance matrix, in this case matrix has to be symmetric}
\item{k}{number of k nearest neighbors=number of effective nearest neighbors("perplexity") Important parameter, if not given Settings of package t-SNE will be used}
  \item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
	\item{method}{	                    method specified by distance string: 
                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
}
\item{Whitening}{A boolean value indicating whether the matrix data should be whitened}
	
	\item{InitialDimensions}{The number of dimensions to use in reduction method.}
	\item{Iterations}{ maximum number of iterations to perform.}

	\item{PlotIt}{
	Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
	 OutputDimension>2: only the first two dimensions will be shown
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
}
\value{
 ProjectedPoints[1:n,OutputDimension], n by OutputDimension matrix containing coordinates of the Projection
}
\description{
  T-distributed Stochastic Neighbor Embedding   res = tSNE(Data, KNN=30,OutputDimension=2) 
	}
 
 \note{
A wrapper for \code{\link[tsne]{tsne}}

 You can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}
 \details{
 An short overview of different types of projection methods can be found in [Thrun, 2018, p.42, Fig. 4.1] (\url{https://doi.org/10.1007/978-3-658-20540-9}).
 }
 
 \author{Michael Thrun}

