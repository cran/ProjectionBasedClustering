\name{interactiveProjectionBasedClustering}
\alias{interactiveProjectionBasedClustering}
\alias{IPBC}

\title{
Interactive Projection-Based Clustering (IPBC)
}
\description{
An interactive clustering tool published in [Thrun et al., 2020] that uses the topographic map visualizations of the generalized U-matrix and a variety of different projection methods. This function receives a dataset and starts a shiny interface where one is able to choose a projection method and generate a plot.ly visualization of the topograhpic map [Thrun et al., 2016] of the generalized U-matrix [Ultsch/Thrun, 2017] combined with projected points. It includes capabilities for interactive clustering within the interface as well as automatic projection-based clustering based on [Thrun/Ultsch, 2020].
}
\usage{
  interactiveProjectionBasedClustering(Data, Cls=NULL)
  
  IPBC(Data, Cls=NULL)
}
\arguments{
  \item{Data}{
    The dataset [1:n,1:d] of n cases and d vriables with which the U-matrix and the projection will be calculated. Please see also the note below.
  }
  \item{Cls}{
     Optional: Prior Classification of the data for the [1:n] cases of k classes.
  }
}
\details{
To cluster data interactively, i.e., select specific data points and create a cluster), first generate the visualization. Thereafter, switch in the menu to clustering, hold the left mouse button and then frame a valley. Simple mouse clicks will not start the lasso functionality of plotly. 

The resulting clustering is stored in Cls which is a numerical vector of the length n (number of cases) with the integer elements of numbers from 1 to k if k is the number of groups in the data.
Each element of Cls as an unambigous mapping to a case of Data indicating by the rownames of Data. If Data has no rownames a vector from 1:n is generated and then Cls is named by it.
}
\value{
Returns a List of:
  \item{Cls}{[1:n] numerical vector of the clustering of the dataset for then cases of k clusters}
  \item{Umatrix}{
 [1:Lines,1:Columns] generalized Umatrix to be plotted, numerical matrix storing the U-heights, see [Thrun, 2018] for definition.
}
\item{Bestmatches}{
[1:n,2]   Matrix of GridConverted Projected Points [1:n, 1:2] called Bestmatches that defines positions for n datapoints, first columns is the position in \code{Lines} and second column in \code{Columns}
}

\item{LastProjectionMethodUsed}{name of last projection method that was used as a string}
  \item{TopView_TopographicMap}{The final plot generated by plot.ly when closing the tool}
}
\author{
Tim Schreier, Felix Pape, Luis Winckelmann, Michael Thrun
}

\references{
[Ultsch/Thrun, 2017]  Ultsch, A., & Thrun, M. C.: Credible Visualizations for Planar Projections, in Cottrell, M. (Ed.), 12th International Workshop on Self-Organizing Maps and Learning Vector Quantization, Clustering and Data Visualization (WSOM), IEEE Xplore, France, 2017.

[Thrun/Ultsch, 2017]   Thrun, M. C., & Ultsch, A. : Projection based Clustering, Proc. International Federation of Classification Societies (IFCS), pp. 250-251, Japanese Classification Society (JCS), Tokyo, Japan, 2017. 

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, Springer, DOI: 10.1007/s00357-020-09373-2, 2020. 

[Thrun et al., 2020]  Thrun, M. C., Pape, F., & Ultsch, A.: Interactive Machine Learning Tool for Clustering in Visual Analytics, 7th IEEE International Conference on Data Science and Advanced Analytics (DSAA 2020), pp. 672-680, DOI 10.1109/DSAA49011.2020.00062, IEEE, Sydney, Australia, 2020.

}

\examples{
if(interactive()){
  data('Hepta')
  Data=Hepta$Data

  V=interactiveProjectionBasedClustering(Data)

  # with prior classification
  Cls=Hepta$Cls
  V=IPBC(Data,Cls)
}
}
\note{
Some dimensionality reduction methods will assume data without missing values, some other DR methods assume unique data points, i.e., no distance=0 for any two cases(rows) of data.
In these cases the IPBC method will crash.
}
\concept{interactive Projection Based Clustering}
\concept{Projection Method}
\concept{Dimensionality Reduction}
\keyword{DR}
\keyword{IPBC}
\keyword{Clustering}
\concept{Cluster Analysis}
