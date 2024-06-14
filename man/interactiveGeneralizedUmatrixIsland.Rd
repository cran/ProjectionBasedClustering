\name{interactiveGeneralizedUmatrixIsland}
\alias{interactiveGeneralizedUmatrixIsland}
\title{GUI for cutting out an Island.}

\usage{
interactiveGeneralizedUmatrixIsland(Umatrix, Bestmatches=NULL,

Cls=NULL, Plotter="plotly",NoLevels=NULL)
}

\description{The toroid Umatrix is usually drawn 4 times, so that connected areas on borders can be seen as a whole. An island is a manual cutout of such a tiled visualization, that is selected such that all connected areas stay intact. This shiny tool allows the user to do this manually.}

\arguments{
\item{Umatrix}{[1:Lines,1:Columns] Matrix of Umatrix Heights
}
\item{Bestmatches}{[1:n, 1:2] Matrix with positions of Bestmatches for n
datapoints, first columns is the position in \code{Lines} and second column in
\code{Columns}
}
\item{Cls}{Classification of the Bestmatches
}
\item{Plotter}{Choose between plotting frameworks: "plotly" and "ggplot2"
}
\item{NoLevels}{number of contour lines in topographic map that will be done.

\code{NULL}: uses default values, for plotly the defuult is 15, for ggplot2 the default is \code{round(maxU2/max(minU2,0.05),0)}
}
}
\value{[1:2*Lines,1:2*Columns] Boolean Matrix that represents the island within the tiled Umatrix. Zeros mark the inside and ones the outside of the island.}
\details{
The Imx is a matrix that overlays the 4-tiled (generalized) Umatrix to define an island within the four tiles. The Umatrix is computed first 4 times (i.e. within 4 tiles) to account for border effects. Then zeros mark which part of the Umatrix shall be shown to the user
as a topogrpahic map and ones change the Umatrix values to zeros which will be visualized as an ocean. The result is an borderless island of high-dimensional structures. Usually the goal is to cut out the island in a way that mountain ranges define the borders of the island.

\code{NoLevels} also influences the number of colors used in the topographic map. 
In general, a lower number will result in faster plotting and therefore improve interactivity but lower the number of details that are visible.

Clicking on "Quit" returns the Imx matrix to the workspace. Details can bee read in [Thrun et al, 2016, Thrun/Ultsch, 2017].
}
\examples{
data("Hepta")
Data=Hepta$Data
Cls=Hepta$Cls
InputDistances=as.matrix(dist(Data))
res=cmdscale(d=InputDistances, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
ProjectedPoints=as.matrix(res$points)
#see also ProjectionBasedClustering package for other common projection methods
\donttest{
library(GeneralizedUmatrix)
resUmatrix=GeneralizedUmatrix(Data,ProjectedPoints)
TopviewTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches,Cls)
#or in 3D if rgl package exists
#plotTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches,Cls)
}
##Interactive Island Generation 
## from a tiled Umatrix (toroidal assumption)

\dontrun{
	Imx = interactiveGeneralizedUmatrixIsland(resUmatrix$Umatrix,

	resUmatrix$Bestmatches)
	plotTopographicMap(resUmatrix$Umatrix,

	resUmatrix$Bestmatches, Imx = Imx)
}
}

\author{Michael Thrun, Quirin Stier}

\references{
[Thrun, et al.,2016] 	Thrun, M. C., Lerch, F., Loetsch, J., Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision,Plzen, 2016. 

[Thrun/Ultsch, 2017]	Thrun, M.C., Ultsch, A.: Projection based Clustering, Conf. Int. Federation of Classification Societies (IFCS),<DOI:10.13140/RG.2.2.13124.53124>, Tokyo, 2017.

}
