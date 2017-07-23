\name{klrank}
\alias{klrank}
\title{
Computes rank-based smoothed precision/recall
}
\description{
Compares the projection in pData with the original data in Data and calculates the rank-based smoothed recall and precision.
}
\usage{
klrank(Data, pData, NeighborhoodSize)
}
\arguments{
  \item{Data}{Matrix of original data}
  \item{pData}{Matrix of projected data}
  \item{NeighborhoodSize}{Sets the 'effective number of neighbors' used to control the width of the Gaussian}
}
\value{
Rank-based smoothed recall and smoothed precision. It's important to note that all values are the real result substracted from one. So precision_best is (1-precision_best).
}
\references{
Jarkko Venna, Jaakko Peltonen, Kristian Nybo, Helena Aidos, and Samuel Kaski. Information Retrieval Perspective to Nonlinear Dimensionality Reduction for Data Visualization. Journal of Machine Learning Research, 11:451-490, 2010.
}
\author{
Felix Pape
}
