interactiveGeneralizedUmatrixIsland <- function(Umatrix, Bestmatches=NULL,
                                                Cls=NULL, Plotter="plotly",NoLevels=NULL){
  # Imx = interactiveGeneralizedUmatrixIsland(Umatrix, Bestmatches, Cls)
  #
  # INPUT
  # Umatrix(1:Lines,1:Columns)	          Umatrix to be plotted
  # Bestmatches(1:n,1:2)		              Positions of Bestmatches to be plotted onto the Umatrix
  # Cls(1:n)			                        Class identifier for the bestmatch at the given point
  # NoLevels                              number of contour lines in topographic map that will be done, lower number results in faster plotting
  #                                       NULL: uses default values, for plotly the defuult is 15, for ggplot2 the default is round(maxU2/max(minU2,0.05),0)
  # Plotter                               Choose between plotting frameworks: "plotly" and "ggplot2"
  # OUTPUT
  # Imx(1:2*Lines,1:2*Columns)            the 4-tiled umatrix can be cut out by the imx to an island
  #                                       one: outside, zero=inside island
  # Author: FL, MT, QS
  outputApp = NULL
  outplot=NULL
  if(Plotter == "ggplot2"){
    outputApp = interactiveGeneralizedUmatrixIsland_ggplot(Umatrix, Bestmatches=Bestmatches, Cls=Cls,NoLevels=NoLevels)
  } else {
    #uses
    #helperTopographicIsland=GeneralizedUmatrix::helperTopographicIsland
    #todo: change to TopviewTopographicMap
    outputApp = interactiveGeneralizedUmatrixIsland_plotly(Umatrix, Bestmatches=Bestmatches, Cls=Cls,NoLevels=NoLevels)
  }
  return(outputApp)
}
