interactiveGeneralizedUmatrixIsland <- function(Umatrix, Bestmatches=NULL,
                                                Cls=NULL, Plotter="plotly"){
  # Imx = interactiveGeneralizedUmatrixIsland(Umatrix, Bestmatches, Cls)
  #
  # INPUT
  # Umatrix
  # Bestmatches
  # Cls
  # Plotter        Choose between plotting frameworks: "plotly" and "ggplot2"
  # OUTPUT
  # island         the generated Imx
  # Author: FL, MT, QS
  outputApp = NULL
  outplot=NULL
  if(Plotter == "ggplot2"){
    outputApp = interactiveGeneralizedUmatrixIsland_ggplot(Umatrix, Bestmatches=Bestmatches, Cls=Cls)
  } else {
    #uses
    #helperTopographicIsland=GeneralizedUmatrix::helperTopographicIsland
    #todo: change to TopviewTopographicMap
    outputApp = interactiveGeneralizedUmatrixIsland_plotly(Umatrix, Bestmatches=Bestmatches, Cls=Cls)
  }
  return(outputApp)
}
