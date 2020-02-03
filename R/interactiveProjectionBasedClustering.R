interactiveProjectionBasedClustering <-
  function(Data, Cls=NULL) {

    # Parameter initialisieren
    Umatrix = NULL 
    bestmatches = NULL
    bmus = NULL 
    mergei = NULL 
    ubmus = NULL 
    udim = NULL 
    uniq = NULL 
    outplot = NULL 
    oubmus = NULL 
    DefaultColorSequence= DefaultColorSequence
    imx<-NA
    
    # Original CLS speichern.
    ClsO=Cls
    
    ## Helper functions ####
    
    # Normalizes the U-Matrix. As used in plotmatrix
    matrixnormalization <- function(Matrix){
      ## MT: Normalization der Umatrix werte
      # Milligan, Copper 1988
      # A Study of Standadization of Variables in Cluster Analysis,
      # robust Normalization Z_5 :"Z_5 is bounded by 0.0 and 1.0
      # with at least one observed value at each of these end points"
      quants <- quantile(as.vector(Matrix), c(0.01, 0.5, 0.99), na.rm = T)
      
      minu <- quants[1]
      maxu <- quants[3]
      # Verhaeltnis zwischen minhoehe/maxHoehe = 1/HeightScale
      
      Matrix <- (Matrix - minu) / (maxu - minu)
      
      #MT: Die Level muessen vor der Begrenzung der Werte
      # auf 0 und 1 gesetz werden, aber nachdem der Wertebereich umgeschoben
      # wurde, damit die Dichte in den Grenzbereichen abgeschetzt werden kann
      indmax <- which(Matrix > 1, arr.ind = T)
      indmin <- which(Matrix < 0, arr.ind = T)
      
      if (length(indmax) > 0)
        Matrix[indmax] <- 1
      if (length(indmin) > 0)
        Matrix[indmin] <- 0
      
      return(Matrix)
    }
    
    
    ## Schritt 0: Check and sanetize input ####
    if (missing(Data))
      stop("No data was given as input")
    
    if (!inherits(Data,"matrix")){
      warning("Input data must be a matrix, assuming data frame and calling as.matrix()")
      Data=as.matrix(Data)
    }
    
    
    Key=rownames(Data)
    if(is.null(Key)){
      warning('"Data" has no rownames. Setting Key to 1:NoOfCases and Naming Output "Cls" accordingly.')
      Key=1:nrow(Data)
      
    }
    ## Schritt 1.9: Prepare GUI things.
    colormap <- c("#3C6DF0", "#3C6DF0", "#3C6DF0", "#006602",
                  "#006A02", "#006D01", "#007101", "#007501", "#007901",
                  "#007C00", "#008000", "#068103", "#118408", "#0B8305",
                  "#17860A", "#1D870D", "#228810", "#288A12", "#2E8B15",
                  "#348D18", "#398E1A", "#3F8F1D", "#45911F", "#4A9222",
                  "#509325", "#569527", "#5C962A", "#61982C", "#67992F",
                  "#6D9A32", "#729C34", "#789D37", "#7E9F39", "#84A03C",
                  "#89A13F", "#8FA341", "#95A444", "#9AA547", "#A0A749",
                  "#A6A84C", "#ACAA4E", "#B1AB51", "#B7AC54", "#BDAE56",
                  "#C3AF59", "#C8B15B", "#CEB25E", "#CBAF5C", "#C8AC59",
                  "#C5A957", "#C3A654", "#C0A352", "#BDA050", "#BA9D4D",
                  "#B7994B", "#B49648", "#B29346", "#AF9044", "#AC8D41",
                  "#A98A3F", "#A6873C", "#A3843A", "#A08138", "#9E7E35",
                  "#9B7B33", "#987830", "#95752E", "#92722B", "#8F6E29",
                  "#8C6B27", "#8A6824", "#876522", "#84621F", "#815F1D",
                  "#7E5C1B", "#7B5918", "#795616", "#765313", "#714E0F",
                  "#6C480B", "#674307", "#6F4D15", "#785822", "#806230",
                  "#896D3E", "#91774C", "#998159", "#A28C67", "#AA9675",
                  "#B3A183", "#BBAB90", "#C3B59E", "#CCC0AC", "#D4CABA",
                  "#DDD5C7", "#E5DFD5", "#E7E1D8", "#E9E4DB", "#EBE6DE",
                  "#ECE8E1", "#EEEAE4", "#F0EDE7", "#F2EFEA", "#F4F1ED",
                  "#F6F4F0", "#F8F6F3", "#F9F8F6", "#FBFAF9", "#FDFDFC",
                  "#FFFFFF", "#FFFFFF", "#FEFEFE", "#FEFEFE", "#FEFEFE",
                  "#FDFDFD", "#FDFDFD", "#FDFDFD", "#FCFCFC", "#FCFCFC",
                  "#FCFCFC", "#FBFBFB", "#FBFBFB", "#FBFBFB", "#FAFAFA",
                  "#FAFAFA", "#FAFAFA", "#F9F9F9", "#F9F9F9", "#FFFFFF",
                  "#FFFFFF")
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      #showticklabels = FALSE,
      showgrid = FALSE
    )
    ay <- list(
      autorange = "reversed",
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      #showticklabels = FALSE,
      showgrid = FALSE
    )
    
    createParams = function (Umatrix, bestmatches, Cls){
      
      udim <<- dim(Umatrix)
      bmus  <<- cbind(bestmatches[, 1], bestmatches[, 2])
      uniq  <<- getUniquePoints(bmus)
      ubmus  <<- uniq$unique
      mergei <<- uniq$mergeind
      if (missing(Cls) || length(Cls) != nrow(bestmatches)){
        Cls <<- rep(1, nrow(ubmus))
      } else {
        Cls <<- Cls[uniq$sortind]
      }
      oubmus <<- cbind(uniq$sortind, uniq$unique[, 1], uniq$unique[, 2])
      
    }
    
    ## Schritt 2: shiny shiny interactive tool ####
    ui <- shinyUI(
      
      tagList(
        ## Script for resizing plotly plots
        tags$head(tags$script('
                              var dimension = [0, 0];
                              $(document).on("shiny:connected", function(e) {
                              var plot = document.getElementsByClassName("col-sm-9")[0];
                              dimension[0] = plot.clientWidth*2;
                              dimension[1] = plot.innerHeight*2;
                              Shiny.onInputChange("dimension", dimension);
                              });
                              $(window).resize(function(e) {
                              var plot = document.getElementsByClassName("col-sm-9")[0];
                              dimension[0] = plot.clientWidth;
                              dimension[1] = plot.innerHeight;
                              Shiny.onInputChange("dimension", dimension);
                              });
                              ')),
        # shinythemes::themeSelector(),
        navbarPage(
          
          theme = shinythemes::shinytheme("flatly"),
          
          "Interactive Projection-based Clustering",
          tabPanel(
            "2D",
            sidebarPanel(tabsetPanel(
              tabPanel(
                "Projection",
                
                tags$h5("Choose Method"),
                selectInput('projections','Projection',choices=c('PCA','CCA','ICA','MDS','NeRV','ProjectionPursuit','SammonsMapping','tSNE'),selected='PCA'),
                
                # Further Parameter-Querys for Projections
                #PCA
                
                conditionalPanel(condition ="input.projections=='PCA'", 
                                 checkboxInput("PCAscale", "Scale", value = FALSE, width = NULL)),
                conditionalPanel(condition ="input.projections=='PCA'", 
                                 checkboxInput("PCACenter", "Center", value = FALSE, width = NULL)),
                
                #CCA
                
                conditionalPanel(condition ="input.projections=='CCA'", selectInput(
                  "CCAMethod", "Method",
                  c('euclidean','cosine','chebychev','jaccard','manhattan','binary'))),
                conditionalPanel(condition ="input.projections=='CCA'",
                                 numericInput("CCAEpochs", "Epochs", value=20, min = 1, max = NA, step = 1)),
                conditionalPanel(condition ="input.projections=='CCA'",
                                 numericInput("CCASteps", "initial Stepsize", value=0.5, min = 0.001, max = NA, step = 0.0001)),
                
                
                
                #ICA
                
                conditionalPanel(condition ="input.projections=='ICA'",
                                 numericInput("ICAIterations", "Iterations", value=100, min = 1, max = NA, step = 1)),
                conditionalPanel(condition ="input.projections=='ICA'",
                                 numericInput("ICASteps", "Alpha", value=1, min = 1, max = 2, step = 0.0001)),
                
                #MDS
                
                conditionalPanel(condition ="input.projections=='MDS'", selectInput(
                  "MDSMethod", "Method", selected="euclidan",
                  c('euclidean','maximum','canberra','manhattan'))),
                
                
                
                #NERV
                
                conditionalPanel(condition ="input.projections=='NeRV'",
                                 numericInput("NERVIterations", "Iterations", value=20, min = 1, max = NA, step = 1)),
                conditionalPanel(condition ="input.projections=='NeRV'",
                                 numericInput("NERVLambda", "Lambda", value=0.1, min = 0.0001, max = NA, step = 0.0001)),
                conditionalPanel(condition ="input.projections=='NeRV'",
                                 numericInput("NERVNeighbors", "Neighbors", value=20, min = 1, max = NA, step = 1)),
                
                #ProjectionPursuit
                
                conditionalPanel(condition ="input.projections=='ProjectionPursuit'", selectInput(
                  "PPMethod", "Indexfunction", c('logcosh','exp'))),
                
                conditionalPanel(condition ="input.projections=='ProjectionPursuit'",
                                 numericInput("PPIterations", "Iterations", value=200, min = 1, max = NA, step = 1)),
                conditionalPanel(condition ="input.projections=='ProjectionPursuit'",
                                 numericInput("PPAlpha", "Alpha", value=1, min = 0.0001, max = NA, step = 0.0001)),
                
                #SammonsMapping
                
                conditionalPanel(condition ="input.projections=='SammonsMapping'", selectInput(
                  "SMMethod", "Method",
                  c('euclidean','maximum','canberra','manhattan'))),
                
                #TSNE
                
                
                conditionalPanel(condition ="input.projections=='tSNE'", selectInput(
                  "tSNEMethod", "Method"  , selected = 'logcosh', c('euclidean','maximum','canberra','manhattan'))),
                
                conditionalPanel(condition ="input.projections=='tSNE'",
                                 numericInput("tSNEIterations", "Iterations", value=1000, min = 1, max = NA, step = 1)),
                conditionalPanel(condition ="input.projections=='tSNE'", 
                                 checkboxInput("tSNEWhite", "Whitening", value = FALSE, width = NULL)),
                
                actionButton("generate", HTML("Visual Analytics <br/> with g. U-Matrix")),
                tags$hr(),
                numericInput("markerSize", "Marker Size", value=7, min = 1, max = 30, step = 0.1)
                
              ),
              tabPanel(
                "Information",
                htmlOutput("umxinfo"),
                tags$h5("Corresponding Data"),
                actionButton("pointinfo", "View selected"),
                tags$hr()
              ),
              tabPanel(
                "Clustering Tools",
                h4("New Clusters:"),
                actionButton(inputId = "Clust", label = "Create cluster"),
                h4("Modify existing:"),
                selectInput(
                  inputId = "ClsSelect",
                  label = "Select Class",
                  choices = unique(Cls)
                ),
                actionButton(inputId = "AddToCls", label = "Add selected"),
                tags$hr()
              )
            ),
            actionButton(
              inputId = "Exit",
              label = "Exit",
              class = "btn-primary"
            ),
            width = 3,
            height="100%")
            ,
            mainPanel(
              
              
              plotly::plotlyOutput("Plot",
                                   # height =  
                                   #   udim[1] * 5 * (quadtile + 1),
                                   # width = 
                                   #   udim[2] * 5 * (quadtile + 1)),
                                   width = "auto"),
              bsModal("printdata",
                      "Data corresponding to selected points",
                      "pointinfo",
                      size = "large",
                      shiny::dataTableOutput("dataout"))
              ,
              width = 9,
              height = "100%"
            )
            
          )
        )
      ))
    
    server <- shiny::shinyServer(function(input, output, session) {
      
      ## Helper functions:
      selectedids <- function() {
        
        # Beware of off by one errors here
        # Keep in mind: shiny and plotly use javascript ennumeration
        selected <- plotly::event_data("plotly_selected")
        if (is.null(selected) || length(selected) == 0) {
          return(NA)
        }
        classes  <- unique(Cls)[selected$curveNumber]
        indicies <- selected$pointNumber
        d        <- cbind(classes, indicies)
        # give toroid copys the id of the original
        # and convert to R indicies
        d[, 2]   <- (d[, 2] %% as.vector(table(Cls))[d[, 1]]) + 1
        # now transfer from inner-cluster indicies to indicies over Cls.
        Clsids   <- apply(
          d,
          MARGIN = 1,
          FUN = function(row) {
            return(which(Cls == row[1])[row[2]])
          }
        )
        return(Clsids)
      }
      
      doplot <- function(Cls) {
        
        #Handle Color:
        
        quants2 = quantile(as.vector(Umatrix), c(0.01, 0.5, 0.99))
        minU2 = quants2[1]
        maxU2 = quants2[3]
        HeightScale = round(maxU2/(2 * max(minU2, 0.05)), 0)
        stretchFactor = sqrt(nrow(Umatrix)^2 + ncol(Umatrix)^2)/sqrt(50^2 + 80^2)
        Nrlevels2 = 2 * HeightScale * stretchFactor
        # levelBreaks <- seq(0, 1.000001, length.out = (Nrlevels2 + 1))
        # splittedGeneralizedUmatrix = Umatrix
        # for (i in 1:Nrlevels2) {
        #   splittedGeneralizedUmatrix[(Umatrix >= levelBreaks[i]) &
        #                                (Umatrix <= levelBreaks[i + 1])] = levelBreaks[i]
        # }
        # splittedGeneralizedUmatrix = (floor(splittedGeneralizedUmatrix *
        #                                       length(colormap))) + 1
        # color = colormap[splittedGeneralizedUmatrix]
        # color = rep(color,4)
        # test <- matrix(color,ncol = udim[2], nrow = udim[1])
        # test <- cbind(rbind(test,test),rbind(test,test))
        
        
        # dmx  <- cbind(z,z)
        
        qdim <- udim * 2
        # Umatrix <- Umatrix * HeightScale * stretchFactor
        dmx  <- cbind(Umatrix,Umatrix)
        qmx  <- rbind(dmx, dmx)
        dbm  <- rbind(ubmus, cbind(ubmus[, 1], ubmus[, 2] + udim[2]))
        qbm  <- rbind(dbm, cbind(dbm[, 1] + udim[1], dbm[, 2]))
        
        #   qmx = matrixnormalization(qmx)
        
        plotumx <- qmx
        plotbmus <- qbm
        plotdim <- qdim
        plotCls <- rep(Cls, 4)
        
        
        output$Plot <- plotly::renderPlotly({
          
          width = (0.95*as.numeric(input$dimension[1]))
          height = udim[1]/udim[2] * (width-80)
          
          plt <- plotly::plot_ly(width = width, height = height*0.75) %>%
            plotly::add_contour(
              x = 1:plotdim[1],
              y = 1:plotdim[2],
              z = plotumx,
              showscale=FALSE,
              line = list(
                color = 'black',
                width = 0.5),
              contours = list(
                start = 0,
                end = 1,
                size = 1/15
              ),
              # colors = color,
              colors = colorRamp(colormap[c(rep(3,6),seq(from = 4,to = length(colormap)-30,length.out = ceiling(Nrlevels2 +1)-7),length(colormap))]),
              name = "UMatrix"
              # , showscale = FALSE
            )
          
          # plt <- plotly::plot_ly(width = 2*width, height = 2*height,type = "contour",
          #                         x = 1:plotdim[1],
          #                         y = 1:plotdim[2],
          #                         z = ~plotumx,
          #                         autocontour = TRUE,
          #                         line = list(smoothing = 0.85))
          
          addclass <- function(class, plot) {
            inds <- which(plotCls == class)
            plot <- plot %>% plotly::add_markers(
              x = plotbmus[inds, 2],
              y = plotbmus[inds, 1],
              marker = list(
                size = input$markerSize,
                color = DefaultColorSequence[class],
                line = list(color = "rgba(80, 80, 80, .8)",
                            width = 1)
              ),
              name = paste("Class", class)
            )
            return(plot)
          }
          for (class in unique(plotCls)){
            plt <- addclass(class, plt)
          }
          
          
          plt <- plt %>% plotly::layout(#title <- "Drop plot title here",
            #bgcolor = "rgb(244, 244, 248)",
            xaxis = ax,
            yaxis = ay,
            dragmode ='lasso'
            #, showlegend = FALSE
          )
          updateSelectInput(
            session,
            "ClsSelect",
            label = "Select Class",
            choices = unique(Cls)
          )
          outplot <<- plt
          plt
        }
        )
      }
      
      selectedpoints <- function() {
        
        d <- plotly::event_data("plotly_selected")
        if (length(d) == 0)
          "Nothing selected!"
        else{
          classes  <- unique(Cls)[d$curveNumber]
          indicies <- d$pointNumber
          d        <- cbind(classes, indicies)
          # give toroid copys the id of the original
          # and convert to R indicies
          d[, 2]    <- (d[, 2] %% as.vector(table(Cls))[d[, 1]]) + 1
          # now transfer from inner-cluster indicies to indicies over Cls.
          Clsids   <- apply(
            d,
            MARGIN = 1,
            FUN = function(row) {
              return(which(Cls == row[1])[row[2]])
            }
          )
          # to get original ids:
          unipointids <- uniq$sortind[Clsids]
          # and now the Points which sit on the same gridpostions:
          # if(length(unipointids) > 1) # TODO
          opoints <- bestmatches[colSums((diag(nrow(
            uniq$IsDuplicate
          )) + uniq$IsDuplicate)[unipointids,]) > 0,]
          if (all(is.na(Data)))
            return(opoints) # Beaware: opoints often more than unipointsid
          Data[opoints[, 1],]
        }
      }
      
      # HTML Output: Umatrix Properties
      output$umxinfo <- renderUI({
        HTML(paste0("
                    <table>
                    <tr>
                    <th>Lines: </th>
                    <td>", udim[1], "</td>
                    </tr>
                    <tr>
                    <th>Columns: </th>
                    <td>", udim[2], "</td>
                    </tr>
                    <tr>
                    <th>Bestmatches: </th>
                    <td>", nrow(bestmatches), "</td>
                    </tr>
                    </table>
                    "))
      })
      
      observeEvent(input$dimension,{ 
      })
      
      # Button: View Selected
      observeEvent(input$pointinfo,{
        seldat <- selectedpoints()
        output$dataout <- renderDataTable({
          seldat
        })
      })
      
      # Button: Merge Clusters
      observeEvent(input$Merge, {
        selected <- plotly::event_data("plotly_selected")
        if (is.null(selected) || length(selected) == 0 ){
          return()
        }
        points <- (selected[, 2] %% nrow(ubmus)) + 1
        Clss <- unique(Cls[points])
        ids <- which(Cls %in% Clss)
        Cls[ids] <- min(Clss)
        Cls <- getUniquePoints(Cls)$mergeind
        Cls <<- Cls
        doplot(Cls)
      })
      
      
      # Button: Create new cluster
      observeEvent(input$Clust, {
        Clsids <- selectedids()
        if (!is.na(Clsids)) {
          id <- max(Cls) + 1
          points <- Clsids
          Cls[points] <- id
          doplot(Cls)
          Cls <<- Cls
        }
      })
      
      # Button: Add to cluster
      observeEvent(input$AddToCls, {
        
        id <- input$ClsSelect
        Clsids <- selectedids()
        
        if (!is.na(Clsids)){
          Cls[Clsids] <- id
          Cls <- getUniquePoints(as.numeric(Cls))$mergeind
          Cls <<- Cls
          doplot(Cls)
        }
      })
      
      # Calculate Projection and GeneralizedUmatrix and plot result.
      observeEvent(input$generate,{
        
        type=input$projections 
        k=2
        project=function(type){
          switch(type,
                 
                 PCA = ProjectionBasedClustering::PCA(Data,Center=input$PCACenter,Scale=input$PCAscale,OutputDimension = k,Cls=Cls)$ProjectedPoints,
                 CCA = ProjectionBasedClustering::CCA(Data,OutputDimension = k,Cls=Cls,Epochs =input$CCAEpochs, method = input$CCAMethod, alpha0=input$CCASteps)$ProjectedPoints,
                 ICA = ProjectionBasedClustering::ICA(Data,OutputDimension = k,Cls=Cls, Iterations =input$ICAIterations,Alpha = input$ICASteps )$ProjectedPoints,
                 MDS = ProjectionBasedClustering::MDS(Data,OutputDimension = k,Cls=Cls,  method=input$MDSMethod)$ProjectedPoints,
                 NeRV = ProjectionBasedClustering::NeRV(Data= Data,OutputDimension = k,Cls=Cls, iterations = input$NERVIterations, lambda = input$NERVLambda, neighbors =input$NERVNeighbors),
                 ProjectionPursuit= ProjectionBasedClustering::ProjectionPursuit(Data,OutputDimension = k,Cls=Cls, Iterations = input$PPIterations, Indexfunction =input$PPMethod , Alpha =input$PPAlpha )$ProjectedPoints,
                 SammonsMapping= ProjectionBasedClustering::SammonsMapping(Data,OutputDimension = k,Cls=Cls, method = input$SMMethod)$ProjectedPoints,
                 tSNE = ProjectionBasedClustering::tSNE(Data,OutputDimension = k,Cls=Cls, method = input$tSNEMethod, Iterations = input$tSNEIterations, Whitening = input$tSNEWhite)$ProjectedPoints
          )
        }
        pData=project(type)
        
        # construct full CLS from unique bestmatches
        if(length(Cls)!=length(ClsO)){
          reCls =1:(length(uniq$mergeind))
          Cls <<- Cls[uniq$mergeind]
        }
        
        
        newU=GeneralizedUmatrix(Data=Data,ProjectedPoints = pData,Cls=Cls)
        Umatrix <<- newU$Umatrix
        bestmatches <<- newU$Bestmatches
        createParams(Umatrix, bestmatches,Cls = Cls)
        
        doplot(Cls)
      })
      
      
      # Button: Exit
      #MT hier fehlt noch names(Cls)=Key
      observeEvent(input$Exit, {
        Cls=normCls(Cls[mergei])$normalizedCls
        names(Cls)=Key
        stopApp(list(Cls = Cls, plot = outplot))
      })
    })
    
    output <- runApp(list(ui = ui, server = server))
    return(output)
  }

getUniquePoints <- function(data,mindist = 1e-10){
  # U <- getUniquePoints(data)
  # return only the unique points in data
  #
  # INPUT
  # data[1:n,1:d]   The vector/matrix with n data points of dimension d
  #				          the points are in the  rows
  # mindist					everything with less distance is considered equal
  #
  # OUTPUT
  # a list U containg:
  # UniqueData  <- U$unique[1:u,1:d]       the data points  without duplicate points
  # UniqueInd   <- U$sortind[1:u]		       an index vector such that unique ==  data[sortind,]
  # Uniq2DataInd<- U$mergeind[1:n] 	       an index vector such that   data ==  unique[mergeind,]
  # IsDuplicate <- U$IsDuplicate[1:n,1:n]  for i!=j IsDuplicate[i,j]== 1  iff data[i,] == data[j,] ;  IsDuplicate[i,i]==0
  #
  # Complete Code rewrite by FP 03/17 
  # 1.Editor: MT 03/17: mindist als Parameter eingefuehrt
  
  
  # when data is a vector, convert to matrix
  if (class(data) == "numeric" || class(data) == "complex") {
    data <- matrix(data, ncol = 1)
  } else if (class(data) == "data.frame") {
    data <- as.matrix(data)
  } else if (class(data) != "matrix") {
    stop("getUniquePoints input is neither a (numeric or complex) vector, matrix or data.frame.")
  }
  
  NumData = nrow(data)
  
  distsmat = as.matrix(dist(data))
  # * 1 is to "hack" TRUE to 1 and FALSE to 0
  IsDuplicate = ((distsmat) < mindist) * 1 - diag(NumData)
  
  # Hack: set distance of every node to itself to NA (we're not interested in these distances)
  # otherwise every element would be a duplicate of itself later.
  distsmat = distsmat + diag(rep(NA, nrow(distsmat)))
  
  # No duplicates found
  if (length(which((distsmat) < mindist)) == 0) {
    return(list(
      "unique" = unique(data),
      "sortind" = c(1:NumData),
      "mergeind" = c(1:NumData),
      IsDuplicate = IsDuplicate
    ))
  }
  
  # save rownames
  origrows = rownames(data)
  # use rownames to save original index
  rownames(data) = c(1:NumData)
  
  #  find out which distances are smaller than mindist (and as such duplicates)
  # (except the diagonal, so we artificially increase it)
  dups = which((distsmat) < mindist, arr.ind = T)
  
  # indicies of rows which will be deleted
  delinds = c()
  while (dim(dups)[1] > 0) {
    ind = dups[1, 1]
    delinds = c(delinds, ind)
    # remove every row in which the duplicate also occurs.
    # this is to prevent to also delete the "original"
    # if not save, encapuslate right hand side in matrix( ... , nrow = 2)
    dups = dups[-(which(dups == ind, arr.ind = T)[, 1]), ]
  }
  # delete duplicates, stay as matrix (important for vector input) and keep rownames
  uniquedata = data[-delinds, ]
  uniquedata = matrix(uniquedata, ncol = ncol(data))
  rownames(uniquedata) = rownames(data)[-delinds]
  
  # the rownames contain the indicies in the original data
  sortind = as.numeric(rownames(uniquedata))
  
  # calculate the mergind. At the end there should not be a NA in mergeind left
  mergeind = rep(NA, NumData)
  mergeind[sortind] = 1:nrow(uniquedata)
  # this works due to the fact that:
  #   union(sortind, delinds) == 1:NumData (if everything is sorted)
  for (i in delinds) {
    candidates = which(IsDuplicate[i, ] == 1)
    original = intersect(candidates, sortind)
    mergeind[i] = which(sortind == original)
  }
  
  # restore rownames of the original data
  rownames(uniquedata) = origrows[-delinds]
  
  return(
    list(
      "unique" = as.matrix(uniquedata),
      "sortind" = sortind,
      "mergeind" = mergeind,
      IsDuplicate = IsDuplicate
    )
  )
  
}

normCls <- function(Cls) {
  #E<-normCls(Cls);
  #NormalizedCls    <- E$normalizedCls      #    Cls consistently recoded to positive consecutive integers
  #NormalizedClasses<- E$normalizedClasses  #    the different class numbers in NormalizedCls
  #UniqueCls        <- E$uniqueClasses      #    the different class numbers in Cls such that 
  #AnzClasses       <- E$numberOfClasses    #    the number of different classes
  # 
  # Values in Cls are consistently recoded to positive consecutive integers
  # INPUT
  # Cls                  vector of class identifiers can be integers or
  #                      NaN's, need not be consecutive nor positive
  # OUTPUT list of 
  # normalizedCls           Cls consistently recoded to positive consecutive integers
  # normalizedClasses        the different class numbers in NormalizedCls
  # uniqueClasses            the different class numbers in Cls such that 
  #                           NormalizedCls(i) <-> UniqueCls(i)
  # numberOfClasses           the number of different classes
  
  # ALU 2014
  # angepasst an Mdbt und Doku standards
  
  
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  unique2Cls <- NULL #  initializing the vector
  
  for (i in 1:length(Cls)) {
    # calculating the indexes of elements of Cls in uniqueClasses
    unique2Cls <- c(unique2Cls, which(uniqueClasses == Cls[i]))
  }
  
  if (numberOfClasses > 0) {
    normalizedClasses <- c(1:numberOfClasses)
    normalizedCls <- normalizedClasses[unique2Cls]
  }
  else {
    normalizedClasses <- Cls
  }
  
  return(
    list(
      normalizedCls = normalizedCls,
      normalizedClasses = normalizedClasses,
      uniqueClasses = uniqueClasses,
      numberOfClasses = numberOfClasses
    )
  )
}
