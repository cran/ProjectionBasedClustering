tSNE = function(DataOrDistances,k,OutputDimension=2,Algorithm='tsne_cpp',method='euclidean',Whitening=FALSE, Iterations=1000,PlotIt=FALSE,Cls,num_threads=1,...){
#  T-distributed Stochastic Neighbor Embedding
#  
#  res = tSNE(Data, k=30,OutputDimension=2)
#   
# INPUT
# DataOrDistances[1:n,1:d]      array of data: n cases in rows, d variables in columns, matrix is not symmetric
#                           or distance matrix, in this case matrix has to be symmetric
# k                         number of k nearest neighbors=number of effective nearest neighbors("perplexity")
#                           Important parameter, if not given Settings of package t-SNE will be used
#
# OPTIONAL
# OutputDimension           data is projected onto a R^p where P is the maximum ( default ==2)
# Algorithm                 'tsne_cpp': T-Distributed Stochastic Neighbor Embedding using a Barnes-HutImplementation in C++
#                            'tsne_r': pure R implementation of the t-SNE algorithm
# method                    method specified by distance string: 
#                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
#
# Iterations                maximum number of iterations to perform.
# Whitening                 A boolean value indicating whether the matrix data should be whitened
# PlotIt                    bool, defaut=FALSE, if =TRUE: ClassPlot of every current Position of Databots will be made.
#                           OutputDimension>2 only the first two dimensions will be shown
# cls                       vector, Classifikation of Data if available, ClassPlots will be colorized
# 
# OUTPUT is a list with following elements:
# ProjectedPoints[1:n,OutputDimension]                   n by OutputDimension matrix containing coordinates of the Projection: A matrix of the fitted configuration.
#
# Note: Details in http://lvdmaaten.github.io/tsne/
# like "Typical values for the perplexity range between 5 and 50."
# author: MT 06/2015 
  if(missing(DataOrDistances))
		stop('No DataOrDistances given')
	DataOrDistances;
	if(!is.matrix(DataOrDistances))
		stop('DataOrDistances has to be a matrix, maybe use as.matrix()')

  #if(missing(k)){
  #  warning('k - optimal number of neighbors value missing, setting k=30, see default value for perplexity in package tsne') 
  #  k=30
  #} 
	is_distance=FALSE
  if(isSymmetric(unname(DataOrDistances))){
    DataDists=DataOrDistances
    AnzVar=ncol(DataOrDistances)
    AnzData=nrow(DataOrDistances)
    is_distance=TRUE
  }else{ #!isSymmetric
    AnzVar=ncol(DataOrDistances)
    AnzData=nrow(DataOrDistances)
    DataDists = as.matrix(dist( x = DataOrDistances, method = method))
  }# end if(isSymmetric(DataOrDistances))
	
  switch(Algorithm,
         tsne_cpp={
           if (!requireNamespace('Rtsne')) {
             message(
               'Subordinate projection package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
             )
             return(
               list(
                 Cls = rep(1, nrow(DataOrDistances)),
                 Object = "Subordinate projection package is missing.
                Please install the package which is defined in 'Suggests'."
               )
             )
           }
           if(missing(num_threads)) {
             if (!requireNamespace('parallel')) {
               message(
                 'parallel package is missing. Setting number of threads for multicore tsne to 1.
            Please install the package which is defined in "Suggests".'
               )
               num_threads = 1
             } else {
               num_threads = parallel::detectCores() - 1
             }
           }
           
           
           if(is_distance){
             if(!missing(k)){
               ModelObject=Rtsne::Rtsne(X = DataDists,dims=OutputDimension,is_distance=is_distance,perplexity = k, max_iter = Iterations,pca=Whitening, num_threads = num_threads,...)#experimental
             }else{
               ModelObject=Rtsne::Rtsne(X = DataDists,dims=OutputDimension,is_distance=is_distance, max_iter = Iterations,pca=Whitening, num_threads = num_threads,...)#experimental
             }
           }else{
             if(!missing(k)){
               ModelObject=Rtsne::Rtsne(X = DataOrDistances,dims=OutputDimension,is_distance=is_distance,perplexity = k, max_iter = Iterations,pca=Whitening, num_threads = num_threads, ...)
             }else{
               ModelObject=Rtsne::Rtsne(X = DataOrDistances,dims=OutputDimension,is_distance=is_distance, max_iter = Iterations,pca=Whitening, num_threads = num_threads, ...)
             }
           }
           ProjectedPoints=ModelObject$Y
         },
  tsne_r={
    if (!requireNamespace('tsne')) {
      message(
        'Subordinate projection package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
      )
      return(
        list(
          Cls = rep(1, nrow(DataOrDistances)),
          Object = "Subordinate projection package is missing.
                Please install the package which is defined in 'Suggests'."
        )
      )
    }
    if(!missing(k)){
      res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening, perplexity = k, max_iter = Iterations, ...)
    }else{
      res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening, max_iter = Iterations, ...)
    }
    ProjectedPoints=res
    ModelObject=NULL
  },
  opt_tsne_cpp={
    if(missing(num_threads)) {
      if (!requireNamespace('parallel')) {
        message(
          'parallel package is missing. Setting number of threads for multicore tsne to 1.
            Please install the package which is defined in "Suggests".'
        )
        num_threads = 1
      } else {
        num_threads = parallel::detectCores() - 1
      }
    }
    additionalArgs = list(...)
    if(is.null(additionalArgs[["theta"]])) {
      theta = 0.5
    } else {
      if(is.numeric(additionalArgs[["theta"]])) {
        theta = additionalArgs[["theta"]]
      } else {
        theta = 0.5
        warning("Value for theta is non numeric, set to default = 0.5")
      }
    }
    if(is.null(additionalArgs[["n_iter_early_exag"]])) {
      n_iter_early_exag = 250
    } else {
      if(is.numeric(additionalArgs[["n_iter_early_exag"]])) {
        n_iter_early_exag = additionalArgs[["n_iter_early_exag"]]
      } else {
        n_iter_early_exag = 250
        warning("Value for n_iter_early_exag is non numeric, set to default = 250")
      }
    }
    if(is.null(additionalArgs[["early_exaggeration"]])) {
      early_exaggeration = 12
    } else {
      if(is.numeric(additionalArgs[["early_exaggeration"]])) {
        early_exaggeration = additionalArgs[["early_exaggeration"]]
      } else {
        early_exaggeration = 12
        warning("Value for early_exaggeration is non numeric, set to default = 12")
      }
    }
    if(is.null(additionalArgs[["learning_rate"]])) {
      learning_rate = 200
    } else {
      if(is.numeric(additionalArgs[["learning_rate"]])) {
        learning_rate = additionalArgs[["learning_rate"]]
      } else {
        learning_rate = 200
        warning("Value for learning_rate is non numeric, set to default = 200 or calculated dynamically if auto_iter is TRUE")
      }
    }
    if(is.null(additionalArgs[["auto_iter"]])) {
      auto_iter = 1
    } else {
      if(is.logical(additionalArgs[["auto_iter"]])) {
        auto_iter = additionalArgs[["auto_iter"]]
      } else {
        auto_iter = 1
        warning("Value for auto_iter (opt-tsne mode) is non logical, set to default = TRUE")
      }
    }
    if(is.null(additionalArgs[["auto_iter_end"]])) {
      auto_iter_end = 0.02
    } else {
      if(is.numeric(additionalArgs[["auto_iter_end"]])) {
        auto_iter_end = additionalArgs[["auto_iter_end"]]
      } else {
        auto_iter_end = 0.02
        warning("Value for auto_iter_end is non numeric, set to default = 0.02")
      }
    }
    if(is.null(additionalArgs[["distance_squared"]])) {
      distance_squared = 0
    } else {
      if(is.logical(additionalArgs[["distance_squared"]])) {
        distance_squared = additionalArgs[["distance_squared"]]
      } else {
        distance_squared = 0
        warning("Value for distance_squared is non numeric, set to default = FALSE")
      }
    }
    
    if(auto_iter == 1) {
      learning_rate = nrow(DataDists) / early_exaggeration
    } 

    if(!missing(k)){
      res=opt_multicore_tnse_cpp(DataDists, OutputDimension, perplexity = k, max_iter = Iterations, num_threads = num_threads, 
                                 theta = theta, n_iter_early_exag = n_iter_early_exag, early_exaggeration = early_exaggeration,
                                 learning_rate = learning_rate, auto_iter = auto_iter, auto_iter_end = auto_iter_end, 
                                 distance_squared = distance_squared)      
      res = t(res)
    }else{
      res=opt_multicore_tnse_cpp(DataDists, OutputDimension, perplexity = 30, max_iter = Iterations, num_threads = num_threads, 
                                 theta = theta, n_iter_early_exag = n_iter_early_exag, early_exaggeration = early_exaggeration,
                                 learning_rate = learning_rate, auto_iter = auto_iter, auto_iter_end = auto_iter_end, 
                                 distance_squared = distance_squared)
      
      res = t(res)
    }
    ProjectedPoints=res
    ModelObject=NULL
  },
  {stop('Please choose either "tsne_cpp", "opt_tsne_cpp" or "tsne_r".')})


  if(PlotIt){
      if(missing(Cls)){
		AnzData=nrow(DataOrDistances)
		Cls=rep(1,AnzData)
	}  
    if(!missing(k)){
    string=paste0('T-distributed SNE with perplexity ',k)
    #ClassPlot(ProjectedPoints[,1],ProjectedPoints[,2],Cls=Cls,Title=string)
    }else{
      string=paste0('T-distributed SNE with perplexity ',30)
    }
    PlotProjectedPoints(ProjectedPoints,Cls,main=string)
  } 
return(list(ProjectedPoints=ProjectedPoints,ModelObject=ModelObject))
}    


