

HMMpredict <- function(object){
  
  predictionDis <- lapply(1:object@nstates, matrix, data = NA, nrow = sum(object@ntimes), ncol = object@nresp)
  variableCheck <- vector(length = object@nresp)
  
  for(s in 1:object@nstates){
    for(v in 1:object@nresp){
      predictionDis[[s]][,v] <-  predictManual(object@response[[s]][[v]])
      if(s == 1) {
        variableCheck[v] <- as.character(object@response[[1]][[v]]@formula[[2]]) 
      }
    }
  }
  
  prediction <- matrix(NA, nrow = sum(object@ntimes), ncol = object@nresp)
  colnames(prediction) <- variableCheck
  for(i in 1:sum(object@ntimes)){
    prediction[i,] <- predictionDis[[object@posterior$state[i]]][i,]
    
  }
  
  

  output <- list(prediction = prediction,
                 variableCheck = variableCheck, 
                 predictionDistribution = predictionDis)
  output
}


