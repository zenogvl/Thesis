
#bobject <- object

#object <- bobject

#object <- depmixFit[[useNstate-1]]


createPredictObject <- function(object, dataNew, lengthTS){
  
  object@ntimes <- lengthTS
  
  #Change the dens array of the object 
  newDens <- array(NA, dim = c(nrow(dataNew), object@nresp, object@nstates))
  
  s <- 2
  v <- 1
  for(s in 1:object@nstates){
    for(v in 1:object@nresp){
      
      #Create new response object
      response <- object@response[[s]][[v]]
      
      #Change outcome data
      response@y <- as.matrix(dataNew[,as.character(response@formula[[2]])]) 
      
      #Change desing matrix
      newX <- matrix(NA, nrow = nrow(dataNew), ncol = ncol(response@x))
      colnames(newX) <- colnames(response@x)  
      newX[,1] <- rep(1, nrow(dataNew)) #Intercept 
      
      p <- colnames(response@x)[-1][1]
      #For multivariate change the data for the other variables
      if(ncol(response@x) > 1){
        for(p in colnames(response@x)[-1]){
          newX[,p] <- dataNew[,p]
        }
      }
      response@x <- newX
      
      object@response[[s]][[v]] <- response
      
      #Use new response object to get the dens
      newDens[,v,s] <-  densManual(object@response[[s]][[v]])
    }
  }
  object@dens <- newDens
  
  object
}

densManual <- function(object,log=FALSE) {
  dnorm(x=object@y,mean=predictManual(object),sd=object@parameters$sd,log=log)
}

predictManual <- function(object) {
  object@x%*%object@parameters$coefficients
}

