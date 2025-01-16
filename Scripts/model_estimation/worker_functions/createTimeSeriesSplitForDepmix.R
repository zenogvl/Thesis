library(tidyverse)

createTSsplitHMM <- function(data, variables, method = c("both", "days", "missing")){
  
  #Create a vector that indicates missing data 
  missingVector <- data[,variables] %>%
    is.na() %>%
    apply(1, any) %>%
    as.vector()
  
  if("beep" %in% colnames(data)){
    if(method == "days"){
      tsSplit <- data$day %>% 
        table() %>%
        as.vector()
      dataOut <- data
    }
    if(method == "missing") {
      #Creates a vector that shows for every row to what seperate time series it belongs. If there is missing data, a new ts starts. 
      sepTS <- rep(0, length(missingVector))
      ts <- 1 #Start with 1 for the first ts
      for(i in 2:length(missingVector)){
        sepTS[i-1] <- ts 
        if(missingVector[i] & !missingVector[i-1]){ #If there is a missing value, a 1 is added to the ts counter to indicate the next time series start. 
          ts <- ts + 1
        }
      }
      #Add ts to the last row (if this is missing, a new ts might start but since it then will be removed this doesn't matter)
      sepTS[length(missingVector)] <- ts
      
      data$sepTS <- sepTS
      
      #Remove all the missing data 
      dataOut <- data[!missingVector,]
      
      #Now get the vector that indicates how long every time serie is
      tsSplit <- dataOut$sepTS %>% 
        table() %>%
        as.vector()
    }
    if(method == "both"){
      #The same logic as for the missing data is used, but now a new ts also starts when a new day starts. 
      sepTS <- rep(0, length(missingVector))
      ts <- 1 
      data$day
      for(i in 2:length(missingVector)){
        sepTS[i-1] <- ts 
        if((missingVector[i] & !missingVector[i-1]) | (data$day[i-1] != data$day[i])) { #Also update the ts counter when a new day starts
          ts <- ts + 1
        }
      }
      sepTS[length(missingVector)] <- ts
      
      data$sepTS <- sepTS
      dataOut <- data[!missingVector,]
      tsSplit <- dataOut$sepTS %>% 
        table() %>%
        as.vector()
    }
  } else {
    #if(method != "missing") warning("data has no beep variable, days")
    if(method == "days"){ #When obs/day=1, no split takes places 
      tsSplit <- nrow(data)
      dataOut <- data
    } else { #Method both and missing are the same 
      sepTS <- rep(0, length(missingVector))
      ts <- 1 #Start with 1 for the first ts
      for(i in 2:length(missingVector)){
        sepTS[i-1] <- ts 
        if(missingVector[i] & !missingVector[i-1]){ #If there is a missing value, a 1 is added to the ts counter to indicate the next time series start. 
          ts <- ts + 1
        }
      }
      sepTS[length(missingVector)] <- ts 
      data$sepTS <- sepTS
      dataOut <- data[!missingVector,]
      tsSplit <- dataOut$sepTS %>% 
        table() %>%
        as.vector()
      
    }
  }
  
  output <- list(data = dataOut, 
                 variables = variables, 
                 lengthTS = tsSplit)
 output 
}
