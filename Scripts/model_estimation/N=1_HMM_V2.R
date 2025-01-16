
install.packages(setdiff(c("tidyverse", "depmixS4", "hmmr", "stats", "gtools", "parallel"), rownames(installed.packages())))  
library(tidyverse)
#library(depmixS4)
source("Scripts/N=1_pipeline/WorkerFunctions/createTimeSeriesSplitForDepmix.R")
source("Scripts/N=1_pipeline/WorkerFunctions/createObjectHMMPrediction.R")
source("Scripts/N=1_pipeline/WorkerFunctions/HMMpredict.R")
source("Scripts/N=1_pipeline/WorkerFunctions/kalmanFilter.R")



###     input <- inputList[[3]]

hmmN1pp <- function(input){
  x <- input$data
  ff <- input$ff
  variables <- input$variables
  maxNumbStates <- input$maxNumbStates
  
  
  ###  print(unique(x$ID))
  
  
  
  #Split the dataset in train and test dataset 
  if(input$oosValidationMethods == "percentage"){
    if(!(input$oosValidationSize > 0 & input$oosValidationSize < 1)){
      stop("for method percentage, oosValidationSize should be between 0 and 1")
    } else {
      splitIndex <- nrow(x) - round(nrow(x) * input$oosValidationSize, 0)
      x_train <- x[1:(splitIndex-1),]
      x_test <- x[splitIndex:nrow(x),]
    }
  } else if (input$oosValidationMethods == "number"){
    if(!(input$oosValidationSize > 1 & is.integer(input$oosValidationSize))){
      stop("for method number, oosValidationSize should be an integer")
    } else {
      x_train <- x[1:(input$oosValidationSize-1),]
      x_test <- x[input$oosValidationSize:nrow(x),]
    }
  }
  
  #Get a vector that indicates how long all the seperate time series are 
  obj_train <- createTSsplitHMM(x_train, variables=variables, method = input$createTSsplitMethod)
  x_train <- obj_train$data
  lengthTS <- obj_train$lengthTS
  
  
  depmixObject <- vector("list", (maxNumbStates-1))
  depmixFit <- vector("list", (maxNumbStates-1))
  tries <- rep(NA, (maxNumbStates-1))
  IC <- data.frame(nstate = 2:maxNumbStates,
                   AIC = rep(NA, maxNumbStates-1),
                   BIC = rep(NA, maxNumbStates-1))
  for(s in 2:maxNumbStates){
    counter <- 0
    #Repeat loop that continues until a converged model is found. 
    repeat{
      counter <- counter + 1
      depmixObject[[s-1]] <-  depmixS4::depmix(ff, 
                                               data = x_train, 
                                               nstates = s, 
                                               family = rep(list(gaussian()),length(variables)),
                                               ntimes = lengthTS)
      invisible(capture.output(try(depmixFit[[s-1]] <- depmixS4::fit(depmixObject[[s-1]]), silent = TRUE))) #Fit model and continu even with an error. 
      
      #If model is empty or did not converge, repeat, if converged, stop. 
      if(is.null(depmixFit[[s-1]])){
        if(counter == input$maxTries) break 
      } else if(grepl("without convergence", depmixFit[[s-1]]@message)){
        if(counter == input$maxTries) break 
      } else {
        break
      }
    }
    if(counter < input$maxTries){
      IC$AIC[s-1] <- AIC(depmixFit[[s-1]])
      IC$BIC[s-1] <- BIC(depmixFit[[s-1]])
    }
    tries[s-1] <- counter

  }
  #depmixFit
  #only continues when at least one model is fitted 
  if(any(!is.na(IC[,2:3]))){
    #Select best model based on a IC
    if(input$ICmethod == "AIC"){
      useNstate <- IC$nstate[which.min(IC$AIC)]
    } else if (input$ICmethod == "BIC"){
      useNstate <- IC$nstate[which.min(IC$BIC)]
    }
    
    #Create the TS split for the the test data
    obj_test <- createTSsplitHMM(x_test, variables=variables, method = "both")
    x_test <- obj_test$data
    lengthTS_test <- obj_test$lengthTS
   
    predictObject <- createPredictObject(depmixFit[[useNstate-1]], dataNew = x_test, lengthTS = lengthTS_test)
    
    if(exists("posteriorPredict")) rm("posteriorPredict")
  
    #Get the most likley state sequance 
    try(posteriorPredict <- depmixS4::viterbi(predictObject))
    if(exists("posteriorPredict")){
      predictObject@posterior <- posteriorPredict
      
      prediction <- HMMpredict(predictObject)
      #v <- variables[1]  ## Remove ##
      RMSE <- vector(mode = "numeric",length = length(variables))
      names(RMSE) <- variables
      
      for(v in variables){
        RMSE[v] <- sqrt(1/nrow(x_test)*
                          sum((x_test[,v] - prediction$prediction[,v])^2)
        )
      }
      return(
        list(depmixObjects = depmixObject,
             depmixFit = depmixFit, 
             IC = IC,
             nstateUsed = useNstate,
             posterior = posteriorPredict,
             depmixPredictionObject = predictObject,
             RMSE = RMSE,
             Ntries = tries)
      )
    } else {
      return(NA)
    }
  } 
}

hhmN1_v2 <- function(data, # data of a single study
                  variables, #vector with variable names 
                  maxNumbStates, #The maximam number of states for the HMM to fit. 
                  oosValidationMethods = c("percentage", "number"), #Use a percentage or a specific value to create test data for out of sample validataion 
                  oosValidationSize, #The percentage of data or number of observation for the test dataset. 
                  studyName,
                  outputLocation,
                  returnResults = TRUE,
                  minDataPoints = 10,
                  multivar = c(TRUE, FALSE),
                  ICmethod = c("AIC", "BIC"),
                  maxTries = 100,
                  createTSsplitMethod = c("both", "days", "missing"),
                  naMethod = c("none", "kalmanFilter")
) {
  oosValidationMethods <- match.arg(oosValidationMethods)
  #multivar <- match.arg(multivar)
  ICmethod <- match.arg(ICmethod)
  naMethod <- match.arg(naMethod)
  #createTSsplitMethod <- mathc.arg(createTSsplitMethod)

  #Get the ID's of all the PP that have more than min data points
  minDataID <- table(na.omit(data)$ID)[table(na.omit(data)$ID) > minDataPoints] %>% #Index the table for above this minimum
    names() %>% #Take the names of this vector to get the IDs
    as.integer() #Transfrom to numeric 
  
  dataLowNRemoved <- data[data$ID %in% minDataID,]
  
  if(naMethod == "kalmanFilter") {
    for(pp in unique(data$ID)){
      #print(pp)
      dataLowNRemoved[dataLowNRemoved$ID == pp, variables] <- kalmanFilter(dataLowNRemoved[dataLowNRemoved$ID == pp, variables]) 
    }
  }
  
  #x <- dataLowNRemoved[dataLowNRemoved$ID == pp, variables]
  
  dataList <- split(dataLowNRemoved, dataLowNRemoved$ID)
  
  #Create the formula list:
  ff <- list()
  if(multivar){
    for(v in variables){
      ff[[v]] <-  paste0(v, "~", "1+", paste0(variables[!(variables == v)], collapse  = "+")) %>% 
        stats::as.formula()
    }
  } else {
    for(v in variables){
      ff[[v]] <-  paste0(v, "~", "1") %>% 
        stats::as.formula()
    }
  }
  
  inputList <- list()
  for(pp in names(dataList)){
    inputList[[pp]] <-  list(data = dataList[[pp]],
                             variables = variables,
                             ff = ff,
                             oosValidationMethods = oosValidationMethods,
                             oosValidationSize = oosValidationSize,
                             maxNumbStates = maxNumbStates,
                             maxTries = maxTries,
                             ICmethod = ICmethod,
                             createTSsplitMethod = createTSsplitMethod)
    
  }
  
  cores <- parallel::detectCores()
  clus <- parallel::makeCluster(cores)
  parallel::clusterExport(clus, c("createTSsplitHMM", 
                                  "hmmN1pp", 
                                  "createPredictObject",
                                  "densManual",
                                  "predictManual", 
                                  "HMMpredict"))
  parallel::clusterEvalQ(clus, {
    library(tidyverse)
    library(depmixS4)
  })
  res <- parallel::parLapply(clus, inputList, hmmN1pp)
  parallel::stopCluster(clus)
 

  
  #Create output matrix RMSE
  RMSE <- matrix(NA, length(res), (length(variables)+1))
  colnames(RMSE) <- c("ID", variables)
  RMSE[,1] <- as.integer(names(dataList))
  
  #Create output matrix for number of tries 
  Ntries <- matrix(NA, length(res), (maxNumbStates))
  colnames(Ntries) <- c("ID", 2:maxNumbStates)
  Ntries[,1] <- as.integer(names(dataList))
  
  #Create output matrix for number of states
  nstateUsed <- matrix(NA, length(res), 2)
  colnames(nstateUsed) <- c("ID", "nstate")
  nstateUsed[,1] <- as.integer(names(dataList))
  
  
  if(length(res)>0){
    for(pp in 1:length(res)){
      #print(pp)
      if(!is.null(res[[pp]]) & !is.na(res[pp])){
        RMSE[pp,-1] <- res[[pp]]$RMSE
        Ntries[pp,-1] <- res[[pp]]$Ntries 
        nstateUsed[pp,-1] <- res[[pp]]$nstateUsed 
      }
    }
    
    
    write.csv(RMSE, paste0(outputLocation, "RMSE/RMSE_", studyName,".csv"))
    write.csv(Ntries, paste0(outputLocation, "Ntries/Ntries_", studyName,".csv"))
    write.csv(nstateUsed, paste0(outputLocation, "Nstates/Nstates_", studyName,".csv"))
    
    
  } 

  saveRDS(res, paste0(outputLocation, "HMMres_", studyName, ".rds"))
  if(returnResults) {
    res
  }
}  


variables_list <- readRDS("Data/variables_list.rds")
f <- list.files("Data/CleanRescaled/")
ds_names <- gsub("Rescaled_|.csv", "", f)

ds_names <- ds_names[!(ds_names %in% c("Kroencke_2020", "Wendt_2020"))]


timeKF <- readRDS("Output/HMMfitTimes/")

ds <- ds_names[37]

timeKF <- data.frame(ds = ds_names, time = rep(NA, length(ds_names)))
#Kalman Filter
for(ds in ds_names[36:37]){
  print(ds)
  startTime <- Sys.time()
  hhmN1_v2(data = read.csv(paste0("Data/CleanRescaled/", "Rescaled_", ds, ".csv")), 
           variables = variables_list[[ds]],
           maxNumbStates = 5, 
           oosValidationMethods = "percentage", #Use a percentage or a specific value to create test data for out of sample validataion 
           oosValidationSize = .2, #The percentage of data or number of observation for the test dataset. 
           studyName = ds,
           outputLocation = "Output/N1_Results/RescaledResults/HMM_kalman_filter_results/",
           returnResults = FALSE,
           minDataPoints = 20,
           multivar = FALSE,
           ICmethod = "AIC",
           maxTries = 100,
           createTSsplitMethod = "days", 
           naMethod = "kalmanFilter"
  )
  endTime  <- Sys.time()
  timeKF[timeKF$ds == ds, 2] <- difftime(endTime, startTime, units = "mins")
  print(endTime - startTime)
}
saveRDS(timeKF, "Output/HMMfitTimes/timeKF.rds")

timeOD <- data.frame(ds = ds_names, time = rep(NA, length(ds_names)))
#Only split on days
for(ds in ds_names){
  print(ds)
  startTime <- Sys.time()
  hhmN1_v2(data = read.csv(paste0("Data/CleanRescaled/", "Rescaled_", ds, ".csv")), 
           variables = variables_list[[ds]],
           maxNumbStates = 5, 
           oosValidationMethods = "percentage", #Use a percentage or a specific value to create test data for out of sample validataion 
           oosValidationSize = .2, #The percentage of data or number of observation for the test dataset. 
           studyName = ds,
           outputLocation = "Output/N1_Results/RescaledResults/HMM_onlyday_split_results/",
           returnResults = FALSE,
           minDataPoints = 20,
           multivar = FALSE,
           ICmethod = "AIC",
           maxTries = 100,
           createTSsplitMethod = "days"
  )
  endTime  <- Sys.time()
  timeOD[timeOD$ds == ds, 2] <- difftime(endTime, startTime, units = "mins")
  print(endTime - startTime)
  
}
saveRDS(timeOD, "Output/HMMfitTimes/timeOD.rds")


#Split both
timeDM <- data.frame(ds = ds_names, time = rep(NA, length(ds_names)))
for(ds in ds_names){
  print(ds)
  startTime <- Sys.time()
  hhmN1_v2(data = read.csv(paste0("Data/CleanRescaled/", "Rescaled_", ds, ".csv")), 
           variables = variables_list[[ds]],
           maxNumbStates = 5, 
           oosValidationMethods = "percentage", #Use a percentage or a specific value to create test data for out of sample validataion 
           oosValidationSize = .2, #The percentage of data or number of observation for the test dataset. 
           studyName = ds,
           outputLocation = "Output/N1_Results/RescaledResults/HMM_daymissing_split_results/",
           returnResults = FALSE,
           minDataPoints = 20,
           multivar = FALSE,
           ICmethod = "AIC",
           maxTries = 100,
           createTSsplitMethod = "both"
  )
  endTime  <- Sys.time()
  timeDM[timeDM$ds == ds, 2] <- difftime(endTime, startTime, units = "mins")
  print(endTime - startTime)
  
}
saveRDS(timeDM, "Output/HMMfitTimes/timeDM.rds")


data = read.csv(paste0("Data/CleanRescaled/Rescaled_", ds, ".csv"))
variables = variables_list[[ds]]
maxNumbStates = 5
oosValidationMethods = "percentage" #Use a percentage or a specific value to create test data for out of sample validataion 
oosValidationSize = .2 #The percentage of data or number of observation for the test dataset. 
studyName = ds
outputLocation = "Output/N1_HMM_Results_V2/"
returnResults = TRUE
minDataPoints = 20
multivar = F
ICmethod = "AIC"
maxTries = 100
createTSsplitMethod = "both"
