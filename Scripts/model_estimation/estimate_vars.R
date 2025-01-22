
addNight <- function(x){
  emptyRow <- rep(NA,ncol(x))
  i <- 1
  while(i < nrow(x)){
    if(x$day[i] != x$day[i + 1]){
      x <- rbind(x[1:i,], emptyRow, x[(i + 1):nrow(x),])
      i <- i + 1
      x[i, which(colnames(x) == "ID")] <- x$ID[i - 1]
    }
    i <- i + 1
  }
  x
}



x <- data
data <- read.csv("Data/Clean/Bringmann_2013.csv", header = T)
variables <- c("Sad", "Angry", "Anxiety", "Depressed", "Stressed", "Happy", "Relexed", "Excited")
oosValidationSize <- .2
oosValidationMethods <- "percentage"



varN1 <- function(data, # data of a single study
                  variables, #vector with variable names 
                  oosValidationMethods = c("percentage", "number"), #Use a percentage or a specific value to create test data for out of sample validataion 
                  oosValidationSize, #The percentage of data or number of observation for the test dataset. 
                  studyName,
                  outputLocation,
                  returnResults = TRUE,
                  minDataPoints = 10,
                  naMethod = c("listwiseDelition", "kalmanFilter")
                  ) 
  {
  
  

  #Get the ID's of all the PP that have more than min data points
  
  minDataID <- data %>%
    na.omit() %>%
    count(ID) %>%
    filter(n > minDataPoints ) %>%
    pull(ID)
  
  data <- data[data$ID %in% minDataID,]
  
  if(naMethod == "kalmanFilter") {
    for(pp in unique(data$ID)){
      data[data$ID == pp, variables] <- kalmanFilter(data[data$ID == pp, variables]) 
    }
  }
  
  
  $data[, variables] <- kalmanFilter(data[,variables]) 
  

  
  if("beep" %in% colnames(data)){
    data <- data %>%  
      addNight() %>% #add a empty row after each day 
      mutate_at(all_of(variables), list(lag = ~lag(.)))  #Create a lag 
  } else {
    data <- data %>%  
      mutate_at(all_of(variables), list(lag = ~lag(.)))  #Create a lag 
    
  }
  
    
  #Create output 
  n <- length(unique(data$ID))
  results <- as.data.frame(matrix(NA, nrow = n, ncol = length(variables) + 1))
  colnames(results) <- c("ID", variables)
  results$ID <- unique(data$ID)
  
  pp <- 1091     
  strangepred <- vector()
  for(pp in unique(data$ID)){
    #print(pp)
    x <- data[data$ID == pp,]
    #Remove all the rows with missing values 
    x <- na.omit(x)
    
    if(nrow(x) < minDataPoints) next #Remove pp with less than 10 observations 

    #Split the data in train and test data set
    if(oosValidationMethods == "percentage"){
      if(!(oosValidationSize > 0 & oosValidationSize < 1)){
        stop("for method percentage, oosValidationSize should be between 0 and 1")
      } else {
        splitIndex <- nrow(x) - round(nrow(x) * oosValidationSize, 0)
        x_train <- x[1:(splitIndex-1),]
        x_test <- x[splitIndex:nrow(x),]
      }
    } else if (oosValidationMethods == "number"){
      if(!(oosValidationSize > 1 & is.integer(oosValidationSize))){
        stop("for method number, oosValidationSize should be an integer")
      } else {
        x_train <- x[1:(oosValidationSize-1),]
        x_test <- x[oosValidationSize:nrow(x),]
      }
    }
    
    
    #Create a fromula where all the lagged variables are the predictors 
    ff_predictors <-  paste0(variables, "_lag", collapse = "+")
    # 
    # 2.635953e-36
    # 
    # x_train$Sad
    # 
    v <- variables[11]
    
    predictions <-  as.data.frame(matrix(NA, nrow = nrow(x_test), ncol = length(variables)))
    colnames(predictions) <- c(variables)
    

    
    for(v in variables){
      ff <- paste0(v,  "~", ff_predictors)
      #Fit the VAR model on the training data
      fit <- stats::lm(ff , x_train)
      
      # 
      # fit$coefficients %>% 
      #   broom::tidy() %>%
      #   filter(names == "Sad_lag") %>% 
      #   select(x) %>%
      #   round(1000)
      # 
      # 
      #   filter( x > 10 | x < 10)
      
      #Predict the values 
      pred <- stats::predict(fit, x_test)
      
      predictions[,v] <- pred
      
      Metrics::rmse(x_test[,v], pred )
      
      RMSE <- sqrt(1/nrow(x_test)*
                     sum((x_test[, v] - pred)^2))
      
      results[results$ID == pp, v] <- RMSE
    }
    strangepred <- c(strangepred, sum(predictions > 2 | predictions < -2)/(nrow(predictions)*ncol(predictions)))
    
    
    
  }
  
write.csv(results, paste0(outputLocation, "RMSE_", studyName, ".csv"))
  if(returnResults) {
    results
  }
 
}
results[1,variables]

rowMeans(results[,variables]) > 100 

results$meanRMSE <- rowMeans(results[,variables])
results$strpd <- strangepred

results %>% 
  filter(meanRMSE > .5)

results$Lively %>% round(5)
results[,variables] %>%
  apply(2,round,3)


(a <- base::round(results,8))

results[58,]

variables_list <- readRDS("Data/variables_list.rds")
f <- list.files("Data/Clean/")
ds_names <- gsub(".csv", "", f)
res_list <- list()

ds <- ds_names[33]

for(ds in ds_names){
  print(ds)
  varN1(data = read.csv(paste0("Data/CleanRescaled/Rescaled_", ds, ".csv")), 
        variables = variables_list[[ds]], 
        oosValidationMethods = "percentage", 
        oosValidationSize = 0.2, 
        studyName = ds, 
        outputLocation = "Output/N1_Results/RescaledResults/VAR_kalman_filter_RMSE/",
        minDataPoints = 20,
        naMethod = "kalmanFilter",
        returnResults = FALSE
  )
}

for(ds in ds_names){
  print(ds)
  varN1(data = read.csv(paste0("Data/CleanRescaled/Rescaled_", ds, ".csv")), 
        variables = variables_list[[ds]], 
        oosValidationMethods = "percentage", 
        oosValidationSize = 0.2, 
        studyName = ds, 
        outputLocation = "Output/N1_Results/RescaledResults/VAR_listwise_deletion_RMSE/",
        minDataPoints = 20,
        naMethod = "listwiseDelition",
        returnResults = FALSE
  )
}
ds <- f


varN1(data = read.csv(paste0("Data/Clean/", "Koval_2015", ".csv")), 
      variables = variables_list[["Koval_2015"]], 
      oosValidationMethods = "percentage", 
      oosValidationSize = 0.2, 
      studyName = ds, 
      outputLocation = "Output/N1_VAR_Results/",
      minDataPoints = 20,
      naMethod = "listwiseDelition"
)

data = read.csv(paste0("Data/Clean/", ds, ".csv"))
variables = variables_list[[ds]]

outputLocation <- "Output/N1_VAR_Results/"
  
pp <- unique(data$ID)[1]

data[data$ID == pp,] %>% nrow()


data[data$ID == pp, variables] <- imputeTS::na_kalman(data[data$ID == pp, variables]) 

?stats::KalmanSmooth
?stats::StructTS


data$missing <- data %>%
  is.na %>%
  apply(1,any) 


data$Sad %>% max(na.rm = T)

ds <- dataset
data = read.csv(paste0("Data/CleanRescaled/Rescaled_", ds, ".csv"))
      variables = variables_list[[ds]] 
      oosValidationMethods = "percentage" 
      oosValidationSize = 0.2
      studyName = ds 
      outputLocation = "Output/N1_VAR_Results/"
      minDataPoints = 20
      naMethod = "kalmanFilter"

