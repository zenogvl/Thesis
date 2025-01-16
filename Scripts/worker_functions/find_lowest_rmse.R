
for(ds in dataset_names){
  for(object_name in model_names){
    
  }
}

find_lowest_rmse <- function(path_list, 
                             dataset_names,
                             model_names = list(),
                             show_equal_results = FALSE){
  
  if(length(path_list) != length(model_names)){
    warning("No names for models")
    model_names <- as.character(1:length(path_list))
  }
  if(show_equal_results & length(path_list) > 2) {
    warning("Equal results only possible with two models")
    show_equal_results <- FALSE
  }
  
  #Collect RMSE data 
  for(object_name in model_names){
    data_input <- list()
    for(ds in dataset_names){
      specific_path <- paste0(path_list[[which(model_names == object_name)]], "RMSE_", ds, ".csv")
      data_input[[ds]] <- read.csv(specific_path) %>% 
        na.omit()
    }
    assign(paste0("rmse_", object_name), data_input)
  } 
  #get the individuals where the model is fitted everywhere. 
  pp_all_models_fitted <- list()
  for(ds in dataset_names){
    pp_fitted_models <- list()
    for(object_name in model_names){
      pp_fitted_models[[object_name]] <- get(paste0("rmse_", object_name))[[ds]][,"ID"]
    }
    print(c(ds,lapply(pp_fitted_models, length)))
    
    pp_all_models_fitted[[ds]] <- Reduce(intersect ,pp_fitted_models )
  }
  
  lowest_rmse_list <- list()
  for(ds in dataset_names){
    print(ds)
    for(object_name in model_names){
      assign(paste0("mean_rmse_", object_name), 
             get(paste0("rmse_", object_name))[[ds]]  %>%
               select(-X) %>%
               filter(ID %in% pp_all_models_fitted[[ds]]) %>%
               rowwise() %>%
               mutate(!!paste0("mean_", object_name) := mean(c_across(!ID))) %>%
               select(ID, paste0("mean_", object_name))
             ) 
    }
    if(show_equal_results){
      
    } else {
      lowest_rmse_list[[ds]] <- mget(paste0("mean_rmse_", model_names)) %>%
        purrr::reduce(inner_join, by = "ID")  %>%
        mutate(ndistinct = n_distinct(c_across(!ID))) %>% 
        filter(ndistinct == length(model_names)) %>% 
        select(-ndistinct)  %>%
        mutate(lowest_rmse_factor = factor(which.min(c_across(!ID)), 
                                           levels = 1:length(model_names),
                                           labels = model_names)) %>%
        ungroup() %>%
        count(lowest_rmse_factor) %>%
        complete(lowest_rmse_factor = factor(model_names, levels = model_names), 
                 fill = list(n = 0)) %>%
        mutate(dataset = rep(ds, n_distinct(lowest_rmse_factor))) %>%
        mutate(lowest_rmse_rate = n/sum(n)) 
      
    }
  }
  return(do.call("rbind", lowest_rmse_list))
}




path_list = list("Output/N1_Results/Rescaled_results/HMM_kalman_filter_results/RMSE/",
                 "Output/N1_Results/Rescaled_results/HMM_no_update_results/RMSE/",
                 "Output/N1_Results/Rescaled_results/HMM_seperate_series_results/RMSE/")

model_names <- c("Kalman", "no_upadata", "sep_series")


dataset_names <-gsub("RMSE_|.csv", "", list.files("Output/N1_Results/Rescaled_results/HMM_kalman_filter_results/RMSE/"))


