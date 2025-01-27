

a <- read.csv("Output/N1_Results/Rescaled_results/HMM_kalman_filter_results/Ntries/Ntries_Bar_2020_ds1.csv")


hmm_path <- "Output/N1_Results/Rescaled_results/HMM_kalman_filter_results/Ntries/"
var_path <- "Output/N1_Results/Rescaled_results/VAR_kalman_filter_RMSE/"

ds <- dataset_names[24]

for(ds in dataset_names){
  
  
  rmse_var <- read.csv(paste0(var_path, "RMSE_", ds,".csv")) %>%
    select(-X) %>% 
    mutate(var_mean_rmse = rowMeans(across(-ID), na.rm = TRUE)) %>%
    select(ID, var_mean_rmse) 
  
  ntries_hmm <- read.csv(paste0(hmm_path, "Ntries_", ds,".csv")) %>%
    select(-X) %>%
    mutate(across(starts_with("X"), ~ replace(.x, .x == 100, NA)))
  
  
  a <- full_join(rmse_var, ntries_hmm, by = "ID") %>%
    mutate(across(-ID, ~replace(.x, !is.na(.x), 1))) %>%
    mutate(across(-ID, ~replace(.x, is.na(.x), 0)))

    
  nrow(rmse_var)
  nrow(ntries_hmm)

}
?full_join

is.numeric(NA)


sum(!is.na(rmse_var$var_mean_rmse))/nrow(rmse_var)


sum(a$var_mean_rmse)/nrow(a)
