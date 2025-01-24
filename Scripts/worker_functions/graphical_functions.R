


clean_dataset_names <- function(dataset_names){
  dataset_names %>%
    gsub(pattern = "_", replacement = " ") %>%
    gsub(pattern = "(\\d{4})", replacement = "(\\1)") %>%
    gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2")  %>%
    gsub(pattern = ") ds1", replacement = "a)")%>%
    gsub(pattern = ") ds2", replacement = "b)")
}


clean_model_names <- function(model_names){
  model_names %>%
    gsub(pattern = "_", replacement = " ") %>% 
    gsub(pattern = "all equal", replacement = "Equal RMSE") %>%
    gsub(pattern = "\\b(\\w)", replacement = "\\U\\1", perl = TRUE)
}


