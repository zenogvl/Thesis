
### Install and load all required packages. 

install.packages(setdiff(c("depmixS4",
                           "gtools",
                           "haven", 
                           "imputeTS", 
                           "Metrics",
                           "mgm", 
                           "parallel",
                           "readxl",
                           "rlist",
                           "stats", 
                           "tidyverse",
                           "vars",
                           "wesanderson"
                           ), rownames(installed.packages())))  
library(tidyverse)


for(f in list.files("Scripts/worker_functions/")){
  source(paste0("Scripts/worker_functions/", f))
}



standard_figure_with <- 5.7


