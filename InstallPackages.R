packages <- c("tidyverse","Rcpp","rstan","rstanarm", "magrittr","gridExtra","ggmcmc","MCMCpack","ggpubr","Matrix","reshape2")

installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages,library,character.only=T))
