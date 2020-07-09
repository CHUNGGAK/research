library(tidyverse)
library(data.table)

vec_balance <- list.files("/home/lee/workspace/respiratory/output/2020-06-22_08:18:23/shiny_data/balance",
                          full.names = TRUE)

excluded_covariates <- data.table()

for (var_file in vec_balance) {
    excluded_covariates <- rbind(excluded_covariates,
                             fread(var_file) %>% 
                                 filter(abs(afterMatchingStdDiff) > 0.1) %>% 
                                 select(covariateName, conceptId))
}

excluded_covariates <- excluded_covariates %>% 
    distinct()

write_csv(excluded_covariates, "/home/lee/workspace/respiratory/output/excluded_covariates_3.csv")
