library(data.table)
library(tidyverse)
library(xtable)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

setwd("E:/Users/DLCG001/workspace/cardiology")
output_path <- file.path("output", "calculated_p_value")
path_assistant(output_path)

# Read data ---------------------------------------------------------------
echo_data <- fread("data/echo_data.csv") %>% 
    mutate(PERSON_ID = as.character(PERSON_ID),
           male = ifelse(GENDER_CONCEPT_ID == 8507, TRUE, FALSE),
           MEASUREMENT_DATE = as.Date(MEASUREMENT_DATE, "%Y/%m/%d"),
           STATIN_DATE = as.Date(STATIN_DATE, "%Y/%m/%d"),
           RAS_DATE = as.Date(RAS_DATE, "%Y/%m/%d"),
           BIS_DATE = as.Date(BIS_DATE, "%Y/%m/%d"),
           statin = ifelse(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           ras = ifelse(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           bis = ifelse(is.na(BIS_DATE) | BIS_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           B_DATE = as.Date(B_DATE, "%Y/%m/%d"),
           A_DATE = as.Date(A_DATE, "%Y/%m/%d"),
           baseline = ifelse(!is.na(A_DATE), TRUE, FALSE),
           follow_up = ifelse(!is.na(B_DATE), TRUE, FALSE),
           MEASUREMENT_SOURCE_VALUE  = replace(MEASUREMENT_SOURCE_VALUE, MEASUREMENT_SOURCE_VALUE == 210030, "HR")) %>% 
    select(PERSON_ID, AGE, male, MEASUREMENT_SOURCE_VALUE, MEASUREMENT_DATE,
           VALUE_AS_NUMBER, statin, ras, bis, GAP_DAY, GAP_VALUE, follow_up, baseline) %>% 
    as.data.table()

lab_data <- fread("data/lab_data.csv") %>% 
    mutate(PERSON_ID = as.character(PERSON_ID),
           male = ifelse(GENDER_CONCEPT_ID == 8507, TRUE, FALSE),
           MEASUREMENT_DATE = as.Date(MEASUREMENT_DATE, "%Y/%m/%d"),
           COHORT_START_DATE = as.Date(COHORT_START_DATE, "%Y/%m/%d"),
           STATIN_DATE = as.Date(STATIN_DATE, "%Y/%m/%d"),
           RAS_DATE = as.Date(RAS_DATE, "%Y/%m/%d"),
           BIS_DATE = as.Date(BIS_DATE, "%Y/%m/%d"),
           statin = ifelse(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           ras = ifelse(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           bis = ifelse(is.na(BIS_DATE) | BIS_DATE > MEASUREMENT_DATE, TRUE, FALSE)) %>% 
    select(PERSON_ID, AGE, male, MEASUREMENT_SOURCE_VALUE, COHORT_START_DATE,
           VALUE_AS_NUMBER, statin, ras, bis) %>% 
    as.data.table()

drug_list <- c("statin", "ras", "bis")

anthropometrics_list <- distinct(echo_data, MEASUREMENT_SOURCE_VALUE)[
    c(15, 7, 13, 21, 20, 26), MEASUREMENT_SOURCE_VALUE]
echo_list1 <- distinct(echo_data, MEASUREMENT_SOURCE_VALUE)[
    c(4, 11, 10, 9, 1, 8, 12, 3, 14, 5, 6, 16, 18, 19, 17, 23, 24, 22, 25, 2),
    MEASUREMENT_SOURCE_VALUE]
echo_list2 <- echo_list1[c(10, 11, 12, 5, 19, 9, 20, 7)]
lab_list <- distinct(lab_data, MEASUREMENT_SOURCE_VALUE)[
    c(1, 2, 7, 8, 6, 5, 16, 3, 14, 17, 4, 12, 11, 13, 10, 15, 9), MEASUREMENT_SOURCE_VALUE]

for (drug_var in drug_list) {
    assign(paste(drug_var, "matching_data", sep = "_"),
           fread(file.path("output/propensity_score_matching",
                           drug_var,
                           "matching_data",
                           paste(drug_var, "matching_group.csv", sep ="_"))) %>% 
               select(subjectId, treatment, cohortStartDate) %>% 
               mutate(subjectId = as.character(subjectId),
                      treatment = ifelse(treatment == 0, TRUE, FALSE),
                      cohortStartDate = as.Date(cohortStartDate, "%Y-%m-%d")))
}


# Calculate p-value -------------------------------------------------------
sink(file.path(output_path, "calculated_p_value.html"))
for (matching_var in c(FALSE, TRUE)) {
    for (drug_var in drug_list) {
        if (matching_var == TRUE) {
            p_echo_data <- get(paste(drug_var, "matching_data", sep = "_")) %>% 
                inner_join(echo_data, by = c("subjectId" = "PERSON_ID",
                                             "cohortStartDate" = "MEASUREMENT_DATE",
                                             "treatment" = drug_var)) %>% 
                as.data.table()
            p_lab_data <- get(paste(drug_var, "matching_data", sep = "_")) %>% 
                inner_join(lab_data, by = c("subjectId" = "PERSON_ID",
                                            "cohortStartDate" = "COHORT_START_DATE",
                                            "treatment" = drug_var)) %>% 
                as.data.table()
            
            prefix <- "PS matching"
            
            # Sheet 3 -----------------------------------------------------------------
            for (echo_var in echo_list1) {
                try(print_t_test(title = paste("Baseline", drug_var, echo_var, "t.test"),
                                 x = p_echo_data[treatment == 0 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     baseline == TRUE,
                                                 VALUE_AS_NUMBER],
                                 y = p_echo_data[treatment == 1 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     baseline == TRUE,
                                                 VALUE_AS_NUMBER]))
                
                try(print_t_test(title = paste("Follow-up", drug_var, echo_var, "t.test"),
                                 x = p_echo_data[treatment == 0 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     follow_up == TRUE,
                                                 VALUE_AS_NUMBER],
                                 y = p_echo_data[treatment == 1 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     follow_up == TRUE,
                                                 VALUE_AS_NUMBER]))
            }
            
            # Sheet 4 -----------------------------------------------------------------
            for (echo_var in echo_list2) {
                try(print_t_test(title = paste("Progression", drug_var, echo_var, "t.test"),
                                 x = p_echo_data[treatment == 0 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     baseline == TRUE & 
                                                     follow_up == TRUE,
                                                 GAP_VALUE / GAP_DAY * 365],
                                 y = p_echo_data[treatment == 1 &
                                                     MEASUREMENT_SOURCE_VALUE == echo_var &
                                                     baseline == TRUE &
                                                     follow_up == TRUE,
                                                 GAP_VALUE / GAP_DAY * 365]))
            }
        } else {
            p_echo_data <- echo_data %>% 
                rename(treatment := !!drug_var)
            p_lab_data <- lab_data %>% 
                rename(treatment := !!drug_var)
            
            prefix <- ""
        }
        
        # Sheet 1 and 2-----------------------------------------------------------------
        # Age
        demographics_data <- p_echo_data %>% 
            group_by(subjectId) %>% 
        try(print_t_test(title = paste(prefix, "Age", drug_var, "t.test"),
                         x = p_echo_data[treatment == 0, AGE],
                         y = p_echo_data[treatment == 1, AGE]))
        
        # Sex
        try(print_chisq_test(title = paste(prefix, "Sex", drug_var, "Chi-square test"),
                         x = matrix(c(table(p_echo_data[treatment == 0, male])[[1]],
                                      table(p_echo_data[treatment == 0, male])[[2]],
                                      table(p_echo_data[treatment == 1, male])[[1]],
                                      table(p_echo_data[treatment == 1, male])[[2]]),
                                    ncol = 2)))

        # Anthropometrics
        for(am_var in anthropometrics_list) {
            try(print_t_test(title = paste(prefix, drug_var, am_var, "t.test"),
                             x = p_echo_data[treatment == 0 & MEASUREMENT_SOURCE_VALUE == am_var, VALUE_AS_NUMBER],
                             y = p_echo_data[treatment == 1 & MEASUREMENT_SOURCE_VALUE == am_var, VALUE_AS_NUMBER]))
        }
        # Lab test
        for (lab_var in lab_list) {
            try(print_t_test(title = paste(prefix, drug_var, lab_var, "t.test"),
                             x = p_lab_data[treatment == 0 & MEASUREMENT_SOURCE_VALUE == lab_var, VALUE_AS_NUMBER],
                             y = p_lab_data[treatment == 1 & MEASUREMENT_SOURCE_VALUE == lab_var, VALUE_AS_NUMBER]))
        }
    }
}
sink()
