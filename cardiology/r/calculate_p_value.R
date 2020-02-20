library(data.table)
library(tidyverse)
library(xtable)

setwd("E:/Users/DLCG001/workspace/cardiology")


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
           follow_up = ifelse(!is.na(A_DATE), TRUE, FALSE),
           baseline = ifelse(!is.na(B_DATE), TRUE, FALSE)) %>% 
    select(PERSON_ID, AGE, male, MEASUREMENT_CONCEPT_ID, MEASUREMENT_SOURCE_VALUE,
           MEASUREMENT_DATE, VALUE_AS_NUMBER, statin, ras, bis, GAP_DAY, GAP_VALUE,
           follow_up, baseline) %>% 
    as.data.table()

lab_data <- fread("data/lab_data.csv") %>% 
    mutate(PERSON_ID = as.character(PERSON_ID),
           male = ifelse(GENDER_CONCEPT_ID == 8507, TRUE, FALSE),
           MEASUREMENT_DATE = as.Date(MEASUREMENT_DATE, "%Y/%m/%d"),
           STATIN_DATE = as.Date(STATIN_DATE, "%Y/%m/%d"),
           RAS_DATE = as.Date(RAS_DATE, "%Y/%m/%d"),
           BIS_DATE = as.Date(BIS_DATE, "%Y/%m/%d"),
           statin = ifelse(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           ras = ifelse(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, TRUE, FALSE),
           bis = ifelse(is.na(BIS_DATE) | BIS_DATE > MEASUREMENT_DATE, TRUE, FALSE)) %>% 
    select(PERSON_ID, AGE, male, MEASUREMENT_CONCEPT_ID, CONCEPT_NAME, VALUE_AS_NUMBER,
           statin, ras, bis) %>% 
    as.data.table()

drug_list <- c("statin", "ras", "bis")
echo_list <- distinct(echo_data, MEASUREMENT_CONCEPT_ID, MEASUREMENT_SOURCE_VALUE)
lab_list <- distinct(lab_data, MEASUREMENT_CONCEPT_ID, CONCEPT_NAME)

for (drug_var in drug_list) {
    assign(paste(drug_var, "matching_data", sep = "_"),
           fread(file.path("output/propensity_score_matching",
                           drug_var,
                           "matching_data",
                           paste(drug_var, "matching_group.csv", sep ="_"))) %>% 
               select(subjectId, treatment, cohortStartDate) %>% 
               mutate(subjectId = as.character(subjectId),
                      treatment = as.logical(treatment),
                      cohortStartDate = as.Date(cohortStartDate, "%Y-%m-%d")))
}


# Calculate p-value -------------------------------------------------------
for (matching_var in c(FALSE, TRUE)) {
    p_echo_data <- echo_data
    p_lab_data <- lab_data
    
    for (drug_var in drug_list) {
        if (matching_var == TRUE) {
            p_echo_data <- get(paste(drug_var, "matching_data", sep = "_")) %>% 
                inner_join(echo_data, by = c("subjectId" = "PERSON_ID",
                                             "cohortStartDate" = "MEASUREMENT_DATE",
                                             "treatment" = drug_var))
            p_lab_data <- get(paste(drug_var, "matching_data", sep = "_")) %>% 
                inner_join(lab_data, by = c("subjectId" = "PERSON_ID",
                                            "cohortStartDate" = "MEASUREMENT_DATE",
                                            "treatment" = drug_var))
        }
        for (echo_var in echo_list$MEASUREMENT_CONCEPT_ID) {
            cat(paste(drug_var,
                      measurement_list$MEASUREMENT_SOURCE_VALUE[match(echo_var, echo_list$MEASUREMENT_CONCEPT_ID)],
                      "t.test result"))
            try(print_t_test(p_echo_data[get(drug_var) == 0 & MEASUREMENT_CONCEPT_ID == echo_var, VALUE_AS_NUMBER],
                             p_echo_data[get(drug_var) == 1 & MEASUREMENT_CONCEPT_ID == echo_var, VALUE_AS_NUMBER]))
            cat(rep("<br>", 2))
        }
        for (lab_var in lab_list$MEASUREMENT_CONCEPT_ID) {
            cat(paste(drug_var,
                      lab_list$CONCEPT_NAME[match(lab_var, lab_list$MEASUREMENT_CONCEPT_ID)],
                      "t.test result"))
            try(print_t_test(p_lab_data[get(drug_var) == 0 & MEASUREMENT_CONCEPT_ID == lab_var, VALUE_AS_NUMBER],
                             p_lab_data[get(drug_var) == 1 & MEASUREMENT_CONCEPT_ID == lab_var, VALUE_AS_NUMBER]))
            cat(rep("<br>", 2))
        }
    }
}

