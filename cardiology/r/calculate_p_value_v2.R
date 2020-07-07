library(data.table)
library(tidyverse)
library(lubridate)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

setwd("E:/Users/DLCG001/workspace/cardiology")
# Sys.setlocale(category = "LC_ALL", locale = "English")

output_path <- file.path("output", "calculate_p_value")
dir.create(output_path)

# Read and Process Data ---------------------------------------------------------------
d_echo_0 <- fread("data/echo_data.csv")
d_lab_0 <- fread("data/lab_data.csv")

d_echo_1 <- d_echo_0 %>% 
    mutate(subjectId = as.character(PERSON_ID),
           gender = factor(ifelse(GENDER_CONCEPT_ID == 8507, "Male", "Female")),
           cohortStartDate = ymd(MEASUREMENT_DATE),
           STATIN_DATE = ymd(STATIN_DATE),
           RAS_DATE = ymd(RAS_DATE),
           BIS_DATE = ymd(BIS_DATE),
           statin = ifelse(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           ras = ifelse(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           B_DATE = ymd(B_DATE),
           A_DATE = ymd(A_DATE),
           baseline = ifelse(!is.na(A_DATE), TRUE, FALSE),
           follow_up = ifelse(!is.na(B_DATE), TRUE, FALSE),
           MEASUREMENT_SOURCE_VALUE  = replace(MEASUREMENT_SOURCE_VALUE, MEASUREMENT_SOURCE_VALUE == 210030, "HR"),
           age = year(cohortStartDate) - YEAR_OF_BIRTH) %>% 
    select(subjectId, age, gender, MEASUREMENT_SOURCE_VALUE, cohortStartDate, VALUE_AS_NUMBER,
           statin, ras, follow_up, baseline, GAP_DAY, GAP_VALUE) %>% 
    as.data.table()

d_lab_1 <- d_lab_0 %>% 
    mutate(subjectId = as.character(PERSON_ID),
           gender = factor(ifelse(GENDER_CONCEPT_ID == 8507, "Male", "Female")),
           MEASUREMENT_DATE = ymd(MEASUREMENT_DATE),
           COHORT_START_DATE = ymd(COHORT_START_DATE),
           STATIN_DATE = ymd(STATIN_DATE),
           RAS_DATE = ymd(RAS_DATE),
           BIS_DATE = ymd(BIS_DATE),
           statin = ifelse(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           ras = ifelse(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, FALSE, TRUE)) %>% 
    select(subjectId, gender, CONCEPT_NAME, MEASUREMENT_DATE,
           VALUE_AS_NUMBER, statin, ras) %>% 
    as.data.table()

v_drug <- c("statin", "ras")

v_anthropometrics <- distinct(d_echo_1, MEASUREMENT_SOURCE_VALUE)[
    c(15, 7, 13, 21, 20, 26), MEASUREMENT_SOURCE_VALUE
    ]
v_echo_1 <- distinct(d_echo_1, MEASUREMENT_SOURCE_VALUE)[
    c(4, 11, 10, 9, 1, 8, 12, 3, 14, 5, 6, 16, 18, 19, 17, 23, 24, 22, 25, 2),
    MEASUREMENT_SOURCE_VALUE
    ]
v_echo_2 <- v_echo_1[c(10, 11, 12, 5, 19, 9, 20, 7)]
v_lab <- distinct(d_lab_1, CONCEPT_NAME)[
    c(1, 2, 7, 8, 6, 5, 16, 3, 14, 17, 4, 12, 11, 13, 10, 15, 9), CONCEPT_NAME
    ]

v_cohort_information <- distinct(d_echo_1, subjectId, cohortStartDate, gender, statin, ras, bis)

for (var_drug in v_drug) {
    assign(paste(var_drug, "matching_data", sep = "_"),
           fread(file.path("output/psm/1on1",
                           paste0(var_drug, "_cov1"),
                           paste0(var_drug, "_Matching_Data.csv"))) %>% 
               select(subjectId, treatment, cohortStartDate) %>% 
               mutate(subjectId = as.character(subjectId),
                      treatment = ifelse(treatment == FALSE, TRUE, FALSE),
                      cohortStartDate = ymd(cohortStartDate)))
}


# Calculate p-value -------------------------------------------------------
sink(file.path(output_path, "calculate_p_value.html"))
for (var_matching in c(FALSE, TRUE)) {
    for (var_drug in v_drug) {
        if (var_matching == TRUE) {
            d_echo_2 <- get(paste0(var_drug, "_matching_data")) %>% 
                inner_join(d_echo_1, by = c("subjectId",
                                            "cohortStartDate",
                                            "treatment" = var_drug)) %>% 
                as.data.table()
            
            d_lab_2 <- get(paste0(var_drug, "_matching_data")) %>% 
                inner_join(d_lab_1, by = c("subjectId",
                                           "cohortStartDate" = "MEASUREMENT_DATE",
                                           "treatment" = var_drug)) %>% 
                as.data.table()
            
            prefix <- "PS matching"
            
            # Sheet 3 -----------------------------------------------------------------
            for (var_echo in v_echo_1) {
                try(print_t_test(title = paste("Baseline", var_drug, var_echo, "t.test"),
                                 x = d_echo_2[treatment == FALSE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  baseline == TRUE,
                                              VALUE_AS_NUMBER],
                                 y = d_echo_2[treatment == TRUE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  baseline == TRUE,
                                              VALUE_AS_NUMBER]))
                
                try(print_t_test(title = paste("Follow-up", var_drug, var_echo, "t.test"),
                                 x = d_echo_2[treatment == FALSE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              VALUE_AS_NUMBER],
                                 y = d_echo_2[treatment == TRUE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              VALUE_AS_NUMBER]))
            }
            
            # Sheet 4 -----------------------------------------------------------------
            try(print_t_test(title = paste("Echo interval", var_drug, "t.test"),
                             x = d_echo_2[treatment == FALSE &
                                              follow_up == TRUE,
                                          GAP_DAY / 365],
                             y = d_echo_2[treatment == TRUE &
                                              follow_up == TRUE,
                                          GAP_DAY / 365]))
            for (var_echo in v_echo_2) {
                try(print_t_test(title = paste("Progression(difference)", var_drug, var_echo, "t.test"),
                                 x = d_echo_2[treatment == FALSE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              GAP_VALUE],
                                 y = d_echo_2[treatment == TRUE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              GAP_VALUE]))
                try(print_t_test(title = paste("Progression(per year)", var_drug, var_echo, "t.test"),
                                 x = d_echo_2[treatment == FALSE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              GAP_VALUE / GAP_DAY * 365],
                                 y = d_echo_2[treatment == TRUE &
                                                  MEASUREMENT_SOURCE_VALUE == var_echo &
                                                  follow_up == TRUE,
                                              GAP_VALUE / GAP_DAY * 365]))
            }
        } else {
            d_echo_2 <- d_echo_1 %>% 
                rename(treatment := !!var_drug)
            d_lab_2 <- d_lab_1 %>% 
                rename(treatment := !!var_drug)
            
            prefix <- ""
        }
        
        # Sheet 1 -----------------------------------------------------------------
        # Age
        d_age <- d_echo_2 %>% 
            group_by(subjectId, treatment, cohortStartDate) %>%
            summarize(age = mean(age)) %>%
            as.data.table()
        d_gender <- distinct(d_echo_2, subjectId, gender, treatment)
        
        try(print_t_test(title = paste(prefix, "Age", var_drug, "t.test"),
                         x = d_age[treatment == FALSE, age],
                         y = d_age[treatment == TRUE, age]))
        
        # Gender
        try(print_chisq_test(title = paste(prefix, "Sex", var_drug, "Chi-square test"),
                             x = matrix(c(table(d_gender[treatment == FALSE, gender])[[1]],
                                          table(d_gender[treatment == FALSE, gender])[[2]],
                                          table(d_gender[treatment == TRUE, gender])[[1]],
                                          table(d_gender[treatment == TRUE, gender])[[2]]),
                                        ncol = 2)))
        
        # Anthropometrics
        for(var_am in v_anthropometrics) {
            try(print_t_test(title = paste(prefix, var_drug, var_am, "t.test"),
                             x = d_echo_2[treatment == FALSE & MEASUREMENT_SOURCE_VALUE == var_am, VALUE_AS_NUMBER],
                             y = d_echo_2[treatment == TRUE & MEASUREMENT_SOURCE_VALUE == var_am, VALUE_AS_NUMBER]))
        }
        # Lab test
        for (var_lab in v_lab) {
            try(print_t_test(title = paste(prefix, var_drug, var_lab, "t.test"),
                             x = d_lab_2[treatment == FALSE & CONCEPT_NAME == var_lab, VALUE_AS_NUMBER],
                             y = d_lab_2[treatment == TRUE & CONCEPT_NAME == var_lab, VALUE_AS_NUMBER]))
        }
    }
}
sink()
