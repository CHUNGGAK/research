setwd("E:/Users/DLCG001/workspace/respiratory")

library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(rms)

d <- fread("data/deathCohort.csv") %>% 
    mutate(COHORT_DEFINITION_ID = as.character(COHORT_DEFINITION_ID),
           SUBJECT_ID = as.character(SUBJECT_ID),
           COHORT_START_DATE = ymd(COHORT_START_DATE),
           COHORT_END_DATE = ymd(COHORT_END_DATE),
           DEATH_DATE = ymd(DEATH_DATE),
           CAUSE_CONCEPT_ID = as.character(CAUSE_CONCEPT_ID),
           death = ifelse(DEATH_DATE >= COHORT_START_DATE - 2 &
                              DEATH_DATE <= COHORT_END_DATE, TRUE, FALSE),
           death = replace_na(death, FALSE),
           days_to_cohort_end = COHORT_END_DATE - COHORT_START_DATE,
           survival_time = ifelse(death == TRUE, DEATH_DATE - COHORT_START_DATE, max(days_to_cohort_end)),
           drug = ifelse(COHORT_DEFINITION_ID == 831, "PPI", "H2RA"))

d$surv_obj <- with(d, Surv(survival_time, death == TRUE))

km <- npsurv(formula = surv_obj ~ drug, data = d)
survplot(fit = km,
         xlim = c(0, 30),
         time.inc = 3,
         dots = TRUE,
         n.risk = TRUE)
