# Set environment ---------------------------------------------------------
setwd("E:/Users/DLCG001/workspace/geriatric")
Sys.setlocale(category = "LC_ALL", locale = "English")

library(data.table)
library(tidyverse)
library(PatientLevelPrediction)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

output_path <- file.path("output")
path_assistant(output_path)

antidepressant_exposure_count <- fread("data/antidepressant_exposure_count.csv") %>% 
    select(PERSON_ID, DRUG_CONCEPT_ID, DRUG_EXPOSURE_COUNT) %>% 
    mutate(PERSON_ID = as.character(PERSON_ID),
           DRUG_CONCEPT_ID = as.character(DRUG_CONCEPT_ID)) %>% 
    group_by(PERSON_ID) %>% 
    summarise(antidepressant_exposure_count = sum(DRUG_EXPOSURE_COUNT))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

cohortTable <- "cohort"
cohortId <- 803
outcomeTable <- "lcg_cohort"
outcomeIds <- 24470006


# Create condition covariate ----------------------------------------------
condition_concept_id <- 440383 # depressive disorder
condition_covariateSettings <- createCovariateSettings(useConditionOccurrenceLongTerm = TRUE,
                                                       includedCovariateConceptIds = condition_concept_id,
                                                       addDescendantsToInclude = TRUE)

condition_data <- getPlpData(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortId = cohortId,
                             outcomeIds = outcomeIds,
                             studyStartDate = "",
                             studyEndDate = "",
                             oracleTempSchema = oracleTempSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             outcomeDatabaseSchema = outcomeDatabaseSchema,
                             outcomeTable = outcomeTable,
                             firstExposureOnly = FALSE,
                             washoutPeriod = 0,
                             covariateSettings = condition_covariateSettings)

cohorts <- condition_data$cohorts %>% 
    select(rowId, subjectId) %>% 
    mutate(rowId = as.character(rowId),
           subjectId = as.character(subjectId))
outcomes <- condition_data$outcomes %>% 
    filter(daysToEvent >= 0 & daysToEvent <= 30) %>% 
    select(rowId) %>% 
    mutate(rowId = as.character(rowId),
           outcome_value = TRUE)
condition_covariates <- condition_data$covariates %>% 
    as.data.frame() %>% 
    distinct(rowId) %>% 
    mutate(rowId = as.character(rowId),
           depressive_disorder_value = TRUE)


# Create score covariate --------------------------------------------------
score_covariateSettings <- createCovariateSettings(useChads2 = TRUE,
                                                   useChads2Vasc = TRUE,
                                                   useDcsi = TRUE,
                                                   useCharlsonIndex = TRUE)

score_data <- getPlpData(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortId = cohortId,
                         outcomeIds = outcomeIds,
                         studyStartDate = "",
                         studyEndDate = "",
                         oracleTempSchema = oracleTempSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         outcomeDatabaseSchema = outcomeDatabaseSchema,
                         outcomeTable = outcomeTable,
                         firstExposureOnly = FALSE,
                         washoutPeriod = 0,
                         covariateSettings = score_covariateSettings)

score_covariates <- score_data$covariates %>% 
    as.data.frame() %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 1901, "charlson_index", 
                                ifelse(covariateId == 1902, "dcsi",
                                       ifelse(covariateId == 1903, "chads2", "chads2vasc")))) %>% 
    spread(key = covariateId, value = covariateValue)


# Create gender covariate -------------------------------------------------
gender_covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE)

gender_data <- getPlpData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortId = cohortId,
                          outcomeIds = outcomeIds,
                          studyStartDate = "",
                          studyEndDate = "",
                          oracleTempSchema = oracleTempSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                          outcomeTable = outcomeTable,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          covariateSettings = gender_covariateSettings)
gender_covariates <- as.data.frame(gender_data$covariates) %>% 
    select(rowId, covariateId) %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 8507001, TRUE, FALSE)) %>% 
    rename(male = covariateId)


# Create visit covariate --------------------------------------------------
visit_covariateSettings <- createCovariateSettings(useVisitConceptCountLongTerm = TRUE)

visit_data <- getPlpData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortId = cohortId,
                          outcomeIds = outcomeIds,
                          studyStartDate = "",
                          studyEndDate = "",
                          oracleTempSchema = oracleTempSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                          outcomeTable = outcomeTable,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          covariateSettings = visit_covariateSettings)

visit_covariates <- visit_data$covariates %>% 
    as.data.frame() %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 9201923, "inpatient",
                                ifelse(covariateId == 9202923, "outpatient",
                                       ifelse(covariateId == 9203923, "emergency", "laboratory")))) %>% 
    spread(key = covariateId, value = covariateValue)


# Join data ---------------------------------------------------------------
datum <- cohorts %>% 
    left_join(gender_covariates, by = "rowId") %>% 
    left_join(antidepressant_exposure_count, by = c("subjectId" = "PERSON_ID")) %>% 
    left_join(condition_covariates, by = "rowId") %>% 
    left_join(score_covariates, by = "rowId") %>% 
    left_join(visit_covariates, by = "rowId") %>% 
    left_join(outcomes, by = "rowId") %>% 
    replace_na(list(antidepressant_exposure_count = 0,
                    depressive_disorder_value = FALSE,
                    charlson_index = 0,
                    dcsu = 0,
                    emergency = 0,
                    laboratory = 0,
                    outpatient = 0,
                    outcome_value = FALSE))
write_csv(datum, "data/information_for_logistic_regressoin.csv")


# Preprocess data ---------------------------------------------------------
p_datum <- datum %>% 
    mutate(antidepressant_exposure_count = remove_outlier_with_qti(antidepressant_exposure_count),
           laboratory = remove_outlier_with_qti(laboratory),
           outpatient = remove_outlier_with_qti(outpatient))


# Check data attributes ---------------------------------------------------
density_plot_path <- file.path(output_path, "indep_var_attribute")
path_assistant(density_plot_path)

continuous_indep <- p_datum %>% 
    select(-c(rowId, subjectId, male, depressive_disorder_value, outcome_value))
for (i in colnames(continuous_indep)) {
    ggplot(aes_string(x = i), data = continuous_indep) + 
        geom_histogram()
    ggsave(file.path(density_plot_path, paste0("preprocced_", i, "_histogram.png")))
}

continuous_indep <- datum %>% 
    select(-c(rowId, subjectId, male, depressive_disorder_value, outcome_value))
for (i in colnames(continuous_indep)) {
    ggplot(aes_string(x = i), data = continuous_indep) + 
        geom_histogram()
    ggsave(file.path(density_plot_path, paste0(i, "_histogram.png")))
}


# Analysis ----------------------------------------------------------------
split_index <- sample(1:nrow(datum), size = round(nrow(datum) * 0.25))
train <- datum[split_index, ]
test <- datum[-split_index, ]

fit <- glm(outcomeValue ~ male + antidepressant_exposure_count + depressive_disorder_value + 
               chads2 + chads2vasc + charlson_index + dcsi + inpatient + outpatient +
               emergency + laboratory, family = "binomial", data = train)
p <- predict(fit, newdata = test, type = "response")
pr <- prediction(p, test$outcomeValue)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")

png("roc_plot.png")
plot(prf)
lines(x = c(0, 1), y = c(0, 1))
dev.off()
