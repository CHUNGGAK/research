# Set environment ---------------------------------------------------------
setwd("E:/Users/DLCG001/workspace/geriatric")
Sys.setlocale(category = "LC_ALL", locale = "English")

library(data.table)
library(tidyverse)
library(PatientLevelPrediction)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

output_path <- file.path("output", "logistic_regression")
path_assistant(output_path)

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

cohortTable <- "cohort"
cohortId <- 803
outcomeTable <- "lcg_cohort"
outcomeIds <- 24470006


# Create covariate settings ----------------------------------------------
plp_data_path <- file.path(output_path, "plp_data")
path_assistant(plp_data_path)

condition_concept_ids <- 440383 # depressive disorder
condition_covariateSettings <- createCovariateSettings(useConditionOccurrenceLongTerm = TRUE,
                                                       includedCovariateConceptIds = condition_concept_ids,
                                                       addDescendantsToInclude = TRUE)

score_covariateSettings <- createCovariateSettings(useChads2 = TRUE,
                                                   useChads2Vasc = TRUE,
                                                   useDcsi = TRUE,
                                                   useCharlsonIndex = TRUE)

gender_covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE)

visit_covariateSettings <- createCovariateSettings(useVisitConceptCountLongTerm = TRUE)

measurement_concept_ids <- c(
    4177340, # height
    4099154 # weight
)
measurement_covariateSettings <- createCovariateSettings(useMeasurementValueLongTerm = TRUE,
                                                         includedCovariateConceptIds = measurement_concept_ids)

drug_covariateSettings <- createCovariateSettings(useDistinctIngredientCountLongTerm = TRUE)


# Extract covariate data ---------------------------------------------------
for (i in c("condition", "score", "gender", "visit", "measurement", "drug")) {
    if (!file.exists(file.path(plp_data_path, paste0(i, "_data"), "cohorts.rds"))) {
        assign(paste0(i, "_data"), getPlpData(connectionDetails = connectionDetails,
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
                                              covariateSettings = get(paste0(i, "_covariateSettings"))))
    } else {
        assign(paste0(i, "_data"), loadPlpData(file.path(plp_data_path, paste0(i, "_data"))))
    }
}


# Preprocess cohorts ------------------------------------------------------
cohorts <- condition_data$cohorts %>% 
    select(rowId, subjectId) %>% 
    mutate(rowId = as.character(rowId),
           subjectId = as.character(subjectId))
outcomes <- condition_data$outcomes %>% 
    filter(daysToEvent >= 0 & daysToEvent <= 30) %>% 
    select(rowId) %>% 
    mutate(rowId = as.character(rowId),
           outcome_value = TRUE)


# Preprocess depressive disorder ------------------------------------------
depressive_disorder <- cohorts %>%
    left_join(
        fread("data/antidepressant_exposure_count.csv") %>% 
            mutate(PERSON_ID = as.character(PERSON_ID)) %>% 
            group_by(PERSON_ID) %>% 
            summarise(drug_exposure_count = sum(DRUG_EXPOSURE_COUNT)) %>% 
            filter(drug_exposure_count >= 7) %>% 
            select(PERSON_ID) %>% 
            mutate(condition_value = TRUE),
        by = c("subjectId" = "PERSON_ID")
    ) %>% 
    left_join(
        condition_data$covariates %>% 
            as.data.frame() %>% 
            mutate(rowId = as.character(rowId)) %>% 
            group_by(rowId) %>% 
            summarise(condition_count = sum(covariateValue)) %>% 
            filter(condition_count >= 2) %>% 
            select(rowId) %>% 
            mutate(drug_value = TRUE),
        by = "rowId"
    ) %>% 
    replace_na(list(condition_value = FALSE, drug_value = FALSE)) %>% 
    mutate(depressive_disorder = condition_value | drug_value) %>% 
    select(rowId, depressive_disorder)


# Preprocess score covariate --------------------------------------------------
score_covariates <- score_data$covariates %>% 
    as.data.frame() %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 1901, "charlson_index", 
                                ifelse(covariateId == 1902, "dcsi",
                                       ifelse(covariateId == 1903, "chads2", "chads2vasc")))) %>% 
    spread(key = covariateId, value = covariateValue)


# Preprocess gender covariate -------------------------------------------------
gender_covariates <- as.data.frame(gender_data$covariates) %>% 
    select(rowId, covariateId) %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 8507001, TRUE, FALSE)) %>% 
    rename(male = covariateId)


# Preprocess visit covariate --------------------------------------------------
visit_covariates <- visit_data$covariates %>% 
    as.data.frame() %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 9201923, "inpatient",
                                ifelse(covariateId == 9202923, "outpatient",
                                       ifelse(covariateId == 9203923, "emergency", "laboratory")))) %>% 
    spread(key = covariateId, value = covariateValue)


# Preprocess measurement covariate --------------------------------------------
measurement_covariates <- measurement_data$covariates %>% 
    as.data.frame() %>% 
    mutate(rowId = as.character(rowId),
           covariateId = ifelse(covariateId == 4177340582706, "height", "weight")) %>% 
    spread(key = covariateId, value = covariateValue)


# Preprocess drug covariate ---------------------------------------------------
drug_covariates <- drug_data$covariates %>% 
    as.data.frame() %>% 
    select(rowId, covariateValue) %>% 
    mutate(rowId = as.character(rowId)) %>% 
    rename(ingredient_count = covariateValue)


# Join data ---------------------------------------------------------------
datum <- cohorts %>% 
    left_join(gender_covariates, by = "rowId") %>% 
    left_join(depressive_disorder, by = "rowId") %>% 
    left_join(condition_covariates, by = "rowId") %>% 
    left_join(score_covariates, by = "rowId") %>% 
    left_join(visit_covariates, by = "rowId") %>% 
    left_join(measurement_covariates, by = "rowId") %>% 
    left_join(drug_covariates, by = "rowId") %>% 
    left_join(outcomes, by = "rowId") %>%
    replace_na(list(antidepressant_exposure_count = 0,
                    depressive_disorder = FALSE,
                    charlson_index = 0,
                    dcsi = 0,
                    emergency = 0,
                    laboratory = 0,
                    outpatient = 0,
                    ingredient_count = 0,
                    outcome_value = FALSE))
write_csv(datum, "data/information_for_logistic_regressoin.csv")


# Preprocess data ---------------------------------------------------------
p_datum <- datum %>% 
    mutate(laboratory = remove_outlier_with_qti(laboratory),
           outpatient = remove_outlier_with_qti(outpatient),
           height = remove_outlier_with_qti(height),
           weight = remove_outlier_with_qti(weight),
           ingredient_count = remove_outlier_with_qti(ingredient_count))


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

fit <- glm(outcome_value ~ male + depressive_disorder + chads2 + chads2vasc +
               charlson_index + dcsi + inpatient + outpatient + emergency +
               laboratory + height + weight + ingredient_count, family = "binomial", data = train)
p <- predict(fit, newdata = test, type = "response")
pr <- prediction(p, test$outcome_value)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")

png(file.path(output_path, "roc_plot.png"))
plot(prf)
lines(x = c(0, 1), y = c(0, 1))
text(0.9, 0.1, labels = paste("AUC =", round(auc@y.values[[1]], 4)))
dev.off()
