# Set environment ---------------------------------------------------------
setwd("E:/Users/DLCG001/workspace/geriatric")
Sys.setlocale(category = "LC_ALL", locale = "English")

library(data.table)
library(tidyverse)
library(PatientLevelPrediction)
library(xtable)
library(ROCR)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

output_path <- file.path("output", "logistic_regression", format(Sys.time(), "%y%m%d_%H%M%S"))
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
plp_data_path <- "output/logistic_regression/plp_data"
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

measurement_concept_ids <- c(
    4177340, # height
    4099154 # weight
)
measurement_covariateSettings <- createCovariateSettings(useMeasurementValueLongTerm = TRUE,
                                                         includedCovariateConceptIds = measurement_concept_ids)

drug_covariateSettings <- createCovariateSettings(useDistinctIngredientCountLongTerm = TRUE)


# Extract covariate data ---------------------------------------------------
for (i in c("condition", "score", "gender", "measurement", "drug")) {
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
        savePlpData(get(paste0(i, "_data")), file.path(plp_data_path, paste0(i, "_data")))
    } else {
        assign(paste0(i, "_data"), loadPlpData(file.path(plp_data_path, paste0(i, "_data"))))
    }
}


# Preprocess cohorts ------------------------------------------------------
cohorts <- condition_data$cohorts %>% 
    select(rowId, subjectId, cohortStartDate) %>% 
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
            mutate(SUBJECT_ID = as.character(SUBJECT_ID),
                   COHORT_START_DATE = as.Date(COHORT_START_DATE, "%Y/%m/%d")) %>% 
            filter(DRUG_EXPOSURE_COUNT >= 7) %>% 
            select(SUBJECT_ID, COHORT_START_DATE) %>% 
            mutate(drug_value = TRUE),
        by = c("subjectId" = "SUBJECT_ID", "cohortStartDate" = "COHORT_START_DATE")
    ) %>% 
    left_join(
        condition_data$covariates %>% 
            as.data.frame() %>% 
            mutate(rowId = as.character(rowId)) %>% 
            group_by(rowId) %>% 
            summarise(condition_count = sum(covariateValue)) %>% 
            filter(condition_count >= 2) %>% 
            select(rowId) %>% 
            mutate(condition_value = TRUE),
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
visit_covariates <- cohorts %>% 
    left_join(fread("data/visit_period.csv") %>% 
                  mutate(SUBJECT_ID = as.character(SUBJECT_ID),
                         COHORT_START_DATE = as.Date(COHORT_START_DATE, "%Y/%m/%d"),
                         VISIT_CONCEPT_ID = ifelse(VISIT_CONCEPT_ID == 9201, "inpatient",
                                                   ifelse(VISIT_CONCEPT_ID == 9202, "outpatient",
                                                          ifelse(VISIT_CONCEPT_ID == 9203, "emergency", "laboratory")))) %>% 
                  spread(key = VISIT_CONCEPT_ID, value = VISIT_PERIOD) %>% 
                  select(-COHORT_END_DATE),
              by = c("subjectId" = "SUBJECT_ID", "cohortStartDate" = "COHORT_START_DATE")) %>% 
    select(rowId, emergency, inpatient, laboratory, outpatient)
              
              
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

continuous_indep <- colnames(p_datum)[!colnames(p_datum) %in%
                                          c("rowId", "subjectId", "cohortStartDate",
                                             "male", "depressive_disorder", "outcome_value")]


for (i in continuous_indep) {
    for (j in c("r", "p")) {
        plot_name <- paste0(i, "_histogram.png")
        if (j == "r") {
            plot_data <- datum
        } else {
            plot_data <- p_datum
            plot_name <- paste0("[preprocced]_", plot_name)
        }
        plot_data <- plot_data %>% 
            drop_na(!!i)
        histogram <- ggplot(aes_string(x = i), data = plot_data) + 
            geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
            theme_classic()
        if (max(plot_data[[i]]) - min(plot_data[[i]]) < 30) {
            histogram <- histogram +
                scale_x_continuous(breaks = seq(min(plot_data[[i]]), max(plot_data[[i]]), 1))
        }
        ggsave(file.path(density_plot_path, plot_name))
    }
}


# Multivariate linear regression ----------------------------------------------------------------
split_index <- sample(1:nrow(datum), size = round(nrow(datum) * 0.25))
train <- datum[split_index, ]
test <- datum[-split_index, ]

indep1 <- c("male", "depressive_disorder", "chads2", "chads2vasc", "charlson_index",
           "dcsi", "inpatient", "outpatient", "emergency", "laboratory", "height",
           "weight", "ingredient_count")
# indep2 <- indep1[!indep1 %in% c("dcsi", "height")]
indep_vec <- c("indep1"
               # , "indep2"
               )

dep <- "outcome_value"

for (i in indep_vec) {
    fit <- glm(as.formula(paste(dep, paste(get(i), collapse = "+"), sep = "~")),
               family = "binomial", data = train)
    sink(file.path(output_path, "multiple_logistic_regression_result.html"))
    fit %>% xtable() %>% print.xtable(type = "html")
    sink()
    
    p <- predict(fit, newdata = test, type = "response")
    pr <- prediction(p, test$outcome_value)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    auc <- performance(pr, measure = "auc")
    
    png(file.path(output_path, paste0("roc_plot_", i,".png")))
    plot(prf)
    lines(x = c(0, 1), y = c(0, 1))
    text(0.9, 0.1, labels = paste("AUC =", round(auc@y.values[[1]], 4)))
    dev.off()
}


# Univariate linear regression -------------------------------------------
incidence_plot_path <- file.path(output_path, "incidence_plot")
path_assistant(incidence_plot_path)

break_list <- list(inpatient = list(breaks = c(-Inf, 6, 9, 12, 15 ,18, Inf),
                                    labels = c("> 6", "6-9", "9-12", "12-15", "15-18", "< 18")),
                   outpatient = list(breaks = c(-Inf, 2, 4, 6, 8, 10, Inf),
                                     labels = c("> 2", "2-4", "4-6", "6-8", "8-10", "< 10")),
                   emergency = list(breaks = c(-Inf, 1, 2, 3, Inf),
                                    labels = c("> 1", "1-2", "2-3", "< 3")),
                   laboratory = list(breaks = c(-Inf, 2, 4, 6, 8, 10, Inf),
                                     labels = c("> 2", "2-4", "4-6", "6-8", "8-10", "< 10")),
                   height = list(breaks = c(-Inf, 150, 155, 160, 165, Inf),
                                 labels = c("< 150", "150-155", "155-160", "160-165", "> 165")),
                   weight = list(breaks = c(-Inf, 50, 55, 60, 65, 70, Inf),
                                 labels = c("< 50", "50-55", "55-60", "60-65", "65-70","> 70")),
                   ingredient_count = list(breaks = c(-Inf, 3, 6, 9, 12, 15, 18, 21, Inf),
                                           labels = c("> 3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21","< 21")))

plot_data <- p_datum
sink(file.path(output_path, "univariate_linear_regression_result.html"))
for (indep_var in indep1[3:length(indep1)]) {
    cat(indep_var, "univariate linear regression")
    uni_lr_result <- glm(as.formula(paste(dep, indep_var, sep = "~")),
                         family = "binomial", data = train)
    uni_lr_result %>% xtable() %>% print.xtable(type = "html")
    cat(rep("<br>", 3))
    
    if (!indep_var %in% c("chads2", "chads2vasc", "charlson_index", "dcsi")) {
        plot_data <- plot_data %>%
            drop_na(!!indep_var)
        plot_data <- plot_data %>% 
            mutate(!!indep_var := cut(plot_data[[indep_var]],
                                breaks = break_list[[indep_var]]$breaks,
                                labels = break_list[[indep_var]]$labels))
    }
    plot_data %>% 
        group_by_at(indep_var) %>% 
        summarise(outcome = sum(outcome_value) / n(),
                  non_outcome = 1 - outcome) %>% 
        gather(key = "group", value = "incidence", "outcome", "non_outcome") %>% 
        ggplot(aes_string(x = indep_var, y = "incidence", fill = "group")) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(incidence, 2))) +
        theme(legend.position = "bottom", legend.title = element_blank())
    ggsave(file.path(incidence_plot_path, paste0(indep_var, "_incidence_plot.png")))
}
sink()
