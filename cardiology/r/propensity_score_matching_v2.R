setwd('E:/Users/DLCG001/workspace/cardiology')
Sys.setlocale(category = "LC_ALL", locale = "English")

library(data.table)
library(tidyverse)
library(CohortMethod)
library(xtable)
library(gridExtra)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")
source("E:/Users/DLCG001/workspace/connect_information.R")

# Merge condition occurrence concept ids ----------------------------------

# for (i in list.files(file.path("data", "covariate_concept_id"))) {
#     temp_df <- fread(file.path("data", "covariate_concept_id", i)) %>% 
#         select(SOURCE_CODE_DESCRIPTION, TARGET_CONCEPT_ID) %>% 
#         mutate(TARGET_CONCEPT_ID = as.factor(TARGET_CONCEPT_ID))
#     
#     if (!exists("covariate_concept_id")) {
#         covariate_concept_id <- temp_df
#     } else {
#         covariate_concept_id <- covariate_concept_id %>% 
#             rbind(temp_df)
#     }
# }
# 
# write_csv(covariate_concept_id, file.path("data", "covariate_concept_id", "covariate_concept_id.csv"))


# Demographics
cov_set_demographics <- createCovariateSettings(useDemographicsGender = TRUE,
                                                useDemographicsAge = TRUE)

cov_set_demographics_age_group <- createCovariateSettings(useDemographicsGender = TRUE,
                                                          useDemographicsAgeGroup = TRUE)

cov_set_demographics_ex_age <- createCovariateSettings(useDemographicsGender = TRUE)

# Condition
covariate_concept_id <- fread(file.path("data", "covariate_concept_id", "covariate_concept_id.csv"))

included_cov_concept_ids_condition <-  c(covariate_concept_id$TARGET_CONCEPT_ID)

cov_set_condition <- createCovariateSettings(useConditionOccurrenceLongTerm = TRUE,
                                             includedCovariateConceptIds = included_cov_concept_ids_condition,
                                             addDescendantsToInclude = TRUE)

# Measurement
included_cov_concept_ids_measurement <- c(3038553, # BMI 4245997에서 업데이트됨
                                          3005424, # BSA
                                          2000000118) #eGFR

cov_set_measurement <- createCovariateSettings(
    useMeasurementLongTerm = TRUE,
    useMeasurementValueLongTerm = TRUE,
    includedCovariateConceptIds = included_cov_concept_ids_measurement,
    addDescendantsToInclude = TRUE)

covariateSettings <- list(cov_set_demographics, cov_set_condition, cov_set_measurement)

cohort_df <- data.frame(cohort_id = c(24470001, 24470003),
                        cohort_name = c("statin", "ras"))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

exposureTable <- "lcg_cohort"
outcomeTable <- "cohort"
outcomeIds <- 122 # death

output_path <- file.path("output", "propensity_score_matching_v3")

dir.create(output_path)

dCaliper <- data.frame(value = c(0.2, 0.05, 0.0125),
                       name = c("2000", "0500", "0125"))

for (cohort_var in cohort_df$cohort_id) {
    var_name <- cohort_df$cohort_name[match(cohort_var, cohort_df$cohort_id)]
    var_path <- file.path(output_path, var_name)
    cm_data_path <- file.path(var_path, "cm_data")

    path_assistant(var_path)
    path_assistant(cm_data_path)
    
    if (!file.exists(file.path(cm_data_path, "cohorts.rds"))) {
        cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  oracleTempSchema = oracleTempSchema,
                                                  targetId = cohort_var - 1, # treatment = 0
                                                  comparatorId = cohort_var, # treatment = 1
                                                  outcomeIds = outcomeIds,
                                                  studyStartDate = "",
                                                  studyEndDate = "",
                                                  exposureDatabaseSchema = exposureDatabaseSchema,
                                                  exposureTable = exposureTable,
                                                  outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                  outcomeTable = outcomeTable,
                                                  excludeDrugsFromCovariates = TRUE,
                                                  maxCohortSize = 0,
                                                  covariateSettings = covariateSettings)
        
        saveCohortMethodData(cohortMethodData, cm_data_path)
    } else {
        cohortMethodData <- loadCohortMethodData(cm_data_path)
    }
    
    studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                                    outcomeId = outcomeIds,
                                                    firstExposureOnly = FALSE,
                                                    restrictToCommonPeriod = FALSE,
                                                    washoutPeriod = 0,
                                                    removeDuplicateSubjects = TRUE,
                                                    removeSubjectsWithPriorOutcome = FALSE,
                                                    priorOutcomeLookback = 99999,
                                                    minDaysAtRisk = 0,
                                                    riskWindowStart = 0,
                                                    addExposureDaysToStart = FALSE,
                                                    riskWindowEnd = 99999,
                                                    addExposureDaysToEnd = FALSE,
                                                    censorAtNewRiskWindow = FALSE)
    
    ps <- createPs(cohortMethodData = cohortMethodData, population = studyPop,
                   control = createControl(maxIterations = 3000))
    
    for (var_caliper in dCaliper$value) {
        strata_name <- paste(var_name, dCaliper$name[match(var_caliper, dCaliper$value)], sep = "_")
        strata_path <- file.path(var_path, strata_name)
        dir.create(strata_path)
        
        matchedPop <- matchOnPs(ps, caliper = var_caliper, caliperScale = "standardized logit",
                                 maxRatio = 1)
        
        write_csv(matchedPop %>% 
                      select(rowId, subjectId, treatment, cohortStartDate, propensityScore,
                             stratumId),
                 file.path(strata_path, paste0(strata_name, "_Matching_data.csv")))
        
        
        # Create propensity model table -------------------------------------------
        
        psModel <- getPsModel(ps, cohortMethodData) %>%
            arrange(desc(coefficient))
        
        write_csv(psModel, file.path(strata_path, "psModel.csv"))
        
        # Craete propensity score plot --------------------------------------------
        
        p1 <- plotPs(ps, scale = "preference", showCountsLabel = TRUE,
                     showAucLabel = TRUE, showEquiposeLabel = TRUE)
        p2 <- plotPs(matchedPop, ps,
                     scale = "preference", showCountsLabel = TRUE,
                     showAucLabel = TRUE, showEquiposeLabel = TRUE)
        g12 <- arrangeGrob(p1, p2, ncol = 2)
        ggsave(filename = file.path(strata_path, "psPlot.png"), plot = g12,
               width = 25, units = "cm")
        
        balance <- computeCovariateBalance(matchedPop, cohortMethodData)
        
        plotCovariateBalanceScatterPlot(balance,
                                        showCovariateCountLabel = TRUE,
                                        showMaxLabel = TRUE,
                                        fileName = file.path(strata_path, "balance_scatter_plot.png"))
        
        plotCovariateBalanceOfTopVariables(balance,
                                           fileName = file.path(strata_path, "balance_of_top_variables_plot.png"))
        
        drawAttritionDiagram(matchedPop, n = "exposure",
                             fileName = file.path(strata_path, "attrition_diagram_exposure.png"))
        drawAttritionDiagram(matchedPop, n = "person",
                             fileName = file.path(strata_path, "attrition_diagram_person.png"))
        
        table1 <- createCmTable1(balance)
        write_csv(table1, file.path(strata_path, "table1.csv"))
    }
}
