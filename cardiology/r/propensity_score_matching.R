setwd('E:/Users/DLCG001/workspace/cardiology')
Sys.setlocale(category = "LC_ALL", locale = "English")

library(data.table)
library(tidyverse)
library(CohortMethod)
library(xtable)
library(gridExtra)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")


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

covariate_concept_id <- fread(file.path("data", "covariate_concept_id", "covariate_concept_id.csv"))

includedCovariateConceptIds = c(8532, 8507, # 성별
                                4245997, # BMI
                                3005424, # BSA
                                2000000118, #eGFR
                                covariate_concept_id$TARGET_CONCEPT_ID)

covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                             useDemographicsAge = TRUE,
                                             useConditionOccurrenceLongTerm = TRUE,
                                             useMeasurementLongTerm = TRUE,
                                             useMeasurementValueLongTerm = TRUE,
                                             includedCovariateConceptIds = includedCovariateConceptIds,
                                             addDescendantsToInclude = TRUE)

cohort_df <- data.frame(cohort_id = c(20011700, 20011702, 20011704),
                        cohort_name = c("statin", "ras", "bis"))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

exposureTable <- "lcg_cohort"
outcomeTable <- "lcg_cohort"
outcomeIds <- 122 # death

output_path <- file.path("output", "propensity_score_matching")

path.assistant(output_path)

for (cohort_var in cohort_df$cohort_id) {
    var_name <- cohort_df$cohort_name[match(cohort_var, cohort_df$cohort_id)]
    var_path <- file.path(output_path, var_name)
    estimation_data_path <- file.path(var_path, "estimation_data")
    plot_path <- file.path(var_path, "plot")
    matching_data_path <- file.path(var_path, "matching_data")
    
    path.assistant(var_path)
    path.assistant(estimation_data_path)
    path.assistant(plot_path)
    path.assistant(matching_data_path)
    
    estimation_data_name <- file.path(estimation_data_path,
                           paste0(var_name, "_estimation_data"))
    matching_data_name <- file.path(matching_data_path,
                               paste0(var_name, "_matching_group.csv"))
    
    if (!file.exists(estimation_data_name)) {
        cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  oracleTempSchema = oracleTempSchema,
                                                  targetId = cohort_var + 1,
                                                  comparatorId = cohort_var,
                                                  outcomeIds = outcomeIds,
                                                  studyStartDate = "",
                                                  studyEndDate = "",
                                                  exposureDatabaseSchema = exposureDatabaseSchema,
                                                  exposureTable = exposureTable,
                                                  outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                  outcomeTable = outcomeTable,
                                                  excludeDrugsFromCovariates = TRUE,
                                                  firstExposureOnly = FALSE,
                                                  removeDuplicateSubjects = FALSE,
                                                  restrictToCommonPeriod = FALSE,
                                                  washoutPeriod = 0,
                                                  maxCohortSize = 0,
                                                  covariateSettings = covariateSettings)
        
        saveCohortMethodData(cohortMethodData, estimation_data_path)
    } else {
        cohortMEthodData <- loadCohortMethodData(estimation_data_path)
    }
    
    studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                                    outcomeId = outcomeIds,
                                                    firstExposureOnly = FALSE,
                                                    restrictToCommonPeriod = FALSE,
                                                    washoutPeriod = 0,
                                                    removeDuplicateSubjects = 'keep all',
                                                    removeSubjectsWithPriorOutcome = FALSE,
                                                    priorOutcomeLookback = 99999,
                                                    minDaysAtRisk = 0,
                                                    riskWindowStart = 0,
                                                    addExposureDaysToStart = FALSE,
                                                    riskWindowEnd = 99999,
                                                    addExposureDaysToEnd = FALSE,
                                                    censorAtNewRiskWindow = FALSE)
    
    ps <- createPs(cohortMethodData = cohortMethodData, population = studyPop)
    
    matchedPop <- matchOnPs(ps, caliper = 0.2, caliperScale = 'standardized logit',
                            maxRatio = 1)
    
    write_csv(matchedPop %>% 
                  select(rowId, subjectId, treatment, cohortStartDate, propensityScore,
                         stratumId),
              matching_data_name)
    
    
    # Create propensity model table -------------------------------------------
    
    sink(file.path(plot_path, paste0(var_name, "_propensity_model.html")))
    getPsModel(ps, cohortMethodData) %>%
        xtable() %>%
        print.xtable(type = "html", include.rownames = FALSE)
    sink()
    
    
    # Craete propensity score plot --------------------------------------------
    
    p1 <- plotPs(ps, scale = "preference", showCountsLabel = TRUE,
                 showAucLabel = TRUE, showEquiposeLabel = TRUE)
    p2 <- plotPs(matchedPop, ps,
                 scale = "preference", showCountsLabel = TRUE,
                 showAucLabel = TRUE, showEquiposeLabel = TRUE)
    g12 <- arrangeGrob(p1, p2, ncol = 2)
    ggsave(file.path(plot_path, paste0(var_name, "_ps_plot.png")), g12,
           width = 25, units = "cm")
    
    balance <- computeCovariateBalance(matchedPop, cohortMethodData)
    
    plotCovariateBalanceScatterPlot(balance,
                                    showCovariateCountLabel = TRUE,
                                    showMaxLabel = TRUE,
                                    fileName = file.path(plot_path, paste0(var_name, "_balance_scatter_plot.png")))
    
    plotCovariateBalanceOfTopVariables(balance,
                                       fileName = file.path(plot_path, paste0(var_name, "_balance_scatter_plot.png")))
    
    drawAttritionDiagram(matchedPop,
                         fileName = file.path(plot_path, paste0(var_name, "_attrition_diagram.png")))
    
    
    # Merge plots -------------------------------------------------------------
    
    # g <- arrangeGrob(p1, p2, p3, p4, p5,
    #                  layout_matrix = rbind(c(1, 2),
    #                                        c(3, 4),
    #                                        c(5, 5)),
    #                  top = as.character(var_name))
    # 
    # ggsave(file.path(output_path, paste0(var_name, "_matching_information.png")), g,
    #        width = 30, height = 56, units = "cm")
    # width 단위 10, height 단위 14
}
