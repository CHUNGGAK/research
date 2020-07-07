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

# Condition
covariate_concept_id <- fread(file.path("data", "covariate_concept_id", "covariate_concept_id.csv"))

included_cov_concept_ids_condition <-  c(covariate_concept_id$TARGET_CONCEPT_ID)

cov_set_condition1 <- createCovariateSettings(useConditionOccurrenceLongTerm = TRUE,
                                              includedCovariateConceptIds = included_cov_concept_ids_condition,
                                              addDescendantsToInclude = TRUE)

cov_set_condition2 <- createCovariateSettings(useConditionGroupEraLongTerm = TRUE,
                                              includedCovariateConceptIds = included_cov_concept_ids_condition,
                                              addDescendantsToInclude = TRUE)

statin_concept_ids <- c(1332497, 1332499, 19122209, 19122208, 19123592, 1545994,
                        1545996, 1545959, 19122208, 1545992, 43527029, 43527032,
                        43527035, 42972637, 42972640, 42972631, 42972634, 19075051,
                        1586255, 19077498, 41085862, 1539469, 1526479, 19112569,
                        44506638, 1539465, 1539407, 1539411, 42969162, 42969112,
                        42968999, 42968981, 42969173, 42969232, 42969291, 42969149,
                        42969154, 42969168, 42969132, 42969135, 40165245, 40165245,
                        40165245, 40165253, 40165253, 40165261, 40165261, 42969040,
                        42969049, 42969022, 42932550, 42969082, 1592113, 40165649,
                        40165649, 40165649, 40165649, 42932547, 42932544, 42932541,
                        42932538)

ras_concept_ids <- c(42961746, 42961742, 19081025, 1340159, 19074673, 19022242,
                     43784844, 40165755, 1341998, 40163749, 40163760, 19102053,
                     19080128, 42801010, 42801015, 40163270, 40163275, 21070867,
                     19101748, 1353820, 19102170, 1334461, 19107180, 1334492,
                     974447, 36883910, 19106542, 1332525, 1332527, 21141332,
                     42948651, 42948648, 42948654, 46275724, 46275724, 46275724,
                     19096678, 19023454, 1308874, 1308851, 19023453, 42932547,
                     42932544, 42932541, 42932538, 42932550, 42969082, 1351583,
                     1351583, 974702, 1351559, 1351559, 42929951, 42929931,
                     42968999, 42968981, 40167849, 40167852, 42938510, 42938513,
                     42930395, 42930392, 40167849, 19028935, 974473, 19028936,
                     974474, 42969040, 42969049, 42969022, 42960027, 42960031,
                     42960037, 42960002, 42960008, 42960018, 42969149, 42969154,
                     42950393, 42950354, 42950421, 42950866, 42950869, 42969112,
                     40185276, 40185276, 40184184, 40184217, 40185304, 40184187,
                     40185304, 19127433, 19127434, 40224172, 40224175, 19102171,
                     19102491, 19096740, 19096752, 42969132, 42969135, 42955514,
                     42955431, 19084947, 974642, 19081558, 19022948, 19078080,
                     19022949, 19022949, 19078101, 42972637, 42972640, 42972631,
                     42972634, 40235487, 40235491, 19022948)

# Measurement
included_cov_concept_ids_measurement <- c(3038553, # BMI 4245997에서 업데이트됨
                                          3005424, # BSA
                                          2000000118, #eGFR
                                          statin_concept_ids,
                                          ras_concept_ids)

cov_set_measurement <- createCovariateSettings(
    useMeasurementLongTerm = TRUE,
    useMeasurementValueLongTerm = TRUE,
    includedCovariateConceptIds = included_cov_concept_ids_measurement,
    addDescendantsToInclude = TRUE)

covariateSettings1 <- list(cov_set_demographics, cov_set_condition1, cov_set_measurement)
covariateSettings2 <- list(cov_set_demographics, cov_set_condition2, cov_set_measurement)

dCohort <- data.frame(cohort_id = c(24470001, 24470003),
                      cohort_name = c("statin", "ras"))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

exposureTable <- "lcg_cohort"
outcomeTable <- "cohort"
outcomeIds <- 122 # death

output_path <- file.path("output", "propensity_score_matching_0610")

dir.create(output_path)

for (cohort_var in dCohort$cohort_id) {
    for (var_cov in 1:2) {
        var_name <- dCohort$cohort_name[match(cohort_var, dCohort$cohort_id)]
        file_path <- file.path(output_path, paste0(var_name, "_cov", var_cov))
        cm_data_path <- file.path(file_path, "cm_data")
        
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
                                                      covariateSettings = get(paste0("covariateSettings", var_cov)))
            
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
        
        matchedPop <- matchOnPs(ps, caliper = 0.2, caliperScale = "standardized logit",
                                maxRatio = 1)
        
        write_csv(matchedPop %>% 
                      select(rowId, subjectId, treatment, cohortStartDate, propensityScore,
                             stratumId),
                  file.path(file_path, paste0(var_name, "_Matching_data.csv")))
        
        
        # Create propensity model table -------------------------------------------
        
        psModel <- getPsModel(ps, cohortMethodData) %>%
            arrange(desc(coefficient))
        
        write_csv(psModel, file.path(file_path, "psModel.csv"))
        
        # Craete propensity score plot --------------------------------------------
        
        p1 <- plotPs(ps, scale = "preference", showCountsLabel = TRUE,
                     showAucLabel = TRUE, showEquiposeLabel = TRUE)
        p2 <- plotPs(matchedPop, ps,
                     scale = "preference", showCountsLabel = TRUE,
                     showAucLabel = TRUE, showEquiposeLabel = TRUE)
        g12 <- arrangeGrob(p1, p2, ncol = 2)
        ggsave(filename = file.path(file_path, "psPlot.png"), plot = g12,
               width = 25, units = "cm")
        
        balance <- computeCovariateBalance(matchedPop, cohortMethodData)
        
        plotCovariateBalanceScatterPlot(balance,
                                        showCovariateCountLabel = TRUE,
                                        showMaxLabel = TRUE,
                                        fileName = file.path(file_path, "balance_scatter_plot.png"))
        
        plotCovariateBalanceOfTopVariables(balance,
                                           fileName = file.path(file_path, "balance_of_top_variables_plot.png"))
        
        drawAttritionDiagram(matchedPop, n = "exposure",
                             fileName = file.path(file_path, "attrition_diagram_exposure.png"))
        drawAttritionDiagram(matchedPop, n = "person",
                             fileName = file.path(file_path, "attrition_diagram_person.png"))
        
        table1 <- createCmTable1(balance)
        write_csv(table1, file.path(file_path, "table1.csv"))
    }
}
