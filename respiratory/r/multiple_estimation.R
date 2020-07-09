setwd("/home/lee/workspace/respiratory")

library(CohortMethod)
library(forestplot)
library(data.table)
library(tidyverse)

source("/media/sf_sf/workspace/connection_details.R")
source("/media/sf_sf/workspace/ltool/ltool_cm.R")

outputFolder <- file.path(getwd(), "output", paste0(format(Sys.time(), "%Y-%m-%d_%H:%M:%S")))
dir.create(outputFolder)

excluded_covariates_1 <- read_csv("/media/sf_sf/workspace/respiratory/data/excluded_covariates/excluded_covariates.csv")
excluded_covariates_2 <- read_csv("/media/sf_sf/workspace/respiratory/data/excluded_covariates/excluded_covariates_2.csv")
excluded_covariates_3 <- read_csv("/media/sf_sf/workspace/respiratory/data/excluded_covariates/excluded_covariates_3.csv")

excludedCovariateConceptIds <- c(941047, 950696, 4009003, 953076, 997276, # H2 receptor
                                 21600095, # PPI
                                 948078, # pantoprazole
                                 948080, # pantoprazole 40 MG Injection,
                                 21600046, # DRUGS FOR ACID RELATED DISORDERS,
                                 21600080, # DRUGS FOR PEPTIC ULCER AND GASTRO-OESOPHAGEAL REFLUX DISEASE (GORD)
                                 19077241, # Famotidine 20 MG Oral Tablet
                                 excluded_covariates_1$conceptId, # Balance value is above 0.1
                                 excluded_covariates_2$conceptId,
                                 excluded_covariates_3$conceptId)

# Include precedure and measurement
covSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                       useDemographicsAge = TRUE,
                                       useDemographicsIndexYear = TRUE,
                                       useConditionGroupEraLongTerm = TRUE,
                                       useDrugEraLongTerm = TRUE,
                                       useDrugGroupEraLongTerm = TRUE,
                                       useConditionOccurrenceLongTerm = TRUE,
                                       useDrugExposureLongTerm = TRUE,
                                       useProcedureOccurrenceLongTerm = TRUE,
                                       useMeasurementLongTerm = TRUE,
                                       useObservationLongTerm = TRUE,
                                       useCharlsonIndex = TRUE,
                                       useChads2Vasc = TRUE,
                                       excludedCovariateConceptIds = excludedCovariateConceptIds)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                    restrictToCommonPeriod = FALSE,
                                                    firstExposureOnly = FALSE,
                                                    removeDuplicateSubjects = "remove all",
                                                    studyStartDate = "",
                                                    studyEndDate = "",
                                                    excludeDrugsFromCovariates = TRUE,
                                                    covariateSettings = covSettings)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                      minDaysAtRisk = 0,
                                                      riskWindowStart = 3,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 33,
                                                      endAnchor = "cohort start")

# createPsArgs <- createCreatePsArgs(control = createControl(maxIterations = 3000))
createPsArgs <- createCreatePsArgs()
matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 1)
trimByPsArgs <- createTrimByPsArgs()
trimByPsToEquipoiseArgs <- createTrimByPsToEquipoiseArgs()

# Cox
fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

# Logistic
fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "logistic",
                                                  stratified = TRUE)

# Cox
fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = FALSE)

# Logistic
fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(modelType = "logistic",
                                                  stratified = FALSE)

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "Cox; Trim to equipoise + 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPsToEquipoise = TRUE,
                                trimByPsToEquipoiseArgs = trimByPsToEquipoiseArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "Logistic; Trim to equipoise + 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPsToEquipoise = TRUE,
                                trimByPsToEquipoiseArgs = trimByPsToEquipoiseArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "Cox; Trim + 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPs = TRUE,
                                trimByPsArgs = trimByPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "Logistic; Trim + 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPs = TRUE,
                                trimByPsArgs = trimByPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                description = "Cox; 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis6 <- createCmAnalysis(analysisId = 6,
                                description = "Logistic; 1:1 Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis7 <- createCmAnalysis(analysisId = 7,
                                description = "No matching; Cox",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

cmAnalysis8 <- createCmAnalysis(analysisId = 8,
                                description = "No matching; Logistic",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs4)



cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4,
                       cmAnalysis5, cmAnalysis6, cmAnalysis7, cmAnalysis8)

saveCmAnalysisList(cmAnalysisList, file.path(outputFolder, "cmAnalysisList.json"))

tcos1 <- createTargetComparatorOutcomes(targetId = c(751, # Original
                                                     756), # >= APACHE score 25
                                        comparatorId = c(752, # Original
                                                         757), # >= APACHE score 25
                                        outcomeIds = 833)

tcos2 <- createTargetComparatorOutcomes(targetId = 758, # C. difficile subgroup
                                        comparatorId = 759,
                                        outcomeIds = 902) # C. difficile

tcos3 <- createTargetComparatorOutcomes(targetId = 754, # Pneumonia subgroup
                                       comparatorId = 755,
                                       outcomeIds = 1241) # Pneumonia 

targetComparatorOutcomesList <- list(tcos1, tcos2, tcos3)

saveTargetComparatorOutcomesList(targetComparatorOutcomesList, file.path(outputFolder, "targetComparatorOutcomesList.json"))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)

result <- runCmAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema,
                        exposureDatabaseSchema = exposureDatabaseSchema,
                        exposureTable = exposureTable,
                        outcomeDatabaseSchema = outcomeDatabaseSchema,
                        outcomeTable = outcomeTable,
                        outputFolder = outputFolder,
                        cmAnalysisList = cmAnalysisList,
                        targetComparatorOutcomesList = targetComparatorOutcomesList)

tidy_cm(result, outputFolder,
        plotTimeToEvent_riskWindowStart = 3,
        plotTimeToEvent_startAnchor = "cohort start",
        plotTimeToEvent_riskWindowEnd = 33,
        plotTimeToEvent_endAnchor = "cohort start",
        plotTimeToEvent_showFittedLines = FALSE,
        plotKaplanMeier = FALSE)
