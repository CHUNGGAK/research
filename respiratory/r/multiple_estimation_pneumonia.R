setwd("/home/lee/workspace/respiratory")

library(CohortMethod)
library(forestplot)

source("/media/sf_shared_folder/workspace/research/ltool/ltool.R")
source("/media/sf_shared_folder/workspace/research/ltool/ltool_cm.R")
source("/media/sf_shared_folder/workspace/connect_information.R")

outputFolder <- file.path(getwd(), "output", paste0("pneumonia_", format(Sys.time(), "%Y-%m-%d_%H:%M:%S")))
path_assistant(outputFolder)

excludedCovariateConceptIds <- c(948078,
                                 # drug_era group during day -365 through 0 days relative to index: pantoprazole
                                 21600081,
                                 # drug_era group during day -365 through 0 days relative to index: H2-receptor antagonists,
                                 21600095,
                                 # drug_era group during day -365 through 0 days relative to index: Proton pump inhibitors,
                                 953076,
                                 # drug_era during day -365 through 0 days relative to index: Famotidine,
                                 948078)
# drug_era during day -365 through 0 days relative to index: pantoprazol

# Include precedure and measurement
covSettings1 <- createCovariateSettings(useDemographicsGender = TRUE,
                                        useDemographicsAge = TRUE,
                                        useConditionGroupEraLongTerm = TRUE,
                                        useDrugEraLongTerm = TRUE,
                                        useProcedureOccurrenceLongTerm = TRUE,
                                        useMeasurementLongTerm = TRUE,
                                        useObservationLongTerm = TRUE,
                                        useCharlsonIndex = TRUE,
                                        useChads2Vasc = TRUE,
                                        excludedCovariateConceptIds = excludedCovariateConceptIds)

# Enclude precedure and measurement
covSettings2 <- createCovariateSettings(useDemographicsGender = TRUE,
                                        useDemographicsAge = TRUE,
                                        useConditionGroupEraLongTerm = TRUE,
                                        useDrugEraLongTerm = TRUE,
                                        useObservationLongTerm = TRUE,
                                        useCharlsonIndex = TRUE,
                                        useChads2Vasc = TRUE,
                                        excludedCovariateConceptIds = excludedCovariateConceptIds)

getDbCmDataArgs1 <- createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                    restrictToCommonPeriod = FALSE,
                                                    firstExposureOnly = FALSE,
                                                    removeDuplicateSubjects = "remove all",
                                                    studyStartDate = "",
                                                    studyEndDate = "",
                                                    excludeDrugsFromCovariates = TRUE,
                                                    covariateSettings = covSettings1)

getDbCmDataArgs2 <- createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                    restrictToCommonPeriod = FALSE,
                                                    firstExposureOnly = FALSE,
                                                    removeDuplicateSubjects = "remove all",
                                                    studyStartDate = "",
                                                    studyEndDate = "",
                                                    excludeDrugsFromCovariates = TRUE,
                                                    covariateSettings = covSettings2)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                      minDaysAtRisk = 0,
                                                      riskWindowStart = 0,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 30,
                                                      endAnchor = "cohort start")

createPsArgs <- createCreatePsArgs(control = createControl(maxIterations = 3000))
matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 1)

# 1:1 matching
fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

# No matching
fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = FALSE)

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "1:1 matching; Include procedure, measurement covariates",
                                getDbCohortMethodDataArgs = getDbCmDataArgs1,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "1:1 matching; Exclude procedure, measurement covariates",
                                getDbCohortMethodDataArgs = getDbCmDataArgs2,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "Not matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs2,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)



cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3)

saveCmAnalysisList(cmAnalysisList, file.path(outputFolder, "cmAnalysisList.json"))

tcos <- createTargetComparatorOutcomes(targetId = 734, # Pneumonia subgroup
                                       comparatorId = 735,
                                       outcomeIds = 1241) # Pneumonia 

targetComparatorOutcomesList <- list(tcos)

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
        plotTimeToEvent_riskWindowStart = 0,
        plotTimeToEvent_startAnchor = "cohort start",
        plotTimeToEvent_riskWindowEnd = 30,
        plotTimeToEvent_endAnchor = "cohort end")
