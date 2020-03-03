setwd("/home/lee/workspace/respiratory")

source("/home/lee/workspace/research/ltool/ltool.R")
source("/home/lee/workspace/connect_information.R")

outputFolder <- file.path("output", format(Sys.time(), "%Y%m%d_%H%M%S"))
path.assistant(outputFolder)

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

covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                             useDemographicsAge = TRUE,
                                             useConditionGroupEraLongTerm = TRUE,
                                             useDrugEraLongTerm = TRUE,
                                             useDrugGroupEraLongTerm = TRUE,
                                             useConditionOccurrenceLongTerm = TRUE,
                                             useDistinctConditionCountLongTerm = TRUE,
                                             useProcedureOccurrenceLongTerm = TRUE,
                                             useMeasurementLongTerm = TRUE,
                                             useObservationLongTerm = TRUE,
                                             useCharlsonIndex = TRUE,
                                             useDcsi = TRUE,
                                             useChads2Vasc = TRUE,
                                             useVisitConceptCountLongTerm = TRUE,
                                             excludedCovariateConceptIds = excludedCovariateConceptIds)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                   restrictToCommonPeriod = FALSE,
                                                   firstExposureOnly = FALSE,
                                                   removeDuplicateSubjects = "remove all",
                                                   studyStartDate = "",
                                                   studyEndDate = "",
                                                   excludeDrugsFromCovariates = TRUE,
                                                   covariateSettings = covariateSettings)

createStudyPopArgs1 <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                      minDaysAtRisk = 0,
                                                      riskWindowStart = 2,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 17,
                                                      endAnchor = "cohort start")

createStudyPopArgs2 <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                      minDaysAtRisk = 0,
                                                      riskWindowStart = 2,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 32,
                                                      endAnchor = "cohort start")

createPsArgs <- createCreatePsArgs(control = createControl(maxIterations = 2000))
matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 1)

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE,
                                                  prior = createPrior("laplace", useCrossValidation = FALSE))

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = FALSE)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = FALSE,
                                                  prior = createPrior("laplace", useCrossValidation = FALSE))

fitOutcomeModelArgs5 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE,
                                                  useCovariates = TRUE)

fitOutcomeModelArgs6 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE,
                                                  prior = createPrior("laplace", useCrossValidation = FALSE),
                                                  useCovariates = TRUE)

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "1:1 matching; Use cross validation; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "1:1 matching; Use cross validation; Risk window end 4 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "1:1 matching; Do not use cross validation; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "1:1 matching; Do not use cross validation; Risk window end 4 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                description = "Not matching; Use cross validation; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

cmAnalysis6 <- createCmAnalysis(analysisId = 6,
                                description = "Not matching; Use cross validation; Risk window end 4 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

cmAnalysis7 <- createCmAnalysis(analysisId = 7,
                                description = "Not matching; Do not use cross validation; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs4)

cmAnalysis8 <- createCmAnalysis(analysisId = 8,
                                description = "Not matching; Do not use cross validation; Risk window end 4 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs4)

cmAnalysis9 <- createCmAnalysis(analysisId = 9,
                                description = "1:1 matching; Use cross validation; Use covariates; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs5)

cmAnalysis10 <- createCmAnalysis(analysisId = 10,
                                description = "1:1 matching; Use cross validation; Use covariates; Risk window end 4 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs5)

cmAnalysis11 <- createCmAnalysis(analysisId = 11,
                                description = "1:1 matching; Do not use cross validation; Use covariates; Risk window end 2 weeks",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs1,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs6)

cmAnalysis12 <- createCmAnalysis(analysisId = 12,
                                 description = "1:1 matching; Do not use cross validation; Use covariates; Risk window end 4 weeks",
                                 getDbCohortMethodDataArgs = getDbCmDataArgs,
                                 createStudyPopArgs = createStudyPopArgs2,
                                 createPs = TRUE,
                                 createPsArgs = createPsArgs,
                                 matchOnPs = TRUE,
                                 matchOnPsArgs = matchOnPsArgs,
                                 fitOutcomeModel = TRUE,
                                 fitOutcomeModelArgs = fitOutcomeModelArgs6)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5,
                       cmAnalysis6, cmAnalysis7, cmAnalysis8, cmAnalysis9, cmAnalysis10,
                       cmAnalysis11, cmAnalysis12)

tcos <- createTargetComparatorOutcomes(targetId = 831,
                                       comparatorId = 832,
                                       outcomeIds = c(833, # GI hemorrhage
                                                      902, # C difficile
                                                      84))

targetComparatorOutcomesList <- list(tcos)

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

print_multiple_cm_result(result = result, output_folder = outputFolder,
                          plot_time_to_event_period_length = 1)
