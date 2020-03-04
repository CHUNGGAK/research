setwd('/home/lee/workspace/etc')

library(PatientLevelPrediction)

source("/home/lee/workspace/connect_information.R")

# Population settings -----------------------------------------------------
studyPop1 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk = 0,
                                           riskWindowStart = 1,
                                           riskWindowEnd = 30,
                                           verbosity = 'INFO')

studyPop2 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk = 0,
                                           riskWindowStart = 1,
                                           riskWindowEnd = 365,
                                           verbosity = 'INFO')

studyPop3 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk = 0,
                                           riskWindowStart = 1,
                                           riskWindowEnd = 1095,
                                           verbosity = 'INFO')

studyPop4 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk = 0,
                                           riskWindowStart = 1,
                                           riskWindowEnd = 1825,
                                           verbosity = 'INFO')
populationSettingList <- list(studyPop1, studyPop2, studyPop3, studyPop4)


# Covariate settings ------------------------------------------------------
covSet1 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useDemographicsAgeGroup = TRUE,
                                   useDrugGroupEraLongTerm = TRUE,
                                   useDrugGroupEraOverlapping = TRUE,
                                   useConditionOccurrenceLongTerm = TRUE,
                                   useProcedureOccurrenceLongTerm = TRUE,
                                   useMeasurementLongTerm = TRUE,
                                   useMeasurementValueLongTerm = TRUE,
                                   useObservationLongTerm = TRUE,
                                   useVisitConceptCountLongTerm = TRUE)
covariateSettingList <- list(covSet1)



# Model settings ----------------------------------------------------------
gbm <- setGradientBoostingMachine()
lr <- setLassoLogisticRegression()
ada <- setAdaBoost()
rf <- setRandomForest()
knn <- setKNN()
nb <- setNaiveBayes()
dt <- setDecisionTree()

modelList <- list(gbm, lr, ada, rf, knn, nb, dt)

modelAnalysisList <- createPlpModelSettings(modelList = modelList,
                                            covariateSettingList = covariateSettingList,
                                            populationSettingList = populationSettingList)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

cdmDatabaseName <- "SNUBH_2019"
cohortTable <- "cohort"
outputFolder <- file.path(getwd(), paste0(format(Sys.time(), "%Y%m%d_%H%M%S")))
cohortIds <- 959
cohortNames <- "Polysomnograpy test patients"
outcomeTable <- "cohort"
outcomeIds <- 963
outcomeNames <- "ER visits"

allresults <- runPlpAnalyses(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cdmDatabaseName = cdmDatabaseName,
                             oracleTempSchema = oracleTempSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             outcomeDatabaseSchema = outcomeDatabaseSchema,
                             outcomeTable = outcomeTable,
                             outputFolder = outputFolder,
                             modelAnalysisList = modelAnalysisList,
                             cohortIds = cohortIds,
                             cohortNames = cohortNames,
                             outcomeIds = outcomeIds,
                             outcomeNames = outcomeNames,
                             maxSampleSize = NULL,
                             minCovariateFraction = 30,
                             normalizeData = TRUE,
                             testSplit = 'subject',
                             testFraction = 0.25,
                             splitSeed = NULL,
                             nfold = 10,
                             verbosity = 'DEBUG')

viewMultiplePlp(analysesLocation = outputFolder)
