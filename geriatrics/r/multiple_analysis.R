setwd('/home/lee/workspace')

library(PatientLevelPrediction)

studyPop1 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk = 0,
                                           riskWindowStart = 0,
                                           riskWindowEnd = 30,
                                           verbosity = 'DEBUG')
populationSettingList <- list(studyPop1)

covSet1 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useConditionGroupEraLongTerm = TRUE,
                                   useDrugGroupEraLongTerm = TRUE,
                                   useConditionOccurrenceLongTerm = TRUE,
                                   useDistinctConditionCountLongTerm = TRUE,
                                   useVisitConceptCountLongTerm = TRUE,
                                   useCharlsonIndex = TRUE,
                                   useDcsi = TRUE,
                                   useChads2 = TRUE,
                                   useChads2Vasc = TRUE)
covariateSettingList <- list(covSet1)

gbm <- setGradientBoostingMachine()
lr <- setLassoLogisticRegression()
ada <- setAdaBoost()
rf <- setRandomForest()
knn <- setKNN()
nb <- setNaiveBayes()
dt <- setDecisionTree()
mlp <- setMLP()
dnn <- setDeepNN()

modelList <- list(gbm, lr, ada, rf, knn, nb, dt, mlp, dnn)

modelAnalysisList <- createPlpModelSettings(modelList = modelList,
                                            covariateSettingList = covariateSettingList,
                                            populationSettingList = populationSettingList)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

cdmDatabaseName <- "SNUBH_2018"
cohortTable <- "cohort"
outputFolder <- file.path(getwd(), paste0('output_', format(Sys.time(), '%y-%m-%d_%H:%M:%S')))
cohortIds <- 803
cohortNames <- 'Cohort'
outcomeTable <- "lcg_cohort"
outcomeIds <- 24470007
outcomeNames <- "Frailty"

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
