setwd("/home/lee/workspace/fall")

library(PatientLevelPrediction)

source("/media/sf_sf/workspace/connection_details.R")

outputFolder <- file.path(getwd(), paste0("output_", format(Sys.time(), "%y-%m-%d_%H:%M:%S")))

cohortIds <- 968
cohortNames <- "Hospitalization"
outcomeIds <- 967
outcomeNames <- "Outcome"
    
gbm <- setGradientBoostingMachine()
lr <- setLassoLogisticRegression()
ada <- setAdaBoost()
rf <- setRandomForest()
knn <- setKNN()
nb <- setNaiveBayes()
dt <- setDecisionTree()

modelList <- list(lr, gbm, ada, rf, knn, nb, dt)

# Covariate long term
covSet1 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useDemographicsAgeGroup = TRUE,
                                   useDemographicsIndexMonth = TRUE,
                                   useConditionGroupEraLongTerm = TRUE,
                                   useDrugEraLongTerm = TRUE,
                                   useConditionOccurrenceLongTerm = TRUE,
                                   useDrugExposureLongTerm = TRUE,
                                   useMeasurementLongTerm = TRUE,
                                   useProcedureOccurrenceLongTerm = TRUE,
                                   useVisitCountLongTerm = TRUE,
                                   useVisitConceptCountLongTerm = TRUE,
                                   useChads2Vasc = TRUE,
                                   useDcsi = TRUE,
                                   useCharlsonIndex = TRUE)

# Covariate medium term
covSet2 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useDemographicsAgeGroup = TRUE,
                                   useDemographicsIndexMonth = TRUE,
                                   useConditionGroupEraMediumTerm = TRUE,
                                   useDrugEraMediumTerm = TRUE,
                                   useConditionOccurrenceMediumTerm = TRUE,
                                   useDrugExposureMediumTerm = TRUE,
                                   useMeasurementMediumTerm = TRUE,
                                   useProcedureOccurrenceMediumTerm = TRUE,
                                   useVisitCountMediumTerm = TRUE,
                                   useVisitConceptCountMediumTerm = TRUE,
                                   useChads2Vasc = TRUE,
                                   useDcsi = TRUE,
                                   useCharlsonIndex = TRUE)

# Covariate short term
covSet3 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useDemographicsAgeGroup = TRUE,
                                   useDemographicsIndexMonth = TRUE,
                                   useConditionGroupEraShortTerm = TRUE,
                                   useDrugEraShortTerm = TRUE,
                                   useConditionOccurrenceShortTerm = TRUE,
                                   useDrugExposureShortTerm = TRUE,
                                   useMeasurementShortTerm = TRUE,
                                   useProcedureOccurrenceShortTerm = TRUE,
                                   useVisitCountShortTerm = TRUE,
                                   useVisitConceptCountShortTerm = TRUE,
                                   useChads2Vasc = TRUE,
                                   useDcsi = TRUE,
                                   useCharlsonIndex = TRUE)

covariateSettingList <- list(covSet1, covSet2, covSet3)

# Study population Settings
studyPop1 <- createStudyPopulationSettings(riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 0,
                                           addExposureDaysToEnd = TRUE,
                                           washoutPeriod = 0,
                                           minTimeAtRisk = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           includeAllOutcomes = TRUE,
                                           requireTimeAtRisk = FALSE,
                                           firstExposureOnly = FALSE,
                                           verbosity = "DEBUG",
                                           binary = TRUE)

populationSettingList <- list(studyPop1)

modelAnalysisList <- createPlpModelSettings(modelList = modelList,
                                            covariateSettingList = covariateSettingList,
                                            populationSettingList = populationSettingList)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

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
                             minCovariateFraction = 0,
                             normalizeData = TRUE,
                             testSplit = "person",
                             testFraction = 0.25,
                             splitSeed = NULL,
                             nfold = 3,
                             verbosity = "DEBUG")

viewMultiplePlp(analysesLocation = outputFolder)
