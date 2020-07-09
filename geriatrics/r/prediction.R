setwd("/home/lee/workspace/geriatrics")

library(PatientLevelPrediction)

source("/media/sf_sf/workspace/connection_details.R")

studyPop1 <- createStudyPopulationSettings(binary = TRUE,
                                           includeAllOutcomes = TRUE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           requireTimeAtRisk = FALSE,
                                           riskWindowStart = 0,
                                           riskWindowEnd = 90)

populationSettingList <- list(studyPop1)

#
drug_era_po_covSet <- createCohortAttrCovariateSettings(attrDatabaseSchema = cohortDatabaseSchema,
                                                        cohortAttrTable = "drug_era_po_attr",
                                                        attrDefinitionTable = "drug_era_po_def")

# Exclude age covariate; End days -1
covSet1 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useConditionGroupEraLongTerm = TRUE,
                                   useConditionOccurrenceLongTerm = TRUE,
                                   useDistinctConditionCountLongTerm = TRUE,
                                   useProcedureOccurrenceLongTerm = TRUE,
                                   useMeasurementLongTerm = TRUE,
                                   useObservationLongTerm = TRUE,
                                   useVisitConceptCountLongTerm = TRUE,
                                   useChads2Vasc = TRUE,
                                   endDays = -1)

covSet1 <- list(covSet1, drug_era_po_covSet)

# Include age covariate; End days -1
covSet2 <- createCovariateSettings(useDemographicsGender = TRUE,
                                   useDemographicsAge = TRUE,
                                   useConditionGroupEraLongTerm = TRUE,
                                   useConditionOccurrenceLongTerm = TRUE,
                                   useDistinctConditionCountLongTerm = TRUE,
                                   useProcedureOccurrenceLongTerm = TRUE,
                                   useMeasurementLongTerm = TRUE,
                                   useObservationLongTerm = TRUE,
                                   useVisitConceptCountLongTerm = TRUE,
                                   useChads2Vasc = TRUE,
                                   endDays = -1)

covariateSettingList <- list(covSet1, covSet2)

gbm <- setGradientBoostingMachine()
lr <- setLassoLogisticRegression()
ada <- setAdaBoost()
rf <- setRandomForest()
knn <- setKNN()
nb <- setNaiveBayes()
dt <- setDecisionTree()

modelList <- list(lr, gbm, ada, rf, knn, nb, dt)

modelAnalysisList <- createPlpModelSettings(modelList = modelList,
                                            covariateSettingList = covariateSettingList,
                                            populationSettingList = populationSettingList)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

cdmDatabaseName <- "SNUBH_2019"
outputFolder <- paste0("Output_", format(Sys.time(), "%Y-%m-%d_%H:%M:%S"))
cohortIds <- c(1075, 1076)
cohortNames <- c("General surgery operation", "Osteosurgery operation")
outcomeTable <- "lcohort"
outcomeIds <- c(13010002, 8010001, 1120001, 1130001)
outcomeNames <- c("Frailty(High MFS, Hip-MFS + Death + Long-term hospitalization + Emergency room re-admission)",
                  "Delirium",
                  "Discharge destination",
                  "ICU Admission")

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
                             testSplit = "subject",
                             testFraction = 0.25,
                             splitSeed = NULL,
                             nfold = 3,
                             verbosity = "INFO")

viewMultiplePlp(analysesLocation = file.path(getwd(), outputFolder))
