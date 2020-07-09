library(PatientLevelPrediction)

source("/media/sf_shared_folder/workspace/connect_information.R")

wd <- "/home/lee/workspace/psg"
dir.create(wd)
setwd(wd)

outputFolder <- file.path(wd, "output", paste("onlyPSG_",format(Sys.time(), "%y-%m-%d_%H:%M:%S")))

cohortIds <- 1141
cohortNames <- "Polysomnography test patients"
outcomeIds <- 963
outcomeNames <- "O: Emergency room visit"


gbm <- setGradientBoostingMachine()
lr <- setLassoLogisticRegression()
ada <- setAdaBoost()
rf <- setRandomForest()
knn <- setKNN()
nb <- setNaiveBayes()
dt <- setDecisionTree()

modelList <- list(lr, gbm, ada, rf, knn, nb, dt)

includedCovariateConceptIds <- read.csv("/media/sf_shared_folder/workspace/research/psg/psg_concept_id.csv")$concept_id

covSet <- createCovariateSettings(useDemographicsGender = TRUE,
                                  useDemographicsAgeGroup = TRUE,
                                  useMeasurementValueShortTerm = TRUE,
                                  useObservationShortTerm = TRUE,
                                  includedCovariateConceptIds = includedCovariateConceptIds,
                                  shortTermStartDays = -7)
        
covariateSettingList <- list(covSet)

studyPop1 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 30,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 30,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 29,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop2 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 30,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 90,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 29,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop3 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 30,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 180,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 29,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop4 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 30,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 364,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop5 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 90,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 364,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop6 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 180,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 364,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop7 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 1095,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 30,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 365,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop8 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 1095,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 90,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 365,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

studyPop9 <- createStudyPopulationSettings(riskWindowStart = 1,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 1095,
                                           addExposureDaysToEnd = FALSE,
                                           washoutPeriod = 180,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk = 365,
                                           includeAllOutcomes = TRUE,
                                           firstExposureOnly = FALSE,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           binary = TRUE,
                                           verbosity = "INFO")

populationSettingList <- list(studyPop1, studyPop2, studyPop3, studyPop4, studyPop5,
                              studyPop6, studyPop7, studyPop8, studyPop9)

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
                             testSplit = "subject",
                             testFraction = 0.25,
                             splitSeed = NULL,
                             nfold = 3,
                             verbosity = "INFO")

viewMultiplePlp(analysesLocation = outputFolder)
