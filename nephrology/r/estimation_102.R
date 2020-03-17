# Set environment ---------------------------------------------------------
library(CohortMethod)
library(lubridate)

setwd("/home/lee/workspace/nephrology")

source("/media/sf_shared_folder/workspace/research/ltool/ltool.R")
source("/media/sf_shared_folder/workspace/connect_information.R")

outputFolder <- file.path("output", paste("N102",
                                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                                          sep = "_"))
path_assistant(outputFolder)

excludedCovariateConceptIds <- c(1503297, #
                                 21601782, # drug_era group during day -365 through 0 days relative to index: AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM
                                 21601822, # drug_era group during day -365 through 0 days relative to index: ANGIOTENSIN II ANTAGONISTS, PLAIN
                                 21601823) # drug_era group during day -365 through 0 days relative to index: Angiotensin II antagonists, plain

covariateSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                             useDemographicsAgeGroup = TRUE,
                                             useDemographicsIndexYear = TRUE,
                                             useDemographicsIndexMonth = TRUE,
                                             useConditionGroupEraLongTerm = TRUE,
                                             useConditionGroupEraShortTerm = TRUE,
                                             useDrugGroupEraLongTerm = TRUE,
                                             useDrugGroupEraShortTerm = TRUE,
                                             useDrugGroupEraOverlapping = TRUE,
                                             useProcedureOccurrenceLongTerm = TRUE,
                                             useProcedureOccurrenceShortTerm = TRUE,
                                             useMeasurementRangeGroupLongTerm = TRUE,
                                             useObservationLongTerm = TRUE,
                                             useObservationShortTerm = TRUE,
                                             useChads2 = TRUE,
                                             useChads2Vasc = TRUE,
                                             useDcsi = TRUE,
                                             useCharlsonIndex = TRUE,
                                             excludedCovariateConceptIds = excludedCovariateConceptIds,
                                             addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(studyStartDate = "",
                                                   studyEndDate = "",
                                                   firstExposureOnly = FALSE,
                                                   removeDuplicateSubjects = "keep all",
                                                   restrictToCommonPeriod = FALSE,
                                                   washoutPeriod = 0,
                                                   maxCohortSize = 0,
                                                   excludeDrugsFromCovariates = TRUE,
                                                   covariateSettings = covariateSettings)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                      priorOutcomeLookback = 99999,
                                                      censorAtNewRiskWindow = TRUE,
                                                      riskWindowStart = 7,
                                                      startAnchor = "cohort start",
                                                      minDaysAtRisk = 1,
                                                      riskWindowEnd = 0,
                                                      endAnchor = "cohort end")

createPsArgs <- createCreatePsArgs(maxCohortSizeForFitting = 250000,
                                   errorOnHighCorrelation = TRUE,
                                   stopOnError = TRUE)

stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5,
                                           baseSelection = "target")

fitOutcomeModelArgs <- createFitOutcomeModelArgs(modelType = "cox",
                                                 stratified = TRUE)

cmAnalysis <- createCmAnalysis(analysisId = 1,
                               description = "Stratification",
                               getDbCohortMethodDataArgs = getDbCmDataArgs,
                               createStudyPopArgs = createStudyPopArgs,
                               createPs = TRUE,
                               createPsArgs = createPsArgs,
                               stratifyByPs = TRUE,
                               stratifyByPsArgs = stratifyByPsArgs,
                               fitOutcomeModel = TRUE,
                               fitOutcomeModelArgs = fitOutcomeModelArgs)

cmAnalysisList <- list(cmAnalysis)

tcos <- createTargetComparatorOutcomes(targetId = 986,
                                       comparatorId = 987,
                                       outcomeIds = 976)

targetComparatorOutcomesList <- list(tcos)

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             user = user,
                                             password = password,
                                             server = server)

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

view_cm(result = result, output_folder = outputFolder,
        plot_time_to_event_period_length = 1)
