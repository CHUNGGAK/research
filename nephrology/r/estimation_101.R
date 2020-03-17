# Set environment ---------------------------------------------------------
library(CohortMethod)
library(lubridate)

setwd("/home/lee/workspace/nephrology")

source("/media/sf_shared_folder/workspace/research/ltool/ltool.R")
source("/media/sf_shared_folder/workspace/connect_information.R")

outputFolder <- file.path("output", paste("N101",
                                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                                          sep = "_"))
path_assistant(outputFolder)

excludedCovariateConceptIds <- 1503297

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

tcos <- createTargetComparatorOutcomes(targetId = 985,
                                       comparatorId = 988,
                                       outcomeIds = 976)

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

result <- read.csv("/home/lee/workspace/nephrology/output/N102_20200316_130322/view_cm/result.csv")

view_cm(result = result, output_folder = outputFolder,
        plot_time_to_event_period_length = 1,
        ps_adjustment = "stratify")
