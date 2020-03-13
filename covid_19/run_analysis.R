library(FluDeathValidation)
library(data.table)
library(tidyverse)
library(xtable)

databaseName <- "CDM_SNUBH_2019"

source("/media/sf_shared_folder/workspace/connect_information.R")

cohortTable <- "FlueDeathValidationCohortTable"

outputFolder <- "/home/lee/workspace/covid_19/output"

options(fftempdir = "/home/lee/workspace/covid_19/fftemp")
connectionDetails <- createConnectionDetails(dbms = dbms,
                                             user = user,
                                             password = password,
                                             server = server)

execute(connectionDetails = connectionDetails,
        databaseName = databaseName,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createCohorts = TRUE,
        runValidation = TRUE,
        packageResults = TRUE)

output_path <- file.path(outputFolder, databaseName)

sink(file.path(outputFolder, "analysis_result.html"))
for (analysis_var in list.files(output_path)) {
    x <- readRDS(file.path(output_path, analysis_var, "validationResult.rds"))
    
    cat(paste(analysis_var, "Model Settings:", x$inputSetting$modelSettings$model))
    cat(rep("<br>", 2))
    
    # x$inputSetting$modelSettings$modelParameters
    
    # x$inputSetting$testSplit
    # x$inputSetting$nfold
    
    cat(paste(analysis_var, "Population Settings"))
    data.table(Setting = c("includeAllOutcomes", "firstExposureOnly", "washoutPeriod",
                           "removeSubjectsWithPriorOutcome", "requireTimeAtRisk",
                           "minTimeAtRisk", "addExposureDaysToStart", "riskWindowStart",
                           "addExposureDaysToEnd", "riskWindowEnd"),
               Value = c(as.character(x$inputSetting$populationSettings$includeAllOutcomes),
                         as.character(x$inputSetting$populationSettings$firstExposureOnly),
                         as.character(x$inputSetting$populationSettings$washoutPeriod),
                         as.character(x$inputSetting$populationSettings$removeSubjectsWithPriorOutcome),
                         as.character(x$inputSetting$populationSettings$requireTimeAtRisk),
                         as.character(x$inputSetting$populationSettings$minTimeAtRisk),
                         as.character(x$inputSetting$populationSettings$addExposureDaysToStart),
                         as.character(x$inputSetting$populationSettings$riskWindowStart),
                         as.character(x$inputSetting$populationSettings$addExposureDaysToEnd),
                         as.character(x$inputSetting$populationSettings$riskWindowEnd))) %>% 
        xtable() %>% 
        print.xtable(type = "html", include.rownames = FALSE)
    cat(rep("<br>", 2))
    
    # x$inputSetting$dataExtrractionSettings$covariateSettings
    
    cat(paste(analysis_var, "Performance Evaluation Statistics"))
    as.data.table(x$performanceEvaluation$evaluationStatistics) %>% 
        select(Metric, Value) %>% 
        xtable() %>% 
        print.xtable(type = "html", include.rownames = FALSE)
    cat(rep("<br>", 2))
}
sink()
