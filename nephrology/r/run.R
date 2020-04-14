library(tidyverse)

# Edit package
code <- file("/home/lee/workspace/nephrology/package/estimation_study_146_export/R/Export.R")
code_lines <- readLines(code)
code_lines <- str_replace(string = code_lines, pattern = "(readChar\\(\\w+, \\w+.\\w+\\(\\w+\\)\\$\\w+)(\\))", replacement = "\\1, useBytes = TRUE\\2")
sink("/home/lee/workspace/nephrology/package/estimation_study_146_export/R/Export.R")
writeLines(code_lines)
sink()

install.packages("/home/lee/workspace/nephrology/package/estimation_study_146_export.tar.xz",
                 repos = NULL)

library(estimation146)

options(fftempdir = "/home/lee/FFtemp")

maxCores <- 1

minCellCount <- 5

outputFolder <- "/home/lee/workspace/nephrology/estimation146"

source("/media/sf_shared_folder/workspace/connect_information.R")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

cohortTable <- "estimation146_cohort"

databaseId <- "SNUBH"
databaseName <- cdmDatabaseName
databaseDescription <- cdmDatabaseName

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createCohorts = TRUE,
        synthesizePositiveControls = TRUE,
        runAnalyses = TRUE,
        runDiagnostics = TRUE,
        packageResults = TRUE,
        maxCores = maxCores)

setwd(file.path(outputFolder, "export"))
prepareForEvidenceExplorer("ResultsSNUBH.zip", "shinyData")
launchEvidenceExplorer("/home/lee/workspace/nephrology/estimation144/export/shinyData", blind = FALSE)
# remove.packages("estimation121", lib="~/R/x86_64-pc-linux-gnu-library/3.6")
