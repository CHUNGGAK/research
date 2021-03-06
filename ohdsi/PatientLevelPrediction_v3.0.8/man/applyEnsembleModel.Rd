% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ensemble.R
\name{applyEnsembleModel}
\alias{applyEnsembleModel}
\title{Apply trained ensemble model on new data Apply a Patient Level Prediction model on Patient Level
Prediction Data and get the predicted risk in [0,1] for each person in the population. If the user
inputs a population with an outcomeCount column then the function also returns the evaluation of
the prediction (AUC, brier score, calibration)}
\usage{
applyEnsembleModel(population, dataList, ensembleModel, analysisId = NULL,
  calculatePerformance = T)
}
\arguments{
\item{population}{The population of people who you want to predict the risk for}

\item{dataList}{The plpData list for the population}

\item{ensembleModel}{The trained ensemble model returned by running runEnsembleModel}

\item{analysisId}{The analysis ID, which is the ID of running ensemble model training.}

\item{calculatePerformance}{Whether to also calculate the performance metrics [default TRUE]}
}
\description{
Apply trained ensemble model on new data Apply a Patient Level Prediction model on Patient Level
Prediction Data and get the predicted risk in [0,1] for each person in the population. If the user
inputs a population with an outcomeCount column then the function also returns the evaluation of
the prediction (AUC, brier score, calibration)
}
\examples{
\dontrun{
# load the model and data
plpData <- loadPlpData("plpdata/")
results <- PatientLevelPrediction::runEnsembleModel(population,
                                                    dataList = list(plpData, plpData),
                                                    modelList = list(model, model),
                                                    testSplit = "person",
                                                    testFraction = 0.2,
                                                    nfold = 3,
                                                    splitSeed = 1000,
                                                    ensembleStrategy = "stacked")
# use the same population settings as the model:
populationSettings <- plpModel$populationSettings
populationSettings$plpData <- plpData
population <- do.call(createStudyPopulation, populationSettings)

# get the prediction, please make sure the ensemble strategy for training and apply is the same:
prediction <- applyEnsembleModel(population,
                                 dataList = list(plpData, plpData),
                                 ensembleModel = results,
                                 analysisId = NULL)$prediction
}
}
