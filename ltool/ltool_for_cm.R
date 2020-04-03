library(CohortMethod)
library(EmpiricalCalibration)

# Draw custom attrition diagram ------------------------------
draw_attrition_diagram <- function (object, targetLabel = "Target", comparatorLabel = "Comparator", 
                                    fileName = NULL, count = "person") {
    attrition <- getAttritionTable(object)
    if (count == "person") {
        targetLabel <- paste(targetLabel, "(Person)")
        comparatorLabel <- paste(comparatorLabel, "(Person)")
        
        addStep <- function(data, attrition, row) {
            label <- paste(strwrap(as.character(attrition$description[row]), 
                                   width = 30), collapse = "\n")
            data$leftBoxText[length(data$leftBoxText) + 1] <- label
            data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel, 
                                                                      ": n = ", data$currentTarget - attrition$targetPersons[row], 
                                                                      "\n", comparatorLabel, ": n = ", data$currentComparator - 
                                                                          attrition$comparatorPersons[row], sep = "")
            data$currentTarget <- attrition$targetPersons[row]
            data$currentComparator <- attrition$comparatorPersons[row]
            return(data)
        }
        
        data <- list(leftBoxText = c(paste("Original cohorts:\n", 
                                           targetLabel, ": n = ", attrition$targetPersons[1], "\n", 
                                           comparatorLabel, ": n = ", attrition$comparatorPersons[1], 
                                           sep = "")), rightBoxText = c(""), currentTarget = attrition$targetPersons[1], 
                     currentComparator = attrition$comparatorPersons[1])
    } else {
        targetLabel <- paste(targetLabel, "(Incident)")
        comparatorLabel <- paste(comparatorLabel, "(Incident)")
        
        addStep <- function(data, attrition, row) {
            label <- paste(strwrap(as.character(attrition$description[row]), 
                                   width = 30), collapse = "\n")
            data$leftBoxText[length(data$leftBoxText) + 1] <- label
            data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel, 
                                                                      ": n = ", data$currentTarget - attrition$targetExposures[row], 
                                                                      "\n", comparatorLabel, ": n = ", data$currentComparator - 
                                                                          attrition$comparatorExposures[row], sep = "")
            data$currentTarget <- attrition$targetExposures[row]
            data$currentComparator <- attrition$comparatorExposures[row]
            return(data)
        }
        
        data <- list(leftBoxText = c(paste("Original cohorts:\n", 
                                           targetLabel, ": n = ", attrition$targetExposures[1], "\n", 
                                           comparatorLabel, ": n = ", attrition$comparatorExposures[1], 
                                           sep = "")), rightBoxText = c(""), currentTarget = attrition$targetExposures[1], 
                     currentComparator = attrition$comparatorExposures[1])
    }
    
    for (i in 2:nrow(attrition)) {
        data <- addStep(data, attrition, i)
    }
    data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study population:\n", 
                                                            targetLabel, ": n = ", data$currentTarget, "\n", comparatorLabel, 
                                                            ": n = ", data$currentComparator, sep = "")
    leftBoxText <- data$leftBoxText
    rightBoxText <- data$rightBoxText
    nSteps <- length(leftBoxText)
    boxHeight <- (1/nSteps) - 0.03
    boxWidth <- 0.45
    shadowOffset <- 0.01
    arrowLength <- 0.01
    x <- function(x) {
        return(0.25 + ((x - 1)/2))
    }
    y <- function(y) {
        return(1 - (y - 0.5) * (1/nSteps))
    }
    downArrow <- function(p, x1, y1, x2, y2) {
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, 
                                                           y = y1, xend = x2, yend = y2))
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2, 
                                                           y = y2, xend = x2 + arrowLength, yend = y2 + arrowLength))
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2, 
                                                           y = y2, xend = x2 - arrowLength, yend = y2 + arrowLength))
        return(p)
    }
    rightArrow <- function(p, x1, y1, x2, y2) {
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, 
                                                           y = y1, xend = x2, yend = y2))
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2, 
                                                           y = y2, xend = x2 - arrowLength, yend = y2 + arrowLength))
        p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2, 
                                                           y = y2, xend = x2 - arrowLength, yend = y2 - arrowLength))
        return(p)
    }
    box <- function(p, x, y) {
        p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - 
                                                            (boxWidth/2) + shadowOffset, ymin = y - (boxHeight/2) - 
                                                            shadowOffset, xmax = x + (boxWidth/2) + shadowOffset, 
                                                        ymax = y + (boxHeight/2) - shadowOffset), fill = rgb(0, 
                                                                                                             0, 0, alpha = 0.2))
        p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - 
                                                            (boxWidth/2), ymin = y - (boxHeight/2), xmax = x + 
                                                            (boxWidth/2), ymax = y + (boxHeight/2)), fill = rgb(0.94, 
                                                                                                                0.94, 0.94), color = "black")
        return(p)
    }
    label <- function(p, x, y, text, hjust = 0) {
        p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, 
                                                        y = y, label = paste("\"", text, "\"", sep = "")), 
                                    hjust = hjust, size = 3.7)
        return(p)
    }
    p <- ggplot2::ggplot()
    for (i in 2:nSteps - 1) {
        p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 
                                                                  1) + (boxHeight/2))
        p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
    }
    for (i in 2:(nSteps - 1)) {
        p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, 
                        y(i))
        p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
    }
    for (i in 1:nSteps) {
        p <- box(p, x(1), y(i))
    }
    for (i in 2:(nSteps - 1)) {
        p <- box(p, x(2), y(i))
    }
    for (i in 1:nSteps) {
        p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
    }
    for (i in 2:(nSteps - 1)) {
        p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
    }
    p <- p + ggplot2::theme(legend.position = "none", plot.background = ggplot2::element_blank(), 
                            panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                            panel.border = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                            axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), 
                            axis.ticks = ggplot2::element_blank())
    if (!is.null(fileName)) 
        ggplot2::ggsave(p, filename = fileName, width = 6, height = 7, 
                        dpi = 400)
    return(p)
}


# print multiple CohortMethod result ------------------------------------------------
view_cm <- function(result, output_folder,
                    plot_time_to_event_period_length = 7,
                    ps_adjustment) {
    # Make result file directory
    file_path <- file.path(output_folder, "view_cm")
    path_assistant(file_path)
    
    result <- as.data.table(result)
    write_csv(result, file.path(file_path, "result.csv"))
    
    # Create result table
    analysisSum <- summarizeAnalyses(result, output_folder)
    
    sink(file.path(file_path, "cm_result.html"))
    analysisSum %>%
        mutate(analysisId = as.character(analysisId),
               targetId = as.character(targetId),
               comparatorId = as.character(comparatorId),
               outcomeId = as.character(outcomeId),
               targetDays = as.integer(targetDays),
               comparatorDays = as.integer(comparatorDays),
               eventsTarget = as.integer(eventsTarget),
               eventsComparator = as.integer(eventsComparator)) %>% 
        xtable() %>%
        print.xtable(type = "html", include.rownames = FALSE)
    sink()
    
    cm_data <- loadCohortMethodData(file.path(output_folder, unique(result$cohortMethodDataFolder)))
    
    for (target_var in result$targetId) {
        for (comparator_var in result$comparatorId) {
            ps_file <- unique(result[targetId == target_var &
                                         comparatorId == comparator_var &
                                         psFile != "",
                                     psFile])
            ps <- readRDS(file.path(output_folder, ps_file))
            
            sink(file.path(file_path, "propensity_model.html"))
            getPsModel(ps, cm_data) %>% 
                arrange(coefficient) %>% 
                xtable() %>% 
                print.xtable(type = "html", include.rownames = FALSE)
            sink()
            
            plotPs(ps, showCountsLabel = TRUE, showEquiposeLabel = TRUE,
                   fileName = file.path(file_path,
                                        paste("T",
                                              target_var,
                                              "C",
                                              comparator_var,
                                              "ps_plot.png",
                                              sep = "_")))
            
            matched_pop_file <- unique(result[targetId == target_var &
                                                  comparatorId == comparator_var &
                                                  strataFile != "",
                                              strataFile])
            
            matched_pop <- readRDS(file.path(output_folder, matched_pop_file))
            plotPs(matched_pop, ps,
                   showCountsLabel = TRUE, showEquiposeLabel = TRUE,
                   fileName = file.path(file_path,
                                        paste("T",
                                              target_var,
                                              "C",
                                              comparator_var,
                                              "matched_pop_ps_plot.png",
                                              sep = "_")))
            
            draw_attrition_diagram(matched_pop, fileName = file.path(file_path, paste("T",
                                                                                      target_var,
                                                                                      "C",
                                                                                      comparator_var,
                                                                                      "person_attrition_diagram(person).png",
                                                                                      sep = "_")))
            draw_attrition_diagram(matched_pop, fileName = file.path(file_path, paste("T",
                                                                                      target_var,
                                                                                      "C",
                                                                                      comparator_var,
                                                                                      "attrition_diagram(incident).png",
                                                                                      sep = "_")),
                                   count = "incident")
            
            # Evaluating covariate balance
            balance <- computeCovariateBalance(matched_pop, cm_data)
            
            if (ps_adjustment == "match") {
                plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE,
                                                fileName = file.path(file_path,
                                                                     paste("T",
                                                                           target_var,
                                                                           "C",
                                                                           comparator_var,
                                                                           "balance_scatter_plot.png",
                                                                           sep = "_")))
                plotCovariateBalanceOfTopVariables(balance,
                                                   fileName = file.path(file_path,
                                                                        paste("T",
                                                                              target_var,
                                                                              "C",
                                                                              comparator_var,
                                                                              "balance_top_variables_plot.png",
                                                                              sep = "_")))
            }
            
            sink(file.path(file_path, paste("T",
                                            target_var,
                                            "C",
                                            comparator_var,
                                            "population_characteristics.html",
                                            sep = "_")))
            createCmTable1(balance) %>% 
                xtable() %>% 
                print.xtable(type = "html", include.rownames = FALSE)
            sink()
            
            # Follow-up and power
            plotFollowUpDistribution(population = matched_pop,
                                     fileName = file.path(file_path,
                                                          paste("T",
                                                                target_var,
                                                                "C",
                                                                comparator_var,
                                                                "follow_up_distribution_plot.png",
                                                                sep = "_")))
            
            # Kaplan-Meier plot
            plotKaplanMeier(matched_pop, includeZero = FALSE,
                            fileName = file.path(file_path,
                                                 paste("T",
                                                       target_var,
                                                       "C",
                                                       comparator_var,
                                                       "kaplan_meier_plot.png",
                                                       sep = "_")))
        }
    }
    
    
    # Empirical Calibration
    # plotCalibrationEffect()
    
    for (outcome_var in unique(result$outcomeId)) {
        # Inspecting the outcome model
        sink(file.path(file_path, paste(outcome_var, "outcome_model.html", sep = "_")))
        for (outcome_model_file_var in unique(result[outcomeId == outcome_var & outcomeModelFile != "", outcomeModelFile])) {
            outcome_model <- readRDS(file.path(output_folder, outcome_model_file_var))
            if (outcome_model$outcomeModelStatus == "OK") {
                full_outcome_model <- getOutcomeModel(outcome_model, cm_data)
                cat(str_sub(outcome_model_file_var, 1, 10))
                full_outcome_model %>% 
                    xtable() %>% 
                    print.xtable(type = "html", include.rownames = FALSE)
                cat(rep("<br>", 2))
            }
        }
        sink()
        
        
        # Time-to-event plot
        plotTimeToEvent(cohortMethodData = cm_data, outcomeId = outcome_var, firstExposureOnly = FALSE,
                        washoutPeriod = 0, removeDuplicateSubjects = FALSE, minDaysAtRisk = 0,
                        riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 365,
                        endAnchor = "cohort end",
                        periodLength = plot_time_to_event_period_length,
                        fileName = file.path(file_path,
                                             paste(outcome_var, "time_to_event_plot.png", sep = "_")))
    }
}