library(CohortMethod)
library(tidyverse)
library(xtable)
library(data.table)
library(scales)

# Path assistant ----------------------------------------------------------
path_assistant <- function(path) {
  if (!dir.exists(path)) {
    cat(paste0("'", path, "'", " does not exist. Therefore, new directory is created."))
    dir.create(path, recursive = TRUE)
  } else {
    cat(paste0("'", path, "'", " exists, Therefore, use an existing path."))
  }
}


# Remove outlier with quantile --------------------------------------------
remove_outlier_with_qti <- function(datum, na.rm = TRUE) {
  qti <- quantile(datum, probs = c(0.25, 0.75), na.rm = na.rm)
  a <- 1.5 * IQR(datum, na.rm = na.rm)
  output <- datum
  output[datum < (qti[1] - a)] <- NA
  output[datum > (qti[2] + a)] <- NA
  return(output)
}


# Print t-test ------------------------------------------------------------
print_t_test <- function(x, y, title, white_space = 2) {
  cat(title)
  
  output <- t.test(x, y)
  data.frame(Method = output$method,
             t = output$statistic,
             degrees_of_freedom = output$parameter,
             p_value = output$p.value,
             alternative_hypothesis = output$alternative,
             confidence_interval_lower = output$conf.int[1],
             confidence_interval_upper = output$conf.int[2],
             mean_of_x = output$estimate[1],
             mean_of_y = output$estimate[2]) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
  
  cat(rep("<br>", white_space))
}


# Print chi-square test ---------------------------------------------------
print_chisq_test <-  function(x, title, white_space = 2) {
  cat(title)
  
  output <- chisq.test(x)
  data.frame(Method = output$method,
             X_squared = output$statistic,
             degrees_of_freedom = output$parameter,
             p_value = output$p.value) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
  cat("<br>")
  
  cat("Observed(Expected)")
  matrix(c(paste0(output$observed[1], "(", round(output$expected[1]), ")"),
           paste0(output$observed[2], "(", round(output$expected[2]), ")"),
           paste0(output$observed[3], "(", round(output$expected[3]), ")"),
           paste0(output$observed[4], "(", round(output$expected[4]), ")")
  ), ncol = 2, dimnames = list(Sex = c("Female", "Male"), Drug = c("False", "True"))) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
  cat("<br>")
  
  cat("Residuals(Stdres)")
  matrix(c(paste0(output$residuals[1], "(", round(output$stdres[1]), ")"),
           paste0(output$residuals[2], "(", round(output$stdres[2]), ")"),
           paste0(output$residuals[3], "(", round(output$stdres[3]), ")"),
           paste0(output$residuals[4], "(", round(output$stdres[4]), ")")
  ), ncol = 2, dimnames = list(Sex = c("Female", "Male"), Drug = c("False", "True"))) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
  cat(rep("<br>", white_space))
}



# print multiple CohortMethod result ------------------------------------------------
view_cm <- function(result, output_folder,
                    plot_time_to_event_period_length = 7) {
  result <- as.data.table(result)
  
  # Make result file directory
  result_file_path <- file.path(output_folder, "result_file")
  path_assistant(result_file_path)
  
  # Create result table
  analysisSum <- summarizeAnalyses(result, output_folder)
  
  sink(file.path(result_file_path, "multiple_cm_result.html"))
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
  
  for (outcome_var in unique(result$outcomeId)) {
    # Create propensity score plots
    ps_file <- unique(result[outcomeId == outcome_var & psFile != "", psFile])
    ps <- readRDS(file.path(output_folder, ps_file))
    plotPs(ps, showCountsLabel = TRUE, showEquiposeLabel = TRUE,
           fileName = file.path(result_file_path,
                                paste(outcome_var, "ps_plot.png", sep = "_")))
    
    matched_pop_file <- unique(result[outcomeId == outcome_var & strataFile != "", strataFile])
    matched_pop <- readRDS(file.path(output_folder, matched_pop_file))
    plotPs(matched_pop, ps,
           showCountsLabel = TRUE, showEquiposeLabel = TRUE,
           fileName = file.path(result_file_path,
                                paste(outcome_var, "matched_pop_ps_plot.png", sep = "_")))
    
    # Evaluating covariate balance
    balance <- computeCovariateBalance(matched_pop, cm_data)
    
    plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE,
                                    fileName = file.path(result_file_path,
                                                         paste(outcome_var, "balance_scatter_plot.png", sep = "_")))
    plotCovariateBalanceOfTopVariables(balance,
                                       fileName = file.path(result_file_path,
                                                            paste(outcome_var, "balance_top_variables_plot.png", sep = "_")))
    
    sink(file.path(result_file_path, paste(outcome_var, "balance_cm_table.html", sep = "_")))
    createCmTable1(balance) %>% 
      xtable() %>% 
      print.xtable(type = "html", include.rownames = FALSE)
    sink()
    
    # Empirical Calibration
    
    # Follow-up and power
    plotFollowUpDistribution(population = matched_pop,
                             fileName = file.path(result_file_path,
                                                  paste(outcome_var, "follow_up_distribution_plot.png", sep = "_")))
    
    # Inspecting the outcome model
    sink(file.path(result_file_path, paste(outcome_var, "full_outcome_model.html", sep = "_")))
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
    
    # Kaplan-Meier plot
    plotKaplanMeier(matched_pop, includeZero = FALSE,
                    fileName = file.path(result_file_path,
                                         paste(outcome_var, "kaplan_meier_plot.png", sep = "_")))
    
    # Time-to-event plot
    plotTimeToEvent(cohortMethodData = cm_data, outcomeId = outcome_var, firstExposureOnly = FALSE,
                    washoutPeriod = 0, removeDuplicateSubjects = FALSE, minDaysAtRisk = 0,
                    riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 365,
                    endAnchor = "cohort end",
                    periodLength = plot_time_to_event_period_length,
                    fileName = file.path(result_file_path,
                                         paste(outcome_var, "time_to_event_plot.png", sep = "_")))
  }
}


# Create frequency and prop table -----------------------------------------
table_freq_prop <- function(x, print_option = FALSE, margin_option = FALSE) {
    if(class(x) != "table") {
        stop("table_freq_prop function requires table class input")
    }
    
    prop <- prop.table(x, margin = margin_option)
    
    for (i in 1:length(x)) {
        x[i] <- paste0(x[i], "(", percent(prop[i]), ")")
    }
    
    if (print_option == FALSE) {
        return(x)
    } else {
        addtorow <- list()
        addtorow$pos <- list(0, 0)
        addtorow$command <- c(paste("<tr> <th></th> <th>", names(attr(x, "dimnames"))[2], "</th> <th></th> </tr><br>"),
                              paste("<tr> <th>", names(attr(x, "dimnames"))[1], "</th> <th> FALSE </th> <th> TRUE </th> </tr> <br>"))
        
        x %>% 
            xtable() %>% 
            print.xtable(type = "html", add.to.row = addtorow, include.colnames = FALSE)
        cat(rep("<br>", 2))
    }
}


# Perform t-test by statistics --------------------------------------------
t_test <- function(m1, m2, m0 = 0, s1, s2, n1, n2, var.equal = FALSE) {
    if (var.equal == FALSE) {
        se <- sqrt(s1^2 / n1 + s2^2 / n2)
        df <- (s1^2 / n1 + s2^2 / n2)^2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    } else {
        se <- sqrt((1 / n1 + 1 / n2) * ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
        df <- n1 + n2 - 2
    }
    
    t <- m1 - m2 - m0 / se
    result <- data.frame(c("Difference of means", "Std Error", "t", "p-value"),
                         c(m1 - m2, se, t, 2 * pt(-abs(t), df)))
}
