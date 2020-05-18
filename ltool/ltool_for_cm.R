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
tidy_cm <- function(result, outputFolder) {
    tidy_cm_folder <- file.path(outputFolder, "tidy_cm")
    dir.create(tidy_cm_folder)
    
    write_csv(result, file.path(tidy_cm_folder, "result.csv"))
    
    vSharedPsFile <- result %>% 
        filter(sharedPsFile != "") %>% 
        pull(sharedPsFile)
    
    ps_folder <- file.path(tidy_cm_folder, "ps")
    dir.create(ps_folder)
    
    for (i in vSharedPsFile) {
        ps <- readRDS(file.path(outputFolder, i))
        
        plotPs(data = ps,
               showCountsLabel = TRUE,
               showEquiposeLabel = TRUE,
               fileName = file.path(ps_folder, paste0(str_extract(i, "Ps_l\\d+_s\\d+_p\\d+_t\\d+_c\\d+"), ".png")))
    }
    
    dPsModel <- result %>%
        filter(sharedPsFile != "" & cohortMethodDataFolder != "" ) %>% 
        distinct(sharedPsFile, cohortMethodDataFolder)
    
    for (i in 1:nrow(dPsModel)) {
        ps <- readRDS(file.path(outputFolder, dPsModel[[i, "sharedPsFile"]]))
        cm <- loadCohortMethodData(file.path(outputFolder, dPsModel[[i, "cohortMethodDataFolder"]]))
        
        psModel <- getPsModel(ps, cm)
        write_csv(psModel, file.path(ps_folder, paste0(str_extract(dPsModel[[i, "sharedPsFile"]], "Ps_l\\d+_s\\d+_p\\d+_t\\d+_c\\d+"), ".csv")))
    }
    
    dTrimmedPop <- result %>%
        filter(sharedPsFile != "" & strataFile != "" ) %>% 
        distinct(sharedPsFile, strataFile)
    
    for (i in 1:nrow(dTrimmedPop)) {
        ps <- readRDS(file.path(outputFolder, dTrimmedPop[[i, "sharedPsFile"]]))
        strata <- readRDS(file.path(outputFolder, dTrimmedPop[[i, "strataFile"]]))
        
        plotPs(strata,
               ps,
               showCountsLabel = TRUE,
               fileName = file.path(ps_folder, paste0(str_extract(dTrimmedPop[[i, "strataFile"]],
                                                                  "StratPop_l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                      ".png")))
    }
    
    vTrimmedPop <- result %>% 
        filter(strataFile != "") %>% 
        pull(strataFile)
    
    attrition_diagram_folder <- file.path(tidy_cm_folder, "attrition_diagram")
    dir.create(attrition_diagram_folder)
    
    follow_up_distribution_folder <- file.path(tidy_cm_folder, "follow_up_distribution_")
    dir.create(follow_up_distribution_folder)
    
    for (i in vTrimmedPop) {
        strata <- readRDS(file.path(outputFolder, i))
        
        drawAttritionDiagram(strata, fileName = file.path(attrition_diagram_folder,
                                                          paste0("AttritionDiagram_",
                                                                 str_extract(i,
                                                                             "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                                 ".png")))
        
        plotFollowUpDistribution(strata, fileName = file.path(follow_up_distribution_folder,
                                                              paste0("FollowUpDistribution_",
                                                                     str_extract(i,
                                                                                 "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                                     ".png")))
    }
    
    balance_folder <- file.path(tidy_cm_folder, "balance")
    dir.create(balance_folder)
    
    table1_folder <- file.path(tidy_cm_folder, "table1")
    dir.create(table1_folder)
    
    dBalance <- result %>%
        filter(strataFile != "" & cohortMethodDataFolder != "" ) %>% 
        distinct(strataFile, cohortMethodDataFolder)
    
    for (i in 1:nrow(dBalance)) {
        strata <- readRDS(file.path(outputFolder, dBalance[[i, "strataFile"]]))
        cm <- loadCohortMethodData(file.path(outputFolder, dBalance[[i, "cohortMethodDataFolder"]]))
        
        balance <- computeCovariateBalance(strata, cm) %>% 
            drop_na()
        
        write_csv(balance, file.path(balance_folder, paste0("Balance_",
                                                            str_extract(dTrimmedPop[[i, "strataFile"]],
                                                                        "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                            ".csv")))
        
        plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE,
                                        fileName = file.path(balance_folder, paste0("BalanceScatterPlot_",
                                                                                    str_extract(dTrimmedPop[[i, "strataFile"]],
                                                                                                "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                                                    ".png")))
        
        plotCovariateBalanceOfTopVariables(balance,
                                           fileName = file.path(balance_folder, paste0("BalanceOfTopVariables_",
                                                                                       str_extract(dTrimmedPop[[i, "strataFile"]],
                                                                                                   "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                                                       ".png")))
        
        table1 <- createCmTable1(balance)
        write_csv(table1, file.path(table1_folder, paste0("Table1_",
                                                          str_extract(dTrimmedPop[[i, "strataFile"]],
                                                                      "l\\d+_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_o\\d+"),
                                                          ".csv")))
    }
}
