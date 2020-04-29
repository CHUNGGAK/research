setwd("E:/Users/DLCG001/workspace/pediatrics/output")

library(data.table)
library(rpsychi)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")

dFile <- data.table(file_name = c("All_DM_DM_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_sex_Group_Measurement_Desc_Stat.csv",
                       "Type 2_DM_age_group_Group_Measurement_Desc_Stat.csv",
                       "Type 2_DM_sex_Group_Measurement_Desc_Stat.csv"),
                    group_by_variable = c("DM", rep(c("age_group", "sex"), 2)))


for (vHospital in c("snubh", "snuh", "amc")) {
    dResult <- data.table()
    
    for (vFile in dFile$file_name) {
        d <- fread(file.path(vHospital, "desc_stat", vFile))
        v <- dFile[file_name == vFile, group_by_variable]
        
        if (dFile[file_name == vFile, group_by_variable] == "DM") {
            x <- "Type 1"
            y <- "Type 2"
        } else if (dFile[file_name == vFile, group_by_variable] == "sex") {
            x <- "Male"
            y <- "Female"
        } else {
            x <- "5-9Y"
            y <- "10-14Y"
        }
        
        for (vMeasurement in c("zbmi", "HBA1C")) {
            for (vTime in c(0, 3, 6, seq(12, 60, 12))) {
                if (vFile == "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv") {
                    d_x <- d[time == vTime &
                                 measurement == vMeasurement]
                    
                    tryCatch(anova_result <- ind.oneway.second(m = d_x[type == "mean", value],
                                                      sd = d_x[type == "sd", value],
                                                      n = d_x[type == "n", value],
                                                      unbiased = FALSE,
                                                      digits = 4),
                             error = function(e) {anova_result <<- NULL})
                    
                    if (is.null(anova_result)) {
                        p_value <- NA
                    } else {
                        p_value <- 1 - pf(anova_result$anova.table$MS[1] / anova_result$anova.table$MS[2], anova_result$anova.table$df[1], anova_result$anova.table$df[2])
                    }
                    
                    dResult <- bind_rows(dResult,
                                         data.table(DM = str_sub(vFile, 1, 6),
                                                    group = v,
                                                    measurement = vMeasurement,
                                                    time = vTime,
                                                    p_value = p_value))
                } else {
                    d_x <- d[get(v) == x &
                                 time == vTime &
                                 measurement == vMeasurement]
                    d_y <- d[get(v) == y &
                                 time == vTime &
                                 measurement == vMeasurement]
                    
                    dResult <- bind_rows(dResult,
                                         data.table(DM = str_sub(vFile, 1, 6),
                                                    group = v,
                                                    measurement = vMeasurement,
                                                    time = vTime,
                                                    p_value = t_test(m1 = d_x[type == "mean", value],
                                                                     m2 = d_y[type == "mean", value],
                                                                     s1 = d_x[type == "sd", value],
                                                                     s2 = d_y[type == "sd", value],
                                                                     n1 = d_x[type == "n", value],
                                                                     n2 = d_y[type == "n", value])$p_value))
                }
            }
        }
    }
    write_csv(dResult, file.path(vHospital, "p-value.csv"))
}
