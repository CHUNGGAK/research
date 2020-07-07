# Set the analysis environment --------------------------------------------
# setwd("Set your working directory")


# Set environment ---------------------------------------------------------
library(data.table)
library(tidyverse)
library(gridExtra)
library(lubridate)


# Make directory ----------------------------------------------------------
data_path <- "data"
output_path <- "output_v4"
dir.create(output_path)

v_time <- c("00", "03", "06", "12", "24", "36", "48", "60")


# Preprocess data ---------------------------------------------------------
if (file.exists(file.path(data_path, "p_data.csv"))) {
    datum <- fread(file.path(data_path, "p_data.csv"))
} else {
    r_data <- fread(file.path(data_path, "data.csv"))
    
    datum <- r_data %>% 
        mutate(PERSON_ID = as.character(PERSON_ID),
               sex = ifelse(GENDER_CONCEPT_ID == 8507, "Male", "Female"),
               sex = as.factor(sex),
               age_00 = ((year(COHORT_START_DATE) - YEAR_OF_BIRTH) * 12) + 
                   (month(COHORT_START_DATE) - MONTH_OF_BIRTH),
               age_group = cut(age_00, breaks = c(0, 5 * 12, 10 * 12, 15 * 12),
                               labels = c("0-4Y", "5-9Y", "10-14Y"),
                               right = FALSE),
               COHORT_START_DATE = ymd(COHORT_START_DATE),
               DKA = ifelse(DKA_YN_NEW == 0, FALSE, TRUE),
               DM = ifelse(DM == 1, "Type 1", "Type 2"),
               DM = as.factor(DM)) %>% 
        filter(DM != 0 & # 조건 1: 1, 2형 당뇨 환자만 포함한다
                   ((!is.na(HEIGHT_00M_AVG) & !is.na(WEIGHT_00M_AVG)) | !is.na(HBA1C_00M_AVG)) &
                   # 조건 2: 키, 몸무게 값이 있거나, HbA1c 값이 있어야 한다
                   age_00 > 0 &
                   age_00 < 15 * 12 & # 조건 3: 0세 미만, 15세 이상 환자 제거
                   !(age_group == "0-4Y" & DM == "Type 2") # 조건 4: 0-4Y세 2형 당뇨 환자 제거
        ) %>% 
        select(PERSON_ID, sex, age_00, age_group, DKA, DM, 
               paste0(c("HEIGHT", "WEIGHT", "HBA1C"),
                      "_",
                      rep(v_time, each = 3),
                      "M_AVG")) %>% 
        rename_at(vars(matches("M_AVG$")), funs(str_replace(., "M_AVG$", ""))) %>% 
        as.data.table()
    
    zbmi <- fread("inst/calculate_zbmi.csv") %>% 
        mutate(sex = ifelse(GENDER == 1, "Male", "Female"),
               sex = as.factor(sex)) %>% 
        select(sex, age_00M, L, M, S) %>% 
        as.data.table()
    
    for (time_var in v_time) {
        # Calculate BMI
        datum[, paste("bmi", time_var, sep = "_")] <- datum[, get(paste("WEIGHT", time_var, sep = "_"))] /
            (datum[, get(paste("HEIGHT", time_var, sep = "_"))] / 100)^2
        
        # Calculate age
        if (time_var != "00") {
            datum[, paste("age", time_var, sep = "_")] <- datum$age_00 + as.integer(time_var)
        }
        
        # Add HbA1c abnormal status
        hba1c_index <- paste("HBA1C", time_var, sep = "_")
        datum[datum[, get(paste("HBA1C", time_var, sep = "_"))] > 30, paste("HBA1C", time_var, sep = "_")] <- NA
        datum[, paste("hba1c_abnormal", time_var, sep = "_")] <-
            cut(datum[, get(paste("HBA1C", time_var, sep = "_"))],
                breaks = c(-Inf, 7, Inf),
                labels = c("FALSE", "TRUE"),
                right = FALSE)
        
        # Calculate BMI z-score and obesity status
        con_vec <- c()
        dis_vec <- c()
        for (i in 1:nrow(datum)) {
            sex_var <- datum[i, sex]
            age_var <- datum[i, age_00]
            l <- zbmi[sex == sex_var & age_00M == age_var, L]
            m <- zbmi[sex == sex_var & age_00M == age_var, M]
            s <- zbmi[sex == sex_var & age_00M == age_var, S]
            y <- (((datum[i, get(paste("bmi", time_var, sep = "_"))] / m)^l) - 1) / (l * s)
            if (is.na(y) | y < -5 | y > 5) {
                y <- NA
                y_group <- NA
            } else if (y < 1.0364) {
                y_group <- FALSE
            } else {
                y_group <- TRUE
            }
            con_vec <- c(con_vec, y)
            dis_vec <- c(dis_vec, y_group)
            # datum[i, paste("zbmi", time_var, sep = "_")] <- y
            # datum[i, paste("obesity", time_var, sep = "_")] <- y_group
        }
        datum[, paste("zbmi", time_var, sep = "_")] <- con_vec
        datum[, paste("obesity", time_var, sep = "_")] <- dis_vec
    }
    
    # Compute the change of BMI z score and HbA1c
    datum <- datum %>% 
        mutate(zbmi_delta_36 = zbmi_36 - zbmi_03,
               zbmi_delta_60 = zbmi_60 - zbmi_03,
               hba1c_delta_36 = HBA1C_36 - HBA1C_03,
               hba1c_delta_60 = HBA1C_60 - HBA1C_03) %>% 
        as.data.table()
    
    write.csv(datum, file.path(data_path, "p_data.csv"))
}


# Create a Table for Menemar Test -----------------------------------------
for (var_dm in c("Type 1", "Type 2")) {
    for (var_group in c("total", "sex", "age_group")) {
        d_tmp <- datum %>% 
            filter()
        for (i in c("obesity", "hba1c_abnormal")) {
            for (j in c(36, 60)) {
                d_mcnemar <- d_tmp %>% 
                    select(obesity_03, paste0(i, "_", j)) %>% 
                    table()
                
                write.table(d_mcnemar, file.path(output_path, paste0(i, "_", j, ".txt")), sep = ",")
            }
        }
    }
}



# Calculate Mean/SD/N -----------------------------------------------------
d_weighted_mean <- data.table(time = v_time,
                              measurement = rep(c("zbmi", "HBA1C"), each = 8 * 11),
                              group = c(rep(c("total"), 8 * 2),
                                        rep(c("Male", "Female"), each = 8),
                                        rep("0-4Y", 8),
                                        rep("5-9Y", 8),
                                        rep("10-14Y", 8),
                                        rep(c("Male", "Female"), each = 8),
                                        rep("5-9Y", 8),
                                        rep("10-14Y", 8)),
                              dm = c(rep(c("Type 1", "Type 2"), each = 8),
                                     rep("Type 1", 8 * 5),
                                     rep("Type 2", 8 * 4)),
                              value = c(-0.45,-0.08,-0.08,0.12,0.34,0.54,0.85,0.98, # zbmi / t1d / total
                                        1.53,1.48,1.31,1.7,1.79,1.65,1.91,1.82, # zbmi / t2d / total
                                        -0.54,-0.16,-0.21,-0.03,0.24,0.41,0.76,0.78, # zbmi / t1d / male
                                        -0.39,0,0.02,0.22,0.42,0.65,0.93,1.15, # zbmi / t1d / female
                                        -0.37,0.28,0.04,0.43,0.29,0.34,0.37,0.41, # zbmi / t1d / 0-4y
                                        -0.45,-0.04,-0.03,0.12,0.44,0.65,1.27,1.5, # zbmi / t1d / 5-9y
                                        -0.48,-0.2,-0.14,0.03,0.3,0.54,0.77,0.92, # zbmi / t1d / 10-14y
                                        1.37,1.31,1.26,1.5,1.64,1.72,2,1.64, # zbmi / t2d / male
                                        1.69,1.73,1.38,1.92,1.92,1.57,1.83,2.03, # zbmi / t2d / female
                                        1.47,1.61,0.81,1.63,1.93,2.19,1.52,2.16, # zbmi / t2d / 5-9y
                                        1.53,1.46,1.42,1.71,1.76,1.57,2.01,1.78, # zbmi / t2d / 10-14y
                                        10.24,7.61,7.82,8.09,8.32,8.47,8.63,8.72, # hba1c / t1d / total
                                        9.45,6.52,6.6,7.06,7.89,8.35,8.19,8.81, # hba1c / t2d / total
                                        9.95,7.65,7.67,7.88,8.04,8.19,8.25,8.27, # hba1c / t1d / male
                                        10.46,7.58,7.93,8.24,8.53,8.69,8.94,9.06, # hba1c / t1d / female
                                        9.49,7.6,7.63,7.77,7.72,7.78,7.85,7.83, # hba1c / t1d / 0-4y
                                        9.79,7.24,7.45,7.75,8.08,8.27,8.55,8.77, # hba1c / t1d / 5-9y
                                        10.63,7.81,8.05,8.37,8.64,8.73,8.86,8.91, # hba1c / t1d / 10-14y
                                        9.63,6.35,6.44,7.02,7.54,8.49,8.11,8.42, # hba1c / t2d / male
                                        9.25,6.71,6.79,7.11,8.21,8.24,8.29,9.22, # hba1c / t2d / female
                                        8.58,6.93,6.57,7.23,8.02,8.46,7.41,8.04, # hba1c / t2d / 5-9y
                                        9.58,6.45,6.6,7.04,8.86,8.34,8.29,8.89)) # hba1c / t2d / 10-14y

d_output <- data.table()

for (var_measurement in c("zbmi", "HBA1C")) {
    for (var_time in v_time) {
        for (var_dm in c("Type 1", "Type 2")) {
            for (var_group in c("total", "Male", "Female", "0-4Y", "5-9Y", "10-14Y")) {
                if (var_dm == "Type 2" & var_group == "0-4Y") {
                    next()
                }
                
                column_name <- paste0(var_measurement, "_", var_time)
                weighted_mean <- d_weighted_mean[measurement == var_measurement &
                                                     time == var_time &
                                                     dm == var_dm &
                                                     group == var_group,
                                                 value]
                
                if (var_group == "total") {
                    value <- datum[DM == var_dm, ..column_name] - weighted_mean
                } else if (var_group %in% c("Male", "Female")) {
                    value <- datum[DM == var_dm &
                                       sex == var_group, ..column_name] - weighted_mean
                } else {
                    value <- datum[DM == var_dm &
                                       age_group == var_group, ..column_name] - weighted_mean
                }
                
                d_tmp <- data.table(measurement = var_measurement,
                           time = var_time,
                           dm = var_dm,
                           group = var_group,
                           value = sum(value^2, na.rm = TRUE),
                           n = sum(!is.na(value)))
                           
                if (nrow(d_output) == 0) {
                    d_output <- d_tmp
                } else {
                    d_output <- bind_rows(d_output,
                                          d_tmp)
                }
                
            }
        }
    }
}

write_csv(d_output, file.path(output_path, "output_v4.csv"))
