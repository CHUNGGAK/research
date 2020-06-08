# Set the analysis environment --------------------------------------------
# setwd("Set your working directory")


# Set environment ---------------------------------------------------------
library(data.table)
library(tidyverse)
library(gridExtra)
library(lubridate)

source("r/ltool.R")


# Make directory ----------------------------------------------------------
data_path <- "data"
output_path <- "output_v3"
dir.create(output_path)


# Preprocess data ---------------------------------------------------------
time_list <- c("00", "03", "06", "12", "24", "36", "48", "60")

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
                  rep(time_list, each = 3),
                  "M_AVG")) %>% 
    rename_at(vars(matches("M_AVG$")), funs(str_replace(., "M_AVG$", ""))) %>% 
    as.data.table()

zbmi <- fread("inst/calculate_zbmi.csv") %>% 
    mutate(sex = ifelse(GENDER == 1, "Male", "Female"),
           sex = as.factor(sex)) %>% 
    select(sex, age_00M, L, M, S) %>% 
    as.data.table()

for (time_var in time_list) {
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


# Create Table 1 ----------------------------------------------------------
dt_weightedSd <- data.table(variable = c("age", "zbmi", "hba1c"),
                            T1DM_deviation = c(sum((datum[DM == "Type 1", age_00] / 12 - 9.87)^2, na.rm = TRUE),
                                               sum((datum[DM == "Type 1", zbmi_00] - (-0.45))^2, na.rm = TRUE),
                                               sum((datum[DM == "Type 1", HBA1C_00] - 10.24)^2, na.rm = TRUE)),
                            T1DM_n = c(sum(!is.na(datum[DM == "Type 1", age_00]), na.rm = TRUE),
                                       sum(!is.na(datum[DM == "Type 1", zbmi_00] ), na.rm = TRUE),
                                       sum(!is.na(datum[DM == "Type 1", HBA1C_00]), na.rm = TRUE)),
                            T2DM_deviation = c(sum((datum[DM == "Type 2", age_00] / 12 - 12.22)^2, na.rm = TRUE),
                                               sum((datum[DM == "Type 2", zbmi_00] - 1.53)^2, na.rm = TRUE),
                                               sum((datum[DM == "Type 2", HBA1C_00] - 9.45)^2, na.rm = TRUE)),
                            T2DM_n = c(sum(!is.na(datum[DM == "Type 2", age_00]), na.rm = TRUE),
                                       sum(!is.na(datum[DM == "Type 2", zbmi_00]), na.rm = TRUE),
                                       sum(!is.na(datum[DM == "Type 2", HBA1C_00]), na.rm = TRUE)),
                            Total_deviation = c(sum((datum$age_00 / 12 - 10.46)^2, na.rm = TRUE),
                                                sum((datum$zbmi_00 - 0.03)^2, na.rm = TRUE),
                                                sum((datum$HBA1C_00 - 10.03)^2, na.rm = TRUE)))

dt_weightedSd <- dt_weightedSd %>% 
    mutate(Total_n = T1DM_n + T2DM_n)

write_csv(dt_weightedSd, file.path(output_path, "Data_for_Weighted_Sd.csv"))
