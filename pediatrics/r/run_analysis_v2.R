# Set the analysis environment --------------------------------------------
setwd("Set your working directory")


# Set environment ---------------------------------------------------------
library(data.table)
library(tidyverse)
library(gridExtra)
library(lubridate)

source("r/ltool.R")


# Make directory ----------------------------------------------------------
data_path <- "data"
output_path <- "output"
desc_stat_path <- file.path(output_path, "desc_stat")
plot_path <- file.path(output_path, "plot")

path_assistant(desc_stat_path, silence = TRUE)
path_assistant(plot_path, silence = TRUE)


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
           DM = ifelse(DM == "Type 1", "Type 1", "Type 2"),
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


# Get abnormal ratio ------------------------------------------------------
sink(file.path(output_path, "3_Abnormal_Ratio_SNUH.html"))
for (x in c("obesity", "hba1c_abnormal")) {
    for (time_var in time_list) {
        cat(paste("The ratio of", x ,"by DM type;", time_var, "time"))
        col_var <- c("DM", paste(x, time_var, sep = "_"))
        try(table(datum[, ..col_var]) %>% 
                xtable() %>% 
                print.xtable(type = "html", include.rownames = FALSE))
        for (dm_var in c("Type 1", "Type 2")) {
            cat(paste("The ratio of", x ,"by sex;", time_var, "time for", dm_var, "DM patients"))
            col_var <- c("sex", paste(x, time_var, sep = "_"))
            try(table(datum[DM == dm_var, ..col_var]) %>% 
                    xtable() %>% 
                    print.xtable(type = "html", include.rownames = FALSE))
            
            cat(paste("The ratio of", x, "by age group;", time_var, "time for", dm_var, "DM patients"))
            col_var <- c("age_group", paste(x, time_var, sep = "_"))
            try(table(datum[DM == dm_var, ..col_var]) %>% 
                    xtable() %>% 
                    print.xtable(type = "html", include.rownames = FALSE))
        }
    }
}
sink()


# Draw graphs -------------------------------------------------------------
p_value_table <- data.table()

for (dm_var in c("All", "Type 1", "Type 2")) {
    if (dm_var != "All") {
        p_data <- datum %>% 
            filter(DM == dm_var)
    } else {
        p_data <- datum
    }
    
    for (group_var in c("DM", "sex", "age_group")) {
        if (dm_var == "All" & group_var != "DM") {
            next()
        } else if (dm_var != "All" & group_var == "DM") {
            next()
        }
        
        for (measurement_var in c("zbmi", "HBA1C")) {
            if (measurement_var == "zbmi") {
                hline_y  <- 1.0364
                annotate_y <- 0.82
                annotate_label <- "Overweight(1.0364)"
                ylim <- c(-3, 3)
                ybreaks <- seq(-3, 3, 1)
                y_lab <- "BMI z score"
            } else {
                hline_y <- 7
                annotate_y <- 6.3
                annotate_label <- "Abnormal(7.0)"
                ylim <- c(4, 14)
                ybreaks <- seq(4, 14, 2)
                y_lab <- "HbA1c(%)"
            }
            
            for (time_var in time_list) {
                if (group_var == "DM") {
                    p_value_table <- bind_rows(p_value_table,
                                               data.frame(dm = dm_var,
                                                          group = group_var,
                                                          measurement = measurement_var,
                                                          time = time_var,
                                                          p_value = t.test(datum[DM == "Type 1", get(paste(measurement_var, time_var, sep = "_"))],
                                                                           datum[DM == "Type 2", get(paste(measurement_var, time_var, sep = "_"))])$p.value))
                } else if (group_var == "sex") {
                    p_value_table <- bind_rows(p_value_table,
                                               data.frame(dm = dm_var,
                                                          group = group_var,
                                                          measurement = measurement_var,
                                                          time = time_var,
                                                          p_value = t.test(datum[sex == "Male", get(paste(measurement_var, time_var, sep = "_"))],
                                                                           datum[sex == "Female", get(paste(measurement_var, time_var, sep = "_"))])$p.value))
                } else if (dm_var == "Type 2" & group_var == "age_group") {
                    p_value_table <- bind_rows(p_value_table,
                                               data.frame(dm = dm_var,
                                                          group = group_var,
                                                          measurement = measurement_var,
                                                          time = time_var,
                                                          p_value = t.test(datum[age_group == "5-9Y", get(paste(measurement_var, time_var, sep = "_"))],
                                                                           datum[age_group == "10-14Y", get(paste(measurement_var, time_var, sep = "_"))])$p.value))
                } else {
                    p_value_table <- bind_rows(p_value_table,
                                               data.frame(dm = dm_var,
                                                          group = group_var,
                                                          measurement = measurement_var,
                                                          time = time_var,
                                                          p_value = summary(aov(data = datum,
                                                                                as.formula(paste(paste(measurement_var, time_var, sep = "_"), group_var, sep = "~"))))[[1]][["Pr(>F)"]][[1]]))
                }
            }
        }
        
        p1_data <- p_data %>% 
            group_by_at(group_var) %>% 
            select(paste(c("zbmi", "HBA1C"), rep(time_list, each = 2), sep = "_"), !!group_var) %>%
            summarise_all(list(mean = function(x) {mean(x, na.rm = TRUE)},
                               sd = function(x) {sd(x, na.rm = TRUE)},
                               median = function(x) {median(x, na.rm = TRUE)},
                               n = function(x) {sum(!is.na(x))})) %>% 
            gather("attribute", "value", 2:ncol(.)) %>% 
            mutate(time = str_extract(attribute, "\\d{2}"),
                   measurement = str_extract(attribute, "^[a-zA-Z0-9]+"),
                   type = str_extract(attribute, "[a-zA-Z]+$")) %>% 
            select(-attribute)
        write_csv(p1_data, file.path(desc_stat_path, paste(dm_var, group_var, "Desc_Stat.csv", sep = "_")))
    }
}
write_csv(p_value_table, file.path(output_path, "4_P_Value.csv"))


# normality test ----------------------------------------------------------
norm_test <- data.table()
norm_test_path <- file.path(plot_path, "norm_test")
path_assistant(norm_test_path, silence = TRUE)
for (measurement_var in c("zbmi", "HBA1C")) {
    for (time_var in time_list) {
        column_var <- paste(measurement_var, time_var, sep = "_")
        datum %>% 
            ggplot(aes_string(sample = column_var)) +
            geom_qq() +
            geom_qq_line()
        suppressMessages(ggsave(file.path(norm_test_path, paste0(column_var, ".png"))))
        
        norm_test <- bind_rows(norm_test, data.table(measurement = column_var, 
                                                     p_value = shapiro.test(datum[[column_var]])$p.value))
    }
}
sink(file.path(norm_test_path, "Normality_Test.html"))
norm_test %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
sink()


# Draw density plot -------------------------------------------------------
box_plot_path <- file.path(plot_path, "box_plot")
path_assistant(box_plot_path, silence = TRUE)

for (dm_var in c("All", "Type 1", "Type 2")) {
    if (dm_var != "All") {
        p_data <- datum %>% 
            filter(DM == dm_var)
    } else {
        p_data <- datum
    }
    
    for (measurement_var in c("zbmi", "HBA1C")) {
        p1_data <- p_data %>% 
            select(matches(paste0(measurement_var, "_\\d{2}$"))) %>% 
            gather(attribute, value) %>% 
            mutate(time = str_extract(attribute, "\\d{2}"),
                   measurement = str_extract(attribute, "^[a-zA-Z0-9]+")) %>% 
            select(-attribute)
        
        if (measurement_var == "zbmi") {
            hline_y <- 1.0364
            text_y <- 3
        } else {
            hline_y <- 7
            text_y <- 14
        }
        
        plt <- p1_data %>% 
            ggplot(aes(x = time, y = value)) +
            geom_boxplot() +
            geom_text(data = p1_data %>% 
                          group_by(time) %>% 
                          summarise(n = sum(!is.na(value))),
                      aes(x = time, y = text_y, label = n)) +
            geom_hline(yintercept = hline_y, linetype = "dashed")
        
        suppressMessages(ggsave(file.path(box_plot_path, paste(dm_var, "DM", measurement_var, "Box_Plot.png", sep = "_")), plt))
    }
}


# Make supplement ---------------------------------------------------------
supplement <- datum %>% 
    select(paste(c("zbmi", "HBA1C"), rep(time_list, each = 2), sep = "_")) %>%
    summarise_all(list(mean = function(x) {mean(x, na.rm = TRUE)},
                       sd = function(x) {sd(x, na.rm = TRUE)},
                       median = function(x) {median(x, na.rm = TRUE)},
                       n = function(x) {sum(!is.na(x))})) %>% 
    gather("attribute", "value") %>% 
    mutate(time = str_extract(attribute, "\\d{2}"),
           measurement = str_extract(attribute, "^[a-zA-Z0-9]+"),
           type = str_extract(attribute, "[a-zA-Z]+$")) %>% 
    select(-attribute)
write_csv(supplement, file.path(desc_stat_path, "Total_Measurement_Desc_Stat.csv"))


# Linear regression -----------------------------------------------------
lm_var_list <- list(indep = data.table(name = c("HBA1C_03", "DKA", "obesity_03", "sex", "age_03"),
                                       type = c("continuous", "discrete", "discrete", "discrete", "continuous")),
                    dep = paste(rep(c("zbmi", "HBA1C", "zbmi_delta", "hba1c_delta"), each = 2), rep(c("36", "60"), times = 4), sep = "_"))

sink(file.path(output_path, "2_Linear_Regression.html"))
for (dm_var in c("All", "Type 1", "Type 2")) {
    if (dm_var != "All") {
        p_data <- datum %>% 
            filter(DM == dm_var)
    } else {
        p_data <- datum
    }
    
    for (dep_var in lm_var_list$dep) {
        for (confound_var in c(FALSE, TRUE)) {
            if (confound_var == FALSE) {
                p_indep <- lm_var_list$indep %>% 
                    filter(!name %in% c("sex", "age_03")) %>% 
                    as.data.table()
                
            } else {
                p_indep <- lm_var_list$indep
            }
            
            f <- paste(dep_var,
                       paste(p_indep$name, collapse = "+"),
                       sep = "~")
            cat(paste(dm_var, "DM; Multivariate Linear Regression: ", f))
            
            tryCatch({multi_lm <- lm(data = p_data, as.formula(f))
            print.xtable(xtable(multi_lm), type = "html")
            cat("<br>Number of observation used: ", nobs(multi_lm), rep("<br>", 2))},
            error = function(e) {cat("Error occurred<br><br>")})
            
            for (uni_var in p_indep$name) {
                if (p_indep[name == uni_var, type] == "continuous") {
                    f <- paste(dep_var, uni_var, sep = "~")
                    
                    cat(paste("Unvariate Linear Regression: ", f))
                    
                    tryCatch({uni_lm <- lm(data = p_data, as.formula(f))
                    print.xtable(xtable(uni_lm), type = "html")},
                    error = function(e) {cat("Error occurred")})
                    cat(rep("<br>", 2))
                    
                    cat(paste(uni_var, "Descriptive Statistics:"))
                    tryCatch({multi_lm$model %>% 
                        select(!!uni_var) %>% 
                        summarise_all(list(n = function(x) {sum(!is.na(x))},
                                           min = function(x) {min(x, na.rm = TRUE)},
                                           q1 = function(x) {quantile(x, 0.25, na.rm = TRUE)},
                                           mean = function(x) {mean(x, na.rm = TRUE)},
                                           median = function(x) {median(x, na.rm = TRUE)},
                                           sd = function(x) {sd(x, na.rm = TRUE)},
                                           q3 = function(x) {quantile(x, 0.75, na.rm = TRUE)},
                                           max = function(x) {max(x, na.rm = TRUE)})) %>% 
                        xtable() %>% 
                        print.xtable(type = "html", include.rownames = FALSE)},
                        error = function(e) {cat("Error occurred")})
                    cat(rep("<br>", 2))
                    
                } else {
                    cat(paste(uni_var, "Descriptive Statistics:"))
                    tryCatch({multi_lm$model %>% 
                        group_by_at(uni_var) %>% 
                        summarise(n = n()) %>% 
                        xtable() %>% 
                        print.xtable(type = "html", include.rownames = FALSE)},
                        error = function(e) {cat("Error occurred")})
                    cat(rep("<br>", 2))
                }
            }
        }
    }
}
sink()


# Baseline characteristics ------------------------------------------------
sink(file.path(output_path, "1_Baseline_Characteristics.html"))
cat("The number of patients by DM type")
datum %>% 
    group_by(DM) %>% 
    summarise(n = n()) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

cat("The number of patients by DM type; sex")
datum %>% 
    group_by(DM, sex) %>% 
    summarise(n = n()) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

cat("The mean of patients by DM type")
datum %>% 
    group_by(DM) %>% 
    summarise(mean = mean(age_00 / 12, na.rm = TRUE),
              sd = sd(age_00 / 12, na.rm = TRUE)) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

print_t_test(title = "Age",
             x = datum[DM == "Type 1", age_00 / 12],
             y = datum[DM == "Type 2", age_00 / 12])

cat("The number of patients by DM type; age group")
datum %>% 
    group_by(DM, age_group) %>% 
    summarise(n = n()) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

cat("The descriptive statistics of BMI z-score by DM type")
datum %>% 
    group_by(DM) %>% 
    summarise(min = min(zbmi_00, na.rm = TRUE),
              q1 = quantile(zbmi_00, 0.25, na.rm = TRUE),
              mean = mean(zbmi_00, na.rm = TRUE),
              sd = sd(zbmi_00, na.rm = TRUE),
              median = median(zbmi_00, na.rm = TRUE),
              q3 = quantile(zbmi_00, 0.75, na.rm = TRUE),
              max = max(zbmi_00, na.rm = TRUE)) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

print_t_test(title = "BMI z-score",
             x = datum[DM == "Type 1", zbmi_00],
             y = datum[DM == "Type 2", zbmi_00])

cat("The number of patients by DM type; DKA")
datum %>% 
    group_by(DM, DKA) %>% 
    summarise(n = n()) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

cat("The descriptive statistics of HbA1c by DM type")
datum %>% 
    group_by(DM) %>% 
    summarise(min = min(HBA1C_00, na.rm = TRUE),
              q1 = quantile(HBA1C_00, 0.25, na.rm = TRUE),
              mean = mean(HBA1C_00, na.rm = TRUE),
              sd = sd(HBA1C_00, na.rm = TRUE),
              median = median(HBA1C_00, na.rm = TRUE),
              q3 = quantile(HBA1C_00, 0.75, na.rm = TRUE),
              max = max(HBA1C_00, na.rm = TRUE)) %>% 
    xtable() %>% 
    print.xtable(type = "html", include.rownames = FALSE)
cat(rep("<br>", 2))

print_t_test(title = "HbA1c",
             x = datum[DM == "Type 1", HBA1C_00],
             y = datum[DM == "Type 2", HBA1C_00])
sink()