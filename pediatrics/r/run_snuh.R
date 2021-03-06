# Set the analysis environment --------------------------------------------
setwd("E:/Users/DLCG001/workspace/pediatrics") 


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

path_assistant(data_path, silence = TRUE)
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
    
    # Add HbA1c abnormal status and remove the outlier
    hba1c_index <- paste("HBA1C", time_var, sep = "_")
    datum[datum[, get(paste("HBA1C", time_var, sep = "_"))] > 30, paste("HBA1C", time_var, sep = "_")] <- NA
    datum[, paste("hba1c_abnormal", time_var, sep = "_")] <-
        cut(datum[, get(paste("HBA1C", time_var, sep = "_"))],
            breaks = c(-Inf, 7.5, Inf),
            labels = c("FALSE", "TRUE"),
            right = FALSE)
    
    # Calculate BMI z-score and obesity status
    con_vec <- c()
    dis_vec <- c()
    for (i in 1:nrow(datum)) {
        sex_var <- datum[i, sex]
        age_var <- datum[i, age_00]
        l <- zbmi[sex == sex_var & age_00 == age_var, L]
        m <- zbmi[sex == sex_var & age_00 == age_var, M]
        s <- zbmi[sex == sex_var & age_00 == age_var, S]
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
write.csv(datum, file.path(data_path, "p_data.csv"))


# Get abnormal ratio ------------------------------------------------------
sink(file.path(output_path, "3_Abnormal_Ratio_SNUH.html"))
for (x in c("obesity", "hba1c_abnormal")) {
    for (time_var in time_list) {
        cat(paste("The ratio of", x ,"by DM type; time"))
        col_var <- c("DM", paste(x, time_var, sep = "_"))
        try(table(datum[, ..col_var]) %>% 
                xtable() %>% 
                print.xtable(type = "html", include.rownames = FALSE))
        for (dm_var in c("Type 1", "Type 2")) {
            cat(paste("The ratio of", x ,"by sex", time_var, "time for", dm_var, "DM patients"))
            col_var <- c("sex", paste(x, time_var, sep = "_"))
            try(table(datum[DM == dm_var, ..col_var]) %>% 
                    xtable() %>% 
                    print.xtable(type = "html", include.rownames = FALSE))
            
            cat(paste("The ratio of", x, "by age group", time_var, "time for", dm_var, "DM patients"))
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
            
            p_value_vec <- c()
            
            for (time_var in time_list) {
                if (group_var == "DM") {
                    p_value_vec <- c(p_value_vec, t.test(datum[DM == "Type 1", get(paste(measurement_var, time_var, sep = "_"))],
                                                         datum[DM == "Type 2", get(paste(measurement_var, time_var, sep = "_"))])$p.value)
                } else {
                    p_value_vec <- c(p_value_vec,
                                     summary(aov(data = datum, as.formula(paste(paste(measurement_var, time_var, sep = "_"), group_var, sep = "~"))))[[1]][["Pr(>F)"]][[1]])
                }
            }
            
            p_value_table <- rbind(p_value_table,
                                   data.table(dm = dm_var,
                                              group = group_var,
                                              measurement = measurement_var,
                                              time = time_list,
                                              p_value = p_value_vec))
            
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
            write_csv(p1_data, file.path(desc_stat_path, paste(dm_var, "DM", group_var, "Group_Measurement_Desc_Stat.csv", sep = "_")))
            
            plt <- p1_data %>% 
                filter(measurement == measurement_var & type == "mean") %>% 
                ggplot(aes_string(x = "time", y = "value", group = group_var)) +
                geom_point(aes_string(shape = group_var), size = 3) +
                geom_line(size = 0.7) +
                geom_hline(yintercept = c(0, hline_y), linetype = "dashed") +
                annotate("text", x = 7.5, y = annotate_y, label = annotate_label) +
                coord_cartesian(ylim = ylim) +
                scale_x_discrete(labels = c("03" = expression(bold(underline("03"))),
                                            "36" = expression(bold(underline("36"))),
                                            "60" = expression(bold(underline("60"))),
                                            parse = TRUE)) +
                scale_y_continuous(breaks = ybreaks) +
                labs(x = "Time(month)", y = y_lab) +
                theme_classic() +
                theme(legend.position = "top",
                      legend.title = element_blank(),
                      axis.title = element_text(size = 15),
                      axis.text = element_text(size = 15))
            
            if (p_value_table[dm == dm_var &
                              group == group_var &
                              measurement == measurement_var &
                              time == "03", p_value] < 0.05 / 3) {
                plt <- plt +
                    annotate("text", x = "03", y = ylim[2] * 0.8, label = "*")
            }
            if (p_value_table[dm == dm_var &
                                     group == group_var &
                                     measurement == measurement_var &
                                     time == "36", p_value] < 0.05 / 3) {
                plt <- plt + 
                    annotate("text", x = "36", y = ylim[2] * 0.8, label = "*")
            }
            if (p_value_table[dm == dm_var &
                                     group == group_var &
                                     measurement == measurement_var &
                                     time == "60", p_value] < 0.05 / 3) {
                plt <- plt +
                    annotate("text", x = "60", y = ylim[2] * 0.8, label = "*")
            }
            
            df <- p1_data %>% 
                filter(measurement == measurement_var & type == "n") %>% 
                select(!!group_var, time, value) %>% 
                spread(key = time, value = value) %>% 
                tableGrob(rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "white", col = "black")),
                                                              colhead = list(bg_params = list(col = "black"))))
            
            g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 1))
            suppressMessages(ggsave(file.path(plot_path,
                                              paste(dm_var, "DM", group_var, "Group", measurement_var, "Plot.tiff", sep = "_")), g))
        }
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
