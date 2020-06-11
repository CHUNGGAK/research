setwd("E:/Users/DLCG001/workspace/pediatrics/output")

library(data.table)
library(tidyverse)
library(gridExtra)

output_path <- "plots"
dir.create(output_path)

vTime <- c("00", "03", "06", "12", "24", "36", "48", "60")

# Function create_pediatrics_plot -------------------------------------------
create_pediatrics_plot <- function(data, p_value, var_measurement, group_key,
                                   file_name, var_file) {
    if (var_measurement == "zbmi") {
        hline_y  <- 1.0364
        annotate_x <- 3.4
        annotate_y <- 0.82
        annotate_label <- "z=1.0364(85th percentile)"
        ylim <- c(-1, 3.5)
        ybreaks <- seq(-1, 3, 1)
        y_lab <- "BMI z score"
    } else {
        hline_y <- 7
        annotate_x <- 6.4
        annotate_y <- 6.55
        annotate_label <- "Target(7.0%)"
        ylim <- c(4, 12)
        ybreaks <- seq(4, 12, 2)
        y_lab <- "HbA1c(%)"
    }
    
    plt <- data %>% 
        ggplot(aes_string(x = "time", y = "mean", group = group_key)) +
        geom_point(aes_string(shape = group_key), size = 3) +
        geom_line(size = 0.7) +
        geom_hline(yintercept = c(0, hline_y), linetype = "dashed") +
        annotate("text", x = annotate_x, y = annotate_y, label = annotate_label) +
        coord_cartesian(ylim = ylim) +
        scale_y_continuous(breaks = ybreaks) +
        labs(x = element_blank(), y = y_lab) +
        theme_classic() +
        theme(legend.position = "top",
              legend.title = element_blank(),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15),
              plot.margin = unit(c(10, 10, -2, 10), "mm"))
    
    for (var_time in vTime) {
        if (p_value[DM == str_sub(var_file, 1, 6) &
                    measurement == var_measurement &
                    group == group_key &
                    time == var_time, p_value] < 0.05 / 8 &
            !is.na(p_value[DM == str_sub(var_file, 1, 6) &
                           measurement == var_measurement &
                           group == group_key &
                           time == var_time, p_value])) {
            plt <- plt +
                annotate("text", x = var_time, y = max(data[time == var_time, mean]) + 1, label = "*", size = 7)
        }
    }
    
    if (group_key == "DM") {
        data_reorder <- data %>% 
            mutate(DM = ifelse(DM == "Type 1", "Type 1 (n)", "Type 2 (n)"),
                   DM = factor(DM, levels = c("Type 2 (n)", "Type 1 (n)")))
    } else if (group_key == "age_group") {
        data_reorder <- data %>% 
            mutate(age_group = ifelse(age_group == "0-4Y", "0-4Y (n)",
                                      ifelse(age_group == "5-9Y", "5-9Y (n)", "10-14Y (n)")),
                   age_group = factor(age_group, levels = c("10-14Y (n)", "5-9Y (n)", "0-4Y (n)")))
    } else {
        data_reorder <- data %>% 
            mutate(sex = ifelse(sex == "Male", "Male (n)", "Female (n)"),
                   sex = factor(sex, levels = c("Female (n)", "Male (n)")))
    }
    
    df <-  ggplot(data_reorder, aes_string(x = "time", y = group_key, label = "n")) +
        geom_text() +
        theme_test() +
        geom_hline(yintercept = 1.5) +
        geom_vline(xintercept = seq(1.5, 7.5, 1)) +
        labs(x = "Time(month)", y = element_blank()) +
        theme(plot.margin = unit(c(0, 10, 10, 5), "mm"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_text(size = 15))
    
    if (str_sub(var_file, 1, 19) == "Type 1_DM_age_group") {
        df <- df +
            geom_hline(yintercept = 2.5)
    }
    
    g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 0.75))
    ggsave(paste(file_name, "tiff", sep = "."), g, dpi = 320, width = 150, height = 150, units = "mm")
    ggsave(paste(file_name, "png", sep = "."), g, dpi = 320, width = 150, height = 150, units = "mm")
}


# Create Plots by Hospital ------------------------------------------------
dFile1 <- data.table(file = c("All_DM_DM_Group_Measurement_Desc_Stat.csv",
                              "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv",
                              "Type 1_DM_sex_Group_Measurement_Desc_Stat.csv",
                              "Type 2_DM_age_group_Group_Measurement_Desc_Stat.csv",
                              "Type 2_DM_sex_Group_Measurement_Desc_Stat.csv"),
                     group_key = c("DM", rep(c("age_group", "sex"), 2)))

for (var_file in dFile1$file) {
    group_key <- dFile1[[match(var_file, dFile1$file), "group_key"]]
    
    for (hospital_var in c("snubh", "snuh", "amc")) {
        d <- fread(file.path(hospital_var, "desc_stat", var_file)) %>% 
            mutate(hospital = hospital_var) %>% 
            spread(type, value)
        
        if (group_key == "DM") {
            d <- d %>% 
                mutate(DM = factor(DM, levels = c("Type 1", "Type 2")))
        } else if (group_key == "age_group") {
            d <- d %>% 
                mutate(age_group = factor(age_group, levels = c("0-4Y", "5-9Y", "10-14Y")))
        } else {
            d <- d %>% 
                mutate(sex = factor(sex, levels = c("Male", "Female")))
        }
        
        for (var_measurement in c("zbmi", "HBA1C")) {
            create_pediatrics_plot(data = d %>%
                                       filter(measurement == var_measurement) %>%
                                       mutate(time = sprintf("%02d", time)) %>%
                                       as.data.table(),
                                   p_value = fread(file.path(hospital_var, "4_P_Value.csv")) %>%
                                       mutate(time = sprintf("%02d" ,time)) %>%
                                       as.data.table(),
                                   var_measurement = var_measurement,
                                   var_file = var_file,
                                   group_key = group_key,
                                   file_name = file.path(output_path,
                                                         paste(hospital_var,
                                                               var_measurement,
                                                               str_extract(var_file, "\\w+_Group"),
                                                               "plot",
                                                               sep = "_")))
        }
    } 
}


# Create Total Plots ------------------------------------------------------
dFile2 <- data.table(file = c("All_DM_DM_Group.csv",
                              "Type 1_DM_age_group_Group.csv",
                              "Type 1_DM_sex_Group.csv",
                              "Type 2_DM_age_group_Group.csv",
                              "Type 2_DM_sex_Group.csv"),
                     group_key = c("DM", rep(c("age_group", "sex"), 2)))

p_value_2 <- fread("4_P_Value.csv") %>%
    mutate(time = sprintf("%02d" ,time)) %>%
    as.data.table()

for (var_file in dFile2$file) {
    group_key <- dFile2[[match(var_file, dFile2$file), "group_key"]]
    
    d <- fread(file.path("merged_output_v3/desc_stat", var_file)) %>% 
        rename(mean = weighted_mean)
    
    if (group_key == "DM") {
        d <- d %>% 
            mutate(DM = factor(DM, levels = c("Type 1", "Type 2")))
    } else if (group_key == "age_group") {
        d <- d %>% 
            mutate(age_group = factor(age_group, levels = c("0-4Y", "5-9Y", "10-14Y")))
    } else {
        d <- d %>% 
            mutate(sex = factor(sex, levels = c("Male", "Female")))
    }
    
    for (var_measurement in c("zbmi", "HBA1C")) {
        create_pediatrics_plot(data = d %>%
                                   filter(measurement == var_measurement) %>%
                                   mutate(time = sprintf("%02d", time)) %>%
                                   as.data.table(),
                               p_value = p_value_2,
                               var_measurement = var_measurement,
                               var_file = var_file,
                               group_key = group_key,
                               file_name = file.path(output_path,
                                                     paste("Total",
                                                           var_measurement,
                                                           str_extract(var_file, "\\w+_Group"),
                                                           "plot",
                                                           sep = "_")))
    }
}
