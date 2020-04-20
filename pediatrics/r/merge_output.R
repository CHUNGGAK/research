setwd("E:/Users/DLCG001/workspace/pediatrics/output")

library(data.table)
library(tidyverse)
library(gridExtra)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")


# Make directory ----------------------------------------------------------
output_path <- "merged_output"
desc_stat_path <- file.path(output_path, "desc_stat")
plot_path <- file.path(output_path, "plot")

path_assistant(desc_stat_path, silence = TRUE)
path_assistant(plot_path, silence = TRUE)

file_table <- data.table(file = c("All_DM_DM_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_sex_Group_Measurement_Desc_Stat.csv",
                                  "Type 2_DM_age_group_Group_Measurement_Desc_Stat.csv",
                                  "Type 2_DM_sex_Group_Measurement_Desc_Stat.csv"),
                         group_key = c("DM", rep(c("age_group", "sex"), 2)))

p_value_table <- data.table()

for (file_var in file_table$file) {
    group_key <- file_table[[match(file_var, file_table$file), "group_key"]]
    
    for (hospital_var in c("snubh", "snuh", "amc")) {
        if (hospital_var == "snubh") {
            d <- fread(file.path(hospital_var, "desc_stat", file_var)) %>% 
                mutate(hospital = hospital_var) %>% 
                spread(type, value)
            
            # Make plot per hospital
            for (measurement_var in c("zbmi", "HBA1C")) {
                make_pediatrics_plot(d = d %>% 
                                         filter(measurement == measurement_var) %>% 
                                         mutate(time = sprintf("%02d", time)) %>% 
                                         as.data.table(),
                                     p_value_table = fread(file.path(hospital_var, "4_P_Value.csv")) %>% 
                                         mutate(time = sprintf("%02d" ,time)) %>% 
                                         as.data.table(),
                                     measurement_var = measurement_var,
                                     group_key = group_key,
                                     file_name = file.path(plot_path, 
                                                           paste(hospital_var,
                                                                 measurement_var,
                                                                 str_extract(file_var, "\\w+_Group"),
                                                                 "plot",
                                                                 sep = "_")),
                                     merged = FALSE)
            }
        } else {
            d_part <- fread(file.path(hospital_var, "desc_stat", file_var)) %>% 
                mutate(hospital = hospital_var) %>% 
                spread(type, value)
            
            # Make plot per hospital
            for (measurement_var in c("zbmi", "HBA1C")) {
                make_pediatrics_plot(d = d_part %>% 
                                         filter(measurement == measurement_var) %>% 
                                         mutate(time = sprintf("%02d", time)) %>% 
                                         as.data.table(),
                                     p_value_table = fread(file.path(hospital_var, "4_P_Value.csv")) %>% 
                                         mutate(time = sprintf("%02d" ,time)) %>% 
                                         as.data.table(),
                                     measurement_var = measurement_var,
                                     group_key = group_key,
                                     file_name = file.path(plot_path, 
                                                           paste(hospital_var,
                                                                 measurement_var,
                                                                 str_extract(file_var, "\\w+_Group"),
                                                                 "plot",
                                                                 sep = "_")),
                                     merged = FALSE)
            }
            
            d <- bind_rows(d,d_part)
        }
    }
    d <- d %>% 
        mutate(time = ifelse(nchar(time) == 1, sprintf("%02d", time), as.character(time)))
    
    rnorm_table <- data.table()
    
    for (i in 1:nrow(d)) {
        rnorm_table <- bind_rows(rnorm_table,
                                 data.table(value = rnorm(n = d[[i, "n"]], mean = d[[i, "mean"]], sd = d[[i, "sd"]]),
                                            placeholder = d[[i, group_key]],
                                            time = d[[i, "time"]],
                                            measurement = d[[i, "measurement"]],
                                            hospital = d[[i, "hospital"]]))
    }
    
    rnorm_table <- rnorm_table %>% 
        rename(!!group_key := placeholder)
    
    group_key_vec <- c(group_key, "time", "measurement")
    
    d1 <- d %>% 
        group_by_at(group_key_vec) %>% 
        summarise(weighted_mean = weighted.mean(mean, n),
                  n = sum(n))
    
    write_csv(d1, file.path(desc_stat_path, paste(str_extract(file_var, "\\w+Group"), "_MergedMeasurementDescStat.csv", sep = "_")))
    
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
        
        time_vec <- c("00", "03", "06", "12", "24", "36", "48", "60")
        
        # Compute p-value ---------------------------------------------------------
        for (time_var in time_vec) {
            if (group_key == "DM") {
                p_value_vec <- c(p_value_vec, t.test(rnorm_table[DM == "Type 1" &
                                                                     time == time_var &
                                                                     measurement == measurement_var, value],
                                                     rnorm_table[DM == "Type 2" &
                                                                     time == time_var &
                                                                     measurement == measurement_var, value])$p.value)
            } else {
                p_value_vec <- c(p_value_vec,
                                 summary(aov(data = rnorm_table[time == time_var &
                                                                    measurement == measurement_var, ],
                                             as.formula(paste("value", group_key, sep = "~"))))[[1]][["Pr(>F)"]][[1]])
            }
        }
        
        p_value_table <- bind_rows(p_value_table,
                                   data.table(file = file_var,
                                              measurement = measurement_var,
                                              time = time_vec,
                                              p_value = p_value_vec))
        
        d2 <- d1 %>% 
            filter(measurement == measurement_var)
        
        plt <- d2 %>% 
            ggplot(aes_string(x = "time", y = "weighted_mean", group = group_key)) +
            geom_point(aes_string(shape = group_key), size = 3) +
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
        
        if (p_value_table[file == file_var &
                          measurement == measurement_var &
                          time == "03", p_value] < 0.05 / 3) {
            plt <- plt +
                annotate("text", x = "03", y = ylim[2] * 0.8, label = "*", size = 7)
        }
        if (p_value_table[file == file_var &
                          measurement == measurement_var &
                          time == "36", p_value] < 0.05 / 3) {
            plt <- plt + 
                annotate("text", x = "36", y = ylim[2] * 0.8, label = "*", size = 7)
        }
        if (p_value_table[file == file_var &
                          measurement == measurement_var &
                          time == "60", p_value] < 0.05 / 3) {
            plt <- plt +
                annotate("text", x = "60", y = ylim[2] * 0.8, label = "*", size = 7)
        }
        
        df <- d2 %>% 
            select(-c(measurement, weighted_mean)) %>% 
            spread(key = time, value = n) %>% 
            tableGrob(rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "white", col = "black")),
                                                          colhead = list(bg_params = list(col = "black"))))
        
        g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 1))
        ggsave(file.path(plot_path,
                         paste(str_extract(file_var, "\\w+Group"), measurement_var, "Plot.tiff", sep = "_")), g)
        ggsave(file.path(plot_path,
                         paste(str_extract(file_var, "\\w+Group"), measurement_var, "Plot.png", sep = "_")), g)
    }
    write_csv(p_value_table, file.path(output_path, "4_P_Value.csv"))
}


# Function make_pediatrics_plot -------------------------------------------
make_pediatrics_plot <- function(d, p_value_table, measurement_var, group_key,
                                 merged, file_name) {
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
    
    if (merged == TRUE) {
        y <- "weighted_mean"
    } else {
        y <- "mean"
    }
    
    plt <- d %>% 
        ggplot(aes_string(x = "time", y = y, group = group_key)) +
        geom_point(aes_string(shape = group_key), size = 3) +
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
    
    if (p_value_table[measurement == measurement_var &
                      group == group_key &
                      time == "03", p_value][[1]] < 0.05 / 3) {
        plt <- plt +
            annotate("text", x = "03", y = ylim[2] * 0.8, label = "*", size = 7)
    }
    if (p_value_table[measurement == measurement_var &
                      group == group_key &
                      time == "36", p_value][[1]] < 0.05 / 3) {
        plt <- plt + 
            annotate("text", x = "36", y = ylim[2] * 0.8, label = "*", size = 7)
    }
    if (p_value_table[measurement == measurement_var &
                      group == group_key &
                      time == "60", p_value][[1]] < 0.05 / 3) {
        plt <- plt +
            annotate("text", x = "60", y = ylim[2] * 0.8, label = "*", size = 7)
    }
    
    df <- d %>% 
        select(!!group_key, time, n) %>% 
        spread(key = time, value = n) %>% 
        tableGrob(rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "white", col = "black")),
                                                      colhead = list(bg_params = list(col = "black"))))
    
    g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 1))
    ggsave(paste(file_name, "tiff", sep = "."), g)
    ggsave(paste(file_name, "png", sep = "."), g)
}
