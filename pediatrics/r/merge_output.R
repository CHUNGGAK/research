setwd("E:/Users/DLCG001/workspace/pediatrics/output")
set.seed(20200427)

library(data.table)
library(tidyverse)
library(gridExtra)


# Make directory ----------------------------------------------------------
output_path <- "merged_output_v3"
desc_stat_path <- file.path(output_path, "desc_stat")
plot_path <- file.path(output_path, "plot")

dir.create(desc_stat_path)
dir.create(plot_path)

file_table <- data.table(file = c("All_DM_DM_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv",
                                  "Type 1_DM_sex_Group_Measurement_Desc_Stat.csv",
                                  "Type 2_DM_age_group_Group_Measurement_Desc_Stat.csv",
                                  "Type 2_DM_sex_Group_Measurement_Desc_Stat.csv"),
                         group_key = c("DM", rep(c("age_group", "sex"), 2)))

time_vec <- c("00", "03", "06", "12", "24", "36", "48", "60")

# Function make_pediatrics_plot -------------------------------------------
make_pediatrics_plot <- function(d, p_value_table, measurement_var, group_key,
                                 merged, file_name, file_var) {
    if (measurement_var == "zbmi") {
        hline_y  <- 1.0364
        annotate_y <- 0.82
        annotate_label <- "z=1.0364(85th percentile)"
        ylim <- c(-1, 3.5)
        ybreaks <- seq(-1, 3, 1)
        y_lab <- "BMI z score"
    } else {
        hline_y <- 7
        annotate_y <- 6.55
        annotate_label <- "Target(7.0%)"
        ylim <- c(4, 12)
        ybreaks <- seq(4, 12, 2)
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
        annotate("text", x = 6.4, y = annotate_y, label = annotate_label) +
        coord_cartesian(ylim = ylim) +
        # scale_x_discrete(labels = c("03" = expression(bold(underline("03"))),
        #                             "36" = expression(bold(underline("36"))),
        #                             "60" = expression(bold(underline("60"))),
        #                             parse = TRUE)) +
        scale_y_continuous(breaks = ybreaks) +
        labs(x = element_blank(), y = y_lab) +
        # labs(x = "Time(month)", y = y_lab) +
        theme_classic() +
        theme(legend.position = "top",
              legend.title = element_blank(),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15),
              plot.margin = unit(c(10, 10, -2, 10), "mm"))
    
    if (p_value_table[DM == str_sub(file_var, 1, 6) &
                      measurement == measurement_var &
                      group == group_key &
                      time == "03", p_value] < 0.05 / 3 &
        !is.na(p_value_table[DM == str_sub(file_var, 1, 6) &
                             measurement == measurement_var &
                             group == group_key &
                             time == "03", p_value])) {
        plt <- plt +
            annotate("text", x = "03", y = max(d[time == "03", mean]) + 1, label = "*", size = 7)
    }
    if (p_value_table[DM == str_sub(file_var, 1, 6) &
                      measurement == measurement_var &
                      group == group_key &
                      time == "36", p_value] < 0.05 / 3 &
        !is.na(p_value_table[DM == str_sub(file_var, 1, 6) &
                             measurement == measurement_var &
                             group == group_key &
                             time == "36", p_value])) {
        plt <- plt + 
            annotate("text", x = "36", y = max(d[time == "36", mean]) + 1, label = "*", size = 7)
    }
    if (p_value_table[DM == str_sub(file_var, 1, 6) &
                      measurement == measurement_var &
                      group == group_key &
                      time == "60", p_value] < 0.05 / 3 &
        !is.na(p_value_table[DM == str_sub(file_var, 1, 6) &
                             measurement == measurement_var &
                             group == group_key &
                             time == "60", p_value])) {
        plt <- plt +
            annotate("text", x = "60", y = max(d[time == "60", mean]) + 1, label = "*", size = 7)
    }
    
    if (group_key == "DM") {
        d_reorder <- d %>% 
            mutate(DM = ifelse(DM == "Type 1", "Type 1 (n)", "Type 2 (n)"),
                   DM = factor(DM, levels = c("Type 2 (n)", "Type 1 (n)")))
    } else if (group_key == "age_group") {
        d_reorder <- d %>% 
            mutate(age_group = ifelse(age_group == "0-4Y", "0-4Y (n)",
                                      ifelse(age_group == "5-9Y", "5-9Y (n)", "10-14Y (n)")),
                   age_group = factor(age_group, levels = c("10-14Y (n)", "5-9Y (n)", "0-4Y (n)")))
    } else {
        d_reorder <- d %>% 
            mutate(sex = ifelse(sex == "Male", "Male (n)", "Female (n)"),
                   sex = factor(sex, levels = c("Female (n)", "Male (n)")))
    }
    
    df <-  ggplot(d_reorder, aes_string(x = "time", y = group_key, label = "n")) +
        geom_text() +
        theme_test() +
        geom_hline(yintercept = 1.5) +
        geom_vline(xintercept = seq(1.5, 7.5, 1)) +
        labs(x = "Time(month)", y = element_blank()) +
        theme(plot.margin = unit(c(0, 10, 10, 5), "mm"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_text(size = 15))
    
    if (file_var == "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv") {
        df <- df +
            geom_hline(yintercept = 2.5)
    }
    
    # df <- d %>% 
    #     select(!!group_key, time, n) %>% 
    #     spread(key = time, value = n) %>% 
    #     tableGrob(rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = "white", col = "black")),
    #                                                   colhead = list(bg_params = list(col = "black"))))
    
    g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 0.75))
    ggsave(paste(file_name, "tiff", sep = "."), g, dpi = 320, width = 150, height = 150, units = "mm")
    ggsave(paste(file_name, "png", sep = "."), g, dpi = 320, width = 150, height = 150, units = "mm")
}

p_value_table <- data.table()

for (file_var in file_table$file) {
    group_key <- file_table[[match(file_var, file_table$file), "group_key"]]
    
    for (hospital_var in c("snubh", "snuh", "amc")) {
        if (hospital_var == "snubh") {
            d <- fread(file.path(hospital_var, "desc_stat", file_var)) %>% 
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
                                     file_var = file_var,
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
            
            if (group_key == "DM") {
                d_part <- d_part %>% 
                    mutate(DM = factor(DM, levels = c("Type 1", "Type 2")))
            } else if (group_key == "age_group") {
                d_part <- d_part %>% 
                    mutate(age_group = factor(age_group, levels = c("0-4Y", "5-9Y", "10-14Y")))
            } else {
                d_part <- d_part %>% 
                    mutate(sex = factor(sex, levels = c("Male", "Female")))
            }
            
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
                                     file_var = file_var,
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
    
    if (group_key == "DM") {
        d_reorder <- d %>% 
            mutate(DM = ifelse(DM == "Type 1", "Type 1 (n)", "Type 2 (n)"),
                   DM = factor(DM, levels = c("Type 2 (n)", "Type 1 (n)"))) %>%
            group_by_at(group_key_vec) %>%
            summarise(weighted_mean = weighted.mean(mean, n),
                      n = sum(n))
    } else if (group_key == "age_group") {
        d_reorder <- d %>% 
            mutate(age_group = ifelse(age_group == "0-4Y", "0-4Y (n)",
                                      ifelse(age_group == "5-9Y", "5-9Y (n)", "10-14Y (n)")),
                   age_group = factor(age_group, levels = c("10-14Y (n)", "5-9Y (n)", "0-4Y (n)"))) %>%
            group_by_at(group_key_vec) %>%
            summarise(weighted_mean = weighted.mean(mean, n),
                      n = sum(n))
    } else {
        d_reorder <- d %>% 
            mutate(sex = ifelse(sex == "Male", "Male (n)", "Female (n)"),
                   sex = factor(sex, levels = c("Female (n)", "Male (n)"))) %>%
            group_by_at(group_key_vec) %>%
            summarise(weighted_mean = weighted.mean(mean, n),
                      n = sum(n))
    }
    
    write_csv(d1, file.path(desc_stat_path, paste(str_extract(file_var, "\\w+Group"), "_MergedMeasurementDescStat.csv", sep = "_")))
    
    for (measurement_var in c("zbmi", "HBA1C")) {
        if (measurement_var == "zbmi") {
            hline_y  <- 1.0364
            annotate_y <- 0.82
            annotate_label <- "z=1.0364(85th percentile)"
            ylim <- c(-1, 3.5)
            ybreaks <- seq(-1, 3, 1)
            y_lab <- "BMI z score"
        } else {
            hline_y <- 7
            annotate_y <- 6.55
            annotate_label <- "Target(7.0%)"
            ylim <- c(4, 12)
            ybreaks <- seq(4, 12, 2)
            y_lab <- "HbA1c(%)"
        }
        
        
        # Compute p-value ---------------------------------------------------------
        for (time_var in time_vec) {
            if (group_key == "DM") {
                p_value_table <- bind_rows(p_value_table,
                                           data.frame(file = file_var,
                                                      measurement = measurement_var,
                                                      time = time_var,
                                                      p_value = t.test(rnorm_table[DM == "Type 1" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var,
                                                                                   value],
                                                                       rnorm_table[DM == "Type 2" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var,
                                                                                   value])$p.value))
            } else if (group_key == "sex") {
                p_value_table <- bind_rows(p_value_table,
                                           data.frame(file = file_var,
                                                      measurement = measurement_var,
                                                      time = time_var,
                                                      p_value = t.test(rnorm_table[sex == "Male" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var, value],
                                                                       rnorm_table[sex == "Female" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var, value])$p.value))
            } else if (file_var == "Type 2_DM_age_group_Group_Measurement_Desc_Stat.csv") {
                p_value_table <- bind_rows(p_value_table,
                                           data.frame(file = file_var,
                                                      measurement = measurement_var,
                                                      time = time_var,
                                                      p_value = t.test(rnorm_table[age_group == "5-9Y" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var, value],
                                                                       rnorm_table[age_group == "10-14Y" &
                                                                                       time == time_var &
                                                                                       measurement == measurement_var, value])$p.value))
            } else {
                p_value_table <- bind_rows(p_value_table,
                                           data.frame(file = file_var,
                                                      measurement = measurement_var,
                                                      time = time_var,
                                                      p_value = summary(aov(data = rnorm_table[time == time_var &
                                                                                                   measurement == measurement_var, ],
                                                                            as.formula(paste("value", group_key, sep = "~"))))[[1]][["Pr(>F)"]][[1]]))
            }
        }
        
        d2 <- d1 %>% 
            filter(measurement == measurement_var) %>% 
            as.data.table()
        
        d2_reorder <- d_reorder %>% 
            filter(measurement == measurement_var)
        
        plt <- d2 %>% 
            ggplot(aes_string(x = "time", y = "weighted_mean", group = group_key)) +
            geom_point(aes_string(shape = group_key), size = 3) +
            geom_line(size = 0.7) +
            geom_hline(yintercept = c(0, hline_y), linetype = "dashed") +
            annotate("text", x = 6.4, y = annotate_y, label = annotate_label) +
            coord_cartesian(ylim = ylim) +
            # scale_x_discrete(labels = c("03" = expression(bold(underline("03"))),
            #                             "36" = expression(bold(underline("36"))),
            #                             "60" = expression(bold(underline("60"))),
            #                             parse = TRUE)) +
            scale_y_continuous(breaks = ybreaks) +
            labs(x = element_blank(), y = y_lab) +
            # labs(x = "Time(month)", y = y_lab) +
            theme_classic() +
            theme(legend.position = "top",
                  legend.title = element_blank(),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15),
                  plot.margin = unit(c(10, 10, -2, 10), "mm"))
        
        if (p_value_table[measurement == measurement_var &
                          file == file_var &
                          time == "03", p_value][[1]] < 0.05 / 3 &
            !is.na(p_value_table[measurement == measurement_var &
                                 file == file_var &
                                 time == "03", p_value][[1]])) {
            plt <- plt +
                annotate("text", x = "03", y = max(d2[time == "03", weighted_mean]) + 1, label = "*", size = 7)
        }
        if (p_value_table[measurement == measurement_var &
                          file == file_var &
                          time == "36", p_value][[1]] < 0.05 / 3 &
            !is.na(p_value_table[measurement == measurement_var &
                                 file == file_var &
                                 time == "36", p_value][[1]])) {
            plt <- plt + 
                annotate("text", x = "36", y = max(d2[time == "36", weighted_mean]) + 1, label = "*", size = 7)
        }
        if (p_value_table[measurement == measurement_var &
                          file == file_var &
                          time == "60", p_value][[1]] < 0.05 / 3 &
            !is.na(p_value_table[measurement == measurement_var &
                                 file == file_var &
                                 time == "60", p_value][[1]])) {
            plt <- plt +
                annotate("text", x = "60", y = max(d2[time == "60", weighted_mean]) + 1, label = "*", size = 7)
        }
        
        df <-  ggplot(d2_reorder, aes_string(x = "time", y = group_key, label = "n")) +
            geom_text() +
            theme_test() +
            geom_hline(yintercept = 1.5) +
            geom_vline(xintercept = seq(1.5, 7.5, 1)) +
            labs(x = "Time(month)", y = element_blank()) +
            theme(plot.margin = unit(c(0, 10, 10, 5), "mm"),
                  axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 15))
        
        if (file_var == "Type 1_DM_age_group_Group_Measurement_Desc_Stat.csv") {
            df <- df +
                geom_hline(yintercept = 2.5)
        }
        
        g <- grid.arrange(grobs = list(plt, df), nrow = 2, heights = c(2, 0.75))
        
        ggsave(file.path(plot_path,
                         paste(str_extract(file_var, "\\w+Group"),
                               measurement_var,
                               "Plot.tiff",
                               sep = "_")), g, dpi = 320, width = 150, height = 150, units = "mm")
        ggsave(file.path(plot_path,
                         paste(str_extract(file_var, "\\w+Group"),
                               measurement_var,
                               "Plot.png",
                               sep = "_")), g, dpi = 320, width = 150, height = 150, units = "mm")
    }
}
write_csv(p_value_table, file.path(output_path, "4_P_Value.csv"))