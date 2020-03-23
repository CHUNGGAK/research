setwd("E:/Users/DLCG001/workspace/cardiology")

library(tidyverse)
library(data.table)
library(DatabaseConnector)
library(lubridate)
library(cowplot)

source("E:/Users/DLCG001/workspace/ltool/ltool.R")
source("E:/Users/DLCG001/workspace/connect_information.R")

output_path <- "output/progression_plot_v2"
# psm_output_path <- file.path(output_path, "psm")

# path_assistant(psm_output_path)
path_assistant(output_path)

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)
conn <- connect(connectionDetails)

r_data <- querySql(conn, 
                   "SELECT person_id, to_char(measurement_date, 'yyyy') - year_of_birth age, gender_concept_id,
                  measurement_source_value, measurement_date, value_as_number,
                  statin_date, ras_date, bis_date, a_date, b_date, gap_day, gap_value
                  FROM gr2457.meas_re")

datum <- r_data %>% 
    mutate(PERSON_ID = as.character(PERSON_ID),
           MEASUREMENT_DATE = ymd(MEASUREMENT_DATE),
           STATIN_DATE = ymd(STATIN_DATE),
           RAS_DATE = ymd(RAS_DATE),
           BIS_DATE = ymd(BIS_DATE),
           baseline = ifelse(!is.na(A_DATE), TRUE, FALSE),
           follow_up = ifelse(!is.na(B_DATE), TRUE, FALSE),
           category = ifelse(RAS_DATE <= MEASUREMENT_DATE & STATIN_DATE <= MEASUREMENT_DATE, "ras(o)-statin(o)", 
                          ifelse(RAS_DATE <= MEASUREMENT_DATE & (is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE), "ras(o)-statin(x)",
                                 ifelse((is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE) & STATIN_DATE <= MEASUREMENT_DATE, "ras(x)-statin(o)",
                                        ifelse((is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE) & (is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE), "ras(x)-statin(x)", NA)))),
           progression_rate = GAP_VALUE / GAP_DAY * 365) %>% 
    filter(follow_up == TRUE) %>%
    select(PERSON_ID, MEASUREMENT_SOURCE_VALUE, MEASUREMENT_DATE, VALUE_AS_NUMBER,
           category, progression_rate) %>% 
    as.data.table()

measurement_list <- distinct(datum, MEASUREMENT_SOURCE_VALUE)[c(1, 5, 9), MEASUREMENT_SOURCE_VALUE]

category_list <- combn(c("ras(o)-statin(o)", "ras(o)-statin(x)", "ras(x)-statin(o)", "ras(x)-statin(x)"), 2)

# for (drug_var in c("statin", "ras", "bis")) {
    # psm_data <- fread(file.path("output/propensity_score_matching",
    #                             drug_var,
    #                             "matching_data",
    #                             paste(drug_var, "matching_group.csv", sep ="_"))) %>% 
    #     select(subjectId, treatment, cohortStartDate) %>% 
    #     mutate(subjectId = as.character(subjectId),
    #            treatment = ifelse(treatment == 0, FALSE, TRUE),
    #            cohortStartDate = ymd(cohortStartDate)) %>% 
    #     inner_join(datum,
    #                by = c("subjectId" = "PERSON_ID",
    #                       "cohortStartDate" = "MEASUREMENT_DATE",
    #                       setNames(drug_var, "treatment")))
    
    for (measurement_var in measurement_list) {
        for (category_index in 1:ncol(category_list)) {
            plot_name <- paste(
                # drug_var,
                paste(category_list[, category_index], collapse = " vs "), measurement_var, 'Progression Plot')
            
            # Draw graphs before PSM --------------------------------------------------
            p_data <- datum %>%  
                filter(MEASUREMENT_SOURCE_VALUE == measurement_var & !is.na(category) & 
                           category %in% category_list[, category_index]) %>% 
                mutate(VALUE_AS_NUMBER = remove_outlier_with_qti(VALUE_AS_NUMBER),
                       progression_rate = remove_outlier_with_qti(progression_rate))
            
            progression_plot <- p_data %>% 
                ggplot(aes(x = VALUE_AS_NUMBER, y = progression_rate,
                           linetype = category)) +
                geom_smooth(color = "black") +
                theme_bw() + 
                labs(title = plot_name,
                     x = paste("Baseline", measurement_var),
                     y = paste("Progression rate(AV", measurement_var,"/ year)"))
            
            density_plot <- p_data %>% 
                ggplot(aes(x = VALUE_AS_NUMBER, linetype = category)) +
                geom_density() +
                labs(title = paste(paste(category_list[, category_index], collapse = " & "), measurement_var, "Density Plot"),
                     x = measurement_var) +
                theme_bw()
            
            plot_grid(progression_plot, density_plot,
                      ncol = 1, rel_heights = c(2, 1))
            ggsave(file.path(output_path, paste0(plot_name, ".png")),
                   width = 18, height = 23, units = "cm")
        }
        
        
        # Draw graphs after PSM ---------------------------------------------------
        # p_psm_data <- psm_data %>% 
        #     filter(MEASUREMENT_SOURCE_VALUE == measurement_var) %>% 
        #     mutate(VALUE_AS_NUMBER = remove_outlier_with_qti(VALUE_AS_NUMBER),
        #            progression_rate = remove_outlier_with_qti(progression_rate))
        # 
        # psm_progression_plot <- p_psm_data %>% 
        #     ggplot(aes(x = VALUE_AS_NUMBER, y = progression_rate,
        #                linetype = treatment)) +
        #     geom_smooth(color = "black") +
        #     theme_bw() + 
        #     labs(title = plot_name,
        #          x  = paste("Baseline", measurement_var),
        #          y = paste("Progression rate(AV", measurement_var,"/ year)"))
        # 
        # psm_density_plot <- p_psm_data %>% 
        #     ggplot(aes(x = VALUE_AS_NUMBER, linetype = treatment)) +
        #     geom_density() +
        #     labs(title = paste(drug_var, measurement_var, "Density Plot"),
        #          x = measurement_var) +
        #     theme_bw()
        # 
        # plot_grid(psm_progression_plot, psm_density_plot,
        #           ncol = 1, rel_heights = c(2, 1))
        # ggsave(file.path(psm_output_path, paste0("psm_", plot_name, ".png")),
        #        width = 18, height = 23, units = "cm")
    }
# }
