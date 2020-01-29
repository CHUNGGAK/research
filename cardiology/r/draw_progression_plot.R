setwd("E:/Users/DLCG001/workspace/cardiology")

library(tidyverse)
library(data.table)

remove_outlier <- function(x, na.rm) {
    qti <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    co <- 1.5 * IQR(x)
    y <- x
    y[x < qti[1] - co] <- NA
    y[x > qti[2] + co] <- NA
    return(y)
}

output_path <- "output/progression_plot"
psm_output_path <- file.path(output_path, "psm")

if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
}

if (!dir.exists(psm_output_path)) {
    dir.create(psm_output_path, recursive = TRUE)
}

datum <- fread("data/data.csv") %>% 
    select(PERSON_ID, MEASUREMENT_CONCEPT_ID, MEASUREMENT_DATE, VALUE_AS_NUMBER,
           GAP_VALUE, STATIN_DATE, RAS_DATE, BIS_DATE, BASE_LINE, FOLLOW_UP,
           GAP_DAY) %>% 
    mutate(PERSON_ID = as.factor(PERSON_ID),
           MEASUREMENT_CONCEPT_ID = as.factor(MEASUREMENT_CONCEPT_ID),
           MEASUREMENT_DATE = as.Date(MEASUREMENT_DATE, "%Y/%m/%d"),
           STATIN_DATE = as.Date(STATIN_DATE, format = "%Y/%m/%d"),
           RAS_DATE = as.Date(RAS_DATE, format = "%Y/%m/%d"),
           BIS_DATE = as.Date(BIS_DATE, format = "%Y/%m/%d"),
           BASE_LINE = as.logical(BASE_LINE),
           FOLLOW_UP = as.logical(FOLLOW_UP)) %>% 
    filter(BASE_LINE == TRUE & FOLLOW_UP == TRUE) %>%
    mutate(statin = if_else(is.na(STATIN_DATE) | STATIN_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           ras = if_else(is.na(RAS_DATE) | RAS_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           bis = if_else(is.na(BIS_DATE) | BIS_DATE > MEASUREMENT_DATE, FALSE, TRUE),
           progression_rate = GAP_VALUE / GAP_DAY * 365) %>% 
    select(PERSON_ID, MEASUREMENT_CONCEPT_ID, MEASUREMENT_DATE, VALUE_AS_NUMBER,
           statin, ras, bis, progression_rate)

measurement_df <- data.frame(id = c(3028570, 21493999, 3003445),
                             name = c('AV Vmax', 'AV meanPG', 'AV area'))

for (drug_var in c("statin", "ras", "bis")) {
    p_data <- fread(file.path("output",
                              "propensity_score_matching",
                              paste(drug_var, "matchedPop.csv", sep = "_"))) %>% 
        select(subjectId, treatment, cohortStartDate) %>%
        mutate(subjectId = as.factor(str_trim(subjectId, side = "both")),
               treatment = as.logical(treatment),
               cohortStartDate = as.Date(cohortStartDate, "%Y-%m-%d")) %>% 
        inner_join(datum, by = c("subjectId" = "PERSON_ID",
                                   setNames(drug_var, "treatment")))
    
    for (measurement_var in measurement_df$id) {
        measurement_name <-  measurement_df[measurement_df$id == measurement_var, "name"]
        plot_name <- paste(drug_var, measurement_name, 'Progression Plot')
        
        datum %>% 
            filter(MEASUREMENT_CONCEPT_ID == measurement_var) %>% 
            mutate(VALUE_AS_NUMBER = remove_outlier(VALUE_AS_NUMBER),
                   progression_rate = remove_outlier(progression_rate)) %>% 
            ggplot(aes(x = VALUE_AS_NUMBER, y = progression_rate)) +
            geom_smooth(color = "statin", se = FALSE) +
            theme_classic() + 
            labs(title = plot_name,
                 x = paste("Baseline", measurement_name),
                 y = paste("Progression rate(AV", measurement_name,"/ year)"))
        
        ggsave(file.path(output_path, paste0(plot_name, ".png")))
        
        p_data %>% 
            filter(MEASUREMENT_CONCEPT_ID == measurement_var) %>% 
            mutate(VALUE_AS_NUMBER = remove_outlier(VALUE_AS_NUMBER),
                   progression_rate = remove_outlier(progression_rate)) %>% 
            ggplot(aes_string(x = "VALUE_AS_NUMBER", y = "progression_rate",
                              linetype = drug_var)) +
            geom_smooth(color = "black", linetype = treatment, se = FALSE) +
            geom_point() +
            theme_classic() + 
            labs(title = plot_name,
                 x = paste("Baseline", measurement_name),
                 y = paste("Progression rate(AV", measurement_name,"/ year)"))
        
        ggsave(file.path(psm_output_path, paste0("psm_", plot_name, ".png")))
    }
}
