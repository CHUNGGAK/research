setwd("E:/Users/DLCG001/workspace/hemato")

library(data.table)
library(tidyverse)
library(lubridate)

output_path <- "table1"
dir.create(output_path)

# Preprocess data ---------------------------------------------------------
d0 <- fread("data/data_1.csv")

d0 <- d0 %>% 
    mutate(INDEX_DATE = ymd(INDEX_DATE),
           NEW_TE_DATE = ymd(NEW_TE_DATE),
           OBSERVATION_PERIOD_END_DATE = ymd(ifelse(ymd(OBSERVATION_PERIOD_END_DATE) < ymd("2002-12-31"), "2019-01-01", 
                                                    ifelse(ymd(OBSERVATION_PERIOD_END_DATE) > ymd("2019-01-01"), "2019-01-01", OBSERVATION_PERIOD_END_DATE))),
           NEW_TE = ifelse(NEW_TE == 1, TRUE, FALSE),
           months_to_event = ifelse(NEW_TE == TRUE, (NEW_TE_DATE - INDEX_DATE) / 12, (OBSERVATION_PERIOD_END_DATE - INDEX_DATE) / 12),
           chad2_group = factor(ifelse(CHAD2 > 1, 2, CHAD2)),
           drug_cnt_group = factor(ifelse(DDI_ALL_DRUG_CNT > 2, 3, DDI_ALL_DRUG_CNT)),
           drug_period_group = factor(ifelse(DDI_ALL_DAYS == 0, "0", 
                                             ifelse(DDI_ALL_DAYS >= 1 & DDI_ALL_DAYS < 7, "1-6",
                                                    ifelse(DDI_ALL_DAYS >= 7 & DDI_ALL_DAYS < 30, "7-29", "30-"))),
                                      levels = c("0", "1-6", "7-29", "30-")),
           DDI_X = ifelse(DDI_X == 1, TRUE, FALSE),
           DDI_ALL = ifelse(DDI_ALL == 1, TRUE, FALSE),
           ddi_grade_group = factor(ifelse(DDI_ALL == FALSE, 0,
                                           ifelse(DDI_X == TRUE, 2, 1))),
           age = year(INDEX_DATE) - YEAR_OF_BIRTH,
           GENDER_CONCEPT_ID = factor(ifelse(GENDER_CONCEPT_ID == 8507, "Male", "Female")),
           WARFARIN = ifelse(WARFARIN == 1, TRUE, FALSE),
           RIVAROXABAN = ifelse(RIVAROXABAN == 1, TRUE, FALSE),
           APIXABAN = ifelse(APIXABAN == 1, TRUE, FALSE),
           DABIGATRAN = ifelse(DABIGATRAN == 1, TRUE, FALSE),
           EDOXABAN = ifelse(EDOXABAN == 1, TRUE, FALSE),
           CHF = ifelse(CHF == 1, TRUE, FALSE),
           HT = ifelse(HT == 1, TRUE, FALSE),
           stroke_tia_te = ifelse(STROKE == 1 | TIA == 1 | TE == 1, TRUE, FALSE),
           vascular_disease = ifelse(CAD == 1 | PAD == 1 | AP == 1, TRUE, FALSE),
           DM = ifelse(DM == 1, TRUE, FALSE),
           AF = ifelse(AF == 1, TRUE, FALSE),
           VTE = ifelse(VTE == 1, TRUE, FALSE))


# Create Table 1 ----------------------------------------------------------
sink(file.path(output_path, "output.txt"))
for (i in c(1, 7, 30)) {
    d0 <- fread(file.path("data", paste0("data_", i, ".csv")))
    
    d0 <- d0 %>% 
        mutate(INDEX_DATE = ymd(INDEX_DATE),
               NEW_TE_DATE = ymd(NEW_TE_DATE),
               OBSERVATION_PERIOD_END_DATE = ymd(ifelse(ymd(OBSERVATION_PERIOD_END_DATE) < ymd("2002-12-31"), "2019-01-01", 
                                                        ifelse(ymd(OBSERVATION_PERIOD_END_DATE) > ymd("2019-01-01"), "2019-01-01", OBSERVATION_PERIOD_END_DATE))),
               NEW_TE = ifelse(NEW_TE == 1, TRUE, FALSE),
               months_to_event = ifelse(NEW_TE == TRUE, (NEW_TE_DATE - INDEX_DATE) / 12, (OBSERVATION_PERIOD_END_DATE - INDEX_DATE) / 12),
               chad2_group = factor(ifelse(CHAD2 > 1, 2, CHAD2)),
               drug_cnt_group = factor(ifelse(DDI_ALL_DRUG_CNT > 2, 3, DDI_ALL_DRUG_CNT)),
               drug_period_group = factor(ifelse(DDI_ALL_DAYS == 0, "0", 
                                                 ifelse(DDI_ALL_DAYS >= 1 & DDI_ALL_DAYS < 7, "1-6",
                                                        ifelse(DDI_ALL_DAYS >= 7 & DDI_ALL_DAYS < 30, "7-29", "30-"))),
                                          levels = c("0", "1-6", "7-29", "30-")),
               DDI_X = ifelse(DDI_X == 1, TRUE, FALSE),
               DDI_ALL = ifelse(DDI_ALL == 1, TRUE, FALSE),
               ddi_grade_group = factor(ifelse(DDI_ALL == FALSE, 0,
                                               ifelse(DDI_X == TRUE, 2, 1))),
               age = year(INDEX_DATE) - YEAR_OF_BIRTH,
               GENDER_CONCEPT_ID = factor(ifelse(GENDER_CONCEPT_ID == 8507, "Male", "Female")),
               WARFARIN = ifelse(WARFARIN == 1, TRUE, FALSE),
               RIVAROXABAN = ifelse(RIVAROXABAN == 1, TRUE, FALSE),
               APIXABAN = ifelse(APIXABAN == 1, TRUE, FALSE),
               DABIGATRAN = ifelse(DABIGATRAN == 1, TRUE, FALSE),
               EDOXABAN = ifelse(EDOXABAN == 1, TRUE, FALSE),
               CHF = ifelse(CHF == 1, TRUE, FALSE),
               HT = ifelse(HT == 1, TRUE, FALSE),
               stroke_tia_te = ifelse(STROKE == 1 | TIA == 1 | TE == 1, TRUE, FALSE),
               vascular_disease = ifelse(CAD == 1 | PAD == 1 | AP == 1, TRUE, FALSE),
               DM = ifelse(DM == 1, TRUE, FALSE),
               AF = ifelse(AF == 1, TRUE, FALSE),
               VTE = ifelse(VTE == 1, TRUE, FALSE)) %>% 
        as.data.table()
    
    d0 %>% 
        summarize(median(age),
                  min = min(age),
                  max = max(age)) %>% 
        print()
    
    d0 %>% 
        group_by(DDI_ALL) %>% 
        summarize(median(age),
                  min = min(age),
                  max = max(age)) %>% 
        print()
    
    try(print(t.test(d0[DDI_ALL == FALSE, age], d0[DDI_ALL == TRUE, age])))
    
    for (j in c("GENDER_CONCEPT_ID",
                "AF", "VTE",
                "WARFARIN", "RIVAROXABAN", "APIXABAN", "DABIGATRAN", "EDOXABAN",
                "CHF", "HT", "stroke_tia_te", "vascular_disease", "DM")) {
        index <- c("DDI_ALL", j)
        table_tmp <- table(d0[, ..index])
        cat(paste("DDI", i, j, "Chi-squared test"))
        cat(rep("\n", 2))
        print(table_tmp)
        cat(rep("\n", 2))
        try(print(chisq.test(table_tmp)))
        cat(rep("-", 30))
        cat(rep("\n", 2))
    }
}
sink()
