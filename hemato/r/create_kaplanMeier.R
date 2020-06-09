setwd("E:/Users/DLCG001/workspace/hemato")

library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

d0 <- fread("data/data.csv")


# Create Total Patients Surv Plot -------------------------------------
d0 <- d0 %>% 
    mutate(INDEX_DATE = ymd(INDEX_DATE),
           NEW_TE_DATE = ymd(NEW_TE_DATE),
           OBSERVATION_PERIOD_END_DATE = ymd(ifelse(ymd(OBSERVATION_PERIOD_END_DATE) < ymd("2002-12-31"), "2019-12-31", OBSERVATION_PERIOD_END_DATE)),
           NEW_TE = ifelse(NEW_TE == 1, TRUE, FALSE),
           months_to_event = ifelse(NEW_TE == TRUE, (NEW_TE_DATE - INDEX_DATE) / 12, (OBSERVATION_PERIOD_END_DATE - INDEX_DATE) / 12),
           chad2_group = factor(ifelse(CHAD2 > 1, 2, CHAD2)),
           drug_cnt_group = factor(ifelse(DDI_ALL_DRUG_CNT > 2, 3, DDI_ALL_DRUG_CNT)),
           drug_period_group = factor(ifelse(DDI_ALL_DAYS >= 1 & DDI_ALL_DAYS < 7, "1-6",
                                             ifelse(DDI_ALL_DAYS >= 7 & DDI_ALL_DAYS < 30, "7-29",
                                                    ifelse(DDI_ALL_DAYS >= 30, "30", "0")))),
           ddi_grade_group = factor(ifelse(DDI_ALL == 0, 0,
                                           ifelse(DDI_X == 1, 2, 1))))

plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ 1, data = d0),
                   data = d0,
                   risk.table = TRUE,
                   censor = FALSE)
plot$plot +
    labs(x = "Time (months)", y = "Probability of TE-free survival") +
    scale_fill_grey() +
    scale_color_grey()


# Create DDI criteria days >= i Patients Surv Plot ------------------------
for (i in c(1, 7, 30)) {
    d_tmp <- d0 %>% 
        filter(DDI_ALL_DAYS >= i)
    
    plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ 1, data = d_tmp),
                       data = d_tmp,
                       risk.table = TRUE,
                       censor = FALSE)
}



# Create Surv Plot by DDI Period ------------------------------------------
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ drug_period_group, data = d0),
                   data = d0,
                   risk.table = TRUE,
                   censor = FALSE)



# Create Surv Plot by DDI Occurrence --------------------------------------
# Non vs C & D & X
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ DDI_ALL, data = d0),
                   data = d0,
                   risk.table = TRUE,
                   censor = FALSE)



# Non vs C & D vs X
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ ddi_grade_group, data = d0),
                   data = d0,
                   risk.table = TRUE,
                   censor = FALSE)

# Non & C & D vs X by DDI period
d_tmp <- d0 %>% 
    mutate(group = factor(ifelse(ddi_grade_group %in% c("0", "1"), 0,
                                 ifelse(ddi_grade_group == 2 & DDI_X_DAYS < 7, 1,
                                        ifelse(ddi_grade_group == 2 & DDI_X_DAYS >= 7 & DDI_X_DAYS < 30, 2, 3)))))

plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ group, data = d_tmp),
                   data = d_tmp,
                   risk.table = TRUE,
                   censor = FALSE)


# Create Surv Plot with DDI C & D & X Patients ----------------------------
d_tmp <- d0 %>% 
    filter(DDI_ALL == 1)

# by Duplicate Drug Count
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ drug_cnt_group, data = d_tmp),
                   data = d_tmp,
                   risk.table = TRUE,
                   censor = FALSE)

# by CHAD2
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ chad2_group, data = d_tmp),
                   data = d_tmp,
                   risk.table = TRUE,
                   censor = FALSE)



# Create Surv Plot with DDI X Patients ----------------------------------------
d_tmp <- d0 %>% 
    filter(DDI_X == 1)

# by Duplicate Drug Count
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ drug_cnt_group, data = d_tmp),
                   data = d_tmp,
                   risk.table = TRUE,
                   censor = FALSE)

# by CHAD2
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ chad2_group, data = d_tmp),
                   data = d_tmp,
                   risk.table = TRUE,
                   censor = FALSE)


# Create Surv Plot by CHAD2 -----------------------------------------------
plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ chad2_group, data = d0),
                   data = d0,
                   risk.table = TRUE,
                   censor = FALSE)


# Create Surv Plot by DDI Occurrence Filter CHAD2 -------------------------
for (i in 0:9) {
    d_tmp <- d0 %>% 
        filter(CHAD2 == i)
    
    plot <- ggsurvplot(fit = survfit(Surv(months_to_event, NEW_TE) ~ DDI_ALL, data = d_tmp),
                       data = d_tmp,
                       risk.table = TRUE,
                       censor = FALSE)
}