setwd("E:/Users/DLCG001/workspace/hemato")

library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

plot_path <- "kaplan_meier"
csv_path <- "csv"

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
                                           ifelse(DDI_X == TRUE, 2, 1))))



# Function analysis -------------------------------------------------------
hemato_analysis <- function(fit,
                            data,
                            group = FALSE,
                            add_legend = FALSE,
                            file_name,
                            plot_path,
                            csv_path = FALSE) {
    if (add_legend) {
        legend.position <- "top"
    } else {
        legend.position <- "none"
    }
    
    if (group == TRUE) {
        palette <- "grey"
    } else {
        palette <- "black"
    }
    
    plot <- ggsurvplot(fit = fit,
                       data = data,
                       linetype = "strata",
                       pval = TRUE,
                       risk.table = TRUE,
                       censor = FALSE,
                       break.time.by = 12,
                       palette = palette,
                       xlim = c(0, 120),
                       ylim = c(0.5, 1),
                       pval.coord = c(0, 0.6))
    
    plot$plot <- plot$plot +
        labs(x = "Time (months)", y = "Probability of TE-free survival") +
        theme(legend.position = legend.position,
              legend.title = element_blank())
    
    ggsave(file.path(plot_path, paste0(file_name, ".png")),
           print(plot),
           width = 15, height = 20, units = "cm")
    
    if (csv_path != FALSE) {
        plot$data.survplot %>%
            group_by(strata) %>% 
            summarize(lower = lower[which.min(abs(time - 24))],
                      upper = upper[which.min(abs(time - 24))]) %>% 
            write_csv(file.path(csv_path, paste0(file_name, ".csv")))
    }
}


# Create Total Patients Surv Plot -------------------------------------
fit <- survfit(Surv(months_to_event, NEW_TE) ~ 1, data = d0)

hemato_analysis(fit = fit,
                data = d0,
                add_legend = FALSE,
                file_name = "total",
                plot_path = plot_path,
                csv_path = FALSE)


# Create DDI criteria days >= i Patients Surv Plot ------------------------
for (i in c(1, 7, 30)) {
    d_tmp <- d0 %>% 
        filter(DDI_ALL_DAYS >= i)
    
    fit <- survfit(Surv(months_to_event, NEW_TE) ~ 1, data = d_tmp)
    
    hemato_analysis(fit = fit,
                    data = d_tmp,
                    add_legend = FALSE,
                    file_name = paste0("ddi_", i),
                    plot_path = plot_path,
                    csv_path = FALSE)
}


# Create Surv Plot by DDI Period ------------------------------------------
fit <- survfit(Surv(months_to_event, NEW_TE) ~ drug_period_group, data = d0)
v_strata <- c("0", "1 ~ 6", "7 ~ 29", "> 29")
names(fit$strata) <- v_strata

hemato_analysis(fit = fit,
                data = d0,
                group = TRUE,
                add_legend = TRUE,
                file_name = "drug_period_group",
                plot_path = plot_path,
                csv_path = csv_path)


# Create Surv Plot by DDI Occurrence --------------------------------------
# Non vs C & D & X
fit <- survfit(Surv(months_to_event, NEW_TE) ~ DDI_ALL, data = d0)
names(fit$strata) <- c("Non", "C & D & X")

hemato_analysis(fit = fit,
                data = d0,
                group = TRUE,
                add_legend = TRUE,
                file_name = "non_vs_cdx",
                plot_path = plot_path,
                csv_path = csv_path)


# Non vs C & D vs X
fit <- survfit(Surv(months_to_event, NEW_TE) ~ ddi_grade_group, data = d0)
names(fit$strata) <- c("Non", "C & D", "X")

hemato_analysis(fit = fit,
                data = d0,
                group = TRUE,
                add_legend = TRUE,
                file_name = "non_vs_cd_vs_x",
                plot_path = plot_path,
                csv_path = csv_path)


# Non & C & D vs X by DDI period
d_tmp <- d0 %>% 
    mutate(group = factor(ifelse(ddi_grade_group %in% c("0", "1"), 0,
                                 ifelse(ddi_grade_group == 2 & DDI_X_DAYS < 7, 1,
                                        ifelse(ddi_grade_group == 2 & DDI_X_DAYS >= 7 & DDI_X_DAYS < 30, 2, 3)))))

fit <- survfit(Surv(months_to_event, NEW_TE) ~ group, data = d_tmp)
names(fit$strata) <- c("Non & C & D", "X; < 7D", "X; 7 ~ 29D", "X; > 29D")

hemato_analysis(fit = fit,
                data = d_tmp,
                group = TRUE,
                add_legend = TRUE,
                file_name = "noncd_vs_xlow_vs_xmidium_vs_xhigh",
                plot_path = plot_path,
                csv_path = csv_path)

# Create Surv Plot with DDI C & D & X Patients ----------------------------
for (i in c("ddi_cd", "ddi_x")) {
    if (i == "ddi_cd") {
        d_tmp <- d0 %>% 
            filter(DDI_ALL == TRUE)
    } else {
        d_tmp <- d0 %>% 
            filter(DDI_X == TRUE)
    }
    
    # by Duplicated Drug Count
    fit <- survfit(Surv(months_to_event, NEW_TE) ~ drug_cnt_group, data = d_tmp)
    names(fit$strata) <- c("1", "2", "> 2")
    
    hemato_analysis(fit = fit,
                    data = d_tmp,
                    group = TRUE,
                    add_legend = TRUE,
                    file_name = paste0(i, "_duplicated_drug_count"),
                    plot_path = plot_path,
                    csv_path = csv_path)
    
    
    # by CHAD2
    fit <- survfit(Surv(months_to_event, NEW_TE) ~ chad2_group, data = d_tmp)
    names(fit$strata) <- c("1", "2", "> 2")
    
    hemato_analysis(fit = fit,
                    data = d_tmp,
                    group = TRUE,
                    add_legend = TRUE,
                    file_name = paste0(i, "_chad2"),
                    plot_path = plot_path,
                    csv_path = csv_path)
}


# Create Surv Plot by DDI Occurrence Filter CHAD2 -------------------------
for (i in 0:9) {
    d_tmp <- d0 %>% 
        filter(CHAD2 == i)
    
    try({
        fit <- survfit(Surv(months_to_event, NEW_TE) ~ DDI_ALL, data = d_tmp)
        names(fit$strata) <- c("Non", "C & D & X")
        
        hemato_analysis(fit = fit,
                        data = d_tmp,
                        group = TRUE,
                        add_legend = TRUE,
                        file_name = paste0("chad2_", i, ".png"),
                        plot_path = plot_path,
                        csv_path = csv_path)
    })
}
