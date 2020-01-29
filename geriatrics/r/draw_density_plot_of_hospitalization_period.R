setwd("E:/Users/DLCG001/workspace/geriatric")

library(data.table)
library(tidyverse)

datum <- fread("data/hospitalization_period.csv") %>% 
    select(SUBJECT_ID, HOSPITALIZATION_PERIOD) %>% 
    mutate(SUBJECT_ID = as.factor(SUBJECT_ID))

prop_table <- prop.table(table(datum$HOSPITALIZATION_PERIOD))

datum %>%
    ggplot(aes(x = HOSPITALIZATION_PERIOD)) +
    geom_density() +
    geom_vline(xintercept = 14, linetype = "dashed") +
    annotate(geom = "text", x = 23, y = quantile(prop_table, 0.05),
             label = round(sum(prop_table[15:31]), 4)) +
    theme_classic()
    
output_path <- file.path("output", "density_plot_of_hospitalization_period")

if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
}

ggsave(file.path(output_path, "density_plot_of_hospitalization_period.png"))
    
