setwd("E:/Users/DLCG001/workspace/geriatric")

library(data.table)
library(tidyverse)

output_path <- file.path("output", "density_plot")

if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
}



# draw hospitalization period ---------------------------------------------
datum <- fread("data/hospitalization_period.csv") %>% 
    select(SUBJECT_ID, HOSPITALIZATION_PERIOD) %>% 
    mutate(SUBJECT_ID = as.factor(SUBJECT_ID))

prop_table <- prop.table(table(datum$HOSPITALIZATION_PERIOD))

# datum %>%
#     ggplot(aes(x = HOSPITALIZATION_PERIOD)) +
#     geom_density() +
#     geom_vline(xintercept = 14, linetype = "dashed") +
#     annotate(geom = "text", x = 23, y = quantile(prop_table, 0.05),
#              label = round(sum(prop_table[15:31]), 4)) +
#     theme_bw()

with(density(datum$HOSPITALIZATION_PERIOD), data.frame(x, y)) %>% 
    ggplot(mapping = aes(x = x, y = y)) +
    geom_line(color = "black") +
    geom_area(mapping = aes(x = ifelse(x >= 14, x, NA)), fill = "lightblue") +
    geom_vline(xintercept = 14, linetype = "dashed") +
    annotate(geom = "text", x = 23, y = quantile(prop_table, 0.05),
             label = round(sum(prop_table[15:31]), 4)) +
    xlim(0, 30) +
    theme_bw()
ggsave(file.path(output_path, "density_plot_of_hospitalization_period.png"))
    

# draw mfs ----------------------------------------------------------------
datum <- fread("data/mfs.csv") %>% 
    select(SUBJECT_ID, MEASUREMENT_CONCEPT_ID, VALUE_AS_NUMBER) %>% 
    mutate(SUBJECT_ID = as.character(SUBJECT_ID),
           MEASUREMENT_CONCEPT_ID = as.factor(MEASUREMENT_CONCEPT_ID))

for (i in unique(datum$MEASUREMENT_CONCEPT_ID)) {
    datum %>% 
        filter(MEASUREMENT_CONCEPT_ID == i) %>% 
        ggplot(aes(x = VALUE_AS_NUMBER)) +
        geom_density(fill = "lightblue") +
        theme_bw()
    ggsave(file.path(output_path, paste0(i, "_density_plot.png")))
}