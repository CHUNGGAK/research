output_path <- file.path("output", "mfs_density_plot")
path_assistant(output_path)

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


