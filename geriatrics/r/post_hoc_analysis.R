result <- loadPlpResult("/home/lee/workspace/geriatrics/output/v1.1.0/Analysis_2/plpResult")

df <- datum$cohorts %>% 
    select(rowId, subjectId) %>% 
    mutate(rowId = as.character(rowId),
           subjectId = as.character(subjectId)) %>% 
    inner_join(datum$outcomes %>% 
                   filter(daysToEvent >= 0 & daysToEvent <= 30) %>% 
                   select(rowId) %>% 
                   mutate(rowId = as.character(rowId)),
               by = "rowId") %>% 
    inner_join(result$prediction %>% 
                   filter(outcomeCount == 1) %>% 
                   select(rowId, value) %>% 
                   mutate(rowId = as.character(rowId)))

df <- result$prediction %>% 
    filter(indexes == -1) %>% 
    select(rowId, subjectId, value) %>% 
    mutate(rowId = as.character(rowId),
           subjectId = as.character(subjectId))

connectionDetails <- createConnectionDetails(dbms = dbms,
                                             server = server,
                                             user = user,
                                             password = password)
conn <- connect(connectionDetails)
mfs_cohorts <- querySql(conn,
                        "SELECT subject_id, measurement_concept_id, value_as_number 
                        FROM cohort
                        INNER JOIN (
                            SELECT person_id, measurement_concept_id, value_as_number
                            FROM cdm_2018_view.measurement
                            WHERE measurement_concept_id IN (2000000018, 37018726)
                            )
                            ON subject_id = person_id
                        WHERE cohort_definition_id = 803") %>% 
    mutate(SUBJECT_ID = as.character(SUBJECT_ID),
           MEASUREMENT_CONCEPT_ID = as.character(MEASUREMENT_CONCEPT_ID))

p_df <- df %>% 
    inner_join(mfs_cohorts, by = c("subjectId" = "SUBJECT_ID")) %>% 
    mutate(predicted_value = ifelse(VALUE_AS_NUMBER >= 3, TRUE, FALSE),
           actual_value = ifelse(value >= 0.308611375, TRUE, FALSE))

table(p_df$predicted_value, p_df$actual_value)

threshold <- result$performanceEvaluation$thresholdSummary %>% 
    filter(Eval == "test") %>% 
    top_n(wt = f1Score, n = 1) %>% 
    select(f1Score) %>% 
    as.numeric()
    
p_df %>% 
    ggplot(aes(x = VALUE_AS_NUMBER, y = value)) +
    geom_point()
