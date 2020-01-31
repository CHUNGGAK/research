SELECT subject_id, cohort_start_date, cohort_end_date,
    cohort_end_date - cohort_start_date hospitalization_period
FROM cohort
WHERE cohort_definition_id = 803
-- ORDER BY hospitalization_period DESC
;