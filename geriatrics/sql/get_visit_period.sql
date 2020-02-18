SELECT subject_id, cohort_start_date, cohort_end_date, visit_concept_id,
    sum(visit_end_date - visit_start_date + 1) visit_period
FROM cohort
INNER JOIN cdm_2018_view.visit_occurrence
    ON subject_id = person_id
WHERE cohort_definition_id = 803
    AND cohort_start_date - 365 <= visit_start_date
    AND cohort_start_date >= visit_start_date
GROUP BY subject_id, cohort_start_date, cohort_end_date, visit_concept_id;