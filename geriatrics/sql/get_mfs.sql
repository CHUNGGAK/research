SELECT subject_id, cohort_start_date, cohort_end_date, measurement_concept_id,
    measurement_date, value_as_number, measurement_source_value
FROM cohort
INNER JOIN cdm_2018_view.measurement
ON subject_id = person_id
WHERE cohort_definition_id = 803
    AND measurement_concept_id IN (2000000018, 37018726)
    AND cohort_start_date - 90 <= measurement_date
    AND cohort_start_date >= measurement_date;