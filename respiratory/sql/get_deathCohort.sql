SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, death_date,
    cause_concept_id, concept_name cause_concept_name
FROM cohort
LEFT JOIN cdm_2019_view.death
    ON subject_id = person_id
LEFT JOIN cdm_voca.concept
    ON cause_concept_id = concept_id
WHERE cohort_definition_id IN (831, 832);