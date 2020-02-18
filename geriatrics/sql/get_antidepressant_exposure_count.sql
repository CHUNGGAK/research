SELECT subject_id, cohort_start_date, cohort_end_date,
    sum(drug_exposure_count) drug_exposure_count
FROM cohort
INNER JOIN cdm_2018_view.drug_era
    ON subject_id = person_id
WHERE cohort_definition_id = 803
    AND drug_concept_id IN (
        SELECT concept_id
        FROM cdm_voca.concept_ancestor
        INNER JOIN cdm_voca.concept
            ON descendant_concept_id = concept_id
        WHERE ancestor_concept_id = 21604686
            AND concept_class_id = 'Ingredient')
    AND cohort_start_date - 365 <= drug_era_start_date
    AND cohort_start_date >= drug_era_start_date
GROUP BY subject_id, cohort_start_date, cohort_end_date;