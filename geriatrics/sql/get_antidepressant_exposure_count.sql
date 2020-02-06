SELECT person_id, drug_concept_id, drug_era_start_date, drug_era_end_date,
    drug_exposure_count
FROM cdm_2018_view.drug_era
WHERE drug_concept_id IN (
    SELECT concept_id
    FROM cdm_voca.concept_ancestor
    INNER JOIN cdm_voca.concept
        ON descendant_concept_id = concept_id
    WHERE ancestor_concept_id = 21604686
        AND concept_class_id = 'Ingredient')
    AND person_id IN (
        SELECT subject_id
        FROM cohort
        WHERE cohort_definition_id = 803);