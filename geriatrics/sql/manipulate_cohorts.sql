-- Modify cohort start date, cohort end date of MFS >= 3 cohort
INSERT INTO lcg_cohort
SELECT 24470006 cohort_definition_id, outcome.subject_id, cohort_start_date,
    cohort_end_date + random_value cohort_end_date
FROM (
    SELECT subject_id, round(dbms_random.value(0, 30)) random_value
    FROM cohort
    WHERE cohort_definition_id = 501
    ) outcome
INNER JOIN (
    SELECT subject_id, cohort_start_date, cohort_end_date
    FROM cohort
    WHERE  cohort_definition_id = 803
    ) target
    ON outcome.subject_id = target.subject_id;
    
-- Merge outcome cohorts
INSERT INTO lcg_cohort
SELECT 5 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM cohort
WHERE cohort_definition_id IN (
    111, -- Hospitalization period >= 21
    203 -- Emergency room re-admission
    )
UNION
SELECT 5 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM lcg_cohort
WHERE cohort_definition_id = 4; -- Death