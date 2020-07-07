CREATE TABLE lcohort AS
SELECT cohort_definition_id, subject_id, cohort_start_date + random_value cohort_start_date,
    cohort_end_date + random_value cohort_end_date
FROM (
    SELECT 13010001 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date,
    round(dbms_random.value(0, 30)) random_value
    FROM cohort
    WHERE cohort_definition_id = 1301)
;

INSERT INTO lcohort
SELECT 13010002 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM lcohort
WHERE cohort_definition_id = 13010001
UNION
    SELECT 13010002 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
    FROM cohort
    WHERE cohort_definition_id IN (122, 203, 111);

INSERT INTO lcohort
SELECT 8010001 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM cohort
WHERE cohort_definition_id = 801;

INSERT INTO lcohort
SELECT 1120001 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM cohort
WHERE cohort_definition_id = 112;

INSERT INTO lcohort
SELECT 1130001 cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
FROM cohort
WHERE cohort_definition_id = 113;