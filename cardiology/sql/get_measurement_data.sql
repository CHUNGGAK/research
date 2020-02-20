-- Export echo measurement data
SPOOL 'E:/Users/DLCG001/workspace/cardiology/data/echo_data.csv';
SELECT /*csv*/ cohort.person_id, to_char(cohort_start_date, 'yyyy') - year_of_birth age,
    gender_concept_id, measurement_concept_id, measurement_source_value,
    measurement_date, value_as_number, statin_date, ras_date, bis_date, gap_day,
    gap_value
FROM gr2457.cohort_re cohort
INNER JOIN gr2457.meas_re echo
    ON cohort.person_id = echo.person_id
        AND cohort.cohort_start_date = measurement_date;
SPOOL OFF;


-- Export lab measurement data
SPOOL 'E:/Users/DLCG001/workspace/cardiology/data/lab_data.csv';
SELECT cohort.person_id, to_char(cohort.cohort_start_date, 'yyyy') - year_of_birth age,
    gender_concept_id, measurement_concept_id, concept_name measurement_source_value,
    value_as_number
    statin_date, ras_date, bis_date
FROM gr2457.cohort_re cohort
INNER JOIN gr2457.meas_lab lab
    ON cohort.person_id = lab.person_id
        AND cohort.cohort_start_date = lab.cohort_start_date;
SPOOL OFF;