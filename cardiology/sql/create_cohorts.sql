-- statin 비사용군
INSERT INTO lcg_cohort
SELECT 24470000 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE statin_date IS NULL
        OR measurement_date < statin_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;


-- statin 사용군
INSERT INTO lcg_cohort
SELECT 24470001 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE measurement_date >= statin_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;


-- ras 비사용군
INSERT INTO lcg_cohort
SELECT 24470002 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE ras_date IS NULL
        OR measurement_date < ras_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;


-- ras 사용군
INSERT INTO lcg_cohort
SELECT 24470003 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE measurement_date >= ras_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;    


-- bis 비사용군
INSERT INTO lcg_cohort
SELECT 24470004 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE bis_date IS NULL
        OR measurement_date < bis_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;


-- bis 사용군
INSERT INTO lcg_cohort
SELECT 24470005 cohort_definition_id, cohort.person_id subject_id,
    cohort_start_date, cohort_end_date
FROM gr2457.cohort_re cohort
INNER JOIN (
    SELECT person_id, measurement_date
    FROM gr2457.meas_re
    WHERE measurement_date >= bis_date
    GROUP BY person_id, measurement_date
    ) measurement
    ON cohort.person_id = measurement.person_id
        AND cohort.cohort_start_date = measurement.measurement_date;