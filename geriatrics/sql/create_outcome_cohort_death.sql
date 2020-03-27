/*#######################################################################
  ���κ����� CDM ��������
  - 2020.03.25 / KS
  - PLP �м����� DEATH�� ������¥ �������� TIME AT RISK�� �����ؾ� ��
    TARGET COHORT�� ��������� �������� �Ǿ� ����
	�׷��� TARGET COHORT�� ���� �������ڿ� TIME AT RISK�� �ش�Ǵ� ������� �ٿ��� ��

#######################################################################*/


SELECT * FROM RESULT_CDM_2019.COHORT WHERE COHORT_DEFINITION_ID = 1075;
SELECT COUNT(*), COUNT(DISTINCT SUBJECT_ID) FROM RESULT_CDM_2019.COHORT WHERE COHORT_DEFINITION_ID = 1075; -- 26514	22926

INSERT INTO lcg_cohort
WITH N_DATA AS (
     SELECT C1.SUBJECT_ID
          , V1.VISIT_START_DATE
          , V1.VISIT_END_DATE
          , MIN(P1.PROCEDURE_DATE) AS PROCEDURE_DATE
          , V1.VISIT_END_DATE - MIN(P1.PROCEDURE_DATE) AS GAP_DAYS /* ������ڿ� SHIFT �ؾߵ� DAYS */
     FROM RESULT_CDM_2019.COHORT C1 -- TARGET COHORT
        , cdm_2019_view.VISIT_OCCURRENCE V1 -- �Կ�
        , cdm_2019_view.PROCEDURE_OCCURRENCE P1 -- ����
     WHERE C1.SUBJECT_ID = V1.PERSON_ID
       AND C1.COHORT_START_DATE = V1.VISIT_START_DATE
       AND C1.COHORT_END_DATE = V1.VISIT_END_DATE
       AND V1.VISIT_OCCURRENCE_ID = P1.VISIT_OCCURRENCE_ID
       AND C1.COHORT_DEFINITION_ID = 1075
       AND V1.VISIT_CONCEPT_ID = 9201
       AND P1.PROCEDURE_TYPE_CONCEPT_ID = 38000275
     GROUP BY C1.SUBJECT_ID, V1.VISIT_START_DATE, V1.VISIT_END_DATE
     )

--SELECT COUNT(*), COUNT(DISTINCT SUBJECT_ID) FROM N_DATA -- 26514	22926 : ���� C1�� CNT, PCNT ����
--SELECT * FROM N_DATA WHERE PROCEDURE_DATE IS NULL -- ���� : PROCEDURE_OCCURRENCE�� OUTER���� INNER�� ����
                                                    -- ��, 1�� �Կ��� �������� ������ �߻��Ͽ� ù��° ������¥�� ������ ��

   , F_DATA AS (
     SELECT N1.SUBJECT_ID
          , N1.VISIT_START_DATE
          , N1.VISIT_END_DATE
          , N1.PROCEDURE_DATE
          , N1.GAP_DAYS
          , C2.COHORT_START_DATE AS DEATH_DATE
          , C2.COHORT_START_DATE + N1.GAP_DAYS AS FINAL_DEATH_DATE 
          , ROW_NUMBER() OVER(PARTITION BY N1.SUBJECT_ID ORDER BY N1.VISIT_START_DATE, N1.PROCEDURE_DATE) AS ORDINAL
     FROM N_DATA N1
        , RESULT_CDM_2019.COHORT C2 -- ���
     WHERE N1.SUBJECT_ID = C2.SUBJECT_ID
       AND C2.COHORT_START_DATE BETWEEN N1.PROCEDURE_DATE AND N1.PROCEDURE_DATE + 90
       AND C2.COHORT_DEFINITION_ID = 122
     )

--SELECT COUNT(*), COUNT(DISTINCT SUBJECT_ID) FROM F_DATA -- 537	525 : 1�� ����� 90�� �Ⱓ �������� ������ ���� �� ����
SELECT 4 cohort_definition_id, subject_id, 
    visit_end_date + (death_date - procedure_date) cohort_start_date,
    visit_end_date + (death_date - procedure_date) + 1 cohort_end_date
FROM F_DATA
-- WHERE SUBJECT_ID IN (SELECT SUBJECT_ID FROM F_DATA WHERE ORDINAL > 1)
-- ORDER BY SUBJECT_ID, ORDINAL
;

