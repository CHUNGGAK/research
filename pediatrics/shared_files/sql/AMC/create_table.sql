/* 소아청소년과 추출항목 (2019-04-09) For MS-SQL(SQL Server 2012 이상)
   - 추출기준일(8개) : 진단일, 3개월, 6개월, 12개월, 24개월, 36개월, 48개월, 60개월
     : 6개월 까지는 전후 1.5개월(45일)을, 12개월 이후 시점부터는 전후 3개월 데이터를 기준으로 추출
     : Index date~45, 46~135, 136~227, 273~455, 638~820, 1003~1185, 1368~1550, 1733~1915
   - DAY_OF_BIRTH 필드 제외
   - 측정데이터 원천 다중화 (원천변경은 IN_PARAM.MEASUREMENT_SOURCE의 값만 변경하면 됨)
     : 키/체중/BMI : MEASUREMENT OR OBSERVATION
     : HbA1c : MEASUREMENT
   - COHORT_ID가 변경되면 IN_PARAM.COHORT_ID의 값을 변경
   - 당뇨병 종류(Type1, Type2) 추가
   - 신환환자만 선택
     1) 0, 3개월 사이의 HbA1c가 1이상 감소한 경우
     2) 0개월 근처(진단시점 INDEX_DATE ~ +3개월 이내) GAD Ab 검사가 최소 1번 있는 경우
     3) 0, 3개월 사이의 Weight가 증가한 경우
   - 해당 환자의 키/몸무게 데이터 추출 시 이상치 제거
     1) Weight : "환자별 0M Weight(Median) * 80%" ~ "환자별 마지막 시점의 Weight(Median) * 120%"
     2) Height : "환자별 0M Height(Median) * 97%" ~ "환자별 마지막 시점의 Height(Median) * 103%"

   - 2019-05-08 수정
     1) DM type 구분하는 CONDITION_DATA_DM에서 5년 기간 제거
     2) DKA는 진단&검사로 수정
     3) DKA 진단기준을 CONCEPT_ID에서 EXT_COND_SOURCE_VALUE_KCD (ICD10)로 변경
     4) 키, 몸무게, BMI와 HbA1c, GAD Ab 검사를 구분하였음
     5) SNU용 DKA 검사에서 코드 2개 수정

   - 2019-06-19 AMC 버전 수정
     1) MEASUREMENT_SOURCE 테이블을 지정하였음 (Observation : 키,몸무게,BMI / Measurement : HbA1c,GAD Ab)
     2) ICD 코드는 CONDITION_SOURCE_VALUE에서 추출하는 것으로 변경
     3) 최종 Select 구문에서 Date field를 char로 type 변경

   - 2019-09-18 무거운 쿼리조각을 임시테이블로 생성하여 처리하도록 수정
     1. 임시테이블 CONDITION_DATA 생성
     2. 임시테이블 CONDITION_DATA_NEW 생성
     3. 임시테이블 CONDITION_DATA_DM 생성
     4. 임시테이블 MEASUREMENT_DATA 생성
     5. 임시테이블 MEASUREMENT_DKA_DATA 생성
     6. 임시테이블 MEASUREMENT_DKA_DATA_NEW 생성
     7. 임시테이블 NORMAL_RANGE 생성
     8. 임시테이블 NORMAL_MEASUREMENT 생성
     9. 메인쿼리 수행
*/


/* 1. 임시테이블 CONDITION_DATA 생성 */
CREATE TABLE @CDM_DB_SCHEMA.CONDITION_DATA (
    PERSON_ID NUMERIC(18) NOT NULL
  , DKA_YN    INTEGER     NOT NULL
  , HHS_YN    INTEGER     NOT NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
          , A1.COHORT_END_DATE
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.CONDITION_DATA
SELECT B1.PERSON_ID
     , MAX(CASE WHEN B2.CONDITION_CONCEPT_ID =  443727 THEN 1 ELSE 0 END) AS DKA_YN
     , MAX(CASE WHEN B2.CONDITION_CONCEPT_ID = 4147719 THEN 1 ELSE 0 END) AS HHS_YN
  FROM COHORT_DATA B1
     , @CDM_DB_SCHEMA.CONDITION_OCCURRENCE B2
 WHERE B1.PERSON_ID = B2.PERSON_ID
   AND B2.CONDITION_START_DATE BETWEEN B1.COHORT_START_DATE AND B1.COHORT_END_DATE
   AND B2.CONDITION_CONCEPT_ID IN (443727, 4147719) /* 443727(DKA여부), 4147719(HHS여부) */
 GROUP BY B1.PERSON_ID
;

COMMIT;


/* 2. 임시테이블 CONDITION_DATA_NEW 생성 */
CREATE TABLE @CDM_DB_SCHEMA.CONDITION_DATA_NEW (
    PERSON_ID NUMERIC(18) NOT NULL
  , DKA_YN    INTEGER     NOT NULL
  , HHS_YN    INTEGER     NOT NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
          , A1.COHORT_END_DATE
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.CONDITION_DATA_NEW
SELECT B1.PERSON_ID
     --, MAX(DECODE(B2.CONDITION_CONCEPT_ID,  443727, 1, 0)) AS DKA_YN
     , MAX(CASE WHEN SUBSTRING(REPLACE(B2.CONDITION_SOURCE_VALUE, '.', ''), 1, 4) IN ('E101','E111','E140','E141') THEN 1 ELSE 0 END) AS DKA_YN
     --, MAX(DECODE(B2.CONDITION_CONCEPT_ID, 4147719, 1, 0)) AS HHS_YN
     , MAX(CASE WHEN B2.CONDITION_CONCEPT_ID = 4147719 THEN 1 ELSE 0 END) AS HHS_YN
  FROM COHORT_DATA B1
     , @CDM_DB_SCHEMA.CONDITION_OCCURRENCE B2
 WHERE B1.PERSON_ID = B2.PERSON_ID
   AND B2.CONDITION_START_DATE BETWEEN DATEADD(DAY, -30, B1.COHORT_START_DATE) AND DATEADD(DAY, 7, B1.COHORT_START_DATE)
   AND (B2.CONDITION_CONCEPT_ID IN (4147719)
    OR SUBSTRING(REPLACE(B2.CONDITION_SOURCE_VALUE, '.', ''), 1, 4) IN ('E101','E111','E140','E141')) /* 443727(DKA여부), 4147719(HHS여부) */
 GROUP BY B1.PERSON_ID
;

COMMIT;


/* 3. 임시테이블 CONDITION_DATA_DM 생성 */
CREATE TABLE @CDM_DB_SCHEMA.CONDITION_DATA_DM (
    PERSON_ID NUMERIC(18) NOT NULL
  , DM1       INTEGER     NOT NULL
  , DM2       INTEGER     NOT NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
          , A1.COHORT_END_DATE
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.CONDITION_DATA_DM
SELECT C1.PERSON_ID
     , CASE WHEN C1.LAST_CONDITION_CONCEPT_ID IN (SELECT DESCENDANT_CONCEPT_ID FROM @VOCABULARY_DB_SCHEMA.CONCEPT_ANCESTOR WHERE ANCESTOR_CONCEPT_ID = 201254) THEN 1 ELSE 0 END AS DM1 /* 제1형 당뇨병 존재여부 */
     , CASE WHEN C1.LAST_CONDITION_CONCEPT_ID IN (SELECT DESCENDANT_CONCEPT_ID FROM @VOCABULARY_DB_SCHEMA.CONCEPT_ANCESTOR WHERE ANCESTOR_CONCEPT_ID = 201826) THEN 1 ELSE 0 END AS DM2 /* 제2형 당뇨병 존재여부 */
  FROM (SELECT B1.PERSON_ID
             , SUBSTRING(MAX(CONCAT(CONVERT(VARCHAR(8), B2.CONDITION_START_DATE, 112), B2.CONDITION_CONCEPT_ID)), 9, 20) AS LAST_CONDITION_CONCEPT_ID /* 가장 마지막 진단의 CONDITION_CONCEPT_ID */
          FROM COHORT_DATA B1
             , @CDM_DB_SCHEMA.CONDITION_OCCURRENCE B2
         WHERE B1.PERSON_ID = B2.PERSON_ID
           --AND B2.CONDITION_START_DATE BETWEEN B1.COHORT_START_DATE AND B1.COHORT_START_DATE_60M
           AND B2.CONDITION_CONCEPT_ID IN (SELECT DESCENDANT_CONCEPT_ID
                                             FROM @VOCABULARY_DB_SCHEMA.CONCEPT_ANCESTOR
                                            WHERE ANCESTOR_CONCEPT_ID IN (201254, 201826) /* 201254(제1형 당뇨병 상위 CONCEPT_ID), 201826(제2형 당뇨병 상위 CONCEPT_ID) */
                                          )
         GROUP BY B1.PERSON_ID
       ) C1
;

COMMIT;


/* 4. 임시테이블 MEASUREMENT_DATA 생성 */
CREATE TABLE @CDM_DB_SCHEMA.MEASUREMENT_DATA (
    PERSON_ID              NUMERIC(18) NOT NULL
  , MEASUREMENT_CONCEPT_ID NUMERIC(18) NOT NULL
  , MEASUREMENT_DATE       DATETIME    NULL
  , VALUE_AS_NUMBER        FLOAT(22)   NULL
  , MEASUREMENT_SOURCE     VARCHAR(20) NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.MEASUREMENT_DATA
SELECT C1.PERSON_ID
     , C2.OBSERVATION_CONCEPT_ID AS MEASUREMENT_CONCEPT_ID
     , C2.OBSERVATION_DATE AS MEASUREMENT_DATE
     , C2.VALUE_AS_NUMBER
     , 'OBSERVATION' AS MEASUREMENT_SOURCE
  FROM COHORT_DATA C1
     , @CDM_DB_SCHEMA.OBSERVATION C2
     , IN_PARAM Z1
 WHERE C1.PERSON_ID = C2.PERSON_ID
   AND C2.OBSERVATION_CONCEPT_ID IN (Z1.CONCEPT_ID_HEIGHT, Z1.CONCEPT_ID_WEIGHT, Z1.CONCEPT_ID_BMI) --, Z1.CONCEPT_ID_GADAB) -- Height and Weight and BMI
 UNION ALL
SELECT C1.PERSON_ID
     , C2.MEASUREMENT_CONCEPT_ID
     , C2.MEASUREMENT_DATE
     , C2.VALUE_AS_NUMBER
     , 'MEASUREMENT' AS MEASUREMENT_SOURCE
  FROM COHORT_DATA C1
     , @CDM_DB_SCHEMA.MEASUREMENT C2
     , IN_PARAM Z1
 WHERE C1.PERSON_ID = C2.PERSON_ID
   AND C2.MEASUREMENT_CONCEPT_ID IN (Z1.CONCEPT_ID_HBA1C, Z1.CONCEPT_ID_GADAB) --,Z1.CONCEPT_ID_HEIGHT, Z1.CONCEPT_ID_WEIGHT, Z1.CONCEPT_ID_BMI)-- HbA1c and GAD Ab
;

COMMIT;


/* 5. 임시테이블 MEASUREMENT_DKA_DATA 생성 */
CREATE TABLE @CDM_DB_SCHEMA.MEASUREMENT_DKA_DATA (
    PERSON_ID NUMERIC(18) NOT NULL
  , DKA_YN    VARCHAR(1)  NOT NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
   , MEASUREMENT_DKA_BASE_DATA AS ( -- INDEX_DATE 기준 3개월 이내 발생한 검사결과들로 비교 (20190207, KS)
     SELECT A1.PERSON_ID, A1.MEASUREMENT_CONCEPT_ID, A1.VALUE_AS_NUMBER, A1.VALUE_AS_CONCEPT_ID
       FROM COHORT_DATA C1
          , @CDM_DB_SCHEMA.MEASUREMENT A1
      WHERE C1.PERSON_ID = A1.PERSON_ID
        AND A1.MEASUREMENT_DATE BETWEEN DATEADD(DAY, -90, C1.COHORT_START_DATE) AND C1.COHORT_START_DATE
     )
INSERT INTO @CDM_DB_SCHEMA.MEASUREMENT_DKA_DATA
SELECT D1.PERSON_ID
     , CASE WHEN COUNT(D2.MEASUREMENT_CONCEPT_ID) =  0 THEN 'N' ELSE 'Y' END AS DKA_YN
  FROM COHORT_DATA D1
 INNER JOIN MEASUREMENT_DKA_BASE_DATA D2 ON D1.PERSON_ID = D2.PERSON_ID
 INNER JOIN MEASUREMENT_DKA_BASE_DATA D3 ON D2.PERSON_ID = D3.PERSON_ID
 WHERE ((D2.MEASUREMENT_CONCEPT_ID IN ('3019977', '3012544')            AND D2.VALUE_AS_NUMBER < 7.3) /* pH < 7.3 */
     OR (D2.MEASUREMENT_CONCEPT_ID IN ('3008152', '3027273', '3015235') AND D2.VALUE_AS_NUMBER < 15)  /* HCO3- < 15 */
       )
   AND ((D3.MEASUREMENT_CONCEPT_ID IN ('3035350')  AND D3.VALUE_AS_CONCEPT_ID IN (4126673,4125547,4126674)) /* urinalysis 에서 ketone 양성(2+이상) : 3028893 에서 바꿈*/
     OR (D3.MEASUREMENT_CONCEPT_ID IN ('3046325')  AND D3.VALUE_AS_NUMBER >= 180)                           /* 혈액 검사에서 ketone 양성(Beta hydroxybutyric acid) : 3003985 에서 바꿈 */
     OR (D3.MEASUREMENT_CONCEPT_ID IN ('40769111') AND D3.VALUE_AS_NUMBER >= 3)                             /* 혈액 검사에서 ketone 양성(Ketone (Beta hydroxybutyate)) */
     -- 20190208 회의에서 아래 결과는 제외해서 분석하기로 논의
     --OR (D3.MEASUREMENT_CONCEPT_ID IN ('3035132')  AND VALUE_AS_CONCEPT_ID IN (4123508,4126673,4125547,4126674)) /* 혈액 검사에서 ketone 양성(Ketone bodies (em) 1+ 이상) */
       )
 GROUP BY D1.PERSON_ID
;

COMMIT;


/* 6. 임시테이블 MEASUREMENT_DKA_DATA_NEW 생성 */
CREATE TABLE @CDM_DB_SCHEMA.MEASUREMENT_DKA_DATA_NEW (
    PERSON_ID NUMERIC(18) NOT NULL
  , DKA_TEST1 VARCHAR(1)  NULL
  , DKA_TEST2 VARCHAR(1)  NULL
  , DKA_TEST3 VARCHAR(1)  NULL
  , DKA_TEST4 VARCHAR(1)  NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.MEASUREMENT_DKA_DATA_NEW
SELECT F1.PERSON_ID /* 2019-04-07 : (COHORT_START_DATE - 30 DAYS) ~ (COHORT_START_DATE + 7 DAYS) 기간 중 COHORT_START_DATE에 가장 가까운 검사결과 값 사용 */
     , MAX(CASE WHEN  F1.MEASUREMENT_CONCEPT_ID IN (3019977, 3012544)          AND F1.VALUE_AS_NUMBER < 7.3 THEN 'Y' END) AS DKA_TEST1
     , MAX(CASE WHEN  F1.MEASUREMENT_CONCEPT_ID IN (3008152, 3027273, 3015235) AND F1.VALUE_AS_NUMBER < 15  THEN 'Y' END) AS DKA_TEST2
     , MAX(CASE WHEN  F1.MEASUREMENT_CONCEPT_ID IN (3035350) AND F1.VALUE_AS_CONCEPT_ID IN (4126673, 4125547, 4126674) THEN 'Y' END) AS DKA_TEST3
     , MAX(CASE WHEN (F1.MEASUREMENT_CONCEPT_ID IN (3046325) AND F1.VALUE_AS_NUMBER >= 180) OR (F1.MEASUREMENT_CONCEPT_ID IN (40769111) AND F1.VALUE_AS_NUMBER >= 3) THEN 'Y' END) AS DKA_TEST4
  FROM (SELECT E2.PERSON_ID, E2.MEASUREMENT_CONCEPT_ID, E2.VALUE_AS_NUMBER, E2.VALUE_AS_CONCEPT_ID, E2.MEASUREMENT_DATE, E1.COHORT_START_DATE
             , ROW_NUMBER() OVER (PARTITION BY E2.PERSON_ID, E2.MEASUREMENT_CONCEPT_ID ORDER BY ABS(DATEDIFF(day, E1.COHORT_START_DATE, E2.MEASUREMENT_DATE))) AS RN
          FROM COHORT_DATA E1
             , @CDM_DB_SCHEMA.MEASUREMENT E2
         WHERE E1.PERSON_ID = E2.PERSON_ID
           AND E2.MEASUREMENT_DATE BETWEEN DATEADD(DAY, -30, E1.COHORT_START_DATE) AND DATEADD(DAY, 7, E1.COHORT_START_DATE)
           AND E2.MEASUREMENT_CONCEPT_ID IN (3019977, 3012544, 3008152, 3027273, 3015235, 3035350, 3046325, 40769111)
       ) F1
 WHERE F1.RN = 1
 GROUP BY F1.PERSON_ID
;

COMMIT;


/* 7. 임시테이블 NORMAL_RANGE 생성 */
CREATE TABLE @CDM_DB_SCHEMA.NORMAL_RANGE (
    PERSON_ID NUMERIC(18) NOT NULL
  , HEIGHT_FR FLOAT(22)   NOT NULL
  , HEIGHT_TO FLOAT(22)   NOT NULL
  , WEIGHT_FR FLOAT(22)   NOT NULL
  , WEIGHT_TO FLOAT(22)   NOT NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
          , 0.97    AS HEIGHT_FR_RT /* HEIGHT 최저 가중치 */
          , 1.03    AS HEIGHT_TO_RT /* HEIGHT 최고 가중치 */
          , 0.8     AS WEIGHT_FR_RT /* WEIGHT 최저 가중치 */
          , 1.2     AS WEIGHT_TO_RT /* WEIGHT 최고 가중치 */
     )
   , COHORT_DATA AS (
     SELECT Z1.COHORT_ID
          , A1.SUBJECT_ID AS PERSON_ID
          , A1.COHORT_START_DATE
          , A1.COHORT_END_DATE
          , DATEADD(DAY, -45, A1.COHORT_START_DATE) AS COHORT_START_DATE_00M_BF
          , DATEADD(DAY,  45, A1.COHORT_START_DATE) AS COHORT_START_DATE_00M_AF
          , DATEADD(DAY,  46, A1.COHORT_START_DATE) AS COHORT_START_DATE_03M_BF
          , DATEADD(DAY, 135, A1.COHORT_START_DATE) AS COHORT_START_DATE_03M_AF
          , DATEADD(DAY, 136, A1.COHORT_START_DATE) AS COHORT_START_DATE_06M_BF
          , DATEADD(DAY, 227, A1.COHORT_START_DATE) AS COHORT_START_DATE_06M_AF
          , DATEADD(month, 9, A1.COHORT_START_DATE) AS COHORT_START_DATE_12M_BF
          , DATEADD(month,15, A1.COHORT_START_DATE) AS COHORT_START_DATE_12M_AF
          , DATEADD(month,21, A1.COHORT_START_DATE) AS COHORT_START_DATE_24M_BF
          , DATEADD(month,27, A1.COHORT_START_DATE) AS COHORT_START_DATE_24M_AF
          , DATEADD(month,33, A1.COHORT_START_DATE) AS COHORT_START_DATE_36M_BF
          , DATEADD(month,39, A1.COHORT_START_DATE) AS COHORT_START_DATE_36M_AF
          , DATEADD(month,45, A1.COHORT_START_DATE) AS COHORT_START_DATE_48M_BF
          , DATEADD(month,51, A1.COHORT_START_DATE) AS COHORT_START_DATE_48M_AF
          , DATEADD(month,57, A1.COHORT_START_DATE) AS COHORT_START_DATE_60M_BF
          , DATEADD(month,63, A1.COHORT_START_DATE) AS COHORT_START_DATE_60M_AF
       FROM @RESULT_DB_SCHEMA.COHORT A1
          , IN_PARAM Z1
      WHERE A1.COHORT_DEFINITION_ID = Z1.COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.NORMAL_RANGE
SELECT T1.PERSON_ID
     , ROUND(ISNULL(T1.HEIGHT_00M_MED,0) * Z1.HEIGHT_FR_RT, 2) AS HEIGHT_FR
     , ROUND(ISNULL(COALESCE(T1.HEIGHT_60M_MED, T1.HEIGHT_48M_MED, T1.HEIGHT_36M_MED, T1.HEIGHT_24M_MED, T1.HEIGHT_12M_MED, T1.HEIGHT_06M_MED, T1.HEIGHT_03M_MED, T1.HEIGHT_00M_MED),0) * Z1.HEIGHT_TO_RT, 2) AS HEIGHT_TO
     , ROUND(ISNULL(T1.WEIGHT_00M_MED,0) * Z1.WEIGHT_FR_RT, 2) AS WEIGHT_FR
     , ROUND(ISNULL(COALESCE(T1.WEIGHT_60M_MED, T1.WEIGHT_48M_MED, T1.WEIGHT_36M_MED, T1.WEIGHT_24M_MED, T1.WEIGHT_12M_MED, T1.WEIGHT_06M_MED, T1.WEIGHT_03M_MED, T1.WEIGHT_00M_MED),0) * Z1.WEIGHT_TO_RT, 2) AS WEIGHT_TO
  FROM (SELECT DISTINCT S1.PERSON_ID
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_00M_BF AND S1.COHORT_START_DATE_00M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_00M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_03M_BF AND S1.COHORT_START_DATE_03M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_03M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_06M_BF AND S1.COHORT_START_DATE_06M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_06M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_12M_BF AND S1.COHORT_START_DATE_12M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_12M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_24M_BF AND S1.COHORT_START_DATE_24M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_24M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_36M_BF AND S1.COHORT_START_DATE_36M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_36M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_48M_BF AND S1.COHORT_START_DATE_48M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_48M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_60M_BF AND S1.COHORT_START_DATE_60M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS HEIGHT_60M_MED

             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_00M_BF AND S1.COHORT_START_DATE_00M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_00M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_03M_BF AND S1.COHORT_START_DATE_03M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_03M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_06M_BF AND S1.COHORT_START_DATE_06M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_06M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_12M_BF AND S1.COHORT_START_DATE_12M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_12M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_24M_BF AND S1.COHORT_START_DATE_24M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_24M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_36M_BF AND S1.COHORT_START_DATE_36M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_36M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_48M_BF AND S1.COHORT_START_DATE_48M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_48M_MED
             , PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY (CASE WHEN S2.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND S2.MEASUREMENT_DATE BETWEEN S1.COHORT_START_DATE_60M_BF AND S1.COHORT_START_DATE_60M_AF THEN S2.VALUE_AS_NUMBER END)) OVER(PARTITION BY S1.PERSON_ID) AS WEIGHT_60M_MED
          FROM COHORT_DATA S1
          LEFT OUTER JOIN @CDM_DB_SCHEMA.MEASUREMENT_DATA S2 ON S1.PERSON_ID = S2.PERSON_ID
         CROSS JOIN IN_PARAM Z1
       ) T1
     , IN_PARAM Z1
;

COMMIT;


/* 8. 임시테이블 NORMAL_MEASUREMENT 생성 */
CREATE TABLE @CDM_DB_SCHEMA.NORMAL_MEASUREMENT (
    PERSON_ID              NUMERIC(18) NOT NULL
  , MEASUREMENT_CONCEPT_ID NUMERIC(18) NOT NULL
  , MEASUREMENT_DATE       DATETIME    NULL
  , VALUE_AS_NUMBER        FLOAT(22)   NULL
);

WITH IN_PARAM AS (
     SELECT 4177340 AS CONCEPT_ID_HEIGHT /* 4177340(Height) */
          , 4099154 AS CONCEPT_ID_WEIGHT /* 4099154(Weight) */
          , 4245997 AS CONCEPT_ID_BMI    /* 4245997(BMI(EMR)) */
          , 3005673 AS CONCEPT_ID_HBA1C  /* 3005673(HbA1c) */
          , 3020148 AS CONCEPT_ID_GADAB  /* 3020148(GAD Ab 검사) */
          , 99999   AS COHORT_ID
     )
INSERT INTO @CDM_DB_SCHEMA.NORMAL_MEASUREMENT
SELECT K1.PERSON_ID
     , K1.MEASUREMENT_CONCEPT_ID
     , K1.MEASUREMENT_DATE
     , K1.VALUE_AS_NUMBER
  FROM @CDM_DB_SCHEMA.MEASUREMENT_DATA K1
     , @CDM_DB_SCHEMA.NORMAL_RANGE K2 /* 해당 환자의 키/몸무게 데이터 추출 시 이상치 제거 */
     , IN_PARAM Z1
 WHERE K1.PERSON_ID = K2.PERSON_ID
   AND ((K1.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_HEIGHT AND K1.VALUE_AS_NUMBER BETWEEN K2.HEIGHT_FR AND K2.HEIGHT_TO)
     OR (K1.MEASUREMENT_CONCEPT_ID = Z1.CONCEPT_ID_WEIGHT AND K1.VALUE_AS_NUMBER BETWEEN K2.WEIGHT_FR AND K2.WEIGHT_TO)
     OR (K1.MEASUREMENT_CONCEPT_ID IN (Z1.CONCEPT_ID_BMI, Z1.CONCEPT_ID_HBA1C, Z1.CONCEPT_ID_GADAB))
       )
;

COMMIT;