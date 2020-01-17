SELECT person_id, MAX_VISIT_END as start_date, MAX_VISIT_END as end_date
INTO #final_cohort
FROM (SELECT person_id, MIN(VISIT_START_DATE) AS MIN_VISIT_START, MAX(VISIT_END_DATE) AS MAX_VISIT_END, 
      LAST_VISIT_START_DATE = (SELECT MAX(VISIT_START_DATE) FROM @cdm_database_schema.VISIT_OCCURRENCE)
      FROM @cdm_database_schema.VISIT_OCCURRENCE
      GROUP BY person_id) X
WHERE MAX_VISIT_END <= DATEADD(YEAR, -1, LAST_VISIT_START_DATE) AND DATEDIFF(DAY,MIN_VISIT_START,MAX_VISIT_END)>=365;


DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM #final_cohort CO
;

TRUNCATE TABLE #final_cohort;
DROP TABLE #final_cohort;