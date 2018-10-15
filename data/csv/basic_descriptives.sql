CREATE VIEW descriptives AS
SELECT 
p.org_code AS org_code, 
(managers/total_episodes) * 1000 AS management_quantity, 
management_quality,
cqc_rating,
ae_score
FROM
(SELECT org_code FROM provider WHERE UPPER(org_type)='ACUTE') p
INNER JOIN
(SELECT org_code, SUM(FTE) AS managers FROM non_medical_staff WHERE UPPER(level) LIKE '%MANAGER%' GROUP BY ORG_CODE) mq
ON p.org_code = mq.org_code
LEFT JOIN 
(SELECT total_episodes, org_code FROM inpatient_data WHERE year="2017") e
ON p.org_code = e.org_code
LEFT JOIN
(SELECT org_code, value AS management_quality FROM nhs_ss_management_score WHERE UPPER(question)="OVERALL") ss
ON p.org_code = ss.org_code
LEFT JOIN
(SELECT org_code, rating_score AS cqc_rating FROM cqc_rating WHERE UPPER(population)="OVERALL" AND UPPER(question)="OVERALL") cqc
ON p.org_code = cqc.org_code
LEFT JOIN
(SELECT org_code, ae_score FROM ae_target) ae
ON p.org_code = ae.org_code;
