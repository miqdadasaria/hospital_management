library("tidyverse")

working_dir = getwd()
setwd("data/csv")

acute_trusts = read_csv("raw_data_2017.csv")

# add quintiles
acute_trusts = acute_trusts %>% filter(SPECIALIST=="Non-specialist") %>%
  mutate(ADM_TERTILE = ntile(ADMISSIONS,3),
    OC_TERTILE = ntile(OPERATING_COST,3),
    STAFF_TERTILE = ntile(ALL_STAFF,3),
    MAN_TERTILE = ntile(MANAGERS,3),
    MAN_PERC_TERTILE = ntile(MANAGERS_PERCENT,3),
    SS_TERTILE = ntile(NHS_SS, 3)) %>%
  select(ORG_CODE, ORG_NAME, 
         OPERATING_COST, ADMISSIONS, ALL_STAFF, MANAGERS, MANAGERS_PERCENT, 
         CQC_RATING, CQC_WELL_LED_RATING, NHS_SS,
         NET_FINANCIAL_POSITION_PERCENT, RTT_SCORE, AE_SCORE, SHMI, 
         ADM_TERTILE, OC_TERTILE, STAFF_TERTILE, MAN_TERTILE, MAN_PERC_TERTILE, SS_TERTILE)

most_managed = acute_trusts %>% group_by(ADM_TERTILE) %>% top_n(3,MANAGERS) %>% arrange(ADMISSIONS) %>% ungroup()

least_managed = acute_trusts %>% group_by(ADM_TERTILE) %>% top_n(-3,MANAGERS) %>% arrange(ADMISSIONS) %>% ungroup()
 
case_studies_man= bind_rows(most_managed,least_managed) %>% arrange(ADM_TERTILE,MANAGERS)

write_csv(case_studies_man, "case_studies_man.csv")

most_adm = acute_trusts %>% group_by(MAN_TERTILE) %>% top_n(3,ADMISSIONS) %>% arrange(MANAGERS) %>% ungroup()

least_adm = acute_trusts %>% group_by(MAN_TERTILE) %>% top_n(-3,ADMISSIONS) %>% arrange(MANAGERS) %>% ungroup()

case_studies_adm = bind_rows(most_adm,least_adm) %>% arrange(MAN_TERTILE,ADMISSIONS)

write_csv(case_studies_adm, "case_studies_adm.csv")

setwd(working_dir)