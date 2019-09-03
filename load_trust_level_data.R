library("tidyverse")
library("lubridate")

working_dir = getwd()
setwd("data/csv")
inpatient = read_csv("inpatient_data.csv")
inpatient = inpatient %>% mutate(ORG_CODE = str_remove(ORG_CODE, "-X"))
hee_region = read_csv("hee_region.csv")
medical_staff = read_csv("medical_staff.csv")
non_medical_staff = read_csv("non_medical_staff.csv")
provider = read_csv("nhs_provider.csv")
pay_scale = read_csv("agenda for change pay scales.csv")
main_staff_group = read_csv("main_staff_group.csv")
staff_group_1 = read_csv("staff_group_1.csv")
staff_group_2 = read_csv("staff_group_2.csv")
cqc_rating = read_csv("CQC_ratings.csv")
cqc_rating_lookup = read_csv("CQC_ratings_lookup.csv")
nhs_ss = read_csv("NHS_SS_management.csv")
nhs_ss_questions = read_csv("NHS_SS_questions.csv")
ae_target = read_csv("AE.csv")
rtt = read_csv("RTT.csv")
financial_position = read_csv("financial_positions.csv")
shmi = read_csv("shmi.csv")
beds = read_csv("beds.csv")
managers_simple = read_csv("managers_simple_def.csv")
consultants_simple = read_csv("consultants_simple_def.csv")
all_staff_simple = read_csv("all_staff_simple_def.csv")
esr_simple = inner_join(managers_simple, consultants_simple) %>% 
  inner_join(provider %>% filter(ORG_TYPE=="Acute") %>% select(ORG_CODE)) %>%
  left_join(all_staff_simple)

# extract financial years from CQC scores
cqc_rating = cqc_rating %>% mutate(DATE = dmy(DATE),
                      YEAR = year(DATE %m-% months(3)),
                      DATE = as.character(DATE))

# calculate total score per question by provider
# and convert data to long 'tidy' format
nhs_ss = nhs_ss %>%
  gather(key="QUESTION", value="VALUE", -c(1:2)) %>% 
  separate(QUESTION, c("QUESTION","SCORE")) %>%
  spread(SCORE,VALUE) %>%
  mutate(TOTAL_SCORE=(SD*1+D*2+NAND*3+A*4+SA*5)/100) %>%
  gather(SCORE,VALUE, -c(1:3))

# calculate overall provider management score as average 
# of total scores across each question
nhs_ss = nhs_ss %>% filter(SCORE=="TOTAL_SCORE") %>% 
  group_by(ORG_CODE,YEAR) %>%
  summarise(TOTAL_SCORE=mean(VALUE, na.rm=TRUE)) %>%
  gather(SCORE, VALUE, -c(1:2)) %>%
  mutate(QUESTION="overall") %>%
  ungroup() %>%
  select(c(ORG_CODE, YEAR, QUESTION, SCORE, VALUE)) %>%
  bind_rows(nhs_ss)

library("RSQLite")
db = dbConnect(SQLite(), dbname="../NHS_management.sqlite3")
dbWriteTable(conn = db, name = "inpatient_data", inpatient, overwrite=TRUE)
dbWriteTable(conn = db, name = "hee_region", hee_region, overwrite=TRUE)
dbWriteTable(conn = db, name = "medical_staff", medical_staff, overwrite=TRUE)
dbWriteTable(conn = db, name = "non_medical_staff", non_medical_staff, overwrite=TRUE)
dbWriteTable(conn = db, name = "provider", provider, overwrite=TRUE)
dbWriteTable(conn = db, name = "pay_scale", pay_scale, overwrite=TRUE)
dbWriteTable(conn = db, name = "main_staff_group", main_staff_group, overwrite=TRUE)
dbWriteTable(conn = db, name = "staff_group_1", staff_group_1, overwrite=TRUE)
dbWriteTable(conn = db, name = "staff_group_2", staff_group_2, overwrite=TRUE)
dbWriteTable(conn = db, name = "cqc_rating", cqc_rating, overwrite=TRUE)
dbWriteTable(conn = db, name = "cqc_rating_lookup", cqc_rating_lookup, overwrite=TRUE)
dbWriteTable(conn = db, name = "nhs_ss_management_score", nhs_ss, overwrite=TRUE)
dbWriteTable(conn = db, name = "nhs_ss_questions", nhs_ss_questions, overwrite=TRUE)
dbWriteTable(conn = db, name = "ae_target", ae_target, overwrite=TRUE)
dbWriteTable(conn = db, name = "rtt_target", rtt, overwrite=TRUE)
dbWriteTable(conn = db, name = "financial_position", financial_position, overwrite=TRUE)
dbWriteTable(conn = db, name = "shmi", shmi, overwrite=TRUE)
dbWriteTable(conn = db, name = "beds", beds, overwrite=TRUE)
dbWriteTable(conn = db, name = "esr_simple", esr_simple, overwrite=TRUE)

dbDisconnect(db)

setwd(working_dir)


