library("tidyverse")
library("RSQLite")
library("dbplyr")

db_file = "/Users/asariam/OneDrive - London School of Economics/NHS management/data/csv/NHS_management.sqlite3"
con = dbConnect(SQLite(), dbname=db_file)

tbl(con, "descriptives")
dbListTables(con)

# summarise descriptives of different definitions of manager
non_medics = tbl(con, "non_medical_staff") %>% 
  filter(upper(LEVEL) %like% "%MANAGER%") %>%
  inner_join(tbl(con, "main_staff_group")) %>%
  inner_join(tbl(con, "staff_group_1"))  %>% 
  inner_join(tbl(con, "staff_group_2")) %>%
  select(ORG_CODE, MAIN_STAFF_GROUP_NAME, STAFF_GROUP_1_NAME, STAFF_GROUP_2_NAME, AREA, LEVEL, AFC_BAND, FTE) %>%
  collect()

non_medics %>% summarise(sum(FTE))

afc_pay = tbl(con, "pay_scale") %>% 
  collect() %>%
  bind_rows(data_frame(YEAR=2017,AFC_BAND="Very Senior Manager",BOTTOM=142500,AVERAGE=142500,TOP=142500)) 

subtotals = non_medics %>% 
  group_by(ORG_CODE, MAIN_STAFF_GROUP_NAME, STAFF_GROUP_1_NAME, STAFF_GROUP_2_NAME, AREA, LEVEL, AFC_BAND) %>%
  filter(grepl("Band 8", AFC_BAND) | grepl("Band 9", AFC_BAND) | grepl("Very", AFC_BAND)) %>%
  summarise(FTE = sum(FTE)) %>%
  left_join(afc_pay %>% select(AFC_BAND,PAY=TOP)) %>%
  mutate(MANAGEMENT_SPEND=round(FTE*PAY)) %>%
  ungroup()

subtotals %>% group_by(AFC_BAND) %>% summarise(sum(FTE))

medics = tbl(con, "medical_staff") %>% collect()

# provider information and performance
providers = tbl(con, "provider") %>%
  left_join(tbl(con, "ae_target") %>% select(ORG_CODE,AE_SCORE)) %>%
  left_join(tbl(con, "cqc_rating") %>% filter(POPULATION=="Overall" & QUESTION=="Overall") %>% select(ORG_CODE,RATING_SCORE)) %>%
  left_join(tbl(con, "cqc_rating_lookup"), by = c("RATING_SCORE"="SCORE")) %>%
  left_join(tbl(con, "nhs_ss_management_score") %>% filter(YEAR==2017 & QUESTION=="overall") %>% mutate(MANAGEMENT_QUALITY=((VALUE-1)/4)*100) %>% select(ORG_CODE, MANAGEMENT_QUALITY)) %>%
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2016) %>% select(ORG_CODE, TOTAL_EPISODES_2016 = TOTAL_EPISODES)) %>%
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2017) %>% select(ORG_CODE, TOTAL_EPISODES_2017 = TOTAL_EPISODES)) %>%
  left_join(tbl(con, "hee_region")) %>%
  collect() %>%
  left_join(subtotals %>% group_by(ORG_CODE) %>% summarise(MANAGEMENT_SPEND=sum(MANAGEMENT_SPEND), MANAGEMENT_FTE=sum(FTE))) %>%
  mutate(MAN_SPEND_PER_1000_FCE = MANAGEMENT_SPEND*1000/TOTAL_EPISODES_2016, 
         MAN_FTE_PER_1000_FCE = MANAGEMENT_FTE*1000/TOTAL_EPISODES_2016,
         RATING = factor(RATING_SCORE,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding")),
         QUALITY_WEIGHTED_SPEND = MAN_SPEND_PER_1000_FCE*MANAGEMENT_QUALITY/100)

acute_providers = providers %>% filter(ORG_TYPE == "Acute" & MAN_FTE_PER_1000_FCE<4)

# plot some basic correlations

fce_vs_fte = ggplot(data=acute_providers) +
  geom_point(aes(TOTAL_EPISODES_2016, MANAGEMENT_FTE))

fce_vs_spend = ggplot(data=acute_providers) +
  geom_point(aes(TOTAL_EPISODES_2016, MANAGEMENT_SPEND))

fte_vs_nss = ggplot(data=acute_providers,
                    aes(MAN_FTE_PER_1000_FCE, MANAGEMENT_QUALITY)) +
  geom_point(aes(size=TOTAL_EPISODES_2016)) +
  geom_smooth(method = "lm", aes(weight=TOTAL_EPISODES_2016), se=FALSE) +
  geom_text(aes(label=ifelse(MAN_FTE_PER_1000_FCE>2,ORG_CODE,'')),hjust=1,vjust=0) +
  ylab("Overall Management Score (NHS Staff Survey)") +
  xlab("Managers per 1000 FCE (FTE)") +
  facet_wrap(RATING~.)

fte_vs_ae = ggplot(data=acute_providers,
                   aes(MAN_FTE_PER_1000_FCE,AE_SCORE)) +
  geom_point(aes(size=TOTAL_EPISODES_2016)) +
  geom_text(aes(label=ifelse(MAN_FTE_PER_1000_FCE>2,ORG_CODE,'')),hjust=1,vjust=0) +
  geom_smooth(method = "lm", aes(weight=TOTAL_EPISODES_2016), se=FALSE) +
  ylab("A&E 4 hour target met (%)") +
  xlab("Managers per 1000 FCE (FTE)") 

qual_vs_ae = ggplot(data=acute_providers,
                   aes(MANAGEMENT_QUALITY,AE_SCORE)) +
  geom_point(aes(size=TOTAL_EPISODES_2016)) +
  geom_smooth(method = "lm", aes(weight=TOTAL_EPISODES_2016), se=FALSE) +
  ylab("A&E 4 hour target met (%)") +
  xlab("Overall Management Score (NHS Staff Survey)") 

spend_vs_ae = ggplot(data=acute_providers,
                   aes(MAN_SPEND_PER_1000_FCE,AE_SCORE)) +
  geom_point(aes(size=TOTAL_EPISODES_2016)) +
  geom_text(aes(label=ifelse(MAN_FTE_PER_1000_FCE>2,ORG_CODE,'')),hjust=1,vjust=0) +
  geom_smooth(method = "lm", aes(weight=TOTAL_EPISODES_2016), se=FALSE) +
  ylab("A&E 4 hour target met (%)") +
  xlab("Spend on managers per 1000 FCE (£)")

qs_vs_ae = ggplot(data=acute_providers,
                     aes(QUALITY_WEIGHTED_SPEND,AE_SCORE)) +
  geom_point(aes(size=TOTAL_EPISODES_2016)) +
  geom_text(aes(label=ifelse(MAN_FTE_PER_1000_FCE>2,ORG_CODE,'')),hjust=1,vjust=0) +
  geom_smooth(method = "lm", aes(weight=TOTAL_EPISODES_2016), se=FALSE) +
  ylab("A&E 4 hour target met (%)") +
  xlab("Quality weighted spend on mangers per 1000 FCE (£)")

db_list_tables(con)

dbDisconnect(db)