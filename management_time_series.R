library("tidyverse")
library("lubridate")
library("RSQLite")
library("scales")

db_file = "data/NHS_management.sqlite3"
con = dbConnect(SQLite(), dbname=db_file)

non_medics = tbl(con, "non_medical_staff") %>% 
  inner_join(tbl(con, "staff_group_1"))  %>% 
  select(DATE, ORG_CODE, STAFF_GROUP = STAFF_GROUP_1_NAME, FTE) %>%
  collect()

medics = tbl(con, "medical_staff") %>% 
  collect()


acute_trusts = tbl(con, "provider") %>%
  collect()

dbDisconnect(con)

acute_trusts = acute_trusts %>% filter(ORG_TYPE=="Acute", SPECIALIST==0) %>% select(ORG_CODE)

esr_2016_2017_2018 = 
  bind_rows(
  non_medics %>% 
  group_by(DATE) %>% 
  summarise(FTE = sum(FTE)) %>%
  mutate(STAFF_GROUP = "NonMed Total"),
  non_medics %>% 
  filter(STAFF_GROUP %in% c("Managers","Senior managers")) %>%
  group_by(DATE,STAFF_GROUP) %>% 
  summarise(FTE = sum(FTE)),
  non_medics %>% 
  filter(STAFF_GROUP %in% c("Managers","Senior managers")) %>%
  group_by(DATE) %>% 
  summarise(FTE = sum(FTE)) %>%
  mutate(STAFF_GROUP = "Manager Total") ,
  medics %>% 
  group_by(DATE) %>% 
  summarise(FTE = sum(FTE)) %>%
  mutate(STAFF_GROUP = "Med Total"),
  medics %>% 
  filter(grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(DATE) %>% 
  summarise(FTE = sum(FTE))%>%
  mutate(STAFF_GROUP = "Consultant")
  ) %>% 
  mutate(STAFF_GROUP = gsub("anagers", "anager",STAFF_GROUP))

esr_2016_2017_2018 = bind_rows(
  esr_2016_2017_2018,
  esr_2016_2017_2018 %>%
    filter(STAFF_GROUP %in% c("NonMed Total", "Med Total")) %>%
    group_by(DATE) %>%
    summarise(FTE = sum(FTE))%>%
    mutate(STAFF_GROUP = "All Staff Total") %>% 
    ungroup
)

esr_2016_2017_2018_acute = 
  bind_rows(
    non_medics %>% 
      inner_join(acute_trusts) %>%
      group_by(DATE) %>% 
      summarise(FTE = sum(FTE)) %>%
      mutate(STAFF_GROUP = "NonMed Total"),
    non_medics %>% 
      inner_join(acute_trusts) %>%
      filter(STAFF_GROUP %in% c("Managers","Senior managers")) %>%
      group_by(DATE,STAFF_GROUP) %>% 
      summarise(FTE = sum(FTE)),
    non_medics %>%
      inner_join(acute_trusts) %>%
      filter(STAFF_GROUP %in% c("Managers","Senior managers")) %>%
      group_by(DATE) %>% 
      summarise(FTE = sum(FTE)) %>%
      mutate(STAFF_GROUP = "Manager Total"),
    medics %>% 
      inner_join(acute_trusts) %>%
      group_by(DATE) %>% 
      summarise(FTE = sum(FTE)) %>%
      mutate(STAFF_GROUP = "Med Total"),
    medics %>% 
      inner_join(acute_trusts) %>%
      filter(grepl("consultant",GRADE,ignore.case=TRUE)) %>%
      group_by(DATE) %>% 
      summarise(FTE = sum(FTE))%>%
      mutate(STAFF_GROUP = "Consultant")
    ) %>%
  mutate(STAFF_GROUP = gsub("anagers", "anager",STAFF_GROUP))

esr_2016_2017_2018_acute = bind_rows(
  esr_2016_2017_2018_acute,
  esr_2016_2017_2018_acute %>%
    filter(STAFF_GROUP %in% c("NonMed Total", "Med Total")) %>%
    group_by(DATE) %>%
    summarise(FTE = sum(FTE))%>%
    mutate(STAFF_GROUP = "All Staff Total") %>% 
    ungroup
)

working_dir = getwd()
setwd("data/csv")
esr = read_csv("non_medics_2009_2015.csv")
esr_medics = read_csv("medics_2009_2015.csv")
setwd(working_dir)

as.month = function(month_name){
  case_when(
    month_name == "JAN" ~ "01",
    month_name == "FEB" ~ "02",
    month_name == "MAR" ~ "03",
    month_name == "APR" ~ "04",
    month_name == "MAY" ~ "05",
    month_name == "JUN" ~ "06",
    month_name == "JUL" ~ "07",
    month_name == "AUG" ~ "08",
    month_name == "SEP" ~ "09",
    month_name == "OCT" ~ "10",
    month_name == "NOV" ~ "11",
    month_name == "DEC" ~ "12"
  )
}

esr = esr %>% 
  mutate(YEAR = substr(`Tm End Date`,1,4), MONTH = as.month(substr(`Tm End Date`,6,9)), DATE = paste("01",MONTH,YEAR,sep="/")) %>%
  mutate(ORG_CODE = `Org Code`) %>%
  inner_join(acute_trusts)

esr_medics = esr_medics %>% 
  mutate(YEAR = substr(`Tm End Date`,1,4), MONTH = as.month(substr(`Tm End Date`,6,9)), DATE = paste("01",MONTH,YEAR,sep="/")) %>%
  mutate(ORG_CODE = `Org Code`) %>%
  inner_join(acute_trusts)

acute_england = esr %>% 
  filter(`Staff Group` %in% c("Manager", "Senior manager", "NonMed Total")) %>%
  select(DATE, ORG_CODE, STAFF_GROUP = 'Staff Group', FTE=Table4NonMedFTE) %>%
  group_by(DATE, STAFF_GROUP) %>%
  summarise(FTE = sum(FTE)) %>%
  ungroup()

acute_england_medics= bind_rows(esr_medics %>% 
  select(DATE, ORG_CODE, FTE=Table2DocFTE) %>%
  group_by(DATE) %>%
  summarise(FTE = sum(FTE)) %>%
  mutate(STAFF_GROUP="Med Total") %>%
  ungroup(),
  esr_medics %>% 
    inner_join(acute_trusts) %>%
    filter(HCHS_GROUP_EQUIVALENT=="Consultant") %>%
    select(DATE,ORG_CODE, FTE=Table2DocFTE) %>%
    group_by(DATE) %>%
    summarise(FTE = sum(FTE)) %>%
    mutate(STAFF_GROUP="Consultant") %>%
    ungroup())

acute_england = bind_rows(acute_england, acute_england_medics)

plot_management_timeseries = function(data, title){
  data = data %>% filter(dmy(DATE)>dmy("31/08/2012"))
  graph_data = bind_rows( 
    data %>%
    filter(STAFF_GROUP %in% c("Manager", "Senior manager")) %>%
    group_by(DATE) %>%
    summarise(FTE = sum(FTE)) %>%
    mutate(STAFF_GROUP="Manager Total") %>%
    ungroup() %>%
    select(DATE, STAFF_GROUP, FTE),
    data %>%
    filter(STAFF_GROUP %in% c("Med Total", "NonMed Total")) %>%
    group_by(DATE) %>%
    summarise(FTE = sum(FTE)) %>%
    mutate(STAFF_GROUP="All Staff Total") %>%
    ungroup() %>%
    select(DATE, STAFF_GROUP, FTE)) %>% 
    spread(STAFF_GROUP, FTE) %>%
    mutate(`Manager %` = 100*`Manager Total`/(`All Staff Total`)) %>%
    gather(key=STAFF_GROUP, value=FTE, -DATE) %>%
    mutate(DATE = dmy(DATE), STAFF_GROUP = factor(STAFF_GROUP, levels = c("Manager Total", "All Staff Total", "Manager %"))) %>%
    arrange(DATE)


  plot = ggplot(data=graph_data, aes(x=DATE, y=FTE)) + 
    geom_line() +
    geom_point(colour="darkred") +
    geom_vline(xintercept=dmy("01/09/2015"), linetype="dashed", colour="darkgrey") +
    scale_x_date(date_labels="%b %Y", breaks="6 months") +
    facet_grid(STAFF_GROUP~., scales = "free") +
    scale_y_continuous(labels = comma, limits=c(0,NA)) +
    ggtitle(title) +
    theme_bw() + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       legend.position="none",
                       text=element_text(family = "Roboto", colour = "#3e3f3a"))

 return(plot)
}

plot_acute_trusts = plot_management_timeseries(data=bind_rows(acute_england, esr_2016_2017_2018_acute), title = "")#Total managers across acute NHS trusts only in England") 

ggsave("figures/acute_nhs_manager.png",plot_acute_trusts, width=14, height=10, units="in", dpi="print")

