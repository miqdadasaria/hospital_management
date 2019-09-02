library("tidyverse")
library("RSQLite")
library("dbplyr")
library("plotly")
library("stargazer")
library("assertthat")
library("plm")
library("Hmisc")

db_file = "data/NHS_management.sqlite3"
con = dbConnect(SQLite(), dbname=db_file)

##### define staff groups ####
non_medics = tbl(con, "non_medical_staff") %>% 
  inner_join(tbl(con, "main_staff_group")) %>%
  inner_join(tbl(con, "staff_group_1"))  %>% 
  inner_join(tbl(con, "staff_group_2")) %>%
  select(YEAR, ORG_CODE, MAIN_STAFF_GROUP_NAME, STAFF_GROUP_1_NAME, STAFF_GROUP_2_NAME, AREA, LEVEL, AFC_BAND, FTE) %>%
  collect()

medics = tbl(con, "medical_staff") %>% 
  collect()

nurses = non_medics %>% 
  filter(grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE) &
           !grepl("manager",LEVEL,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(NURSES=round(sum(FTE),1)) %>%
  ungroup()

other_clinical = non_medics %>% 
  filter(!grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE) &
           grepl("Professionally qualified clinical staff",MAIN_STAFF_GROUP_NAME,ignore.case=TRUE) &
           !grepl("manager",LEVEL,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(OTHER_CLINICAL_STAFF=round(sum(FTE),1)) %>%
  ungroup()

others = non_medics %>%
  filter(!grepl("Professionally qualified clinical staff",MAIN_STAFF_GROUP_NAME,ignore.case=TRUE) &
           !grepl("manager",LEVEL,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(OTHER_STAFF=round(sum(FTE),1)) %>%
  ungroup()

consultants = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(CONSULTANTS=round(sum(FTE),1)) %>%
  ungroup()

cardiologists = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE) & grepl("cardiology",SPECIALTY,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(CARDIOLOGISTS=round(sum(FTE),1)) %>%
  ungroup()

ae_docs = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE) & grepl("emergency medicine",SPECIALTY,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(EMERGENCY_MEDICS=round(sum(FTE),1)) %>%
  ungroup()

non_ae_docs = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE) & !grepl("emergency medicine",SPECIALTY,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(NON_EMERGENCY_MEDICS=round(sum(FTE),1)) %>%
  ungroup()

orthopods = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE) & grepl("orthopaedic",SPECIALTY,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(ORTHOPAEDIC_SURGEONS=round(sum(FTE),1)) %>%
  ungroup()

doctors = medics %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(DOCTORS=round(sum(FTE),1)) %>%
  ungroup()

all_staff = non_medics %>% 
  group_by(ORG_CODE, YEAR) %>% 
  summarise(NM_FTE=sum(FTE)) %>%
  inner_join(medics %>% 
               group_by(ORG_CODE, YEAR) %>% 
               summarise(M_FTE=sum(FTE)) ) %>%
  mutate(ALL_STAFF = round(NM_FTE + M_FTE,1)) %>%
  select(YEAR, ORG_CODE, ALL_STAFF) %>%
  ungroup()


junior_doctors = medics %>%
  filter(!grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(ORG_CODE, YEAR) %>%
  summarise(JUNIOR_DOCTORS=round(sum(FTE),1)) %>%
  ungroup()

staff = others %>%
  left_join(all_staff) %>%
  left_join(nurses) %>%
  left_join(junior_doctors) %>%
  left_join(consultants) %>%
  left_join(doctors) %>%
  left_join(other_clinical) %>%
  left_join(cardiologists) %>%
  left_join(orthopods) %>%
  left_join(ae_docs) %>%
  left_join(non_ae_docs) %>%
  mutate(CLINICAL_STAFF=DOCTORS+NURSES+OTHER_CLINICAL_STAFF, 
         CLINICAL_STAFF_PERCENT=round(100*CLINICAL_STAFF/ALL_STAFF,1),
         JUNIOR_DOCTORS_PERCENT=round(100*JUNIOR_DOCTORS/ALL_STAFF,1),
         DOCTORS_PERCENT=round(100*DOCTORS/ALL_STAFF,1),
         CONSULTANTS_PERCENT=round(100*CONSULTANTS/ALL_STAFF,1),
         OTHER_STAFF_PERCENT=round(100*OTHER_STAFF/ALL_STAFF,1),
         OTHER_CLINICAL_STAFF_PERCENT=round(100*OTHER_CLINICAL_STAFF/ALL_STAFF,1),
         NURSES_PERCENT=round(100*NURSES/ALL_STAFF,1)
         )


afc_pay = tbl(con, "pay_scale") %>% 
  collect() %>%
  bind_rows(tibble(YEAR=2017,AFC_BAND=c("Very Senior Manager","Non AfC Grade"),BOTTOM=142500,AVERAGE=142500,TOP=142500)) 

##### construct provider level dataset #### 
providers =  tbl(con, "esr_simple") %>% select(ORG_CODE,YEAR,SIMPLE_MANAGERS_FTE,SIMPLE_CONSULTANTS_FTE) %>%
  collect() %>%
  left_join(staff %>% filter(YEAR < 2019)) %>%
  left_join(
  tbl(con, "provider") %>% 
  left_join(tbl(con, "hee_region")) %>%
  left_join(tbl(con, "cqc_rating") %>% filter(POPULATION=="Overall" & QUESTION=="Overall") %>% select(ORG_CODE,RATING_SCORE)) %>%
  left_join(tbl(con, "cqc_rating") %>% filter(POPULATION=="Overall" & QUESTION=="Well-led") %>% select(ORG_CODE,RATING_SCORE_LEADERSHIP=RATING_SCORE)) %>%
  collect()) %>% 
  filter(ORG_TYPE == "Acute") %>%
  left_join(
  tbl(con, "nhs_ss_management_score") %>% filter(QUESTION=="overall") %>% mutate(MANAGEMENT_QUALITY=round(((VALUE-1)/4)*100,1)) %>% select(ORG_CODE,YEAR,MANAGEMENT_QUALITY) %>%
  left_join(tbl(con, "financial_position") %>% select(ORG_CODE,YEAR,OP_COST,FINANCIAL_POSITION)) %>%
  left_join(tbl(con, "rtt_target") %>% select(ORG_CODE,YEAR,RTT_SCORE)) %>%
  left_join(tbl(con, "ae_target") %>% select(ORG_CODE,YEAR,AE_SCORE)) %>%
  left_join(tbl(con, "inpatient_data") %>% select(ORG_CODE,YEAR,TOTAL_EPISODES,FEMALE_ADMISSIONS,"0-14","15-29","30-44","45-59","60-74","75-89","90+")) %>%
  left_join(tbl(con, "shmi") %>% select(ORG_CODE,YEAR,SHMI)) %>%
  left_join(tbl(con, "beds") %>% select(ORG_CODE,YEAR,BEDS,BEDS_OCCUPIED_PERCENT)) %>%
  collect()) %>%
  mutate(OP_COST = OP_COST/-1000, FINANCIAL_POSITION = FINANCIAL_POSITION/1000) %>%
  mutate(AE_SCORE=round(AE_SCORE*100,1), RTT_SCORE=round(RTT_SCORE*100,1)) %>%
  mutate(RATING = factor(RATING_SCORE,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  mutate(RATING_LEADERSHIP = factor(RATING_SCORE_LEADERSHIP,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  mutate(FEMALE_ADMISSIONS=round(FEMALE_ADMISSIONS/TOTAL_EPISODES,2)*100,
         all_age = `0-14`+`15-29`+`30-44`+`45-59`+`60-74`+`75-89`+`90+`,
         AGE_0_14=round(`0-14`/all_age,2)*100,
         AGE_15_29=round(`15-29`/all_age,2)*100,
         AGE_30_44=round(`30-44`/all_age,2)*100,
         AGE_45_59=round(`45-59`/all_age,2)*100,
         AGE_60_74=round(`60-74`/all_age,2)*100,
         AGE_75_89=round(`75-89`/all_age,2)*100,
         AGE_90_PLUS=round(`90+`/all_age,2)*100)

dbDisconnect(con)

managers = non_medics %>% 
  filter(grepl("manager",LEVEL,ignore.case=TRUE)) %>% 
  inner_join(providers %>% select(ORG_CODE,YEAR)) %>% 
  mutate(STAFF_GROUP = if_else(grepl("manager",STAFF_GROUP_1_NAME,ignore.case = TRUE),paste("Manager",gsub("00[1234]_","",AREA)),STAFF_GROUP_1_NAME)) %>% 
  group_by(YEAR, ORG_CODE,STAFF_GROUP,AFC_BAND) %>% 
  summarise(FTE=sum(FTE)) %>%
  ungroup()

##### import variables to use in data analysis ####
variable_definitions = read_csv("data/variable_definitions.csv") %>% arrange(label)


all_vars=as.list(variable_definitions$code)
names(all_vars)=variable_definitions$label

get_variable = function(var, definitions){
  v = definitions %>% filter(code %in% var) %>% select(variable, label)
  return(v)
}

##### construct management measure as defined by UI ####
attach_management_measure = function(providers, afc_pay, managers, selected_staff_group, selected_pay_grade, year){
  
  pay_grade = vector()
  if("afc_2" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 2")  
  }
  
  if("afc_3" %in% selected_pay_grade ){
    pay_grade = append(pay_grade,"Band 3")  
  }
  
  if("afc_4" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 4")  
  }
  
  if("afc_5" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 5")  
  }
  
  if("afc_6" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 6")  
  }
  
  if("afc_7" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 7")  
  }
  
  if("afc_8a" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 8a")  
  }
  
  if("afc_8b" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 8b")  
  }
  
  if("afc_8c" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 8c")  
  }
  
  if("afc_8d" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 8d")  
  }
  
  if("afc_9" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Band 9")  
  }
  
  if("afc_vsm" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Very Senior Manager")  
  }
  
  if("afc_nafc" %in% selected_pay_grade){
    pay_grade = append(pay_grade,"Non AfC Grade")  
  }
  
  managers = managers %>% filter(AFC_BAND %in% pay_grade)
  
  staff_group = vector()
  if("man_central_functions" %in% selected_staff_group){
    staff_group = append(staff_group,"Manager Central functions")
  }
  
  if("man_estates" %in% selected_staff_group){
    staff_group = append(staff_group,"Manager Hotel, property & estates")
  }
  
  if("man_clinical_support" %in% selected_staff_group){
    staff_group = append(staff_group,"Manager Clinical support")
  }
  
  if("man_scientific" %in% selected_staff_group){
    staff_group = append(staff_group,"Manager Scientific, therapeutic & technical support")
  }
  
  if("nurse" %in% selected_staff_group){
    staff_group = append(staff_group,"Nurses & health visitors")    
  }
  
  if("scientific" %in% selected_staff_group){
    staff_group = append(staff_group,"Scientific, therapeutic & technical staff")    
  }
  
  managers = managers %>% filter(STAFF_GROUP %in% staff_group)
  
  managers = managers %>%
    left_join(afc_pay %>% select(AFC_BAND,PAY=TOP)) %>%
    mutate(MANAGEMENT_SPEND=round(FTE*PAY,0)) %>%
    group_by(ORG_CODE, YEAR) %>%
    summarise(MANAGEMENT_SPEND=sum(MANAGEMENT_SPEND), MANAGERS=round(sum(FTE),1)) %>%
    ungroup()
  
  
  results = providers %>%
    left_join(managers) %>% 
    mutate(MANAGERS_SQ = round(MANAGERS^2,1),
           SIMPLE_MANAGERS_FTE_SQ = round(SIMPLE_MANAGERS_FTE^2,1),
           MAN_SPEND_PER_1000_FCE = round(MANAGEMENT_SPEND*1000/TOTAL_EPISODES,0), 
           MAN_FTE_PER_1000_FCE = round(MANAGERS*1000/TOTAL_EPISODES,2),
           MANAGEMENT_SPEND_SQ = round((MANAGEMENT_SPEND^2)/100000,2),
           MANAGEMENT_SPEND = round((MANAGEMENT_SPEND)/100000,2),
           MANAGERS_PERCENT = round((MANAGERS/ALL_STAFF)*100,2),
           QUALITY_WEIGHTED_FTE = round(MAN_FTE_PER_1000_FCE*MANAGEMENT_QUALITY/100,2),
           QUALITY_WEIGHTED_SPEND = round(MAN_SPEND_PER_1000_FCE*MANAGEMENT_QUALITY/100,0),
           FIN_POS_PERC = round((FINANCIAL_POSITION/OP_COST)*100,2),
           HEE_REGION_NAME = gsub("Health Education ","",HEE_REGION_NAME),
           ORG_NAME = gsub("NHS Trust","",ORG_NAME),
           ORG_NAME = gsub("NHS Foundation Trust","",ORG_NAME),
           MAN_FTE_PER_TEN_MIL_OC = round((MANAGERS)/(OP_COST*0.1),2),
           MAN_FTE_PER_10_BED = round((MANAGERS*10)/BEDS,2),
           SPECIALIST = factor(SPECIALIST, levels=c(0,1), labels=c("Non-specialist","Specialist")),
           MANAGER_CLINICIAN_RATIO=round(MANAGERS/CLINICAL_STAFF,2),
           FCE_PER_MAN = round(TOTAL_EPISODES/MANAGERS,0),
           OC_MIL_PER_MAN = round((OP_COST)/(MANAGERS),2),
           STAFF_PER_MAN = round(ALL_STAFF/MANAGERS,2),
           BEDS_PER_MAN = round(BEDS/MANAGERS,2),
           FCE_PER_NON_AE_CONSULTANT = (TOTAL_EPISODES/NON_EMERGENCY_MEDICS)
           ) %>%
    select(ORG_CODE,
           ORG_NAME,
           YEAR,
           SPECIALIST,
           HEE_REGION=HEE_REGION_NAME,
           OPERATING_COST=OP_COST,
           BEDS,
           BEDS_OCCUPIED_PERCENT,
           ADMISSIONS=TOTAL_EPISODES,
           FEMALE_ADMISSIONS,
           AGE_0_14,
           AGE_15_29,
           AGE_30_44,
           AGE_45_59,
           AGE_60_74,
           AGE_75_89,
           AGE_90_PLUS,
           CQC_RATING=RATING,
           CQC_WELL_LED_RATING=RATING_LEADERSHIP,
           NET_FINANCIAL_POSITION=FINANCIAL_POSITION,
           NET_FINANCIAL_POSITION_PERCENT=FIN_POS_PERC,
           RTT_SCORE,
           AE_SCORE,
           SHMI,
           ALL_STAFF,
           JUNIOR_DOCTORS,
           CONSULTANTS,
           CARDIOLOGISTS,
           EMERGENCY_MEDICS,
           NON_EMERGENCY_MEDICS,
           ORTHOPAEDIC_SURGEONS,
           DOCTORS,
           NURSES,
           OTHER_CLINICAL_STAFF,
           CLINICAL_STAFF,
           OTHER_STAFF,
           MANAGERS,
           JUNIOR_DOCTORS_PERCENT,
           CONSULTANTS_PERCENT,
           DOCTORS_PERCENT,
           NURSES_PERCENT,
           OTHER_CLINICAL_STAFF_PERCENT,
           CLINICAL_STAFF_PERCENT,
           OTHER_STAFF_PERCENT,
           MANAGERS_PERCENT,
           NHS_SS=MANAGEMENT_QUALITY,
           MANAGEMENT_SPEND,
           MAN_FTE_PER_TEN_MIL_OC,
           MAN_FTE_PER_1000_FCE,
           MAN_FTE_PER_10_BED,
           MAN_SPEND_PER_1000_FCE,
           FCE_PER_MAN,
           OC_MIL_PER_MAN,
           STAFF_PER_MAN,
           BEDS_PER_MAN,
           QUALITY_WEIGHTED_FTE,
           QUALITY_WEIGHTED_SPEND,
           MANAGER_CLINICIAN_RATIO,
           MANAGERS_SQ,
           MANAGEMENT_SPEND_SQ,
           FCE_PER_NON_AE_CONSULTANT,
           SIMPLE_MANAGERS_FTE,
           SIMPLE_MANAGERS_FTE_SQ,
           SIMPLE_CONSULTANTS_FTE)
  
  changes_18 = calculate_changes_over_time(results,2017,2018)
  changes_17 = calculate_changes_over_time(results,2016,2017)
  # changes_16 = calculate_changes_over_time(results,2015,2016)
  # changes_15 = calculate_changes_over_time(results,2014,2015)
  # changes_14 = calculate_changes_over_time(results,2013,2014)
  results = results %>% filter(YEAR==year)
  results = left_join(results,changes_17) %>% 
    left_join(changes_18)
  
  return(results)
}


calculate_changes_over_time = function(providers,t1,t2){
  providers_t2 = providers %>% 
    filter(YEAR==t2) %>%
    select(-c(ORG_NAME,
           YEAR,
           SPECIALIST,
           HEE_REGION,        
           CQC_RATING,
           CQC_WELL_LED_RATING))

  providers_t1 = providers %>% 
    filter(YEAR==t1) %>%
    select(-c(ORG_NAME,
              YEAR,
              SPECIALIST,
              HEE_REGION,        
              CQC_RATING,
              CQC_WELL_LED_RATING))
  
    change_vars = inner_join(providers_t1 %>% gather("VAR","VAL.t1",-ORG_CODE),
    providers_t2 %>% gather("VAR","VAL.t2",-ORG_CODE), by=c("ORG_CODE","VAR")) %>%
      mutate(CHANGE=VAL.t2-VAL.t1) %>%
      select(ORG_CODE,VAR,CHANGE) %>%
      spread(VAR,CHANGE,sep="_")
    
    names(change_vars) = gsub("VAR_",paste0("CHANGE_",t1,"_",t2,"_"),names(change_vars))
  
    return(change_vars)
}


##### scatter plots and histigrams for UI ####
scatter_plot = function(acute_providers, variables, x_var, y_var, size_var, trim, specialist, trend_line, facet_var, log_x_var, log_y_var, year, show_titles, include_outliers){
  if(!specialist){
    acute_providers = acute_providers %>% filter(SPECIALIST=="Non-specialist")
  }
  
  if(!include_outliers){
    # e.g RAJ = Southend has a suspiciously low number of managers
    acute_providers = acute_providers %>% filter(MANAGERS_PERCENT>quantile(MANAGERS_PERCENT,probs=0.05) & MANAGERS_PERCENT<quantile(MANAGERS_PERCENT,probs=0.95))
  }
  
  x = get_variable(x_var, variables)
  y = get_variable(y_var, variables)
  if(size_var != "none"){
    s = get_variable(size_var, variables)
  }
  if(facet_var != "none"){
    f = get_variable(facet_var, variables)
  }
  
  if(any(!is.na(acute_providers$MAN_FTE_PER_1000_FCE))){
    graph_data = acute_providers %>% 
      filter(MAN_FTE_PER_1000_FCE<trim)
  } else {
    graph_data = acute_providers
  }
  
  if(log_x_var){
    graph_data[x$variable] = log(graph_data[x$variable])
    x$label = paste("log ", x$label)
  }

  if(log_y_var){
    graph_data[y$variable] = log(graph_data[y$variable])
    y$label = paste("log ", y$label)
  }
  
  rows = graph_data %>% 
    select(y$variable, x$variable) %>% 
    filter(!(is.na(.[[1]]) | is.na(.[[2]]))) %>% 
    summarise(n())
  
  plot = ggplot(data=graph_data, aes_string(x=x$variable, y=y$variable, label="ORG_NAME")) +
    ylab(y$label) +
    xlab(x$label)
  
  if(size_var != "none"){
    plot = plot + geom_point(aes_string(size=s$variable, colour="SPECIALIST"), alpha=0.4)
    if(trend_line){
      plot = plot + geom_smooth(method = "lm", aes_string(weight=s$variable), colour="darkred", linetype="dashed", size=0.8, se=FALSE)
    }
  } else {
    plot = plot + geom_point(aes_string(colour="SPECIALIST"), alpha=0.4)
    if(trend_line){
      plot = plot + geom_smooth(method = "lm", colour="darkred", linetype="dashed", size=0.8, se=FALSE)
    }
  }
  
  plot = plot + scale_colour_manual(values=c("darkblue","darkgreen"))   
  
  if(facet_var != "none"){
    plot = plot + facet_wrap(f$variable)
  }
  
  if(grepl("shmi", y_var)){
    plot = plot + geom_hline(yintercept=1, linetype="dashed", colour="darkgrey", size=0.5)
  } else if(min(graph_data[y$variable],na.rm=TRUE)<0 & max(graph_data[y$variable],na.rm=TRUE)>0){
    plot = plot + geom_hline(yintercept=0, linetype="dashed", colour="darkgrey", size=0.5)
  } 
  
  if(grepl("shmi", x_var)){
    plot = plot + geom_vline(xintercept=1, linetype="dashed", colour="darkgrey", size=0.5)
  } else if(min(graph_data[x$variable],na.rm=TRUE)<0 & max(graph_data[x$variable],na.rm=TRUE)>0){
    plot = plot + geom_vline(xintercept=0, linetype="dashed", colour="darkgrey", size=0.5)
  } 
  
  if(show_titles){
    plot = plot + ggtitle(paste0("Based on data from the ",rows," NHS trusts with data available on both variables in ",year))
  }
  
  plot = plot + theme_bw() + theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   plot.title = element_blank(),
                                   plot.margin = unit(c(1, 1, 1, 1), "lines"),
                                   legend.position="none",
                                   text=element_text(family = "Roboto", colour = "#3e3f3a"))
  plotly = ggplotly(plot)
  return(plotly)
}

##### ranking tables ####
create_ranking_table = function(providers, variables, rank_var){
  x = get_variable(rank_var, variables)$variable
  
  ranked_providers = providers %>%
    mutate(RANK = rank(desc(get(x)))) %>%
    select(RANK,
           ORG_CODE,
           ORG_NAME,
           x,
           BEDS, 
           ALL_STAFF,
           MANAGERS,
           MANAGERS_PERCENT,
           NHS_SS,
           MANAGER_CLINICIAN_RATIO) %>% 
    arrange(RANK)
  
  return(ranked_providers)
}

histogram_plot = function(acute_providers, variables, x_var, specialist, year, show_titles, include_outliers){
  
  if(!specialist){
    acute_providers = acute_providers %>% filter(SPECIALIST=="Non-specialist")
  }
  
  if(!include_outliers){
    # e.g RAJ = Southend has a suspiciously low number of managers
    acute_providers = acute_providers %>% filter(MANAGERS_PERCENT>quantile(MANAGERS_PERCENT,probs=0.05) & MANAGERS_PERCENT<quantile(MANAGERS_PERCENT,probs=0.95))
  }
  
  x = get_variable(x_var, variables)
  
  graph_data = acute_providers

    
  plot = ggplot(data=graph_data,aes_string(x$variable)) + 
    geom_histogram(fill="#E69F00", colour="black") + 
    xlab(x$label) +
    ylab("Count of acute NHS trusts") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
 
  if(show_titles){
    plot = plot + ggtitle(paste0("Based on data from the ",sum(!is.na(graph_data[x$variable]))," NHS trusts with data available for ",year))
  }
  
  plotly = ggplotly(plot)
  return(plotly)
}

##### tables of desriptives on managers ####
manager_counts = function(managers, acute_providers, specialist, year, include_outliers){
  if(!specialist){
    acute_providers = acute_providers %>% filter(SPECIALIST=="Non-specialist")
  }
  
  if(!include_outliers){
    # e.g RAJ = Southend has a suspiciously low number of managers
    acute_providers = acute_providers %>% filter(MANAGERS_PERCENT>quantile(MANAGERS_PERCENT,probs=0.05) & MANAGERS_PERCENT<quantile(MANAGERS_PERCENT,probs=0.95))
  }
  
  manager_counts = managers %>%
    filter(YEAR==year) %>%
    inner_join(acute_providers %>% select(ORG_CODE,YEAR)) %>%
    group_by(STAFF_GROUP,AFC_BAND) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(AFC_BAND, FTE, fill = 0) %>%
    ungroup
  
  manager_counts = manager_counts %>%
    bind_rows(manager_counts[-1] %>% 
                summarise_if(is.numeric,sum) %>% 
                bind_cols("STAFF_GROUP"="Total"))
  
  manager_counts = manager_counts %>% 
    bind_cols("Total"=rowSums(manager_counts[,-1]))
  
  manager_percentages = manager_counts %>% mutate_if(is.numeric, function(x){round(x*100/max(manager_counts[,-1]),2)}) 
  
  managers = list("counts" = manager_counts %>% mutate_if(is.numeric, function(x){round(x,1)}), "percentages" = manager_percentages)
  
  return(managers)
}

##### run regressions ####
run_regression = function(acute_providers, variables, dependent_vars, independent_vars, mean_centre, log_dep_vars, log_indep_vars, interactions, output, specialist, include_outliers, all_outcomes, panel=FALSE){
  
  independent_vars_string = vector(mode="character")
  independent_vars_labels = vector(mode="character")
  dependent_vars_labels = vector(mode="character")
  
  if(!specialist){
    acute_providers = acute_providers %>% filter(SPECIALIST=="Non-specialist")
  }
  
  if(!include_outliers){
    # e.g RAJ = Southend has a suspiciously low number of managers
    acute_providers = acute_providers %>% filter(MANAGERS_PERCENT>quantile(MANAGERS_PERCENT,probs=0.05) & MANAGERS_PERCENT<quantile(MANAGERS_PERCENT,probs=0.95))
  }
  
  if(all_outcomes){
    for(y_var in dependent_vars){
      y = get_variable(y_var, variables)
      acute_providers = acute_providers %>% drop_na(y$variable)
    }
  }
  
  if("casemix" %in% independent_vars){
    independent_vars = c(independent_vars[-which(independent_vars %in% "casemix")],"female","age_0_14","age_15_29","age_30_44","age_45_59","age_60_74","age_75_89","age_90")
  }
  
  for(x_var in independent_vars){
    x = get_variable(x_var, variables)
    if(log_indep_vars & acute_providers %>% select(x$variable) %>% summarise_all(is.numeric) & x_var %in% c("fte", "fte_sq","nhs_ss","simple_fte","simple_fte_sq","simple_consultants","consultants","nurses","spend","spend_sq")){
      indep_var = paste0("log(",x$variable,")")
      indep_var_label = paste0("log ", x$label)
      # remove zeros as upsets log
      acute_providers[which(acute_providers[x$variable]==0),x$variable]=0.01
    } else {
      indep_var = x$variable
      indep_var_label = x$label
    }    
    independent_vars_string = c(independent_vars_string, indep_var)
    independent_vars_labels = c(independent_vars_labels, indep_var_label)
  }
  
  if(interactions){
    formula_independents = paste(independent_vars_string, collapse=" * ")
  } else {
    formula_independents = paste(independent_vars_string, collapse=" + ")
  }
  
  independent_vars = acute_providers %>% select(c(YEAR,ORG_CODE,gsub("log\\(|)","",independent_vars_string)))
  independent_vars = independent_vars %>% mutate(YEAR = as_factor(YEAR), ORG_CODE = as_factor(ORG_CODE))
  if(mean_centre){
    independent_vars = independent_vars %>% mutate_if(is.numeric,scale,center=TRUE,scale=FALSE)
  }
  regressions = list()
  regression_se = list()
  for(y_var in dependent_vars){
    y = get_variable(y_var, variables)
    if(log_dep_vars){
      if(any(acute_providers[y$variable]<0)) next
      dep_var = paste0("log(",y$variable,")")
      dep_var_label = paste0("log ", y$label)
      # remove zeros as upsets log
      acute_providers[which(acute_providers[y$variable]==0),y$variable]=0.01
    } else {
      dep_var = y$variable
      dep_var_label = y$label
    }
    dependent_vars_labels = c(dependent_vars_labels, dep_var_label)
    reg_formula = as.formula(paste(dep_var,formula_independents,sep=" ~ "))
    #print(reg_formula)
    if(panel){
      panel_data = pdata.frame(bind_cols(independent_vars,acute_providers%>%select(y$variable)), index=c("ORG_CODE","YEAR"), drop.index=TRUE, row.names=TRUE)
      fe_model = plm(reg_formula, 
                     data = panel_data,
                     model = "within", 
                     effect = "twoways")
      
      regressions[[y$variable]] = fe_model
      vcov = try(vcovHC(fe_model, type="sss", cluster="group"), TRUE)
      if(grepl("Error", vcov[1])){
        vcov=vcov(fe_model)
        print(paste0("Error calculating clustered standard errors for: ", y$label, " using default stadard errors"))
      }
    } else {
        lm_model = lm(reg_formula, data=bind_cols(independent_vars,acute_providers%>%select(y$variable)))
        regressions[[y$variable]] = lm_model
        vcov = vcov(lm_model)
    }
    regression_se[[y$variable]] = sqrt(diag(vcov))
  }
  
  if (output=="latex") {
    independent_vars_labels = sapply(independent_vars_labels,latexTranslate)
    dependent_vars_labels = sapply(dependent_vars_labels,latexTranslate)
    
  }
  
  results = paste(capture.output(stargazer(regressions, 
                                           covariate.labels=independent_vars_labels,
                                           dep.var.labels=dependent_vars_labels,
                                           se=regression_se,
                                           type=output)), collapse="\n") 
  return(results)
}

make_regression_formula = function(dependent_var, independent_vars, logged, lagged){
  independent_vars_string = vector(mode="character")
  independent_vars_labels = vector(mode="character")
  for(x_var in independent_vars){
    x = get_variable(x_var, variable_definitions)
    if(logged & x_var %in% c("fte", "fte_sq","nhs_ss","simple_fte","simple_fte_sq","simple_consultants","spend","spend_sq")){
      indep_var = paste0("log(",x$variable,")")
      indep_var_label = paste0("log ", latexTranslate(x$label))
    } else {
      indep_var = x$variable
      indep_var_label = latexTranslate(x$label)
    }    
    if(!(dependent_var=="shmi" & x_var=="specialist")){
      independent_vars_string = c(independent_vars_string, indep_var)
      independent_vars_labels = c(independent_vars_labels, indep_var_label)
    }
  }
  
  y = get_variable(dependent_var, variable_definitions)
  
  if(lagged){
    if(logged){
      indep_var = paste0("log(plm::lag(",y$variable,"))")
      indep_var_label = paste0("Lagged log ", latexTranslate(y$label))
    } else {
      indep_var = paste0("plm::lag(",y$variable,")")
      indep_var_label = paste0("Lagged ", latexTranslate(y$label))
    } 
    independent_vars_string = c(independent_vars_string, indep_var)
    independent_vars_labels = c(independent_vars_labels, indep_var_label)
  }
  
  if(logged){
    dep_var = paste0("log(",y$variable,")")
    dep_var_label = paste0("log ", latexTranslate(y$label))
  } else {
    dep_var = y$variable
    dep_var_label = latexTranslate(y$label)
  }
  
  reg_formula = as.formula(paste(dep_var,paste(independent_vars_string, collapse=" + "),sep=" ~ "))
  
  results = list()
  results[["formula"]] = reg_formula
  results[["covar_labels"]] = independent_vars_labels
  results[["depvar_label"]] = dep_var_label
  
  return(results)
}

appendix_regression = function(acute_providers, independent_var, dependent_var, logged, output_filename){
  
  independent_vars_basic = c("simple_consultants","nhs_ss","beds","op_cost","fce","female","age_0_14","age_15_29","age_30_44","age_45_59","age_60_74","age_75_89","age_90")
  
  if(logged){
    if(independent_var=="fte"){
      covars = c(independent_var, paste0("simple_",independent_var), independent_vars_basic, "specialist",rep("year",5))
    } else {
      covars = c(independent_var, independent_vars_basic, "specialist","year")
    }
    independent_vars_basic_simple = c(paste0("simple_",independent_var), independent_vars_basic)
    independent_vars_basic = c(independent_var, independent_vars_basic)
  } else {
    if(independent_var=="fte"){
      covars = c(independent_var, paste0(independent_var,"_sq"), paste0("simple_",independent_var), paste0("simple_",independent_var,"_sq"), independent_vars_basic, "specialist",rep("year",5))
    } else {
      covars = c(independent_var, paste0(independent_var,"_sq"), independent_vars_basic, "specialist","year")
    }
    independent_vars_basic_simple = c(paste0("simple_",independent_var), paste0("simple_",independent_var,"_sq"), independent_vars_basic)
    independent_vars_basic = c(independent_var, paste0(independent_var,"_sq"), independent_vars_basic)
  }
  
  regressions = list()
  se_clustered = list()
  
  # model 1 - 16/17 cross section
  if (dependent_var=="shmi") {
    independent_vars = independent_vars_basic
  } else {
    independent_vars = c(independent_vars_basic, "specialist")
  }
  model_1_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
  regressions[[1]] = plm(model_1_formula$formula, 
                         data = acute_providers %>% filter(YEAR=="2016"),
                         index=c("ORG_CODE","YEAR"),
                         model = "pooling")
  vcov = regressions[[1]]$vcov
  se_clustered[[1]] = sqrt(diag(vcov))
  
  # model 2 - 17/18 cross section
  independent_vars = c(independent_vars_basic, "specialist")
  model_2_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
  regressions[[2]] = plm(model_2_formula$formula, 
                         data = acute_providers %>% filter(YEAR=="2017"),
                         index=c("ORG_CODE","YEAR"),
                         model = "pooling")
  vcov = regressions[[2]]$vcov
  se_clustered[[2]] = sqrt(diag(vcov))
  
  # model 3 - 16/17 - 17/18 pooled with year dummies
  independent_vars = c(independent_vars_basic, "specialist","year")
  model_3_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
  regressions[[3]] = plm(model_3_formula$formula, 
                         data = acute_providers,
                         index=c("ORG_CODE","YEAR"),
                         model = "pooling")
  
  vcov = try(vcovHC(regressions[[3]], type="sss", cluster="group"), TRUE)
  if(grepl("Error", vcov[1])){
    vcov = regressions[[3]]$vcov
    print(paste0("Error calculating clustered standard errors for: model 3 using default stadard errors"))
  }
  se_clustered[[3]] = sqrt(diag(vcov))
  
  # model 4 - 16/17 - 17-18 fixed effects twoway
  independent_vars = independent_vars_basic
  model_4_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
  regressions[[4]] = plm(model_4_formula$formula, 
                         data = acute_providers,
                         index=c("ORG_CODE","YEAR"),
                         model = "within", 
                         effect = "twoways")
  
  vcov = try(vcovHC(regressions[[4]], type="sss", cluster="group"), TRUE)
  if(grepl("Error", vcov[1])){
    vcov = regressions[[4]]$vcov
    print(paste0("Error calculating clustered standard errors for: model 4 using default stadard errors"))
  }
  se_clustered[[4]] = sqrt(diag(vcov))
  
  # model 5 - 16/17 - 17/18 pooled with lagged dep variable
  independent_vars = c(independent_vars_basic, "specialist")
  model_5_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=TRUE)
  regressions[[5]] = plm(model_5_formula$formula, 
                         data = acute_providers,
                         index=c("ORG_CODE","YEAR"),
                         model = "pooling")
  
  vcov = try(vcovHC(regressions[[5]], type="sss", cluster="group"), TRUE)
  if(grepl("Error", vcov[1])){
    vcov = regressions[[5]]$vcov
    print(paste0("Error calculating clustered standard errors for: model 5 using default stadard errors"))
  }
  se_clustered[[5]] = sqrt(diag(vcov))
  
  if(independent_var=="fte"){
    # model 6 - 16/17 - 17-18 random effects individual
    independent_vars = c(independent_vars_basic, "specialist")
    model_6_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
    regressions[[6]] = plm(model_6_formula$formula, 
                           data = acute_providers,
                           index=c("ORG_CODE","YEAR"),
                           model = "random", 
                           effect = "individual")
    vcov = try(vcovHC(regressions[[6]], type="sss", cluster="group"), TRUE)
    if(grepl("Error", vcov[1])){
      vcov = regressions[[6]]$vcov
      print(paste0("Error calculating clustered standard errors for: model 6 using default stadard errors"))
    }
    se_clustered[[6]] = sqrt(diag(vcov))
    
    # model 7 - 12/13 - 17/18  pooled simple management def
    independent_vars = c(independent_vars_basic_simple, "specialist","year")
    model_7_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
    regressions[[7]] = plm(model_7_formula$formula, 
                           data = acute_providers,
                           index=c("ORG_CODE","YEAR"),
                           model = "pooling")
    
    vcov = try(vcovHC(regressions[[7]], type="sss", cluster="group"), TRUE)
    if(grepl("Error", vcov[1])){
      vcov = regressions[[7]]$vcov
      print(paste0("Error calculating clustered standard errors for: model 7 using default stadard errors"))
    }
    se_clustered[[7]] = sqrt(diag(vcov))
    
    # model 8 - 12/13 - 17/18 fixed effects simple management def
    independent_vars = independent_vars_basic_simple
    model_8_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
    regressions[[8]] = plm(model_8_formula$formula, 
                           data = acute_providers,
                           index=c("ORG_CODE","YEAR"),
                           model = "within", 
                           effect = "twoways")
    
    vcov = try(vcovHC(regressions[[8]], type="sss", cluster="group"), TRUE)
    if(grepl("Error", vcov[1])){
      vcov = regressions[[8]]$vcov
      print(paste0("Error calculating clustered standard errors for: model 8 using default stadard errors"))
    }
    se_clustered[[8]] = sqrt(diag(vcov))
    
    # model 9 - 12/13 - 17/18 pooled simple management with lagged dep variable
    independent_vars = c(independent_vars_basic_simple, "specialist")
    model_9_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=TRUE)
    regressions[[9]] = plm(model_9_formula$formula, 
                           data = acute_providers,
                           index=c("ORG_CODE","YEAR"),
                           model = "pooling")
    
    vcov = try(vcovHC(regressions[[9]], type="sss", cluster="group"), TRUE)
    if(grepl("Error", vcov[1])){
      vcov = regressions[[9]]$vcov
      print(paste0("Error calculating clustered standard errors for: model 9 using default stadard errors"))
    }
    se_clustered[[9]] = sqrt(diag(vcov))
    
    # model 10 - 12/13 - 17/18 random effects simple management def
    independent_vars = independent_vars_basic_simple
    model_10_formula = make_regression_formula(dependent_var, independent_vars, logged, lagged=FALSE)
    regressions[[10]] = plm(model_10_formula$formula, 
                            data = acute_providers,
                            index=c("ORG_CODE","YEAR"),
                            model = "random", 
                            effect = "individual")
    vcov = try(vcovHC(regressions[[10]], type="sss", cluster="group"), TRUE)
    if(grepl("Error", vcov[1])){
      vcov = regressions[[10]]$vcov
      print(paste0("Error calculating clustered standard errors for: model 10 using default stadard errors"))
    }
    se_clustered[[10]] = sqrt(diag(vcov))
  }
  
  #independent_vars_labels = c(independent_vars_labels_fte[1:2],independent_vars_labels_spend[1:2],independent_vars_labels_changes[3:length(independent_vars_changes)])
  if(independent_var=="fte"){
    dependent_vars_labels = rep(model_1_formula$depvar_label,10)
  } else {
    dependent_vars_labels = rep(model_1_formula$depvar_label,5)
  }
  
  independent_vars_labels = make_regression_formula(dependent_var, covars, logged, lagged = TRUE)$covar_labels
  
  regression_results = paste(capture.output(stargazer(regressions, 
                                                      covariate.labels=independent_vars_labels,
                                                      dep.var.labels=dependent_vars_labels,
                                                      model.names = TRUE,
                                                      model.numbers = TRUE,
                                                      type="latex")), collapse="\n") 
  sink(file=paste0("figures/",output_filename))
  cat(regression_results)
  sink()
}

##### misc descriptives for presentations / journal articles ####

create_sample_providers_dataset = function(year, specialist, include_outliers, all_outcomes, all_pay_grades=FALSE){
  
  if(all_pay_grades){
    selected_pay_grade = c("afc_nafc","afc_2","afc_3","afc_4","afc_5","afc_6","afc_7","afc_8a","afc_8b","afc_8c","afc_8d","afc_9","afc_vsm")
  } else {
    selected_pay_grade = c("afc_7","afc_8a","afc_8b","afc_8c","afc_8d","afc_9","afc_vsm")
  }
  
  selected_staff_group = c("man_central_functions","man_estates","man_clinical_support","man_scientific","nurse","scientific")
  
  acute_providers = attach_management_measure(providers, afc_pay, managers, selected_staff_group, selected_pay_grade, year)
  
  if(!specialist){
    acute_providers = acute_providers %>% filter(SPECIALIST=="Non-specialist")
  }
  
  if(!include_outliers){
    # e.g RAJ = Southend has a suspiciously low number of managers
    acute_providers = acute_providers %>% filter(MANAGERS_PERCENT>quantile(MANAGERS_PERCENT,probs=0.05) & MANAGERS_PERCENT<quantile(MANAGERS_PERCENT,probs=0.95))
  }
  
  if(all_outcomes){
    acute_providers = acute_providers %>% drop_na(NET_FINANCIAL_POSITION,RTT_SCORE,AE_SCORE,SHMI,FCE_PER_NON_AE_CONSULTANT) 
  }
  
  return(acute_providers)
}

descriptives = function(acute_providers, percentage){
  
  doctors = medics %>% 
    inner_join(acute_providers) %>% 
    mutate(CONSULTANT=GRADE_SORT_ORDER %in% c(1,20)) %>% 
    group_by(CONSULTANT) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(CONSULTANT,FTE) %>% 
    mutate(MAIN_STAFF_GROUP_NAME="Doctor", STAFF_GROUP='Doctor') %>% 
    select(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER=`TRUE`, OTHER=`FALSE`) %>% 
    ungroup()
  
  others = non_medics %>%
    inner_join(acute_providers) %>% 
   # mutate(MANAGER=grepl("manager",LEVEL,ignore.case=TRUE) & AFC_BAND %in% c("Band 7","Band 8a","Band 8b","Band 8c","Band 8d","Band 9","Very Senior Manager")) %>%
    mutate(MANAGER=grepl("manager",LEVEL,ignore.case=TRUE) ) %>%
    mutate(STAFF_GROUP = if_else(grepl("manager",STAFF_GROUP_1_NAME,ignore.case = TRUE), paste("Manager",gsub("00[1234]_","",AREA)),STAFF_GROUP_1_NAME)) %>% 
    group_by(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(MANAGER,FTE,fill=0) %>% 
    select(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER=`TRUE`, OTHER=`FALSE`) %>% 
    ungroup()
    
  staff_summary = bind_rows(others, doctors) %>% 
    mutate(MAIN_STAFF_GROUP=if_else(grepl("Doctor",STAFF_GROUP),
                                    "Doctor",
                                    if_else(grepl("Nurse",STAFF_GROUP),
                                           "Nurse",
                                                   if_else(grepl("Professionally",MAIN_STAFF_GROUP_NAME),
                                                           "Other clinical",
                                                   "Other non-clinical")))) %>%
    select(MAIN_STAFF_GROUP,STAFF_GROUP, MANAGER, OTHER)
  
  if(percentage){
    staff_summary = staff_summary %>% mutate_if(is.numeric,function(x){x*100/sum(staff_summary[-c(1,2)])})
  }
  
  staff_summary = staff_summary %>% group_by(MAIN_STAFF_GROUP) %>% summarise(MANAGER=sum(MANAGER),OTHER=sum(OTHER)) %>% mutate(TOTAL = MANAGER+OTHER)
 
  staff_summary = rbind(staff_summary, data.frame(MAIN_STAFF_GROUP="Total",t(colSums(staff_summary[,-1]))))
  return(staff_summary)
}

create_summary_tables = function(year=2017, specialist=FALSE, include_outliers=FALSE, all_outcomes=TRUE, all_paygrades=FALSE, file_prefix="basecase"){
  acute_providers = create_sample_providers_dataset(year, specialist, include_outliers, all_outcomes, all_paygrades)
  
  staff_breakdown_numbers = descriptives(acute_providers, FALSE)
  
  write_csv(staff_breakdown_numbers,paste0("figures/",file_prefix,"_staff_breakdown_numbers.csv"))
  
  staff_total =  staff_breakdown_numbers %>% filter(MAIN_STAFF_GROUP!="Total") %>% select(TOTAL) %>% sum()
  
  manager_total = staff_breakdown_numbers %>% filter(!(MAIN_STAFF_GROUP %in% c("Doctor","Total"))) %>% select(MANAGER) %>% sum()
  
  manager_percent = (manager_total/staff_total) * 100
  
  staff_breakdown = descriptives(acute_providers, TRUE)
  
  write_csv(staff_breakdown,paste0("figures/",file_prefix,"_staff_breakdown_percentage.csv"))
  
  manager_percent_check = staff_breakdown %>% filter(!(MAIN_STAFF_GROUP %in% c("Doctor","Total"))) %>% select(MANAGER) %>% sum()
  
  assert_that(manager_percent == manager_percent_check)
  
  print(paste0("Number of managers: ",manager_total," out of: ",staff_total," staff overall (",round(manager_percent,2),"%)"))
  
  managers_by_grade = manager_counts(managers, acute_providers, specialist, year, TRUE)
  
  write_csv(managers_by_grade$counts,paste0("figures/",file_prefix,"_staff_breakdown_by_grade_numbers.csv"))
  
  write_csv(managers_by_grade$percentages,paste0("figures/",file_prefix,"_staff_breakdown_by_grade_percentages.csv"))
  
  manager_total_check = managers_by_grade[["counts"]] %>% filter(STAFF_GROUP=="Total") %>% select("Band 7","Band 8a","Band 8b","Band 8c","Band 8d","Band 9","Very Senior Manager") %>% sum()
  
  if(all_paygrades!=TRUE){
    assert_that(round(manager_total) != round(manager_total_check))
  }
  
  print(paste0("Number of managers at specified pay grades: ",manager_total_check," (",round(100*manager_total_check/staff_total,2),"%)"))
  
  manager_total_check = managers_by_grade[["counts"]] %>% filter(STAFF_GROUP=="Total") %>%  select(Total)
  
  assert_that(abs(manager_total-manager_total_check)<1)
  
  # histograms of key management input variables
  management_code = c("fte", "spend", "man_percent", "nhs_ss")
  management_variables = get_variable(management_code, variable_definitions) %>% arrange(variable)
  graph_data_1 = acute_providers %>% 
    select(management_variables$variable) %>% 
    gather() 
  graph_data_1$key = factor(graph_data_1$key,management_variables$variable,management_variables$label)
  managers_plot = ggplot(data=graph_data_1,aes(x=value)) + 
    geom_histogram(fill="#E69F00", colour="black") + 
    ylab("Count of acute NHS trusts") +
    xlab("") +
    facet_wrap(key ~ ., scales="free", labeller = labeller(key = label_wrap_gen(30))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  ggsave(paste0("figures/",file_prefix,"_manager_numbers.png"), managers_plot, width=16, height=15, units="cm", dpi="print")
  
  # scatter plots of management adjusted by size
  management_code_2 = c("fce_k_per_man","oc_mil_per_man","beds_per_man", "man_percent")
  management_variables_2 = get_variable(management_code_2, variable_definitions) %>% arrange(variable)
  graph_data_2 = acute_providers %>% 
    select(management_variables_2$variable) %>% 
    gather("key", "value", -MANAGERS_PERCENT) 
  graph_data_2$key = factor(graph_data_2$key,management_variables_2$variable,management_variables_2$label)
  
  manager_scatter_plots = ggplot(data=graph_data_2, aes_string(x="MANAGERS_PERCENT", y="value")) +
    xlab("Managers (% of all staff)") + 
    ylab("") +
    geom_point(colour="#E69F00") +
    geom_smooth(method = "lm", colour="darkred", linetype="dashed", size=0.8, se=FALSE) + 
    facet_wrap(.~key, scales = "free", labeller = labeller(key = label_wrap_gen(30))) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   plot.title = element_blank(),
                                   plot.margin = unit(c(1, 1, 1, 1), "lines"),
                                   legend.position="none",
                                   text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  ggsave(paste0("figures/",file_prefix,"_manager_scatter.png"), manager_scatter_plots, width=16, height=10, units="cm", dpi="print")
  
  
  # histograms of outcome variables
  outcomes_code = c("fce_per_non_ae_consultant","fin_pos","ae","rtt","shmi")
  outcomes_variables = get_variable(outcomes_code, variable_definitions) %>% arrange(variable)
  graph_data_3 = acute_providers %>% 
    select(outcomes_variables$variable) %>% 
    gather() 
  graph_data_3$key = factor(graph_data_3$key,outcomes_variables$variable,outcomes_variables$label)
  outcomes_plot = ggplot(data=graph_data_3,aes(x=value)) + 
    geom_histogram(fill="#E69F00", colour="black") + 
    ylab("Count of acute NHS trusts") +
    xlab("") +
    facet_wrap(key ~ ., scales="free", labeller = labeller(key = label_wrap_gen(30))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  ggsave(paste0("figures/",file_prefix,"_outcomes.png"), outcomes_plot, width=20, height=15, units="cm", dpi="print")
  
  #############################################################
  # run regressions for paper on selected subset of dataset 
  # fixed effects with restricted management definition
  #############################################################
  
  dependent_vars = c("fce_per_non_ae_consultant","fin_pos","ae","rtt","shmi")
  
  dependent_vars_log = c("fce_per_non_ae_consultant","ae","rtt","shmi")
  
  independent_vars =c("simple_consultants","nhs_ss","beds","op_cost","fce","casemix")
  
  independent_vars_fte = c("fte", "fte_sq", independent_vars)
  
  independent_vars_spend = c("spend", "spend_sq",independent_vars)

  acute_providers_2016 = create_sample_providers_dataset(2016, FALSE, FALSE, TRUE, FALSE)
  acute_providers_2017 = create_sample_providers_dataset(2017, FALSE, FALSE, TRUE, FALSE)
  acute_providers_paper = bind_rows(acute_providers_2016 %>% filter(ORG_CODE %in% acute_providers_2017$ORG_CODE), 
                                       acute_providers_2017)
  
  regression_results_fte_levels = run_regression(acute_providers_paper, 
                 variable_definitions, 
                 dependent_vars, 
                 c("fte", "fte_sq", independent_vars), 
                 mean_centre=FALSE, 
                 log_dep_vars=FALSE, 
                 log_indep_vars=FALSE, 
                 interactions=FALSE, 
                 output="latex", 
                 specialist=FALSE, 
                 include_outliers=TRUE, 
                 all_outcomes=TRUE, 
                 panel=TRUE)
  sink(file="figures/basecase_fte_regressions.tex")
  cat(regression_results_fte_levels)
  sink()
  
  regression_results_fte_logs = run_regression(acute_providers_paper, 
                                                 variable_definitions, 
                                                 dependent_vars_log, 
                                                 c("fte", independent_vars), 
                                                 mean_centre=FALSE, 
                                                 log_dep_vars=TRUE, 
                                                 log_indep_vars=TRUE, 
                                                 interactions=FALSE, 
                                                 output="latex", 
                                                 specialist=FALSE, 
                                                 include_outliers=TRUE, 
                                                 all_outcomes=TRUE, 
                                                 panel=TRUE)
  sink(file="figures/basecase_fte_log_regressions.tex")
  cat(regression_results_fte_logs)
  sink()
  
  regression_results_spend_levels = run_regression(acute_providers_paper, 
                                                 variable_definitions, 
                                                 dependent_vars, 
                                                 c("spend", "spend_sq", independent_vars), 
                                                 mean_centre=FALSE, 
                                                 log_dep_vars=FALSE, 
                                                 log_indep_vars=FALSE, 
                                                 interactions=FALSE, 
                                                 output="latex", 
                                                 specialist=FALSE, 
                                                 include_outliers=TRUE, 
                                                 all_outcomes=TRUE, 
                                                 panel=TRUE)
  sink(file="figures/basecase_spend_regressions.tex")
  cat(regression_results_spend_levels)
  sink()
  
  regression_results_spend_logs = run_regression(acute_providers_paper, 
                                               variable_definitions, 
                                               dependent_vars_log, 
                                               c("spend", independent_vars), 
                                               mean_centre=FALSE, 
                                               log_dep_vars=TRUE, 
                                               log_indep_vars=TRUE, 
                                               interactions=FALSE, 
                                               output="latex", 
                                               specialist=FALSE, 
                                               include_outliers=TRUE, 
                                               all_outcomes=TRUE, 
                                               panel=TRUE)
  sink(file="figures/basecase_spend_log_regressions.tex")
  cat(regression_results_spend_logs)
  sink()
  #############################################################
  # run regressions for appendix on full dataset with various
  # models with unrestricted management definition + simple def
  #############################################################
  
  acute_providers_2012 = create_sample_providers_dataset(2012, TRUE, TRUE, FALSE, TRUE)
  acute_providers_2013 = create_sample_providers_dataset(2013, TRUE, TRUE, FALSE, TRUE)
  acute_providers_2014 = create_sample_providers_dataset(2014, TRUE, TRUE, FALSE, TRUE)
  acute_providers_2015 = create_sample_providers_dataset(2015, TRUE, TRUE, FALSE, TRUE)
  acute_providers_2016 = create_sample_providers_dataset(2016, TRUE, TRUE, FALSE, TRUE)
  acute_providers_2017 = create_sample_providers_dataset(2017, TRUE, TRUE, FALSE, TRUE)
  acute_providers_appendix = bind_rows(acute_providers_2012, 
                              acute_providers_2013, 
                              acute_providers_2014, 
                              acute_providers_2015, 
                              acute_providers_2016, 
                              acute_providers_2017)
  acute_providers_appendix = acute_providers_appendix %>% mutate(YEAR=as_factor(YEAR), ORG_CODE=as_factor(ORG_CODE))
  
  for (dep_var in dependent_vars) {
    for (indep_var in c("fte","spend")) {
      for (log_var in c(FALSE,TRUE)) {
        if(dep_var=="fin_pos" & log_var) next
        filename = paste("appendix",dep_var,indep_var,ifelse(log_var,"logged","linear"),"regression.tex",sep="_")
        print(paste("generating:",filename))
        appendix_regression(acute_providers_appendix,
                            dependent_var = dep_var,
                            independent_var = indep_var,
                            logged = log_var,
                            output_filename = filename)
      }
    }
  }
  
  
}


produce_figures_for_paper = function(figure_year=2017){
  
  # basecase exclude specialist trusts and outliers 
  # restrict to managers on AFC 7 and above  
  # only include trusts that have data on all outcomes
  create_summary_tables(year=figure_year)

  # results for appendix include all trusts and managers
  create_summary_tables(year=figure_year,
                      specialist=TRUE,
                      all_outcomes=FALSE,
                      all_paygrades=TRUE,
                      include_outliers=TRUE,
                      file_prefix="allmanagers")
}
