library("tidyverse")
library("RSQLite")
library("dbplyr")
library("plotly")
library("stargazer")

db_file = "data/NHS_management.sqlite3"
con = dbConnect(SQLite(), dbname=db_file)

##### define staff groups ####
non_medics = tbl(con, "non_medical_staff") %>% 
  inner_join(tbl(con, "main_staff_group")) %>%
  inner_join(tbl(con, "staff_group_1"))  %>% 
  inner_join(tbl(con, "staff_group_2")) %>%
  select(ORG_CODE, MAIN_STAFF_GROUP_NAME, STAFF_GROUP_1_NAME, STAFF_GROUP_2_NAME, AREA, LEVEL, AFC_BAND, FTE) %>%
  collect()

medics = tbl(con, "medical_staff") %>% collect()

nurses = non_medics %>% 
  filter(grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(NURSES=round(sum(FTE),1))

other_clinical = non_medics %>% 
  filter(!grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE) &
           grepl("Professionally qualified clinical staff",MAIN_STAFF_GROUP_NAME,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(OTHER_CLINICAL_STAFF=round(sum(FTE),1))

others = non_medics %>%
  filter(!grepl("Professionally qualified clinical staff",MAIN_STAFF_GROUP_NAME,ignore.case=TRUE) &
           !grepl("manager",STAFF_GROUP_2_NAME,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(OTHER_STAFF=round(sum(FTE),1))

consultants = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(CONSULTANTS=round(sum(FTE),1))

doctors = medics %>%
  group_by(ORG_CODE) %>%
  summarise(DOCTORS=round(sum(FTE),1))

all_staff = non_medics %>% 
  group_by(ORG_CODE) %>% 
  summarise(NM_FTE=sum(FTE)) %>%
  inner_join(medics %>% 
               group_by(ORG_CODE) %>% 
               summarise(M_FTE=sum(FTE)) ) %>%
  mutate(ALL_STAFF = round(NM_FTE + M_FTE,1)) %>%
  select(ORG_CODE, ALL_STAFF)


junior_doctors = medics %>%
  filter(!grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(JUNIOR_DOCTORS=round(sum(FTE),1))

staff = others %>%
  left_join(nurses) %>%
  left_join(junior_doctors) %>%
  left_join(consultants) %>%
  left_join(doctors) %>%
  left_join(other_clinical) %>%
  left_join(all_staff) %>%
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
  bind_rows(data_frame(YEAR=2017,AFC_BAND=c("Very Senior Manager","Non AfC Grade"),BOTTOM=142500,AVERAGE=142500,TOP=142500)) 

##### construct provider level dataset #### 
providers = tbl(con, "provider") %>%
  left_join(tbl(con, "financial_position") %>% select(ORG_CODE,OP_COST,FINANCIAL_POSITION)) %>%
  left_join(tbl(con, "ae_target") %>% select(ORG_CODE,AE_SCORE)) %>%
  left_join(tbl(con, "rtt_target") %>% select(ORG_CODE,RTT_SCORE)) %>%
  left_join(tbl(con, "dtoc") %>% select(ORG_CODE,ACUTE_DTOC,NON_ACUTE_DTOC,TOTAL_DTOC)) %>%
  left_join(tbl(con, "cqc_rating") %>% filter(POPULATION=="Overall" & QUESTION=="Overall") %>% select(ORG_CODE,RATING_SCORE)) %>%
  left_join(tbl(con, "cqc_rating") %>% filter(POPULATION=="Overall" & QUESTION=="Well-led") %>% select(ORG_CODE,RATING_SCORE_LEADERSHIP=RATING_SCORE)) %>%
  left_join(tbl(con, "nhs_ss_management_score") %>% filter(YEAR==2017 & QUESTION=="overall") %>% mutate(MANAGEMENT_QUALITY=round(((VALUE-1)/4)*100,1)) %>% select(ORG_CODE, MANAGEMENT_QUALITY)) %>%
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2016) %>% select(ORG_CODE, TOTAL_EPISODES_2016 = TOTAL_EPISODES,FEMALE_ADMISSIONS,"0-14","15-29","30-44","45-59","60-74","75-89","90+")) %>%
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2017) %>% select(ORG_CODE, TOTAL_EPISODES_2017 = TOTAL_EPISODES)) %>%
  left_join(tbl(con, "shmi") %>% filter(YEAR==2017) %>% select(ORG_CODE, SHMI)) %>%
  left_join(tbl(con, "stability") %>% filter(YEAR==2017 & STABILITY>0) %>% select(ORG_CODE, STABILITY)) %>%
  left_join(tbl(con, "hee_region")) %>%
  collect() %>%
  filter(ORG_TYPE == "Acute") %>%
  left_join(staff) %>%
  mutate(RATING = factor(RATING_SCORE,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  mutate(RATING_LEADERSHIP = factor(RATING_SCORE_LEADERSHIP,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  mutate(ACUTE_DTOC=round(ACUTE_DTOC,1),NON_ACUTE_DTOC=round(NON_ACUTE_DTOC,1),TOTAL_DTOC=round(TOTAL_DTOC,1)) %>%
  mutate(FEMALE_ADMISSIONS=round(FEMALE_ADMISSIONS/TOTAL_EPISODES_2016,2)*100,
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
  inner_join(providers %>% select(ORG_CODE)) %>% 
  mutate(STAFF_GROUP = if_else(grepl("manager",STAFF_GROUP_1_NAME,ignore.case = TRUE),paste("Manager",gsub("00[1234]_","",AREA)),STAFF_GROUP_1_NAME)) %>% 
  group_by(ORG_CODE,STAFF_GROUP,AFC_BAND) %>% 
  summarise(FTE=sum(FTE)) %>%
  ungroup()

##### import variables to use in data analysis ####
variable_definitions = read_csv("data/variable_definitions.csv")

all_vars=as.list(variable_definitions$code)
names(all_vars)=variable_definitions$label

get_variable = function(var, definitions){
  v = definitions %>% filter(code==var) %>% select(variable, label)
  return(v)
}

##### construct management measure as defined by UI ####
attach_management_measure = function(providers, afc_pay, managers, selected_staff_group, selected_pay_grade){
  
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
    group_by(ORG_CODE) %>%
    summarise(MANAGEMENT_SPEND=sum(MANAGEMENT_SPEND), MANAGERS=round(sum(FTE),1))
  
  
  results = providers %>%
    left_join(managers) %>% 
    mutate(MAN_SPEND_PER_1000_FCE = round(MANAGEMENT_SPEND*1000/TOTAL_EPISODES_2016,0), 
           MAN_FTE_PER_1000_FCE = round(MANAGERS*1000/TOTAL_EPISODES_2016,2),
           MANAGERS_PERCENT = round((MANAGERS/ALL_STAFF)*100,2),
           QUALITY_WEIGHTED_FTE = round(MAN_FTE_PER_1000_FCE*MANAGEMENT_QUALITY/100,2),
           QUALITY_WEIGHTED_SPEND = round(MAN_SPEND_PER_1000_FCE*MANAGEMENT_QUALITY/100,0),
           FIN_POS_PERC = round((FINANCIAL_POSITION/OP_COST)*100,2),
           HEE_REGION_NAME = gsub("Health Education ","",HEE_REGION_NAME),
           ORG_NAME = gsub("NHS Trust","",ORG_NAME),
           ORG_NAME = gsub("NHS Foundation Trust","",ORG_NAME),
           MAN_FTE_PER_TEN_MIL_OC = round((MANAGERS*10000000)/OP_COST,2),
           SPECIALIST = factor(SPECIALIST, levels=c(0,1), labels=c("Non-specialist","Specialist")),
           MANAGER_CLINICIAN_RATIO=round(MANAGERS/CLINICAL_STAFF,2)) %>%
    select(ORG_CODE,
           ORG_NAME,
           SPECIALIST,
           HEE_REGION=HEE_REGION_NAME,
           OPERATING_COST=OP_COST,
           ADMISSIONS=TOTAL_EPISODES_2016,
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
           STABILITY,
           RTT_SCORE,
           AE_SCORE,
           ACUTE_DTOC,
           NON_ACUTE_DTOC,
           TOTAL_DTOC,
           SHMI,
           ALL_STAFF,
           JUNIOR_DOCTORS,
           CONSULTANTS,
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
           MAN_SPEND_PER_1000_FCE,
           QUALITY_WEIGHTED_FTE,
           QUALITY_WEIGHTED_SPEND,
           MANAGER_CLINICIAN_RATIO)  
  return(results)
}


##### scatter plots and histigrams for UI ####
scatter_plot = function(acute_providers, variables, x_var, y_var, size_var, trim, specialist, trend_line, facet_var){
  
  x = get_variable(x_var, variables)
  y = get_variable(y_var, variables)
  if(size_var != "none"){
    s = get_variable(size_var, variables)
  }
  if(facet_var != "none"){
    f = get_variable(facet_var, variables)
  }
  
  graph_data = acute_providers %>% 
    filter(MAN_FTE_PER_1000_FCE<trim)
  
  if(!specialist){
    graph_data = graph_data %>% filter(SPECIALIST=="Non-specialist")
  }
  
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
  
  if(grepl("fin_pos", y_var)){
    plot = plot + geom_hline(yintercept=0, linetype="dashed", colour="darkgrey", size=0.5)
  }
  
  if(grepl("shmi", y_var)){
    plot = plot + geom_hline(yintercept=1, linetype="dashed", colour="darkgrey", size=0.5)
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

histogram_plot = function(providers, variables, x_var, specialist){
  
  x = get_variable(x_var, variables)
  
  graph_data = providers
  if(!specialist){
    graph_data = graph_data %>% filter(SPECIALIST=="Non-specialist")
  }
    
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
  plotly = ggplotly(plot)
  return(plotly)
}

##### tables of desriptives on managers ####
manager_counts = function(managers){
  manager_counts = managers %>% 
    group_by(STAFF_GROUP,AFC_BAND) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(AFC_BAND, FTE, fill = 0)
  
  manager_counts = manager_counts %>%
    bind_rows(manager_counts[-1] %>% 
                summarise_if(is.numeric,sum) %>% 
                bind_cols("STAFF_GROUP"="Total"))
  
  manager_counts = manager_counts %>% 
    bind_cols("Total"=rowSums(manager_counts[,-1]))
  
  manager_percentages = manager_counts %>% mutate_if(is.numeric, function(x){round(x*100/max(manager_counts[,-1]),2)}) 
  
  managers = list("counts" = manager_counts, "percentages" = manager_percentages)
  
  return(managers)
}

##### run regressions ####
run_regression = function(acute_providers, variables, dependent_vars, independent_vars, mean_centre, log_dep_vars, log_indep_vars, interactions, output){
  
  independent_vars_string = vector(mode="character")
  independent_vars_labels = vector(mode="character")
  dependent_vars_labels = vector(mode="character")
  
  if("casemix" %in% independent_vars){
    independent_vars = c(independent_vars[-which(independent_vars %in% "casemix")],"female","age_0_14","age_15_29","age_30_44","age_45_59","age_60_74","age_75_89","age_90")
  }
  
  for(x_var in independent_vars){
    x = get_variable(x_var, variables)
    if(log_indep_vars & acute_providers %>% select(x$variable) %>% summarise_all(is.numeric)){
      indep_var = paste0("log(",x$variable,")")
      indep_var_label = paste0("log ", x$label)
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
  
  independent_vars = acute_providers %>% select(gsub("log\\(|)","",independent_vars_string))
  if(mean_centre){
    independent_vars = independent_vars %>% mutate_if(is.numeric,scale,center=TRUE,scale=FALSE)
  }
  str(independent_vars)
  regressions = list()
  for(y_var in dependent_vars){
    y = get_variable(y_var, variables)
    if(log_dep_vars){
      dep_var = paste0("log(",y$variable,")")
      dep_var_label = paste0("log ", y$label)
    } else {
      dep_var = y$variable
      dep_var_label = y$label
    }
    dependent_vars_labels = c(dependent_vars_labels, dep_var_label)
    reg_formula = as.formula(paste(dep_var,formula_independents,sep=" ~ "))
    regressions[[y$variable]] = lm(reg_formula, data=bind_cols(independent_vars,acute_providers%>%select(y$variable)))
  }
  
  results = paste(capture.output(stargazer(regressions, 
                                           covariate.labels=independent_vars_labels,
                                           dep.var.labels=dependent_vars_labels,
                                           type=output)), collapse="\n") 
  return(results)
}

##### misc descriptives for prenentations / journal articles ####
descriptives = function(providers){
  doctors = medics %>% 
    inner_join(providers) %>% 
    mutate(CONSULTANT=GRADE_SORT_ORDER==1) %>% 
    group_by(CONSULTANT) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(CONSULTANT,FTE) %>% 
    mutate(MAIN_STAFF_GROUP_NAME="Doctor", STAFF_GROUP='Doctor') %>% 
    select(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER=`TRUE`, OTHER=`FALSE`) %>% 
    ungroup()
  
  others = non_medics %>%
    inner_join(providers) %>% 
    mutate(MANAGER=grepl("manager",LEVEL,ignore.case=TRUE)) %>%
    mutate(STAFF_GROUP = if_else(grepl("manager",STAFF_GROUP_1_NAME,ignore.case = TRUE),paste("Manager",gsub("00[1234]_","",AREA)),STAFF_GROUP_1_NAME)) %>% 
    group_by(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER) %>% 
    summarise(FTE=sum(FTE)) %>% 
    spread(MANAGER,FTE,fill=0) %>% 
    select(MAIN_STAFF_GROUP_NAME, STAFF_GROUP, MANAGER=`TRUE`, OTHER=`FALSE`) %>% 
    ungroup()
    
  all_staff = bind_rows(others, doctors) %>% 
    mutate(MAIN_STAFF_GROUP=if_else(grepl("Doctor",STAFF_GROUP),
                                    "Doctor",
                                    if_else(grepl("Nurse",STAFF_GROUP),
                                           "Nurse",
                                                   if_else(grepl("Professionally",MAIN_STAFF_GROUP_NAME),
                                                           "Other clinical",
                                                   "Other non-clinical")))) %>%
    select(MAIN_STAFF_GROUP,STAFF_GROUP, MANAGER, OTHER)
  
  all_staff_percentages = all_staff %>% mutate_if(is.numeric,function(x){x*100/sum(all_staff[-c(1,2)])})
  
  staff_summary = all_staff_percentages %>% group_by(MAIN_STAFF_GROUP) %>% summarise(MANAGER=sum(MANAGER),OTHER=sum(OTHER)) %>% mutate(TOTAL = MANAGER+OTHER)
}
