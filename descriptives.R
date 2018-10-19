library("tidyverse")
library("RSQLite")
library("dbplyr")
library("plotly")
library("stargazer")

db_file = "data/NHS_management.sqlite3"
con = dbConnect(SQLite(), dbname=db_file)

# summarise descriptives of different definitions of manager
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
  mutate(CLINICAL_STAFF=DOCTORS+NURSES+OTHER_CLINICAL_STAFF)

afc_pay = tbl(con, "pay_scale") %>% 
  collect() %>%
  bind_rows(data_frame(YEAR=2017,AFC_BAND=c("Very Senior Manager","Non AfC Grade"),BOTTOM=142500,AVERAGE=142500,TOP=142500)) 

# provider information and performance
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

variable_definitions = read_csv("data/variable_definitions.csv")

get_variable = function(var, definitions){
  v = definitions %>% filter(code==var) %>% select(variable, label)
  return(v)
}

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
           RTT_SCORE,
           AE_SCORE,
           ACUTE_DTOC,
           NON_ACUTE_DTOC,
           TOTAL_DTOC,
           JUNIOR_DOCTORS,
           CONSULTANTS,
           DOCTORS,
           NURSES,
           OTHER_CLINICAL_STAFF,
           CLINICAL_STAFF,
           OTHER_STAFF,
           MANAGERS,
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
  
  plot = plot + theme_bw() + theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(), 
                            plot.title = element_blank(),
                            plot.margin = unit(c(1, 1, 1, 1), "lines"),
                            legend.position="none",
                            text=element_text(family = "Roboto", colour = "#3e3f3a"))
  plotly = ggplotly(plot)
  return(plotly)
}

manager_plot = function(providers){
  plot = ggplot(data=providers,aes(x=MANAGERS)) + 
    geom_histogram(fill="#E69F00", colour="black") + 
    xlab("Number of Managers by NHS Trust (FTE)") +
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

run_regression = function(acute_providers, variables, dependent_vars, independent_vars, mean_centre){
  
  independent_vars_string = vector(mode="character")
  independent_vars_labels = vector(mode="character")
  dependent_vars_labels = vector(mode="character")
  
  if("casemix" %in% independent_vars){
    independent_vars = c(independent_vars[-which(independent_vars %in% "casemix")],"female","age_0_14","age_15_29","age_30_44","age_45_59","age_60_74","age_75_89","age_90")
  }
  
  for(x_var in independent_vars){
      x = get_variable(x_var, variables)
      independent_vars_string = c(independent_vars_string, x$variable)
      independent_vars_labels = c(independent_vars_labels, x$label)
  }
  
  formula_independents = paste(independent_vars_string, collapse=" + ")
  
  independent_vars = acute_providers %>% select(independent_vars_string)
  if(mean_centre){
    independent_vars = independent_vars %>% mutate_if(is.numeric,scale,center=TRUE,scale=FALSE)
  }
  
  regressions = list()
  for(y_var in dependent_vars){
    y = get_variable(y_var, variables)
    dependent_vars_labels = c(dependent_vars_labels, y$label)
    reg_formula = as.formula(paste(y$variable,formula_independents,sep=" ~ "))
    regressions[[y$variable]] = lm(reg_formula, data=bind_cols(independent_vars,acute_providers%>%select(y$variable)))
  }
  
  results = paste(capture.output(stargazer(regressions, 
                                           covariate.labels=independent_vars_labels,
                                           dep.var.labels=dependent_vars_labels,
                                           type="html")), collapse="") 
  return(results)
}

                   