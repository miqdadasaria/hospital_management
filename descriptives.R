library("tidyverse")
library("RSQLite")
library("dbplyr")
library("plotly")

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

managers = non_medics %>% filter(grepl("manager",LEVEL,ignore.case=TRUE))

nurses = non_medics %>% 
  filter(grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(NURSES=sum(FTE))

others = non_medics %>%
  filter(!grepl("nurse",STAFF_GROUP_2_NAME,ignore.case=TRUE) &
           !grepl("manager",STAFF_GROUP_2_NAME,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(OTHER_STAFF=sum(FTE))
  
consultants = medics %>%
  filter(grepl("consultant",GRADE,ignore.case=TRUE)) %>%
  group_by(ORG_CODE) %>%
  summarise(CONSULTANTS=sum(FTE))

doctors = medics %>%
  group_by(ORG_CODE) %>%
  summarise(DOCTORS=sum(FTE))

staff = others %>%
  left_join(nurses) %>%
  left_join(consultants) %>%
  left_join(doctors) %>%
  mutate(CLINICAL_STAFF=DOCTORS+NURSES)


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
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2016) %>% select(ORG_CODE, TOTAL_EPISODES_2016 = TOTAL_EPISODES)) %>%
  left_join(tbl(con, "inpatient_data") %>% filter(YEAR==2017) %>% select(ORG_CODE, TOTAL_EPISODES_2017 = TOTAL_EPISODES)) %>%
  left_join(tbl(con, "hee_region")) %>%
  collect() %>%
  left_join(staff) %>%
  mutate(RATING = factor(RATING_SCORE,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  mutate(RATING_LEADERSHIP = factor(RATING_SCORE_LEADERSHIP,levels=c(0,1,2,3), labels=c("Inadequate","Requires improvement","Good","Outstanding"))) %>%
  filter(ORG_TYPE == "Acute") 
 
dbDisconnect(con)

attach_management_measure = function(providers, afc_pay, non_medics, medics, specialist, afc_8, level_1_manager, central_functions){
  managers = non_medics %>% 
    group_by(ORG_CODE, MAIN_STAFF_GROUP_NAME, STAFF_GROUP_1_NAME, STAFF_GROUP_2_NAME, AREA, LEVEL, AFC_BAND)
  
  if(afc_8){
    managers = managers %>%
      filter(grepl("Band 8", AFC_BAND) | grepl("Band 9", AFC_BAND) | grepl("Very", AFC_BAND) | grepl("Non AfC", AFC_BAND))
  } 
  
  if(level_1_manager){
    managers = managers %>%
      filter(grepl("Managers", STAFF_GROUP_1_NAME) | grepl("Senior managers", STAFF_GROUP_1_NAME))
  }
  
  if(central_functions){
    managers = managers %>%
      filter(grepl("Central", AREA))
  }

  managers = managers %>%
    summarise(FTE = sum(FTE)) %>%
    left_join(afc_pay %>% select(AFC_BAND,PAY=TOP)) %>%
    mutate(MANAGEMENT_SPEND=round(FTE*PAY)) %>%
    ungroup()
  
  results = providers
  
  if(!specialist){
    results = results %>% filter(SPECIALIST==FALSE)
  }
  
  results = results %>%
    left_join(managers %>% group_by(ORG_CODE) %>% 
              summarise(MANAGEMENT_SPEND=sum(MANAGEMENT_SPEND), MANAGEMENT_FTE=sum(FTE))) %>%
    mutate(MAN_SPEND_PER_1000_FCE = round(MANAGEMENT_SPEND*1000/TOTAL_EPISODES_2016,0), 
           MAN_FTE_PER_1000_FCE = round(MANAGEMENT_FTE*1000/TOTAL_EPISODES_2016,2),
           QUALITY_WEIGHTED_FTE = round(MAN_FTE_PER_1000_FCE*MANAGEMENT_QUALITY/100,2),
           QUALITY_WEIGHTED_SPEND = round(MAN_SPEND_PER_1000_FCE*MANAGEMENT_QUALITY/100,0),
           FIN_POS_PERC = round((FINANCIAL_POSITION/OP_COST)*100,2),
           HEE_REGION_NAME = gsub("Health Education ","",HEE_REGION_NAME),
           ORG_NAME = gsub("NHS Trust","",ORG_NAME),
           ORG_NAME = gsub("NHS Foundation Trust","",ORG_NAME),
           MAN_FTE_PER_TEN_MIL_OC = round((MANAGEMENT_FTE*10000000)/OP_COST,2),
           SPECIALIST = factor(SPECIALIST, levels=c(0,1), labels=c("Non-specialist","Specialist")),
           MANAGER_CLINICIAN_RATIO=MANAGEMENT_FTE/CLINICAL_STAFF) %>%
    select(ORG_CODE,
           ORG_NAME,
           SPECIALIST,
           HEE_REGION=HEE_REGION_NAME,
           ADMISSIONS=TOTAL_EPISODES_2016,
           CQC_RATING=RATING,
           CQC_WELL_LED_RATING=RATING_LEADERSHIP,
           OPERATING_COST=OP_COST,
           NET_FINANCIAL_POSITION=FINANCIAL_POSITION,
           NET_FINANCIAL_POSITION_PERCENT=FIN_POS_PERC,
           RTT_SCORE,
           AE_SCORE,
           ACUTE_DTOC,
           NON_ACUTE_DTOC,
           TOTAL_DTOC,
           NHS_SS=MANAGEMENT_QUALITY,
           CONSULTANTS,
           DOCTORS,
           NURSES,
           CLINICAL_STAFF,
           OTHER_STAFF,
           MANAGEMENT_FTE,
           MANAGEMENT_SPEND,
           MAN_FTE_PER_TEN_MIL_OC,
           MAN_FTE_PER_1000_FCE,
           MAN_SPEND_PER_1000_FCE,
           QUALITY_WEIGHTED_FTE,
           QUALITY_WEIGHTED_SPEND,
           MANAGER_CLINICIAN_RATIO)  
  return(results)
}

scatter_plot = function(acute_providers, x_var, y_var, size_var, trim, trend_line, facet_var){
  
  variables = data_frame(
    code=c("fte_clin_ratio","fte_per_ten_mil","fin_pos_perc","op_cost","fin_pos","acute_dtoc","non_acute_dtoc","total_dtoc","rtt","ae","cqc_rating","cqc_well_led_rating","hee_region","nhs_ss","fce","fte","spend","fte_fce","spend_fce","fte_quality","spend_quality"),
    variable=c("MANAGER_CLINICIAN_RATIO","MAN_FTE_PER_TEN_MIL_OC","NET_FINANCIAL_POSITION_PERCENT","OPERATING_COST","NET_FINANCIAL_POSITION","ACUTE_DTOC","NON_ACUTE_DTOC","TOTAL_DTOC","RTT_SCORE","AE_SCORE","CQC_RATING","CQC_WELL_LED_RATING","HEE_REGION","NHS_SS","ADMISSIONS","MANAGEMENT_FTE","MANAGEMENT_SPEND","MAN_FTE_PER_1000_FCE","MAN_SPEND_PER_1000_FCE","QUALITY_WEIGHTED_FTE","QUALITY_WEIGHTED_SPEND"),
    label=c("Manager to clinical staff ratio","Management (FTE per £10 million operating cost)","Net financial position (%)","Total Operating Cost (£)","Net financial position (£)","Acute delayed transfers of care","Non-acute delayed transfers of care","Total delayed transfers of care","Referral to treatment in 18 weeks (%)","A&E 4 hour wait target met (%)","CQC Overall Rating","CQC Well Led Rating","Health Education England Region","NHS staff survey consolidated management score","Total inpatient admissions (2016/17)","Management (FTE)","Management Spend (£)","Management (FTE per 1000 admissions)","Management Spend (£ per 1000 admissions)","Quality adjusted (FTE per 1000 admissions)","Quality adjusted (£ per 1000 admissions)")
  )
  
  x = variables %>% filter(code==x_var) %>% select(variable, label)
  y = variables %>% filter(code==y_var) %>% select(variable, label)
  if(size_var != "none"){
    s = variables %>% filter(code==size_var) %>% select(variable)
  }
  if(facet_var != "none"){
    f = variables %>% filter(code==facet_var) %>% select(variable)
  }
  
  graph_data = acute_providers %>% 
    filter(MAN_FTE_PER_1000_FCE<trim)
  
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