# Shiny ui for management and hospital performance
# 
# Author: Miqdad Asaria
# Date: 11/10/2018
########################################################

library(shiny)
library(plotly)
library(DT)

shinyUI(
  navbarPage(theme = "sandstone.css",
	"Management and Hospital Performance",
	
  
	##### descriptives UI panel ####
	
	tabPanel(
	  "Descriptives",
	  sidebarPanel(
	    selectInput("x_var", 
	                "Input variable to plot on x-axis:",
	                list("Management (FTE)" = "fte", 
	                     "Management Spend (£)" = "spend",
	                     "Management (% of all staff)" = "man_percent",
	                     "Management (FTE per £10 million operating cost)" = "fte_per_ten_mil",
	                     "Management (FTE per 1000 admissions)" = "fte_fce", 
	                     "Management Spend (£ per 1000 admissions)" = "spend_fce", 
	                     "Manager to clinical staff ratio" = "fte_clin_ratio",
	                     "NHS staff survey management score" = "nhs_ss", 
	                     "Quality adjusted (FTE per 1000 admissions)" = "fte_quality", 
	                     "Quality adjusted (£ per 1000 admissions)" = "spend_quality",
	                     "Total operating cost" = "op_cost"), 
	                selected="fte_fce"),
	    
	    selectInput("y_var", 
	                "Outcome variable to plot on y-axis:",
	                list("Net financial position (%)" = "fin_pos_perc",
	                     "Net financial position (£)" = "fin_pos",
	                     "Stability index excl. drs in training (%)" = "stability",	                     "A&E target" = "ae",
	                     "RTT target" = "rtt",
	                     "Delayed transfers of care (acute)" = "acute_dtoc",
	                     "Delayed transfers of care (non-acute)" = "non_acute_dtoc",
	                     "Delayed transfers of care (total)" = "total_dtoc",
	                     "Summary Hospital-level Mortality Indicator" = "shmi",
	                     "NHS staff survey management score" = "nhs_ss",
	                     "# inpatient admissions" = "fce",
	                     "Total operating cost (£)" = "op_cost"), 
	                selected="fin_pos_perc"),
	    
	    selectInput("size_var", 
	                "Adjust point weight according to:",
	                list("# inpatient admissions" = "fce",
	                     "Total operating cost (£)" = "op_cost",
	                     "None" = "none"), 
	                selected="fce"),
	    
	    checkboxInput("specialist", label="Include specialist trusts", value=TRUE),
	    
	    checkboxInput("trend_line", label="Show linear trend line on the plot", value=TRUE),
	    
	    selectInput("facet_var", 
	                "Split plots by:",
	                list("CQC overall rating" = "cqc_rating",
	                     "CQC well-led rating" = "cqc_well_led_rating",
	                     "HEE Region" = "hee_region", 
	                     "None" = "none"), 
	                selected="none"),
	    
	    sliderInput("trim", 
	                "Plot trusts with upto managers per 1000 inpatient admissions", 
	                min=0, max=30, value=4),
	    tags$div(
	      HTML("<small><small><img src='lse_logo.svg' alt='London School of Economics and Political Science' width=50%/>
         <p>This site was produced by <a href='https://github.com/miqdadasaria/'>Miqdad Asaria</a> 
         as part of a fellowship funded by the <a href='https://www.health.org.uk'>Health Foundation</a>
         based at the <a href='https://www.lse.ac.uk/'>London School of Economics</a>. 
         <p>Source code can be found <a href='https://github.com/miqdadasaria/hospital_management'>here</a>.
         </small></small>")
	    )
	  ),
	  mainPanel(plotlyOutput("scatter_plot", height="100%", width="100%"))
	),
	
	# end descriptives UI panel
	
	##### management UI panel ####
	
tabPanel("Managment Definition",
         
         sidebarPanel(
           h4("Definition of manager from ESR"),
         selectInput("staff_group", 
                     "Include staff groups:",
                     list("Manager Central functions" = "man_central_functions",
                          "Manager Clinical support" = "man_clinical_support",
                          "Manager Hotel, property & estates" = "man_estates",
                          "Manager Scientific, therapeutic & technical support" = "man_scientific",
                          "Nurses & health visitors" = "nurse",
                          "Scientific, therapeutic & technical staff" = "scientific"), 
                     selected=c("man_central_functions","man_clinical_support","man_estates","man_scientific","nurse","scientific"),
                     multiple=TRUE),
         
         selectInput("pay_grade", 
                     "Include staff on AfC pay grades:",
                     list("Grade 2" = "afc_2",
                          "Grade 3" = "afc_3",
                          "Grade 4" = "afc_4",
                          "Grade 5" = "afc_5",
                          "Grade 6" = "afc_6",
                          "Grade 7" = "afc_7",
                          "Grade 8a" = "afc_8a",
                          "Grade 8b" = "afc_8b",
                          "Grade 8c" = "afc_8c",
                          "Grade 8d" = "afc_8d",
                          "Grade 9" = "afc_9",
                          "Very Senior Manager" = "afc_vsm",
                          "Non AfC Grade" = "afc_nafc"), 
                     selected=c("afc_7","afc_8a","afc_8b","afc_8c","afc_8d","afc_9","afc_vsm","afc_nafc"),
                     multiple=TRUE),
         selectInput("x_var_hist_man", 
                     "Adjust mangers by:",
                     list("# inpatient admissions" = "fte_fce",
                          "Total operating cost (£)" = "fte_per_ten_mil",
                          "As percentage of all staff" = "man_percent",
                          "Unadjusted" = "fte"), 
                     selected="man_percent"),
                     checkboxInput("specialist_hist_man", label="Include specialist hospitals", value=FALSE)
         ),
         mainPanel(
           tabsetPanel(id="tabset",
                       tabPanel("Managers Histogram", plotlyOutput("manager_plot", height="100%", width="100%")),
                       tabPanel("Managers (counts)", div(dataTableOutput("manager_counts"), style = "font-size:70%")),
                       tabPanel("Managers (%)", div(dataTableOutput("manager_percentages"), style = "font-size:70%"))
           )
           
         )
  ),

  # end management UI panel

  ##### raw data UI panel ####

  tabPanel("Raw Data", 
           
           tabsetPanel(id="tabset",
                       tabPanel("Trust Details", div(dataTableOutput("trust_data"), style = "font-size:70%")),
                       tabPanel("Staff", div(dataTableOutput("staff_data"), style = "font-size:70%")),
                       tabPanel("Outcomes", div(dataTableOutput("outcomes_data"), style = "font-size:70%")),
                       tabPanel("Management Measures", div(dataTableOutput("management_data"), style = "font-size:70%")),
                       tabPanel("Plot distributions",
                                sidebarPanel(
                                  h4("Plot distributions of variables by acute NHS Trust"),

                                  selectInput("x_var_hist", 
                                              "Variable to plot distribution of:",
                                              list("A&E 4 hour wait target met (%)" = "ae",
                                                   "Acute delayed transfers of care" = "acute_dtoc",
                                                   "All clinical staff (FTE)" = "all_clinical",
                                                   "All doctors (FTE)" = "doctors",
                                                   "All staff (FTE)" = "all_staff",
                                                   "Consultants (FTE)" = "consultants",
                                                   "Female share of inpatient admissions (%)" = "female",
                                                   "Inpatient admissions age 0-14 (%)" = "age_0_14",
                                                   "Inpatient admissions age 15-29 (%)" = "age_15_29",
                                                   "Inpatient admissions age 30-44 (%)" = "age_30_44",
                                                   "Inpatient admissions age 45-59 (%)" = "age_45_59",
                                                   "Inpatient admissions age 60-74 (%)" = "age_60_74",
                                                   "Inpatient admissions age 75-89 (%)" = "age_75_89",
                                                   "Inpatient admissions age 90+ (%)" = "age_90",
                                                   "Junior Doctors (FTE)" = "junior_doctors",
                                                   "Management (% of all staff)" = "man_percent",
                                                   "Management (FTE per £10 million operating cost)" = "fte_per_ten_mil",
                                                   "Management (FTE per 1000 admissions)" = "fte_fce",
                                                   "Management (FTE)" = "fte",
                                                   "Management Spend (£ per 1000 admissions)" = "spend_fce",
                                                   "Management Spend (£)" = "spend",
                                                   "Manager to clinical staff ratio" = "fte_clin_ratio",
                                                   "Net financial position (% of operating cost)" = "fin_pos_perc",
                                                   "Net financial position (£)" = "fin_pos",
                                                   "NHS staff survey consolidated management score (%)" = "nhs_ss",
                                                   "Non-acute delayed transfers of care" = "non_acute_dtoc",
                                                   "Nurses (FTE)" = "nurses",
                                                   "Other clinical (FTE)" = "other_clinical",
                                                   "Other non-clinical (FTE)" = "other_non_clinical",
                                                   "Quality adjusted (£ per 1000 admissions)" = "spend_quality",
                                                   "Quality adjusted (FTE per 1000 admissions)" = "fte_quality",
                                                   "Referral to treatment in 18 weeks (%)" = "rtt",
                                                   "Stability index excluding doctors in training (%)" = "stability",
                                                   "Summary Hospital-level Mortality Indicator" = "shmi",
                                                   "Total delayed transfers of care" = "total_dtoc",
                                                   "Total inpatient admissions (2016/17)" = "fce",
                                                   "Total Operating Cost (£)" = "op_cost"), 
                                              selected="ae"),
                                  checkboxInput("specialist_hist", label="Include specialist hospitals", value=FALSE)
                                ),
                                mainPanel(
                                  plotlyOutput("histogram", height="100%", width="100%")
                                )
                       )
              )
                       
  ),

  # end raw data UI panel

  ##### regression UI panel ####

  tabPanel("Regression",
           sidebarPanel(
             selectInput("dependent_vars", 
                         "Dependent variables in the regression:",
                         list("Net financial position (£)" = "fin_pos",
                              "Net financial position (% of op cost)" = "fin_pos_perc",
                              "Staff stabilty (%)" = "stability",
                              "A&E target" = "ae",
                              "RTT target" = "rtt",
                              "SHMI" = "shmi",
                              "# inpatient admissions" = "fce"), 
                         selected=c("fin_pos_perc","stability","ae","rtt","shmi"),
                         multiple=TRUE),
             selectInput("independent_vars", 
                         "Explanatory and control variables in regression:",
                         list("Managers (FTE)" = "fte",
                              "Managers (% of all staff)" = "man_percent",
                              "Junior Doctors (FTE)" = "junior_doctors",
                              "Consultants (FTE)" = "consultants",
                              "All doctors (FTE)" = "doctors",
                              "Nurses (FTE)" = "nurses",
                              "Other clinical (FTE)" = "other_clinical",
                              "All clinical staff (FTE)" = "all_clinical",
                              "Other non-clinical (FTE)" = "other_non_clinical",
                              "All staff (FTE)" = "all_staff",
                              "Management Spend (£)" = "spend", 
                              "NHS staff survey management score" = "nhs_ss", 
                              "Total operating cost" = "op_cost",
                              "# inpatient admissions" = "fce",
                              "Case-mix adjust for age and sex" = "casemix",
                              "Specialist status" = "specialist"), 
                         selected=c("fte","all_clinical","other_non_clinical","op_cost"),
                         multiple=TRUE),
             
             checkboxInput("mean_centre", label="Mean centre covariates", value=FALSE),
             checkboxInput("log_dep_vars", label="Log dependent variables", value=FALSE),
             checkboxInput("log_indep_vars", label="Log independent variables", value=FALSE),
             checkboxInput("interactions", label="Include interaction terms", value=FALSE)
           ),
           mainPanel(
             htmlOutput("regression_results")
           )        
    ),
  
  # end raw data UI panel

  ##### notes UI panel ####
  tabPanel("Notes", tags$div(HTML("<p>&nbsp;<p>

          <dl class='dl-horizontal'>
          <dt>Management</dt> 
          <dd>Our management variable is tacken from the NHS ESR and can have a number of definitions.</dd>
          <p>
          
          <dt>A&E Target</dt>
          <dd>Our A&E target variable measures the percentage of patients arriving at A&E who are seen within 4 hours</dd>
          <p>
          </dl>"))
           
         )
  # end notes UI panel
  )
)