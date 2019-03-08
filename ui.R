# Shiny ui for management and hospital performance
# 
# Author: Miqdad Asaria
# Date: 11/10/2018
########################################################

library(shiny)
library(plotly)
library(DT)
source("descriptives.R")

shinyUI(
  navbarPage(theme = "sandstone.css",
	"Management and Hospital Performance",
	

	##### descriptives UI panel ####
	
	tabPanel(
	  "Descriptives",
	  sidebarPanel(

	    
	    selectInput("x_var", 
	                "Input variable to plot on x-axis:",
	                all_vars, 
	                selected="fte_fce"),
	    
	    selectInput("y_var", 
	                "Outcome variable to plot on y-axis:",
	                all_vars, 
	                selected="fin_pos_perc"),
	    
	    selectInput("size_var", 
	                "Adjust point weight according to:",
	                list("# inpatient admissions" = "fce",
	                     "Total operating cost (£)" = "op_cost",
	                     "None" = "none"), 
	                selected="fce"),
	    
	    checkboxInput("specialist", label="Include specialist trusts", value=TRUE),
	    
	    checkboxInput("trend_line", label="Show linear trend line on the plot", value=TRUE),

	    checkboxInput("log_x_var", label="Log the x-axis variable", value=FALSE),

	    checkboxInput("log_y_var", label="Log the y-axis variable", value=FALSE),
	    
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
                     selected=c("afc_7","afc_8a","afc_8b","afc_8c","afc_8d","afc_9","afc_vsm"),
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
                       tabPanel("Staff Numbers", div(dataTableOutput("staff_num_data"), style = "font-size:70%")),
                       tabPanel("Staff Percentages", div(dataTableOutput("staff_percent_data"), style = "font-size:70%")),                       tabPanel("Outcomes", div(dataTableOutput("outcomes_data"), style = "font-size:70%")),
                       tabPanel("Management Measures", div(dataTableOutput("management_data"), style = "font-size:70%")),
                       tabPanel("Explore and Download Data",
                                sidebarPanel(
                                  h4("Plot distributions of variables by acute NHS Trust"),

                                  selectInput("x_var_hist", 
                                              "Variable to plot distribution of:",
                                              all_vars, 
                                              selected="ae"),
                                  checkboxInput("specialist_hist", label="Include specialist hospitals", value=FALSE),
                                  downloadButton("download_raw_data", "Download Full Dataset in CSV format")
                                ),
                                mainPanel(
                                  plotlyOutput("histogram", height="100%", width="100%")
                                )
                       )
              )
                       
  ),

  # end raw data UI panel

  ##### ranking data UI panel ####
tabPanel("Ranking Table", 
         sidebarPanel(
           h4("Rank acute NHS Trusts"),
           
           selectInput("rank_var", 
                       "Rank by:",
                       all_vars),
           downloadButton("download_ranked_data", "Download Ranked Dataset in CSV format")
         ),
         mainPanel(
           div(dataTableOutput("ranking_table"), style = "font-size:70%")
         )
),
  # end ranking panel

  ##### regression UI panel ####

  tabPanel("Regression",
           sidebarPanel(
             selectInput("dependent_vars", 
                         "Dependent variables in the regression:",
                         all_vars, 
                         selected=c("fce","fin_pos","ae","rtt","shmi"),
                         multiple=TRUE),
             selectInput("independent_vars", 
                         "Explanatory and control variables in regression:",
                          c("Case-mix adjust for age and sex" = "casemix",all_vars), 
                         selected=c("fte","nhs_ss","consultants","op_cost","casemix"),
                         multiple=TRUE),
             
             checkboxInput("mean_centre", label="Mean centre covariates", value=FALSE),
             checkboxInput("log_dep_vars", label="Log dependent variables", value=FALSE),
             checkboxInput("log_indep_vars", label="Log independent variables", value=FALSE),
             checkboxInput("interactions", label="Include interaction terms", value=FALSE),
             selectInput("regression_output_type", "Choose a download format:",
                         list("LaTeX" = "latex",
                              "Text" = "text",
                              "HTML" = "html"), 
                         selected=c("latex")),
             
             # Button
             downloadButton("download_regression", "Download Regression Results")
             
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
           
         ),
  # end notes UI panel
  selectInput("year",
            "Input data year",
            list("2017/18"="2017",
                 "2016/17"="2016"),
            selected="2016")
  )
)