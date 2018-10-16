# Shiny ui for management and hospital performance
# 
# Author: Miqdad Asaria
# Date: 11/10/2018
########################################################

library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(theme = "sandstone.css",
			
  tags$head(tags$meta(name="description", content="Compare NHS Hospital performance based on quantities and quality of management input.")),
  
	titlePanel("Management and Hospital Performance"),
				
	sidebarPanel(
	    selectInput("x_var", 
	              "Input variable to plot on x-axis:",
	              list("Management (FTE)" = "fte", 
	                   "Management Spend (£)" = "spend", 
	                   "Management (FTE per £10 million operating cost)" = "fte_per_ten_mil",
	                   "Management (FTE per 1000 admissions)" = "fte_fce", 
	                   "Management Spend (£ per 1000 admissions)" = "spend_fce", 
	                   "NHS staff survey management score" = "nhs_ss", 
	                   "Quality adjusted (FTE per 1000 admissions)" = "fte_quality", 
	                   "Quality adjusted (£ per 1000 admissions)" = "spend_quality",
	                   "Total operating cost" = "nhs_ss"), 
	              selected="fte_fce"),
	  
	  selectInput("y_var", 
	              "Outcome variable to plot on y-axis:",
	              list("Net financial position (%)" = "fin_pos_perc",
	                   "Net financial position (£)" = "fin_pos",
	                   "A&E target" = "ae",
	                   "RTT target" = "rtt",
	                   "Delayed transfers of care (acute)" = "acute_dtoc",
	                   "Delayed transfers of care (non-acute)" = "non_acute_dtoc",
	                   "Delayed transfers of care (total)" = "total_dtoc",
	                   "NHS staff survey management score" = "nhs_ss",
	                   "# inpatient admissions" = "fce",
	                   "overall operating cost (£)" = "op_cost"), 
	              selected="fin_pos_perc"),
	  
	  selectInput("size_var", 
	              "Adjust point size according to:",
	              list("# inpatient admissions" = "fce",
	                   "overall operating cost (£)" = "op_cost",
	                   "None" = "none"), 
	              selected="linear"),
	  
	  sliderInput("trim", 
	              "Plot trusts with upto managers per 1000 inpatient admissions", 
	              min=0, max=31, value=4),
	  
	  checkboxInput("trend_line", label="Show linear trend line on the plot", value=TRUE),

	  checkboxInput("specialist", label="Include specialist trusts", value=TRUE),
	  	
	  selectInput("facet_var", 
	              "Split plots by:",
	              list("CQC overall rating" = "cqc_rating",
	                   "CQC well-led rating" = "cqc_well_led_rating",
	                   "HEE Region" = "hee_region", 
	                   "None" = "none"), 
	              selected="none"),

	  h4("Definition of manager from ESR"),
	  checkboxInput("afc_8", label="Restrict to those on AFC grade 8 and above", value=TRUE),
	  checkboxInput("level_1_manager", label="Restrict to those whose primary job title is manager", value=FALSE),
	  checkboxInput("central_functions", label="Restrict to those working in area central functions", value=FALSE),

	  tags$div(
	    HTML("<small><small><img src='lse_logo.svg' alt='London School of Economics and Political Science' width=90%/>
           <p>This site was produced by <a href='https://www.york.ac.uk/che/staff/research/miqdad-asaria/'>Miqdad Asaria</a> 
           as part of a fellowship funded by the <a href='https://www.health.org.uk'>Health Foundation</a>
           based at the <a href='https://www.lse.ac.uk/'>London School of Economics</a>. 
	         <p>Source code can be found <a href='https://github.com/miqdadasaria/hospital_management'>here</a>.
	         </small></small>")
	  )
	  
	),
	
	mainPanel(
	      h3(textOutput("title")),
	      textOutput("ccg"),
	      tabsetPanel(id="tabset",
	        tabPanel("Scatter", plotlyOutput("scatter_plot")),
				  tabPanel("Table", div(dataTableOutput("trust_data"), style = "font-size:70%")),
				  tabPanel("Notes", tags$div(HTML("<p>&nbsp;<p>

                                           <dl class='dl-horizontal'>
				                                   <dt>Management</dt> 
                                           <dd>Our management variable is tacken from the NHS ESR and can have a number of definitions.</dd>
                                           <p>

                                           <dt>A&E Target</dt>
				                                   <dd>Our A&E target variable measures the percentage of patients arriving at A&E who are seen within 4 hours</dd>
                                           <p>
				                                  </dl>"
				                                  )))
				),
				tags$div(
				  HTML("<small><small><small><p>&nbsp;<p>
                <p>Contains National Statistics data © Crown copyright and database right 2016. 
                Contains OS data © Crown copyright and database right 2016.
				        Data licensed under the <a href='http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/'>Open Government Licence v3.0</a></small></small></small>")
				)
					
		)
))