# Shiny server for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
source("descriptives.R")

shinyServer(function(input, output, session) {
  
  providerData = reactive({
    attach_management_measure(
      providers, 
      afc_pay, 
      managers, 
      input$staff_group, 
      input$pay_grade)
  })
  
  output$scatter_plot = renderPlotly({
    withProgress(message = paste0('Updating trust scatter plot'),{
      scatter_plot(providerData(), variable_definitions, input$x_var, input$y_var, input$size_var, input$trim, input$specialist, input$trend_line, input$facet_var)
    })
  })
  
  output$manager_plot = renderPlotly({
    withProgress(message = paste0('Updating managers plot'),{
      manager_plot(providerData())
    })
  })
  
  output$manager_counts = renderDataTable({
    withProgress(message = 'Loading manager summary counts data table',{
      table = manager_counts(managers)[["counts"]]
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
    })
  })

  output$manager_percentages = renderDataTable({
    withProgress(message = 'Loading manager summary percentages data table',{
      table = manager_counts(managers)[["percentages"]]
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
    })
  })
  
    
	output$trust_data = renderDataTable({
	  withProgress(message = 'Loading trust data table',{
	    table = providerData()
	    table = table %>% select(1:14)
	    datatable(table,
        style = 'bootstrap',
	      rownames = FALSE,
	      colnames = gsub("_"," ",colnames(table)),
	      options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$outcomes_data = renderDataTable({
	  withProgress(message = 'Loading outcomes data table',{
	    table = providerData()
	    table = table %>% select(c(2,15:23))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$staff_data = renderDataTable({
	  withProgress(message = 'Loading staff data table',{
	    table = providerData()
	    table = table %>% select(c(2,24:31))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$management_data = renderDataTable({
	  withProgress(message = 'Loading management data table',{
	    table = providerData()
	    table = table %>% select(c(2,31:39))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$regression_results = renderText({
	  withProgress(message = paste0('Calculating regression results'),{
	    run_regression(providerData(), variable_definitions, input$dependent_vars, input$independent_vars, input$mean_centre, input$log_dep_vars, input$log_indep_vars, input$interactions)
	  })
	})
	
})
