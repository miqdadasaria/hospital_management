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
      scatter_plot(providerData(), input$x_var, input$y_var, input$size_var, input$trim, input$specialist, input$trend_line, input$facet_var)
    })
  })
  
	
	output$trust_data = renderDataTable({
	  withProgress(message = 'Loading trust data table',{
	    table = providerData()
	    datatable(table,
        style = 'bootstrap',
	      rownames = FALSE,
	      colnames = gsub("_"," ",colnames(table)),
	      options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
})
