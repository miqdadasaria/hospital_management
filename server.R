# Shiny server for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
source("descriptives.R")

shinyServer(function(input, output, session) {

  ##### central provider level dataset that drives all outouts ####
  providerData = reactive({
    attach_management_measure(
      providers, 
      afc_pay, 
      managers, 
      input$staff_group, 
      input$pay_grade,
      input$year)
  })

  ##### outputs on descriptives tab ####  
  output$scatter_plot = renderPlotly({
    withProgress(message = paste0('Updating trust scatter plot'),{
      scatter_plot(providerData(), variable_definitions, input$x_var, input$y_var, input$size_var, input$trim, input$specialist, input$trend_line, input$facet_var, input$log_x_var, input$log_y_var)
    })
  })
  
  ##### outputs on management definitions tab ####
  output$manager_plot = renderPlotly({
    withProgress(message = paste0('Updating managers plot'),{
      histogram_plot(providerData(), variable_definitions, input$x_var_hist_man, input$specialist_hist_man)
    })
  })
  
  output$manager_counts = renderDataTable({
    withProgress(message = 'Loading manager summary counts data table',{
      table = manager_counts(managers, input$year)[["counts"]]
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
    })
  })

  output$manager_percentages = renderDataTable({
    withProgress(message = 'Loading manager summary percentages data table',{
      table = manager_counts(managers, input$year)[["percentages"]]
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
    })
  })
  
  ##### outputs on ranking data tab ####
  
  output$ranking_table = renderDataTable({
    withProgress(message = 'Loading ranking data table',{
      table = create_ranking_table(providerData(), variable_definitions, input$rank_var)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 10, autoWidth = TRUE, dom='ftrpi'))
    })
  })
  
  
  ##### outputs on raw data tab ####
  
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
	    table = table %>% select(c(2,15:25))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$staff_num_data = renderDataTable({
	  withProgress(message = 'Loading staff data table',{
	    table = providerData()
	    table = table %>% select(c(2,26:37))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})

	output$staff_percent_data = renderDataTable({
	  withProgress(message = 'Loading staff data table',{
	    table = providerData()
	    table = table %>% select(c(2,38:45))
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
	    table = table %>% select(c(2,37,45:53))
	    datatable(table,
	              style = 'bootstrap',
	              rownames = FALSE,
	              colnames = gsub("_"," ",colnames(table)),
	              options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$histogram = renderPlotly({
	  withProgress(message = paste0('Updating variable distribution plot'),{
	    histogram_plot(providerData(), variable_definitions, input$x_var_hist, input$specialist_hist)
	  })
	})
	
	output$download_raw_data <- downloadHandler(
	  filename = paste0("raw_data_",input$year,".csv"),
	  content = function(file) {
	    results = providerData()
	    write_csv(results,file)
	  }
	)
	
	##### outputs on regressions tab ####
	
	output$regression_results = renderText({
	  withProgress(message = paste0('Calculating regression results'),{
	    run_regression(providerData(), variable_definitions, input$dependent_vars, input$independent_vars, input$mean_centre, input$log_dep_vars, input$log_indep_vars, input$interactions, "html")
	  })
	})
	
	output$download_regression <- downloadHandler(
	  filename = paste0("regression_results_",input$year,".txt"),
	  content = function(file) {
	    results = run_regression(providerData(), variable_definitions, input$dependent_vars, input$independent_vars, input$mean_centre, input$log_dep_vars, input$log_indep_vars, input$interactions, input$regression_output_type)
	    cat(results,file=file)
	  }
	)
	
})
