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
      scatter_plot(providerData(), variable_definitions, input$x_var, input$y_var, input$size_var, input$specialist, input$trend_line, input$facet_var, input$log_x_var, input$log_y_var, input$year, input$show_titles, input$outliers)
    })
  })
  
  ##### outputs on management definitions tab ####
  output$manager_plot = renderPlotly({
    withProgress(message = paste0('Updating managers plot'),{
      histogram_plot(providerData(), variable_definitions, input$x_var_hist_man, input$specialist_hist_man, input$year, input$show_titles_hist, input$outliers_man)
    })
  })
  
  output$manager_counts = renderDataTable({
    withProgress(message = 'Loading manager summary counts data table',{
      table = manager_counts(managers, providerData(), input$specialist_hist_man, input$year, input$outliers_man)[["counts"]]
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 8, autoWidth = TRUE, dom='ftrpi'))
    })
  })

  output$manager_percentages = renderDataTable({
    withProgress(message = 'Loading manager summary percentages data table',{
      table = manager_counts(managers, providerData(), input$specialist_hist_man, input$year, input$outliers_man)[["percentages"]]
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
  
  output$download_ranked_data <- downloadHandler(
    filename = function() {
      paste0("ranked_data_",input$rank_var,"_",input$year,".csv")},
    content = function(file) {
      print(input$rank_var)
      table = create_ranking_table(providerData(), variable_definitions, input$rank_var)
      write_csv(table,file)
    }
  )
  
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
	    histogram_plot(providerData(), variable_definitions, input$x_var_hist, input$specialist_hist, input$year, input$show_titles_hist, input$outliers_hist)
	  })
	})
	
	output$download_raw_data <- downloadHandler(
	  filename = function() {
	    paste0("raw_data_",input$year,".csv")},
	  content = function(file) {
	    results = providerData()
	    write_csv(results,file)
	  }
	)
	
	
	##### outputs on regressions tab ####
	
	output$regression_results = renderText({
	  withProgress(message = paste0('Calculating regression results'),{
	    run_regression(providerData(), variable_definitions, input$dependent_vars, input$independent_vars, input$log_dep_vars, input$log_indep_vars, input$interactions, "html", input$specialist_reg, input$outliers_reg, input$all_outcomes_reg, (input$fixed_effects & input$year=="pooled"), input$year, input$labels_reg, input$balanced)
	  })
	})
	
	output$download_regression <- downloadHandler(
	  filename = function() {
	    paste0("regression_results_",input$year,".txt")},
	  content = function(file) {
	    results = run_regression(providerData(), variable_definitions, input$dependent_vars, input$independent_vars, input$log_dep_vars, input$log_indep_vars, input$interactions, input$regression_output_type, input$specialist_reg, input$outliers_reg, input$all_outcomes_reg, (input$fixed_effects & input$year=="pooled"), input$year, input$labels_reg, input$balanced)
	    cat(results,file=file)
	  }
	)
	
})
