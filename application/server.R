## server.R script ##
source("global.R")

function(input, output, session) {

	# Make data selectors on the fly
	output$loadData = shiny::renderUI({
		switch(input$typeDataUpload,
					 "uploadData" = shiny::fileInput(inputId = "uploadData", label = NULL),
					 "linkData" = shiny::textInput(inputId = "linkData", label = NULL),
					 "pasteData" = shiny::textAreaInput(inputId = "pasteData", label = NULL))
	})

	# Functions to load data
	data = shiny::reactive({
		inFile = switch(input$typeDataUpload,
										"uploadData" = input$uploadData,
										"linkData" = input$linkData,
										"pasteData" = input$pasteData)
		df = if (is.null(inFile)) {
			return(NULL)
		} else {
			ptn = "\\.[[:alnum:]]{1,8}$"
			suf = tolower(regmatches(inFile$name, regexpr(ptn, inFile$name)))
			switch(suf,
						 ".csv" = readr::read_csv(inFile$datapath),
						 ".dta" = haven::read_dta(inFile$datapath),
						 ".sav" = haven::read_sav(inFile$datapath),
						 ".sas7bdat" = haven::read_sas(inFile$datapath),
						 ".rds" = readRDS(inFile$datapath))
		}

		# Return dataset
		return(df)
	})

	# Make variable selectors
	output$defineVariables = shiny::renderUI({
		shiny::validate(shiny::need(!is.null(data()), "Upload a dataset first."))
		tagList(
			div(style = "display: inline-block; vertical-align: top; width: 19%; min-width: 150px",
					shiny::selectInput(inputId = "defineEstvarname",
														 label = "Point estimates:",
														 choices = names(data()))),
			div(style = "display: inline-block; vertical-align: top; width: 19%; min-width: 150px",
					shiny::selectInput(inputId = "defineSe",
														 label = "Standard errors:",
														 choices = names(data()))),
			div(style = "display: inline-block; vertical-align: top; width: 19%; min-width: 150px",
					shiny::numericInput(inputId = "defineTrue",
															label = "True value:",
															value = 0)),
			div(style = "display: inline-block; vertical-align: top; width: 19%; min-width: 150px",
					shiny::selectInput(inputId = "defineMethod",
														 label = "Methods:",
														 choices = names(data()),
														 multiple = TRUE)),
			div(style = "display: inline-block; vertical-align: top; width: 19%; min-width: 150px",
					shiny::selectInput(inputId = "defineBy",
														 label = "By factors:",
														 choices = names(data()),
														 multiple = TRUE))
		)
	})

	# Make a data table with the original dataset
	output$uploadedDataTable = shiny::renderDataTable({
		shiny::validate(shiny::need(!is.null(data()), "Upload a dataset first, using the 'data' tab."))
		data()
	})

	# Make a reference method selector for the summary statistics table
	output$summaryRefMethodSelector = shiny::renderUI({
		if (length(input$defineMethod) > 0) shiny::selectInput(inputId = "summaryRefMethodSelector",
																													 label = "Select reference method:",
																													 choices = unique(data()[[input$defineMethod[1]]]))
	})

	# Make summary statistics
	summ = shiny::reactive({
		s = rsimsum::simsum(data = data(),
							 estvarname = input$defineEstvarname,
							 true = input$defineTrue,
							 se = input$defineSe,
							 methodvar = input$defineMethod[1],
							 ref = input$summaryRefMethodSelector)
		return(s)
	})

	# Make a data table with the summary statistics
	output$summaryStatisticsDataTable = shiny::renderDataTable({
		summ()$summ
	})

	# Make summary table in LaTeX
	output$summaryStatisticsLaTeX = shiny::renderPrint({
		print(xtable::xtable(x = summ()$summ, caption = input$summaryStatisticsLaTeXCaption, digits = input$summaryStatisticsLaTeXDigits))
	})

	# Download data.frame with summary statistics
	output$exportSummaryStatisticsButton = shiny::downloadHandler(
		filename = function() {
			extension = switch(input$exportSummaryStatisticsType,
												 "csv" = ".csv",
												 "tsv" = ".tsv",
												 "r" = ".rds",
												 "stata" = ".dta",
												 "spss" = ".sav",
												 "sas" = ".sas7bdat")
			paste0(input$exportSummaryStatisticsName, extension)
		},
		content = function(file) {
			if (input$exportSummaryStatisticsType == "csv") {
				readr::write_csv(x = summ()$summ, path = file)
			} else if (input$exportSummaryStatisticsType == "tsv") {
				readr::write_tsv(x = summ()$summ, path = file)
			} else if (input$exportSummaryStatisticsType == "r") {
				saveRDS(object = summ()$summ, file = file)
			} else if (input$exportSummaryStatisticsType == "stata") {
				haven::write_dta(data = summ()$summ, path = file)
			} else if (input$exportSummaryStatisticsType == "spss") {
				haven::write_sav(data = summ()$summ, path = file)
			} else if (input$exportSummaryStatisticsType == "sas") {
				haven::write_sas(data = summ()$summ, path = file)
			}
		}
	)

	# Make facets selectors
	output$plotFacetX = shiny::renderUI({
		shiny::validate(shiny::need(!is.null(data()), "Upload a dataset first, using the 'data' tab."))
		shiny::selectInput(inputId = "plotFacetX",
											 label = "Select X-axis faceting variable:",
											 choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
											 multiple = TRUE)
	})
	output$plotFacetY = shiny::renderUI({
		shiny::validate(shiny::need(!is.null(data()), "Upload a dataset first, using the 'data' tab."))
		shiny::selectInput(inputId = "plotFacetY",
											 label = "Select Y-axis faceting variable:",
											 choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
											 multiple = TRUE)
	})


	# Make estimates plot
	makePlotEstimates = function() {
		switch(input$selectPlotEstimates,
					 "b_vs_se" =  {
					 	plot = ggplot(data(), aes_string(x = input$defineSe, y = input$defineEstvarname)) +
					 		geom_point() +
					 		theme_bw() +
					 		labs(x = input$customXlab, y = input$customYlab)
					 	# if (!is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
					 	# 	plot = plot + facet_grid(reformulate(input$plotFacetX, input$plotFacetY), labeller = label_both)
					 	# } else if (is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
					 	# 	plot = plot + facet_grid(reformulate(".", input$plotFacetY), labeller = label_both)
					 	# } else if (!is.null(input$plotFacetX) & is.null(input$plotFacetY)) {
					 	# 	plot = plot + facet_grid(reformulate(input$plotFacetX, "."), labeller = label_both)
					 	# }
					 	plot
					 })
	}

	# Print estimates plot
	output$outPlotEstimates = renderPlot({
		makePlotEstimates()
	})

	# Make summaries plot

	makePlotSummary = function() {

		df = out() %>%
			rename(yy = !!input$select_sstat)

		if (input$select_sstat %in% c("bias", "cov")) {
			df = rename(df, yy_mcse = !!(paste0(input$select_sstat, "_mcse")))
		}

		switch(input$select_plot,
					 "barplot" = {
					 	plot = ggplot(df, aes(x = method, y = yy)) +
					 		geom_bar(stat = "identity") +
					 		theme_bw() +
					 		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
					 		labs(x = input$custom_xlab, y = input$custom_ylab)
					 	if (input$select_sstat %in% c("bias", "cov")) {
					 		plot = plot +
					 			geom_errorbar(aes(ymin = yy - 1.96 * yy_mcse, ymax = yy + 1.96 * yy_mcse), width = 1/3)
					 	}
					 	plot
					 },
					 "lollyplot" = {
					 	plot = ggplot(df, aes(y = method, x = yy)) +
					 		geom_vline(xintercept = case_when(input$select_sstat == "convergence_n" ~ 1000,
					 																			input$select_sstat == "convergence_p" ~ 1,
					 																			input$select_sstat %in% c("bias", "pbias", "esd", "mse") ~ 0,
					 																			input$select_sstat == "cov" ~ 95),
					 							 color = "red", lty = "dotted") +
					 		geom_segment(aes(yend = method,
					 										 xend = case_when(input$select_sstat == "convergence_n" ~ 1000,
					 										 								 input$select_sstat == "convergence_p" ~ 1,
					 										 								 input$select_sstat %in% c("bias", "pbias", "esd", "mse") ~ 0,
					 										 								 input$select_sstat == "cov" ~ 95)), color = "grey50") +
					 		geom_point() +
					 		theme_bw() +
					 		labs(x = input$custom_xlab, y = input$custom_ylab)
					 	if (input$select_sstat %in% c("bias", "cov")) {
					 		plot = plot +
					 			geom_point(aes(x = yy - 1.96 * yy_mcse), shape = "(") +
					 			geom_point(aes(x = yy + 1.96 * yy_mcse), shape = ")")
					 	}
					 	plot
					 })
	}
	#
	#
	# output$download_plot <- downloadHandler(
	# 	filename = function() { paste0("plot.", input$plot_format) },
	# 	content = function(file) {
	# 		outplot = make_plot()
	# 		ggsave(file, plot = outplot, device = input$plot_format, width = input$plot_width, height = input$plot_height, dpi = input$plot_resolution)
	# 	}
	# )

}
