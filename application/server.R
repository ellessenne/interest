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

	# Insert values in variables seletors
	shiny::observe({
		shiny::req(data())
		shiny::updateSelectInput(session, inputId = "defineEstvarname", choices = names(data()))
		shiny::updateSelectInput(session, inputId = "defineSe", choices = names(data()))
		shiny::updateSelectInput(session, inputId = "defineMethod", choices = c("", names(data())))
		shiny::updateSelectInput(session, inputId = "defineBy", choices = names(data()))
	})

	# Detect methods if method is specified
	shiny::observe({
		shiny::req(input$defineMethod)
		shiny::updateSelectInput(session, "defineRefMethod", choices = unique(data()[[input$defineMethod]]))
	})

	output$Method = shiny::renderText(paste("Method:", input$defineMethod, "- is.null:", is.null(input$defineMethod), "- =''", input$defineMethod == "", "-is.na:", is.na(input$defineMethod)))
	output$RefMethod = shiny::renderText(paste("RefMethod:", input$defineRefMethod, "- is.null:", is.null(input$defineRefMethod), "- =''", input$defineRefMethod == "", "-is.na:", is.na(input$defineRefMethod)))
	output$By = shiny::renderText(paste("By:", input$defineBy, "- is.null:", is.null(input$defineBy), "- =''", input$defineBy == "", "-is.na:", is.na(input$defineBy)))

	# Make a data table with the original dataset
	output$uploadedDataTable = shiny::renderDataTable({
		shiny::validate(shiny::need(!is.null(data()), "Upload a dataset first, using the 'data' tab."))
		data()
	})

	# Make summary statistics
	summ = shiny::reactive({
		shiny::req(data())
		if (input$defineMethod != "" & !is.null(input$defineBy)) {
			s = rsimsum::simsum(data = data(),
													estvarname = input$defineEstvarname,
													true = input$defineTrue,
													se = input$defineSe,
													methodvar = input$defineMethod,
													ref = input$defineRefMethod,
													by = input$defineBy)
		} else if (input$defineMethod != "" & is.null(input$defineBy)) {
			s = rsimsum::simsum(data = data(),
													estvarname = input$defineEstvarname,
													true = input$defineTrue,
													se = input$defineSe,
													methodvar = input$defineMethod,
													ref = input$defineRefMethod)
		} else if (input$defineMethod == "" & !is.null(input$defineBy)) {
			s = rsimsum::simsum(data = data(),
													estvarname = input$defineEstvarname,
													true = input$defineTrue,
													se = input$defineSe,
													by = input$defineBy)
		} else {
			s = rsimsum::simsum(data = data(),
													estvarname = input$defineEstvarname,
													true = input$defineTrue,
													se = input$defineSe)
		}
		return(summary(s))
	})

	# Make a data table with the summary statistics
	output$summaryStatisticsDataTable = shiny::renderDataTable({
		shiny::req(data())
		summ()$summ
	})

	# Make summary table in LaTeX
	output$summaryStatisticsLaTeX = shiny::renderPrint({
		shiny::req(data())
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

	# Update names in facets selectors
	observe({
		shiny::req(data())
		shiny::updateSelectInput(session,
														 inputId = "plotFacetX",
														 choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)])
		shiny::updateSelectInput(session,
														 inputId = "plotFacetY",
														 choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)])
	})


	# Make estimates plot
	makePlotEstimates = function() {
		shiny::req(data())
		switch(input$selectPlotEstimates,
					 "b_vs_se" =  {
					 	plot = ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineSe, y = input$defineEstvarname)) +
					 		ggplot2::geom_point() +
					 		ggplot2::theme_bw() +
					 		ggplot2::labs(x = input$customXlab, y = input$customYlab)
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
	output$outPlotEstimates = shiny::renderPlot({
		makePlotEstimates()
	})

	# Make summaries plot
	makePlotSummary = function() {
		switch(input$selectPlotSummary,
					 "barplot" = {
					 	plot = ggplot2::ggplot(summ()$summ[summ()$summ$stat == input$selectPlotSummaryStat,], ggplot2::aes_string(x = input$defineMethod, y = "coef")) +
					 		ggplot2::geom_bar(stat = "identity") +
					 		ggplot2::theme_bw() +
					 		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
					 		ggplot2::labs(x = input$customXlab, y = input$customYlab) +
					 		ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"), width = 1/3)
					 	plot
					 },
					 "lollyplot" = {
					 	plot = ggplot2::ggplot(summ()$summ[summ()$summ$stat == input$selectPlotSummaryStat,], ggplot2::aes_string(y = input$defineMethod, x = "coef")) +
					 		ggplot2::geom_vline(xintercept = dplyr::case_when(input$selectPlotSummaryStat %in% c("bias", "esd", "mse", "relprec", "modelse", "relerror") ~ 0,
					 																											input$selectPlotSummaryStat %in% c("cover", "power") ~ summ()$level * 100),
					 												color = "red",
					 												lty = "dotted") +
					 		ggplot2::geom_segment(ggplot2::aes_string(yend = input$defineMethod,
					 																							xend = dplyr::case_when(input$selectPlotSummaryStat %in% c("bias", "esd", "mse", "relprec", "modelse", "relerror") ~ 0,
					 																																			input$selectPlotSummaryStat %in% c("cover", "power") ~ summ()$level * 100)),
					 																							color = "grey50") +
					 														ggplot2::geom_point() +
					 														ggplot2::theme_bw() +
					 														ggplot2::labs(x = input$customXlab, y = input$customYlab) +
					 														ggplot2::geom_point(ggplot2::aes_string(x = "lower"), shape = "(") +
					 														ggplot2::geom_point(ggplot2::aes_string(x = "upper"), shape = ")")
					 													plot
					 })
	}

	# Print summaries plot
	output$outPlotSummary = shiny::renderPlot({
		makePlotSummary()
	})

	# output$download_plot <- downloadHandler(
	# 	filename = function() { paste0("plot.", input$plot_format) },
	# 	content = function(file) {
	# 		outplot = make_plot()
	# 		ggsave(file, plot = outplot, device = input$plot_format, width = input$plot_width, height = input$plot_height, dpi = input$plot_resolution)
	# 	}
	# )

}
