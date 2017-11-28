## server.R script ##
source("helpers.R")

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

		return(s$summ)
	})

	# Make a data table with the summary statistics
	output$summaryStatisticsDataTable = shiny::renderDataTable({
		summ()
	})

	# Make summary table in LaTeX
	output$summaryStatisticsLaTeX <- renderPrint({
		summ() %>%
			xtable(caption = input$summaryStatisticsLaTeXCaption, digits = input$summaryStatisticsLaTeXDigits) %>%
			print()
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
				readr::write_csv(x = summ(), path = file)
			} else if (input$exportSummaryStatisticsType == "tsv") {
				readr::write_tsv(x = summ(), path = file)
			} else if (input$exportSummaryStatisticsType == "r") {
				saveRDS(object = summ(), file = file)
			} else if (input$exportSummaryStatisticsType == "stata") {
				haven::write_dta(data = summ(), path = file)
			} else if (input$exportSummaryStatisticsType == "spss") {
				haven::write_sav(data = summ(), path = file)
			} else if (input$exportSummaryStatisticsType == "sas") {
				haven::write_sas(data = summ(), path = file)
			}
		}
	)
}
