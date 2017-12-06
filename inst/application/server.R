## server.R script ##
source("global.R")

function(input, output, session) {
	# Make data selectors on the fly
	output$loadData = shiny::renderUI({
		switch(
			input$typeDataUpload,
			"uploadData" = shiny::fileInput(inputId = "uploadData", label = NULL),
			"linkData" = shiny::textInput(inputId = "linkData", label = NULL),
			"pasteData" = shiny::textAreaInput(inputId = "pasteData", label = NULL)
		)
	})

	# Functions to load data
	data = shiny::reactive({
		switch(
			input$typeDataUpload,
			"uploadData" = {
				shiny::req(input$uploadData)
				inFile = input$uploadData
				ptn = "\\.[[:alnum:]]{1,8}$"
				suf = tolower(regmatches(inFile$name, regexpr(ptn, inFile$name)))
				df = switch(
					suf,
					".csv" = readr::read_csv(inFile$datapath),
					".dta" = haven::read_dta(inFile$datapath),
					".sav" = haven::read_sav(inFile$datapath),
					".sas7bdat" = haven::read_sas(inFile$datapath),
					".rds" = readRDS(inFile$datapath)
				)
				return(df)
			},
			"linkData" = {
				shiny::req(input$linkData)
				ptn = "\\.[[:alnum:]]{1,8}$"
				suf = tolower(regmatches(input$linkData, regexpr(ptn, input$linkData)))
				df = switch(
					suf,
					".csv" = readr::read_csv(input$linkData),
					".dta" = haven::read_dta(input$linkData),
					".sav" = haven::read_sav(input$linkData),
					".sas7bdat" = haven::read_sas(input$linkData),
					".rds" = readRDS(file = gzcon(url(
						input$linkData
					)))
				)
				return(df)
			},
			"pasteData" = {
				shiny::req(input$pasteData)
				df = readr::read_tsv(input$pasteData)
				return(df)
			}
		)
	})

	# Insert values in variables selectors
	shiny::observe({
		shiny::req(data())
		shiny::updateSelectInput(session, inputId = "defineEstvarname", choices = names(data()))
		shiny::updateSelectInput(session, inputId = "defineSe", choices = names(data()))
		shiny::updateSelectInput(session,
														 inputId = "defineMethod",
														 choices = c("", names(data())))
		shiny::updateSelectInput(session, inputId = "defineBy", choices = names(data()))
	})

	# Detect methods if method is specified
	shiny::observe({
		shiny::req(input$defineMethod)
		shiny::updateSelectInput(session, "defineRefMethod", choices = unique(data()[[input$defineMethod]]))
	})

	# Make DGM selectors if `by` is specified
	output$summaryStatisticsSelectDGM = shiny::renderUI({
		shiny::validate(
			shiny::need(
				!is.null(input$defineBy),
				"DGM selectors not available when no `by` factors are specified."
			)
		)
		lapply(input$defineBy, function(x)
			shiny::selectInput(
				inputId = paste0("table", x),
				label = x,
				choices = sort(unique(data()[[x]]))
			))
	})

	output$plotSelectDGM = shiny::renderUI({
		shiny::validate(
			shiny::need(
				!is.null(input$defineBy),
				"DGM selectors not available when no `by` factors are specified."
			)
		)
		lapply(input$defineBy, function(x)
			shiny::selectInput(
				inputId = paste0("plot", x),
				label = x,
				choices = sort(unique(data()[[x]]))
			))
	})

	# Make a data table with the original dataset
	output$uploadedDataTable = shiny::renderDataTable({
		shiny::validate(shiny::need(
			!is.null(data()),
			"Upload a dataset first, using the 'data' tab."
		))
		data()
	}, options = list(pageLength = 10))

	# Make summary statistics
	summ = shiny::reactive({
		shiny::req(data())
		if (input$defineMethod != "" & !is.null(input$defineBy)) {
			s = rsimsum::simsum(
				data = data(),
				estvarname = input$defineEstvarname,
				true = input$defineTrue,
				se = input$defineSe,
				methodvar = input$defineMethod,
				ref = input$defineRefMethod,
				by = input$defineBy
			)
		} else if (input$defineMethod != "" & is.null(input$defineBy)) {
			s = rsimsum::simsum(
				data = data(),
				estvarname = input$defineEstvarname,
				true = input$defineTrue,
				se = input$defineSe,
				methodvar = input$defineMethod,
				ref = input$defineRefMethod
			)
		} else if (input$defineMethod == "" & !is.null(input$defineBy)) {
			s = rsimsum::simsum(
				data = data(),
				estvarname = input$defineEstvarname,
				true = input$defineTrue,
				se = input$defineSe,
				by = input$defineBy
			)
		} else {
			s = rsimsum::simsum(
				data = data(),
				estvarname = input$defineEstvarname,
				true = input$defineTrue,
				se = input$defineSe
			)
		}
		s = summary(s)
		s$summ = s$summ[s$summ$stat %in% input$summaryStatisticsWhich, ]
		return(s)
	})

	# Make summary table pretty for printing
	prettySumm = shiny::reactive({
		shiny::req(data())
		s = summ()$summ
		if (input$summaryStatisticsIncludeMCSE) {
			s$value = paste0(sprintf(
				paste0("%.", input$summaryStatisticsSigDigits, "f"),
				s$coef
			),
			" (",
			sprintf(
				paste0("%.", input$summaryStatisticsSigDigits, "f"),
				s$mcse
			),
			")")
		} else {
			s$value = sprintf(paste0("%.", input$summaryStatisticsSigDigits, "f"),
												s$coef)
		}
		s$coef = NULL
		s$mcse = NULL
		s$lower = NULL
		s$upper = NULL
		# Only selected DGM if `by` is specified
		if (!is.null(input[["defineBy"]])) {
			s = split(s, f = lapply(input$defineBy, function(f)
				s[[f]]))[[paste(sapply(input$defineBy, function(x)
					input[[paste0("table", x)]]), collapse = ".")]]
			for (i in input$defineBy) {
				s[[i]] = NULL
			}
		}

		# Spread over `method` if `method` is specified
		if (input$defineMethod != "") {
			s = tidyr::spread(
				data = s,
				key = !!input$defineMethod,
				value = value
			)
		}

		# Factorise summary statistics
		s$stat = factor(
			s$stat,
			levels = c(
				"bsims",
				"sesims",
				"bmean",
				"bmedian",
				"se2mean",
				"se2median",
				"bias",
				"esd",
				"mse",
				"relprec",
				"modelse",
				"relerror",
				"cover",
				"power"
			),
			labels = c(
				"Non-missing point estimates",
				"Non-missing standard errors",
				"Average point estimate",
				"Median point estimate",
				"Average standard error",
				"Median standard error",
				"Bias in point estimate",
				"Empirical standard error",
				"Mean squared error",
				"% gain in precision relative to reference method",
				"Model-based standard error",
				"Relative % error in standard error",
				"Coverage of nominal 95% confidence interval",
				"Power of 5% level test"
			)
		)
		s = dplyr::arrange(s, stat)
		return(s)
	})

	# Make a data table with the summary statistics
	output$summaryStatisticsDataTable = shiny::renderDataTable({
		shiny::req(data())
		s = prettySumm()
	})

	# Make summary table in LaTeX
	output$summaryStatisticsLaTeX = shiny::renderPrint({
		shiny::req(data())
		s = prettySumm()
		print(
			xtable::xtable(
				x = s,
				caption = input$summaryStatisticsLaTeXCaption,
				digits = input$summaryStatisticsSigDigits
			),
			include.rownames = FALSE
		)
	})

	# Update caption of LaTeX table with current `by` scenario, if specified
	shiny::observe({
		shiny::req(input$defineBy)
		value = paste(sapply(input$defineBy, function(x)
			paste0(x, ": ", input[[paste0("table", x)]])), collapse = ", ")
		shiny::updateTextInput(session, "summaryStatisticsLaTeXCaption", value = value)
	})

	# Download data.frame with summary statistics
	output$exportSummaryStatisticsButton = shiny::downloadHandler(
		filename = function() {
			extension = switch(
				input$exportSummaryStatisticsType,
				"csv" = ".csv",
				"tsv" = ".tsv",
				"r" = ".rds",
				"stata" = ".dta",
				"spss" = ".sav",
				"sas" = ".sas7bdat"
			)
			paste0(input$exportSummaryStatisticsName, extension)
		},
		content = function(file) {
			if (input$exportSummaryStatisticsTidy) {
				s = summ()$summ
			} else {
				s = prettySumm()
			}

			if (input$exportSummaryStatisticsType == "csv") {
				readr::write_csv(x = s, path = file)
			} else if (input$exportSummaryStatisticsType == "tsv") {
				readr::write_tsv(x = s, path = file)
			} else if (input$exportSummaryStatisticsType == "r") {
				saveRDS(object = s, file = file)
			} else if (input$exportSummaryStatisticsType == "stata") {
				haven::write_dta(data = s, path = file)
			} else if (input$exportSummaryStatisticsType == "spss") {
				haven::write_sav(data = s, path = file)
			} else if (input$exportSummaryStatisticsType == "sas") {
				haven::write_sas(data = s, path = file)
			}
		}
	)

	# Update options for coloring b_vs_se plot
	shiny::observe({
		shiny::req(data())
		if (!is.null(input[["defineMethod"]]) & !is.null(input[["defineBy"]])) {
			shiny::updateSelectInput(session, inputId = "plotEstimatesColorMethodBy",
															 choices = c("None", input$defineMethod, input$defineBy))
		} else if (!is.null(input[["defineMethod"]]) & is.null(input[["defineBy"]])) {
			shiny::updateSelectInput(session, inputId = "plotEstimatesColorMethodBy",
															 choices = c("None", input$defineMethod))
		} else if (is.null(input[["defineMethod"]]) & !is.null(input[["defineBy"]])) {
			shiny::updateSelectInput(session, inputId = "plotEstimatesColorMethodBy",
															 choices = c("None", input$defineBy))
		}
	})

	# Make facets selectors
	output$plotFacet = shiny::renderUI({
		shiny::validate(
			shiny::need(
				!is.null(input$defineBy),
				"Facet selectors not available when no `by` factors are specified."
			)
		)
		shiny::tagList(shiny::selectInput(inputId = "plotFacetX",
																			label = "X-axis faceting variable(s):",
																			choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
																			multiple = TRUE),
									 shiny::selectInput(inputId = "plotFacetY",
									 									 label = "Y-axis faceting variable(s):",
									 									 choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
									 									 multiple = TRUE)
		)
	})

	# Remove "None" option from selectOrFacet if "by" factors are defined
	shiny::observe({
		shiny::req(data())
		if (!is.null(input[["defineBy"]])) {
			shiny::updateRadioButtons(session, inputId = "selectOrFacet", choices = c("Select DGM", "Facet"), selected = "Select DGM")
		} else {
			shiny::updateRadioButtons(session, inputId = "selectOrFacet", choices = c("None", "Select DGM", "Facet"), selected = "None")
		}
	})

	# Update available summary statistics for plotting
	shiny::observe({
		shiny::req(data())
		shiny::updateSelectInput(session,
														 inputId = "selectPlotSummaryStat",
														 choices = input$summaryStatisticsWhich)
	})

	# Make estimates plot
	makePlotEstimates = function() {
		shiny::req(data())

		# Subset data if required
		df = data()
		if (input$selectOrFacet == "Select DGM") {
			if (!is.null(input[["defineBy"]])) {
				df = split(data(), f = lapply(input$defineBy, function(f)
					data()[[f]]))[[paste(sapply(input$defineBy, function(x)
						input[[paste0("plot", x)]]), collapse = ".")]]
			}
		}

		# Make plots
		switch(input$selectPlotEstimates,
					 "b_vs_se" =  {
					 	if (input$plotEstimatesColorMethodBy != "None") {
					 		plot = ggplot2::ggplot(data(),
					 													 ggplot2::aes_string(
					 													 	x = input$defineSe,
					 													 	y = input$defineEstvarname,
					 													 	color = input$plotEstimatesColorMethodBy
					 													 ))
					 	} else {
					 		plot = ggplot2::ggplot(
					 			df,
					 			ggplot2::aes_string(
					 				x = input$defineSe,
					 				y = input$defineEstvarname
					 			)
					 		)
					 	}
					 	plot = plot + ggplot2::geom_point() +
					 		ggplot2::labs(x = input$customXlab, y = input$customYlab)
					 	if (input$selectOrFacet == "Facet") {
					 		shiny::validate(shiny::need(!is.null(input$plotFacetX) | !is.null(input$plotFacetY), "You need to define at least one faceting variable."))
					 		if (!is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
					 			plot = plot + ggplot2::facet_grid(reformulate(input$plotFacetX, input$plotFacetY), labeller = ggplot2::label_both)
					 		} else if (is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
					 			plot = plot + ggplot2::facet_grid(reformulate(".", input$plotFacetY), labeller = ggplot2::label_both)
					 		} else if (!is.null(input$plotFacetX) & is.null(input$plotFacetY)) {
					 			plot = plot + ggplot2::facet_grid(reformulate(input$plotFacetX, "."), labeller = ggplot2::label_both)
					 		}
					 	}
					 	plot = apply_theme(ggobject = plot, theme = input$customTheme)
					 	plot = apply_colour(ggobject = plot, theme = input$customTheme)
					 	plot
					 })
	}

	# Print estimates plot
	output$outPlotEstimates = shiny::renderPlot({
		shiny::req(data())
		makePlotEstimates()
	})

	# Make summaries plot
	makePlotSummary = function() {
		shiny::req(data())
		shiny::validate(
			shiny::need(input$defineMethod != "", message = "Plots not meaningful if there are no methods to compare :(")
		)
		df = summ()$summ
		# Select subset of DGMS
		if (input$selectOrFacet == "Select DGM") {
			if (!is.null(input[["defineBy"]])) {
				df = split(summ()$summ, f = lapply(input$defineBy, function(f)
					summ()$summ[[f]]))[[paste(sapply(input$defineBy, function(x)
						input[[paste0("plot", x)]]), collapse = ".")]]
			}
		}

		# Make plots
		switch(input$selectPlotSummary,
					 "barplot" = {
					 	plot = ggplot2::ggplot(df[df$stat == input$selectPlotSummaryStat, ],
					 												 ggplot2::aes_string(x = input$defineMethod, y = "coef")) +
					 		ggplot2::geom_bar(stat = "identity") +
					 		ggplot2::theme(axis.text.x = ggplot2::element_text(
					 			angle = 90,
					 			vjust = 0.5,
					 			hjust = 1
					 		))
					 	if (input$plotMCSEConfidenceIntervals) plot = plot + ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"), width = 1 / 3)
					 },
					 "lollyplot" = {
					 	plot = ggplot2::ggplot(df[df$stat == input$selectPlotSummaryStat, ],
					 												 ggplot2::aes_string(y = input$defineMethod, x = "coef")) +
					 		ggplot2::geom_vline(
					 			xintercept = dplyr::case_when(
					 				input$selectPlotSummaryStat %in% c("se2mean", "se2median", "bias", "esd", "mse", "relprec", "modelse", "relerror") ~ 0,
					 				input$selectPlotSummaryStat %in% c("cover", "power") ~ summ()$level * 100,
					 				input$selectPlotSummaryStat %in% c("bmean", "bmedian") ~ input$defineTrue
					 			),
					 			color = "red",
					 			lty = "dotted"
					 		) +
					 		ggplot2::geom_segment(ggplot2::aes_string(
					 			yend = input$defineMethod,
					 			xend = dplyr::case_when(
					 				input$selectPlotSummaryStat %in% c("se2mean", "se2median", "bias", "esd", "mse", "relprec", "modelse", "relerror") ~ 0,
					 				input$selectPlotSummaryStat %in% c("cover", "power") ~ summ()$level * 100,
					 				input$selectPlotSummaryStat %in% c("bmean", "bmedian") ~ input$defineTrue
					 			)
					 		))
					 	if (input$plotMCSEConfidenceIntervals) plot = plot + ggplot2::geom_point(ggplot2::aes_string(x = "lower"), shape = 40) + ggplot2::geom_point(ggplot2::aes_string(x = "upper"), shape = 41)
					 })
		if (input$selectOrFacet == "Facet") {
			shiny::validate(shiny::need(all(input$defineBy %in% c(input$plotFacetX, input$plotFacetY)), "All 'by' factors must be selected as faceting variables."))
			if (!is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
				plot = plot + ggplot2::facet_grid(reformulate(input$plotFacetX, input$plotFacetY), labeller = ggplot2::label_both)
			} else if (is.null(input$plotFacetX) & !is.null(input$plotFacetY)) {
				plot = plot + ggplot2::facet_grid(reformulate(".", input$plotFacetY), labeller = ggplot2::label_both)
			} else if (!is.null(input$plotFacetX) & is.null(input$plotFacetY)) {
				plot = plot + ggplot2::facet_grid(reformulate(input$plotFacetX, "."), labeller = ggplot2::label_both)
			}
		}
		plot = plot + ggplot2::labs(x = input$customXlab, y = input$customYlab)
		plot = apply_theme(ggobject = plot, theme = input$customTheme)
		plot = apply_colour(ggobject = plot, theme = input$customTheme)
		plot
	}

	# Update default axis labels
	shiny::observe({
		shiny::req(data())
		if (input$tabPlots == "Plot estimates") {
			switch(input$selectPlotEstimates,
						 "b_vs_se" = {
						 	shiny::updateTextInput(session, inputId = "customXlab", value = "Point Estimate")
						 	shiny::updateTextInput(session, inputId = "customYlab", value = "Standard Error")
						 })
		} else {
			switch(input$selectPlotSummary,
						 "barplot" = {
						 	shiny::updateTextInput(session,
						 												 inputId = "customXlab",
						 												 value = input$defineMethod)
						 	shiny::updateTextInput(session,
						 												 inputId = "customYlab",
						 												 value = input$selectPlotSummaryStat)
						 },
						 "lollyplot" = {
						 	shiny::updateTextInput(session,
						 												 inputId = "customXlab",
						 												 value = input$selectPlotSummaryStat)
						 	shiny::updateTextInput(session,
						 												 inputId = "customYlab",
						 												 value = input$defineMethod)
						 })
		}
	})

	# Print summaries plot
	output$outPlotSummary = shiny::renderPlot({
		shiny::req(data())
		makePlotSummary()
	})

	# Download plot of current tab
	output$exportPlotButton = shiny::downloadHandler(
		filename = function() {
			paste0("plot.", input$plotFormat)
		},
		content = function(file) {
			if (input$tabPlots == "Plot estimates") {
				plot = makePlotEstimates()
			} else {
				plot = makePlotSummary()
			}
			ggplot2::ggsave(
				file,
				plot = plot,
				device = input$plotFormat,
				width = input$plotWidth,
				height = input$plotHeight,
				dpi = input$plotResolution
			)
		}
	)

	# Make download buttons only when data() is loaded
	shiny::observe({
		shiny::req(data())
		output$summaryStatisticsButton = shiny::renderUI(
			shiny::downloadButton(
				outputId = "exportSummaryStatisticsButton",
				label = "Download summary statistics",
				icon = shiny::icon("download")
			)
		)
		output$plotButton = shiny::renderUI(
			shiny::downloadButton(
				outputId = "exportPlotButton",
				label = "Save plot",
				icon = shiny::icon("download")
			)
		)
	})
}
