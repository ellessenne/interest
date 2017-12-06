## ui.R script ##
source("global.R")

header = shinydashboard::dashboardHeader(title = "INTEREST")

sidebar = shinydashboard::dashboardSidebar(
	shinydashboard::sidebarMenu(
		shinydashboard::menuItem(
			"Introduction",
			tabName = "introductionTab",
			icon = shiny::icon("info")
		),
		shinydashboard::menuItem(
			"Data",
			tabName = "dataTab",
			icon = shiny::icon("database")
		),
		shinydashboard::menuItem(
			"View uploaded data",
			tabName = "viewDataTab",
			icon = shiny::icon("search")
		),
		shinydashboard::menuItem(
			"Summary statistics",
			tabName = "summaryStatisticsTab",
			icon = shiny::icon("list-ol")
		),
		shinydashboard::menuItem(
			"Plots",
			tabName = "plotsTab",
			icon = shiny::icon("bar-chart")
		),
		shinydashboard::menuItem(
			"Technical details",
			tabName = "technicalDetailsTab",
			icon = shiny::icon("book")
		),
		shinydashboard::menuItem(
			"Feedback and comments",
			href = "mailto:ag475@leicester.ac.uk?Subject=Feedback%20and%20comments%20for%20INTEREST",
			icon = shiny::icon("commenting")
		),
		shinydashboard::menuItem(
			"Issues and bug report",
			href = "https://github.com/ellessenne/interest/issues/",
			icon = icon("exclamation-triangle ")
		),
		shinydashboard::menuItem(
			"Source code",
			href = "https://github.com/ellessenne/interest/",
			icon = shiny::icon("github")
		)
	)
)

body = shinydashboard::dashboardBody(
	shiny::tags$head(
		shiny::tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
	),
	shinydashboard::tabItems(
		shinydashboard::tabItem(tabName = "introductionTab",
														shiny::fluidRow(
															shinydashboard::box(
																title = "Introduction to INTEREST",
																width = 12,
																solidHeader = TRUE
															)
														)),
		shinydashboard::tabItem(
			tabName = "dataTab",
			shiny::fluidRow(
				shinydashboard::box(
					title = "Data upload form",
					width = 12,
					solidHeader = TRUE,
					shiny::p(
						"INTEREST supports uploading data, provinding a link, or even pasting data. The uploaded or linked file can be a comma-separated (.csv) file, a Stata version 8-14 file (.dta), an SPSS file (.sav), a SAS file (.sas7bdat), or an R serialised file (.rds); format will be inferred automatically, as long as you provide a file with the appropriate extension (case insensitive). Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed. Pasted data is read as tab-separated values."
					),
					shiny::p(
						"The dataset must be in tidy format, with variables in columns and observations in rows. See",
						shiny::a(href = "https://www.jstatsoft.org/article/view/v059i10", "here"),
						"for more details on tidy data. The app - at the moment - can handle a single estimand at once, and the uploaded dataset must include the following variables:"
					),
					shiny::tags$ul(
						shiny::tags$li(
							"A variable representing an estimated coefficient or value from the simulation study;"
						),
						shiny::tags$li(
							"A variable representing the standard error of the estimated coefficient."
						)
					),
					shiny::p(
						"The true value of the estimand must be specified by the user. Additionally, a dataset could contain the following variables:"
					),
					shiny::tags$ul(
						shiny::tags$li(
							"A variable representing methods compared with the simulation study;"
						),
						shiny::tags$li(
							"A list of variables representing",
							shiny::em("by"),
							"factors that identify the various data-generating mechanisms, such as sample size, true distribution, etc."
						)
					),
					shiny::p(
						"After uploading a dataset to INTEREST it will be possible to assign each variable to estimands, SEs, etc."
					)
				)
			),
			shiny::fluidRow(
				shinydashboard::box(
					title = "Select method for uploading data",
					width = 12,
					solidHeader = TRUE,
					shiny::radioButtons(
						inputId = "typeDataUpload",
						label = NULL,
						choices = c(
							"Upload file" = "uploadData",
							"Link file" = "linkData",
							"Paste file" = "pasteData"
						),
						selected = "uploadData",
						inline = TRUE
					),
					shiny::uiOutput(outputId = "loadData")
				)
			),
			shiny::fluidRow(
				shinydashboard::box(
					title = "Define variables",
					width = 4,
					solidHeader = TRUE,
					shiny::selectInput(
						inputId = "defineEstvarname",
						label = "Point estimates:",
						choices = ""
					),
					shiny::selectInput(
						inputId = "defineSe",
						label = "Standard errors:",
						choices = ""
					),
					shiny::numericInput(
						inputId = "defineTrue",
						label = "True value:",
						value = 0
					)
				),
				shinydashboard::box(
					title = "Define methods",
					width = 4,
					solidHeader = TRUE,
					shiny::selectInput(
						inputId = "defineMethod",
						label = "Method variable:",
						choices = ""
					),
					shiny::selectInput(
						inputId = "defineRefMethod",
						label = "Reference method:",
						choices = ""
					)
				),
				shinydashboard::box(
					title = "Define by factors",
					width = 4,
					solidHeader = TRUE,
					shiny::selectInput(
						inputId = "defineBy",
						label = "By factors:",
						choices = "",
						multiple = TRUE
					)
				)
			)
		),
		shinydashboard::tabItem(tabName = "viewDataTab",
														shiny::fluidRow(
															shinydashboard::box(
																width = 12,
																solidHeader = TRUE,
																shiny::dataTableOutput(outputId = "uploadedDataTable")
															)
														)),
		shinydashboard::tabItem(
			tabName = "summaryStatisticsTab",
			shiny::fluidRow(
				shinydashboard::tabBox(
					width = 3,
					shiny::tabPanel(
						title = "Select DGM",
						shiny::uiOutput(outputId = "summaryStatisticsSelectDGM")),
					shiny::tabPanel(
						title = "Customise",
						shiny::selectInput(
							inputId = "summaryStatisticsWhich",
							label = "Summary statistics of interest:",
							choices = c(
								"Non-missing point estimates" = "bsims",
								"Non-missing standard errors" = "sesims",
								"Average point estimate" = "bmean",
								"Median point estimate" = "bmedian",
								"Average standard error" = "semean",
								"Median standard error" = "semedian",
								"Bias in point estimate" = "bias",
								"Empirical standard error" = "esd",
								"Mean squared error" = "mse",
								"% gain in precision relative to reference method" = "relprec",
								"Model-based standard error" = "modelse",
								"Relative % error in standard error" = "relerror",
								"Coverage of nominal 95% confidence interval" = "cover",
								"Power of 5% level test" = "power"
							),
							multiple = TRUE,
							selected = c(
								"bsims",
								"sesims",
								"bmean",
								"bmedian",
								"semean",
								"semedian",
								"bias",
								"esd",
								"mse",
								"relprec",
								"modelse",
								"relerror",
								"cover",
								"power"
							)
						),
						shiny::sliderInput(
							inputId = "summaryStatisticsSigDigits",
							label = "Number of significant digits:",
							min = 0,
							max = 10,
							value = 4,
							step = 1,
							round = TRUE
						),
						shiny::checkboxInput(
							inputId = "summaryStatisticsIncludeMCSE",
							label = "Include Monte Carlo Standard Errors",
							value = TRUE
						)
					)
				),
				shinydashboard::tabBox(
					width = 9,
					shiny::tabPanel(
						"View summary statistics",
						shiny::dataTableOutput(outputId = "summaryStatisticsDataTable")
					),
					shiny::tabPanel(
						"Export table",
						shiny::textInput(inputId = "summaryStatisticsLaTeXCaption",
														 label = "Table caption:"),
						shiny::verbatimTextOutput(outputId = "summaryStatisticsLaTeX")
					),
					shiny::tabPanel(
						"Export summary statistics",
						shiny::p("If you want to export the table as visualised in the", shiny::em("View summary statistics"), "tab, untick the", shiny::em("export tidy data"), "checkbox; otherwise, a tidy dataset with summary statistics for each method and each DGM is exported."),
						shiny::textInput(
							inputId = "exportSummaryStatisticsName",
							label = "Name the file to export:",
							value = "summary_statistics"
						),
						shiny::radioButtons(
							inputId = "exportSummaryStatisticsType",
							label = "Format of the file to export:",
							choices = c(
								"Comma-separated" = "csv",
								"Tab-separated" = "tsv",
								"R" = "r",
								"Stata" = "stata",
								"SPSS" = "spss",
								"SAS" = "sas"
							),
							selected = "csv",
							inline = TRUE
						),
						shiny::checkboxInput(
							inputId = "exportSummaryStatisticsTidy",
							label = "Export tidy data",
							value = TRUE
						),
						shiny::downloadButton(
							outputId = "exportSummaryStatisticsButton",
							label = "Download summary statistics",
							icon = shiny::icon("download")
						)
					)
				)
			)
		),
		shinydashboard::tabItem(
			tabName = "plotsTab",
			shiny::fluidRow(
				shinydashboard::box(
					title = "Customise plots",
					width = 3,
					solidHeader = TRUE,
					# shiny::p("Define facets:"),
					# shiny::selectInput(
					# 	inputId = "plotFacetX",
					# 	label = "Select X-axis faceting variable:",
					# 	choices = "",
					# 	multiple = TRUE
					# ),
					# shiny::selectInput(
					# 	inputId = "plotFacetY",
					# 	label = "Select Y-axis faceting variable:",
					# 	choices = "",
					# 	multiple = TRUE
					# ),
					shiny::textInput(inputId = "customXlab", label = "X-axis label:"),
					shiny::textInput(inputId = "customYlab", label = "Y-axis label:"),
					shiny::numericInput(
						inputId = "plotWidth",
						"Plot width:",
						value = 6,
						min = 1,
						max = 50
					),
					shiny::numericInput(
						inputId = "plotHeight",
						"Plot height:",
						value = 6,
						min = 1,
						max = 50
					),
					shiny::numericInput(
						inputId = "plotResolution",
						"Plot resolution (in DPI):",
						value = 150,
						min = 72,
						max = 320
					),
					shiny::radioButtons(
						inputId = "plotFormat",
						label = "Format:",
						choices = c("png", "pdf"),
						selected = "png"
					),
					div(
						style = "display: inline-block; margin: 0 auto; text-align: center;",
						shiny::downloadButton(
							outputId = "exportPlot",
							label = "Save plot",
							icon = shiny::icon("download")
						)
					)
				),
				shinydashboard::tabBox(
					id = "tabPlots",
					width = 9,
					shiny::tabPanel(
						"Plot estimates",
						shiny::radioButtons(
							inputId = "selectPlotEstimates",
							label = "Select plot type:",
							choices = c("Point estimates vs SEs" = "b_vs_se"),
							inline = TRUE
						),
						shiny::plotOutput(outputId = "outPlotEstimates", width = "100%"),
						shiny::verbatimTextOutput(outputId = "outPlotEstimatesCode")
					),
					shiny::tabPanel(
						"Plot summary statistics",
						shiny::selectInput(
							inputId = "selectPlotSummaryStat",
							label = "Select summary statistic:",
							choices = ""
						),
						div(
							style = "display: inline-block; vertical-align:top; margin: 0 auto;",
							shiny::selectInput(
								inputId = "selectPlotSummary",
								label = "Select plot type:",
								choices = c("Barplot" = "barplot",
														"Lollyplot" = "lollyplot")
							)
						),
						shiny::plotOutput(outputId = "outPlotSummary", width = "100%"),
						shiny::verbatimTextOutput(outputId = "outPlotSummaryCode")
					)
				)
			)
		),
		shinydashboard::tabItem(tabName = "technicalDetailsTab",
														shiny::fluidRow(
															shinydashboard::box(
																title = "Technical details about INTEREST",
																width = 12,
																solidHeader = TRUE
															)
														))

	)
)

shinydashboard::dashboardPage(header, sidebar, body)
