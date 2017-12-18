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
						title = "Select factors",
						shiny::uiOutput(outputId = "summaryStatisticsSelectFactors")
					),
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
								"Average standard error" = "se2mean",
								"Median standard error" = "se2median",
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
						shiny::p(
							"If you want to export the table as visualised in the",
							shiny::em("View summary statistics"),
							"tab, untick the",
							shiny::em("export tidy data"),
							"checkbox; otherwise, a tidy dataset with summary statistics for each method and each 'by' factor is exported."
						),
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
						shiny::uiOutput(outputId = "summaryStatisticsButton")
					)
				)
			)
		),
		shinydashboard::tabItem(
			tabName = "plotsTab",
			shiny::fluidRow(
				shinydashboard::tabBox(
					width = 3,
					shiny::tabPanel(
						title = "Select factors / Facet",
						shiny::radioButtons(
							inputId = "selectOrFacet",
							label = "Select factors or facet?",
							choices = c("None", "Select factors", "Facet"),
							selected = "None"
						),
						shiny::conditionalPanel(condition = "input.selectOrFacet == 'Select factors'",
																		shiny::uiOutput(outputId = "plotSelectFactors")),
						shiny::conditionalPanel(condition = "input.selectOrFacet == 'Facet'",
																		shiny::uiOutput(outputId = "plotFacet"))
					),
					shiny::tabPanel(
						title = "Customise plots",
						shiny::textInput(inputId = "customXlab", label = "X-axis label:"),
						shiny::textInput(inputId = "customYlab", label = "Y-axis label:"),
						shiny::selectInput(
							inputId = "customTheme",
							label = "Plot theme:",
							choices = c(
								"Base" = "theme_base",
								"Black and white" = "theme_bw",
								"Calc" = "theme_calc",
								"Classic" = "theme_classic",
								"Cowplot" = "theme_cowplot",
								"Dark" = "theme_dark",
								"Economist" = "theme_economist",
								"Excel" = "theme_excel",
								"Few" = "theme_few",
								"Fivethirtyeight" = "theme_fivethirtyeight",
								"Google docs" = "theme_gdocs",
								"Grey" = "theme_grey",
								"HC" = "theme_hc",
								"iGray" = "theme_igray",
								"Light" = "theme_light",
								"Linedraw" = "theme_linedraw",
								"Minimal" = "theme_minimal",
								"Pander" = "theme_pander",
								"Solarised" = "theme_solarized",
								# "Stata (s2color)" = "theme_stata_s2color",
								# "Stata (s2mono)" = "theme_stata_s2mono",
								# "Stata (s1color)" = "theme_stata_s1color",
								# "Stata (s1rcolor)" = "theme_stata_s1rcolor",
								# "Stata (s1mono)" = "theme_stata_s1mono",
								# "Stata (s2manual)" = "theme_stata_s2manual",
								# "Stata (s1manual)" = "theme_stata_s1manual",
								# "Stata (sj)" = "theme_stata_sj",
								"Tufte" = "theme_tufte",
								"WSJ" = "theme_wsj"
							),
							selected = "Black and white"
						),
						shiny::sliderInput(
							inputId = "plotWidth",
							"Plot width:",
							value = 6,
							min = 1,
							max = 50
						),
						shiny::sliderInput(
							inputId = "plotHeight",
							"Plot height:",
							value = 6,
							min = 1,
							max = 50
						),
						shiny::numericInput(
							inputId = "plotResolution",
							"Plot resolution (DPI):",
							value = 320,
							min = 72,
							max = 1200
						),
						shiny::selectInput(
							inputId = "plotFormat",
							label = "Format:",
							choices = c(
								"eps",
								"ps",
								"tex (pictex)" = "tex",
								"pdf",
								"jpeg",
								"tiff",
								"png",
								"bmp",
								"svg"
							),
							selected = "png"
						),
						shiny::uiOutput(outputId = "plotButton")
					)
				),
				shinydashboard::tabBox(
					id = "tabPlots",
					width = 9,
					shiny::tabPanel(
						"Plot estimates",
						shiny::selectInput(
							inputId = "selectPlotEstimates",
							label = "Select plot type:",
							choices = c("Point estimates vs SEs" = "b_vs_se")
						),
						shiny::selectInput(
							inputId = "plotEstimatesColorMethodBy",
							label = "Color:",
							choices = c("None"),
							selected = "none"
						),
						shiny::plotOutput(outputId = "outPlotEstimates", width = "100%"),
						shiny::verbatimTextOutput(outputId = "outPlotEstimatesCode")
					),
					shiny::tabPanel(
						title = "Plot summary statistics",
						shiny::selectInput(
							inputId = "selectPlotSummaryStat",
							label = "Select summary statistic:",
							choices = ""
						),
						shiny::selectInput(
							inputId = "selectPlotSummary",
							label = "Select plot type:",
							choices = c("Barplot" = "barplot",
													"Lollyplot" = "lollyplot")
						),
						shiny::checkboxInput(
							inputId = "plotMCSEConfidenceIntervals",
							label = "Plot confidence intervals based on Monte Carlo standard errors?",
							value = TRUE
						),
						shiny::plotOutput(outputId = "outPlotSummary", width = "100%"),
						shiny::verbatimTextOutput(outputId = "outPlotSummaryCode")
					)
				)
			)
		),
		shinydashboard::tabItem(tabName = "technicalDetailsTab",
														shiny::fluidRow(
															shinydashboard::tabBox(
																id = "tabPlots",
																width = 9,
																shiny::tabPanel(title = "Summary statistics"),
																shiny::tabPanel(title = "Monte Carlo SEs"),
																shiny::tabPanel(
																	title = "Plot themes",
																	shiny::p(
																		"The following themes are available for customising the appearance of plots in INTEREST:"
																	),
																	shiny::tags$ul(
																		shiny::tags$li(
																			shiny::em("Base"),
																			": theme similar to the default settings of the base R graphics;"
																		),
																		shiny::tags$li(
																			shiny::em("Black and white"),
																			": the classic dark-on-light ggplot2 theme. May work better for presentations displayed with a projector;"
																		),
																		shiny::tags$li(
																			shiny::em("Calc"),
																			": theme similar to the default settings of LibreOffice Calc charts;"
																		),
																		shiny::tags$li(
																			shiny::em("Classic"),
																			": a classic-looking theme, with x and y axis lines and no gridlines;"
																		),
																		shiny::tags$li(
																			shiny::em("Cowplot"),
																			": the default theme from the cowplot package;"
																		),
																		shiny::tags$li(
																			shiny::em("Dark"),
																			": The dark cousin of the 'Light' theme, with similar line sizes but a dark background. Useful to make thin coloured lines pop out;"
																		),
																		shiny::tags$li(
																			shiny::em("Economist"),
																			": style plots similar to those in The Economist;"
																		),
																		shiny::tags$li(
																			shiny::em("Excel"),
																			": theme to replicate the ugly monstrosity that was the old gray-background Excel chart. Please never use this;"
																		),
																		shiny::tags$li(
																			shiny::em("Few"),
																			": theme based on the rules and examples in Stephen Few's 'Practical Rules for Using Color in Charts';"
																		),
																		shiny::tags$li(
																			shiny::em("Fivethirtyeight"),
																			": theme inspired by the plots on http://fivethirtyeight.com;"
																		),
																		shiny::tags$li(
																			shiny::em("Google docs"),
																			": theme similar to the default look of charts in Google Docs;"
																		),
																		shiny::tags$li(
																			shiny::em("Grey"),
																			": the signature ggplot2 theme with a grey background and white gridlines, designed to put the data forward yet make comparisons easy;"
																		),
																		shiny::tags$li(shiny::em("HC"), ": theme based on the plots in Highcharts JS;"),
																		shiny::tags$li(
																			shiny::em("iGray"),
																			": theme with white panel and gray background;"
																		),
																		shiny::tags$li(
																			shiny::em("Light"),
																			": a theme similar to the 'Linedraw' theme but with light grey lines and axes, to direct more attention towards the data;"
																		),
																		shiny::tags$li(
																			shiny::em("Linedraw"),
																			": a theme with only black lines of various widths on white backgrounds, reminiscent of a line drawings. Serves a purpose similar to the 'Black and white' theme. Note that this theme has some very thin lines (<< 1 pt) which some journals may refuse;"
																		),
																		shiny::tags$li(
																			shiny::em("Minimal"),
																			": a minimalistic theme with no background annotations;"
																		),
																		shiny::tags$li(
																			shiny::em("Pander"),
																			": a ggplot theme originated from the pander package;"
																		),
																		shiny::tags$li(
																			shiny::em("Solarised"),
																			": ggplot color themes based on the Solarised color palette;"
																		),
																		# shiny::tags$li(shiny::em("Stata (s2color)"), ": a theme based on the s2color Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s2mono)"), ": a theme based on the s2mono Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s1color)"), ": a theme based on the s1color Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s1rcolor)"), ": a theme based on the s1rcolor Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s1mono)"), ": a theme based on the s1mono Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s2manual)"), ": a theme based on the s2manual Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (s1manual)"), ": a theme based on the s1manual Stata graph scheme;"),
																		# shiny::tags$li(shiny::em("Stata (sj)"), ": a theme based on the sj Stata graph scheme;"),
																		shiny::tags$li(
																			shiny::em("Tufte"),
																			": theme based on Chapter 6 of Edward Tufte's The Visual Display of Quantitative Information. No border, no axis lines, no grids;"
																		),
																		shiny::tags$li(
																			shiny::em("WSJ"),
																			": theme based on the plots in The Wall Street Journal."
																		)
																	)
																)
															)
														))
	)
)

shinydashboard::dashboardPage(header, sidebar, body)
