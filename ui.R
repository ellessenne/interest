## ui.R script ##
source("helpers.R")

header <- dashboardHeader(title = "SiReX")

sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("Introduction", tabName = "introductionTab", icon = icon("info")),
		menuItem("Data", tabName = "dataTab", icon = icon("database")),
		menuItem("View uploaded data", tabName = "viewDataTab", icon = icon("search")),
		menuItem("Summary statistics", tabName = "summaryStatisticsTab", icon = icon("list-ol")),
		menuItem("Plots", tabName = "plotsTab", icon = icon("bar-chart")),
		menuItem("Feedback and comments", href = "mailto:ag475@leicester.ac.uk?Subject=SiReX%20feedback%20and%20comments", icon = icon("commenting")),
		menuItem("Issues and bug report", href = "https://github.com/ellessenne/sirex/issues/", icon = icon("exclamation-triangle ")),
		menuItem("Source code", href = "https://github.com/ellessenne/sirex/", icon = icon("github"))
	)
)

body <- dashboardBody(
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
	),
	tabItems(
		tabItem(tabName = "introductionTab"),

		tabItem(tabName = "dataTab",
						box(width = 12,
								solidHeader = TRUE,
								p("Use the form below to upload your dataset."),
								p("The file can be a comma-separated (.csv) file, a Stata version 8-14 file (.dta), an SPSS file (.sav), a SAS file (.sas7bdat), or an R serialised file (.rds). You can either upload a file stored on your computer or provide a link to an online file (e.g. on GitHub). Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed. Files starting with http://, https://, ftp://, or ftps:// will be automatically downloaded. Remote gz files can also be automatically downloaded and decompressed."),
								p("The dataset must be in tidy format, with variables in columns and observations in rows. See", a(href = "https://www.jstatsoft.org/article/view/v059i10", "here"), "for more details on tidy data. The app can handle a single estimand at once, and the uploaded dataset must include the following variables:"),
								tags$ul(
									tags$li("A variable representing an estimated coefficient or value from the simulation study;"),
									tags$li("A variable representing the standard error of the estimated coefficient;"),
									tags$li("A variable representing the true value of a given parameter.")),
								p("Additionally, a dataset could contain the following variables:"),
								tags$ul(
									tags$li("A variable representing methods compared with the simulation study;"),
									tags$li("A list of variables representing", em("by"), "factors that identify the various data-generating mechanisms, such as sample size, true distribution, etc.")),
								p("It will be possible to define each variable after uploading a data set to SiReX.")),
						box(width = 12,
								solidHeader = TRUE,
								p("Select method for uploading data:"),
								radioButtons(inputId = "typeDataUpload",
														 label = NULL,
														 choices = c("Upload file" = "uploadData", "Link file" = "linkData", "Paste file" = "pasteData"),
														 selected = "uploadData",
														 inline = TRUE),
								uiOutput(outputId = "loadData")),
						box(width = 12,
								solidHeader = TRUE,
								shiny::p("Define variables:"),
								shiny::uiOutput(outputId = "defineVariables"))
		),
		tabItem(tabName = "viewDataTab",
						box(width = 12,
								solidHeader = TRUE,
								dataTableOutput(outputId = "uploadedDataTable"))),
		tabItem(tabName = "summaryStatisticsTab",
						tabBox(
							width = 12,
							tabPanel("View summary statistics",
											 uiOutput(outputId = "summaryRefMethodSelector"),
											 dataTableOutput(outputId = "summaryStatisticsDataTable")),
							tabPanel("Export table",
											 shiny::sliderInput(inputId = "summaryStatisticsLaTeXDigits",
											 									 label = "Number of significant digits:",
											 									 min = 0,
											 									 max = 10,
											 									 value = 4,
											 									 step = 1,
											 									 round = TRUE),
											 shiny::textInput(inputId = "summaryStatisticsLaTeXCaption",
											 								 label = "Table caption:"),
											 shiny::verbatimTextOutput(outputId = "summaryStatisticsLaTeX")),
							tabPanel("Export data",
											 radioButtons(inputId = "exportSummaryStatisticsType",
											 						 label = "Format of the file to export:",
											 						 choices = c("Comma-separated" = "csv", "Tab-separated" = "tsv", "R" = "r", "Stata" = "stata", "SPSS" = "spss", "SAS" = "sas"),
											 						 selected = "csv",
											 						 inline = TRUE),
											 textInput(inputId = "exportSummaryStatisticsName",
											 					label = "Name the file to export:",
											 					value = "summary_statistics"),
											 downloadButton(outputId = "exportSummaryStatisticsButton",
											 							 label = "Export",
											 							 icon = icon("download"))))),
		tabItem(tabName = "plotsTab")
	)
)

dashboardPage(header, sidebar, body)
