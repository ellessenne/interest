## ui.R script ##
source("helpers.R")

header <- dashboardHeader(title = "SiReX")

sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("Introduction", icon = icon("info"),
						 menuSubItem("Introduction #1", tabName = "introduction1"),
						 menuSubItem("Introduction #2", tabName = "introduction2"),
						 menuSubItem("Introduction #3", tabName = "introduction3"),
						 menuSubItem("Source code and feedback", href = "https://github.com/ellessenne/sirex/")),
		menuItem("Data", icon = icon("database"),
						 menuSubItem("Load data", tabName = "load_data"),
						 menuSubItem("View data", tabName = "view_data")),
		menuItem("Explore results", tabName = "results", icon = icon("search"))
	)
)

body <- dashboardBody(
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
	),
	tabItems(
		tabItem(tabName = "introduction1", p("Introduction tab #1!")),
		tabItem(tabName = "introduction2", p("Introduction tab #2!")),
		tabItem(tabName = "introduction3", p("Introduction tab #3!")),

		tabItem(tabName = "load_data",
						fluidRow(
							box(width = 12,
									title = "Load dataset",
									solidHeader = TRUE,
									p("Load a dataset here containing results from a simulation study. The dataset needs to be in tidy format, and it needs to contain the following columns:"),
									tags$div(
										tags$ul(
											tags$li(
												code("method"), ", a column representing the different models that are being compared;"
											),
											tags$li(
												code("par"), ", a column representing each parameter estimated and compared;"
											),
											tags$li(
												code("true_value"), ", the true value of a given parameter, under a given simulation scenario;"
											),
											tags$li(
												code("value"), ", the estimated coefficient from a given model, for a given parameter, under a given simulation scenario;"
											),
											tags$li(
												code("se"), ", the estimated standard error of a given estimated coefficient;"
											)
										)
									),
									p("All the remaining columns from the dataset are assumed to be the parameters that define each simulation scenario."),
									p("If you wish, you can download a sample dataset ", a(href = "", "here"), "."),
									fileInput('file1', 'Choose CSV File',
														accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
									)
							)
						),

		tabItem(tabName = "view_data",
						fluidRow(
							box(width = 12,
									solidHeader = TRUE,
									title = "View dataset",
									dataTableOutput("data_table"))
						)
		),

		tabItem(tabName = "results",
						fluidRow(
							tags$head(tags$style(HTML("#results_table {margin: auto; width: 0.666;}"))),
							box(width = 2,
									solidHeader = TRUE,
									title = "Select scenario",
									tableOutput("renUI")),
							box(width = 5,
									solidHeader = TRUE,
									title = "Table",
									tableOutput("results_table"),
									hr(),
									p("LaTeX code for this table:"),
									verbatimTextOutput("table_latex")),
							box(width = 5,
									solidHeader = TRUE,
									title = "Plot",
									p("Select summary statistic to plot:"),
									selectInput("plot_stat", "Select statistic", choices = c("Bias (mean)", "Bias (median)", "SE (mean)", "SE (median)")),
									plotOutput("results_plot"),
									hr(),
									p("Export plot:"),
									numericInput("plot_width", "Plot width", value = 6, min = 1, max = 50),
									numericInput("plot_height", "Plot height", value = 6, min = 1, max = 50),
									numericInput("plot_resolution", "Plot resolution (in DPI)", value = 150, min = 72, max = 320),
									radioButtons("plot_format", label = "Format", choices = c("png", "pdf"), selected = "png"),
									downloadButton("download_plot", label = "Download", icon = icon("download")),
									p("N.B.: height and width are in inches, not pixels.")
							)
							)
						)
	)
)

dashboardPage(header, sidebar, body)
