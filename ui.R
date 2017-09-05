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
								p("Use the tabs below to upload your dataset."),
								p("The file must be a comma-separated (.csv) file, and you can either upload a file stored on your computer or provide a link to a file store online (e.g. on GitHub). It is allowed to upload or link files ending in .gz, .bz2, .xz, or .zip: they will be automatically decompressed. Links to data must start with http://, https://, ftp://, or ftps://."),
								p("The dataset must be in tidy format, with variables in columns and observations in rows. It must include the following variables:"),
								tags$ul(
									tags$li(em("i"), ", a variable representing the replication ID;"),
									tags$li(em("par"), ", a variable representing an estimand;"),
									tags$li(em("coef"), ", a variable representing an estimated coefficient or value from the simulation study;"),
									tags$li(em("se"), ", a variable representing the standard error of the estimated coefficient;"),
									tags$li(em("true"), ", a variable representing the true value of a given parameter;"),
									tags$li(em("method"), ", a variable representing the methods compared with this simulation study.")),
								p("The remaining variables will be assumed to be covariates defining each data-generating mechanisms, such as sample size, true distribution, etc.")),
						box(width = 12,
								solidHeader = TRUE,
								p("Select method for uploading data:"),
								radioButtons(inputId = "typeDataUpload",
														 label = NULL,
														 choices = c("Upload file" = "uploadData", "Link file" = "linkData", "Paste file" = "pasteData"),
														 selected = "uploadData",
														 inline = TRUE),
								uiOutput(outputId = "loadData"))
		),
		tabItem(tabName = "viewDataTab",
						box(width = 12,
								solidHeader = TRUE,
								dataTableOutput(outputId = "uploadedDataTable"))),
		tabItem(tabName = "summaryStatisticsTab"),
		tabItem(tabName = "plotsTab")
	)
)

dashboardPage(header, sidebar, body)
