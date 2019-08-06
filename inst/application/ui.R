## ui.R script ##
source("global.R")

header <- shinydashboard::dashboardHeader(title = "INTEREST")

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem(
      "Home",
      tabName = "homeTab",
      icon = shiny::icon("home")
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
      "Missing data",
      tabName = "missingDataTab",
      icon = shiny::icon("question-circle")
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
      "Options",
      tabName = "optionsTab",
      icon = shiny::icon("cogs")
    ),
    shinydashboard::menuItem(
      "User guide",
      tabName = "userGuideTab",
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

body <- shinydashboard::dashboardBody(
  shiny::includeCSS("www/theme.css"),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "homeTab",
      shiny::fluidRow(
        shinydashboard::box(
          shiny::HTML(paste0("<div class='jumbotron' style='background:white;'>
          <div class='container'>
          <h1>Welcome to INTEREST!</h1>
          <p>INTEREST is a Shiny app that allows exploring results from Monte Carlo simulation studies interactively and producing plots and tables ready to use in manuscripts and presentations.</p>
          <p>INTEREST is also great for disseminating and inspecting results from simulation studies published in the literature where the dataset with the results is openly available.</p>
          </div>
          </div>")),
          solidHeader = TRUE,
          width = 12
        )
      )
    ),
    #########################################################
    ### Data tab
    shinydashboard::tabItem(
      tabName = "dataTab",
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
          width = 3,
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
          width = 3,
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
          title = "Define DGMs",
          width = 3,
          solidHeader = TRUE,
          shiny::selectInput(
            inputId = "defineBy",
            label = "By factors:",
            choices = "",
            multiple = TRUE
          )
        ),
        shinydashboard::box(
          title = "Define estimands",
          width = 3,
          solidHeader = TRUE,
          shiny::em("Coming soon...")
        )
      )
    ),
    #########################################################
    ### View uploaded data tab
    shinydashboard::tabItem(
      tabName = "viewDataTab",
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          solidHeader = TRUE,
          shiny::dataTableOutput(outputId = "uploadedDataTable")
        )
      )
    ),
    #########################################################
    ### Missing data tab
    shinydashboard::tabItem(
      tabName = "missingDataTab",
      shiny::fluidRow(
        shinydashboard::tabBox(
          id = "tabPlots",
          width = 12,
          shiny::tabPanel(
            title = "Visualising missing data",
            shiny::selectInput(inputId = "missingDataPlotType", label = "Select visualisation:", choices = ""),
            shiny::plotOutput(outputId = "missingDataPlot", height = "500px"),
            shiny::uiOutput(outputId = "plotMissingButton")
          ),
          shiny::tabPanel(
            title = "Table of missing data",
            shiny::dataTableOutput(outputId = "missingDataTable"),
            shiny::hr(),
            shiny::textInput(inputId = "missingDataLaTeXTableCaption", label = "Table caption:"),
            shiny::verbatimTextOutput(outputId = "missingDataLaTeXTable")
          )
        )
      )
    ),
    #########################################################
    ### Summary statistics tab
    shinydashboard::tabItem(
      tabName = "summaryStatisticsTab",
      shiny::fluidRow(
        shinydashboard::tabBox(
          width = 12,
          shiny::tabPanel(
            "View summary statistics",
            shiny::uiOutput(outputId = "summaryStatisticsSelectFactors"),
            shiny::dataTableOutput(outputId = "summaryStatisticsDataTable"),
            shiny::hr(),
            shiny::textInput(
              inputId = "summaryStatisticsLaTeXCaption",
              label = "Table caption:"
            ),
            shiny::verbatimTextOutput(outputId = "summaryStatisticsLaTeX")
          ),
          shiny::tabPanel(
            "Export summary statistics",
            shiny::p(
              "If you want to export the table as visualised in the",
              shiny::em("View summary statistics"),
              "tab, untick the",
              shiny::em("export tidy data"),
              "checkbox; otherwise, a tidy dataset with summary statistics for each method and each data-generating factor is exported."
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
    #########################################################
    ### Plots tab
    shinydashboard::tabItem(
      tabName = "plotsTab",
      shiny::fluidRow(
        shinydashboard::tabBox(
          id = "tabPlots",
          width = 12,
          shiny::tabPanel(
            "Estimates plots",
            shiny::selectInput(
              inputId = "selectPlotEstimates",
              label = "Select plot type:",
              choices = c(
                "Scatter Plot (Estimates vs Estimates)" = "est",
                "Scatter Plot (SEs vs SEs)" = "se",
                "Bland-Altman Plot (Estimates vs Estimates)" = "est_ba",
                "Bland-Altman Plot (SEs vs SEs)" = "se_ba",
                "Ridgeline Plot (Estimates vs Estimates)" = "est_ridge",
                "Ridgeline Plot (SEs vs SEs)" = "se_ridge"
              )
            ),
            shiny::plotOutput(outputId = "outPlotEstimates", height = "500px"),
            shiny::uiOutput(outputId = "plotEstimatesButton")
          ),
          shiny::tabPanel(
            title = "Summary statistics plots",
            shiny::selectInput(
              inputId = "selectPlotSummaryStat",
              label = "Select summary statistic:",
              choices = ""
            ),
            shiny::selectInput(
              inputId = "selectPlotSummary",
              label = "Select plot type:",
              choices = c(
                "Forest plot" = "forest",
                "Lolly plot" = "lolly",
                "Zip plot" = "zip",
                "Heat plot" = "heat",
                "Nested loop plot" = "nlp"
              )
            ),
            shiny::plotOutput(outputId = "outPlotSummary", height = "500px"),
            shiny::uiOutput(outputId = "plotSummaryButton")
          )
        )
      )
    ),
    #########################################################
    ### Options tab
    shinydashboard::tabItem(
      tabName = "optionsTab",
      shiny::fluidRow(
        shinydashboard::tabBox(
          width = 12,
          shiny::tabPanel(
            "Display options",
            shiny::selectInput(
              inputId = "summaryStatisticsWhich",
              label = "Summary statistics of interest:",
              choices = c(
                "Non-missing estimates/SEs" = "nsim",
                "Average point estimate" = "thetamean",
                "Median point estimate" = "thetamedian",
                "Average standard error" = "se2mean",
                "Median standard error" = "se2median",
                "Bias in point estimate" = "bias",
                "Empirical standard error" = "empse",
                "Mean squared error" = "mse",
                "% gain in precision relative to reference method" = "relprec",
                "Model-based standard error" = "modelse",
                "Relative % error in standard error" = "relerror",
                "Coverage of nominal 95% CI" = "cover",
                "Bias-eliminated coverage of nominal 95% CI" = "becover",
                "Power of 5% level test" = "power"
              ),
              multiple = TRUE,
              selected = c(
                "nsim",
                "thetamean",
                "thetamedian",
                "se2mean",
                "se2median",
                "bias",
                "empse",
                "mse",
                "relprec",
                "modelse",
                "relerror",
                "cover",
                "becover",
                "power"
              )
            ),
            shiny::sliderInput(
              inputId = "significantDigits",
              label = "Number of significant digits:",
              min = 0,
              max = 10,
              value = 4,
              step = 1,
              round = TRUE
            )
          ),
          shiny::tabPanel(
            "Simulation study options",
            shiny::checkboxInput(
              inputId = "includeMCSE",
              label = "Monte Carlo SEs",
              value = TRUE
            ),
            shiny::sliderInput(
              inputId = "rsimsumLevel",
              label = "Significance level for coverage, bias-eliminated coverage, power:",
              min = 0,
              max = 1,
              value = 0.95,
              step = 0.01,
              round = TRUE
            ),
            shiny::checkboxInput(
              inputId = "rsimsum.na.rm",
              label = "Remove point estimates or standard errors when either is missing",
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = "rsimsumDropbig",
              label = "Drop point estimates or standard errors above a maximum acceptable value",
              value = TRUE
            ),
            shiny::sliderInput(
              inputId = "rsimsumDropbig.max",
              label = "Maximum acceptable absolute value of the point estimates, after standardisation:",
              min = 0,
              max = 100,
              value = 10,
              step = 0.1,
              round = TRUE
            ),
            shiny::sliderInput(
              inputId = "rsimsumDropbig.semax",
              label = "Maximum acceptable absolute value of the point estimates, after standardisation:",
              min = 0,
              max = 1000,
              value = 100,
              step = 0.1,
              round = TRUE
            ),
            shiny::checkboxInput(
              inputId = "rsimsumDropbig.robust",
              label = "Use robust standardisation (using median and inter-quartile range) rather that normal standardisation (using mean and standard deviation)",
              value = TRUE
            )
          ),
          shiny::tabPanel(
            "Plot options",
            shiny::textInput(inputId = "customXlab", label = "Label of x-axis:"),
            shiny::textInput(inputId = "customYlab", label = "Label of y-axis:"),
            shiny::selectInput(
              inputId = "customTheme",
              label = "Plot theme:",
              choices = c(
                "Black and white" = "theme_bw",
                "Few" = "theme_few",
                "Grey" = "theme_grey",
                "Light" = "theme_light",
                "Linedraw" = "theme_linedraw",
                "Minimal" = "theme_minimal",
                "Tufte" = "theme_tufte",
                "Viridis" = "theme_viridis"
              ),
              selected = "theme_grey"
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
              value = 300,
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
            )
          )
        )
      )
    ),
    #########################################################
    ### User guide tab
    shinydashboard::tabItem(
      tabName = "userGuideTab",
      shiny::fluidRow(
        shinydashboard::tabBox(
          id = "tabPlots",
          width = 12,
          shiny::tabPanel(
            title = "Uploading data",
            shiny::p("INTEREST supports uploading data, provinding a link, or even pasting data."),
            shiny::p("The uploaded or linked file can be a comma-separated (.csv) file, a Stata version 8-14 file (.dta), an SPSS file (.sav), a SAS file (.sas7bdat), or an R serialised file (.rds); format will be inferred automatically, as long as you provide a file with the appropriate extension (case insensitive). Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed. Pasted data is read as tab-separated values."),
            shiny::p("The dataset must be in tidy format, with variables in columns and observations in rows. See", shiny::a(href = "https://www.jstatsoft.org/article/view/v059i10", "here"), "for more details on tidy data. The app - at the moment - can handle a single estimand at once, and the uploaded dataset must include the following variables:"),
            shiny::tags$ul(
              shiny::tags$li("A variable representing an estimated coefficient or value from the simulation study;"),
              shiny::tags$li("A variable representing the standard error of the estimated coefficient.")
            ),
            shiny::p("The true value of the estimand must be specified by the user. Additionally, a dataset could contain the following variables:"),
            shiny::tags$ul(
              shiny::tags$li("A variable representing methods compared with the simulation study;"),
              shiny::tags$li("A list of variables representing the various data-generating factors [DGMs], e.g. sample size, true distribution, etc.")
            ),
            shiny::p("After uploading a dataset to INTEREST it will be possible to assign each variable to estimands, SEs, etc.")
          ),
          shiny::tabPanel(
            title = "Summary statistics and Monte Carlo standard errors",
            shiny::p("INTEREST supports the following summary statistics:"),
            shiny::tags$ul(
              shiny::tags$li("Bias, which quantifies whether the estimator targets the true value on average;"),
              shiny::tags$li("Empirical standard error, estimating the standard deviation of the estimated values over all replications"),
              shiny::tags$li("Relative precision of a given method B against a reference method A, useful when comparing several methods at once;"),
              shiny::tags$li("Mean squared error, a measure that takes into account both precision and accuracy of a method. It is the sum of the squared bias and variance of the estimated values;"),
              shiny::tags$li("Model based standard error, computed by averaging the estimated standard errors for each replication;"),
              shiny::tags$li("Relative error in model standard error, a measure that quantifies how well the model standard error targets the empirical standard error;"),
              shiny::tags$li("Coverage, another key property of an estimator. It is defined as the probability that a confidence interval contains the true value;"),
              shiny::tags$li("Bias-eliminated coverage, a useful measure as under coverage may be induced by bias;"),
              shiny::tags$li("Power of a significance test at a given level alpha.")
            ),
            shiny::p("Each summary statistic comes with its Monte Carlo standard error by default, to help understanding the role of chance in results of simulation studies."),
            shiny::p("Further information on each summary statistics and Monte Carlo standard errors, including formulae, can be found here:"),
            shiny::tags$ul(
              shiny::tags$li("White, I.R. 2010.", shiny::em("simsum: Analyses of simulation studies including Monte Carlo error"), "The Stata Journal 10(3): 369-385 <", shiny::tags$a(href = "http://www.stata-journal.com/article.html?article=st0200", "http://www.stata-journal.com/article.html?article=st0200"), ">"),
              shiny::tags$li("Morris, T.P, White, I.R., and Crowther, M.J. 2019.", shiny::em("Using simulation studies to evaluate statistical methods"), "<", shiny::tags$a(href = "https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8086", "https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8086"), ">")
            )
          ),
          shiny::tabPanel(
            title = "Plots",
            shiny::p("INTEREST can produce a variety of plots to visualise results from simulation studies automatically."),
            shiny::p("Plots produced by INTEREST can be categorised into two broad groups:"),
            shiny::tags$ol(
              shiny::tags$li("plots of estimated values (and standard errors);"),
              shiny::tags$li("plots of summary statistics.")
            ),
            shiny::p("Plots for estimated values and standard errors are:"),
            shiny::tags$ul(
              shiny::tags$li("scatter plot with method-wise comparison of point estimates (or standard errors);"),
              shiny::tags$li("Bland-Altman plot with method-wise comparison of point estimates (or standard errors);"),
              shiny::tags$li("Ridgelines plot with the method-wise comparison of the distribution of point estimates (or standard errors).")
            ),
            shiny::p("Each plot will include colours defined by method (if any) and automatic faceting by DGMs (if any)."),
            shiny::p("Conversely, the following plots are supported for summary statistics:"),
            shiny::tags$ul(
              shiny::tags$li("plots of summary statistics with confidence intervals based on Monte Carlo standard errors. There are two types of this plot: forest plots and lolly plots;"),
              shiny::tags$li("heat plots of summary statistics: these plots are mosaic plots where the factor on the x-axis is represented by methods (if defined) and the factor on the y-axis is represented by a DGM, as selected by the user;"),
              shiny::tags$li("zip plots for visually explaining the summary statistic coverage by plotting the confidence intervals directly.")
            ),
            shiny::p("Each plot can be customised and exported for use in manuscript, reports, presentations via the Customise plot tab. In terms of customisation, it is possible to:"),
            shiny::tags$ol(
              shiny::tags$li("define a custom label for the x-axis and the y-axis;"),
              shiny::tags$li("change the overall appearance of the plot by applying one of the predefined themes.")
            ),
            shiny::p("In terms of exporting plots, it is possible to define the width, height, and resolution of the plot to export, and the format of the file to export. To suit a wide variety of possible use cases, INTEREST supports several image formats: among others, pdf, png, svg, and eps.")
          ),
          shiny::tabPanel(
            title = "Plot themes",
            shiny::p(
              "The following themes are available for customising the appearance of plots in INTEREST:"
            ),
            shiny::tags$ul(
              shiny::tags$li(
                shiny::em("Default"),
                ": no custom theme is applied;"
              ),
              shiny::tags$li(
                shiny::em("Black and white"),
                ": the classic dark-on-light ggplot2 theme. May work better for presentations displayed with a projector;"
              ),
              shiny::tags$li(
                shiny::em("Few"),
                ": theme based on the rules and examples in Stephen Few's 'Practical Rules for Using Color in Charts';"
              ),
              shiny::tags$li(
                shiny::em("Grey"),
                ": the signature ggplot2 theme with a grey background and white gridlines, designed to put the data forward yet make comparisons easy;"
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
                shiny::em("Tufte"),
                ": theme based on Chapter 6 of Edward Tufte's The Visual Display of Quantitative Information. No border, no axis lines, no grids;"
              ),
              shiny::tags$li(
                shiny::em("Viridis"),
                ": a theme based on the viridis colour palette created by",
                shiny::tags$a(href = "https://github.com/stefanv", "Stéfan van der Walt"),
                "and",
                shiny::tags$a(href = "https://github.com/njsmith", "Nathaniel Smith"),
                "for the Python",
                shiny::em("matplotlib"),
                "library."
              )
            )
          )
        )
      )
    )
  )
)

shinydashboard::dashboardPage(header, sidebar, body)
