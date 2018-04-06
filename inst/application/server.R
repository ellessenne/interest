## server.R script ##
source("global.R")

function(input, output, session) {
  ### Make data selectors on the fly
  output$loadData <- shiny::renderUI({
    switch(
      input$typeDataUpload,
      "uploadData" = shiny::fileInput(inputId = "uploadData", label = NULL),
      "linkData" = shiny::textInput(inputId = "linkData", label = NULL),
      "pasteData" = shiny::textAreaInput(inputId = "pasteData", label = NULL)
    )
  })

  ### Function for reading data
  data <- shiny::reactive({
    switch(
      input$typeDataUpload,
      "uploadData" = {
        shiny::req(input$uploadData)
        inFile <- input$uploadData
        ptn <- "\\.[[:alnum:]]{1,8}$"
        suf <- tolower(regmatches(inFile$name, regexpr(ptn, inFile$name)))
        df <- switch(
          suf,
          ".csv" = readr::read_csv(inFile$datapath),
          ".dta" = haven::read_dta(inFile$datapath),
          ".sav" = haven::read_sav(inFile$datapath),
          ".sas7bdat" = haven::read_sas(inFile$datapath),
          ".rds" = readRDS(inFile$datapath)
        )
        attr(df, "spec") <- NULL
        df <- labelled::remove_labels(df, user_na_to_na = TRUE)
        return(df)
      },
      "linkData" = {
        shiny::req(input$linkData)
        ptn <- "\\.[[:alnum:]]{1,8}$"
        suf <- tolower(regmatches(input$linkData, regexpr(ptn, input$linkData)))
        df <- switch(
          suf,
          ".csv" = readr::read_csv(input$linkData),
          ".dta" = haven::read_dta(input$linkData),
          ".sav" = haven::read_sav(input$linkData),
          ".sas7bdat" = haven::read_sas(input$linkData),
          ".rds" = readRDS(file = gzcon(url(
            input$linkData
          )))
        )
        attr(df, "spec") <- NULL
        df <- labelled::remove_labels(df, user_na_to_na = TRUE)
        return(df)
      },
      "pasteData" = {
        shiny::req(input$pasteData)
        df <- readr::read_tsv(input$pasteData)
        attr(df, "spec") <- NULL
        return(df)
      }
    )
  })

  ### Populate select lists when data is present
  # Insert values in variables selectors
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
    shiny::updateSelectInput(session, inputId = "defineRefMethod", choices = unique(data()[[input$defineMethod]]))
  })
  # Remove 'defineRefMethod' if method is not specified
  shiny::observe({
    shiny::req(data())
    if (input$defineMethod == "") shiny::updateSelectInput(session, inputId = "defineRefMethod", choices = "")
  })

  ### Make factors selectors if 'by'is specified
  output$summaryStatisticsSelectFactors <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        !is.null(input$defineBy),
        "Factors selectors not available when no `by` factors are specified."
      )
    )
    lapply(input$defineBy, function(x)
      shiny::selectInput(
        inputId = paste0("table", x),
        label = x,
        choices = sort(unique(data()[[x]]))
      ))
  })
  output$plotSelectFactors <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        !is.null(input$defineBy),
        "Factors selectors not available when no `by` factors are specified."
      )
    )
    lapply(input$defineBy, function(x)
      shiny::selectInput(
        inputId = paste0("plot", x),
        label = x,
        choices = sort(unique(data()[[x]]))
      ))
  })

  ### DT with original dataset in the "View uploaded data" tab
  output$uploadedDataTable <- shiny::renderDataTable({
    shiny::validate(
      shiny::need(
        !is.null(data()),
        "Upload a dataset first, via the 'Data' tab."
      )
    )
    data()
  }, options = list(pageLength = 15))


  ### Missing data plots
  # Populate select input depending on the presence of 'method' or not
  shiny::observe({
    shiny::req(data())
    if (input$defineMethod != "") {
      shiny::updateSelectInput(session, inputId = "missingDataPlotType", choices = c("Bars (# Missing)" = "barsn", "Bars (% Missing)" = "barsp", "Amount of missing data" = "amount", "Scatter plot" = "scatter", "Heat plot" = "heat"))
    } else {
      shiny::updateSelectInput(session, inputId = "missingDataPlotType", choices = c("Bars (# Missing)" = "barsn", "Bars (% Missing)" = "barsp", "Amount of missing data" = "amount", "Scatter plot" = "scatter"))
    }
  })

  # Create plot of interest
  makeMissingDataPlot <- shiny::reactive({
    shiny::req(data())
    if (input$defineMethod != "") {
      if (!is.null(input$defineBy)) {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar:::gg_miss_var_create_n_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy)))) + ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both),
          "barsp" = naniar:::gg_miss_var_create_pct_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy)))) + ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data()),
          "scatter" = ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) + naniar::geom_miss_point() + ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both),
          "heat" = ggplot2::ggplot(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy))), ggplot2::aes_string(x = input$defineMethod, y = "variable", fill = "pct_miss")) + ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "#56B4E9", high = "#D55E00") + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both)
        )
      } else {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar:::gg_miss_var_create_n_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineMethod))) + ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both),
          "barsp" = naniar:::gg_miss_var_create_pct_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineMethod))) + ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data()),
          "scatter" = ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) + naniar::geom_miss_point() + ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both),
          "heat" = ggplot2::ggplot(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineMethod)), ggplot2::aes_string(x = input$defineMethod, y = "variable", fill = "pct_miss")) + ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "#56B4E9", high = "#D55E00")
        )
      }
    } else {
      if (!is.null(input$defineBy)) {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar:::gg_miss_var_create_n_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineBy))) + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both),
          "barsp" = naniar:::gg_miss_var_create_pct_miss(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineBy))) + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data()),
          "scatter" = ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) + naniar::geom_miss_point() + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both)
        )
      } else {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar:::gg_miss_var_create_n_miss(naniar::miss_var_summary(data())),
          "barsp" = naniar:::gg_miss_var_create_pct_miss(naniar::miss_var_summary(data())),
          "amount" = naniar::vis_miss(data()),
          "scatter" = ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) + naniar::geom_miss_point()
        )
      }
    }

    # Fix typo in % Missing plot
    # This looks weeeeird
    if (input$missingDataPlotType == "barsp") plot <- plot + ggplot2::labs(y = "% Missing")

    # Custom axis labels
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- interest:::apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- interest:::apply_colour(ggobject = plot, theme = input$customTheme)

    # Return plot
    plot
  })

  # Print plot of interest
  output$missingDataPlot <- shiny::renderPlot({
    makeMissingDataPlot()
  })


  ### Missing data tab
  # Create table with missing data
  missingDataTable <- shiny::reactive({
    shiny::req(data())
    vars <- c()
    if (input$defineMethod != "") vars <- c(vars, input$defineMethod)
    if (!is.null(input$defineBy)) vars <- c(vars, input$defineBy)
    subdata <- data()[, c(vars, input$defineEstvarname, input$defineSe)]
    if (length(vars) > 0) {
      out <- naniar::miss_var_summary(dplyr::group_by_at(subdata, .vars = vars))
    } else {
      out <- naniar::miss_var_summary(subdata)
    }
    names(out)[(length(names(out)) - 3):length(names(out))] <- c("Variable", "N. missing", "% Missing", "Cumulative N. missing")
    out
  })
  # Make DT of missing data table
  output$missingDataTable <- shiny::renderDataTable({
    missingDataTable()
  }, options = list(pageLength = 10))
  # Make LaTeX version of missing data table
  output$missingDataLaTeXTable <- shiny::renderPrint({
    shiny::req(data())
    print(
      xtable::xtable(
        x = missingDataTable(),
        caption = input$missingDataLaTeXTableCaption,
        digits = input$significantDigits
      ),
      include.rownames = FALSE
    )
  })

  ### Make summary statistics, and summary of 'simsum' object
  s <- reactive({
    shiny::req(data())
    if (input$defineMethod != "" & !is.null(input$defineBy)) {
      s <- rsimsum::simsum(
        data = data(),
        estvarname = input$defineEstvarname,
        true = input$defineTrue,
        se = input$defineSe,
        methodvar = input$defineMethod,
        ref = input$defineRefMethod,
        by = input$defineBy,
        x = TRUE
      )
    } else if (input$defineMethod != "" & is.null(input$defineBy)) {
      s <- rsimsum::simsum(
        data = data(),
        estvarname = input$defineEstvarname,
        true = input$defineTrue,
        se = input$defineSe,
        methodvar = input$defineMethod,
        ref = input$defineRefMethod,
        x = TRUE
      )
    } else if (input$defineMethod == "" & !is.null(input$defineBy)) {
      s <- rsimsum::simsum(
        data = data(),
        estvarname = input$defineEstvarname,
        true = input$defineTrue,
        se = input$defineSe,
        by = input$defineBy,
        x = TRUE
      )
    } else {
      s <- rsimsum::simsum(
        data = data(),
        estvarname = input$defineEstvarname,
        true = input$defineTrue,
        se = input$defineSe,
        x = TRUE
      )
    }
    s
  })

  summ <- shiny::reactive({
    shiny::req(data())
    s <- summary(s())
    s[["summ"]] <- rsimsum::get_data(s)[rsimsum::get_data(s)[["stat"]] %in% input$summaryStatisticsWhich, ]
    if (input$defineMethod != "") {
      s[["summ"]][[input$defineMethod]] <- factor(rsimsum::get_data(s)[[input$defineMethod]])
    }
    if (input$defineRefMethod != "") {
      s[["summ"]][[input$defineMethod]] <- relevel(rsimsum::get_data(s)[[input$defineMethod]], ref = input$defineRefMethod)
    }
    if (!is.null(input$defineBy)) {
      for (w in input$defineBy)
        s[["summ"]][[w]] <- factor(rsimsum::get_data(s)[[w]])
    }
    return(s)
  })

  ### Make summary table pretty for printing
  prettySumm <- shiny::reactive({
    shiny::req(data())
    s <- rsimsum::get_data(summ())
    if (input$includeMCSE) {
      s[["est"]] <- paste0(sprintf(paste0("%.", input$significantDigits, "f"), s[["est"]]), " (", sprintf(paste0("%.", input$significantDigits, "f"), s[["mcse"]]), ")")
    } else {
      s[["est"]] <- sprintf(paste0("%.", input$significantDigits, "f"), s[["est"]])
    }
    s[["mcse"]] <- NULL
    s[["lower"]] <- NULL
    s[["upper"]] <- NULL
    # Only selected factors if `by` is specified
    if (!is.null(input[["defineBy"]])) {
      s <- split(s, f = lapply(input$defineBy, function(f)
        s[[f]]))[[paste(sapply(input$defineBy, function(x)
        input[[paste0("table", x)]]), collapse = ".")]]
      for (i in input$defineBy) {
        s[[i]] <- NULL
      }
    }

    # Spread over `method` if `method` is specified
    if (input$defineMethod != "") {
      s <- tidyr::spread(
        data = s,
        key = !! input$defineMethod,
        value = est
      )
    }

    # Factorise summary statistics
    s$stat <- factor(
      s$stat,
      levels = c(
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
        "bccover",
        "power"
      ),
      labels = c(
        "Simulations with non-missing estimates/SEs",
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
        "Coverage of nominal 95% CI",
        "Bias corrected coverage of nominal 95% CI",
        "Power of 5% level test"
      )
    )
    s <- dplyr::arrange(s, stat)
    return(s)
  })

  ### Make a data table with the summary statistics
  output$summaryStatisticsDataTable <- shiny::renderDataTable({
    shiny::req(data())
    s <- prettySumm()
  })

  ### Make summary table in LaTeX
  output$summaryStatisticsLaTeX <- shiny::renderPrint({
    shiny::req(data())
    print(
      xtable::xtable(
        x = prettySumm(),
        caption = input$summaryStatisticsLaTeXCaption,
        digits = input$significantDigits
      ),
      include.rownames = FALSE
    )
  })

  ### Update caption of LaTeX table with current `by` scenario, if specified
  shiny::observe({
    shiny::req(input$defineBy)
    value <- paste(sapply(input$defineBy, function(x)
      paste0(x, ": ", input[[paste0("table", x)]])), collapse = ", ")
    shiny::updateTextInput(session, "summaryStatisticsLaTeXCaption", value = value)
  })

  # Download data.frame with summary statistics
  output$exportSummaryStatisticsButton <- shiny::downloadHandler(
    filename = function() {
      extension <- switch(
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
        s <- rsimsum::get_data(summ())
      } else {
        s <- prettySumm()
      }
      # Fix names
      names(s) <- make.names(names(s))

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
    if (!is.null(input[["defineMethod"]]) &
      !is.null(input[["defineBy"]])) {
      shiny::updateSelectInput(
        session,
        inputId = "plotEstimatesColorMethodBy",
        choices = c("None", input$defineMethod, input$defineBy)
      )
    } else if (!is.null(input[["defineMethod"]]) &
      is.null(input[["defineBy"]])) {
      shiny::updateSelectInput(
        session,
        inputId = "plotEstimatesColorMethodBy",
        choices = c("None", input$defineMethod)
      )
    } else if (is.null(input[["defineMethod"]]) &
      !is.null(input[["defineBy"]])) {
      shiny::updateSelectInput(
        session,
        inputId = "plotEstimatesColorMethodBy",
        choices = c("None", input$defineBy)
      )
    }
  })

  # Make facets selectors
  output$plotFacet <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        !is.null(input$defineBy),
        "Facet selectors not available when no `by` factors are specified."
      )
    )
    shiny::tagList(
      shiny::selectInput(
        inputId = "plotFacetX",
        label = "Faceting variables (x-axis):",
        choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
        multiple = TRUE
      ),
      shiny::selectInput(
        inputId = "plotFacetY",
        label = "Faceting variables (y-axis):",
        choices = names(data())[names(data()) %in% c(input$defineMethod, input$defineBy)],
        multiple = TRUE
      )
    )
  })

  # Remove "None" option from selectOrFacet if "by" factors are defined
  shiny::observe({
    shiny::req(data())
    if (!is.null(input[["defineBy"]])) {
      shiny::updateRadioButtons(
        session,
        inputId = "selectOrFacet",
        choices = c("Select DGM", "Facet"),
        selected = "Select DGM"
      )
    } else {
      shiny::updateRadioButtons(
        session,
        inputId = "selectOrFacet",
        choices = c("None", "Select DGM", "Facet"),
        selected = "None"
      )
    }
  })

  ### Update available summary statistics for plotting
  shiny::observe({
    shiny::req(data())
    shiny::updateSelectInput(
      session,
      inputId = "selectPlotSummaryStat",
      choices = SummaryStatistics[SummaryStatistics %in% input$summaryStatisticsWhich]
    )
  })

  ### Update Y factors for heat plots
  shiny::observe({
    shiny::req(data())
    if (!is.null(input$defineBy)) {
      shiny::updateSelectInput(
        session,
        inputId = "selectHeatY",
        choices = input$defineBy
      )
    }
  })

  ### Update select summary to 'coverage' if zip plot
  shiny::observe({
    shiny::req(data())
    if (input$selectPlotSummary == "zip") {
      shiny::updateSelectInput(
        session,
        inputId = "selectPlotSummaryStat",
        selected = "cover"
      )
    }
  })

  ### Make estimates plot
  makePlotEstimates <- function() {
    shiny::req(data())
    df <- data()
    if (input$defineMethod != "") df[[input$defineMethod]] <- factor(df[[input$defineMethod]])

    # Make plots
    plot <- switch(input$selectPlotEstimates,
      "b_vs_se" = {
        rsimsum::pattern(s())
      },
      "b_vs_b" = {},
      "se_vs_se" = {},
      "dist_b" = {
        if (input$defineMethod == "") {
          ggplot2::ggplot(df, ggplot2::aes_string(x = input$defineEstvarname)) +
            ggplot2::geom_density(alpha = 1 / 3)
        } else {
          ggplot2::ggplot(df, ggplot2::aes_string(x = input$defineEstvarname, color = input$defineMethod, fill = input$defineMethod)) +
            ggplot2::geom_density(alpha = 1 / 3)
        }
      },
      "dist_se" = {
        if (input$defineMethod == "") {
          ggplot2::ggplot(df, ggplot2::aes_string(x = input$defineSe)) +
            ggplot2::geom_density(alpha = 1 / 3)
        } else {
          ggplot2::ggplot(df, ggplot2::aes_string(x = input$defineSe, color = input$defineMethod, fill = input$defineMethod)) +
            ggplot2::geom_density(alpha = 1 / 3)
        }
      }
    )

    if (!is.null(input$defineBy)) plot <- plot + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both)

    # Custom axis label
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- interest:::apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- interest:::apply_colour(ggobject = plot, theme = input$customTheme)

    # Return plot
    plot
  }

  # Print estimates plot
  output$outPlotEstimates <- shiny::renderPlot({
    shiny::req(data())
    makePlotEstimates()
  })

  # Make summaries plot
  makePlotSummary <- function() {
    shiny::req(data())
    shiny::validate(
      shiny::need(input$defineMethod != "", message = "Plots not meaningful if there are no methods to compare :(")
    )

    # Infer target if sstat = 'nsim'
    if (input$selectPlotSummaryStat == "nsim") {
      vars <- c()
      if (input$defineMethod != "") vars <- c(vars, input$defineMethod)
      if (!is.null(input$defineBy)) vars <- c(vars, input$defineBy)
      nsim_target <- max(dplyr::summarise(dplyr::group_by_at(data(), .vars = vars), n = n())$n)
    }

    # Make plots
    plot <- switch(input$selectPlotSummary,
      "forest" = {
        if (input$selectPlotSummaryStat == "nsim") {
          if (!is.null(input$defineBy)) {
            rsimsum::forest(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          } else {
            rsimsum::forest(obj = s(), sstat = input$selectPlotSummaryStat, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          }
        } else {
          if (!is.null(input$defineBy)) {
            rsimsum::forest(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy)
          } else {
            rsimsum::forest(obj = s(), sstat = input$selectPlotSummaryStat)
          }
        }
      },
      "bar" = {
        if (input$selectPlotSummaryStat == "nsim") {
          if (!is.null(input$defineBy)) {
            rsimsum::bar(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          } else {
            rsimsum::bar(obj = s(), sstat = input$selectPlotSummaryStat, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          }
        } else {
          if (!is.null(input$defineBy)) {
            rsimsum::bar(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy)
          } else {
            rsimsum::bar(obj = s(), sstat = input$selectPlotSummaryStat)
          }
        }
      },
      "lolly" = {
        if (input$selectPlotSummaryStat == "nsim") {
          if (!is.null(input$defineBy)) {
            rsimsum::lolly(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          } else {
            rsimsum::lolly(obj = s(), sstat = input$selectPlotSummaryStat, target = ifelse(input$selectPlotSummaryStat == "nsim", nsim_target, NULL))
          }
        } else {
          if (!is.null(input$defineBy)) {
            rsimsum::lolly(obj = s(), sstat = input$selectPlotSummaryStat, by = input$defineBy)
          } else {
            rsimsum::lolly(obj = s(), sstat = input$selectPlotSummaryStat)
          }
        }
      },
      "zip" = {
        rsimsum::zip(s())
      },
      "heat" = {
        shiny::validate(shiny::need(!is.null(input$defineBy), message = "Heat plots require DGMs... :("))
        if (length(input$defineBy) == 1) {
          if (input$selectPlotSummaryStat == "nsim") {
            rsimsum::heat(s(), sstat = input$selectPlotSummaryStat, y = input$defineBy, text = input$textInHeat, target = nsim_target)
          } else {
            rsimsum::heat(s(), sstat = input$selectPlotSummaryStat, y = input$defineBy, text = input$textInHeat)
          }
        }
      }
    )

    # Custom axis label
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- interest:::apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- interest:::apply_colour(ggobject = plot, theme = input$customTheme)

    # Return plot
    plot
  }

  # Print summaries plot
  output$outPlotSummary <- shiny::renderPlot({
    shiny::req(data())
    makePlotSummary()
  })

  # Download plots
  output$exportPlotMissingButton <- shiny::downloadHandler(
    filename = function() {
      paste0("plot.", input$plotFormat)
    },
    content = function(file) {
      plot <- makeMissingDataPlot()
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
  output$exportPlotEstimates <- shiny::downloadHandler(
    filename = function() {
      paste0("plot.", input$plotFormat)
    },
    content = function(file) {
      plot <- makePlotEstimates()
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
  output$exportPlotSummaryButton <- shiny::downloadHandler(
    filename = function() {
      paste0("plot.", input$plotFormat)
    },
    content = function(file) {
      plot <- makePlotSummary()
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
    output$summaryStatisticsButton <- shiny::renderUI(
      shiny::downloadButton(
        outputId = "exportSummaryStatisticsButton",
        label = "Download summary statistics",
        icon = shiny::icon("download")
      )
    )
    output$plotMissingButton <- shiny::renderUI(
      shiny::downloadButton(
        outputId = "exportPlotMissingButton",
        label = "Save plot",
        icon = shiny::icon("download")
      )
    )
    output$plotEstimatesButton <- shiny::renderUI(
      shiny::downloadButton(
        outputId = "exportPlotEstimatesButton",
        label = "Save plot",
        icon = shiny::icon("download")
      )
    )
    output$plotSummaryButton <- shiny::renderUI(
      shiny::downloadButton(
        outputId = "exportPlotSummaryButton",
        label = "Save plot",
        icon = shiny::icon("download")
      )
    )
  })
}
