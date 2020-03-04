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
      },
      "exampleData" = {
        df <- readRDS("data/tt.RDS")
        return(df)
      }
    )
  })

  ### Populate select lists when data is present
  # Insert values in variables selectors
  shiny::observe({
    shiny::req(data())
    shiny::updateSelectInput(session, inputId = "defineEstvarname", choices = c(names(data())))
    shiny::updateSelectInput(session, inputId = "defineSe", choices = c("", names(data())))
    shiny::updateSelectInput(session, inputId = "defineMethod", choices = c("", names(data())))
    shiny::updateSelectInput(session, inputId = "defineBy", choices = names(data()))
    shiny::updateSelectInput(session, inputId = "defineTrueCol", choices = names(data()))
    shiny::updateSelectInput(session, inputId = "defineCIlower", choices = names(data()))
    shiny::updateSelectInput(session, inputId = "defineCIupper", choices = names(data()))
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
        "Factors selectors not available when no DGMs are specified."
      )
    )
    lapply(input$defineBy, function(x) {
      shiny::selectInput(
        inputId = paste0("table", x),
        label = x,
        choices = sort(unique(data()[[x]]))
      )
    })
  })
  output$plotSelectFactors <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        !is.null(input$defineBy),
        "Factors selectors not available when no DGMs are specified."
      )
    )
    lapply(input$defineBy, function(x) {
      shiny::selectInput(
        inputId = paste0("plot", x),
        label = x,
        choices = sort(unique(data()[[x]]))
      )
    })
  })

  ### DT with original dataset in the "View uploaded data" tab
  output$uploadedDataTable <- DT::renderDT(
    {
      shiny::validate(
        shiny::need(
          !is.null(data()),
          "Upload a dataset first, via the 'Data' tab."
        )
      )
      data()
    },
    options = list(pageLength = 20)
  )


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
          "barsn" = naniar::gg_miss_var(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy)), show_pct = FALSE) + ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both),
          "barsp" = naniar::gg_miss_var(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy)), show_pct = TRUE) + ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data(), warn_large_data = FALSE),
          "scatter" = {
            shiny::validate(
              shiny::need(input$defineSe != "", message = "Plot not meaningful if SEs are not provided.")
            )
            ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) +
              naniar::geom_miss_point() +
              ggplot2::facet_grid(reformulate(input$defineMethod, input$defineBy), labeller = ggplot2::label_both)
          },
          "heat" = ggplot2::ggplot(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = c(input$defineMethod, input$defineBy))), ggplot2::aes_string(x = input$defineMethod, y = "variable", fill = "pct_miss")) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_gradient(low = "#56B4E9", high = "#D55E00") +
            ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both)
        )
      } else {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar::gg_miss_var(dplyr::group_by_at(data(), .vars = input$defineMethod), show_pct = FALSE) + ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both),
          "barsp" = naniar:::gg_miss_var(dplyr::group_by_at(data(), .vars = input$defineMethod), show_pct = TRUE) + ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data(), warn_large_data = FALSE),
          "scatter" = {
            shiny::validate(
              shiny::need(input$defineSe != "", message = "Plot not meaningful if SEs are not provided.")
            )
            ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) +
              naniar::geom_miss_point() +
              ggplot2::facet_wrap(facets = input$defineMethod, labeller = ggplot2::label_both)
          },
          "heat" = ggplot2::ggplot(naniar::miss_var_summary(dplyr::group_by_at(data(), .vars = input$defineMethod)), ggplot2::aes_string(x = input$defineMethod, y = "variable", fill = "pct_miss")) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_gradient(low = "#56B4E9", high = "#D55E00")
        )
      }
    } else {
      if (!is.null(input$defineBy)) {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar::gg_miss_var(dplyr::group_by_at(data(), .vars = input$defineBy), show_pct = FALSE) + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both),
          "barsp" = naniar::gg_miss_var(dplyr::group_by_at(data(), .vars = input$defineBy), show_pct = TRUE) + ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both),
          "amount" = naniar::vis_miss(data(), warn_large_data = FALSE),
          "scatter" = {
            shiny::validate(
              shiny::need(input$defineSe != "", message = "Plot not meaningful if SEs are not provided.")
            )
            ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) +
              naniar::geom_miss_point() +
              ggplot2::facet_wrap(facets = input$defineBy, labeller = ggplot2::label_both)
          }
        )
      } else {
        plot <- switch(input$missingDataPlotType,
          "barsn" = naniar::gg_miss_var(data(), show_pct = FALSE),
          "barsp" = naniar::gg_miss_var(data(), show_pct = TRUE),
          "amount" = naniar::vis_miss(data(), warn_large_data = FALSE),
          "scatter" = {
            shiny::validate(
              shiny::need(input$defineSe != "", message = "Plot not meaningful if SEs are not provided.")
            )
            ggplot2::ggplot(data(), ggplot2::aes_string(x = input$defineEstvarname, y = input$defineSe)) +
              naniar::geom_miss_point()
          }
        )
      }
    }

    # Custom axis labels
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- apply_colour(ggobject = plot, theme = input$customTheme)

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

    if (input$defineSe != "") {
      subdata <- data()[, c(vars, input$defineEstvarname, input$defineSe), drop = FALSE]
    } else {
      subdata <- data()[, c(vars, input$defineEstvarname), drop = FALSE]
    }
    if (length(vars) > 0) {
      out <- naniar::miss_var_summary(dplyr::group_by_at(subdata, .vars = vars))
      names(out)[(length(names(out)) - 2):length(names(out))] <- c("Variable", "N. missing", "% Missing")
    } else {
      out <- naniar::miss_var_summary(subdata)
      names(out) <- c("Variable", "N. missing", "% Missing")
    }
    out
  })
  # Make DT of missing data table
  output$missingDataTable <- DT::renderDT(
    {
      missingDataTable()
    },
    options = list(pageLength = 20)
  )
  # Make LaTeX version of missing data table
  output$missingDataLaTeXTable <- shiny::renderPrint({
    shiny::req(data())
    knitr::kable(
      x = missingDataTable(),
      format = "latex",
      booktabs = input$booktabs,
      caption = input$missingDataLaTeXTableCaption,
      digits = input$significantDigits,
      row.names = FALSE
    )
  })

  ### Make summary statistics, and summary of 'simsum' object
  s <- reactive({
    shiny::req(data())

    # Process inputs
    if (input$defineSe == "") {
      inse <- NULL
    } else {
      inse <- input$defineSe
    }
    if (input$defineMethod == "") {
      inmethod <- NULL
    } else {
      inmethod <- input$defineMethod
    }
    if (input$defineRefMethod == "") {
      inref <- NULL
    } else {
      inref <- input$defineRefMethod
    }

    # Control estimation
    ctrl <- list(
      mcse = input$includeMCSE,
      level = input$rsimsumLevel,
      na.rm = input$rsimsum.na.rm,
      dropbig.max = input$rsimsumDropbig.max,
      dropbig.semax = input$rsimsumDropbig.semax,
      dropbig.robust = input$rsimsumDropbig.robust
    )
    # True
    if (input$whichTrue == "fixed") {
      intrue <- input$defineTrue
    } else if (input$whichTrue == "row-specific") {
      intrue <- input$defineTrueCol
    } else {
      intrue <- NULL
    }
    # CIs
    if (input$whichCIs == "fixed") {
      inci.limits <- NULL
    } else {
      inci.limits <- c(input$defineCIlower, input$defineCIupper)
    }

    # Call rsimsum
    s <- rsimsum::simsum(
      data = data(),
      estvarname = input$defineEstvarname,
      true = intrue,
      se = inse,
      methodvar = inmethod,
      ref = inref,
      by = input$defineBy,
      x = TRUE,
      ci.limits = inci.limits,
      dropbig = input$rsimsumDropbig,
      control = ctrl
    )
    s
  })

  summ <- shiny::reactive({
    shiny::req(data())
    s <- summary(s(), stats = input$summaryStatisticsWhich)
    return(s)
  })

  ### Make summary table pretty for printing
  prettySumm <- shiny::reactive({
    shiny::req(data())
    # Format summary table
    s <- rsimsum:::.format(x = summ(), digits = input$significantDigits, mcse = input$includeMCSE)
    # Make names of the summary table
    names(s$summ)[names(s$summ) == "description"] <- "Performance Measure"
    names(s$summ)[names(s$summ) == "est"] <- "Estimate"
    # Order data.frame with results
    s$summ <- rsimsum:::.order(data = s$summ, by = c("Performance Measure", s$methodvar, s$by))
    # If methodvar, put them side by side
    if (!is.null(s$methodvar)) {
      s$summ <- rsimsum:::.bind_methods(data = s$summ, by = s$by, methodvar = s$methodvar)
    }
    # Only selected factors if `by` is specified
    if (!is.null(input[["defineBy"]])) {
      s$summ <- split(s$summ, f = lapply(input$defineBy, function(f) s$summ[[f]]))[[paste(sapply(input$defineBy, function(x) input[[paste0("table", x)]]), collapse = ".")]]
      for (i in input$defineBy) {
        s$summ[[i]] <- NULL
      }
    }
    # Return
    return(s$summ)
  })

  ### Make a data table with the summary statistics
  shiny::observe({
    output$summaryStatisticsDataTable <- DT::renderDT(
      {
        shiny::req(data())
        s <- prettySumm()
      },
      options = list(pageLength = 20, dom = "t"),
      rownames = FALSE
    )
  })

  ### Make summary table in LaTeX
  shiny::observe({
    output$summaryStatisticsLaTeX <- shiny::renderPrint({
      shiny::req(data())
      knitr::kable(
        x = prettySumm(),
        format = "latex",
        booktabs = input$booktabs,
        linesep = "",
        caption = input$summaryStatisticsLaTeXCaption,
        digits = input$significantDigits,
        row.names = FALSE
      )
    })
  })

  ### Update caption of LaTeX table with current `by` scenario, if specified
  shiny::observe({
    shiny::req(input$defineBy)
    value <- paste(sapply(input$defineBy, function(x) {
      paste0(x, ": ", input[[paste0("table", x)]])
    }), collapse = ", ")
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

  ### Update available summary statistics for plotting
  shiny::observe({
    if (input$defineSe == "") {
      shiny::updateSelectInput(
        session,
        inputId = "selectPlotEstimates",
        choices = c(
          "Scatter Plot (Estimates vs Estimates)" = "est",
          "Bland-Altman Plot (Estimates vs Estimates)" = "est_ba",
          "Ridgeline Plot (Estimates vs Estimates)" = "est_ridge",
          "Density Plot (Estimates vs Estimates)" = "est_density",
          "Hexbin Plot (Estimates vs Estimates)" = "est_hex"
        )
      )
    } else {
      shiny::updateSelectInput(
        session,
        inputId = "selectPlotEstimates",
        choices = c(
          "Scatter Plot (Estimates vs Estimates)" = "est",
          "Scatter Plot (SEs vs SEs)" = "se",
          "Bland-Altman Plot (Estimates vs Estimates)" = "est_ba",
          "Bland-Altman Plot (SEs vs SEs)" = "se_ba",
          "Ridgeline Plot (Estimates vs Estimates)" = "est_ridge",
          "Ridgeline Plot (SEs vs SEs)" = "se_ridge",
          "Density Plot (Estimates vs Estimates)" = "est_density",
          "Density Plot (SEs vs SEs)" = "se_density",
          "Hexbin Plot (Estimates vs Estimates)" = "est_hex",
          "Hexbin Plot (SEs vs SEs)" = "se_hex"
        )
      )
    }
    if (input$whichTrue == "undefined" & input$defineSe == "") {
      if (input$defineMethod == "") {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "empse")]
      } else {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "empse", "relprec")]
      }
      shiny::updateSelectInput(
        session,
        inputId = "summaryStatisticsWhich",
        choices = nv,
        selected = nv
      )
    } else if (input$whichTrue != "undefined" & input$defineSe == "") {
      if (input$defineMethod == "") {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "bias", "empse", "mse")]
      } else {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "bias", "empse", "mse", "relprec")]
      }
      shiny::updateSelectInput(
        session,
        inputId = "summaryStatisticsWhich",
        choices = nv,
        selected = nv
      )
    } else if (input$whichTrue == "undefined" & input$defineSe != "") {
      if (input$defineMethod == "") {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "empse", "modelse", "relerror", "becover", "power")]
      } else {
        nv <- SummaryStatistics[SummaryStatistics %in% c("nsim", "thetamean", "thetamedian", "se2mean", "se2median", "empse", "relprec", "modelse", "relerror", "becover", "power")]
      }
      shiny::updateSelectInput(
        session,
        inputId = "summaryStatisticsWhich",
        choices = nv,
        selected = nv
      )
    } else {
      if (input$defineMethod == "") {
        nv <- SummaryStatistics[!(SummaryStatistics == "relprec")]
      } else {
        nv <- SummaryStatistics
      }
      shiny::updateSelectInput(
        session,
        inputId = "summaryStatisticsWhich",
        choices = nv,
        selected = nv
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
    shiny::validate(
      shiny::need(input$defineMethod != "", message = "Plots not meaningful if there are no methods to compare.")
    )

    df <- data()
    if (input$defineMethod != "") df[[input$defineMethod]] <- factor(df[[input$defineMethod]])

    # Make plots
    plot <- ggplot2::autoplot(s(), type = input$selectPlotEstimates)

    # Custom axis label
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- apply_colour(ggobject = plot, theme = input$customTheme)

    # Return plot
    plot
  }

  # Print estimates plot
  shiny::observe({
    output$outPlotEstimates <- shiny::renderPlot({
      shiny::req(data())
      makePlotEstimates()
    })
  })

  # Make summaries plot
  makePlotSummary <- function() {
    shiny::req(data())
    shiny::validate(
      shiny::need(!(input$whichTrue != "fixed" & input$selectPlotSummaryStat %in% c("thetamean", "thetamedian") & input$selectPlotSummary == "lolly"), message = "Lolly plot not available for the selected performance measure without a fixed true value."),
      shiny::need(!((input$defineSe == "" | input$whichTrue != "fixed") & input$selectPlotSummary == "zip"), message = "Zip plot not available without a (fixed) true value or standard errors."),
      shiny::need(!(is.null(input$defineBy) & input$selectPlotSummary == "nlp"), message = "Nested loop plot not meaningful when no 'by' factors are defined.")
    )

    # Make plots
    plot <- ggplot2::autoplot(summary(s()), type = input$selectPlotSummary, stats = input$selectPlotSummaryStat)

    # Custom axis label
    if (input$customXlab != "") plot <- plot + ggplot2::labs(x = input$customXlab)
    if (input$customYlab != "") plot <- plot + ggplot2::labs(y = input$customYlab)

    # Custom theme
    plot <- apply_theme(ggobject = plot, theme = input$customTheme)
    plot <- apply_colour(ggobject = plot, theme = input$customTheme)

    # Return plot
    plot
  }

  # Print summaries plot
  shiny::observe({
    output$outPlotSummary <- shiny::renderPlot({
      shiny::req(data())
      makePlotSummary()
    })
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
