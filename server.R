## server.R script ##
source("helpers.R")

function(input, output, session) {
	data <- reactive({
		inFile <- input$file1
		if (is.null(inFile)) return(NULL)
		data <- read_csv(inFile$datapath)
		validate(
			need(all(c("method", "par", "true_value", "value", "se") %in% names(data)), "Input dataset not formatted properly.")
		)
		data
	})

	results <- reactive({
		if (is.null(data())) return(NULL)
		results <- data() %>%
			mutate(bias = (true_value - value)) %>%
			group_by_(.dots = lapply(c("method", "par", names(data())[!(names(data()) %in% c("method", "par", "true_value", "value", "se"))]), as.symbol)) %>%
			summarise(`Bias (mean)` = mean(bias),
								`Bias (median)` = median(bias),
								`SE (mean)` = mean(se),
								`SE (median)` = median(se)) %>%
			ungroup()
		results
	})

	output$renUI <- renderUI({
		vars <- names(data())[!(names(data()) %in% c("method", "true_value", "value", "se"))]
		tL <- lapply(vars, function(x) selectInput(x, x, unique(get(x, data()))))
		tagList(tL)
	})

	output$data_table <- renderDataTable({
		data()
	})

	make_results_table <- function() {
		vars <- names(data())[!(names(data()) %in% c("method", "true_value", "value", "se"))]
		if (is.null(results())) {
		} else {
			out <- results() %>%
				mutate_(.dots = lapply(vars, as.character)) %>%
				filter_(.dots = lapply(vars, function(v) paste0(v, " == '", input[[v]], "'"))) %>%
				select_(.dots = lapply(vars, function(v) paste0("-", v)))
			out %>%
				gather_(key_col = "Statistic", value_col = "value", gather_cols = names(out)[which(names(out) != "method")]) %>%
				spread_(key_col = "method", value_col = "value")
		}
	}

	output$results_table <- renderTable({
		make_results_table()
	},
	width = 100,
	digits = 4,
	striped = TRUE,
	hover = TRUE,
	bordered = TRUE
	)

	output$table_latex <- renderPrint({
		make_results_table() %>%
			xtable() %>%
			print()
	})

	make_barplot <- function() {
		vars <- names(data())[!(names(data()) %in% c("method", "true_value", "value", "se"))]
		if (is.null(results())) {
		} else {
			out <- results() %>%
				mutate_(.dots = lapply(vars, as.character)) %>%
				filter_(.dots = lapply(vars, function(v) paste0(v, " == '", input[[v]], "'"))) %>%
				select_(.dots = lapply(vars, function(v) paste0("-", v)))
			out %>%
				gather_(key_col = "Statistic", value_col = "value", gather_cols = names(out)[which(names(out) != "method")]) %>%
				filter(Statistic == input$plot_stat) %>%
				ggplot(aes(x = method, y = value)) +
				geom_bar(stat = "identity") +
				theme_bw() +
				labs(x = "Method", y = input$plot_stat)
		}
	}

	output$results_plot <- renderPlot({
		make_barplot()
	})

	output$download_plot <- downloadHandler(
		filename = function() {paste0("plot.", input$plot_format)},
		content = function(file) {
			ggsave(file, plot = make_barplot(), width = input$plot_width, height = input$plot_height, dpi = input$plot_resolution, device = input$plot_format)
		}
	)

}
