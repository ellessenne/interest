## server.R script ##
source("helpers.R")

function(input, output, session) {

	# Make data selectors on the fly
	output$loadData = renderUI({
		switch(input$typeDataUpload,
					 "uploadData" = fileInput(inputId = "uploadData", label = NULL, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
					 "linkData" = textInput(inputId = "linkData", label = NULL),
					 "pasteData" = textAreaInput(inputId = "pasteData", label = NULL))
	})

	# Functions to load data
	data = reactive({
		df = switch(input$typeDataUpload,
									"uploadData" = {
										inFile = input$uploadData
										if (is.null(inFile)) return(NULL)
										read_csv(inFile$datapath)
										},
									"linkData" = {
										inFile = input$linkData
										if (is.null(inFile)) return(NULL)
										read_csv(inFile)
										},
									"pasteData" = {
										inFile = input$linkData
										if (is.null(inFile)) return(NULL)
										read_csv(inFile)
										})
		if (!is.null(df)) {
			validate(
				need("i" %in% names(df), "Replication ID 'i' not in the dataset"),
				need("par" %in% names(df), "Estimand 'par' not in the dataset"),
				need("coef" %in% names(df), "Estimated values 'coef' not in the dataset"),
				need("se" %in% names(df), "Standard errors 'se' not in the dataset"),
				need("true" %in% names(df), "True value for each estimand 'true' not in the dataset"),
				need("method" %in% names(df), "Variable with methods to compare 'method' not in the dataset")
				)}
		return(df)
		})

	# Make a data table with the original dataset
	output$uploadedDataTable = renderDataTable({
		data()
	})
}
