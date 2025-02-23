## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----module_ui----------------------------------------------------------------
library(teal)

# UI function for the custom histogram module
histogram_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("dataset"),
      "Select Dataset",
      choices = c("iris", "mtcars")
    ),
    shiny::selectInput(
      ns("variable"),
      "Select Variable",
      choices = c(names(iris), names(mtcars))
    ),
    shiny::plotOutput(ns("histogram_plot")),
    shiny::verbatimTextOutput(ns("plot_code")) # To display the reactive plot code
  )
}

## ----module_server------------------------------------------------------------
# Server function for the custom histogram module with injected variables in within()
histogram_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Update dataset choices based on available datasets in teal_data
    shiny::observe({
      shiny::updateSelectInput(
        session,
        "dataset",
        choices = names(data())
      )
    })

    # Update variable choices based on selected dataset, only including numeric variables
    observeEvent(input$dataset, {
      req(input$dataset) # Ensure dataset is selected
      numeric_vars <- names(data()[[input$dataset]])[sapply(data()[[input$dataset]], is.numeric)]
      shiny::updateSelectInput(session, "variable", choices = numeric_vars)
    })

    # Create a reactive `teal_data` object with the histogram plot
    result <- reactive({
      req(input$dataset, input$variable) # Ensure both dataset and variable are selected

      # Create a new teal_data object with the histogram plot
      new_data <- within(
        data(),
        {
          my_plot <- hist(
            input_dataset[[input_vars]],
            las = 1,
            main = paste("Histogram of", input_vars),
            xlab = input_vars,
            col = "lightblue",
            border = "black"
          )
        },
        input_dataset = as.name(input$dataset), # Replace `input_dataset` with input$dataset
        input_vars = input$variable # Replace `input_vars` with input$variable
      )
      new_data
    })

    # Render the histogram from the updated teal_data object
    output$histogram_plot <- shiny::renderPlot({
      result()[["my_plot"]] # Access and render the plot stored in `new_data`
    })

    # Reactive expression to get the generated code for the plot
    output$plot_code <- shiny::renderText({
      teal.code::get_code(result()) # Retrieve and display the code for the updated `teal_data` object
    })
  })
}

## ----app_module---------------------------------------------------------------
# Custom histogram module creation
create_histogram_module <- function(label = "Histogram Module") {
  teal::module(
    label = label,
    ui = histogram_module_ui,
    server = histogram_module_server,
    datanames = "all"
  )
}

## ----app_init-----------------------------------------------------------------
# Define datasets in `teal_data`
data_obj <- teal_data(
  iris = iris,
  mtcars = mtcars
)

# Initialize the teal app
app <- init(
  data = data_obj,
  modules = modules(create_histogram_module())
)

# Run the app
if (interactive()) {
  shiny::shinyApp(ui = app$ui, server = app$server)
}

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("module_ui"),
#   knitr::knit_code$get("module_server"),
#   knitr::knit_code$get("app_module"),
#   knitr::knit_code$get("app_init")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

