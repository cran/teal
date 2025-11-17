## ----setup, include=FALSE-----------------------------------------------------
library(teal)
library(teal.reporter)

## ----as_interactive, eval=FALSE, echo=FALSE-----------------------------------
# interactive <- function() TRUE

## ----module_1-----------------------------------------------------------------
library(teal)
library(teal.reporter)

my_module <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data) {
      checkmate::assert_class(isolate(data()), "teal_data")

      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(names(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(selectInput(ns("dataname"), "Choose a dataset", choices = NULL)),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

## ----app_1--------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = my_module()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("module_1"),
#   knitr::knit_code$get("app_1")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----module_2-----------------------------------------------------------------
my_module_with_card <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(names(data())))

        # Prepare the report:
        report <- reactive({
          req(obj <- data())
          teal_card(obj) <-
            c(
              teal_card("# Module with reporting"),
              teal_card(obj),
              teal_card("## Module's code")
            )
          obj
        })

        # Add to the report the code of the module
        data_r <- reactive({
          req(teal_data <- report(), input$dataname)
          within(teal_data, table, table = as.name(input$dataname))
        })

        output$dataset <- renderPrint({
          req(teal_data <- data_r())
          teal_data[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(selectInput(ns("dataname"), "Choose a dataset", choices = NULL)),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

## ----app_2--------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = my_module_with_card()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("module_2"),
#   knitr::knit_code$get("app_2")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----module_3-----------------------------------------------------------------
my_module_with_reporting <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(names(data())))

        # Prepare the report:
        report <- reactive({
          req(obj <- data())
          teal_card(obj) <-
            c(
              teal_card("# Module with reporting"),
              teal_card(obj),
              teal_card("## Module's code")
            )
          obj
        })

        # Add to the report the code of the module
        data_r <- reactive({
          req(rtd <- report(), input$dataname)
          within(rtd, table, table = as.name(input$dataname))
        })

        output$dataset <- renderPrint({
          req(dr <- data_r())
          dr[[input$dataname]]
        })

        # the reactive teal_report is returned by the module
        data_r
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(selectInput(ns("dataname"), "Choose a dataset", choices = NULL)),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

## ----app_3--------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = my_module_with_reporting()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_3, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("module_3"),
#   knitr::knit_code$get("app_3")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----app_4--------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = my_module_with_reporting() |> disable_src()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_4, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("module_3"),
#   knitr::knit_code$get("app_4")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----eval = FALSE-------------------------------------------------------------
# app <- init(
#   data = teal_data(IRIS = iris, MTCARS = mtcars),
#   modules = c(
#     modules(
#       label = "One nested module disabled",
#       example_module(label = "Module 1"),
#       example_module(label = "Module 2"),
#       example_module(label = "Module 3") |> disable_src()
#     ),
#     modules(
#       label = "Nested modules without source",
#       example_module(label = "Module 1"),
#       example_module(label = "Module 2")
#     ) |> disable_src()
#   )
# )

## ----app_5--------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = my_module_with_reporting() |> disable_report()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_5, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("module_3"),
#   knitr::knit_code$get("app_5")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----reporter_6---------------------------------------------------------------
reporter <- Reporter$new()
template_fun <- function(document) {
  header <- teal_card("Here comes header text.")
  logo_url <- "https://raw.githubusercontent.com/insightsengineering/teal/refs/heads/main/man/figures/logo.svg"
  footer <- teal_card(paste0(
    "Here comes footer text. Report generated with teal ![logo](%s 'teal logo'){height=70}",
    logo_url
  ))
  c(header, document, footer)
}
reporter$set_template(template_fun)

card1 <- teal_card("## Header 2 text", "Regular text")
metadata(card1, "title") <- "Welcome card"
reporter$append_cards(card1)

