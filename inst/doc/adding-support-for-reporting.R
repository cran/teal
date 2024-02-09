## ----message=FALSE------------------------------------------------------------
library(teal)
example_module <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data) {
      checkmate::assert_class(data, "reactive")
      checkmate::assert_class(isolate(data()), "teal_data")

      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
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

## ----eval = FALSE-------------------------------------------------------------
#  app <- init(
#    data = teal_data(IRIS = iris, MTCARS = mtcars),
#    modules = example_module()
#  )
#  
#  if (interactive()) shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
example_module_with_reporting <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data, reporter) {
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
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

## -----------------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = example_module_with_reporting()
)

if (interactive()) shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
example_module_with_reporting <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data, reporter) {
      moduleServer(id, function(input, output, session) {
        teal.reporter::simple_reporter_srv(
          id = "reporter",
          reporter = reporter,
          card_fun = function(card) card
        )
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(
          teal.reporter::simple_reporter_ui(ns("reporter")),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
        ),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

## -----------------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = example_module_with_reporting()
)

if (interactive()) shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
custom_function <- function(card = teal.reporter::ReportCard$new()) {
  card$append_text("This is content from a custom teal module!")
  card
}

example_module_with_reporting <- function(label = "example teal module") {
  module(
    label = label,
    server = function(id, data, reporter) {
      moduleServer(id, function(input, output, session) {
        teal.reporter::simple_reporter_srv(
          id = "reporter",
          reporter = reporter,
          card_fun = custom_function
        )
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(
        sidebarPanel(
          teal.reporter::simple_reporter_ui(ns("reporter")),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
        ),
        mainPanel(verbatimTextOutput(ns("dataset")))
      )
    }
  )
}

## -----------------------------------------------------------------------------
app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = example_module_with_reporting()
)

if (interactive()) shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
custom_function <- function(card = TealReportCard$new()) {
  # ... some code ... #
  card
}

## -----------------------------------------------------------------------------
library(teal)
library(teal.reporter)

example_reporter_module <- function(label = "Example") {
  module(
    label = label,
    server = function(id, data, reporter, filter_panel_api) {
      with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelApi")
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        dat <- reactive(data()[[input$dataname]])
        observe({
          req(dat())
          updateSliderInput(session, "nrow", max = nrow(dat()), value = floor(nrow(dat()) / 5))
        })

        table_q <- reactive({
          req(input$dataname)
          req(input$nrow)
          within(
            data(),
            result <- head(dataset, nrows),
            dataset = as.name(input$dataname),
            nrows = input$nrow
          )
        })

        output$table <- renderTable(table_q()[["result"]])

        ### REPORTER
        card_fun <- function(card = teal.reporter::ReportCard$new(), comment) {
          card$set_name("Table Module")
          card$append_text(paste("Selected dataset", input$dataname), "header2")
          card$append_text("Selected Filters", "header3")
          if (with_filter) {
            card$append_text(filter_panel_api$get_filter_state(), "verbatim")
          }
          card$append_text("Encoding", "header3")
          card$append_text(
            yaml::as.yaml(
              stats::setNames(
                lapply(c("dataname", "nrow"), function(x) input[[x]]), c("dataname", "nrow")
              )
            ),
            "verbatim"
          )
          card$append_text("Module Table", "header3")
          card$append_table(table_q()[["result"]])
          card$append_text("Show R Code", "header3")
          card$append_text(teal.code::get_code(table_q()), "verbatim")
          if (!comment == "") {
            card$append_text("Comment", "header3")
            card$append_text(comment)
          }
          card
        }
        teal.reporter::add_card_button_srv(
          "addReportCard",
          reporter = reporter,
          card_fun = card_fun
        )
        teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
        teal.reporter::reset_report_button_srv("resetButton", reporter)
        ###
      })
    },
    ui = function(id) {
      ns <- NS(id)

      sidebarLayout(
        sidebarPanel(selectInput(ns("dataname"), "Choose a dataset", choices = NULL)),
        mainPanel(
          teal.reporter::simple_reporter_ui(ns("reporter")),
          verbatimTextOutput(ns("dataset"))
        )
      )

      sidebarLayout(
        sidebarPanel(
          div(
            teal.reporter::add_card_button_ui(ns("addReportCard")),
            teal.reporter::download_report_button_ui(ns("downloadButton")),
            teal.reporter::reset_report_button_ui(ns("resetButton"))
          ),
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          sliderInput(ns("nrow"), "Number of rows", min = 1, max = 1, value = 1, step = 1)
        ),
        mainPanel(tableOutput(ns("table")))
      )
    }
  )
}

app <- init(
  data = teal_data(AIR = airquality, IRIS = iris),
  modules = list(
    example_reporter_module(label = "with Reporter"),
    example_module(label = "without Reporter")
  ),
  filter = teal_slices(teal_slice(dataname = "AIR", varname = "Temp", selected = c(72, 85))),
  header = "Example teal app with reporter"
)

if (interactive()) shinyApp(app$ui, app$server)

