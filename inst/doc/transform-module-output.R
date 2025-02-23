## ----setup, include=FALSE-----------------------------------------------------
library(teal)
library(ggplot2)

## ----static_decorator---------------------------------------------------------
static_decorator <- teal_transform_module(
  label = "Static decorator",
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(), {
          plot <- plot +
            ggtitle("This is a better title") +
            xlab("the real x axis")
        })
      })
    })
  }
)

## ----interactive_decorator----------------------------------------------------
interactive_decorator <- teal_transform_module(
  label = "Interactive decorator",
  ui = function(id) {
    ns <- NS(id)
    div(
      textInput(ns("x_axis_title"), "X axis title", value = "the suggested x axis")
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(),
          {
            plot <- plot +
              ggtitle("This is a better title") +
              xlab(my_title)
          },
          my_title = input$x_axis_title
        )
      })
    })
  }
)

## ----dynamic_decorator--------------------------------------------------------
dynamic_decorator <- function(output_name) {
  teal_transform_module(
    label = "Dynamic decorator",
    ui = function(id) {
      ns <- NS(id)
      div(
        textInput(ns("x_axis_title"), "X axis title", value = "the syggested x axis")
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          req(data())
          within(data(),
            {
              output_name <- output_name +
                xlab(x_axis_title)
            },
            output_name = as.name(output_name),
            x_axis_title = input$x_axis_title
          )
        })
      })
    }
  )
}

## ----pseudo_module, eval = FALSE----------------------------------------------
# # styler: off
# pseudo_decorated_module <- function(
#   label = "Pseudo Module with Decorator Support",
#   decorators = list()                                                       # <--- added block (1)
# ) {
#   module(
#     label = label,
#     ui_args = list(decorators = decorators),                                  # <--- added block (2)
#     server_args = list(decorators = decorators),                              # <--- added block (2)
#     ui = function(id, decorators) {
#       ns <- NS(id)
#       div(
#         # <input widgets>,
#         # <output widgets>,
#         ui_transform_teal_data(ns("decorate"), transformators = decorators)   # <--- added block (3)
#       )
#     },
#     server = function(id, data, decorators) {
#       moduleServer(id, function(input, output, session) {
#         # <receive inputs>
#         # <process data>
#         data_with_output <- reactive({
#           within(data(), output_item <- generate_output())
#         })
#         data_with_output_decorated <- srv_transform_teal_data(                # <--- added block (3)
#           "decorate",                                                         # <-
#           data = data_with_output,                                            # <-
#           transformators = decorators                                         # <-
#         )                                                                     # <--- added block (3)
#         # <render output>
#       })
#     }
#   )
# }
# # styler: on

## ----tm_decorated_plot--------------------------------------------------------
tm_decorated_plot <- function(label = "module", decorators = list()) {
  checkmate::assert_list(decorators, "teal_transform_module", null.ok = TRUE)

  module(
    label = label,
    ui_args = list(decorators = decorators),
    server_args = list(decorators = decorators),
    ui = function(id, decorators) {
      ns <- NS(id)
      div(
        selectInput(ns("dataname"), label = "select dataname", choices = NULL),
        selectInput(ns("x"), label = "select x", choices = NULL),
        selectInput(ns("y"), label = "select y", choices = NULL),
        ui_transform_teal_data(ns("decorate"), transformators = decorators),
        plotOutput(ns("plot")),
        verbatimTextOutput(ns("text"))
      )
    },
    server = function(id, data, decorators) {
      moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
          updateSelectInput(inputId = "dataname", choices = names(data()))
        })

        observeEvent(input$dataname, {
          req(input$dataname)
          updateSelectInput(inputId = "x", choices = colnames(data()[[input$dataname]]))
          updateSelectInput(inputId = "y", choices = colnames(data()[[input$dataname]]))
        })

        dataname <- reactive(req(input$dataname))
        x <- reactive({
          req(input$x, input$x %in% colnames(data()[[dataname()]]))
          input$x
        })
        y <- reactive({
          req(input$y, input$y %in% colnames(data()[[dataname()]]))
          input$y
        })

        # Plot is created within the teal_data object
        data_with_plot <- reactive({
          req(dataname(), x(), y())
          within(data(),
            {
              plot <- ggplot2::ggplot(dataname, ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_point()
            },
            dataname = as.name(dataname()),
            x = as.name(x()),
            y = as.name(y())
          )
        })

        # Decorators are applied
        data_with_plot_decorated <- srv_transform_teal_data(
          "decorate",
          data = data_with_plot,
          transformators = decorators
        )

        # (Decorated) plot object is extracted for rendering
        plot_r <- reactive({
          data_with_plot_decorated()[["plot"]]
        })

        # Add plot printing statement to reproducible code
        ## This does not affect the analysis but when the code is "replayed"
        ## in an interactive session it will send the plot to a graphics device.
        reproducible_code <- reactive({
          within(data_with_plot_decorated(), expr = plot) |>
            teal.code::get_code()
        })

        output$plot <- renderPlot(plot_r())
        output$text <- renderText(reproducible_code())
      })
    }
  )
}

## ----app_1--------------------------------------------------------------------
app <- init(
  data = teal_data(iris = iris, mtcars = mtcars),
  modules = modules(
    tm_decorated_plot("undecorated"),
    tm_decorated_plot("static", decorators = list(static_decorator)),
    tm_decorated_plot("interactive", decorators = list(interactive_decorator)),
    tm_decorated_plot("dynamic", decorators = list(dynamic_decorator("plot")))
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("static_decorator"),
#   knitr::knit_code$get("interactive_decorator"),
#   knitr::knit_code$get("dynamic_decorator"),
#   knitr::knit_code$get("tm_decorated_plot"),
#   knitr::knit_code$get("app_1")
# ), collapse = "\n")
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----plot_decorator-----------------------------------------------------------
plot_decorator <- teal_transform_module(
  label = "Decorate plot",
  ui = function(id) {
    ns <- NS(id)
    textInput(ns("plot_title"), "Plot Title", value = "Title (editable)")
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(),
          {
            plot <- plot + ggplot2::ggtitle(ptitle) +
              ggplot2::theme_minimal() +
              ggplot2::theme(
                plot.title = element_text(face = "bold", size = 30, color = "blue")
              )
          },
          ptitle = input$plot_title
        )
      })
    })
  }
)

## ----table_decorator----------------------------------------------------------
table_decorator <- teal_transform_module(
  label = "Decorate table",
  ui = function(id) shiny::tags$p("No UI needed for table decorator and could be ommited."),
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(), {
          table_data[["Added by decorator"]] <- paste0("Row ", seq_len(nrow(table_data)))
        })
      })
    })
  }
)

## ----tm_decorated_plot_table--------------------------------------------------
tm_decorated_plot_table <- function(label = "module with two outputs", decorators = list()) {
  checkmate::assert_list(decorators, "teal_transform_module", null.ok = TRUE)

  module(
    label = label,
    ui_args = list(decorators = decorators),
    server_args = list(decorators = decorators),
    ui = function(id, decorators) {
      ns <- NS(id)
      div(
        selectInput(ns("dataname"), label = "Select dataset", choices = NULL),
        selectInput(ns("x"), label = "Select x-axis", choices = NULL),
        selectInput(ns("y"), label = "Select y-axis", choices = NULL),

        # Separately inject UI for plot and table decorators
        ui_transform_teal_data(ns("decorate_plot"), transformators = decorators$plot),
        ui_transform_teal_data(ns("decorate_table"), transformators = decorators$table),
        plotOutput(ns("plot")),
        tableOutput(ns("table")),
        verbatimTextOutput(ns("text"))
      )
    },
    server = function(id, data, decorators) {
      moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
          updateSelectInput(inputId = "dataname", choices = names(data()))
        })

        dataname <- reactive(req(input$dataname))

        observeEvent(dataname(), {
          updateSelectInput(inputId = "x", choices = colnames(data()[[input$dataname]]))
          updateSelectInput(inputId = "y", choices = colnames(data()[[input$dataname]]))
        })
        x <- reactive({
          req(input$x, input$x %in% colnames(data()[[dataname()]]))
          input$x
        })
        y <- reactive({
          req(input$y, input$y %in% colnames(data()[[dataname()]]))
          input$y
        })

        # Separately create outputs within teal_data objects in separate reactive expressions
        plot_data <- reactive({
          req(dataname(), x(), y())
          within(data(),
            {
              plot <- ggplot2::ggplot(dataname, ggplot2::aes(x = xvar, y = yvar)) +
                ggplot2::geom_point()
            },
            dataname = as.name(dataname()),
            xvar = as.name(x()),
            yvar = as.name(y())
          )
        })
        table_data <- reactive({
          req(dataname())
          within(data(),
            {
              table_data <- data.frame(lapply(dataname, mean, na.rm = TRUE))
            },
            dataname = as.name(dataname())
          )
        })

        # Separately apply decoration to the outputs
        decorated_plot <- srv_transform_teal_data(
          "decorate_plot",
          data = plot_data,
          transformators = decorators$plot
        )
        decorated_table <- srv_transform_teal_data(
          "decorate_table",
          data = table_data,
          transformators = decorators$table
        )

        output$plot <- renderPlot(decorated_plot()[["plot"]])
        output$table <- renderTable(decorated_table()[["table_data"]])

        output$text <- renderText({
          plot_code <- teal.code::get_code(req(decorated_plot()))
          table_code <- teal.code::get_code(req(decorated_table()))
          paste("# Plot Code:", plot_code, "\n\n# Table Code:", table_code)
        })
      })
    }
  )
}

## ----app_2--------------------------------------------------------------------
app <- init(
  data = teal_data(iris = iris, mtcars = mtcars),
  modules = modules(
    tm_decorated_plot_table(
      "plot_and_table",
      decorators = list(
        plot = plot_decorator,
        table = table_decorator
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("plot_decorator"),
#   knitr::knit_code$get("table_decorator"),
#   knitr::knit_code$get("tm_decorated_plot_table"),
#   knitr::knit_code$get("app_2")
# ), collapse = "\n")
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----eval=FALSE---------------------------------------------------------------
# teal_transform_module(
#   label = "Static decorator",
#   ui = function(id) {
#     ns <- NS(id)
#     div(
#       textInput(ns("x_axis_title"), "X axis title", value = "x axis")
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         req(data())
#         within(
#           data(),
#           {
#             plot <- plot + ggtitle("This is a better title") + xlab(x_axis_title)
#           },
#           x_axis_title = input$x_axis_title
#         )
#       })
#     })
#   }
# )
# 
# teal_transform_module(
#   label = "Static decorator (language)",
#   ui = function(id) {
#     ns <- NS(id)
#     div(
#       textInput(ns("x_axis_title"), "X axis title", value = "x axis")
#     )
#   },
#   server = make_teal_transform_server(
#     expression(
#       plot <- plot + ggtitle("This is a better title") + xlab(x_axis_title)
#     )
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# # in the module UI function
# div(
#   id = ns("deorator_container"),
#   lapply(names(decorators), function(decorator_name) {
#     div(
#       id = ns(paste0("decorate_", decorator_name)),
#       ui_transform_teal_data(
#         ns(paste0("decorate_", decorator_name)),
#         transformators = decorators[[decorator_name]]
#       )
#     )
#   })
# )
# # in the module server function
# output_data <- reactive(teal_data())
# decorations <- lapply(names(decorators), function(decorator_name) {
#   function(data) {
#     srv_transform_teal_data(
#       paste0("decorate_", decorator_name),
#       data = data,
#       transformators = decorators[[decorator_name]]
#     )
#   }
# })
# output_data_decorated <- Reduce(function(f, ...) f(...), decorations, init = output_data, right = TRUE)

