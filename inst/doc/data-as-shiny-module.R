## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----as_interactive, eval=FALSE, echo=FALSE-----------------------------------
# interactive <- function() TRUE

## ----app_1--------------------------------------------------------------------
library(teal)

data_module <- teal_data_module(
  ui = function(id) tags$div(),
  server = function(id) {
    moduleServer(id, function(input, output, session) {
      reactive({
        data <- within(
          teal_data(),
          {
            dataset1 <- iris
            dataset2 <- mtcars
          }
        )
        data
      })
    })
  }
)


app <- init(
  data = data_module,
  modules = example_module()
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("app_1")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----app_2--------------------------------------------------------------------
data <- within(teal_data(), {
  dataset1 <- iris
  dataset2 <- mtcars
})

data_module <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      selectInput(ns("species"), "Select species to keep",
        choices = unique(iris$Species), multiple = TRUE
      ),
      actionButton(ns("submit"), "Submit")
    )
  },
  server = function(id) {
    moduleServer(id, function(input, output, session) {
      eventReactive(input$submit, {
        data_modified <- within(
          data,
          dataset1 <- subset(dataset1, Species %in% selected),
          selected = input$species
        )
        data_modified
      })
    })
  }
)

app <- init(
  data = data_module,
  modules = example_module()
)

if (interactive()) {
  shiny::shinyApp(ui = app$ui, server = app$server)
}

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("app_2")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----app_3_prep, eval=FALSE, echo=FALSE---------------------------------------
# library(teal)
# 
# data <- within(teal_data(), {
#   dataset1 <- iris
#   dataset2 <- mtcars
# })
# 
# data_module <- teal_data_module(
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       selectInput(ns("species"), "Select species to keep",
#         choices = unique(iris$Species), multiple = TRUE
#       ),
#       actionButton(ns("submit"), "Submit")
#     )
#   },
#   server = function(id) {
#     moduleServer(id, function(input, output, session) {
#       eventReactive(input$submit, {
#         data_modified <- within(
#           data,
#           dataset1 <- subset(dataset1, Species %in% selected),
#           selected = input$species
#         )
#         data_modified
#       })
#     })
#   }
# )

## ----app_3--------------------------------------------------------------------
data_module_2 <- within(
  data_module,
  {
    # Create new column with Ratio of Sepal.Width and Petal.Width
    dataset1$Ratio.Sepal.Petal.Width <- round(dataset1$Sepal.Width / dataset1$Petal.Width, digits = 2L)
    # Create new column that converts Miles per Galon to Liter per 100 Km
    dataset2$lp100km <- round(dataset2$mpg * 0.42514371, digits = 2L)
  }
)

app <- init(
  data = data_module_2,
  modules = example_module()
)

if (interactive()) {
  shiny::shinyApp(ui = app$ui, server = app$server)
}

## ----shinylive_iframe_3, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("app_3_prep"),
#   knitr::knit_code$get("app_3")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

