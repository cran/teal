## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----as_interactive, eval=FALSE, echo=FALSE-----------------------------------
# interactive <- function() TRUE

## ----app_1, eval = FALSE------------------------------------------------------
# library(teal)
# 
# data <- within(teal_data(), {
#   iris <- iris
#   mtcars <- mtcars
# })
# 
# app <- init(
#   data = data,
#   modules = example_module()
# )
# 
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("app_1")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----teal_lib_call, eval=FALSE, echo=FALSE------------------------------------
# library(teal)

## ----app_2, eval = FALSE------------------------------------------------------
# data <- within(teal_data(), {
#   iris <- iris
#   mtcars <- mtcars
# })
# 
# transformator_iris <- teal_transform_module(
#   label = "Custom transformator for iris",
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       numericInput(ns("n_rows"), "Number of rows to display", value = 6, min = 1, max = 150, step = 1)
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         within(
#           data(),
#           iris <- head(iris, num_rows),
#           num_rows = input$n_rows
#         )
#       })
#     })
#   }
# )
# 
# app <- init(
#   data = data,
#   modules = example_module(transformators = list(transformator_iris))
# )
# 
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("app_2")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----app_3, eval = FALSE------------------------------------------------------
# data <- within(teal_data(), {
#   iris <- iris
#   mtcars <- mtcars
# })
# 
# transformator_iris <- teal_transform_module(
#   label = "Custom transformator for iris",
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       numericInput(ns("n_rows"), "Number of rows to subset", value = 6, min = 1, max = 150, step = 1)
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         within(
#           data(),
#           iris <- head(iris, num_rows),
#           num_rows = input$n_rows
#         )
#       })
#     })
#   }
# )
# 
# transformator_mtcars <- teal_transform_module(
#   label = "Custom transformator for mtcars",
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       "Adding rownames column to mtcars"
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         within(data(), {
#           mtcars$rownames <- rownames(mtcars)
#           rownames(mtcars) <- NULL
#         })
#       })
#     })
#   }
# )
# 
# my_transformators <- list(
#   transformator_iris,
#   transformator_mtcars
# )
# 
# app <- init(
#   data = data,
#   modules = example_module(transformators = my_transformators)
# )
# 
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }

## ----shinylive_iframe_3, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("app_3")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----app_4, eval = FALSE------------------------------------------------------
# data <- within(teal_data(), {
#   iris <- iris
#   mtcars <- mtcars
# })
# 
# transformator_iris_scale <- teal_transform_module(
#   label = "Scaling transformator for iris",
#   ui = function(id) {
#     ns <- NS(id)
#     uiOutput(ns("scaled_columns_container"))
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       ns <- session$ns
# 
#       scalable_columns <- names(Filter(is.numeric, data()[["iris"]])) |> isolate()
# 
#       output$scaled_columns_container <- renderUI({
#         selectInput(
#           inputId = ns("scaled_columns"),
#           label = "Columns to scale",
#           choices = scalable_columns,
#           selected = input$scaled_columns,
#           multiple = TRUE
#         )
#       })
# 
#       reactive({
#         within(
#           data(),
#           {
#             iris[scaled_columns] <- scale(iris[scaled_columns])
#           },
#           scaled_columns = input$scaled_columns
#         )
#       })
#     })
#   }
# )
# 
# transformator_iris <- teal_transform_module(
#   label = "Custom transformator for iris",
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       numericInput(ns("n_rows"), "Number of rows to subset", value = 6, min = 1, max = 150, step = 1)
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         within(
#           data(),
#           iris <- head(iris, num_rows),
#           num_rows = input$n_rows
#         )
#       })
#     })
#   }
# )
# 
# transformator_mtcars <- teal_transform_module(
#   label = "Custom transformator for mtcars",
#   ui = function(id) {
#     ns <- NS(id)
#     tags$div(
#       "Adding rownames column to mtcars"
#     )
#   },
#   server = function(id, data) {
#     moduleServer(id, function(input, output, session) {
#       reactive({
#         within(data(), {
#           mtcars$rownames <- rownames(mtcars)
#           rownames(mtcars) <- NULL
#         })
#       })
#     })
#   }
# )
# 
# my_transformators <- list(
#   transformator_iris,
#   transformator_iris_scale,
#   transformator_mtcars
# )
# 
# app <- init(
#   data = data,
#   modules = example_module(transformators = my_transformators)
# )
# 
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }

## ----shinylive_iframe_4, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   knitr::knit_code$get("as_interactive"),
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("app_4")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

