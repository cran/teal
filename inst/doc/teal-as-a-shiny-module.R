## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----app----------------------------------------------------------------------
library(teal)

data <- teal_data() |> within({
  iris <- iris
  mtcars <- mtcars
  df <- data.frame(a = 1:10, b = letters[1:10])
})

mods <- modules(
  example_module("mod1"),
  example_module("mod2")
)

ui_app <- fluidPage(
  title = "Your app with teal as a module",
  selectInput("datasets", "Select datasets", choices = c("iris", "mtcars", "df"), selected = "iris", multiple = TRUE),
  ui_teal("teal", mods),
  ui_session_info("session_info")
)

srv_app <- function(input, output, session) {
  data_subset <- reactive(data[input$datasets])
  srv_teal("teal", data = data_subset, modules = mods)
  srv_session_info("session_info")
}

if (interactive()) {
  shinyApp(ui_app, srv_app)
}

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("app")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

