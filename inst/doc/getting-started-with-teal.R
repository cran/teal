## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----app----------------------------------------------------------------------
library(teal)

app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(
    example_module("Module 1"),
    example_module("Module 2")
  ),
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", selected = "setosa")
  )
) |>
  modify_title("My first teal application") |>
  modify_header(h3("My first teal application")) |>
  modify_footer(tags$div(a("Powered by teal", href = "https://insightsengineering.github.io/teal/latest-tag/")))

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("app")
# ), collapse = "\n")
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

