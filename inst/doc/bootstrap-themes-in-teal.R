## ----include=FALSE------------------------------------------------------------
knitr::opts_template$set(
  remove_linter_comments = list(tidy = function(code, ...) gsub(pattern = "#\\s?nolint.*", replacement = "", code))
)

## ----eval = FALSE, opts.label=c("remove_linter_comments")---------------------
# options("teal.bs_theme" = bslib::bs_theme(version = "5"))
# 
# library(teal)
# 
# app <- init(
#   data = teal_data(IRIS = iris), # nolint: line_length.
#   filter = teal_slices(teal_slice("IRIS", "Sepal.Length", selected = c(5, 7))),
#   modules = modules(example_module(), example_module()),
# )
# 
# bslib::run_with_themer(shinyApp(app$ui, app$server))

## ----eval = FALSE-------------------------------------------------------------
# ####  Update your bs_theme() R code with:  #####
# bs_theme_update(theme, bootswatch = "minty")

## ----setup, include=FALSE-----------------------------------------------------
library(teal)

## ----app----------------------------------------------------------------------
options(
  "teal.bs_theme" = bslib::bs_theme(
    version = "5",
    font_scale = 1.25,
    bootswatch = "minty"
  )
)

library(teal)

app <- init(
  data = teal_data(IRIS = iris),
  filter = teal_slices(teal_slice("IRIS", "Sepal.Length", selected = c(5, 7))),
  modules = modules(example_module(), example_module())
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("app")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

