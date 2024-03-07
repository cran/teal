## ----include=FALSE------------------------------------------------------------
knitr::opts_template$set(
  remove_linter_comments = list(tidy = function(code, ...) gsub(pattern = "#\\s?nolint.*", replacement = "", code))
)

## ----eval = FALSE, opts.label=c("remove_linter_comments")---------------------
#  options("teal.bs_theme" = bslib::bs_theme(version = "5"))
#  
#  library(teal)
#  
#  app <- init(
#    data = teal_data(IRIS = iris), # nolint: line_length.
#    filter = teal_slices(teal_slice("IRIS", "Sepal.Length", selected = c(5, 7))),
#    modules = modules(example_module(), example_module()),
#    header = "My first teal application"
#  )
#  
#  bslib::run_with_themer(shinyApp(app$ui, app$server))

## ----eval = FALSE-------------------------------------------------------------
#  ####  Update your bs_theme() R code with:  #####
#  bs_theme_update(theme, bootswatch = "minty")

## ----eval = FALSE-------------------------------------------------------------
#  options(
#    "teal.bs_theme" = bslib::bs_theme(
#      version = "5",
#      font_scale = 1.25,
#      `enable-rounded` = FALSE,
#      bootswatch = "minty"
#    )
#  )
#  
#  library(teal)
#  
#  app <- init(
#    data = teal_data(IRIS = iris),
#    filter = teal_slices(teal_slice("IRIS", "Sepal.Length", selected = c(5, 7))),
#    modules = modules(example_module(), example_module())
#  )
#  
#  shinyApp(app$ui, app$server)

