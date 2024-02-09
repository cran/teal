## ----message=FALSE------------------------------------------------------------
library(teal)

app <- init(
  data = teal_data(IRIS = iris, MTCARS = mtcars),
  modules = modules(
    example_module("Module 1"),
    example_module("Module 2")
  ),
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", selected = "setosa")
  ),
  title = build_app_title(title = "My first teal app"),
  header = h3("My first teal application"),
  footer = div(a("Powered by teal", href = "https://insightsengineering.github.io/teal/latest-tag/"))
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

