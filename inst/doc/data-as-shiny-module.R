## ----message = FALSE, warning = FALSE-----------------------------------------
library(teal)

## -----------------------------------------------------------------------------
data_module <- teal_data_module(
  ui = function(id) div(),
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
        datanames(data) <- c("dataset1", "dataset2") # optional
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

## -----------------------------------------------------------------------------
data <- within(teal_data(), {
  dataset1 <- iris
  dataset2 <- mtcars
})
datanames(data) <- c("dataset1", "dataset2")

data_module <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    div(
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
  shinyApp(app$ui, app$server)
}

## -----------------------------------------------------------------------------
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
  shinyApp(app$ui, app$server)
}

