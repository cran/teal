## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(teal)

# create teal_data
data <- teal_data(iris = iris, cars = mtcars)

## ----eval=FALSE---------------------------------------------------------------
# # build app
# app <- init(
#   data = data,
#   modules = example_module()
# )
# 
# # run app
# shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
# create empty object
data_empty <- teal_data()

# run code in the object
data_populated_1 <- eval_code(data_empty, code = "iris <- iris
                                                  cars <- mtcars")
# alternative
data_populated_2 <- within(data_empty, {
  iris <- iris
  cars <- mtcars
})

## -----------------------------------------------------------------------------
data_with_code <- teal_data(
  iris = iris, cars = mtcars,
  code = "iris <- iris
          cars <- mtcars"
)

## ----eval=FALSE---------------------------------------------------------------
# # not run
# data_from_file <- teal_data()
# data_from_file <- eval_code(data, readLines("<path_to_file>"))

## -----------------------------------------------------------------------------
# create cdisc_data
data_cdisc <- cdisc_data(ADSL = teal.data::rADSL, ADTTE = teal.data::rADSL)

names(data_cdisc)
join_keys(data_cdisc)

## ----eval=FALSE---------------------------------------------------------------
# app <- init(
#   data = data_cdisc,
#   modules = example_module()
# )
# shinyApp(app$ui, app$server)

## ----eval=FALSE---------------------------------------------------------------
# library(MultiAssayExperiment)
# utils::data(miniACC)
# 
# data_mae <- teal_data(MAE = miniACC)
# 
# app <- init(
#   data = data_mae,
#   modules = example_module()
# )
# shinyApp(app$ui, app$server)

## -----------------------------------------------------------------------------
ds1 <- data.frame(
  id = seq(1, 10),
  group = rep(c("A", "B"), each = 5)
)
ds2 <- data.frame(
  group = c("A", "B"),
  condition = c("condition1", "condition2")
)
keys <- join_keys(
  join_key("DS1", keys = "id"),
  join_key("DS2", keys = "group"),
  join_key("DS1", "DS2", keys = c("group" = "group"))
)
data_relational1 <- teal_data(DS1 = ds1, DS2 = ds2, join_keys = keys)
data_relational2 <- teal_data(DS1 = ds1, DS2 = ds2)
join_keys(data_relational2) <- keys

## -----------------------------------------------------------------------------
data_with_code

data_with_objects_and_code <- teal_data(iris = iris, cars = mtcars, code = expression(iris <- iris, cars <- mtcars))
data_with_objects_and_code

data_with_objects_and_code_ver <- verify(data_with_objects_and_code)
data_with_objects_and_code_ver

## -----------------------------------------------------------------------------
my_data <- teal_data()
my_data <- within(my_data, {
  .data1 <- data.frame(id = 1:10, x = 11:20)
  .data2 <- data.frame(id = 1:10, y = 11:20)
  data <- merge(.data1, .data2)
})

ls(my_data)
names(my_data)

app <- init(data = my_data, modules = example_module())

if (interactive()) {
  shinyApp(app$ui, app$server)
}

