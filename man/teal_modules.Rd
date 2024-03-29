% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{teal_modules}
\alias{teal_modules}
\alias{module}
\alias{teal_module}
\alias{modules}
\alias{format.teal_module}
\alias{print.teal_module}
\alias{format.teal_modules}
\alias{print.teal_modules}
\title{Create \code{teal_module} and \code{teal_modules} objects}
\usage{
module(
  label = "module",
  server = function(id, ...) {
     moduleServer(id, function(input, output, session) {
 
       })
 },
  ui = function(id, ...) {
     tags$p(paste0("This module has no UI (id: ", id, " )"))

    },
  filters,
  datanames = "all",
  server_args = NULL,
  ui_args = NULL
)

modules(..., label = "root")

\method{format}{teal_module}(x, indent = 0, ...)

\method{print}{teal_module}(x, ...)

\method{format}{teal_modules}(x, indent = 0, ...)

\method{print}{teal_modules}(x, ...)
}
\arguments{
\item{label}{(\code{character(1)}) Label shown in the navigation item for the module or module group.
For \code{modules()} defaults to \code{"root"}. See \code{Details}.}

\item{server}{(\code{function}) \code{shiny} module with following arguments:
\itemize{
\item \code{id} - \code{teal} will set proper \code{shiny} namespace for this module (see \code{\link[shiny:moduleServer]{shiny::moduleServer()}}).
\item \code{input}, \code{output}, \code{session} - (optional; not recommended) When provided, then \code{\link[shiny:callModule]{shiny::callModule()}}
will be used to call a module. From \code{shiny} 1.5.0, the recommended way is to use
\code{\link[shiny:moduleServer]{shiny::moduleServer()}} instead which doesn't require these arguments.
\item \code{data} (optional) When provided, the module will be called with \code{teal_data} object (i.e. a list of
reactive (filtered) data specified in the \code{filters} argument) as the value of this argument.
\item \code{datasets} (optional) When provided, the module will be called with \code{FilteredData} object as the
value of this argument. (See \code{\link[teal.slice:FilteredData]{teal.slice::FilteredData}}).
\item \code{reporter} (optional) When provided, the module will be called with \code{Reporter} object as the value
of this argument. (See \code{\link[teal.reporter:Reporter]{teal.reporter::Reporter}}).
\item \code{filter_panel_api} (optional) When provided, the module will be called with \code{FilterPanelAPI} object
as the value of this argument. (See \code{\link[teal.slice:FilterPanelAPI]{teal.slice::FilterPanelAPI}}).
\item \code{...} (optional) When provided, \code{server_args} elements will be passed to the module named argument
or to the \code{...}.
}}

\item{ui}{(\code{function}) \code{shiny} UI module function with following arguments:
\itemize{
\item \code{id} - \code{teal} will set proper \code{shiny} namespace for this module.
\item \code{...} (optional) When provided, \code{ui_args} elements will be passed to the module named argument
or to the \code{...}.
}}

\item{filters}{(\code{character}) Deprecated. Use \code{datanames} instead.}

\item{datanames}{(\code{character}) A vector with \code{datanames} that are relevant for the item. The
filter panel will automatically update the shown filters to include only
filters in the listed datasets. \code{NULL} will hide the filter panel,
and the keyword \code{"all"} will show filters of all datasets. \code{datanames} also determines
a subset of datasets which are appended to the \code{data} argument in server function.}

\item{server_args}{(named \code{list}) with additional arguments passed on to the server function.}

\item{ui_args}{(named \code{list}) with additional arguments passed on to the UI function.}

\item{...}{\itemize{
\item For \code{modules()}: (\code{teal_module} or \code{teal_modules}) Objects to wrap into a tab.
\item For \code{format()} and \code{print()}: Arguments passed to other methods.
}}

\item{x}{(\code{teal_module} or \code{teal_modules}) Object to format/print.}

\item{indent}{(\code{integer(1)}) Indention level; each nested element is indented one level more.}
}
\value{
\code{module()} returns an object of class \code{teal_module}.

\code{modules()} returns a \code{teal_modules} object which contains following fields:
\itemize{
\item \code{label}: taken from the \code{label} argument.
\item \code{children}: a list containing objects passed in \code{...}. List elements are named after
their \code{label} attribute converted to a valid \code{shiny} id.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Create a nested tab structure to embed modules in a \code{teal} application.
}
\details{
\code{module()} creates an instance of a \code{teal_module} that can be placed in a \code{teal} application.
\code{modules()} shapes the structure of a the application by organizing \code{teal_module} within the navigation panel.
It wraps \code{teal_module} and \code{teal_modules} objects in a \code{teal_modules} object,
which results in a nested structure corresponding to the nested tabs in the final application.

Note that for \code{modules()} \code{label} comes after \code{...}, so it must be passed as a named argument,
otherwise it will be captured by \code{...}.

The labels \code{"global_filters"} and \code{"Report previewer"} are reserved
because they are used by the \code{mapping} argument of \code{\link[=teal_slices]{teal_slices()}}
and the report previewer module \code{\link[=reporter_previewer_module]{reporter_previewer_module()}}, respectively.
}
\examples{
library(shiny)

module_1 <- module(
  label = "a module",
  server = function(id, data) {
    moduleServer(
      id,
      module = function(input, output, session) {
        output$data <- renderDataTable(data()[["iris"]])
      }
    )
  },
  ui = function(id) {
    ns <- NS(id)
    tagList(dataTableOutput(ns("data")))
  },
  datanames = "all"
)

module_2 <- module(
  label = "another module",
  server = function(id) {
    moduleServer(
      id,
      module = function(input, output, session) {
        output$text <- renderText("Another Module")
      }
    )
  },
  ui = function(id) {
    ns <- NS(id)
    tagList(textOutput(ns("text")))
  },
  datanames = NULL
)

modules <- modules(
  label = "modules",
  modules(
    label = "nested modules",
    module_1
  ),
  module_2
)

app <- init(
  data = teal_data(iris = iris),
  modules = modules
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
}
