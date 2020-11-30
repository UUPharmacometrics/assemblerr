#' @include facet.R
#' @include nm_model.R
#' @include model.R
#'
InputVariable <- setClass(
  "InputVariable",
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "InputVariableFacet")
)

InputVariableFacet <- setClass(
  "InputVariableFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "InputVariable")
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "InputVariable"),
  definition = function(target, source, component) {
    target + nm_input(name = component@name)
  }
)

input_variable <- function(name){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  InputVariable(name = name)
}

dataset <- function(path){
  if (grepl(".csv$",path)){
    tab <- read.csv(path, header = TRUE, nrows = 1)
  }else{
    tab <- read.table(path, header = T, nrows = 1)
  }
  purrr::map(tolower(colnames(tab)), input_variable) %>%
    purrr::reduce(`+`) +
    meta_entry("dataset", path)
}
