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
  prototype = prototype(entry_class = "InputVariable",
                        label = "input variables")
)

input_variable <- function(name){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  InputVariable(name = name)
}

#' @importFrom utils read.csv
#' @importFrom utils read.table
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
