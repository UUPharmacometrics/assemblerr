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

setMethod(
  f = "defined_variables",
  signature = "InputVariableFacet",
  definition = function(x) names(x@entries)
)

#' @export
input_variable <- function(name){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  InputVariable(name = name)
}

#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @export
dataset <- function(path){
  if (grepl(".csv$",path)) {
    tab <- read.csv(path, header = TRUE, nrows = 1, check.names = FALSE)
  }else{
    tab <- read.table(path, header = TRUE, nrows = 1, check.names = FALSE)
  }
  col_names <- colnames(tab)
  if (any(duplicated(col_names))) {
    duplicated_names <- col_names[duplicated(col_names)] %>%
      unique()
    rlang::warn(
      c("Duplicated column names",
        i = cli::pluralize("The column name{?s} {duplicated_names} appeared multiple times and {?was/were} adjusted.")
      )
    )
    col_names <- make.unique(col_names, "_")
  }
  purrr::map(tolower(col_names), input_variable) %>%
    purrr::reduce(`+`) +
    metadata("dataset", path)
}


add_missing_variables <- function(model, issues, warn = TRUE) {
  variables <- purrr::keep(issues, ~is(.x, "MissingVariableIssue")) %>%
    purrr::map(~.x@variables) %>%
    purrr::flatten_chr()
  if (warn) {
    rlang::warn(
      c(
        "Undefined variables added",
        x = interp("The variable{?s} {sq(variables)} {?was/were} not defined and {?has/have} been added as input variable{?s}.")
      )
    )
  }
  variables %>%
    purrr::map(input_variable) %>%
    purrr::reduce(`+`, .init = model)
}
