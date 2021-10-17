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
  definition = function(x) {
    purrr::map(x@entries, ~DataDefinedVariable(.x@name)) %>%
      purrr::reduce(.init = VariableList(), .f = combine)
  }
)

setMethod(
  f = "rename_variables",
  signature = "InputVariableFacet",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@entries)) {
      if (x@entries[[i]]@name %in% names(variable_mapping)) {
        x@entries[[i]]@name <- variable_mapping[x@entries[[i]]@name]
      }
    }
    names(x@entries) <- names(x)
    return(x)
  }
)

#' Input variables
#'
#' These building block declare input variables, i.e., variables that are defined in the dataset.
#'
#' An input variable is defined in the dataset and is declared so that it can be used in the rest of the model definition. The function
#' `input_variable()` declares a single variable whereas the `dataset()` function reads the header of the file provided and
#' declares all variables found.
#'
#' @param name Variable name
#'
#' @return A building block of type 'input_variable'
#'
#' @export
#' @examples
#' m <- model() +
#'     input_variable("dose") +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50") +
#'     obs_additive(eff~emax*dose/(ed50+dose))
#' render(m)
#' @md
#' @rdname input_variables
input_variable <- function(name){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  InputVariable(name = name)
}

#' @param path Dataset path
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @export
#' @rdname input_variables
dataset <- function(path, use_only_filename = FALSE){
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
  if (!use_only_filename) path <- basename(path)
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
