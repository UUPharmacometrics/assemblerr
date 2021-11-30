#' @include node-classes.R



Variable <- setClass(
  Class = "Variable",
  contains = "NamedNode"
)

setMethod(
  f = "initialize",
  signature = "Variable",
  definition = function(.Object, name, ...) {
    callNextMethod(.Object, name = name,  ...)  }
)


VariableList <- setClass(
  "VariableList",
  contains = "NamedNodeList",
  prototype = prototype(node_class = "Variable")
)

select_variables <- function(var_list, ...) {
  expr <- rlang::expr(c(...))
  rlang::with_handlers(
    pos <- tidyselect::eval_select(expr, data = unclass(var_list)),
    vctrs_error_subscript_oob = function(e) {
      rlang::abort(
        c("Can't select variables that don't exist",
        x = interp("The variable `{e$i}` does not exist")
        )
      )
    }

  )
  VariableList(!!!var_list[pos])
}

# variable defined in the data
DataDefinedVariable <- setClass(
  Class = "DataDefinedVariable",
  contains = "Variable"
)

# variable defined as a parameter
ParameterVariable <- setClass(
  Class = "ParameterVariable",
  contains = "Variable"
)

# variable defined as a compartment
CompartmentAmountVariable <- setClass(
  Class = "CompartmentAmountVariable",
  contains = "Variable"
)

# variable defined via an algebraic relationship
AlgebraicVariable <- setClass(
  Class = "AlgebraicVariable",
  contains = "Variable"
)

# variable defined in an observation model
ObservationVariable <- setClass(
  Class = "ObservationVariable",
  contains = "Variable"
)

# variable that is predefined in a particular target
PredefinedVariable <- setClass(
  Class = "PredefinedVariable",
  contains = "Variable"
)

# random effect variable
RandomEffectVariable <- setClass(
  Class = "RandomEffectVariable",
  contains = "PredefinedVariable"
)

PredictionVariable <- setClass(
  Class = "PredictionVariable",
  contains = "PredefinedVariable"
)

ResidualVariable <- setClass(
  Class = "ResidualVariable",
  contains = "PredefinedVariable"
)

WeightedResidualVariable <- setClass(
  Class = "WeightedResidualVariable",
  contains = "PredefinedVariable"
)

# construction helper -----------------------------------------------------


create_variable_list <- function(variable_names, variable_types) {
  vec_assert(variable_names, character())
  vec_assert(variable_types, character())
  variable_types <- vec_recycle(variable_types, vec_size(variable_names))
  vars <- purrr::map2(variable_names, variable_types, ~rlang::exec(.y, .x))
  VariableList(!!!vars)
}


# selection helpers -------------------------------------------------------

#' Selecting model variables
#'
#' @description
#' The output task allows to select model variables using
#' a concise mini language. You can select variables by
#' name or using one of the helper functions described below.
#'
#' ## Overview of selection features
#'
#' The selection of variables builds on the tidyselect package
#' which implements a powerful variable selection language (see [tidyselect::language]).
#' The following features are most relevant for the
#' selection of model variables:
#'
#' * `|` for selecting the union of several variables
#' * `c()`  for combining selections
#' * `!` for taking the complement of a set of variables
#'
#' In addition, you can select variables using a combination of the following helper functions:
#'
#' * `vars_prms()` selects all model parameters
#' * `vars_data()` selects all data defined variables
#' * `vars_eta()` selects all eta variables
#' * `vars_nm_std()` selects the standard NONMEM variables DV, PRED, RES, WRES, IPREDI, IWRESI
#' * `vars_starts_with()` selects variables that start with a prefix
#' * `vars_matches()` selects variables that match a regular expression
#'
#' @param vars A character vector of variable names (taken from the selection context)
#'
#' @return A selection context
#'
#'
#' @examples
#'
#' m <- model() +
#'   input_variable("dose") +
#'   prm_log_normal("emax", median = 10, var_log = 0.09) +
#'   prm_log_normal("ed50", median = 50, var_log = 0.09) +
#'   algebraic(effect~emax*dose/(ed50 + dose)) +
#'   obs_proportional(~effect, var_prop = 1)
#'
#' # output all model parameter and eta variables
#' render(m, tasks = tsk_output("prms", variables = vars_prms() | vars_eta()))
#' @export
#' @md
#' @name model-variable-selection
vars_prms <- function(vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_prms")
  sel <- purrr::map_lgl(vars, ~is(.x, "ParameterVariable"))
  which(sel)
}

#' @export
#' @rdname model-variable-selection
vars_data <- function(vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_data")
  sel <- purrr::map_lgl(vars, ~is(.x, "DataDefinedVariable"))
  which(sel)
}

#' @export
#' @rdname model-variable-selection
vars_eta <- function(vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_eta")
  sel_type <- purrr::map_lgl(vars, ~is(.x, "RandomEffectVariable"))
  which(sel_type)
}

#' @export
#' @rdname model-variable-selection
vars_nm_std <- function(vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_nm_std")
  tidyselect::all_of(c("DV", "PRED", "RES", "WRES", "IPREDI", "IWRESI", "CWRES"))
}

#' @param match A character vector to match against
#' @export
#' @rdname model-variable-selection
vars_starts_with <- function(match, vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_starts_with")
  tidyselect::starts_with(match)
}

#' @export
#' @rdname model-variable-selection
vars_matches <- function(match, vars) {
  if (rlang::is_missing(vars)) vars <- tidyselect::peek_data(fn = "vars_matches")
  tidyselect::matches(match)
}
