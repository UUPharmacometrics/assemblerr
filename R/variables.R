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
  pos <- tidyselect::eval_select(expr, data = unclass(var_list))
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
