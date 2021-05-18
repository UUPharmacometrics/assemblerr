#' @include node-classes.R

Task <- setClass("Task",
                 contains = "Node")

setMethod(
  f = "description",
  signature = "Task",
  definition = function(x) {
    class(x) %>%
      split_camelcase() %>%
      tolower() %>%
      gsub("task$", "", x = .)
  }
)

ModelingTasks <- setClass(
  "ModelingTasks",
  contains = "NodeList",
  prototype = prototype(node_class = "Task")
)

EstimationTask <- setClass(
  "EstimationTask",
  contains = "Task",
  slots = c(algorithm = "character", standard_errors = "logical", target_options = "list")
)

setMethod(
  f = "description",
  signature = "EstimationTask",
  definition = function(x) {
    interp("estimation: {x@algorithm}")
  }
)

CovarianceTask <- setClass(
  "CovarianceTask",
  contains = "Task",
  slots = c(matrix = "character")
)


ModelElementSelector <- setClass(
  "ModelElementSelector",
  contains = "Node",
  slots = c(selector = "character")
)

ParametersSelector <- setClass(
  "ParametersSelector",
  contains = "ModelElementSelector"
)

InputVariablesSelector <- setClass(
  "InputVariablesSelector",
  contains = "ModelElementSelector"
)

ModelElementSelectorList <- setClass(
  "ModelElementSelectorList",
  contains = "NodeList",
  prototype = prototype(node_class = "ModelElementSelector")
)

OutputTask <- setClass(
  "OutputTask",
  contains = "Task",
  slots = c(filename = "character", items = "ModelElementSelectorList")
)


tsk_estimation <- function(algorithm = c("foce", "foce-inter", "foce-no-inter", "fo", "imp", "saem"),
                           se = FALSE,
                           target_options = list()) {
  algorithm <- rlang::arg_match(algorithm)
  ModelingTasks() +
    EstimationTask(algorithm = algorithm,
                   standard_errors = se,
                   target_options = target_options)
}

tsk_output <- function(filename = "sdtab", items = select_prms()) {
  ModelingTasks() +
    OutputTask(filename = filename, items = items)
}

select_prms <- function() {
  ModelElementSelectorList() +
    ParametersSelector()
}

select_input_vars <- function() {
  ModelElementSelectorList() +
    InputVariablesSelector()
}
