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

setOldClass("quosure")
OutputTask <- setClass(
  "OutputTask",
  contains = "Task",
  slots = c(
    filename = "character",
    selector = "quosure"
  )
)

#' Model tasks
#'
#' These functions define model tasks that can be combined using `+` and then be included during rendering.
#'  * `tsk_estimation()` adds a parameter estimation task allowing to specify
#'  the estimation algorithm to be used and whether a $COV step should be run.
#'  * `tsk_ouput()` adds an output task allowing to display model variables
#'  in a table.
#'  * `tsk_ouput_xpose()` adds several tables as output following the xpose4 output
#'  standard.
#'
#'
#' @param algorithm The estimation algorithm to use for the task
#' @param se Whether to calculate parameter uncertainties
#' @param target_options List of additional options that should be passed to
#' NONMEM
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
#' # add estimation task using importance sampling, covariance step
#' # and user-defined ISAMPLE option
#' render(
#'   model = m,
#'   tasks = tsk_estimation(
#'     algorithm = "imp",
#'     se = TRUE,
#'     target_options = list(isample=1000)
#'   )
#' )
#'
#' @export
#' @md
tsk_estimation <- function(algorithm = c("foce", "foce-inter", "foce-no-inter", "fo", "imp", "saem"),
                           se = FALSE,
                           target_options = list()) {
  algorithm <- rlang::arg_match(algorithm)
  ModelingTasks() +
    EstimationTask(algorithm = algorithm,
                   standard_errors = se,
                   target_options = target_options)
}

#' @param filename The filename for the output file
#' @param variables The model variables that should be output
#'
#' @examples
#' # output model parameters to file 'prms'
#' render(m, tasks = tsk_output("prms", variables = vars_prms()))
#' @rdname tsk_estimation
#' @export
tsk_output <- function(filename = "sdtab", variables) {
  expr <- rlang::enquo(variables)
  ModelingTasks() +
    OutputTask(filename = filename, selector = expr)
}

#' @rdname tsk_estimation
#' @export
tsk_output_xpose4 <- function() {
  ModelingTasks() +
    OutputTask(filename = "sdtab", selector = rlang::quo(vars_nm_std())) +
    OutputTask(filename = "patab", selector = rlang::quo(vars_prms() | vars_eta()))
}
