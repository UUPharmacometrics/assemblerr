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

#' Task estimation
#'
#'
#' This function defines an estimation task allowing to specify the estimation algorithm, estimation options, and whether standard errors should be obtained.
#'
#' @includeRmd man/rmd/tasks.Rmd
#'
#' @details
#' ## Estimation tasks
#'
#' Estimation tasks provide details on the parameter estimation process, in terms of estimation algorithm, estimation options and whether standard errors should be obtained.
#'
#' ### Algorithm
#'
#' The algorithm argument allows to select the estimation algorithm among the following options:
#'
#' |               |                                                                    |
#' | ---------     | ------------------------------------------------------------------ |
#' | foce          | First-order conditional estimation with interaction detection      |
#' | foce-inter    | First-order conditional estimation with interaction                |
#' | foce-no-inter | First-order conditional estimation without interaction             |
#' | fo            | First-order estimation                                             |
#' | imp           | Importance sampling                                                |
#' | saem          | Stochastic approximation expectation maximization                  |
#'
#' The default algorithm `"foce"` detects whether the observation model includes an epsilon-eta interaction and includes the `INTERACTION` option accordingly.
#' The `foce-inter` option forces the use of the `INTERACTION` argument independent of the residual error model, `foce-no-inter` enforces no interaction.
#'
#' Each algorithm includes a set of default options that the package authors consider sensible defaults (for example `MAXEVAL=999999` for FOCE). These defaults can be
#' overwritten using the `target_options=` argument which is described below.
#'
#' ### Standard errors
#'
#' The `se=` argument allows to request the calculation of parameter standard errors. When standard errors are requested (`se=TRUE`) it will results
#' in the inclusion of the `$COVARIANCE` record in the generated control stream.
#'
#' ### Target options
#'
#' The `target_options=` argument provides a mechanism to specify additional estimation options for the selected algorithm. The options should be provided
#' as a list, e.g.,
#'
#' ```
#' tsk_estimation(algorithm = "foce", target_options = list(mceta=100))
#' ```
#'
#' The provided options are passed verbatim to the target tool and not checked by assemblerr for correctness.
#'
#' The `target_options=` argument
#'
#' ## Multiple estimation tasks
#'
#' A sequence of estimation tasks can be specified in assemblerr by combining multiple estimations, for example
#'
#' ```r
#' render(m, tasks = tsk_estimation("foce") + tsk_estimation("imp"))
#' ```
#'
#' will create model code that contains
#'
#' @param algorithm The estimation algorithm to use for the task ("foce", "foce-inter", "foce-no-inter", "fo", "imp", "saem")
#' @param se Whether to calculate parameter uncertainties
#' @param target_options List of additional options that should be passed to
#' NONMEM
#'
#' @family tasks
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
tsk_estimation <- function(algorithm = "foce",
                           se = FALSE,
                           target_options = list()) {
  algorithms <- c("foce", "foce-inter", "foce-no-inter", "fo", "imp", "saem")
  algorithm <- rlang::arg_match(algorithm, algorithm)
  ModelingTasks() +
    EstimationTask(algorithm = algorithm,
                   standard_errors = se,
                   target_options = target_options)
}

#' Task output
#'
#'
#' These functions define output tasks that include the selected variables in the output of the generated model.
#'
#' @includeRmd man/rmd/tasks.Rmd
#'
#' @details
#' ## Output tasks
#'
#' For NONMEM, an output task defines the `$TABLE` records by specifying the `filename=` as well as the `variables=` to include.
#'
#' The variables can be specified by providing a character vector of variable names (e.g., `variables =  c('cl','v')`) or by
#' using a set of variable selection helpers (e.g., `variables = vars_prms()`). The latter is shorter if many variables are to
#' be selected and allows the specification of tasks independent from the model. The details of the variable selection language
#' can be found on the help pages for [model-variable-selection].
#'
#' ## xpose4 output task
#'
#' The `tsk_output_xpose4()` function includes `$TABLE` records that follow the output conventions of the model diagnostic package xpose4.
#' It is a shortcut for the following two output tasks:
#'
#' ```
#'  xpose4_output <- tsk_output("sdtab", variables = any_of(c("id","time")) | vars_nm_std()) +
#'   tsk_output("patab", variables = vars_prms() | vars_eta())
#' ```
#'
#' @param filename The filename for the output file
#' @param variables The model variables that be included in the output
#'
#' @examples
#' # output model parameters to file 'prms'
#' render(m, tasks = tsk_output("prms", variables = vars_prms()))
#' @export
#' @md
tsk_output <- function(filename = "sdtab", variables) {
  expr <- rlang::enquo(variables)
  ModelingTasks() +
    OutputTask(filename = filename, selector = expr)
}

#' @rdname tsk_output
#' @export
#' @examples
#' # output variables required by xpose4
#' render(m, tasks = tsk_output_xpose4())
tsk_output_xpose4 <- function() {
  ModelingTasks() +
    OutputTask(filename = "sdtab", selector = rlang::quo(any_of(c("id","time")) | vars_nm_std())) +
    OutputTask(filename = "patab", selector = rlang::quo(vars_prms() | vars_eta()))
}
