#' @include model.R
#' @include pk_model.R

#' @importFrom methods .valueClassTest
setGeneric(name = "render_component",
           def = function(x, ...) standardGeneric("render_component"),
           valueClass = "character")

#' Generate model code
#'
#' This function generates the code for a model object, prints it to the console or writes it to a file.
#'
#'
#' The generated code will be written to the file specified by `filename=` or printed to the console if the filename is
#' set to `NULL`. Only `'nonmem'` is currently supported as a `target_tool=` option. The `tasks=` argument allows the
#' specification of model tasks and the `options=` argument customizes the generated code.
#'
#' ## Task specification
#'
#' Tasks are building blocks that allow to specify what a model should "do". Like other model building blocks, they can be combined using the `+` operator.
#' For example, the following adds an estimation task and an xpose4 output task to the generated code:
#'
#' ```
#'    render(m, tasks = tsk_estimation() +
#'         tsk_output_xpose4())
#'  ```
#'
#' The default argument (`tasks=tsk_estimation()`) adds an FOCE estimation task to the code.
#'
#' ## Rendering options
#'
#' The `options=` argument allows to modify the rendering process and, hence,  the generated code. Options are provided
#' as a list and the `assemblerr_options()` function helps to generate list with the proper formatting.
#'
#' The following code block renders the model `m` with automatic mu-referencing for the model parameters
#'
#' ```
#'    render(m, options = assemblerr_options(prm.use_mu_referencing = TRUE))
#' ```
#'
#' @param model A model object
#' @param filename Name of the model file to create or NULL
#' @param target_tool Name of the target tool (currently only 'nonmem')
#' @param tasks A task specification
#' @param options List of options for model generation
#'
#' @examples
#' m <- model() +
#'     input_variable("dose") +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50") +
#'     obs_additive(eff~emax*dose/(ed50+dose))
#' # render to console
#' render(m)
#'
#' # render to file
#' \dontrun{
#' render(m, "run1.mod")
#' }
#'
#' # render to console with estimation & output task
#' render(m, tasks = tsk_estimation() + tsk_output_xpose4())
#'
#' @export
#' @md
render <- function(model,
                   filename = NULL,
                   target_tool = "nonmem",
                   tasks = tsk_estimation(),
                   options = assemblerr_options()) {
  model_arg_label <- rlang::as_label(rlang::enexpr(model))
  if (target_tool == "nonmem") {
    issues <- check_component(model)
    if ("MissingVariableIssue" %in% issue_types(issues)) {
      if (options$issues.missing_variables %in% c("fix", "fix-warn")) {
        model <- add_missing_variables(
          model = model,
          issues = issues,
          warn = options$issues.missing_variables == "fix-warn"
        )
      }
      if (options$issues.missing_variables %in% c("fix", "fix-warn", "warn", "ignore")) {
        issues <- discard_issues(issues, "MissingVariableIssue")
      }
    }
    if ("ChangedVariableCapitalizationIssue" %in% issue_types(issues)) {
      variable_mapping <- issues %>%
        purrr::keep(~is(.x, "ChangedVariableCapitalizationIssue")) %>%
        purrr::map(purrr::attr_getter("variables")) %>%
        purrr::flatten_chr() %>%
        generate_unique_name_mapping()
      model <- rename_variables(model, variable_mapping)
      variable_mapping_ui <- paste(names(variable_mapping), variable_mapping, sep = "->")
      rlang::warn(
        c(
          "Variables renamed",
          x = interp("Variables with differing capitalization are not supported by NONMEM and were renamed ({variable_mapping_ui}).")
        )
      )
      issues <- discard_issues(issues, "ChangedVariableCapitalizationIssue")
    }
    if (length(issues) > 0) {
      rlang::abort(
        c("Critical issues found",
          x = interp("The model contains {length(issues)} issue{?s} that need to be fixed before rendering it."),
          i = interp("Get a list of issue by running: `check_component({model_arg_label})`")
        )
      )
    }
    code <- convert(nm_model(), model, options = options) %>%
      convert(source = model, component = tasks, options = options) %>%
      render_component() %>%
      glue::glue_collapse("\n")
  } else {
    cli::cli_alert_danger("Tool '{target_tool}' is currently not supported.")
    return()
  }
  if (is.null(filename)) {
    return(code)
  }else{
    res <- try(silent = TRUE,
               cat(code, sep = "\n", file = filename)
    )
    if (inherits(res, "try-error")) {
      cli::cli_alert_danger("Could not write model file '{filename}'.")
    }else{
      cli::cli_alert_success("Model file '{filename}' successfully written.")
      return(invisible(code))
    }
  }
}
