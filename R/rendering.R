#' @include model.R
#' @include pk_model.R

#' @importFrom methods .valueClassTest
setGeneric(name = "render_component",
           def = function(x, ...) standardGeneric("render_component"),
           valueClass = "character")

#' Generate model code
#'
#' This function generates NONMEM code for a model object. The generated code will be written to the file specified by 'filename' or printed to the
#' console if the argument is set to NULL.
#'
#' @param model A model object
#' @param filename Name of the model file to create or NULL
#' @param target_tool Name of the target tool (currently only 'nonmem')
#' @param options List of options for model generation
#'
#' @export
render <- function(model,
                   filename = NULL,
                   target_tool = "nonmem",
                   tasks = tsk_estimation(),
                   options = assemblerr_options()) {
  model_arg_label <- rlang::as_label(rlang::enexpr(model))
  if (target_tool == "nonmem") {
    issues <- check(model)
    if ("MissingVariableIssue" %in% issue_types(issues)) {
      if (options$issues.missing_variables %in% c("fix", "fix-warn")) {
        model <- add_missing_variables(
          model = model,
          issues = issues,
          warn = options$issues.missing_variables == "fix-warn"
        )
      }
      if (options$issues.missing_variables %in% c("fix", "fix-warn", "warn", "ignore")) {
        issues <- purrr::discard(issues, ~is(.x, "MissingVariableIssue"))
      }
    }
    if (length(issues) > 0) {
      rlang::abort(
        c("Critical issues found",
          x = interp("The model contains {length(issues)} issue{?s} that need to be fixed before rendering it."),
          i = interp("Get a list of issue by running: `check({model_arg_label})`")
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
