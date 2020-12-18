#' @include model.R
#' @include pk_model.R

#' @importFrom methods .valueClassTest
setGeneric(name = "render_component",
           def = function(x, ...) standardGeneric("render_component"),
           valueClass = "character")

#' @export
render <- function(model,
                   filename = NULL,
                   target_tool = "nonmem",
                   options = assemblerr_options()) {

  if (target_tool == "nonmem") {
    code <- convert(nm_model(), model, options = options) %>%
      render_component()
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


# setMethod(
#   f = "render",
#   signature = c(x = "Model"),
#   definition = function(x) {
#     convert(nm_model(), x) %>%
#       render()
#   }
# )
#
#
# setMethod(
#   f = "render",
#   signature = c(x = "PkModel"),
#   definition = function(x) {
#     convert(model(), x) %>%
#     convert(nm_model(), .) %>%
#       render()
#   }
# )
