#' @include input_variable.R
#' @include nm_model.R
#' @include model.R

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "InputVariable"),
  definition = function(target, source, component, options) {
    target + nm_input(name = component@name)
  }
)
