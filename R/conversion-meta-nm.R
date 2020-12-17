#' @include meta.R
#' @include model.R
#' @include nm_model.R


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "MetaEntry"),
  definition = function(target, source, component, options) {
    if(component@name == "dataset") {
      target <- target + nm_data(path = component@value)
    }
    target
  }
)
