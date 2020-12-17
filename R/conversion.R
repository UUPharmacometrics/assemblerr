#' @include model.R
#' @include pk_model.R
#' @include facet.R


setGeneric(name = "convert",
           def = function(target, source, component, options) standardGeneric("convert"))

setMethod(
  f = "convert",
  signature = c(target = "ANY", source = "GenericModel", component = "missing"),
  definition = function(target, source, component, options) {
    convert(model(), source, options = options) %>%
      convert(target = target, source = ., options = options)
  }
)


setMethod(
  f = "convert",
  signature = c(target = "ANY", source = "ANY", component = "Facet"),
  definition = function(target, source, component, options) {
    purrr::reduce(component@entries, ~convert(target = .x, source = source, component = .y, options = options), .init = target)
  }
)
