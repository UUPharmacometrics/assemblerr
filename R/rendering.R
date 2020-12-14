#' @include model.R
#' @include pk_model.R

#' @export
setGeneric(name = "render",
           def = function(x, ...) standardGeneric("render"),
           valueClass = "character")

setMethod(
  f = "render",
  signature = c(x = "Model"),
  definition = function(x) {
    convert(nm_model(), x) %>%
      render()
  }
)


setMethod(
  f = "render",
  signature = c(x = "PkModel"),
  definition = function(x) {
    convert(model(), x) %>%
    convert(nm_model(), .) %>%
      render()
  }
)
