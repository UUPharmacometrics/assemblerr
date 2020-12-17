#' @include facet.R
#' @include nm_model.R
#' @include model.R
PkModel <- setClass("PkModel", contains = "GenericModel")

setMethod(
  f = "initialize",
  signature = "PkModel",
  definition = function(.Object, ...) {
    callNextMethod(.Object,
                   facets = list(ParameterFacet(),
                                 AlgebraicFacet(),
                                 PkComponentFacet(),
                                 ObservationFacet(),
                                 InputVariableFacet(),
                                 MetaEntryFacet()),
                   ...)
  }
)



#' Create a PK model
#'
#' @return A pk_model
#' @export
pk_model <- function(){
  PkModel()
}


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "missing"),
  definition = function(target, source, component, options) {
    target <- convert(target, source, source@facets[["PkComponentFacet"]], options = options)
    purrr::discard(source@facets, ~inherits(.x, "PkComponentFacet")) %>%
      purrr::reduce(combine, .init = target)
  }
)



