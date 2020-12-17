

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "missing"),
  definition = function(target, source, component, options) {
    source <- optimize_for_conversion(source, target, options = options)
    target <- convert(target, source, source@facets[["ParameterFacet"]], options) %>%
      convert(source, source@facets[["CompartmentFacet"]], options) %>%
      convert(source, source@facets[["FlowFacet"]], options) %>%
      convert(source, source@facets[["AlgebraicFacet"]], options) %>%
      convert(source, source@facets[["ObservationFacet"]], options) %>%
      convert(source, source@facets[["InputVariableFacet"]], options) %>%
      convert(source, source@facets[["MetaEntryFacet"]], options)
    if (vec_is_empty(source@facets[["InputVariableFacet"]]@entries)) {
      target  <-    target +
        nm_input("id", "id") +
        nm_input("time", "time") +
        nm_input("dv", "dv") +
        nm_input("amt", "amt")
    }
    target

  }
)
