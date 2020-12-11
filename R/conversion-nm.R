

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "missing"),
  definition = function(target, source, component) {
    source <- optimize_for_conversion(source, target)
    target <- convert(target, source, source@facets[["ParameterFacet"]]) %>%
      convert(source, source@facets[["CompartmentFacet"]]) %>%
      convert(source, source@facets[["FlowFacet"]]) %>%
      convert(source, source@facets[["AlgebraicFacet"]]) %>%
      convert(source, source@facets[["ObservationFacet"]]) %>%
      convert(source, source@facets[["InputVariableFacet"]]) %>%
      convert(source, source@facets[["MetaEntryFacet"]])
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
