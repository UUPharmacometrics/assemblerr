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
                                 MetadataFacet()),
                   ...)
  }
)

setMethod(
  f = "show",
  signature = "PkModel",
  definition = function(object) {
    print_shortened_tree_description(
      tree_description = compact_description(object),
      type = "pk_model",
      child_type = "facet{?s}",
      show = c("PkComponentFacet","ParameterFacet", "AlgebraicFacet", "ObservationFacet")
    )
    print_issues_warning(check_component(object))
  }
)


#' Create a PK model
#'
#' This function creates the basis for a pharmacokinetic model.
#'
#' The function creates the fundament for a pharmacokinetic model to which different building blocks can be added.
#' The following building blocks are relevant for this model type:
#'
#'   * Parameters: `r md_doc_links_for_package_functions("^prm_")`
#'   * Observations: `r md_doc_links_for_package_functions("^obs_")`
#'   * Algebraic relationships: [algebraic]
#'   * PK components: `r md_doc_links_for_package_functions("^pk_")`
#'   * Input variables: [input_variable], [dataset]
#'
#' @return A pk_model
#' @md
#' @export
pk_model <- function(){
  PkModel()
}


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "missing"),
  definition = function(target, source, component, options) {
    # ensure to process in alphabetical order (i.e., absorption, distribution, elimination)
    cmp_order <- order(names(source@facets[["PkComponentFacet"]]@entries))
    source@facets[["PkComponentFacet"]]@entries <- source@facets[["PkComponentFacet"]]@entries[cmp_order]
    target <- convert(target, source, source@facets[["PkComponentFacet"]], options = options)
    purrr::discard(source@facets, ~inherits(.x, "PkComponentFacet")) %>%
      purrr::reduce(combine, .init = target)
  }
)



