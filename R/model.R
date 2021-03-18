#' @include facet.R
#' @include conversion.R
Model <- setClass("Model", contains = "GenericModel")

setMethod(
  f = "initialize",
  signature = "Model",
  definition = function(.Object, ...) {
    callNextMethod(.Object,
                   facets = list(ParameterFacet(),
                                 AlgebraicFacet(),
                                 CompartmentFacet(),
                                 FlowFacet(),
                                 ObservationFacet(),
                                 InputVariableFacet(),
                                 MetadataFacet()),
                   ...)
  }
)

setMethod(
  f = "show",
  signature = "Model",
  definition = function(object) {
    print_shortened_tree_description(
      tree_description =  compact_description(object),
      type = "model",
      child_type = "facet{?s}",
      show = c("ParameterFacet", "AlgebraicFacet", "CompartmentFacet", "FlowFacet", "ObservationFacet")
    )
    print_issues_warning(check(object))
  }
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "Model", component = "missing"),
  definition = function(target, source, component, options) {
    source
  }
)

#' General model
#'
#' This building block creates the basis for a general pharmacometric model, a flexible but verbose model type.
#'
#' The object created is just an empty structure to which different building blocks need to be added. In a general model, each building block
#' can modify one or more of the following facets
#'   * [Parameters][parameter-model]
#'   * [Observations][observation-model]
#'   * [Algebraic relationships][algebraic()]
#'   * [Compartments][compartment()]
#'   * [Flows][flow()]
#'   * [Input variables][input_variables()]
#'   * Meta data
#'
#' The more specialized [pk_model()] is converted to a general model during the rendering process.
#'
#' @return A general pharmacometric model
#' @export
#' @importFrom magrittr %>%
#' @examples
#' m <- model() +
#'     input_variable("dose") +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50") +
#'     obs_additive(eff~emax*dose/(ed50+dose))
#' render(m)
#' @md
model <- function(){
  Model()
}

setOldClass("model")

setMethod(
  f = "check",
  signature = signature(x = "Model"),
  definition = function(x, ...) {
    issues <- c(IssueList(),
                callNextMethod(x))
    return(issues)
  }
)



check_for_undefined_variables <- function(dcls, defined_vars, facet_label) {
  required_vars <- dcl_external_variables(dcls) %>%
    as.character()
  missing_vars <- setdiff(required_vars, defined_vars)
  if (!vec_is_empty(missing_vars)) {
    return(
      MissingVariableIssue(interp("Undefined {qty(length(missing_vars))}variable{?s} {sq(missing_vars)} in {facet_label}"), variables = missing_vars)
    )
  } else{
    return(NULL)
  }
}

collect_defined_variables <- function(model, facets, additional_variables = character(0)) {
  existing_facets <- facets[facets %in% names(model@facets)]
  purrr::map(model@facets[existing_facets], defined_variables) %>%
    purrr::flatten_chr() %>%
    c(additional_variables)
}
