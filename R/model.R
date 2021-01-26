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
                                 MetaEntryFacet()),
                   ...)
  }
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "Model", component = "missing"),
  definition = function(target, source, component, options) {
    source
  }
)


# setMethod(
#   f = "optimize_for_conversion",
#   signature = signature(source = "Model", target = "NmModel"),
#   definition = function(source, target, ...) {
#     return(source)
#   }
# )


#' General model
#'
#' \code{model()} creates the foundation for a general pharmacometric model
#'
#' This function creates a model object, the foundation for a general, software-agnostic description of a pharmacometric model.
#' The object created is an empty structure. In general, one will want to add components to the model,
#' then convert it to a software-specific model object and finally create the model code. The following components can be added
#' to a general model:
#'
#' @return A general pharmacometric model
#' @export
#' @importFrom magrittr %>%
#' @examples
#' m <- model()+
#'     obs_additive(eff~emax*dose/(ed50+dose)) +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50")
model <- function(){
  Model()
}

setOldClass("model")

setMethod(
  f = "check",
  signature = signature(x = "Model"),
  definition = function(x) {
    issues <- c(IssueList(),
                callNextMethod(x),
                check_for_undefined_variables(model = x))
    return(issues)
  }
)

check_for_undefined_variables <- function(model){
  issues <- IssueList()
  external_vars <- names(model@facets[["InputVariableFacet"]]@entries)
  prms <- names(model@facets[["ParameterFacet"]]@entries)
  algebraic_vars <- names(model@facets[["AlgebraicFacet"]]@entries)
  conc_vars <- paste0('C["',names(model@facets[["CompartmentFacet"]]@entries), '"]')
  a_vars <- paste0('A["',names(model@facets[["CompartmentFacet"]]@entries), '"]')
  if (!vec_is_empty(algebraic_vars)) {
    algebraic_dcls <- purrr::map(model@facets[["AlgebraicFacet"]]@entries, ~.x@definition)
    dcl <- vec_c(!!!unname(algebraic_dcls))
    vars <- dcl_external_variables(dcl) %>%
      as.character()
    missing_vars <- setdiff(vars, union(prms, external_vars))
    if (!vec_is_empty(missing_vars)) {
      missing_vars <- paste0("'", missing_vars ,"'")
      issues <- c(issues,
                   CriticalIssue(cli::pluralize("Undefined variable{?s} {missing_vars} in algebraics"))
                  )
    }
  }
  if (!vec_is_empty(names(model@facets[["ObservationFacet"]]@entries))) {
    obs_dcls <- purrr::map(model@facets[["ObservationFacet"]]@entries, ~.x@prediction)
    dcl <- vec_c(!!!unname(obs_dcls))
    vars <- dcl_external_variables(dcl) %>%
      as.character()
    missing_vars <- setdiff(vars, c(prms, external_vars, algebraic_vars, conc_vars, a_vars))
    if (!vec_is_empty(missing_vars)) {
      missing_vars <- paste0("'", missing_vars ,"'")
      issues <- c(issues,
                  CriticalIssue(cli::pluralize("Undefined variable{?s} {missing_vars} in observation model"))
                )
    }
  }
  return(issues)
}


