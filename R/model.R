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
  algebraic_dcls <- purrr::map(model@facets[["AlgebraicFacet"]]@entries, ~.x@definition)
  if (!vec_is_empty(algebraic_dcls)) {
    dcl <- vec_c(!!!unname(algebraic_dcls))
    vars <- dcl_external_variables(dcl) %>% as.character()
    prms <- names(model@facets[["ParameterFacet"]]@entries)
    external_vars <- names(model@facets[["InputVariableFacet"]]@entries)
    missing_vars <- setdiff(vars, union(prms, external_vars))
    if (!vec_is_empty(missing_vars)) {
       issue <- CriticalIssue(cli::pluralize("Undefined variable{?s} {missing_vars} in algebraics"))
       return(issue)
    }
  }
  return(NULL)
}


