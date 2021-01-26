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




