#' @include facet.R
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
  definition = function(target, source, component) {
    source
  }
)


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
#' \itemize{
#'    \item \code{\link{parameter}}
#'    \item \code{\link{algebraic}}
#'    \item \code{\link{compartment}}
#'    \item \code{\link{flow}}
#'    \item \code{\link{observation}}
#'    \item \code{\link{meta_tag}}
#' }
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

#' #' @export
#' print.model <- function(x,...){
#'   name <- NA
#'   title <- get_first(x, "meta_tags", name == "title")$value
#'   if(!is.null(title)){
#'     cat('assemblerr model "', title,'":\n')
#'   } else{
#'     cat('assemblerr model:\n')
#'   }
#'   prms <- x$parameters$name %>% paste(collapse = ", ")
#'   cat('  - Parameters: ', prms, '\n')
#'   obs_txt <- x$observations %>%
#'     dplyr::mutate(txt = paste0("    + \"", .data$name, "\" (", .data$type, ")")) %>%
#'     purrr::pluck("txt") %>%
#'     paste(collapse = "\n")
#'   cat('  - Observations: \n')
#'   cat(obs_txt, "\n")
#'   comp_txt <- x$compartments$name %>% paste(collapse = ", ")
#'   if(nrow(x$compartments)>0) cat('  - Compartments: ', comp_txt, "\n")
#'   alg_count <- x$algebraics %>% nrow()
#'   if(alg_count>0) cat('  - Algebraic relationships: ', alg_count, "\n")
#' }



#' Create a meta tag facet
#'
#' @param name Tag name
#' @param value Tag value
#'
#' @return A meta tag facet
#' @export
meta_tag <- function(name, value){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  fragment(meta_tags = list(name = name, value = value))
}

convert_compartments <- function(to, from) UseMethod("convert_compartments")

convert_observations <- function(to, from) UseMethod("convert_observations")

convert_parameters <- function(to, from) UseMethod("convert_parameters")

convert_algebraics <- function(to, from) UseMethod("convert_algebraics")

convert_meta_tags <- function(to, from) UseMethod("convert_meta_tags")

