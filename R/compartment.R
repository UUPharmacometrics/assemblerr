#' @include facet.R
#' @include nm_model.R
#' @include model.R
Compartment <- setClass(
  "Compartment",
  slots = c(volume = "assemblerr_declaration"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "CompartmentFacet")
)

CompartmentFacet <- setClass(
  "CompartmentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "Compartment", label = "compartments")
)


Flow <- setClass(
  "Flow",
  slots = c(
    from = "character",
    to = "character",
    definition = "assemblerr_declaration"
  ),
  contains = "FacetEntry",
  prototype = prototype(facet_class = "FlowFacet")
)

FlowFacet <- setClass("FlowFacet",
                             contains = "Facet",
                             prototype = prototype(entry_class = "Flow", label = "flows"))

setMethod(
  f = "show",
  signature = signature(object = "FlowFacet"),
  definition = function(object){
    flows <- purrr::map_chr(object@entries, ~paste0(.x@from, "-->", .x@to))
    cli::cli_text("{object@label}: {flows}{?none//}")
    invisible(NULL)
  }
)

#' Compartment
#'
#' Defines name and volume of compartment
#'
#' @seealso \code{\link{model}}
#' @param name Name of the compartment
#' @param volume Defintion of the compartment volume as a number, formula or declaration
#'
#' @return A compartment fragment
#' @export
#' @examples
#' # compartment with name "central" and volume Vc
#' comp1 <- compartment("central", volume = declaration(~Vc))
compartment <- function(name, volume = declaration(~1)){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  volume <- as_declaration(volume)
  vec_assert(volume, ptype = declaration(), size = 1)
  Compartment(name = name, volume = volume)
}

#' @export
#' @describeIn compartment Is an simple alias for compartment.
cmp <- compartment

#' Flows between compartments
#'
#' Creates a fragment describing a flow from between compartmens.
#'
#' @param from Name of the source compartment or NULL
#' @param to Name of the sink compartment or NULL
#' @param definition Declaration of the flow using the special variable A (amount in 'from' compartment) and C (concentration in 'from' compartment)
#'
#' @return A flow fragment
#' @export
#' @examples
#' f <- flow(from = "depot", to = "central", definition = declaration(~ka*A))
flow <- function(from = NA_character_, to = NA_character_, definition){
  if (!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  definition <- as_declaration(definition)
  vec_assert(definition, ptype = declaration(), size = 1)
  Flow(from = from, to = to, definition = definition)
}

