#' @include facet.R
#' @include nm_model.R
#' @include model.R
#'
Metadata <- setClass(
  "Metadata",
  slots = c(value = "character"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "MetadataFacet")
)

MetadataFacet <- setClass(
  "MetadataFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "Metadata")
)



metadata <- function(name, value){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  Metadata(name = name, value = value)
}

