#' @include facet.R
#' @include nm_model.R
#' @include model.R
#'
MetaEntry <- setClass(
  "MetaEntry",
  slots = c(value = "character"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "MetaEntryFacet")
)

MetaEntryFacet <- setClass(
  "MetaEntryFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "MetaEntry", "meta entries")
)



meta_entry <- function(name, value){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  MetaEntry(name = name, value = value)
}

