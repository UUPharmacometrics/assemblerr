#' @include component.R
#' @include component-list.R
#'
#'  S4 class to represent a list of named components
#'
#' This is the base class for a list of named components. The list accepts only elements that are of the right class
#' as specified by the component_class slot.
#' @slot component_class A length-one character vector
#' @slot elements A list
NamedComponentList <- setClass(
  "NamedComponentList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NamedComponent")
)

setMethod(
  f = "initialize",
  signature = "NamedComponentList",
  definition = function(.Object, elements = NULL, ...){
    .Object <- callNextMethod(.Object, ...)
    if (length(elements) > 0 ) {
      assert_compatible_list_elements(elements, .Object)
      for (e in elements) {
        .Object@elements[[name(e)]] <- e
      }

    }
    return(.Object)
  }
)

setMethod(
  "index_of",
  signature = signature(facet = "NamedComponentList"),
  definition = function(facet, x) {
    which(names(facet@elements) == x)
  }
)

setMethod(
  "append",
  signature = signature(x = "NamedComponentList"),
  definition = function(x, values, after = length(x)) {
    assert_compatible_list_elements(values, x)
    if (!rlang::is_bare_list(values)) values <- list(values)
    names(values) <- vapply(values, name, character(1))
    x@elements <- append(x@elements, values, after)
    return(x)
  }
)

setMethod(
  "names",
  signature = signature(x = "NamedComponentList"),
  definition = function(x) return(names(x@elements))
)


setReplaceMethod("[[", "NamedComponentList",
                 function(x, i, j, ..., value) {
                   if (!missing(j) || length(list(...)) > 0) rlang::abort("Incorrect number of subscripts.")
                   if (is.null(value)) {
                     x@elements[i] <- NULL
                   } else {
                     assert_compatible_list_elements(value, x)
                     x@elements[[name(value)]] <- value
                   }
                   return(x)
                 }
)
