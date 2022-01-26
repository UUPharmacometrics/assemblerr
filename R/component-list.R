#' @include component.R
#'
#'  S4 class to represent a list of components
#'
#' This is the base class for a list of components. The list accepts only elements that are of the right class
#' as specified by the component_class slot.
#' @slot component_class A length-one character vector
#' @slot elements A list
ComponentList <- setClass(
  "ComponentList",
  contains = "Component",
  slots = c(
    component_class = "character",
    elements = "list"
  ),
  prototype = prototype(component_class = "Component")
)

setMethod(
  f = "initialize",
  signature = "ComponentList",
  definition = function(.Object, elements = NULL, ...){
    .Object <- callNextMethod(.Object, ...)
    if (length(elements) > 0) {
      assert_compatible_list_elements(elements, .Object)
      .Object@elements <- append(.Object@elements, elements)
    }
    return(.Object)
  }
)

assert_compatible_list_elements <- function(lst, obj) {
  if (!rlang::is_bare_list(lst)) lst <- list(lst)
  for (l in lst) {
    if (!is_compatible(obj, l)) rlang::abort(interp("Entry class {class(l)} not compatible with component list."))
  }
  return(TRUE)
}

setGeneric(
  "component_class",
  def = function(x) {
    standardGeneric("component_class")
  }
)

setMethod(
  "component_class",
  signature = signature(x = "ComponentList"),
  definition = function(x) {
    return(x@component_class)
  }
)


setMethod(
  "is_compatible",
  signature = signature(x = "ComponentList", y = "Component"),
  definition = function(x, y) {
    return(is(y, component_class(x)))
  }
)



setMethod(
  "length",
  signature = signature(x = "ComponentList"),
  definition = function(x) {
    return(length(x@elements))
  }
)

setMethod(
  "append",
  signature = signature(x = "ComponentList"),
  definition = function(x, values, after = length(x)) {
    assert_compatible_list_elements(values, x)
    x@elements <- append(x@elements, values, after)
    return(x)
  }
)


setMethod(
  "[[",
  signature = signature(x = "ComponentList"),
  definition = function(x, i, j, ...) {
    if (!missing(j)) rlang::abort("Incorrect number of subscripts.")
    return(x@elements[[i]])
  }
)

setReplaceMethod("[[", "ComponentList",
  function(x, i, j, ..., value) {
    if (!missing(j) || length(list(...)) > 0) rlang::abort("Incorrect number of subscripts.")
    if (is.null(value)) {
      x@elements[i] <- NULL
    } else {
      assert_compatible_list_elements(value, x)
      x@elements[[i]] <- value
    }
    return(x)
  }
)


#' @describeIn add_component y is added as a list element if has the right class
setMethod(
  "add_component",
  signature = signature(x = "ComponentList", "Component"),
  definition = function(x, y, ...) {
    if (is_compatible(x,y)) {
      x <- append(x, y)
    }
    return(x)
  }
)


#' S4 class to represent a list of named components
#'
#' This is the base class for a list of named components.
NamedComponentList <- setClass(
  "NamedComponentList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NamedComponent")
)


#' Check whether list is empty
#'
#' @noRd
setGeneric(
  "is_empty",
  def = function(x) {
    standardGeneric("is_empty")
  }
)

setMethod(
  "is_empty",
  signature = signature(x = "ComponentList"),
  definition = function(x) {
    return(length(x@elements) == 0)
  }
)

