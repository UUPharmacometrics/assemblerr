#' @importFrom methods new
#' @importFrom methods show
#' @importFrom methods callNextMethod
#' @importFrom methods getSlots
NULL

# TODO: Need to implement fragment as a node list accepting everything

#' S4 class to represent a component
#'
#' This is the base class for all building blocks in assemblerr and allows the combination
#' to more complex constructs.
#' @noRd
Component <- setClass("Component")

#' S4 class to represent a named component
#'
#' This is the base class for components with a name
#' @slot name A length-one character vector
NamedComponent <- setClass(
  "NamedComponent",
  contains = "Component",
  slots = c(name = "character")
)




#' Check whether component x and y are compatible
#'
#' @noRd
setGeneric(
  "is_compatible",
  def = function(x, y) {
    standardGeneric("is_compatible")
  }
)

setMethod(
  "is_compatible",
  signature = signature(x = "Component", y = "ANY"),
  definition = function(x, y) {
    return(FALSE)
  }
)


setMethod(
  "is_compatible",
  signature = signature(x = "Component", y = "Component"),
  definition = function(x, y) {
      return(is(y, class(x)))
  }
)



#' Check whether component y can be added to slot s in x
#'
#' @noRd
setGeneric(
  "is_compatible_slot",
  def = function(x, s, y) {
    standardGeneric("is_compatible_slot")
  }
)

setMethod(
  "is_compatible_slot",
  signature = signature(x = "Component", y = "Component"),
  definition = function(x, s, y) {
    return(is_compatible(slot(x, s), y))
  }
)

#' Add y to slot s in x
#'
#' @noRd
setGeneric(
  "add_to_slot",
  def = function(x, s, y) {
    standardGeneric("add_to_slot")
  }
)

setMethod(
  "add_to_slot",
  signature = signature(x = "Component", y = "Component"),
  definition = function(x, s, y) {
    if (is(slot(x, s), "ComponentList")) {
      slot(x, s) <- append(slot(x, s), y)
    } else {
      slot(x, s) <- y
    }
    return(x)
  }
)


#' Add a component to another one
#'
#' The function adds component y to x if x has a compatible slot
#' @noRd
setGeneric(
  "add_component",
  def = function(x, y, ...) {
    standardGeneric("add_component")
  }
)

#' @describeIn add_component y is added to the first slot with the right class type
setMethod(
  "add_component",
  signature = signature(x = "Component", "Component"),
  definition = function(x, y, ...) {
    slots <- getSlots(class(x))
    for (s in names(slots)) {
      if (is_compatible_slot(x, s, y)) {
        x <- add_to_slot(x, s, y)
        break
      }
    }
    return(x)
  }
)

#' Add components
#' @param e1 A component
#' @param e2 A component
#' @keywords internal
setMethod(
  f = "+",
  signature = signature(e1 = "Component", e2 = "Component"),
  definition =  function(e1, e2) {
    add_component(e1, e2)
  }
)


setGeneric(
  "name",
  def = function(x) standardGeneric("name")
)

setMethod(
  "name",
  signature = "NamedComponent",
  definition = function(x) x@name
)

setGeneric(
  "name<-",
  def = function(x, value) standardGeneric("name<-")
)

setMethod(
  "name<-",
  signature = "NamedComponent",
  definition = function(x, value) {
    x@name <- value
    return(x)
  }
)


