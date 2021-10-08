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

ComponentList <- setClass(
  "ComponentList",
  contains = "Component",
  slots = c(
    elements = "list"
  )
)

NamedComponentList <- setClass(
  "NamedComponentList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NamedComponent")
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
  signature = signature(x = "Component", y = "Component"),
  definition = function(x, y) {
      return(is(y, class(x)))
  }
)


setMethod(
  "is_compatible",
  signature = signature(x = "Component", y = "ComponentList"),
  definition = function(x, y) {
    return(is(y, component_class(x)))
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


ComponentList <- setClass(
  "ComponentList",
  contains = "Component",
  slots = c(
    elements = "list"
  )
)

#' Component class
#'
#' @noRd
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
    return("Component")
  }
)

setMethod(
  f = "add_component",
  signature = signature(x = "ComponentList", y = "Component"),
  definition = function(x, y, ...) {
    if (is(y, x@component_class)) {
      x@elements <- append(x@elements, y)
    } else {
      cnd <- rlang::cnd("test")
      rlang::cnd_signal(cnd)
    }
    return(x)
  }
)


NamedComponentList <- setClass(
  "NamedComponentList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NamedComponent")
)

setMethod(
  f = "add_component",
  signature = signature(x = "NamedComponentList", y = "NamedComponent"),
  definition = function(x, y, ...) {
    if (is(y, x@component_class)) {
      x@elements[[name(y)]] <- y
    } else {
      cnd <- rlang::cnd("test")
      rlang::cnd_signal(cnd)
    }
    return(x)
  }
)


# setMethod(
#   f = "description",
#   signature = "Node",
#   definition = function(x) {
#     class(x)
#   }
# )
#
# NamedNode <- setClass(
#   "NamedNode",
#   contains = "Node",
#   slots = c(name = "character")
# )
#
#
#
# NodeList <- setClass("NodeList",
#                      contains = "list",
#                      slots = c(node_class = "character"),
#                      prototype = prototype(node_class = "Node"))
#
# setMethod(
#   f = "initialize",
#   signature = "NodeList",
#   definition = function(.Object, ...){
#     dots <- rlang::list2(...)
#     callNextMethod(.Object, dots)
#   }
# )
#
# setIs("NodeList", "BuildingBlock")
#
# setMethod(
#   f = "combine",
#   signature = signature(x = "NodeList", y = "Node"),
#   definition = function(x, y) {
#     if (is(y, x@node_class)) {
#       x@.Data <- append(x@.Data, y)
#     }
#     return(x)
#   }
# )
#
# setMethod(
#   f = "combine",
#   signature = signature(x = "NodeList", y = "NodeList"),
#   definition = function(x, y) {
#     if (x@node_class == y@node_class) {
#       x@.Data <- vec_c(x@.Data, y@.Data)
#     }
#     return(x)
#   }
# )
#
# setMethod(
#   f = "description",
#   signature = "NodeList",
#   definition = function(x) {
#     TreeDescription(class(x), purrr::map_chr(x, description))
#   }
# )
#
# NamedNodeList <- setClass(
#   "NamedNodeList",
#   contains = "NodeList",
#   prototype = prototype(node_class = "NamedNode")
# )
#
# setMethod(
#   f = "initialize",
#   signature = "NamedNodeList",
#   definition = function(.Object, ...){
#     .Object <- callNextMethod(.Object, ...)
#     if (length(.Object) > 0) {
#       nms <- purrr::map_chr(.Object, "name")
#       names(.Object) <- nms
#     }
#     return(.Object)
#   }
# )
#
#
#
# setMethod(
#   f = "combine",
#   signature = signature(x = "NamedNodeList", y = "NamedNode"),
#   definition = function(x, y) {
#     if (is(y, x@node_class)) {
#       x[[y@name]] <- y
#     }
#     return(x)
#   }
# )
#
# setMethod(
#   f = "combine",
#   signature = signature(x = "NamedNodeList", y = "NamedNodeList"),
#   definition = function(x, y) {
#     if (x@node_class == y@node_class) {
#       new_names <- vec_c(names(x), names(y))
#       x@.Data <- vec_c(x@.Data, y@.Data)
#       names(x) <- new_names
#     }
#     return(x)
#   }
# )


