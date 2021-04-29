#' @importFrom methods new
#' @importFrom methods show
#' @importFrom methods callNextMethod


# TODO: Need to implement fragment as a node list accepting everything



Node <- setClass("Node")

setIs("Node", "BuildingBlock")

setMethod(
  f = "combine",
  signature = signature(x = "Node"),
  definition = function(x, y) {
    slots <- getSlots(class(x))
    for (s in names(slots)) {
      if (is(y, class(slot(x, s)))) {
        slot(x, s) <- y
        break
      } else if (is(slot(x, s), "NodeList")) {
        slot(x, s) <- combine(slot(x, s), y)
        break
      }
    }
    return(x)
  }
)

setMethod(
  f = "description",
  signature = "Node",
  definition = function(x) {
    class(x)
  }
)

NamedNode <- setClass(
  "NamedNode",
  contains = "Node",
  slots = c(name = "character")
)



NodeList <- setClass("NodeList",
                     contains = "list",
                     slots = c(node_class = "character"),
                     prototype = prototype(node_class = "Node"))

setIs("NodeList", "BuildingBlock")

setMethod(
  f = "combine",
  signature = signature(x = "NodeList", y = "Node"),
  definition = function(x, y) {
    if (is(y, x@node_class)) {
      x@.Data <- append(x@.Data, y)
    }
    return(x)
  }
)

setMethod(
  f = "combine",
  signature = signature(x = "NodeList", y = "NodeList"),
  definition = function(x, y) {
    if (x@node_class == y@node_class) {
      x@.Data <- vec_c(x@.Data, y@.Data)
    }
    return(x)
  }
)

setMethod(
  f = "description",
  signature = "NodeList",
  definition = function(x) {
    TreeDescription(class(x), purrr::map_chr(x, description))
  }
)

NamedNodeList <- setClass(
  "NamedNodeList",
  contains = "NodeList",
  prototype = prototype(node_class = "NamedNode")
)


setMethod(
  f = "combine",
  signature = signature(x = "NamedNodeList", y = "NamedNode"),
  definition = function(x, y) {
    if (is(y, x@node_class)) {
      x@.Data[[y@name]] <- y
    }
    return(x)
  }
)

setMethod(
  f = "combine",
  signature = signature(x = "NamedNodeList", y = "NamedNodeList"),
  definition = function(x, y) {
    if (x@node_class == y@node_class) {
      new_names <- vec_c(names(x), names(y))
      x@.Data <- vec_c(x@.Data, y@.Data)
      names(x) <- new_names
    }
    return(x)
  }
)


