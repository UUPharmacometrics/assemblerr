#' @importFrom methods new
#' @importFrom methods show
#' @importFrom methods callNextMethod


# TODO: Need to implement fragment as a node list accepting everything



setGeneric(
  name = "combine",
  def = function(x, y) standardGeneric("combine")
)

Node <- setClass("Node")


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

NamedNode <- setClass(
  "NamedNode",
  contains = "Node",
  slots = c(name = "character")
)



NodeList <- setClass("NodeList",
                     contains = "list",
                     slots = c(node_class = "character"),
                     prototype = prototype(node_class = "Node"))

setMethod(
  f = "combine",
  signature = signature(x = "NodeList"),
  definition = function(x, y) {
    if (is(y, x@node_class)) {
      x@.Data <- append(x@.Data, y)
    }
    return(x)
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


