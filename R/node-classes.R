#' @importFrom methods new
#' @importFrom methods show
#' @importFrom methods callNextMethod

setGeneric(
  name = "appendNode",
  def = function(x, y) standardGeneric("appendNode")
)

Node <- setClass("Node")


setMethod(
  f = "appendNode",
  signature = signature(x = "Node"),
  definition = function(x, y) {
    slots <- getSlots(class(x))
    for (s in names(slots)) {
      if (is(y, class(slot(x, s)))) {
        slot(x, s) <- y
        break
      } else if (is(slot(x, s), "NodeList")) {
        slot(x, s) <- appendNode(slot(x, s), y)
      }
    }
    return(x)
  }
)



NodeList <- setClass("NodeList",
                     contains = "list",
                     slots = c(node_class = "character"),
                     prototype = prototype(node_class = "Node"))

setMethod(
  f = "appendNode",
  signature = signature(x = "NodeList"),
  definition = function(x, y) {
    if (is(y, x@node_class)) {
      x@.Data <- append(x@.Data, y)
    }
    return(x)
  }
)

Distribution <- setClass("Distribution",
                         slots = c(parameters = "character"),
                         contains = "Node")

ParameterList

PKM <- setClass("PKM",
                slots = c(distribution = "Distribution", parameters = "NodeList"),
                contains = "Node")

