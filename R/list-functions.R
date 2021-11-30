
setGeneric(
  "modify",
  def = function(.x, .f, ...) {
    standardGeneric("modify")
  }
)

setMethod(
  "modify",
  signature = signature(.x = "list"),
  definition = function(.x, .f, ...) {
    return(purrr::modify(.x, .f, ...))
  }
)

setMethod(
  "modify",
  signature = signature(.x = "ComponentList"),
  definition = function(.x, .f, ...) {
    .x@elements <- modify(.x@elements, .f, ...)
    return(.x)
  }
)


setGeneric(
  "map",
  def = function(.x, .f, ...) {
    standardGeneric("map")
  }
)

setMethod(
  "map",
  signature = signature(.x = "list"),
  definition = function(.x, .f, ...) {
    return(purrr::map(.x, .f, ...))
  }
)

setMethod(
  "map",
  signature = signature(.x = "ComponentList"),
  definition = function(.x, .f, ...) {
    return(purrr::map(.x@elements, .f, ...))
  }
)

map_chr <- function(.x, .f, ...) {
  as.character(map(.x, .f, ...))
}


map_lgl <- function(.x, .f, ...) {
  as.logical(map(.x, .f, ...))
}
