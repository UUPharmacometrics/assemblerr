#' @importFrom methods new
#' @importFrom methods show
#' @importFrom methods callNextMethod
FacetEntry <- setClass("FacetEntry",
         slots = c(label = "character", facet_class = "character"),
         prototype = prototype(label = "unnamed", facet_class = "Facet"))


setGeneric(
  name = "description",
  def = function(x) standardGeneric("description")
)

setMethod(
  f = "description",
  signature = "FacetEntry",
  definition = function(x) {
      x@label
  }
)

setGeneric(
  name = "compact_description",
  def = function(x, compact) standardGeneric("compact_description")
)

setMethod(
  f = "compact_description",
  signature = "ANY",
  definition = function(x) description(x)
)

setMethod(
  f = "show",
  signature = "FacetEntry",
  definition = function(object) {
    print_shortened_tree_description(
      tree_description = description(object),
      skip_root = FALSE
    )
  }
)

NamedFacetEntry <- setClass("NamedFacetEntry",
         slots = c(name = "character"),
         contains = "FacetEntry",
         prototype = prototype(facet_class = "NamedFacet"))

setMethod(
  f = "description",
  signature = "NamedFacetEntry",
  definition = function(x) {
    x@name
  }
)


setMethod(f = "initialize",
          signature = "NamedFacetEntry",
          definition = function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if (rlang::is_empty(.Object@name)) stop("The facet entry needs to be named", call. = FALSE)
            .Object
          })




Facet <- setClass("Facet",
                  slots = c(label = "character", entries = "list", entry_class = "character"),
                  prototype = prototype(
                    label = "unnamed facet",
                    entries = list(),
                    entry_class = "FacetEntry"
                    )
                  )

setMethod(f = "initialize",
          signature = "Facet",
          definition = function(.Object, entries = list(), ...){
            .Object <- callNextMethod(.Object, ...)
            if (!all(purrr::map_lgl(entries, ~is(.x, .Object@entry_class)))) {
              stop("All entries need to be of type ", .Object@entry_class,  call. = FALSE)
            }
            .Object@entries <- entries
            .Object
          })


setMethod(
  f = "description",
  signature = "Facet",
  definition = function(x) {
    TreeDescription(facet_names_to_labels(class(x)), purrr::map_chr(x@entries, description))
  }
)

setMethod(
  f = "show",
  signature = "Facet",
  definition = function(object) {
    print_shortened_tree_description(
      tree_description = compact_description(object),
      skip_root = FALSE
    )
  }
)

setGeneric(
  name = "index_of",
  def = function(facet, x) standardGeneric("index_of")
)

setMethod(
  f = "index_of",
  signature = signature(facet = "Facet", x = "FacetEntry"),
  definition = function(facet, x) {
    which(purrr::map_lgl(facet@entries, ~identical(.x, x)))
  }
)

#'@export
setGeneric(
  name = "check",
  def = function(x, ...) standardGeneric("check")
)

setMethod(
  f = "check",
  signature = signature(x = "Facet"),
  definition = function(x, ...) {
    return(IssueList())
  }
)

setGeneric(
  name = "defined_variables",
  def = function(x) standardGeneric("defined_variables")
)

setMethod(
  f = "defined_variables",
  signature = signature(x = "Facet"),
  definition = function(x) {
    return(character(0))
  }
)

setGeneric(name = "add_entry",
           def = function(x, y) standardGeneric("add_entry"))

setMethod(
  f = "add_entry",
  signature = c(x = "Facet", y = "FacetEntry"),
  definition = function(x, y){
    x@entries <- c(x@entries, y)
    x
  }
)

NamedFacet <- setClass("NamedFacet", contains = "Facet", prototype = prototype(entry_class = "NamedFacetEntry"))

setMethod(f = "initialize",
          signature = "NamedFacet",
          definition = function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            names(.Object@entries) <- purrr::map_chr(.Object@entries, "name")
            .Object
          })



setMethod(
  f = "index_of",
  signature = signature(facet = "NamedFacet", x = "character"),
  definition = function(facet, x) {
    which(names(facet@entries) == x)
  }
)

#' Access facet names
#' @param x a named facet
#' @keywords internal
setMethod(
  f = "names",
  signature = c(x = "NamedFacet"),
  definition = function(x){
    names(x@entries)
  }
)

setMethod(
  f = "add_entry",
  signature = c(x = "NamedFacet", y = "NamedFacetEntry"),
  definition = function(x, y){
    x@entries[[y@name]] <- y
    x
  }
)

Fragment <- setClass("Fragment", slots = c(facets = "list"))

setMethod(f = "initialize",
          signature = "Fragment",
          definition = function(.Object, facets = list(), ...){
            .Object <- callNextMethod(.Object, ...)
            if (!all(purrr::map_lgl(facets, ~is(.x, "Facet")))) {
              stop("All entries need to be of type 'Facet'",  call. = FALSE)
            }
            .Object@facets <- facets
            names(.Object@facets) <- purrr::map_chr(.Object@facets, class)
            .Object
          })

setMethod(
  f = "description",
  signature = "Fragment",
  definition = function(x) {
    TreeDescription(class(x), purrr::map(x@facets, description))
  }
)

setMethod(
  f = "compact_description",
  signature = "Fragment",
  definition = function(x) {
    TreeDescription(class(x), purrr::map(x@facets, compact_description))
  }
)



setClass("GenericModel",
         slots = c(options = "list"),
         contains = "Fragment")

setMethod(
  f = "check",
  signature = signature(x = "GenericModel"),
  definition = function(x) {
    issues <- purrr::map(x@facets, check, model = x)
    purrr::reduce(issues, c, .init = IssueList())
  }
)

setGeneric(
  name = "optimize_for_conversion",
  def = function(source, target, component, options, ...) standardGeneric("optimize_for_conversion")
)

setMethod(
  f = "optimize_for_conversion",
  signature = signature(source = "GenericModel", target = "ANY", component = "ANY"),
  definition = function(source, target, component, options, ...) {
    source
  }
)


setMethod(
  f = "optimize_for_conversion",
  signature = signature(source = "GenericModel", target = "ANY", component = "missing"),
  definition = function(source, target, component, options, ...) {
    purrr::reduce(source@facets, ~optimize_for_conversion(.x, target, .y, options = options), .init = source)
  }
)



setGeneric(name = "combine",
           def = function(x, y) standardGeneric("combine"))


setMethod(
  f = "combine",
  signature = c(x = "FacetEntry", y = "FacetEntry"),
  definition = function(x, y){
    combine(new(x@facet_class, entries = list(x)), y)
  }
)

setMethod(
  f = "combine",
  signature = c(x = "Facet", y = "FacetEntry"),
  definition = function(x, y){
    if (is(y, x@entry_class)) {
      add_entry(x, y)
    }else{
      combine(Fragment(facets = list(x)), y)
    }
  }
)

#' @importFrom  methods slot slot<- slotNames
setMethod(
  f = "combine",
  signature = c(x = "Facet", y = "Facet"),
  definition = function(x, y){
    if (class(x) == class(y)) {
      slots <- slotNames(x)
      slots <- slots[slots != "entries"]
      x <- purrr::reduce(slots, .init = x, function(obj, s) {
          slot(obj, s) <- slot(y, s)
          obj
      })
      purrr::reduce(y@entries, ~add_entry(.x, .y), .init = x)
    }else{
      combine(Fragment(facets = list(x)), y)
    }
  }
)

setMethod(
  f = "combine",
  signature = c(x = "Fragment", y = "FacetEntry"),
  definition = function(x, y){
    if (y@facet_class %in% names(x@facets)) {
      x@facets[[y@facet_class]] <- combine(x@facets[[y@facet_class]], y)
    } else {
      x@facets[[y@facet_class]] <- new(y@facet_class, entries = list(y))
    }
    x
  }
)

setMethod(
  f = "combine",
  signature = c(x = "GenericModel", y = "FacetEntry"),
  definition = function(x, y){
    if (!y@facet_class %in% names(x@facets)) {
      facet <- new(y@facet_class)
      rlang::warn(
        c(glue::glue("Ignoring '{y@label}' building block"),
          x = glue::glue("The model does not have a facet '{facet@label}'"),
          i = "Not all building blocks can be added to a specific model type")
      )
      return(x)
    }
    callNextMethod(x, y)
  }
)

setMethod(
  f = "combine",
  signature = c(x = "Fragment", y = "Facet"),
  definition = function(x, y){
    if (class(y) %in% names(x@facets)) {
      x@facets[[class(y)]] <- combine(x@facets[[class(y)]], y)
    } else {
      x@facets[[class(y)]] <- y
    }
    x
  }
)

setMethod(
  f = "combine",
  signature = c(x = "GenericModel", y = "Facet"),
  definition = function(x, y){
    if (!class(y) %in% names(x@facets)) {
      blk <- y@entries[[1]]
      rlang::warn(
        c(cli::pluralize("Ignoring {length(blk)} '{blk@label}' building block{?s}"),
          x = glue::glue("The model does not have a facet '{y@label}'"),
          i = "Not all building blocks can be added to a specific model type")
      )
      return(x)
    }
    callNextMethod(x, y)
  }
)

setMethod(
  f = "show",
  signature = "GenericModel",
  definition = function(object) {
    compact_description(object) %>%
      print_tdesc_as_list()
    issues <- check(object)
    if (length(issues) > 0) {
      cli::cli_text("{length(issues)} issue{?s}")
    }
  }
)

setMethod(
  f = "combine",
  signature = c(x = "Fragment", y = "Fragment"),
  definition = function(x, y){
    purrr::reduce(y@facets, ~combine(.x, .y), .init = x)
  }
)


setClassUnion("BuildingBlock", members = c("FacetEntry", "Facet", "Fragment"))

#' Add building blocks
#' @param e1 A building block
#' @param e2 A building block
#' @keywords internal
setMethod(
  f = "+",
  signature = c(e1 = "BuildingBlock", e2 = "BuildingBlock"),
  definition =  function(e1, e2) {
    combine(e1, e2)
  }
)

#' Add building blocks
#' @param e1 A building block
#' @param e2 NULL
#' @keywords internal
setMethod(
  f = "+",
  signature = c(e1 = "BuildingBlock", e2 = "NULL"),
  definition =  function(e1, e2) {
    e1
  }
)


setMethod(
  f = "show",
  signature = "BuildingBlock",
  definition = function(object) {
    print_shortened_tree_description(
      tree_description = compact_description(object)
    )
  }
)

