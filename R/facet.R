FacetEntry <- setClass("FacetEntry",
         slots = c(facet_class = "character"),
         prototype = prototype(facet_class = "Facet"))

NamedFacetEntry <- setClass("NamedFacetEntry",
         slots = c(name = "character"),
         contains = "FacetEntry",
         prototype = prototype(facet_class = "NamedFacet"))

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
setMethod(
  f = "show",
  signature = signature(object = "Facet"),
  definition = function(object){
    cli::cli_text("{object@label}: {cli::no(length(object@entries))} entr{?y/ies}")
    invisible(NULL)
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

#'@export
setMethod(
  f = "show",
  signature = signature(object = "NamedFacet"),
  definition = function(object){
    cli::cli_text("{object@label}: {names(object@entries)}{?none//}")
    invisible(NULL)
  }
)


setMethod(
  f = "index_of",
  signature = signature(facet = "NamedFacet", x = "character"),
  definition = function(facet, x) {
    which(names(facet@entries) == x)
  }
)

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
  f = "show",
  signature = signature(object = "Fragment"),
  definition = function(object){
    nfacets <- length(object@facets)
    cli::cli_text("assemblerr fragment (size {nfacets})")
    div <- cli::cli_div()
    dl <- cli::cli_dl()
    purrr::walk(object@facets, function(x) {
      li <- cli::cli_li()
      show(x)
      cli::cli_end(li)
    })
    cli::cli_end(dl)
    cli::cli_end(div)
    invisible(NULL)
  }
)

setClass("GenericModel",
         slots = c(options = "list"),
         contains = "Fragment")

setMethod(f = "initialize",
          signature = "GenericModel",
          definition = function(.Object, facets, options = list(), ...){
            .Object <- callNextMethod(.Object, options = options, ...)
            .Object <- purrr::reduce(facets, ~combine(.x, .y), .init = .Object)
            .Object
          })

setMethod(
  f = "show",
  signature = signature(object = "GenericModel"),
  definition = function(object){
    nfacets <- length(object@facets)
    cli::cli_text("assemblerr model")
    div <- cli::cli_div()
    dl <- cli::cli_dl()
    purrr::discard(object@facets, ~length(.x@entries)==0) %>%
    purrr::walk(function(x) {
      li <- cli::cli_li()
      show(x)
      cli::cli_end(li)
    })
    nempty <- purrr::map_int(object@facets, ~as.integer(length(.x@entries)==0)) %>%
      sum()
    li <- cli::cli_li()
    cli::cli_text("({nempty} additional empty facet{?s})")
    cli::cli_end(li)
    cli::cli_end(dl)
    cli::cli_end(div)
    invisible(NULL)
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

setMethod(
  f = "combine",
  signature = c(x = "Facet", y = "Facet"),
  definition = function(x, y){
    if (class(x) == class(y)) {
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
  signature = c(x = "Fragment", y = "Fragment"),
  definition = function(x, y){
    purrr::reduce(y@facets, ~combine(.x, .y), .init = x)
  }
)


setClassUnion("BuildingBlock", members = c("FacetEntry", "Facet", "Fragment"))

setMethod(
  f = "+",
  signature = c(e1 = "BuildingBlock", e2 = "BuildingBlock"),
  definition =  function(e1, e2) {
    combine(e1, e2)
  }
)





