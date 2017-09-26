#' Facet
#'
#' Base class for all aspects of a model that can be manipulated.
#'
#' @section Usage:
#' \preformatted{facet <- Facet$new()
#'
#'
#' @importFrom R6 R6Class
#' @name Facet
NULL


Facet <- R6Class("Facet",
  public = list(
    initialize = function(){
      private$items <- list()
    },
    add = function(item, name){
      if(!is.null(private$type)) assertthat::assert_that(inherits(item, private$type))
      if(missing(name)|is.null(name)){
        private$items <- append(private$items, item)
      }else{
        assertthat::assert_that(!name %in% names(private$items))
        private$items[[name]] <- item
      }
      return(item)
    },
    # creates an object of the facet type and adds it
    add_new = function(...){
      args <- list(...)
      object <- get(private$type)$new(...)
      return(self$add(object, args$name))
    },
    get = function(name){
      return(private$items[[name]])
    },
    has = function(name){
      return(name %in% names(private$items))
    },
    count = function() return(length(private$items)),
    map = function(f) purrr::map(private$items, f)
  ),
  private = list(
    items = NULL,
    type = NULL
  )
)

