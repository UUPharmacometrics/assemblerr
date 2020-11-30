#' @include model.R
#' @include pk_model.R


#' Convert model to NONMEM model
#'
#' @param from Model to convert
#' @export
as_nm_model <- function(from) UseMethod("as_nm_model")

setGeneric("as_nm_model",
           def = function(x, options = options_nm()){
             convert(target = nm_model(options = options),
                     source = x)
           },
           valueClass = "NmModel"
            )
setMethod(
  f = "as_nm_model",
  signature = signature(x = "PkModel"),
  definition = function(x, options = options_nm()){
    convert(model(), x) %>%
      convert(nm_model(options = options), .)
  }
)

#' Convert to a model
#'
#' @param from The source
#' @export
as_model <- function(from) UseMethod("as_model")

#' @export
as_model.model <- function(from) return(from)

