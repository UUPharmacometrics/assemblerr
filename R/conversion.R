#' @include model.R
#' @include pk_model.R


#' @export
setGeneric("as_nm_model",
           def = function(x, options = options_nm()){
             convert(target = nm_model(options = options),
                     source = x)
           },
           valueClass = "NmModel"
            )

#' @export
setMethod(
  f = "as_nm_model",
  signature = signature(x = "PkModel"),
  definition = function(x, options = options_nm()){
    convert(model(), x) %>%
      convert(nm_model(options = options), .)
  }
)
