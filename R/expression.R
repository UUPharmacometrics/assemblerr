#'Convert to an expression
#'
#' @param x Object to convert
#' @export
as_expr <- function(x) UseMethod("as_expr")

#' @rdname as_expr
#' @export
as_expr.formula <- function(x) {
  if(fml_is_anonymous(x)) {
    return(fml_get_rhs(x))
  }
  else {
    expr <- bquote(.(fml_get_lhs(x)) <- .(fml_get_rhs(x)))
    return(expr)
  }
}
#' @rdname as_expr
#' @export
as_expr.list <- function(x) {
  purrr::map(x, as_expr)
}
#' @rdname as_expr
#' @export
as_expr.call <- function(x) {
  if(rlang::call_name(x)=="~"){
    as_expr.formula(x)
  } else{
    stop("Can not convert ", x, " to an expression")
  }
}
