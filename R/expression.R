
as_expr <- function(x) UseMethod("as_expr")

as_expr.formula <- function(x) {
  if(fml_is_anonymous(x)) {
    return(fml_get_rhs(x))
  }
  else {
    expr <- bquote(.(fml_get_lhs(x)) <- .(fml_get_rhs(x)))
    return(expr)
  }
}
