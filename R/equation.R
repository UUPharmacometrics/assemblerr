new_equation <- function(lhs = NULL, rhs = NULL){
  list(lhs = lhs, rhs = rhs) %>%
  structure(class = "equation")
}

#' @export
equation <- function(expr){
  expr <- rlang::enexpr(expr)
  if(rlang::is_formula(expr)) {
    return(new_equation(lhs = rlang::f_lhs(expr), rhs = rlang::f_rhs(expr)))
  }else{
    return(new_equation(rhs = expr))
  }
}

#' @export
empty_equation <- function() return(structure(list(), class = "equation"))

is_equationish <- function(o) return(rlang::is_formulaish(o) | is(o, "equation") | is.numeric(o))


#' @export
as_equation <- function(x) UseMethod("as_equation")
#' @export
as_equation.equation <- function(x) x
#' @export
as_equation.character <- function(x){
  expr <- rlang::parse_expr(paste0("~", x))
  if(rlang::is_formula(expr)){
    return(as_equation.language(expr))
  }else{
    stop("Couldn't interpret text '", x , "' as an equation")
  }
}
#' @export
as_equation.numeric <- function(x){
  new_equation(rhs = x)
}

#' @export
as_equation.formula <- function(x){
  new_equation(lhs = rlang::f_lhs(x),
               rhs = rlang::f_rhs(x))
}

as_equation.language <- as_equation.formula

#' @export
set_rhs <- function(eqn, rhs){
  purrr::update_list(eqn, rhs = rlang::enexpr(rhs))
}
#' @export
set_lhs <- function(eqn, lhs){
  purrr::update_list(eqn, lhs = rlang::enexpr(lhs))

}

#' @export
print.equation <- function(x){
  cat("Equation:\n")
  if(is.null(x$lhs)){
    cat("\t", deparse(x$rhs))
  }else{
    cat("\t", deparse(x$lhs), "=", deparse(x$rhs))
  }
}


#' @export
variables <- function(x){
  if(!is(x, "equation")) stop("Function expects an equation as input")
  all.vars(x$rhs)
}


#' @export
`+.equation` <- function(x, y){
  stopifnot(is(y, "equation"))
  if(rlang::is_empty(x) && rlang::is_empty(y)) return(as_equation(~0))
  if(rlang::is_empty(x)) return(y)
  if(rlang::is_empty(y)) return(x)
  x$rhs <- base::substitute(x + y, list(x = x$rhs, y = y$rhs))
  x
}
#' @export
`-.equation` <- function(x, y){
  stopifnot(is(y, "equation"))
  if(rlang::is_empty(x)&&rlang::is_empty(y)) return(as_equation(~0))
  if(rlang::is_empty(y)) return(x)
  if(rlang::is_empty(x)){
    y$rhs <- base::substitute(-x, list(x = y$rhs))
    return(y)
  }else{
    x$rhs <- base::substitute(x - y, list(x = x$rhs, y = y$rhs))
    return(x)
  }
}



#' @export
substitute_indicies <- function(eqn, array_name, substitutions){
  purrr::modify(eqn, ~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
}
#' @export
index_transformer <- function(node, array_name, substitutions){
  # if vector access
  if(rlang::is_lang(node) && rlang::lang_name(node) == "[" && node[[2]] == rlang::sym(array_name)){
    if(!exists(node[[3]], substitutions)) rlang::cnd_signal("missing_substitution", index = node[[3]])
    node[[3]] <- substitutions[[node[[3]]]]
  }
  node
}
#' @export
transform_ast <- function(node, transformer, ...){
  if(rlang::is_atomic(node) || rlang::is_symbol(node)) return(transformer(node, ...))
  else if(rlang::is_lang(node)){
    transformer(node, ...) %>%
      lapply(transform_ast, transformer, ...) %>%
      as.call() %>%
      return()
  }else if(rlang::is_pairlist(node)){
    lapply(node, transform_ast, ...) %>%
      rlang::as_pairlist() %>%
      return()
  }else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

#' @export
substitute <- function(x, ...) UseMethod("substitute")
#' @export
substitute.default <- base::substitute
#' @export
substitute.equation <- function(x, ...){
  args <- rlang::dots_list(...)
  purrr::map_if(x, is.language, ~pryr::substitute_q(.x, args)) %>%
    structure(class = "equation")
}
