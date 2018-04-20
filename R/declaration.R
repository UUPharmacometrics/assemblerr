
new_declaration <- function(identifier = NULL, definition = NULL){
  list(identifier = identifier,
       definition = definition) %>%
    structure(class = "declaration")
}


#' Create a declaration
#'
#' Declarations are elementary building blocks of a model that provide the definition for an identifier
#'
#' @param identifier The name of the variable to be declared
#' @param definition An expression
#'
#' @return A declaration object
#' @export
#'
#' @examples
#' d <- declaration("cl", theta*exp(eta))
declaration <- function(identifier = NULL, definition){
  if(missing(definition)) stop("a definition needs to be provided")
  definition <- rlang::enexpr(definition)
  if(!is.null(identifier) && identifier!=make.names(identifier)) stop("the left-hand side of the provided expression needs to be a valid variable name")
  new_declaration(identifier, definition)
}

#' Test for declarations
#'
#' Tests if an object is a declaration or if it could behave as one
#'
#' @param o An object to be tested
#'
#' @return Boolean
#' @export
#'
is_declaration <- function(o) {
  return(is(o, "declaration"))
}

#' @describeIn is_declaration Has a wider definition of declaration and also returns true if the object could be converted to a declaration
#' @export
is_declarationish <- function(o) {
  return(rlang::is_formulaish(o) | is_declaration(o) | is.numeric(o))
}

#' @describeIn is_declaration Tests whether a declaration has an identifier
#' @export
is_anonymous <- function(o) {
  if(!is_declaration(o)) stop("Function expects a declaration as input")
  return(is.null(o$identifier))
}

#' Conversion to a declaration
#'
#' @param x Object to convert
#' @return A declaration
#' @export
#' @examples
#' d1 <- as_declaration("a+b+c")
#' d2 <- as_declaration(1)
#' d3 <- as_declaration(y~b+1)
as_declaration <- function(x) UseMethod("as_declaration")

#' @export
as_declaration.declaration <- function(x) x

#' @describeIn as_declaration Returns a declaration with the definition corresponding to the parsed character vector or an error if parsing was not successful.
#' @export
as_declaration.character <- function(x){
  expr <- rlang::parse_expr(paste0("~", x))
  if(rlang::is_formula(expr)){
    return(as_declaration.language(expr))
  }else{
    stop("Couldn't interpret text '", x , "' as a declaration")
  }
}

#' @describeIn as_declaration Returns a declaration with the definition set to the number provided.
#' @export
as_declaration.numeric <- function(x){
  new_declaration(definition = x)
}

#' @describeIn as_declaration Returns a declaration with the identifier and definition taken from the LHS and RHS of provided formula.
#' @export
as_declaration.formula <- function(x){
  lhs <- rlang::f_lhs(x)
  rhs <- rlang::f_rhs(x)
  identifier <- NULL
  if(!is.null(lhs)) {
    identifier <- deparse(lhs)
    if(identifier!=make.names(identifier)) stop("the left-hand side of a two-sided formula must be a single valid variable name")
  }
  new_declaration(identifier = identifier, definition = rhs)
}
as_declaration.language <- as_declaration.formula


#' Set identifier or definition of a declaration
#'
#' @param d A declaration
#' @param identifier The name of the new identifier
#' @param definition An expression for the new definition
#'
#' @return The modified declaration
#' @export
#'
#' @examples
#' d <- declaration("cl", theta*exp(eta))
#' d2 <- set_identifier(d, "v")
#' d3 <- set_definition(d, theta)
set_definition <- function(d, definition){
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  purrr::update_list(d, definition = rlang::enexpr(definition))
}
#' @export
#' @rdname set_definition
set_identifier <- function(d, identifier = NULL){
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  if(!(is.character(identifier) | is.null(identifier))) stop("identifier needs to be a character vector or NULL")
  purrr::update_list(d, identifier = identifier)
}

#' @export
print.declaration <- function(x){
  if(is_anonymous(x)){
    cat("Anonymous declaration:\n")
    cat("\t", deparse(x$definition))
  }else{
    cat("Declaration:\n")
    cat("\t", x$identifier, "=", deparse(x$definition))
  }
}


#' List variables and functions of a declaration
#'
#' @param d A declaration
#'
#' @return A character vector of variables or function used in the definition.
#' @export
#'
#' @examples
#' d <- declaration("cl", theta*exp(eta))
#' variables(d)
#' functions(d)
variables <- function(d){
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  all.vars(x$definition)
}

#' @export
#' @rdname variables
functions <- function(d){
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  setdiff(all.names(x$definition, unique = T), all.vars(x$definition))
}

#' Combine two declarations
#'
#' The function allows the combination of two declarations using an arbitrary operation.
#'
#' @param d1 Declaration
#' @param d2 Declaration
#' @param op Operation (function) to combine the declarations with
#' @param identifier Identifier for the resulting declaration (taken from d1 if missing)
#'
#' @return A declaration with the specified identifier and the defintion resulting from combining d1 with d2 using op
#' @export
#'
#' @examples
#' d1 <- declaration(definition = ka*A["depot"])
#' d2 <- declaration(definition = ke*A["central"])
#' d3 <- combine_dec(d1, d2, "-", "dA")
combine_dec <- function(d1, d2, op = "+", identifier){
  if(!is_declaration(d1)||!is_declaration(d2)) stop("Function expects two declarations as input")
  def <- rlang::lang(op, d1$definition, d2$definition)
  if(missing(identifier)){
    identifier <- d1$identifier
  }
  return(new_declaration(identifier = identifier, definition = def))
}


#' @export
substitute_indicies <- function(d, array_name, substitutions){
  purrr::modify_at(d, "definition", ~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
}

index_transformer <- function(node, array_name, substitutions){
  # if vector access
  if(rlang::is_lang(node) && rlang::lang_name(node) == "[" && node[[2]] == rlang::sym(array_name)){
    if(!exists(node[[3]], substitutions)) rlang::cnd_signal("missing_substitution", index = node[[3]])
    node[[3]] <- substitutions[[node[[3]]]]
  }
  node
}

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
substitute.declaration <- function(x, ...){
  args <- rlang::dots_list(...)
  purrr::map_if(x, is.language, ~pryr::substitute_q(.x, args)) %>%
    structure(class = "declaration")
}
