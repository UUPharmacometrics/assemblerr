
new_declaration <- function(identifier = NULL, definition = NULL){
  list(identifier = identifier,
       definition = definition) %>%
    structure(class = "declaration")
}

make_identifier <- function(expr){
  if(is.null(expr)) return(NULL)
  # check if the expression is actually a name
  name <- purrr::possibly(eval, NULL)(expr)
  # check if the name is a valid variable name
  if(is.character(name)){
    if(make.names(name) != name) stop("identifier needs to be a valid variable name", call. = F)
    identifier <- rlang::sym(name)
  }else{
    # check if expr is valid for lhs
    contains_no_functions <- setdiff(all.names(expr, unique = T), all.vars(expr)) %>% setdiff("[") %>% rlang::is_empty()

    if(contains_no_functions) {
      identifier <- expr
    }else{
      stop("invalid identifier expression", call. = F)
    }
  }
  return(identifier)
}

#' Create a declaration
#'
#' Declarations are elementary building blocks of a model that provide the definition for an identifier
#'
#' @param identifier An expression or the name of the variable to be declared
#' @param definition An expression
#'
#' @return A declaration object
#' @export
#'
#' @examples
#' d <- declaration("cl", theta*exp(eta))
declaration <- function(identifier, definition){
  if(missing(identifier)){
    identifier <- NULL
  }else{
    identifier <- rlang::enexpr(identifier) %>% make_identifier()
  }
  if(missing(definition)){
    definition <- NULL
  }else{
    definition <- rlang::enexpr(definition)
  }
  # if only the identifer is given
  if(is.null(definition) && !is.null(identifier)) {
    definition <- identifier
    identifier <- NULL
  }
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
is_declarationish <- function(o, parse = F) {
  if(!parse) return(rlang::is_formulaish(o) | is_declaration(o) | is.numeric(o) | is.character(o))
  parses_succefully <- T
  if(is.character(o) || rlang::is_formulaish(o)) parses_succefully <- !is.null(purrr::possibly(as_declaration, NULL)(o))
  return(parses_succefully | is_declaration(o) | is.numeric(o))
}

#' @describeIn is_declaration Tests whether a declaration has an identifier
#' @export
is_anonymous <- function(o) {
  if(!is_declaration(o)) stop("Function expects a declaration as input")
  return(is.null(o$identifier))
}

#' @describeIn is_declaration Tests whether a declaration is empty
#' @export
is_empty_declaration <- function(o){
  return(is_declaration(o) & is.null(o$identifier) & is.null(o$definition))
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
  d <- NULL
  try({
    expr <- rlang::parse_expr(x)
    if(rlang::is_formula(expr)){
      d <- new_declaration(rlang::f_lhs(expr), rlang::f_rhs(expr))
    }else{
      d <- new_declaration(definition = expr)
    }
  },
  silent = T
  )
  if(!is.null(d)){
    return(d)
  }else{
    stop("Couldn't interpret text '", x , "' as a declaration")
  }
}

#' @describeIn as_declaration Returns an anonymous declaration with the definition set to the number provided.
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
    identifier <- make_identifier(lhs)
  }
  new_declaration(identifier = identifier, definition = rhs)
}
as_declaration.language <- as_declaration.formula


#' Interpret function argument as declaration
#'
#' Helper function to simplify the common task of checking whether an argument could serve as a declaration, raising an error or
#' doing the conversion.
#'
#' @param arg Variable to be interpreted
#'
#' @return A declaration or an error message if the variable could not be interpreted as a declaration
#' @export
arg_as_declaration <- function(arg){
  arg_expr <- rlang::enexpr(arg)
  if(!is_declarationish(arg, parse = T)) stop("Argument '", arg_expr %>% as.character(), "' can not be interpreted as a declaration", call. = F)
  return(as_declaration(arg))
}

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
  d <- arg_as_declaration(d)
  identifier <- rlang::enexpr(identifier)
  purrr::update_list(d, identifier = make_identifier(identifier))
}

get_identifier <- function(d) {
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  return(d$identifier)
}

get_definition <- function(d) {
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  return(d$definition)
}

#' @export
print.declaration <- function(x){
  if(is.null(x$definition) && is.null(x$identifier)){
    cat("Empty declaration")
  }else{
    if(is_anonymous(x)){
      cat("Anonymous declaration:\n")
      cat("\t", deparse(x$definition))
    }else{
      cat("Declaration:\n")
      cat("\t", deparse(x$identifier), "=", deparse(x$definition))
    }
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
  d1 <- arg_as_declaration(d1)
  d2 <- arg_as_declaration(d2)
  identifier <- rlang::enexpr(identifier)
  if(is_empty_declaration(d2)){
    def <- d1$definition
  }else if(is_empty_declaration(d1)){
    def <- d2$definition
  }else{
    def <- rlang::lang(op, d1$definition, d2$definition)
  }
  if(missing(identifier)){
    identifier <- get_identifier(d1)
  }else{
    identifier <- make_identifier(identifier)
  }
  return(new_declaration(identifier = identifier, definition = def))
}


#' Substitute the index names in a declaration
#'
#' @param d Declaration
#' @param array_name Name of the array the index names should be replaced for
#' @param substitutions List of subsitutions
#'
#' @return Declaration with replaced index names
#'
#' @examples
#' d <- declaration("dA", ka*A["depot"]-ke*A["central"])
#' index_subs_dec(d, "A", list(depot = 1, central = 2))
index_subs_dec <- function(d, array_name, substitutions){
  if(!is_declaration(d)) stop("Function expects a declaration as input")
  purrr::modify(d, ~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
}

index_transformer <- function(node, array_name, substitutions){
  # if vector access
  if(rlang::is_lang(node) && rlang::lang_name(node) == "[" && node[[2]] == rlang::sym(array_name)){
    if(!exists(node[[3]], substitutions)) rlang::cnd_signal("missing_substitution", index = node[[3]])
    node[[3]] <- substitutions[[node[[3]]]]
  }
  node
}


#' Substitute symbols in a declaration
#'
#' @param d Declaration
#' @param ... A list of declarations
#'
#' @return Declaration with substituted symbols in the definition
#'
#' @export
#' @examples
#' d <- declaration("cl", theta*exp(eta))
#' d1 <- declaration("theta", theta+a)
#' subs_dec(d, d1)
subs_dec <- function(d, ...){
  substitutions <- list(...) %>% as_declaration_list()
  d <- arg_as_declaration(d)
  if(any(purrr::map_lgl(substitutions, is_anonymous))) stop("substitutions need to be named")
  substitutions <- purrr::set_names(substitutions, purrr::map(substitutions, ~get_identifier(.x) %>% deparse))
  purrr::modify(d, ~transform_ast(.x, subs_transformer, substitutions = substitutions))
}

subs_transformer <- function(node, substitutions){
  # if vector access and replacement contains vector variable
  if(rlang::is_atomic(node) || rlang::is_symbol(node) || (rlang::is_lang(node) && rlang::lang_name(node) == "[")){
    if(exists(deparse(node), substitutions)){
      node <-  substitutions[[deparse(node)]] %>% get_definition()
    }
  }
  node
}



#' Modify AST
#'
#' This recursive function is the work-horse for all expression transformations. It takes a language node and a transformer function,
#' and applies the transformer recursivly to the node and all its child nodes.
#'
#' @param node A language node
#' @param transformer A transformer function
#' @param ... Additional arguments to the transformer function
#'
#' @return The transformed language node
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

