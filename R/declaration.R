
#' `declaration` constructor
#'
#' The internal constructor for a declaration vector. The user-facing version is `declaration`.
#'
#' The arguments `identifier` and `definition` are lists of R expressions. For `identifier` only symbols, array expressions
#' (e.g., theta[1]), or NULL are permitted.
#'
#' @param identifier List of expressions
#' @param definition List of expressions
#'
#' @return An assemblerr_declaration object
#' @export
#' @keywords internal
new_declaration <- function(identifier = list(), definition = list()){
  vctrs::vec_assert(identifier, ptype = list())
  vctrs::vec_assert(definition, ptype = list())
  if (!rlang::is_empty(definition) && all(!purrr::map_lgl(definition, rlang::is_expression)))
    rlang::abort(message = "`definition` must be an expression")
  if (!rlang::is_empty(identifier) && all(!purrr::map_lgl(identifier, rlang::is_expression)))
    rlang::abort(message = "`identifier` must be an expression")
  if (!all(purrr::map_lgl(identifier, is_valid_lhs)))
    rlang::abort("The identifiers need to be symbols or array expressions")
  vctrs::new_rcrd(list(identifier = identifier, definition = definition), class = "assemblerr_declaration")
}

#' Declaration
#'
#' A declaration is the mathematical definition of a set of variables. It is the lowest level building block for a model
#' in `assemblerr`. A declaration consists of the variable names being declared (the identifiers) and their definition. The
#' `declaration` function allows the specification of a declaration using `R` formulae.
#'
#' @param ... List of R formulae with a single symbol on the left-hand side and a valid R expression on the right
#'
#' @return A declaration vector
#' @export
#'
#' @examples
#' d <- declaration(cl~theta[1]+eta[1])
declaration <- function(...){
  fmls <- list(...)
  if (!all(purrr::map_lgl(fmls, rlang::is_formula)))
    rlang::abort("Each argument needs to be valid R formulae.")
  identifier <- purrr::map(fmls, rlang::f_lhs)
  if (!all(purrr::map_lgl(identifier, is_valid_lhs)))
    rlang::abort("The left-hand side of each formula needs to be a symbol or an array expression")
  definition <- purrr::map(fmls, rlang::f_rhs)
  return(new_declaration(identifier, definition))
}

#' @export
vec_proxy_equal.assemblerr_declaration <- function(x, ...){
  data_frame(identifier = dcl_id_label(x, null_value = "."),
                   definition = dcl_def(x))
}

#' Access declaration fields
#'
#' @param dcl An assemblerr_declaration
#'
#' @return An expression
#' @export
#' @keywords internal
dcl_id <- function(dcl){
  vctrs::field(dcl, "identifier")
}
#' @rdname dcl_id
dcl_def <- function(dcl){
  vctrs::field(dcl, "definition")
}

dcl_id_label <- function(dcl, null_value = NA_character_){
  labels <- purrr::map_chr(dcl_id(dcl), rlang::expr_text)
  if (!is.null(null_value)) labels[labels == "NULL"] <- null_value
  labels
}

dcl_def_label <- function(dcl) {
  purrr::map_chr(dcl_def(dcl), rlang::expr_text)
}

#' Test if an expression is a valid LHS for a declaration
#'
#' @param expr an expression
#'
#' @return TRUE/FALSE
is_valid_lhs <- function(expr){
  contains_no_functions <- setdiff(all.names(expr, unique = T), all.vars(expr)) %>% setdiff("[") %>% rlang::is_empty()
  return(contains_no_functions)
}


#' @export
format.assemblerr_declaration <- function(x, ...){
  id_txt <- dcl_id_label(x, null_value = ".")
  def_txt <- dcl_def_label(x)
  out <- paste0("`", id_txt, " ~ ", def_txt, "`")
}

vec_ptype_abbr.assemblerr_declaration <- function(x, ...) "dcl"
vec_ptype_full.assemblerr_declaration <- function(x, ...) "declaration"

list_of_declaration <- function(...){
  list_of(..., .ptype = new_declaration())
}
