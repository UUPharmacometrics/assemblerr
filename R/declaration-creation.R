
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
  identifier <- rlang::set_names(identifier, NULL)
  definition <- rlang::set_names(definition, NULL)
  if (!rlang::is_empty(definition) && all(!purrr::map_lgl(definition, rlang::is_expression)))
    rlang::abort(message = "`definition` must be an expression")
  if (!rlang::is_empty(identifier) && all(!purrr::map_lgl(identifier, rlang::is_expression)))
    rlang::abort(message = "`identifier` must be an expression")
  if (!all(purrr::map_lgl(identifier, is_valid_lhs)))
    rlang::abort("The identifiers need to be symbols or array expressions")
  vctrs::new_rcrd(list(identifier = identifier, definition = definition),
                  class = "assemblerr_declaration")
}

setOldClass("assemblerr_declaration")


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
#' d2 <- declaration(v=theta[2]*exp(eta[2]))
declaration <- function(...){
  dots <- rlang::exprs(...)
  lhs <- purrr::map_if(dots, rlang::is_formula, rlang::f_lhs, .else = ~NULL)
  new_identifier <- purrr::imap(lhs, function(x, y) {
    if (is.null(x)) {
      if (y == "") {
        NULL
      }else{
        rlang::sym(y)
      }
    }else{
      x
    }
  })
  if (!all(purrr::map_lgl(new_identifier, is_valid_lhs)))
    rlang::abort("The left-hand side of each formula needs to be a symbol or an array expression")
  definition <- purrr::map_if(dots, rlang::is_formula, rlang::f_rhs)
  return(new_declaration(new_identifier, definition))
}


#' @export
is_declaration <- function(x) {
  return(inherits(x, "assemblerr_declaration"))
}

#' @export
as_declaration <- function(x) UseMethod("as_declaration")

as_declaration.assemblerr_declaration <- function(x) x

as_declaration.formula <- function(x) declaration(!!x)

as_declaration.numeric <- function(x) new_declaration(vec_rep(list(NULL), vec_size(x)), definition = as.list(x))

as_declaration.character <- function(x) {
  if (!all(is_valid_variable_name(x))) rlang::abort("Invalid variable name")
  new_declaration(vec_rep(list(NULL), vec_size(x)), rlang::syms(x))
}

as_declaration.name <- function(x) new_declaration(list(NULL), list(x))

# user-facing version with informative error message
ui_as_declaration <- function(x) {
  rlang::with_handlers(
    rlang::with_abort(as_declaration(x)),
    error = ~ rlang::abort(
      c(
        "Invalid declaration",
        x = paste0("'", rlang::as_label(rlang::enexpr(x)), "' can not be interpreted as a declaration."),
        i = "A declaration can be specified as a formula, number or the name of a variable."
      )
    )
  )
}

#' @export
as.list.assemblerr_declaration <- function(x, ...) {
  lbls <- purrr::map_if(dcl_id(x), ~!is.null(.x), deparse, .else = ~"")
  rlang::set_names(dcl_def(x), lbls)
}
