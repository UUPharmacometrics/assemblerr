#' One or several imperative instructions
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
statement <- function(...){
  list(
    expressions = rlang::enexprs(...)
  ) %>%
    structure(class = "statement")
}

#' @export
#' @describeIn statement
stm <- statement


#' @export
print.statement <- function(x){
  if(length(x$expression)==1) cat("Statement:\n")
  else cat("Statements:\n")
  x$expressions %>%
    purrr::walk(~cat(deparse(.), sep = "\n"))
}

#' @export
as_statement <- function(x) UseMethod("as_statement")
#' @export
as_statement.declaration <- function(x) {
  if(is_anonymous(x)) return(stm(!!dec_get_def(x)))
  else return(stm(!!dec_get_id(x) <- !!dec_get_def(x)))
}
#' @export
as_statement.list <- function(x){
  if(!is_declarationish_list(x)) stop("All elements of a list need to be interpretable as a declaration.")
  purrr::map(x, as_statement) %>%
    purrr::map("expressions") %>%
    purrr::flatten() %>%
    purrr::invoke(stm, .)
}
