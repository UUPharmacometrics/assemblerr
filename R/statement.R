new_statement <- function(expr = list()){
  vec_assert(expr, ptype = list())
  new_vctr(expr, class = "assemblerr_statement")
}

statement <- function(...){
  dots <- rlang::enexprs(..., .named = FALSE) %>%
    purrr::set_names(NULL) %>%
    purrr::map(~`attributes<-`(.x, NULL))
  new_statement(expr = dots)
}

#' @export
format.assemblerr_statement <- function(x, ...){
  purrr::map(x, deparse, width.cutoff = 20L) %>%
    purrr::map(trimws) %>%
    purrr::map_chr(paste, collapse = ";") %>%
    purrr::modify_if(~nchar(.x)>20, ~paste0(substr(.x, 1, 20), "..."))
}

as_statement <- function(x, ...){
  UseMethod("as_statement")
}

as_statement.assemblerr_declaration <- function(x, ...){
  ids <- dcl_id(x)
  defs <- dcl_def(x)
  exprs <- purrr::map2(
    .x = ids,
    .y = defs,
    .f = function(id, def) {
      if (is.null(id)) {
        bquote(.(def))
      }else{
        bquote(.(id) <- .(def))
      }
    }
  )
  statement(!!!exprs)
}

vec_ptype_abbr.assemblerr_statement <- function(x, ...) "stm"
vec_ptype_full.assemblerr_statement <- function(x, ...) "statement"

as_code <- function(x, ...) UseMethod("as_code")

as_code.assemblerr_statement <- function(x, ...){

}
