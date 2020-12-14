#' @include rendering.R


new_statement <- function(expr = list()){
  vec_assert(expr, ptype = list())
  if (!rlang::is_empty(expr) && all(!purrr::map_lgl(expr, rlang::is_expression)))
    rlang::abort(message = "`expr` must be an expression")
  new_vctr(expr, class = "assemblerr_statement")
}

statement <- function(...){
  dots <- rlang::dots_list(..., .named = FALSE) %>%
    rlang::set_names(NULL)
  new_statement(expr = dots)
}

setOldClass("assemblerr_statement")

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


setMethod(
  f = "render",
  signature = c(x = "assemblerr_statement"),
  definition = function(x) {
    vec_data(x) %>%
      purrr::map(transform_ast, transformer = vec2fcall_transformer) %>%
      purrr::map(transform_ast, transformer = assignment_transformer) %>%
      purrr::map_chr(deparse, control = c(), width.cutoff = 200) %>%
      glue::glue_collapse(sep = "\n") %>%
      toupper()
  }
)


assignment_transformer <- function(node){
  if (rlang::is_call(node) && rlang::call_name(node) == "<-") {
    node[[1]] <- quote(`=`)
  }
  node
}

vec2fcall_transformer <- function(node){
  if (rlang::is_call(node) && rlang::call_name(node) == "[") {
    node[[1]] <- node[[2]]
    node[[2]] <- NULL
  }
  node
}
