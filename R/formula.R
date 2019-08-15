#' Test if LHS of formula is valid
#'
#' @param fml A formula
#'
#' @return
fml_has_valid_lhs <- function(fml, allow_null = TRUE){
  if(!is(fml, "formula")) return(FALSE)
  lhs <- rlang::f_lhs(fml)
  if(is.null(lhs)) return(allow_null)
  # check if expr is valid for lhs
  contains_no_functions <- setdiff(all.names(lhs, unique = T), all.vars(lhs)) %>% setdiff("[") %>% rlang::is_empty()
  return(contains_no_functions)
}

fml_is_anonymous <- function(fml){
  return(is.null(rlang::f_lhs(fml)))
}

fml_get_lhs <- function(fml) rlang::f_lhs(fml)
fml_get_rhs <- function(fml) rlang::f_rhs(fml)
fml_set_lhs <- function(fml, value) rlang::`f_lhs<-`(fml, value)
fml_set_rhs <- function(fml, value) rlang::`f_rhs<-`(fml, value)

# fml_is_convertable <- function(fml, parse = FALSE){
#   if(!parse) return(rlang::is_formulaish(fml) | is_declaration(fml) | is.numeric(fml) | is.character(fml))
#   if(is.character(fml) || rlang::is_formulaish(o)) {
#     parses_succefully <- !is.null(purrr::possibly(as_declaration, NULL)(o))
#   }else{
#     parses_succefully <- FALSE
#   }
#   return(parses_succefully | is_declaration(o) | is.numeric(o))
# }

