#' Test if LHS of formula is valid
#'
#' @param fml A formula
#'
#' @return TRUE/FALSE
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

fml_vars <- function(fml) all.vars(fml)
fml_funs <- function(fml) setdiff(all.names(fml, unique = T), c(all.vars(fml), "~"))

fml_combine <- function(fml1, fml2, op = '+', lhs){
  rhs1 <- fml_get_rhs(fml1)
  rhs2 <- fml_get_rhs(fml2)
  fml <- rlang::call2(op, rhs1, rhs2) %>%
    deparse() %>%
    paste0("~", .) %>%
    formula(env = rlang::f_env(fml1))
  if(!missing(lhs)) fml <- fml_set_lhs(fml, lhs)
  return(fml)
}

fml_subs_idx <- function(fml, array_name, substitutions){
  substitutions <- as.list(substitutions)
  purrr::modify_if(fml, ~!is.null(.x), ~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
}

index_transformer <- function(node, array_name, substitutions){
  # if vector access
  if(rlang::is_call(node) && rlang::call_name(node) == "[" && node[[2]] == rlang::sym(array_name)){
    if(!exists(node[[3]], substitutions)) rlang::warn("missing_substitution", index = node[[3]])
    node[[3]] <- substitutions[[node[[3]]]]
  }
  node
}



# fml_is_convertable <- function(fml, parse = FALSE){
#   if(!parse) return(rlang::is_formulaish(fml) | is_declaration(fml) | is.numeric(fml) | is.character(fml))
#   if(is.character(fml) || rlang::is_formulaish(o)) {
#     parses_succefully <- !is.null(purrr::possibly(as_declaration, NULL)(o))
#   }else{
#     parses_succefully <- FALSE
#   }
#   return(parses_succefully | is_declaration(o) | is.numeric(o))
# }

