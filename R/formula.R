#' Test if LHS of formula is valid
#'
#' @param fml A formula
#' @param allow_null Whether an empty LHS should be accepted
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

fml_vars <- function(fml, include_indicies = FALSE) {
  if(include_indicies) return(unique(unlist(find_vars_with_indicies(fml))))
  else return(all.vars(fml))
}

fml_funs <- function(fml) setdiff(all.names(fml, unique = T), c(all.vars(fml), "~"))

fml_combine <- function(fml1, fml2, op = '+', lhs){
  rhs1 <- fml_get_rhs(fml1)
  rhs2 <- fml_get_rhs(fml2)
  fml <- rlang::call2(op, rhs1, rhs2) %>%
    deparse() %>%
    paste0("~", .) %>%
    stats::formula(env = rlang::f_env(fml1))
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

fml_subs_sym <- function(fml, ...) do.call(substitute, args = list(fml, env = list(...)))

# returns true if var1 depends on var2
fml_depends_on <- function(var1, var2, fmls, include_indicies = TRUE) {
  to_visit <- var1
  visited <- c()
  while(!purrr::is_empty(to_visit)){
    visited <- union(visited, to_visit[1])
    to_visit <- fmls %>%
      purrr::keep(~deparse(fml_get_lhs(.x)) == to_visit[1]) %>%
      purrr::map(~fml_vars(.x, include_indicies = include_indicies)) %>%
      unlist() %>%
      union(to_visit) %>%
      setdiff(visited)
    if(var2 %in% to_visit) return(TRUE)
  }
  return(FALSE)
}

find_vars_with_indicies <- function(fml){
  if(rlang::is_call(fml)){
    if(rlang::call_name(fml) == "[") return(deparse(fml))
    results <- list()
    for(i  in 2:length(fml)) results[[i]] <- find_vars_with_indicies(fml[[i]])
    results <- purrr::compact(results)
    return(results)
  }else if(rlang::is_symbol(fml)) return(deparse(fml))
  else if(rlang::is_pairlist(fml)){
    lapply(fml, find_vars_with_indicies) %>%
      return()
  }else{
    return(NULL)
  }
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

