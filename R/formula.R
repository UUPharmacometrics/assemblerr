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

fml_vars <- function(fml, include_indicies = FALSE, include_lhs = TRUE) {
  if(!include_lhs) fml <- fml_set_lhs(fml, NULL)
  if(include_indicies) return(unique(unlist(find_vars_with_indicies(fml))))
  else return(all.vars(fml))
}

fml_funs <- function(fml) setdiff(all.names(fml, unique = T), c(all.vars(fml), "~"))

fml_combine <- function(fml1, fml2, op = '+', lhs = NULL){
  if(is.null(fml1)) return(fml_set_lhs(fml2, lhs))
  if(is.null(fml2)) return(fml_set_lhs(fml1, lhs))
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

fml_subs_fml <- function(fml, ...) {
  substitutions <- list(...)

  fmls <- list(...)
  lhs <- purrr::map(fmls, fml_get_lhs) %>%
    purrr::map_chr(deparse)
  substitutions <- purrr::set_names(substitutions, lhs)
  return(transform_ast(fml, fml_subs_transformer, substitutions = substitutions))
}


fml_subs_transformer <- function(node, substitutions){
  # if vector access and replacement contains vector variable
  if(rlang::is_atomic(node) || rlang::is_symbol(node) || (rlang::is_call(node) && rlang::call_name(node) == "[")){
    if(exists(deparse(node), substitutions)){
      node <-  substitutions[[deparse(node)]] %>% fml_get_rhs()
    }
  }
  node
}


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

# returns a list of indicies from the declaration list that depend on the variable
fmls_direct_dependants <- function(fmls, variable){
  fmls %>%
    purrr::map(fml_vars, include_lhs = FALSE) %>%
    purrr::map_lgl(~ variable  %in% .x) %>%
    which()
}

# orders the provided declaration list topologically
fmls_topologic_order <- function(fmls){
  # DFS (https://en.wikipedia.org/wiki/Topological_sorting)
  l <- c() # list of sorted nodes
  marked_perm <- c() # nodes completed
  marked_temp <- c() # nodes visited but not completed
  unmarked <- seq_along(fmls) %>% rev() # nodes not yet visited
  while(!rlang::is_empty(unmarked)){
    i <- unmarked[1]
    ret <- topologic_visit(fmls, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
    unmarked <- setdiff(unmarked, c(marked_perm, marked_temp))
  }
  return(l)
}

topologic_visit <- function(fml, index, marked_perm, marked_temp, l){
  if(index %in% marked_perm) return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
  if(index %in% marked_temp) stop("Error")
  marked_temp <- c(marked_temp, index)
  # find all nodes that depend on the current node
  var <- fml_get_lhs(fml[[index]]) %>% deparse()
  for(i in fmls_direct_dependants(fml, var)){
    ret <- topologic_visit(fml, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
  }
  marked_temp <- marked_temp %>% purrr::discard(~.x == index)
  marked_perm <- c(marked_perm, index)
  l <- c(index, l)
  return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
}

fmls_topologic_sort <- function(fmls){
  fmls[fmls_topologic_order(fmls)]
}

fmls_external_dependencies <- function(fmls){
  fmls[topologic_order(fmls) %>% rev()] %>% # process equations in reverse topological order
    purrr::reduce(~c(.x, fml_vars(.y)) %>% purrr::discard(function(x) x == deparse(fml_get_lhs(.y))) , .init = c()) # at each step add all variables from the rhs and remove variable from lhs
}

as_fml <- function(x) UseMethod("as_fml")

as_fml.formula <- function(x) return(x)

as_fml.numeric <- function(x) stats::as.formula(paste0("~",x))

fml_is <- function(x){
  return(is(x, "formula") || is.numeric(x))
}

#' Interpret function argument as a formula
#'
#' Helper function to simplify the common task of checking whether an argument could serve as a formula,
#' raising an error or doing the conversion.
#'
#' @param arg Variable to be interpreted
#'
#' @return A formula or an error message if the variable could not be interpreted as such
arg2fml <- function(arg){
  arg_expr <- rlang::enexpr(arg)
  if(!fml_is(arg)) rlang::abort(paste("The argument", arg_expr %>% as.character(), "needs to be a valid formula."))
  return(as_fml(arg))
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

