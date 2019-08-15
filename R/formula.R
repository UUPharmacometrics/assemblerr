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
