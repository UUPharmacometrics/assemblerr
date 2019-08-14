#' Make a list of declarations
#'
#' Converts each element of a list to a declaration
#'
#' @param dl list to be converted
#'
#' @return list of declaration objects or error if any element could not be converted
as_declaration_list <- function(dl){
  #declarationish <- dl %>%
  #  purrr::map_lgl(is_declarationish, parse = T)
  #if(any(!declarationish)) stop("Elements ", dl[!declarationish], " could not be interpreted as a declaration.", call. = F)
  if(is_declarationish(dl, parse = T)) return(list(as_declaration(dl)))
  return(purrr::map(dl, as_declaration))
}

is_declarationish_list <- function(dl, parse = F) {
  return(is_declarationish(dl, parse) || all(purrr::map_lgl(dl, is_declarationish)))
}

arg_as_declaration_list <- function(arg){
  arg_expr <- rlang::enexpr(arg)
  if(!is_declarationish_list(arg, parse = T)) stop("Argument '", arg_expr %>% as.character(), "' can not be interpreted as a declaration list", call. = F)
  return(as_declaration_list(arg))
}


# returns true if var1 depends on var2
depends_on <- function(var1, var2, dl) {
  to_visit <- var1
  visited <- c()
  while(!purrr::is_empty(to_visit)){
    visited <- union(visited, to_visit[1])
    to_visit <- dl %>%
      purrr::keep(~deparse(dec_get_id(.x)) == to_visit[1]) %>%
      purrr::map(~dec_vars(.x)) %>%
      unlist() %>%
      union(to_visit) %>%
      setdiff(visited)
    if(var2 %in% to_visit) return(TRUE)
  }
  return(FALSE)
}



# returns a list of indicies from the declaration list that depend on the variable
get_direct_dependants <- function(dl, variable){
  dl %>%
    purrr::map(dec_vars) %>%
    purrr::map_lgl(~ variable  %in% .x) %>%
    which()
}

# orders the provided declaration list topologically
topologic_order <- function(dl){
  # DFS (https://en.wikipedia.org/wiki/Topological_sorting)
  l <- c() # list of sorted nodes
  marked_perm <- c() # nodes completed
  marked_temp <- c() # nodes visited but not completed
  unmarked <- seq_along(dl) %>% rev() # nodes not yet visited
  while(!rlang::is_empty(unmarked)){
    i <- unmarked[1]
    ret <- topologic_visit(dl, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
    unmarked <- setdiff(unmarked, c(marked_perm, marked_temp))
  }
  return(l)
}

topologic_sort <- function(dl){
  dl[topologic_order(dl)]
}


topologic_visit <- function(dl, index, marked_perm, marked_temp, l){
  if(index %in% marked_perm) return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
  if(index %in% marked_temp) stop("Error")
  marked_temp <- c(marked_temp, index)
  # find all nodes that depend on the current node
  var <- dec_get_id(dl[[index]]) %>% deparse()
  for(i in get_direct_dependants(dl, var)){
    ret <- topologic_visit(dl, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
  }
  marked_temp <- marked_temp %>% purrr::discard(~.x == index)
  marked_perm <- c(marked_perm, index)
  l <- c(index, l)
  return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
}

# get a list of variables the equations depend on
get_external_dependencies <- function(dl){
  dl[topologic_order(dl) %>% rev()] %>% # process equations in reverse topological order
    purrr::reduce(~c(.x, dec_vars(.y)) %>% purrr::discard(function(x) x == deparse(dec_get_id(.y))) , .init = c()) # at each step add all variables from the rhs and remove variable from lhs
}
