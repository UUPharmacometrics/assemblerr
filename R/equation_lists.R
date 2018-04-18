

# returns a list of indicies from the equation list that depend on the variable
get_dependants <- function(equation_list, variable){
  equation_list %>%
    purrr::map(variables) %>%
    purrr::map_lgl(~ variable  %in% .x) %>%
    which()
}


# orders the provided equation_list topologically
topologic_order <- function(equation_list){
  # DFS (https://en.wikipedia.org/wiki/Topological_sorting)
  l <- c() # list of sorted nodes
  marked_perm <- c() # nodes completed
  marked_temp <- c() # nodes visited but not completed
  unmarked <- seq_along(equation_list) # nodes never visited
  while(!rlang::is_empty(unmarked)){
    i <- unmarked[1]
    ret <- topologic_visit(equation_list, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
    unmarked <- setdiff(unmarked, c(marked_perm, marked_temp))
  }
  return(l)
}

topologic_visit <- function(equation_list, index, marked_perm, marked_temp, l){
  if(index %in% marked_perm) return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
  if(index %in% marked_temp) stop("Error")
  marked_temp <- c(marked_temp, index)
  # find all nodes that depend on the current node
  var <- equation_list[[index]]$lhs %>% as.character()
  for(i in get_dependants(equation_list, var)){
    ret <- topologic_visit(equation_list, i, marked_perm, marked_temp, l)
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
get_external_dependencies <- function(equation_list){
  equation_list[topologic_order(equation_list) %>% rev()] %>% # process equations in reverse topological order
    purrr::reduce(~c(.x, variables(.y)) %>% purrr::discard(function(x) x == as.character(.y$lhs)) , .init = c()) # add all variables from the rhs and remove lhs
}
