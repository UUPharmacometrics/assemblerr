add_algebraics <- function(to, from) {
  dl <- from$algebraics %>%
    purrr::transpose() %>%
    purrr::map("definition") %>%
    purrr::map(replace_compartment_references, to = to, from = from) %>%
    topologic_sort()
  add_declarations_to_facets(to, from, dl)
}

# function to determine whether an algebraic declaration needs to go to PK, DES or ERROR
add_declarations_to_facets <- function(to, from, dl) UseMethod("add_declarations_to_facets")

add_declarations_to_facets.nm_model <- function(to, from, dl){
  # get lhs of ODEs
  odes <- generate_ode_equations(from)
  ode_identifiers <- purrr::map_chr(odes, ~dec_get_id(.x) %>% deparse())
  dl %>%
    purrr::map(function(d){
      # name of the variable being defined
      var <- dec_get_id(d) %>% deparse()
      if(!depends_on(var, "A", dl)){
        nm_pk("", as_statement(d))
      }else if(any(purrr::map_lgl(ode_identifiers, ~depends_on(.x, var, odes)))){
           nm_des("", as_statement(d))
      }else{
          nm_error("", as_statement(d))
      }
    }) %>%
    purrr::reduce(.init = to, `+`)
}
