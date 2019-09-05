add_algebraics <- function(to, from) {
  fmls <- from$algebraics %>%
    purrr::transpose() %>%
    purrr::map("definition") %>%
    purrr::map(replace_compartment_references, to = to, from = from) %>%
    fmls_topologic_sort()
  add_declarations_to_facets(to, from, fmls)
}

# function to determine whether an algebraic formula needs to go to PK, DES or ERROR
add_declarations_to_facets <- function(to, from, fmls) UseMethod("add_declarations_to_facets")

add_declarations_to_facets.nm_model <- function(to, from, fmls){
  # get lhs of ODEs
  odes <- generate_ode_equations(from)
  ode_identifiers <- purrr::map_chr(odes, ~fml_get_lhs(.x) %>% deparse())
  fmls %>%
    purrr::map(function(fml){
      # name of the variable being defined
      var <- fml_get_lhs(fml) %>% deparse()
      if(!fml_depends_on(var, "A", fmls)){
        nm_pk("", as_expr(fml))
      }else if(any(purrr::map_lgl(ode_identifiers, ~fml_depends_on(.x, var, odes)))){
           nm_des("", as_expr(fml))
      }else{
          nm_error("", as_expr(fml))
      }
    }) %>%
    purrr::reduce(.init = to, `+`)
}
