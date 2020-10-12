add_odes <- function(to, from){
  if (vec_size(from$compartments) == 0) return(to)
  cmp_names <- from$compartments$name
  ode_dcl <- generate_ode_equations(from)
  frgmt <- get_ode_item(to, cmp_names, ode_dcl)
  to + frgmt
}

generate_ode_equations <- function(model){
  flows <- model$flows %>%
    dplyr::rowwise() %>%
    dplyr::group_map(function(flow, ...){
      dcl <- flow$definition
      cmp_from <- get_by_name(model, "compartments", flow$from)
      if ("C" %in% dcl_vars_chr(flow$definition)) {
        volume <- dcl_def(cmp_from$volume)[[1]]
        dcl <- dcl_substitute(dcl, list(C = bquote(A/.(volume))))
      }
      dcl <- dcl_substitute(dcl, list(A = bquote(A[.(cmp_from$index)])))
      flow$definition  <-  dcl
      flow
    }) %>%
    dplyr::bind_rows()

  dcl_list <- model$compartments %>%
    dplyr::rowwise() %>%
    dplyr::group_map(function(cmp, ...){
      dcl_outflow <- dplyr::filter(flows, from == cmp$name) %>%
        dplyr::pull("definition") %>%
        dcl_sum()
      dcl_inflow <- dplyr::filter(flows, to == cmp$name) %>%
        dplyr::pull("definition") %>%
        dcl_sum()
      dcl_substract(dcl_inflow, dcl_outflow, lhs = bquote(dadt[.(cmp$index)]))
    }) %>%
    {vec_c(!!!.)}

  return(dcl_list)
}

get_ode_item <- function(model, name, dcl, ...) UseMethod("get_ode_item")

get_ode_item.nm_model <- function(model, name, dcl, ...) nm_des(name, as_statement(dcl))

