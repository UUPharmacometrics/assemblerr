add_odes <- function(to, from){
  # replace generic amount and concentration variables in flow equations
  generate_ode_equations(from) %>%
    purrr::imap(~get_ode_item(to, .y, .x)) %>%
    purrr::reduce(.init = to, `+`)
}

generate_ode_equations <- function(model){
  flows <- model$flows %>%
    purrr::transpose() %>%
    purrr::map(function(flow){
      new_def <- flow$definition
      if("C" %in% fml_vars(flow$definition)){
        volume <- get_by_name(model, "compartments", flow$from)$volume %>%
          fml_get_rhs()
        new_def <- fml_subs_sym(new_def, C = bquote(A/.(volume)))
      }
      cmp_index <- get_by_name(model, "compartments", flow$from)$index
      new_def <- fml_subs_sym(new_def, A = bquote(A[.(cmp_index)]))
      purrr::list_modify(flow, definition = new_def)
    })

  model$compartments %>%
    purrr::transpose() %>%
    purrr::map(function(cmp){
                    outflow_eqn <- flows %>%
                      purrr::keep(~!is.na(.x$from)&&.x$from==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(fml_combine, op = "+", .init = NULL)
                    inflow_eqn <- flows %>%
                      purrr::keep(~!is.na(.x$to)&&.x$to==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(fml_combine, op = "+", .init = NULL)
                    eqn <- fml_combine(inflow_eqn, outflow_eqn, op = "-", lhs = bquote(dadt[.(cmp$index)]))
                    return(eqn)
                  }) %>%
    purrr::set_names(model$compartments$name)
}

get_ode_item <- function(model, name, fml, ...) UseMethod("get_ode_item")

get_ode_item.nm_model <- function(model, name, fml, ...) nm_des(name, as_expr(fml))

