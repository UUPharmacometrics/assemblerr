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
      if("C" %in% dec_vars(flow$definition)){
        volume <- get_by_name(model, "compartments", flow$from)$volume %>%
          dec_set_id(vol)
        conc <- as_declaration(C ~ A/vol) %>%
          dec_subs(volume)
        new_def <- dec_subs(new_def, conc)
      }
      cmp_index <- get_by_name(model, "compartments", flow$from)$index
      cmp <- declaration(A, A[!!(cmp_index)])
      new_def <- dec_subs(new_def, cmp)
      purrr::list_modify(flow, definition = new_def)
    })

  model$compartments %>%
    purrr::transpose() %>%
    purrr::map(function(cmp){

                    outflow_eqn <- flows %>%
                      purrr::keep(~.x$from==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(dec_combine, op = "+", .init = declaration())

                    inflow_eqn <- flows %>%
                      purrr::keep(~!is.na(.x$to)&&.x$to==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(dec_combine, op = "+", .init = declaration())


                    eqn <- dec_combine(inflow_eqn, outflow_eqn, op = "-", identifier = dadt[!!(cmp$index)])

                    return(eqn)
                  }) %>%
    purrr::set_names(model$compartments$name)
}

get_ode_item <- function(model, name, dec, ...) UseMethod("get_ode_item")

get_ode_item.nm_model <- function(model, name, dec, ...) nm_des(name, as_statement(dec))

