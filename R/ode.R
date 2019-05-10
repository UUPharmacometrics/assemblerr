add_odes <- function(to, from){
  # replace generic amount and concentration variables in flow equations

  flows <- from$flows %>%
    purrr::transpose() %>%
    purrr::map(function(flow){
      new_def <- flow$definition
      if("C" %in% dec_vars(flow$definition)){
        volume <- get_by_name(from, "compartments", flow$from)$volume %>%
          dec_set_id(vol)
        conc <- as_declaration(C ~ A/vol) %>%
          dec_subs(volume)
        new_def <- dec_subs(new_def, conc)
      }
      cmp_index <- get_by_name(from, "compartments", flow$from)$index
      cmp <- declaration(A, A[!!(cmp_index)])
      new_def <- dec_subs(new_def, cmp)
      purrr::list_modify(flow, definition = new_def)
    })

  from$compartments %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model, cmp){

                    outflow_eqn <- flows %>%
                      purrr::keep(~.x$from==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(dec_combine, op = "+", .init = declaration())

                    inflow_eqn <- flows %>%
                      purrr::keep(~!is.na(.x$to)&&.x$to==cmp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(dec_combine, op = "+", .init = declaration())


                    eqn <- dec_combine(inflow_eqn, outflow_eqn, op = "-", identifier = dadt[!!(cmp$index)])

                    model + get_ode_item(model, cmp$name, eqn)
                  })
}

get_ode_item <- function(model, name, dec, ...) UseMethod("get_ode_item")

get_ode_item.nm_model <- function(model, name, dec, ...) nm_des(name, as_statement(dec))

