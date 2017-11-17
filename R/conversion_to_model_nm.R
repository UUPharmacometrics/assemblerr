#' @export
as_model_nm <- function(from) UseMethod("as_model_nm")

#' @export
as_model_nm.pk_model <- function(from) {
  as_model(from) %>% as_model_nm()
}

#' @export
as_model_nm.model <- function(from){
  model_nm() %>%
    convert_compartments(from) %>%
    convert_parameters(from) %>%
    convert_observations(from) %>%
    convert_variables(from) %|+%
    data_items(c("ID", "DV", "TIME"))
}

convert_compartments.model_nm <- function(to, from){
  # replace generic amount and concentration variables in flow equations
  flows <- from$flows %>%
    purrr::transpose() %>%
    purrr::map(~purrr::update_list(.x, equation = substitute(.x$equation, C = equation(A/.vol)$rhs))) %>%
    purrr::map(~purrr::update_list(.x, equation = substitute(.x$equation, .vol = get_by_name(from, "compartments", .x$from)$volume$rhs))) %>%
    purrr::map(~purrr::update_list(.x, equation = substitute(.x$equation, A = rlang::lang("[", rlang::sym("A"), get_by_name(from, "compartments", .x$from)$index))))

  from$compartments %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(nm_model, comp){
                    outflow_eqn <- flows %>%
                      purrr::keep(~.x$from==comp$name) %>%
                      purrr::map("equation") %>%
                      purrr::reduce(`+`, .init = empty_equation())

                    inflow_eqn <- flows %>%
                      purrr::keep(~.x$to==comp$name) %>%
                      purrr::map("equation") %>%
                      purrr::reduce(`+`, .init = empty_equation())


                    eqn <- {inflow_eqn - outflow_eqn} %>%
                      set_lhs(dadt[.comp_index]) %>%
                      substitute(.comp_index = comp$index)

                    nm_model + ode(name = comp$name, equation = eqn)
                  })
}

convert_observations.model_nm <- function(to, from) {
  compartment_indicies <- to$compartments %>%
    {purrr::set_names(as.list(.$index), .$name)}

  from$observations %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model_nm, observation){
                    call_converter("observations", observation$type, from, model_nm, observation)
                  } )
}

convert_parameters.model_nm <- function(to, from){
  from$parameters %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model_nm, parameter){
                    call_converter("parameters", parameter$type, from, model_nm, parameter)
                  })
}

convert_variables.model_nm <- function(to, from){
  from$variables %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model, variable){
                    eqn <- variable$equation %>%
                      set_lhs(!!rlang::sym(variable$name))
                    model + pk_variable(variable$name, equation = eqn)
                  })
}

convert_parameter_model <- function(to, from, parameter){
  .parameter_conversion[[parameter$type]][[class(to)[1]]](to, from, parameter)
}

get_parameter_value <- function(model, parameter_name, type) get_first(model, "parameter_values", parameter1 == parameter_name | parameter2 == parameter_name, type == !!type)

add_converter(
  facet = "parameters",
  name = "log-normal",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    eqn <- equation(.par ~ theta[.theta]*exp(eta[.eta]))
    to <- to +
      theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value, lbound = 0) +
      omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value)

    to + parameter_equation(name = parameter$name,
                            equation = substitute(eqn,
                                                  .par = rlang::sym(parameter$name),
                                                  .theta = get_by_name(to, "thetas", parameter$name)$index,
                                                  .eta = get_by_name(to, "omegas", parameter$name)$index)
    )
  }
)

add_converter(
  facet = "parameters",
  name = "normal",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    eqn <- equation(.par ~ theta[.theta]*(1+eta[.eta]))
    {to +
        theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value) +
        omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value)} %>%
        {. +
            parameter_equation(name = parameter$name,
                               equation = substitute(eqn,
                                                     .par = rlang::sym(parameter$name),
                                                     .theta = get_by_name(., "thetas", parameter$name)$index,
                                                     .eta = get_by_name(., "omegas", parameter$name)$index)
            )}
  }
)

add_converter(
  facet = "parameters",
  name = "novar",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    eqn <- equation(.par ~ theta[.theta])
    {to +
        theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value)} %>%
        {. +
            parameter_equation(name = parameter$name,
                               equation = substitute(eqn,
                                                     .par = rlang::sym(parameter$name),
                                                     .theta = get_by_name(., "thetas", parameter$name)$index)
            )}
  }
)


add_converter(
  facet = "observations",
  name = "additive",
  target = "model_nm",
  converter_fn = function(to, from, obs) {
    compartment_indicies <- to$odes %>%
    {
      purrr::set_names(as.list(.$index), .$name)
    }
    ipred_eqn <- obs$equation %>%
      set_lhs(ipred)
    # replace reference to compartment amount with index
    if ("A" %in% variables(ipred_eqn)) {
      ipred_eqn <-
        substitute_indicies(ipred_eqn, "A", compartment_indicies)
    }
    to <-
      to + sigma("ruv-add", initial = get_parameter_value(from, "ruv-add", 'ruv')$value)
    ruv_eqn <- equation(y ~ ipred + eps[.eps]) %>%
      substitute(.eps = get_by_name(to, "sigmas", "ruv-add")$index)

    to + observation_equation(
      name = obs$name,
      ipred_equation = ipred_eqn,
      ruv_equation = ruv_eqn
    )
  }
)

add_converter(
  facet = "observations",
  name = "proportional",
  target = "model_nm",
  converter_fn = function(to, from, obs) {
    compartment_indicies <- to$odes %>%
    {
      purrr::set_names(as.list(.$index), .$name)
    }
    ipred_eqn <- obs$equation %>%
      set_lhs(ipred)
    # replace reference to compartment amount with index
    if ("A" %in% variables(ipred_eqn)) {
      ipred_eqn <-
        substitute_indicies(ipred_eqn, "A", compartment_indicies)
    }
    to <-
      to + sigma("ruv-prop", initial = get_parameter_value(from, "ruv-prop", 'ruv')$value)
    ruv_eqn <- equation(y ~ ipred + (1 + eps[.eps])) %>%
      substitute(.eps = get_by_name(to, "sigmas", "ruv-prop")$index)
    to + observation_equation(
      name = obs$name,
      ipred_equation = ipred_eqn,
      ruv_equation = ruv_eqn
    )
  }
)
