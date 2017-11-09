#' @export
as_nm_model <- function(from) UseMethod("as_nm_model")

#' @export
as_nm_model.model <- function(from){
  create_nm_model() %>%
    convert_compartments(from) %>%
    convert_parameters(from) %>%
    convert_observations(from)
}

convert_compartments <- function(to, from) UseMethod("convert_compartments")

convert_compartments.nm_model <- function(to, from){
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

                    nm_model %>%
                      add_compartment(name = comp$name, equation = eqn)
                  })
}

convert_observations <- function(to, from) UseMethod("convert_observations")

convert_observations.nm_model <- function(to, from) {
  compartment_indicies <- to$compartments %>%
    {purrr::set_names(as.list(.$index), .$name)}

  from$observations %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  ~convert_observation_model(.x, from, .y))
}

convert_parameters <- function(to, from) UseMethod("convert_parameters")

convert_parameters.nm_model <- function(to, from){
  from$parameters %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  ~convert_parameter_model(.x, from, .y))
}

convert_parameter_model <- function(to, from, parameter){
  .parameter_conversion[[parameter$distribution]][[class(to)[1]]](to, from, parameter)
}

.parameter_conversion <- list()
.parameter_conversion[["log-normal"]] <- list()
.parameter_conversion[["normal"]] <- list()
.parameter_conversion[["log-normal"]][["nm_model"]] <- function(to, from, parameter){
    eqn <- equation(.par ~ theta[.theta]*exp(eta[.eta]))
    to %>%
      add_theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value, lbound = 0) %>%
      add_omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value) %>%
      add_parameter(name = parameter$name,
                    equation = substitute(eqn,
                                          .par = rlang::sym(parameter$name),
                                          .theta = get_by_name(., "thetas", parameter$name)$index,
                                          .eta = get_by_name(., "omegas", parameter$name)$index)
      )
  }
.parameter_conversion[["normal"]][["nm_model"]] <- function(to, from, parameter){
  eqn <- equation(.par ~ theta[.theta]*(1+eta[.eta]))
  to %>%
    add_theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value) %>%
    add_omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value) %>%
    add_parameter(name = parameter$name,
                  equation = substitute(eqn,
                                        .par = rlang::sym(parameter$name),
                                        .theta = get_by_name(., "thetas", parameter$name)$index,
                                        .eta = get_by_name(., "omegas", parameter$name)$index)
    )
}

convert_observation_model <- function(to, from, observation){
  .observation_model_conversion[[observation$model]][[class(to)[1]]](to, from, observation)
}

.observation_model_conversion <- list()
.observation_model_conversion[["additive"]] <- list()
.observation_model_conversion[["proportional"]] <- list()
.observation_model_conversion[["additive"]][["nm_model"]] <- function(to, from, obs){
  compartment_indicies <- to$compartments %>%
    {purrr::set_names(as.list(.$index), .$name)}
  ipred_eqn <- obs$equation %>%
    set_lhs(ipred)
  # replace reference to compartment amount with index
  if("A" %in% variables(ipred_eqn)){
    ipred_eqn <- substitute_indicies(ipred_eqn, "A", compartment_indicies)
  }
  to <- add_sigma(to, obs$name)
  ruv_eqn <- equation(y~ipred+eps[.eps]) %>%
    substitute(.eps = get_by_name(to, "sigmas", obs$name)$index)
  add_observation(to, obs$name, ipred_eqn, ruv_eqn)
}
.observation_model_conversion[["proportional"]][["nm_model"]] <- function(to, from, obs){
  compartment_indicies <- to$compartments %>%
  {purrr::set_names(as.list(.$index), .$name)}
  ipred_eqn <- obs$equation %>%
    set_lhs(ipred)
  # replace reference to compartment amount with index
  if("A" %in% variables(ipred_eqn)){
    ipred_eqn <- substitute_indicies(ipred_eqn, "A", compartment_indicies)
  }
  to <- add_sigma(to, obs$name)
  ruv_eqn <- equation(y~ipred+(1+eps[.eps])) %>%
    substitute(.eps = get_by_name(to, "sigmas", obs$name)$index)
  add_observation(to, obs$name, ipred_eqn, ruv_eqn)
}
