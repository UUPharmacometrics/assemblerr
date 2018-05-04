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
    convert_algebraics(from) %>%
    convert_meta_tags(from)
}

convert_compartments.model_nm <- function(to, from){
  # replace generic amount and concentration variables in flow equations
  flows <- from$flows %>%
    purrr::transpose() %>%
    purrr::map(function(flow){
      new_def <- flow$definition
      if("C" %in% variables(flow$definition)){
        volume <- get_by_name(from, "compartments", flow$from)$volume %>%
          set_identifier(vol)
        conc <- as_declaration(C ~ A/vol) %>%
          subs_dec(volume)
        new_def <- subs_dec(new_def, conc)
      }
      cmp_index <- get_by_name(from, "compartments", flow$from)$index
      cmp <- declaration(A, A[!!(cmp_index)])
      new_def <- subs_dec(new_def, cmp)
      purrr::list_modify(flow, definition = new_def)
    })

  from$compartments %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(nm_model, comp){

                    outflow_eqn <- flows %>%
                      purrr::keep(~.x$from==comp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(combine_dec, op = "+", .init = declaration())

                    inflow_eqn <- flows %>%
                      purrr::keep(~.x$to==comp$name) %>%
                      purrr::map("definition") %>%
                      purrr::reduce(combine_dec, op = "+", .init = declaration())


                    eqn <- combine_dec(inflow_eqn, outflow_eqn, op = "-", identifier = dadt[!!(comp$index)])

                    nm_model + ode(name = comp$name, equation = eqn)
                  })
}

convert_observations.model_nm <- function(to, from) {
  compartment_indicies <- to$compartments %>%
    {purrr::set_names(as.list(.$index), .$name)}

  to <- from$observations %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model_nm, observation){
                    call_converter("observations", observation$type, from, model_nm, observation)
                  } )
  if(nrow(from$observations)>1){  # if there are more than one observation models, a dvid variable is needed
    mapping <- from$observations %>%
    {purrr::set_names(.[["index"]], .[["name"]])}

    to <- to +
      data_item("DVID", "dvid", mapping = mapping)
  }
  to
}

convert_parameters.model_nm <- function(to, from){
  from$parameters %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model_nm, parameter){
                    call_converter("parameters", parameter$type, from, model_nm, parameter)
                  })
}

convert_algebraics.model_nm <- function(to, from){
  from$algebraics %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model, algebraic){
                    eqn <- algebraic$definition
                    model + algebraic_equation(algebraic$name, equation = eqn)
                  })
}

convert_meta_tags.model_nm <- function(to, from){
  from$meta_tags %>%
    purrr::transpose() %>%
    purrr::reduce(.init = to,
                  function(model, tag){
                    model + meta_tag(tag$name, tag$value)
                  })
}

get_parameter_value <- function(model, parameter_name, type) get_first(model, "parameter_values", parameter1 == parameter_name | parameter2 == parameter_name, type == !!type)

add_converter(
  facet = "parameters",
  name = "log-normal",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    to <- to +
      theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value, lbound = 0) +
      omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value)

    theta_index <-  get_by_name(to, "thetas", parameter$name)$index
    eta_index <- get_by_name(to, "omegas", parameter$name)$index
    eqn <- declaration(!!(parameter$name), theta[!!(theta_index)]*exp(eta[!!(eta_index)]))
    to + parameter_equation(name = parameter$name,
                            equation = eqn)
  }
)

add_converter(
  facet = "parameters",
  name = "normal",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    to <- to +
        theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value) +
        omega(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'iiv')$value)
    theta_index <-  get_by_name(to, "thetas", parameter$name)$index
    eta_index <- get_by_name(to, "omegas", parameter$name)$index
    eqn <- declaration(!!(parameter$name), theta[!!(theta_index)]*(1+eta[!!(eta_index)]))
    to + parameter_equation(name = parameter$name,
                            equation = eqn)
  }
)

add_converter(
  facet = "parameters",
  name = "novar",
  target = "model_nm",
  converter_fn = function(to, from, parameter){
    to <- to +
        theta(name = parameter$name, initial = get_parameter_value(from, parameter$name, 'typical')$value)
    theta_index <-  get_by_name(to, "thetas", parameter$name)$index
    eqn <- declaration(!!(parameter$name), theta[!!(theta_index)])
    to + parameter_equation(name = parameter$name,
                            equation = eqn)
  }
)

make_ipred_equation <- function(to, from, obs){
  compartment_indicies <- to$odes %>%
  {
    purrr::set_names(as.list(.$index), .$name)
  }
  # generate replacement rules for concentration
  conc_declarations <- from$compartments %>%
    purrr::transpose() %>%
    purrr::map(~declaration(C[!!(.x$name)], A[!!(.x$name)]/vol) %>% subs_dec(set_identifier(.x$volume, vol)))

  # define ipred, replace concentration expressions and compartment indicies
  ipred_eqn <- obs$definition %>%
    purrr::invoke(.f = subs_dec, .x = conc_declarations, d = .) %>%
    index_subs_dec("A", compartment_indicies)

  if(is_anonymous(ipred_eqn)){
    ipred_eqn <- list(set_identifier(ipred_eqn, ipred))
  }else{
    ipred_eqn <- list(ipred_eqn, declaration("ipred", !!(get_identifier(ipred_eqn))))
  }

  return(ipred_eqn)
}

add_converter(
  facet = "observations",
  name = "additive",
  target = "model_nm",
  converter_fn = function(to, from, obs) {
    ipred_eqn <- make_ipred_equation(to, from, obs)

    sigma_name <- c("ruv", obs$name, "add") %>%
      purrr::discard(~.x=="") %>%
      paste0(collapse="-")
    to <-
      to + sigma(sigma_name, initial = get_parameter_value(from, sigma_name, 'ruv')$value)

    ruv_eqn <- as_declaration(y ~ ipred + eps[.eps]) %>%
      subs_dec(declaration(.eps, !!(get_by_name(to, "sigmas", sigma_name)$index)))

    to + observation_declaration(
      name = obs$name,
      declarations = c(ipred_eqn, list(ruv_eqn))
    )
  }
)

add_converter(
  facet = "observations",
  name = "proportional",
  target = "model_nm",
  converter_fn = function(to, from, obs) {
    ipred_eqn <- make_ipred_equation(to, from, obs)
    sigma_name <- c("ruv", obs$name, "prop") %>%
      purrr::discard(~.x=="") %>%
      paste0(collapse="-")
    to <-
      to + sigma(sigma_name, initial = get_parameter_value(from, sigma_name, 'ruv')$value)
    ruv_eqn <- as_declaration(y ~ ipred + (1 + eps[.eps])) %>%
      subs_dec(declaration(.eps, !!(get_by_name(to, "sigmas", sigma_name)$index)))

    to + observation_declaration(
      name = obs$name,
      declarations = c(ipred_eqn, list(ruv_eqn))
    )
  }
)

add_converter(
  facet = "observations",
  name = "power",
  target = "model_nm",
  converter_fn = function(to, from, obs) {
    ipred_eqn <- make_ipred_equation(to, from, obs)
    sigma_name <- c("ruv", obs$name, "sig") %>%
      purrr::discard(~.x=="") %>%
      paste0(collapse="-")
    theta_name <- c("ruv", obs$name, "power") %>%
      purrr::discard(~.x=="") %>%
      paste0(collapse="-")
    to <-
      to +
      sigma(sigma_name, initial = get_parameter_value(from, sigma_name, 'ruv')$value) +
      theta(theta_name, initial = get_parameter_value(from, theta_name, 'typical')$value)
    ruv_eqn <- as_declaration(y ~ ipred + eps[.eps]*ipred^theta[.theta]) %>%
      subs_dec(declaration(.eps, !!(get_by_name(to, "sigmas", sigma_name)$index)),
               declaration(.theta, !!(get_by_name(to, "thetas", theta_name)$index)))
    to + observation_declaration(
      name = obs$name,
      declarations = c(ipred_eqn, list(ruv_eqn))
    )
  }
)
