#' @include observation.R

setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "ObsNormalCombined"),
  definition = function(target, source, component, options) {
    ruv_add_dcl <- declaration(~0)
    ruv_prop_dcl <- declaration(~0)
    values <- parameter_values(component)
    if (component@additive_term) {
      target <- target + nm_sigma("add", initial = values["var_add"])
      eps_index <- index_of(target@facets$NmSigmaParameterFacet, "add")
      ruv_add_dcl <- dcl_substitute(declaration(~eps[i]), c(i = eps_index))
    }
    if (component@proportional_term) {
      target <- target + nm_sigma("prop", initial = values["var_prop"])
      eps_index <- index_of(target@facets$NmSigmaParameterFacet, "prop")
      ruv_prop_dcl <- dcl_substitute(declaration(~f*eps[i]), c(i = eps_index))
    }
    ipred_dcl <- component@prediction
    if (vec_size(source@facets$CompartmentFacet@entries) > 0) {
      ipred_dcl <- replace_compartment_references(ipred_dcl, target, source)
    }
    f <- dcl_id(ipred_dcl)
    if (is.null(f[[1]])) {
      f <- dcl_def(ipred_dcl)
      ipred_dcl <- NULL
    }
    ruv_dcl <- vec_c(declaration(y~f), ruv_add_dcl, ruv_prop_dcl) %>%
      dcl_sum() %>%
      dcl_substitute(
        list(
          f = f
        )
      )
    d <- vec_c(ipred_dcl,
               ruv_dcl)
    target <- target + nm_error(statement = as_statement(d))
    target
  }
)


replace_compartment_references <- function(dcl, target, source){
  if (any(c("C","A","dadt") %in% dcl_vars_chr(dcl))) {
    conc_dcls <- as.list(generate_concentration_substitutions(source@facets[["CompartmentFacet"]]@entries))
    compartment_indicies <- name_index_map(target@facets$NmCompartmentFacet)
    dcl <- dcl %>%
      dcl_substitute(substitutions = conc_dcls) %>%
      dcl_substitute_index("A", compartment_indicies) %>%
      dcl_substitute_index("dadt", compartment_indicies)
  }
  return(dcl)
}

replace_compartment_references2 <- function(dcl, compartment_indicies, source){
  if (any(c("C","A","dadt") %in% dcl_vars_chr(dcl))) {
    conc_dcls <- as.list(generate_concentration_substitutions(source@facets[["CompartmentFacet"]]@entries))
    dcl <- dcl %>%
      dcl_substitute(substitutions = conc_dcls) %>%
      dcl_substitute_index("A", compartment_indicies) %>%
      dcl_substitute_index("dadt", compartment_indicies)
  }
  return(dcl)
}


generate_concentration_substitutions <- function(cmps){
  names <- names(cmps)
  volume <- vec_c(!!!unname(purrr::map(cmps, "volume")))
  d <- dcl_substitute(declaration(C[name]~A[name]),
                      substitutions = list(name = names))
  dcl_devide(d, volume)
}

