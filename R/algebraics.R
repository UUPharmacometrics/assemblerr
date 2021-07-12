#' @include facet.R
#' @include declaration.R
#' @include nm_model.R
Algebraic <- setClass("Algebraic",
                      slots = c(definition = "assemblerr_declaration"),
                      contains = "NamedFacetEntry",
                      prototype = prototype(facet_class = "AlgebraicFacet"))

setMethod(
  f = "description",
  signature = "Algebraic",
  definition = function(x) {
    format(x@definition)
  }
)



setMethod(f = "initialize",
          signature = "Algebraic",
          definition = function(.Object, definition = declaration(), ...){
            callNextMethod(.Object, name = dcl_id_label(definition), definition = definition)
          })

AlgebraicFacet <- setClass("AlgebraicFacet",
                           contains = "NamedFacet",
                           prototype = prototype(entry_class = "Algebraic", label = "algebraics"))

setMethod(
  f = "compact_description",
  signature = "AlgebraicFacet",
  definition = function(x) {
    interp("algebraics: {none(names(x@entries))}")
  }
)

setMethod(
  f = "check_component",
  signature = signature(x = "AlgebraicFacet"),
  definition = function(x, model) {
    issues <- c(IssueList(),
                check_for_undefined_algebraics_variables(x, model = model))
    return(issues)
  }
)

check_for_undefined_algebraics_variables <- function(algebraics_facet, model){
  if (!vec_is_empty(names(algebraics_facet))) {
    algebraic_dcls <- purrr::map(algebraics_facet@entries, ~.x@definition)
    dcl <- vec_c(!!!unname(algebraic_dcls))
    defined_vars <- collect_defined_variables(
      model = model,
      facets = c("ParameterFacet", "InputVariableFacet", "CompartmentFacet", "PkComponentFacet")
    )
    return(
      check_for_undefined_variables(
        dcls = dcl,
        defined_vars = defined_vars,
        facet_label = "algebraics"
      )
    )
  }
  return(NULL)
}


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "ANY", component = "AlgebraicFacet"),
  definition = function(target, source, component, options) {
    l <- purrr::map(component@entries, "definition") %>%
      purrr::set_names(NULL)
    if (vec_is_empty(l)) return(target)
    dcl <- vec_c(!!!l) %>%
      replace_compartment_references(target = target, source = source) %>%
      vec_sort()
    library_calls <- dcl_get_library_function_name(dcl)
    library_calls <- library_calls[!is.na(library_calls)] %>%
      sort()
    if (!vec_is_empty(library_calls)) {
      target <- purrr::map(library_calls, nm_subroutine) %>%
        purrr::reduce(`+`, .init = target)
    }
    dcl <- dcl[!dcl_is_library_function_call(dcl)]
    ode_dependent <- dcl_depends_on(dcl, "A", include_indicies = FALSE)
    pk_stms <- dcl[!ode_dependent] %>%
      as_statement() %>%
      nm_pk()
    error_stms <- dcl[ode_dependent] %>%
      as_statement() %>%
      nm_error()
    target +
      pk_stms +
      error_stms
  }
)

setMethod(
  f = "defined_variables",
  signature = "AlgebraicFacet",
  definition = function(x) {
    purrr::map(x@entries, ~AlgebraicVariable(.x@name)) %>%
      purrr::reduce(.init = VariableList(), .f = combine)
  }
)

setMethod(
  f = "rename_variables",
  signature = "AlgebraicFacet",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@entries)) {
      x@entries[[i]]@definition <- dcl_substitute(x@entries[[i]]@definition, rlang::syms(variable_mapping))
      x@entries[[i]]@name <- dcl_id_label(x@entries[[i]]@definition)
    }
    names(x@entries) <- names(x)
    return(x)
  }
)



#' Algebraic relationship
#'
#' This building block defines a model variable as a function of other variables.
#'
#' Algebraic relationships are equations where one variable is defined as a function of multiple other variables. assemblerr
#' uses R formulas to implement these equations. For example, the Emax dose response model
#' \deqn{effect=emax*dose/(ed50 + dose)}
#' could be declared as
#'
#' ```
#'     algebraic(effect~emax*dose/(ed50+dose))
#' ```
#'
#' where the tilde `~` replaced the equal sign `=` in the definition.
#'
#' @param definition A definition of the model variable
#' @examples
#' m <- model() +
#'   input_variable("dose") +
#'   prm_log_normal("emax", 10, 0.3) +
#'   prm_no_var("ed50", 5) +
#'   algebraic(effect~emax*dose/(ed50+dose)) +
#'   obs_additive(~effect)
#' @md
#' @export
algebraic <- function(definition){
  definition <- as_declaration(definition)
  vec_assert(definition, ptype = declaration())
  vec_chop(definition) %>%
    purrr::map(Algebraic) %>%
    purrr::reduce(`+`)
  #Algebraic(definition = definition)
}

