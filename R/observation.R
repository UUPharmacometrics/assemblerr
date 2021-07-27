#' @include facet.R
#' @include declaration.R

Observation <-  setClass("Observation",
         contains = "NamedFacetEntry",
         slots = c(values = "numeric"),
         prototype = prototype(facet_class = "ObservationFacet"))


ObservationFacet <- setClass("ObservationFacet",
                           contains = "NamedFacet",
                           prototype = prototype(
                             entry_class = "Observation",
                             label = "observations"
                          ))

setMethod(
  f = "check_component",
  signature = signature(x = "ObservationFacet"),
  definition = function(x, model) {
    issues <- c(
      IssueList(),
      check_for_number_of_observations(x),
      check_for_undefined_observations_variables(x, model = model)
      )
    return(issues)
  }
)

check_for_number_of_observations <- function(observation_facet) {
  if (vec_is_empty(observation_facet@entries)) {
    CriticalIssue("No observation specified")
  } else if (vec_size(observation_facet@entries) > 1) {
    CriticalIssue("More than one observation specified")
  }else{
    NULL
  }
}

check_for_undefined_observations_variables <- function(observation_facet, model){
  issues <- IssueList()
  if (!vec_is_empty(names(observation_facet@entries))) {
    obs_dcls <- purrr::map(observation_facet@entries, ~.x@prediction)
    dcl <- vec_c(!!!unname(obs_dcls))
    defined_vars <- collect_defined_variables(
      model,
      c("ParameterFacet", "InputVariableFacet", "AlgebraicFacet", "CompartmentFacet", "PkComponentFacet")
    )
    issues <- c(
      issues,
      check_for_undefined_variables(
        dcls = dcl,
        defined_vars = defined_vars,
        facet_label = "observation"
      )
    )
  }
  return(issues)
}


setMethod(
  f = "defined_variables",
  signature = "ObservationFacet",
  definition = function(x)  {
    purrr::map(x@entries, ~ObservationVariable(.x@name)) %>%
      purrr::reduce(.init = VariableList(), .f = combine)
  }
)

setMethod(
  f = "rename_variables",
  signature = "ObservationFacet",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@entries)) {
      x@entries[[i]] <- rename_variables(x@entries[[i]], variable_mapping)
    }
    return(x)
  }
)


setMethod(
  f = "compact_description",
  signature = "ObservationFacet",
  definition = function(x) {
    cdesc <- purrr::map_chr(x@entries, compact_description)
    interp("observations: {none(cdesc)}")
  }
)


ObsNormalCombined = setClass("ObsNormalCombined",
         slots = c(prediction = "assemblerr_declaration", additive_term = "logical", proportional_term = "logical"),
         prototype = prototype(additive_term = TRUE, proportional_term = TRUE),
         contains = "Observation")

setMethod(
  f = "rename_variables",
  signature = "ObsNormalCombined",
  definition = function(x, variable_mapping) {
    x@prediction <- dcl_substitute(x@prediction, rlang::syms(variable_mapping))
    return(x)
  }
)

setMethod(
  f = "description",
  signature = "ObsNormalCombined",
  definition = function(x) {
      interp("{x@name}: ", format(x@prediction))
  }
)

setMethod(
  f = "compact_description",
  signature = "ObsNormalCombined",
  definition = function(x) {
    interp("{format(x@prediction)} (combined)")
  }
)

setMethod("initialize",
          signature = "ObsNormalCombined",
          definition = function(.Object, prediction = declaration(), ...){
            callNextMethod(.Object, prediction = prediction, ...)
          })

#' Observation with combined error
#'
#' This building block declares an observation model with a combined residual error model (\eqn{y = f + f \epsilon_1 + \epsilon_2}).
#'
#' @includeRmd man/rmd/observation-model.Rmd
#'
#' @param prediction A definition of the model prediction
#' @param name A name for the observation (automatically derived if missing)
#' @param var_prop Variance of the proportional error component
#' @param var_add Variance of the additive error component
#'
#' @return A building block of type 'observation'
#'
#' @export
#' @family observation models
#' @examples
#' # additve RUV model for observing the variable WT
#' m <- model() +
#'   prm_log_normal("wt") +
#'   obs_additive(~wt)
#'
#' # EMAX dose-response model with proportional RUV
#' m2 <- model() +
#'   input_variable("dose") +
#'   prm_no_var("emax") +
#'   prm_no_var("ed50") +
#'   obs_proportional(effect~emax*dose/(ed50+dose))
obs_combined <- function(prediction, name, var_prop = 0.1, var_add = 1) {
  validate_and_create_observation(
    constructor = ObsNormalCombined,
    prediction = prediction,
    name = rlang::maybe_missing(name),
    values = c(var_prop = var_prop, var_add = var_add)
  )
}

validate_and_create_observation <- function(constructor, prediction, name, values) {
  prediction <- ui_as_declaration(prediction)
  if (rlang::is_missing(name)) name <- dcl_make_obs_names(prediction)
  assert_valid_observation_name(name)
  constructor(prediction = prediction, name = name, values = values)
}


# additive ----------------------------------------------------------------


AdditiveObservation <- setClass("AdditiveObservation",
                                contains = "ObsNormalCombined",
                                prototype = prototype(additive_term = TRUE, proportional_term = FALSE))

setMethod(
  f = "compact_description",
  signature = "AdditiveObservation",
  definition = function(x) {
    interp("{format(x@prediction)} (additive)")
  }
)

#' Observation with additive error
#'
#' This building block declares an observation model with an additive residual error model (\eqn{y = f + \epsilon_1}).
#'
#' @includeRmd man/rmd/observation-model.Rmd
#'
#' @param prediction A definition of the model prediction
#' @param name A name for the observation (automatically derived if missing)
#' @param var_add Variance of the additive error
#'
#' @return A building block of type 'observation'
#'
#' @export
#' @family observation models
#' @inherit obs_combined examples
obs_additive <- function(prediction, name, var_add = 1) {
  validate_and_create_observation(
    constructor = AdditiveObservation,
    prediction = prediction,
    name = rlang::maybe_missing(name),
    values = c(var_add = var_add, var_prop = 0)
  )
}



# proportional ------------------------------------------------------------



ProportionalObservation <- setClass("ProportionalObservation",
                                    contains = "ObsNormalCombined",
                                    prototype = prototype(additive_term = FALSE, proportional_term = TRUE))

setMethod(
  f = "compact_description",
  signature = "ProportionalObservation",
  definition = function(x) {
    interp("{format(x@prediction)} (proportional)")
  }
)


#' Observation with proportional error
#'
#' This building block declares an observation model with a proportional residual error model (\eqn{y = f + f \epsilon_1}).
#'
#' @includeRmd man/rmd/observation-model.Rmd
#'
#' @param prediction A definition of the model prediction
#' @param name A name for the observation (automatically derived if missing)
#' @param var_prop Variance of the proportional error
#'
#' @return A building block of type 'observation'
#'
#' @export
#' @family observation models
#' @inherit obs_combined examples
obs_proportional <- function(prediction, name, var_prop = 0.1) {
  validate_and_create_observation(
    constructor = ProportionalObservation,
    prediction = prediction,
    name = rlang::maybe_missing(name),
    values = c(var_prop = var_prop, var_add = 0)
  )
}


dcl_make_obs_names <- function(dcl) {
  ids <- dcl_id(dcl)
  is_null <- purrr::map_lgl(ids, is.null)
  obs_names <- as.character(ids)
  if (any(is_null)) {
    derived_names <- dcl_def(dcl)[is_null] %>%
      purrr::map_if(rlang::is_symbol, as.character) %>%
      purrr::map_if(expr_is_arr, ~paste0(.x[[2]], "_", .x[[3]]))
    is_complex_expr <- purrr::map_lgl(derived_names, purrr::negate(is.character))
    derived_names[is_complex_expr] <- paste0("obs", seq_len(sum(is_complex_expr)))
    obs_names[is_null] <- as.character(derived_names)
  }
  obs_names
}

assert_valid_observation_name <- function(name) {
  if (!is.character(name) || !is_valid_variable_name(name)) {
    rlang::abort(
      c(
        "Invalid observation name",
        i = "An observation name can contain letters and numbers and needs to start with a letter."
      )
    )
  }
}
