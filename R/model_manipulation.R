
#' @importFrom magrittr %>%


#' Create a new model
#'
#' @return An empty model
#' @export
create_model <- function(){
  structure(list(), class = "model") %>%
    add_facet("compartments", list(volume = list())) %>%
    add_facet("flows", list(from = character(), to = character(), equation = list()), name_column = F) %>%
    add_facet("parameters", list(distribution = character(), parameters = list())) %>%
    add_facet("variables", list(equation = list())) %>%
    add_facet("observations", list(equation = list(), model = character()))
}


add_compartment <- function(m, ...) UseMethod("add_compartment")

#' Add compartment to model
#'
#' @param m A model
#' @param name Compartment name
#' @param volume The compartment volume
#'
#' @return A modified model
#' @export
add_compartment.model <- function(m, name, volume = 1) add_entry(m, "compartments", list(name = name, volume = list(volume)))

add_flow <- function(m, ...) UseMethod("add_flow")

#' Add flow between compartments to model
#'
#' @param m A model
#' @param from Source compartment name
#' @param to Sink compartment name
#' @param equation Equation describing the flow
#'
#' @return A modified model
#' @export
add_flow.model <- function(m, from = NULL, to = NULL, equation) add_entry(m, "flows", list(from = from, to = to, equation = list(as_equation(equation))))

add_parameter <-function(m, ...) UseMethod("add_parameter")

#' Add parameter to model
#'
#' @param m A model
#' @param name Parameter name
#' @param distribution Name of the distribution
#' @param parameters List of parameter values
#'
#' @return A modified model
#' @export
add_parameter.model <- function(m, name, distribution = "log-normal", parameters = NULL) add_entry(m, "parameters", list(name = name, distribution = distribution, parameters = parameters))

add_observation <-function(m, ...) UseMethod("add_observation")

#' Add an observation model to model
#'
#' @param m A model
#' @param name Observation model name
#' @param equation Equation describing the observation
#' @param model Observation model type
#'
#' @return A modified model
#' @export
add_observation.model <- function(m, name, equation, model = "additive") add_entry(m, "observations", list(name = name, model = model, equation = list(as_equation(equation))))


model <- create_model() %>%
  add_compartment("central") %>%
  add_compartment("depot") %>%
  add_flow("central", equation = ~cl*A) %>%
  add_flow("depot", "central", equation = ~ka*A) %>%
  add_parameter("cl") %>%
  add_observation("central", ~A["central"])

model %>%
  as_nm_model() %>%
  save_file()

