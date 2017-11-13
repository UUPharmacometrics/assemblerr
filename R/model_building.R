#' Create a new model object
#'
#' @return A general model
#' @export
#' @importFrom magrittr %>%
#'
model <- function(){
  structure(list(), class = c("model", "fragment")) %>%
    add_facet("compartments", list(volume = list())) %>%
    add_facet("flows", list(from = character(), to = character(), equation = list()), name_column = F) %>%
    add_facet("parameters", list(type = character())) %>%
    add_facet("variables", list(equation = list())) %>%
    add_facet("observations", list(equation = list(), type = character())) %>%
    add_facet("parameter_values", list(parameter1 = character(), parameter2 = character(), type = character(), value = numeric()), name_column = F)
}

#' @export
`+.fragment` <- function(x, y){
  if(!is(y, "fragment")) stop("Only two fragments can be combined.")
  add_fragment(x, y)
}

#' @export
`%|+%` <- function(x,y) UseMethod("%|+%")

#' @export
`%|+%.fragment` <- function(x, y){
  if(!is(y, "fragment")) stop("Only two fragments can be combined.")
  add_fragment_unless_exists(x, y)
}



#' Create an individual entry fragment
#'
#' @param facet The name of the facet this item should be added too
#' @param ... Properties of the item
#'
#' @return A fragment consisting of just one entry
#' @export
#' @keywords internal
item <- function(facet, ...){
  list(..., index = 1) %>%
    purrr::map_if(purrr::is_list, ~list(.x)) %>%
    purrr::compact() %>%
    tibble::as_tibble() %>%
    list() %>%
    purrr::set_names(facet) %>%
    structure(class = c("fragment"))
}


#' Create a new compartment
#'
#' @param name
#' @param volume
#'
#' @return A compartment item
#' @export
compartment <- function(name, volume = 1){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is_equationish(volume)) stop ("'volume' needs to be interpretable as an equation")
  item("compartments", name = name, volume = as_equation(volume))
}

#' Create a new flow from or to compartments
#'
#' @param from Name of the source compartment or Null
#' @param to Name of the sink compartment or Null
#' @param equation An equation describing the flow
#'
#' @return A flow item
#' @export
flow <- function(from = NULL, to = NULL, equation){
  if(!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  if(!is_equationish(equation)) stop("'equation' needs to interpretable as an equation")
  item("flows", from = from, to = to, equation = as_equation(equation))
}

#' Create a new model parameter
#'
#' @param name
#' @param type
#'
#' @return A parameter fragment
#' @export
parameter <- function(name, type = "log-normal"){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is.character(type)) stop("'type' needs to be a character vector")
  item("parameters", name = name, type = type)
}


#' Create a new observation
#'
#' @param name
#' @param equation
#' @param type
#'
#' @return A parameter fragment
#' @export
observation <- function(name = "ruv", equation,  type){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is_equationish(equation)) stop("'equation' needs to be interpreatable as an equation")
  if(!is.character(type)) stop("'type' needs to be a character vector")
  item("observations", name = name, equation = as_equation(equation), type = type)
}


#' Create a new parameter_value
#'
#' @param parameter1
#' @param type
#' @param value
#' @param parameter2
#'
#' @return A parameter value fragment
#' @export
parameter_value <- function(parameter1, type, value, parameter2 = NULL){
  if(!is.character(parameter1)) stop("'parameter1' needs to be a character vector")
  if(!is.null(parameter2) && !is.character(parameter2)) stop("'parameter2' needs to be a character vector")
  item("parameter_values", parameter1 = parameter1, type = type, value = value, parameter2 = parameter2)
}
#' @export
parameter_value_table <- function(values, types){
  values %>%
    tibble::enframe(name = "parameter1") %>%
    dplyr::mutate(type = types, index = seq_len(n())) %>%
    {purrr::set_names(list(.), "parameter_values")} %>%
    structure(class = "fragment")
}

