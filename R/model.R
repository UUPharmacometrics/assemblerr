#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
Model <- function(){
  return(
    structure(
      list(
        parameters = list(),
        compartments = list(),
        flows = list()
      ), class = "Model")
  )
}

#' @export
add_parameter <- function(o, name) UseMethod("add_parameter")

#' @export
add_parameter.Model <- function(o, name) {
  o$parameters[[name]] <-LogNormalParameter(name)
  return(o)
}

Parameter <- function(name){
  return(
    structure(
      list(
        name = name
      ),
      class = "Parameter"
    )
  )
}

LogNormalParameter <- function(...){
  parameter <- Parameter(...)
  class(parameter) <- append("LogNormalParameter", class(parameter))
  return(parameter)
}

# m <- Model() %>%
#    add_parameter("ka") %>%
#    as_nmtran()

#
# %>%
#   add_compartment("depot") %>%
#   add_compartment("central") %>%
#   add_flow(from = "depot", to = "central", equation = "-ka*C") %>%
#   as_nmtran()
#
# PKModel() %>%
#   add_fo_absorption() %>%
#   as_nmtran()

