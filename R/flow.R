
Flow <- function(from, to, equation){
  #TODO: check usage of A and C when from is NULL
  structure(
    list(
      from = from,
      to = to,
      equation = equation
    ),
    class = "Flow"
  )
}

#' @export
add_flow <- function(model, from, to, equation) UseMethod("add_flow")

#' @export
add_flow.Model <- function(model, from = NULL, to = NULL, equation) {
  equation <- as_equation(equation)
  if(!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  if(!is.null(from)) {
    if(is.null(model$compartments[[from]])) stop(paste("compartment", from,  "does not exist"))
    append(model$compartments[[from]]$outflows) <- Flow(from = from, to = to, equation = equation)
  }
  if(!is.null(to)){
    if(is.null(model$compartments[[to]])) stop(paste("compartment", to,  "does not exist"))
    append(model$compartments[[to]]$inflows) <- Flow(from = from, to = to, equation = equation)
  }
  return(model)
}
