
Flow <- function(from, to, equation){
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
add_flow <- function(model, from, to, equation, mass_balance) UseMethod("add_flow")

#' @export
add_flow.Model <- function(model, from, to, equation, mass_balance = T) {
  assertthat::assert_that(!is.null(model$compartments[[from]]), msg = paste("compartment", from,  "does not exist"))
  assertthat::assert_that(!is.null(model$compartments[[to]]), msg = paste("compartment", to,  "does not exist"))
  assertthat::assert_that(is.language(equation))
  if(is(equation, "formula")) equation <- equation[[2]]
  model$flows[[length(model$flows)+1]] <- Flow(from = from, to = to, equation = equation)
  if(mass_balance) {
    model$flows[[length(model$flows)+1]] <- Flow(from = to, to = from, equation = substitute(-x, list(x = equation)))
  }
  return(model)
}
