Compartment <- function(name, volume){
  structure(
      list(
        name = name,
        volume = volume
      ),
      class = "Compartment"
    )
}

#' @export
add_compartment <- function(model, name, volume) UseMethod("add_compartment")

#' @export
add_compartment.Model <- function(model, name, volume = 1) {
  model$compartments[[name]] <- Compartment(name =  name, volume = volume)
  return(model)
}
