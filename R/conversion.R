#' @export
as_nm_model <- function(from) UseMethod("as_nm_model")


#' @export
as_nm_model.model <- function(from){
  nmm <- nm_model() %>%
    add_parameters(from) %>%
    add_odes(from) %>%
    add_observations(from)

  nmm +
    nm_input("id", "id")
}

