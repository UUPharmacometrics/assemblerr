#' Create a new model
#'
#' @importFrom magrittr %>%
#' @return An empty model
#' @export
pkmodel <- function(){
  create_model() %>%
    add_facet("pk_components", list(type = character())) %>%
    rlang::set_attrs(class = c("pkmodel", class(.)))
}


#' @export
add_pk_component <- function(m, ...) UseMethod("add_pk_component")
#' @export
add_pk_component.pkmodel <- function(m, name, type) add_entry(m, "pk_components", list(name = name, type = type))

