#' @export
pk_model <- function(){
  model() %>%
    add_facet("pk_components", list(type = character())) %>%
    rlang::set_attrs(class = c("pk_model", class(.)))
}

#' @export
pk_component <- function(name, type){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  if(!is.character(type)) stop("'type' needs to be a character vector")

  item("pk_components", name = name, type = type)
}
