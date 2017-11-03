#' Add new facet to model
#'
#' @param model A model
#' @param facet Name of the facet to add
#' @param property_def List of properties with their type
#' @param name_column Whether to add a unique name column
#'
#' @return A modified model
#' @export
#'
add_facet <- function(model, facet, property_def = list(), name_column = TRUE){
  additional_columns <- list(index = integer())
  if(name_column) additional_columns <- rlang::modify(additional_columns, name = character())
  model[[facet]] <- do.call(tibble::tibble, c(property_def, additional_columns))
  return(model)
}

#' Add an entry to a facet
#'
#' @param model A model
#' @param facet Name of the facet
#' @param entry List of property value pairs
#'
#' @return  A modified model
#' @export
add_entry <- function(model, facet, entry){
  unknown_properties <- setdiff(names(entry), names(model[[facet]]))
  if(!is_empty(unknown_properties)) rlang::warn("Properties dropped that are not part of the facet")
  entry[unknown_properties] <- NULL

  # ensure uniquness of name
  if(exists("name", model[[facet]]) && entry[["name"]] %in% model[[facet]][["name"]]) rlang::abort("Entry id needs to be unique")
  # generate index
  entry$index <- max(0, model[[facet]]$index) + 1
  model %>%
    purrr::modify_at(facet, ~bind_rows(.x, purrr::discard(entry, rlang::is_null)))
}

#' Retrieve an entry by name
#'
#' @param model A model
#' @param facet Name of the facet
#' @param name Name to retrieve
#'
#' @return List representation of the entry found or an empty list
#' @export
get_by_name <- function(model, facet, name){
  lookup <- name
  model[[facet]] %>%
  dplyr::filter(name == lookup) %>%
    dplyr::slice(1) %>%
    purrr::transpose() %>%
    purrr::flatten()
}
