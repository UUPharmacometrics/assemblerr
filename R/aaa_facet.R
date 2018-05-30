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

#' Test if a model has a facet
#'
#' @param model A model
#' @param facet Name of the facet
#'
#' @return TRUE/FALSE
#' @export
#' @keywords internal
#'
has_facet <- function(model, facet){
  return(exists(facet, model))
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


#' @export
`%+!%` <- function(x,y) UseMethod("%+!%")

#' @export
`%+!%.fragment` <- function(x, y){
  if(!is(y, "fragment")) stop("Only two fragments can be combined.")
  rlang::with_handlers(
    entries_updated = rlang::inplace(~1, muffle = T),
    {
      add_fragment(x, y)
    }
  )
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
    purrr::map_if(~purrr::is_list(.x)|purrr::is_function(.x), ~list(.x)) %>%
    purrr::compact() %>%
    tibble::as_tibble() %>%
    list() %>%
    purrr::set_names(facet) %>%
    structure(class = c("fragment"))
}




#' Add a fragment to another one
#'
#' @param fragment1 Fragment to add to
#' @param fragment2 Fragment to add
#'
#' @return  A combination of fragment1 and fragment2
#' @export
add_fragment <- function(fragment1, fragment2){

  purrr::reduce(names(fragment2), .init = fragment1, function(frag, facet) {
    if(!exists(facet, frag)){
      frag[[facet]] <- fragment2[[facet]]
    }else if(exists("name", frag[[facet]])){
      # if there is no name in the second fragment, set it to "missing"
      if(!exists("name", fragment2[[facet]])) fragment2[[facet]]$name <- as.character(NA)

      # determine all entries that will not change and update index column
      unchanged <- dplyr::anti_join(frag[[facet]], fragment2[[facet]], by = "name") %>%
        dplyr::arrange(index) %>%
        dplyr::mutate(index = seq_len(n()))
      # generate index for updated or added entries
      changed <- dplyr::mutate(fragment2[[facet]], index = seq_len(n())+nrow(unchanged))
      # determined added and updated entries
      added <- dplyr::anti_join(changed, frag[[facet]], by = "name")
      updated <- dplyr::semi_join(changed, frag[[facet]], by = "name")
      # warn if entries will be updated
      if(nrow(updated)!=0) rlang::warning_cnd("entries_updated", .msg = paste("Entries", paste(updated$name, collapse = ", "), "in facet", facet, "have been replaced"), .mufflable = T)
      # combine and sort by index
      frag[[facet]] <- dplyr::bind_rows(unchanged, added, updated) %>%
        dplyr::arrange(index)
    }else{
      frag[[facet]] <- dplyr::bind_rows(frag[[facet]], fragment2[[facet]])
    }
    frag
  })
}

add_fragment_unless_exists <- function(fragment1, fragment2){
  purrr::reduce(names(fragment2), .init = fragment1, function(frag, facet) {
    if(!exists(facet, frag)){
      frag[[facet]] <- fragment2[[facet]]
    }else if(exists("name", frag[[facet]])){
      # determined entries not contained in the fragment
      added <- dplyr::anti_join(fragment2[[facet]], frag[[facet]], by = "name") %>%
        dplyr::mutate(index = seq_len(n()) + nrow(frag[[facet]]))
      # combine and sort by index
      frag[[facet]] <- dplyr::bind_rows(frag[[facet]], added) %>%
        dplyr::arrange(index)
    }else{
      frag[[facet]] <- dplyr::bind_rows(frag[[facet]], fragment2[[facet]])
    }
    frag
  })
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
  get_first(model, facet, name == !!name)
}

get_first <- function(model, facet, ...){
  if(!exists(facet, model)) stop("Facet '", facet, "' not found in the model")
  dplyr::filter(model[[facet]], ...) %>%
    dplyr::slice(1) %>%
    purrr::transpose() %>%
    purrr::flatten()
}
