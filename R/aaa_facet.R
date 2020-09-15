
#' Model facets
#'
#' @param name Name of the facet to add
#' @param property_definitions List of properties with their type
#' @param default_definitions List of properties that will be added to the facet by default with their type
#' @param id_columns Column names that together yield a unique identifier of the facet entries
#'
#' @return A facet
#' @export
#' @keywords internal
#'
#' @examples
#' f <- new_facet("theta", list(initial = numeric(), lbound = numeric(), ubound = numeric()))
new_facet <- function(facet_name = character(),
                      x = list(),
                      .id_columns = character()) {
  vec_assert(facet_name, ptype = character())
  vec_assert(.id_columns, ptype = character())
  if (!rlang::is_empty(.id_columns) && !all(.id_columns %in% names(x))) {
    rlang::abort("id_columns not present")
  }
  x <- vec_recycle_common(!!!x)
  index <- seq_len(vec_size_common(!!!x))
  x[["index"]] <- index
  new_data_frame(x, facet_name = facet_name, id_columns = .id_columns, class = "assemblerr_facet")
}

partial_facet <- function(){
  new_partial(learned = data_frame(), class = "assemblerr_partial_facet")
}

facet <- function(facet_name,
                  ...) {
  x <- rlang::dots_list(..., .named = TRUE)
  if ("name" %in% names(x)) {
    .id_columns <- "name"
  } else {
      .id_columns <- character()
  }
  new_facet(facet_name, x, .id_columns = .id_columns)
}


is_facet <- function(x){
  inherits(x, "assemblerr_facet")
}

facet_name <- function(x) {
  attr(x, "facet_name", exact = TRUE)
}

facet_id_columns <- function(x) {
  stopifnot(is_facet(x))
  attr(x, "id_columns", exact = TRUE)
}

facet_has_id_columns <- function(x){
  !rlang::is_empty(facet_id_columns(x))
}



#' @export
`+.assemblerr_fragment` <- function(x, y){
  if (!is(y, "assemblerr_fragment")) stop("Only two fragments can be combined.")
  add_fragments(x, y)
}


`%|+%` <- function(x,y) UseMethod("%|+%")


`%|+%.fragment` <- function(x, y){
  if(!is(y, "fragment")) stop("Only two fragments can be combined.")
  add_fragment_unless_exists(x, y)
}



`%+!%` <- function(x,y) UseMethod("%+!%")

`%+!%.fragment` <- function(x, y){
  if(!is(y, "fragment")) stop("Only two fragments can be combined.")
  rlang::with_handlers(
    entries_updated = rlang::calling(~1),
    {
      add_fragment(x, y)
    }
  )
}




# a fragment is a list of facets
new_fragment <- function(facets = list(), ..., class = NULL){
  l <- new_vctr(facets, ..., class = vec_c(class, "assemblerr_fragment"))
  facet_names <- purrr::map_chr(l, facet_name)
  rlang::set_names(l, facet_names)
}

fragment <- function(...){
  dots <- rlang::dots_list(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
  facets <- purrr::imap(dots, ~facet(.y, !!!.x))
  new_fragment(facets)
}

is_fragment <- function(x){
  inherits(x, "assemblerr_fragment")
}

list_facets <- function(x){
  stopifnot(is_fragment(x))
  names(x)
}

has_facet <- function(x, facet_name){
  is_fragment(x) && facet_name %in% list_facets(x)
}



#' Add a fragment to another one
#'
#' @param fragment1 Fragment to add to
#' @param fragment2 Fragment to be added
#'
#' @return  A combination of fragment1 and fragment2
#' @export
add_fragments <- function(fragment1, fragment2) {
  purrr::reduce(fragment2, .init = fragment1, function(frgmt, fct){
    if (!has_facet(frgmt, facet_name(fct))) {
      frgmt <- vec_c(frgmt, new_fragment(list(fct)))
    }else if (facet_has_id_columns(frgmt[[facet_name(fct)]])) {
      id_columns <- facet_id_columns(frgmt[[facet_name(fct)]])
      for (row in seq_len(vec_size(fct))) {
        entry <- vec_slice(fct, row)
        matching_row <- which(vec_equal(entry[,id_columns], frgmt[[facet_name(fct)]][, id_columns]))
        if (rlang::is_empty(matching_row)) {
          entry[["index"]] <- vec_size(frgmt[[facet_name(fct)]]) + 1L
          frgmt[[facet_name(fct)]] <- vec_rbind(frgmt[[facet_name(fct)]], entry)
        }else{
          frgmt[[facet_name(fct)]][["index"]][-matching_row] <- frgmt[[facet_name(fct)]][["index"]][-matching_row] - 1
          entry[["index"]] <- vec_size(frgmt[[facet_name(fct)]])
          frgmt[[facet_name(fct)]][matching_row, ] <- entry
          frgmt[[facet_name(fct)]] <- frgmt[[facet_name(fct)]][order(frgmt[[facet_name(fct)]][["index"]]), ]
        }
      }
    }else{
      fct[["index"]] <- max(0, frgmt[[facet_name(fct)]][["index"]]) + seq_len(vec_size(fct))
      frgmt[[facet_name(fct)]] <- vec_rbind(frgmt[[facet_name(fct)]], fct)
    }
    frgmt
  })
}

#' Retrieve an entry by name
#'
#' @param frgmt A fragment
#' @param facet Name of the facet
#' @param name Name to retrieve
#'
#' @return List representation of the entry found or an empty list
#' @export
get_by_name <- function(frgmt, facet, name){
  get_first(frgmt, facet, name == !!name)
}

get_first <- function(frgmt, facet, ...){
  if (!exists(facet, frgmt)) stop("Facet '", facet, "' not found in the fragment")
  dplyr::filter(frgmt[[facet]], ...)
}


get_all <- function(frgmt, facet, ...){
  if (!exists(facet, frgmt)) stop("Facet '", facet, "' not found in the fragment")
  dplyr::filter(frgmt[[facet]], ...)
}
