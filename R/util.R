sym <- function(...) rlang::sym(do.call(paste0, args = list(...)))


name_index_map <- function(x){
  names(x) %>%
    {purrr::set_names(seq_along(.),.)}
}

generate_permutations <- function(indicies){
  if (vec_size(indicies) == 1) return(indicies)
  permutations <- matrix(nrow = 0, ncol = length(indicies))
  for (i in seq_along(indicies)) {
    permutations <- rbind(permutations,
                          cbind(indicies[i], generate_permutations(indicies[-i])))
  }
  return(permutations)
}

permutation_matrix <- function(index){
  purrr::map(index, ~as.numeric(seq_along(index)==.x)) %>%
    do.call(cbind, args = .)
}


is_valid_variable_name <- function(name){
  grepl(x = name, pattern = "^[a-zA-Z][0-9a-zA-Z_]*$")
}

#' Convert facet class names to labels
#'
#' @param names Character vector of facet class names
#'
#' @return Character vector with class names translated to labels
#'
#' @examples
#' assemblerr:::facet_names_to_labels(c("ParameterFacet", "InputVariableFacet"))
#' @keywords internal
facet_names_to_labels <- function(names) {
  names[grepl("Facet$", names)] <- names[grepl("Facet$", names)] %>%
    gsub("Facet$","", x = .) %>%
    split_camelcase() %>%
    tolower()
  names
}

split_camelcase <- function(x) gsub("\\B([A-Z])", " \\1", x = x)

#' Perform string interpolation with pluralization
#'
#' @param ... Character vectors to interpolate
#' @param .envir Environment for lookup
#'
#' @return A character vector
#'
#' @examples
#' x <- 1:10
#' assemblerr:::interp("x has elements {x}")
#' @keywords internal
interp <- function(..., .envir = parent.frame()) {
  res <- cli::pluralize(..., .envir = .envir)
  as.character(res)
}

#'@importFrom cli qty

none <- function(x) {
  if (vec_is_empty(x)) return(cli::col_grey("none"))
  x
}

sq <- function(x) {
  return(paste0("'",x,"'"))
}

