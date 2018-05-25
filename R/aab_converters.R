converters <- list()


add_converter <- function(facet, name, target, converter_fn) {
  if(length(name)>1) name <- paste0(name, collapse = "::")
  fragment <- item(facet, name = name, target = target, fn = converter_fn)
  converters <<- add_fragment(converters, fragment)
  invisible()
}

get_converter <- function(facet, name, target){
  if(length(name)>1) name <- paste0(name, collapse = "::")
  get_first(converters, facet, name == !!name, target == !!target)$fn
}

call_converter <- function(facet, name, from, to, fragment) {
  get_converter(facet, name, class(to)[1])(to, from, fragment)
}

#' @export
list_converters <- function(query = "", target = NULL){
  matches <- converters %>%
    purrr::map(dplyr::select_at, c("name", "target")) %>%
    purrr::imap(~tibble::tibble(facet = .y, data = list(.x))) %>%
    dplyr::bind_rows() %>%
    tidyr::unnest() %>%
    dplyr::filter(grepl(query, name)|grepl(query, facet))
  if(!is.null(target)) matches <- matches %>%  dplyr::filter(target == !!(target))
  return(matches)
}

#' @export
lc <- list_converters
