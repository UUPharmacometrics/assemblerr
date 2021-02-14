TreeDescription <- setClass("TreeDescription",
                            contains = "character",
                            slots = c(children = "list"))

setMethod(f = "initialize",
          signature = "TreeDescription",
          definition = function(.Object, description = character(), children = NULL){
            .Object <- callNextMethod(.Object,
                           description)
            .Object@children <- purrr::map(children, as, "TreeDescription")
            .Object
          })

setMethod(f = "show",
          signature = "TreeDescription",
          definition = function(object) {
            show_levels <- function(tree, level = 0) {
              tabs <- strrep("\t", level)
              cat(paste0(tabs, tree, "\n"))
              purrr::walk(tree@children, show_levels, level = level + 1)
            }
            show_levels(object)
          })

print_tdesc_as_tree <- function(tree_description) {
  flatten_tree <- function(tree) {
    if (rlang::is_empty(tree@children)) return(data_frame(parent = as.character(tree), children = list(character())))
    own <- data_frame(parent = as.character(tree), children = list(purrr::map_chr(tree@children, as.character)))
    children <- purrr::map(unname(tree@children), flatten_tree)
    return(vec_c(own, !!!children))
  }
  flatten_tree(tree_description) %>%
    cli::tree() %>%
    print()
}

print_tdesc_as_list <- function(tree_description) {
  print_recursive <- function(tree) {
    outer <- cli::cli_ul()
    purrr::walk(tree@children, function(x){
      cli::cli_li(x)
      print_recursive(x)
    })
    cli::cli_end(outer)
  }
  cli::cli_text(tree_description)
  print_recursive(tree_description)
}

print_tdesc_as_simple_list <- function(tree_description, skip_root = FALSE) {
  format_recursive <- function(tree, level = 0) {
    tabs <- strrep("  ", level)
    str <- character()
    if (skip_root && level == 0) {
      root <- character()
    } else {
      root <- paste0(tabs, tree)
    }
    vec_c(
      root,
      purrr::map(tree@children, format_recursive, level = level + 1) %>% purrr::flatten_chr()
    )
  }
  cli::cat_line(format_recursive(tree_description))
}

print_type_header <- function(type) {
  cat(
    cli::style_italic(
      cli::col_grey("# an assemblerr ",type)
    ),
  "\n")
}

#' Print tree description in compact format
#'
#' @param tree_description A TreeDescription object
#' @param type type label to print to console
#' @param show which child nodes of the root to print or 'all' to print all
#' @param child_type type label for child elements
#' @param skip_root whether the root node should be printed
#'
#' @importFrom methods as
#'
#' @return The function prints to the console
print_shortened_tree_description <- function(tree_description,
                                             type = "building block",
                                             show = 'all',
                                             child_type = "entries",
                                             skip_root = TRUE) {
  print_type_header(type)
  tree_description <- as(tree_description, "TreeDescription")
  n_before <- length(tree_description@children)
  if (show[1] != "all") {
    tree_description@children[!names(tree_description@children) %in% show] <- NULL
  }
  n_after <- length(tree_description@children)
  print_tdesc_as_simple_list(tree_description, skip_root = skip_root)
  if (n_before != n_after) {
    cat(
      cli::style_italic(
        cli::col_grey(interp("# ...{n_before - n_after} more ", child_type))
      ),
      "\n")
  }
}

print_issues_warning <- function(issues) {
  if (length(issues) > 0) {
    cat(
      cli::style_italic(
        cli::col_red(
          cli::pluralize("! {length(issues)} critical issue{?s}")
        )
      ),
      "\n")
  }
}


describe <- function(x) {
  description(x) %>%
    print_tdesc_as_tree()
}

