#' Modify AST
#'
#' This recursive function is the work-horse for all expression transformations. It takes a language node and a transformer function,
#' and applies the transformer recursivly to the node and all its child nodes.
#'
#' @param node A language node
#' @param transformer A transformer function
#' @param ... Additional arguments to the transformer function
#'
#' @return The transformed language node
#' @keywords internal
transform_ast <- function(node, transformer, ...){
  if(rlang::is_atomic(node) || rlang::is_symbol(node)) return(transformer(node, ...))
  else if(rlang::is_call(node)){
    node <- transformer(node, ...)
    if(rlang::is_call(node)) for(i  in 1:length(node)) node[[i]] <- transform_ast(node[[i]], transformer, ...)
    return(node)
  }else if(rlang::is_pairlist(node)){
    lapply(node, transform_ast, ...) %>%
      as.pairlist() %>%
      return()
  }else if(is.null(node)){
    return(node)
  } else {
    stop("Don't know how to handle type ", typeof(node),
         call. = FALSE)
  }
}


substitution_transformer <- function(node, substitutions){
  if (exists(deparse(node, width.cutoff = 500L), substitutions)) {
    node <-  substitutions[[deparse(node)]]
  }
  node
}

index_transformer <- function(node, array_name, substitutions){
  # if vector access
  if(rlang::is_call(node) && rlang::call_name(node) == "[" && node[[2]] == rlang::sym(array_name)){
    if(!exists(node[[3]], substitutions)) rlang::warn("missing_substitution", index = node[[3]])
    node[[3]] <- substitutions[[node[[3]]]]
  }
  node
}

remove_array_transformer <- function(node) {
  if (expr_is_arr(node)){
    node <- node[[2]]
  }
  node
}

expr_is_arr <- function(node) {
  rlang::is_call(node) && rlang::call_name(node) == "["
}
