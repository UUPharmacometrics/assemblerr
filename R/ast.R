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
transform_ast <- function(node, transformer, ...){
  if(rlang::is_atomic(node) || rlang::is_symbol(node)) return(transformer(node, ...))
  else if(rlang::is_call(node)){
    node <- transformer(node, ...)
    if(rlang::is_call(node)) for(i  in 1:length(node)) node[[i]] <- transform_ast(node[[i]], transformer, ...)
    return(node)
  }else if(rlang::is_pairlist(node)){
    lapply(node, transform_ast, ...) %>%
      rlang::as_pairlist() %>%
      return()
  }else if(is.null(node)){
    return(NULL)
  } else {
    stop("Don't know how to handle type ", typeof(node),
         call. = FALSE)
  }
}
