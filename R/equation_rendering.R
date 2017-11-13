#' @export
render.equation <- function(eqn, capitalize = T, round_vec_brackets = T, equal_assign_op = T){
  if(!is.null(eqn$lhs)) {
    expr <- base::substitute(lhs <-  rhs, eqn)
  }else{
    expr <- eqn$rhs
  }
  if(capitalize){
    expr <- transform_ast(expr, capitalize_transformer)
  }
  if(round_vec_brackets){
    expr <- transform_ast(expr, vec2fcall_transformer)
  }
  if(equal_assign_op){
    expr <- transform_ast(expr, assignment_transformer)
  }
  str <- deparse(expr, control = c(), width.cutoff = 200)
  if(capitalize){
    return(toupper(str))
  }else{
    return(str)
  }
}



vec2fcall_transformer <- function(node){
  if(rlang::is_lang(node) && rlang::lang_name(node) == "["){
    node[[1]] <- node[[2]]
    node[[2]] <- NULL
  }
  node
}

capitalize_transformer <- function(node){
  if(rlang::is_symbol(node)){
    node <- toupper(node) %>%
      rlang::sym()
  }
  node
}

assignment_transformer <- function(node){
  if(rlang::is_lang(node) && rlang::lang_name(node) == "<-"){
    node[[1]] <- quote(`=`)
  }
  node
}
