#' @export
render.declaration <- function(d, opts){
  test_allowed_funs(d, opts$allowed_functions)
  if(!rlang::is_empty(opts$function_subtitutions)) d <- dec_funs_subs(d, opts$function_subtitutions)
  if(is_anonymous(d)){
    expr <- dec_get_def(d)
  }else{
    expr <- base::substitute(identifier <- definition, d)
  }
  if(opts$capitalize){
    expr <- transform_ast(expr, capitalize_transformer)
  }
  if(opts$round_vec_brackets){
    expr <- transform_ast(expr, vec2fcall_transformer)
  }
  if(opts$equal_assign_op){
    expr <- transform_ast(expr, assignment_transformer)
  }
  str <- deparse(expr, control = c(), width.cutoff = 200)
  str <- stringr::str_replace_all(str, "\\s%([^%]*)%\\s", "\\1") #replace 'escaped' binary operators
  if(opts$capitalize){
    return(toupper(str))
  }else{
    return(str)
  }
}

test_allowed_funs <- function(d, allowed_functions){
  if(missing(allowed_functions) || is.null(allowed_functions)) return(TRUE)
  funs <- dec_funs(d)
  if(any(!funs %in% allowed_functions)) {

    not_allowed <- funs[!funs %in% allowed_functions]  %>%  paste(collapse = ", ")
    stop("The functions ", not_allowed, " are not allowed in the selected target", call. = F)
  }
  return(TRUE)
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
