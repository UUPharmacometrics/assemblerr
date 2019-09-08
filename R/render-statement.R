
render_expr <- function(object, opts){
  object %>%
    transform_if(.if = opts$round_vec_brackets, vec2fcall_transformer) %>%
    transform_if(.if = opts$equal_assign_op, assignment_transformer) %>%
    purrr::map_chr(function(expr) {
      deparse(expr, control = c(), width.cutoff = 200) %>%
        paste(collapse = "\n") %>%
        stringr::str_replace_all("\\s%([^%]*)%\\s", "\\1") %>%    #replace 'escaped' binary operators
        nm_if_reformatter() %>%
        {if(opts$capitalize) {
          toupper(.)
        }else{
          .
        }}
    }) %>%
    paste(collapse = "\n") %>%
    return()
}

nm_if_reformatter <- function(string){
  gsub(pattern = "if\\s\\((.*)\\)\\s(?P<then>\\{((?>[^{}]|(?P>then))*)\\})",
       x = string,
       replacement = "IF(\\1) THEN\\3ENDIF" , perl = T)
}

assignment_transformer <- function(node){
  if(rlang::is_call(node) && rlang::call_name(node) == "<-"){
    node[[1]] <- quote(`=`)
  }
  node
}

vec2fcall_transformer <- function(node){
  if(rlang::is_call(node) && rlang::call_name(node) == "["){
    node[[1]] <- node[[2]]
    node[[2]] <- NULL
  }
  node
}

transform_if <- function(l, .if,  transformer, ...){
  if(.if) return(purrr::map(l, transform_ast, transformer = transformer, ...))
  return(l)
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
