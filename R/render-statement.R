# This function should be replaced with a custom deparser as done in the rlang package, for now it utilizes the default
# deparser and employs a mixture of AST and regex transformations to get the desired output.
#' @export
render.statement <- function(stm, opts){
  stm$expressions %>%
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
  if(rlang::is_lang(node) && rlang::lang_name(node) == "<-"){
    node[[1]] <- quote(`=`)
  }
  node
}

vec2fcall_transformer <- function(node){
  if(rlang::is_lang(node) && rlang::lang_name(node) == "["){
    node[[1]] <- node[[2]]
    node[[2]] <- NULL
  }
  node
}
