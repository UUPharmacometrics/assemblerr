#' @export
render.declaration <- function(object, opts){
  test_allowed_funs(object, opts$allowed_functions)
  if(is_anonymous(object)){
    expr <- dec_get_def(object)
  }else{
    expr <- base::substitute(identifier <- definition, object)
  }
  transform_cases2if(expr) %>%
    transform_if(.if = !rlang::is_empty(opts$function_subtitutions), functions_transformer, opts$function_subtitutions) %>%
    transform_if(.if = opts$round_vec_brackets, vec2fcall_transformer) %>%
    transform_if(.if = opts$equal_assign_op, assignment_transformer) %>%
    purrr::map_chr(function(expr) {
      deparse(expr, control = c(), width.cutoff = 200) %>%
        stringr::str_replace_all("\\s%([^%]*)%\\s", "\\1") %>%    #replace 'escaped' binary operators
        {if(opts$capitalize) {
          toupper(.)
        }else{
          .
        }}
    }) %>%
    paste(collapse = "\n") %>%
    return()
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

vec2fcall_transformer <- function(node){
  if(rlang::is_lang(node) && rlang::lang_name(node) == "["){
    node[[1]] <- node[[2]]
    node[[2]] <- NULL
  }
  node
}

assignment_transformer <- function(node){
  if(rlang::is_lang(node) && rlang::lang_name(node) == "<-"){
    node[[1]] <- quote(`=`)
  }
  node
}

functions_transformer <- function(node, substitutions){
  # if function call and replacement contains vector variable
  if(rlang::is_lang(node) && rlang::lang_name(node) %in% names(substitutions)){
    node[[1]] <-  purrr::pluck(substitutions, deparse(node[[1]])) %>% rlang::sym()
  }
  node
}



transform_cases2if <- function(expr){
  if(!rlang::is_call(expr) && rlang::call_name(expr)!="<-") stop("function expects an assignment")

   # 1. extract all cases calls from the definition
  cases_calls <- extract_cases_calls(expr)

  if(rlang::is_empty(cases_calls)) return(list(expr))

  case_consequences <- cases_calls %>%
    purrr::map(~rlang::call_args(.x)[-1]) %>%
    purrr::modify_depth(2, ~.x[[3]])

  case_conditions <- cases_calls %>%
    purrr::map(~rlang::call_args(.x)) %>%
    purrr::map(~purrr::map(.x[-1], function(case_arg) rlang::expr(!!.x[[1]]==!!case_arg[[2]])))

  # 2. generate the cross product for all case conditions and consequences
  case_consequence_combinations <- purrr::cross(case_consequences)
  case_condition_combinations <- purrr::cross(case_conditions)

  # 3. for each case consequence combination, replace cases calls with case consequences
  combined_definitions <- case_consequence_combinations %>%
    purrr::map(~purrr::reduce2(.x, cases_calls, ~transform_ast(node = ..1, call_transformer, call = ..3, replacement = ..2), .init = expr))


  # 4. for each case condition combination, generate joint condition clause
  comb <- function(c1, c2 = NULL){
    if(is.null(c2)) return(c1)
    return(rlang::expr(!!c1&&!!c2))
  }
  combined_conditions <-  case_condition_combinations %>%
    purrr::map(~purrr::reduce(.x, comb))

  final_statements <- purrr::map2(combined_conditions, combined_definitions, ~rlang::expr(if(!!.x)!!.y))
  return(final_statements)
}

call_transformer <- function(node, call, replacement){
  if(rlang::is_call(node) && length(node)==length(call) && node == call) return(replacement)
  return(node)
}

extract_cases_calls <- function(node){
  if(rlang::is_call(node) && rlang::call_name(node) == 'cases'){
    return(list(node))
  }else if(rlang::is_call(node)){
    rlang::call_args(node) %>%
      purrr::map(extract_cases_calls) %>%
      purrr::compact() %>%
      purrr::flatten() %>%
      return()
  }
}

