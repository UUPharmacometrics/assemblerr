
#' `declaration` constructor
#'
#' The internal constructor for a declaration vector. The user-facing version is `declaration`.
#'
#' The arguments `identifier` and `definition` are lists of R expressions. For `identifier` only symbols, array expressions
#' (e.g., theta[1]), or NULL are permitted.
#'
#' @param identifier List of expressions
#' @param definition List of expressions
#'
#' @return An assemblerr_declaration object
#' @export
#' @keywords internal
new_declaration <- function(identifier = list(), definition = list()){
  vctrs::vec_assert(identifier, ptype = list())
  vctrs::vec_assert(definition, ptype = list())
  identifier <- rlang::set_names(identifier, NULL)
  definition <- rlang::set_names(definition, NULL)
  if (!rlang::is_empty(definition) && all(!purrr::map_lgl(definition, rlang::is_expression)))
    rlang::abort(message = "`definition` must be an expression")
  if (!rlang::is_empty(identifier) && all(!purrr::map_lgl(identifier, rlang::is_expression)))
    rlang::abort(message = "`identifier` must be an expression")
  if (!all(purrr::map_lgl(identifier, is_valid_lhs)))
    rlang::abort("The identifiers need to be symbols or array expressions")
  vctrs::new_rcrd(list(identifier = identifier, definition = definition),
                  class = "assemblerr_declaration")
}

setOldClass("assemblerr_declaration")


#' Declaration
#'
#' A declaration is the mathematical definition of a set of variables. It is the lowest level building block for a model
#' in `assemblerr`. A declaration consists of the variable names being declared (the identifiers) and their definition. The
#' `declaration` function allows the specification of a declaration using `R` formulae.
#'
#' @param ... List of R formulae with a single symbol on the left-hand side and a valid R expression on the right
#'
#' @return A declaration vector
#' @export
#'
#' @examples
#' d <- declaration(cl~theta[1]+eta[1])
#' d2 <- declaration(v=theta[2]*exp(eta[2]))
declaration <- function(...){
  dots <- rlang::exprs(...)
  lhs <- purrr::map_if(dots, rlang::is_formula, rlang::f_lhs, .else = ~NULL)
  new_identifier <- purrr::imap(lhs, function(x, y) {
    if (is.null(x)) {
      if (y == "") {
        NULL
      }else{
        rlang::sym(y)
      }
    }else{
     x
    }
  })
  if (!all(purrr::map_lgl(new_identifier, is_valid_lhs)))
    rlang::abort("The left-hand side of each formula needs to be a symbol or an array expression")
  definition <- purrr::map_if(dots, rlang::is_formula, rlang::f_rhs)
  return(new_declaration(new_identifier, definition))
}


#' @export
is_declaration <- function(x) {
  return(inherits(x, "assemblerr_declaration"))
}

#' @export
as_declaration <- function(x) UseMethod("as_declaration")

as_declaration.assemblerr_declaration <- function(x) x

as_declaration.formula <- function(x) declaration(!!x)

as_declaration.numeric <- function(x) new_declaration(vec_rep(list(NULL), vec_size(x)), definition = as.list(x))

as_declaration.character <- function(x) new_declaration(vec_rep(list(NULL), vec_size(x)), rlang::syms(x))

#' @export
as.list.assemblerr_declaration <- function(x, ...) {
  lbls <- purrr::map_if(dcl_id(x), ~!is.null(.x), deparse, .else = ~"")
  rlang::set_names(dcl_def(x), lbls)
}


#' Get declaration fields
#'
#' These function get identifier or definition fields of a declaration.
#'
#' @param dcl A declaration
#'
#' @return An expression
#' @export
#' @keywords internal
dcl_id <- function(dcl){
  vctrs::field(dcl, "identifier")
}
#' @rdname dcl_id
dcl_def <- function(dcl){
  vctrs::field(dcl, "definition")
}


#' Set declaration fields
#'
#' @param dcl A declaration
#' @param value An expression or a list of expressions
#'
#' @return The modified declaration
#' @export
#'
#' @examples
#' d <- declaration(cl~theta*exp(eta))
#' dcl_id(d) <- quote(v)
`dcl_id<-` <- function(dcl, value){
  vec_assert(dcl, ptype = declaration())
  if (rlang::is_expression(value)) value <- list(value)
  ids <- vec_recycle(value, size = vec_size(dcl))
  new_declaration(identifier = ids, definition = dcl_def(dcl))
}


dcl_id_label <- function(dcl, null_value = NA_character_){
  labels <- purrr::map_chr(dcl_id(dcl), rlang::expr_text)
  if (!is.null(null_value)) labels[labels == "NULL"] <- null_value
  labels
}

dcl_def_label <- function(dcl) {
  purrr::map_chr(dcl_def(dcl), rlang::expr_text)
}

dcl_vars_chr <- function(dcl, include_indicies = FALSE, include_lhs = TRUE, unique = TRUE) {
  vars <- dcl_vars(dcl, include_indicies = include_indicies, include_lhs = include_lhs, unique = unique)
  if (!unique) {
    return(purrr::map(vars, as.character))
  } else {
    as.character(vars)
  }
}

dcl_vars <- function(dcl, include_indicies = FALSE, include_lhs = TRUE, unique = TRUE){
  lhs_vars <- list()
  if (include_lhs) {
    lhs_vars <- dcl_id(dcl)
  }
  if (include_indicies) {
    rhs_vars <- purrr::map(dcl_def(dcl), find_vars_with_indicies)
  }else{
    rhs_vars <- purrr::map(dcl_def(dcl), all.vars) %>%
      purrr::map(rlang::syms)
  }
  if (unique) {
    vars <- vec_c(lhs_vars, purrr::flatten(rhs_vars))
    unique(vars)
  }else{
    vec_c(lhs_vars, rhs_vars)
  }
}


find_vars_with_indicies <- function(expr){
  if (rlang::is_call(expr) && rlang::call_name(expr) == "[") {
    results <- list(expr)
  }else if (rlang::is_call(expr)) {
    results <- list()
    for (i  in 2:length(expr)) results <- vec_c(results, find_vars_with_indicies(expr[[i]]))
  }else if (rlang::is_symbol(expr)) {
    results <- list(expr)
  } else if (rlang::is_pairlist(expr)) {
    results <- lapply(expr, find_vars_with_indicies)
  }else{
    results <- list()
  }
  return(results)
}

dcl_substitute <- function(dcl,
                           substitutions = list(),
                           .include_lhs = TRUE){
  # wrap in list to allow recycling
  substitutions <- purrr::map_if(substitutions, rlang::is_expression, ~list(.))
  args <- vec_recycle_common(dcl, !!!substitutions)
  dcl <- args[[1]]
  substitutions <- purrr::transpose(args[-1])
  id <- field(dcl, "identifier")
  if (.include_lhs) id <- purrr::map2(id, substitutions,
                                      ~transform_ast(.x, substitution_transformer, substitutions = .y))
  def <- purrr::map2(field(dcl, "definition"), substitutions,
                     ~transform_ast(.x, substitution_transformer, substitutions = .y))
  new_declaration(identifier = id,
                  definition = def)
}

#' Arithmetically combine declarations
#'
#' These functions allow to combine two declarations using addition, substraction, multiplication, or devision.
#'
#' @param dcl1 A declaration
#' @param dcl2 A declaration
#' @param lhs List of expressions for the left-hand side of the resulting declaration
#'
#' @return A declaration
#' @export
#' @describeIn dcl_add Addition of the declarations
dcl_add <- function(dcl1, dcl2, lhs = dcl_id(dcl1)){
  add <-  function(e1, e2){
    if (e1 == quote(0)) return(e2)
    if (e2 == quote(0)) return(e1)
    return(call("+", e1, e2))
  }
  combine_declarations(dcl1, dcl2, lhs, add)
}

#' @export
#' @describeIn dcl_add Substraction of the declarations
#' @examples
#' d1 <- declaration(dadt~k12*C)
#' d2 <- declaration(data~k21*C)
#' dcl_substract(d1, d2)
dcl_substract <- function(dcl1, dcl2, lhs = dcl_id(dcl1)) {
  substract <- function(e1, e2){
    if (e1 == quote(0) && e2 == quote(0)) return(quote(0))
    if (e1 == quote(0)) return(call("-", e2))
    if (e2 == quote(0)) return(e1)
    return(call("-", e1, e2))
  }
  combine_declarations(dcl1, dcl2, lhs, substract)
}

#' @export
#' @describeIn dcl_add Multiplications of the declarations
dcl_multiply <- function(dcl1, dcl2, lhs = dcl_id(dcl1)) {
  mult <-  function(e1, e2){
    if (e1 == quote(0) || e2 == quote(0)) return(quote(0))
    if (e1 == quote(1)) return(e2)
    if (e2 == quote(1)) return(e1)
    return(call("*", e1, e2))
  }
  combine_declarations(dcl1, dcl2, lhs, mult)
}

#' @export
#' @describeIn dcl_add Devision of the declarations
dcl_devide <- function(dcl1, dcl2, lhs = dcl_id(dcl1)) {
  devide <-  function(e1, e2){
    if (e1 == quote(1) && e2 == quote(1)) return(quote(1))
    if (e2 == quote(1)) return(e1)
    return(call("/", e1, e2))
  }
  combine_declarations(dcl1, dcl2, lhs, devide)
}

combine_declarations <- function(dcl1, dcl2, lhs, fn) {
  vec_assert(dcl1, declaration())
  vec_assert(dcl2, declaration())
  if (rlang::is_expression(lhs)) lhs <- list(lhs)
  dcls <- vec_recycle_common(dcl1, dcl2)
  id <- vec_recycle(lhs, vec_size(dcls[[1]]))
  def <- purrr::map2(dcl_def(dcls[[1]]), dcl_def(dcls[[2]]), .f = fn)
  new_declaration(id, def)
}


dcl_sum <- function(dcl) {
  vec_assert(dcl)
  purrr::reduce(dcl, dcl_add, .init = declaration(~0))
}

dcl_prod <- function(dcl) {
  vec_assert(dcl)
  purrr::reduce(dcl, dcl_multiply, .init = declaration(~1))
}



dcl_substitute_index <- function(dcl, array_name, substitutions){
  substitutions <- as.list(substitutions)
  id <- field(dcl, "identifier") %>%
    purrr::modify_if(~!is.null(.),
                     ~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
  def <- field(dcl, "definition") %>%
    purrr::modify(~transform_ast(.x, index_transformer, array_name = array_name, substitutions = substitutions))
  new_declaration(identifier = id,
                  definition = def)
}



#' Test if an expression is a valid LHS for a declaration
#'
#' @param expr an expression
#'
#' @return TRUE/FALSE
is_valid_lhs <- function(expr){
  contains_no_functions <- setdiff(all.names(expr, unique = T), all.vars(expr)) %>% setdiff("[") %>% rlang::is_empty()
  return(contains_no_functions)
}


#' @export
format.assemblerr_declaration <- function(x, ...){
  id_txt <- dcl_id_label(x, null_value = ".")
  def_txt <- dcl_def_label(x)
  out <- paste0("`", id_txt, " ~ ", def_txt, "`")
}

vec_ptype_abbr.assemblerr_declaration <- function(x, ...) "dcl"
vec_ptype_full.assemblerr_declaration <- function(x, ...) "declaration"

list_of_declaration <- function(...){
  list_of(..., .ptype = new_declaration())
}

#' @export
vec_proxy_equal.assemblerr_declaration <- function(x, ...){
  data_frame(identifier = dcl_id_label(x, null_value = "."),
             definition = dcl_def(x))
}

#' @export
vec_proxy_compare.assemblerr_declaration <- function(x, ...){
  proxy <- rlang::rep_along(x, 0)
  proxy[topologic_order(x)] <- seq_along(x)
  proxy
}

# return logical vector
dcl_depends_on <- function(dcl, variable_names, include_indicies = TRUE){
  dependent <- vec_rep(FALSE, vec_size(dcl))
  while (!vec_is_empty(variable_names)) {
    new_dependent <- dcl_vars_chr(dcl, include_indicies = include_indicies, include_lhs = FALSE, unique = FALSE) %>%
      purrr::map_lgl(~!vec_is_empty(intersect(variable_names, .x )))
    variable_names <- dcl_id_label(dcl[new_dependent])
    dependent <- dependent | new_dependent
  }
  dependent
}



# orders the provided declaration list topologically
topologic_order <- function(dcl){
  # DFS (https://en.wikipedia.org/wiki/Topological_sorting)
  l <- c() # list of sorted nodes
  marked_perm <- c() # nodes completed
  marked_temp <- c() # nodes visited but not completed
  unmarked <- seq_along(dcl) %>% rev() # nodes not yet visited
  while (!rlang::is_empty(unmarked)) {
    i <- unmarked[1]
    ret <- topologic_visit(dcl, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
    unmarked <- setdiff(unmarked, c(marked_perm, marked_temp))
  }
  return(l)
}

topologic_visit <- function(dcl, index, marked_perm, marked_temp, l){
  if (index %in% marked_perm) return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
  if (index %in% marked_temp) stop("Error")
  marked_temp <- c(marked_temp, index)
  # find all nodes that depend on the current node
  var <- dcl_id_label(dcl)[index]
  for (i in direct_dependants(dcl, var)) {
    ret <- topologic_visit(dcl, i, marked_perm, marked_temp, l)
    marked_perm <- ret$marked_perm
    marked_temp <- ret$marked_temp
    l <- ret$l
  }
  marked_temp <- marked_temp %>% purrr::discard(~.x == index)
  marked_perm <- c(marked_perm, index)
  l <- c(index, l)
  return(list(marked_perm = marked_perm, marked_temp = marked_temp, l = l))
}

direct_dependants <- function(dcl, variable){
  dcl_vars_chr(dcl, include_indicies = FALSE, include_lhs = FALSE, unique = FALSE) %>%
    purrr::map_lgl(~ variable  %in% .x) %>%
    which()
}

dcl_linear_in <- function(dcl, variable){
  purrr::map_lgl(dcl_def(dcl), function(expr){
    terms <- collect_multiplications(expr)
    matching <- purrr::map_lgl(terms, ~exprs_match_ignore_index(.x, variable))
    return(sum(matching) == 1)
  })
}

dcl_factor_out <- function(dcl, variable){
  new_def <- purrr::map(dcl_def(dcl), function(expr){
    terms <- collect_multiplications(expr)
    if (vec_size(terms) == 1) {
      if (expr == variable) return(quote(1))
      else return(expr)
    }
    index <- which(purrr::map_lgl(terms, ~exprs_match_ignore_index(.x, variable)))
    if (vec_is_empty(index)) return(expr)
    terms[[index]] <- NULL
    purrr::reduce(terms, ~call("*", .x, .y))
  })
  new_declaration(dcl_id(dcl), new_def)
}

dcl_collect_denominators <- function(dcl){
  new_def <- purrr::map(dcl_def(dcl), function(expr) {
    terms <- collect_multiplications(expr)
    if (vec_size(terms) == 1) return(expr)
    terms %>%
      purrr::keep(~length(.x)>1 && .x[[1]] == quote(`/`) && .x[[2]] == 1) %>%
      purrr::map(~.x[[3]]) %>%
      purrr::reduce(~call("*", .x, .y))
  })
  new_id <- vec_rep(list(NULL), vec_size(new_def))
  new_declaration(new_id, new_def)
}

dcl_discard_denominators <- function(dcl){
  new_def <- purrr::map(dcl_def(dcl), function(expr) {
    terms <- collect_multiplications(expr)
    if (vec_size(terms) == 1) return(expr)
    terms %>%
      purrr::discard(~length(.x)>1 && .x[[1]] == quote(`/`) && .x[[2]] == 1) %>%
      purrr::reduce(~call("*", .x, .y))
  })
  new_id <- vec_rep(list(NULL), vec_size(new_def))
  new_declaration(new_id, new_def)
}

collect_multiplications <- function(node){
  if (length(node) > 1 && node[[1]] == quote(`*`)) {
    return(vec_c(collect_multiplications(node[[2]]), collect_multiplications(node[[3]])))
  } else if (length(node) > 1 && node[[1]] == quote(`/`)) {
    return(vec_c(collect_multiplications(node[[2]]), list(bquote(1/.(node[[3]])))))
  } else if (node == 1) {
    return(list())
  } else {
    return(list(node))
  }
}

exprs_match_ignore_index <- function(expr1, expr2) {
  if (expr1 == expr2) return(TRUE)
  if (length(expr1) > 1 && expr1[[1]] == quote(`[`) && length(expr2) > 1 && expr2[[1]] == quote(`[`)) {
    return(expr1[[2]] == expr2[[2]])
  }
  return(FALSE)
}

dcl_discard_identities <- function(dcl) {
  identical <- purrr::map2_lgl(dcl_id(dcl), dcl_def(dcl), ~.x==.y)
  dcl[!identical]
}

dcl_create_function_call <- function(function_name, arguments = list(), identifier = NULL){
  fn_call <- rlang::call2(function_name, !!!arguments)
  new_declaration(list(identifier), list(fn_call))
}

dcl_create_library_function_call <- function(function_name, arguments = list(), identifier = NULL){
  dcl_create_function_call(paste0(".", function_name), arguments, identifier)
}

dcl_is_library_function_call <- function(dcl){
  purrr::map_lgl(dcl_def(dcl), function(node){
    if (rlang::is_call(node) && grepl("^\\.", as.character(node[[1]]))) return(TRUE)
    return(FALSE)
  })
}

dcl_get_library_function_name <- function(dcl){
  fn_name <- vec_rep(NA_character_, vec_size(dcl))
  is_call <- dcl_is_library_function_call(dcl)
  fns <- purrr::map_chr(as.list(dcl[is_call]), ~as.character(.x[[1]]))
  is_lib_call <- grepl("^.", fns)
  fns[!is_lib_call] <- NA
  fns[is_lib_call] <- substring(fns[is_lib_call], 2)
  fn_name[is_call] <- fns
  fn_name
}
