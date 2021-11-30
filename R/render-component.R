render_component2 <- function(component, template = get_default_template(component), wrap = FALSE) {
  if (is.null(template)) rlang::abort("Template can not be NULL.")
  e <- new.env()
  l <- list()
  for (s in names(getSlots(class(component)))) {
    assign(s, slot(component, s), envir = e)
    l[[s]] <- slot(component, s)
  }
  assign("self", component, envir = e)
  if (rlang::is_function(template)) template <- rlang::exec(template, !!!l)

  render_in_environment(
    template = template,
    envir = e
  )
}

render_collection <- function(component, collapse = " ", template_list = .templates) {
  map_chr(component, ~render_component2(.x, template = get_default_template(.x, template_list = template_list))) %>%
    paste(collapse = collapse)
}

get_default_template <- function(component, template_list = .templates) {
  super_classes <- extends(class(component))
  has_template <- super_classes %in% names(template_list)
  if (!any(has_template)) return(NULL)
  template_list[[super_classes[has_template][1]]]
}

render_in_environment <- function(template, envir) {
  # transformer to:
  #   - protect from length 0
  #   - allow {?'TOL='tol} syntax
  transformer <- function(text, envir) {
    prefix <- ""
    if (grepl("^\\?", text)) {
      prefix <- gsub("^\\?'(.*)'.*", "\\1", text)
      text <- gsub("^\\?'.*'", "", text)
    }
    res <- glue::identity_transformer(text, envir)
    if (rlang::is_empty(res)) {
      res <- ""
    }
    else {
      res <- paste0(prefix, res)
    }
    return(res)
  }
  tryCatch(
    glue::glue(template, .envir = envir, .transformer = transformer),
    error = function(e) {
      rlang::abort(message = paste0("Template rendering failed with error message: '", e$message, "'"))
    }
  )
}

wrap <- function(str, width = 80, exdent = 4) {
  strwrap(str, width = 80, exdent = 4) %>%
    paste(collapse = "\n")
}

ie <- function(test, yes = "", no = "") ifelse(test, yes, no)

p <- function(...) {
  l <- list(...)
  if (any(map_lgl(l, rlang::is_empty))) return("")
  return(rlang::exec(paste0,!!!l))
}
