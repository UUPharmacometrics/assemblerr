#' General model
#'
#' \code{model()} creates the foundation for a general pharmacometric model
#'
#' This function creates a model object, the foundation for a general, software-agnostic description of a pharmacometric model.
#' The object created is an empty structure. In general, one will want to add components to the model,
#' then convert it to a software-specific model object and finally create the model code. The following components can be added
#' to a general model:
#' \itemize{
#'    \item \code{\link{parameter}}
#'    \item \code{\link{algebraic}}
#'    \item \code{\link{compartment}}
#'    \item \code{\link{flow}}
#'    \item \code{\link{observation}}
#'    \item \code{\link{meta_tag}}
#' }
#'
#' @return A general pharmacometric model
#' @export
#' @importFrom magrittr %>%
#' @examples
#' m <- model()+
#'     obs_additive(eff~emax*dose/(ed50+dose)) +
#'     prm_log_normal("emax") +
#'     prm_log_normal("ed50")
model <- function(){
  structure(list(), class = c("model", "fragment")) %>%
    add_facet("compartments", list(volume = list())) %>%
    add_facet("flows", list(from = character(), to = character(), definition = list()), name_column = FALSE) %>%
    add_facet("parameters", list(type = character(), values = list(), options = list())) %>%
    add_facet("algebraics", list(definition = list())) %>%
    add_facet("observations", list(type = character(), values = list(), options = list())) %>%
    add_facet("meta_tags", list(value = character()))
}

#' @export
print.model <- function(x,...){
  name <- NA
  title <- get_first(x, "meta_tags", name == "title")$value
  if(!is.null(title)){
    cat('assemblerr model "', title,'":\n')
  } else{
    cat('assemblerr model:\n')
  }
  prms <- x$parameters$name %>% paste(collapse = ", ")
  cat('  - Parameters: ', prms, '\n')
  obs_txt <- x$observations %>%
    dplyr::mutate(txt = paste0("    + \"", .data$name, "\" (", .data$type, ")")) %>%
    purrr::pluck("txt") %>%
    paste(collapse = "\n")
  cat('  - Observations: \n')
  cat(obs_txt, "\n")
  comp_txt <- x$compartments$name %>% paste(collapse = ", ")
  if(nrow(x$compartments)>0) cat('  - Compartments: ', comp_txt, "\n")
  alg_count <- x$algebraics %>% nrow()
  if(alg_count>0) cat('  - Algebraic relationships: ', alg_count, "\n")
}


#' Compartment
#'
#' Defines name and volume of compartment
#'
#' @seealso \code{\link{model}}
#' @param name Name of the compartment
#' @param volume Defintion of the compartment volume as a number, formula or declaration
#'
#' @return A compartment fragment
#' @export
#' @examples
#' # compartment with name "central" and volume Vc
#' comp1 <- compartment("central", volume = ~Vc)
compartment <- function(name, volume = 1){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  volume <- arg2fml(volume)
  item("compartments", name = name, volume = volume)
}

#' @export
#' @describeIn compartment Is an simple alias for compartment.
cmp <- compartment

#' Flows between compartments
#'
#' Creates a fragment describing a flow from between compartmens.
#'
#' @param from Name of the source compartment or NULL
#' @param to Name of the sink compartment or NULL
#' @param definition Declaration of the flow using the special variable A (amount in 'from' compartment) and C (concentration in 'from' compartment)
#'
#' @return A flow fragment
#' @export
#' @examples
#' f <- flow(from = "depot", to = "central", definition = ~ka*A)
flow <- function(from = NULL, to = NULL, definition){
  if(!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  definition <- arg2fml(definition)
  item("flows", from = from, to = to, definition = definition)
}



#' Create an algebraic relationship
#'
#' @param definition The definition
#'
#' @return A algebraic fragment
#' @export
algebraic <- function(definition){
  definition <- arg2fml(definition)
  if(fml_is_anonymous(definition)) stop("'definition' needs to be named")
  item("algebraics", name = fml_get_lhs(definition) %>% deparse(), definition = definition)
}

#' Create a meta tag facet
#'
#' @param name Tag name
#' @param value Tag value
#'
#' @return A meta tag facet
#' @export
meta_tag <- function(name, value){
  if(!is.character(name)) stop("'name' needs to be a character vector")
  item("meta_tags", name = name, value = value)
}

convert_compartments <- function(to, from) UseMethod("convert_compartments")

convert_observations <- function(to, from) UseMethod("convert_observations")

convert_parameters <- function(to, from) UseMethod("convert_parameters")

convert_algebraics <- function(to, from) UseMethod("convert_algebraics")

convert_meta_tags <- function(to, from) UseMethod("convert_meta_tags")

