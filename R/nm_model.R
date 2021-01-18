#' @include facet.R
#' @include statement.R
#' @include generics.R

NmModel <- setClass("NmModel",
                    contains = "GenericModel")

setMethod(
  f = "initialize",
  signature = "NmModel",
  definition = function(.Object, ...) {
    callNextMethod(.Object,
                   facets = list(NmInputEntryFacet(),
                                 NmDataFacet(),
                                 NmSubroutinesFacet(),
                                 NmCompartmentFacet(),
                                 NmPkCodeFacet(),
                                 NmDesCodeFacet(),
                                 NmErrorCodeFacet(),
                                 NmEstimationStepFacet(invisible = TRUE),
                                 NmCovarianceStepFacet(invisible = TRUE),
                                 NmThetaParameterFacet(),
                                 NmOmegaParameterFacet(),
                                 NmSigmaParameterFacet()),
                   ...)
  }
)


setMethod(
  f = "render_component",
  signature = c(x = "NmModel"),
  definition = function(x, ...) {
    is_pred <- vec_is_empty(x@facets[['NmCompartmentFacet']]@entries)
    is_general_advan <- any(paste0("advan", c(5,6,7,8,9,13,14,15)) %in% names(x@facets[["NmSubroutinesFacet"]]))
    if (is_pred || !is_general_advan) {
      x@facets[['NmCompartmentFacet']] <- NULL
      x@facets[['NmDesCodeFacet']] <- NULL
    }
    vec_c(
      glue::glue("$PROBLEM"),
      purrr::map(x@facets, render_component, is_pred = is_pred) %>%
        purrr::discard(vec_is_empty) %>%
        glue::as_glue()
    )

  }
)

#' NONMEM model
#'
#' \code{nm_model()} creates the foundation for a NONMEM model
#'
#' This function creates a NONMEM model object, a software-specific version of the general
#' \code{\link{model}}. Like for the general model,this function only creates the empty
#' base object which then needs to be filled with components before it can be rendered. The
#' following components can be added
#' to a NONMEM model:
#' \itemize{
#'    \item \code{\link{nm_problem}}
#'    \item \code{\link{nm_input}}
#'    \item \code{\link{nm_pk}}
#'    \item \code{\link{nm_des}}
#'    \item \code{\link{nm_error}}
#'    \item \code{\link{nm_theta}}
#'    \item \code{\link{nm_omega}}
#'    \item \code{\link{nm_sigma}}
#' }
#'
#' @return An nm_model
#' @export
#'

nm_model <- function(){
  NmModel()
}


NmRecordOption <- setClass(
  "NmRecordOption",
  slots = c(name = "character", value = "character"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmRecord")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmRecordOption"),
  definition = function(x, ...) {
    glue::glue("{toupper(x@name)}={toupper(x@value)}")
  }
)

NmRecord <- setClass("NmRecord",
                     slots = c(name = "character", invisible = "logical"),
                      contains = "NamedFacet",
                      prototype = prototype(entry_class = "NmRecordOption"))

setMethod(
  f = "initialize",
  signature = "NmRecord",
  definition = function(.Object, options = list(), invisible = FALSE, ...) {
    .Object <- callNextMethod(.Object, invisible = invisible, ...)
    .Object@entries <- options %>%
      purrr::compact() %>%
      purrr::imap(~new(.Object@entry_class, name = .y, value = .x))
    .Object
  }
)

setMethod(
  f = "render_component",
  signature = c(x = "NmRecord"),
  definition = function(x, ...) {
    if (x@invisible) return(character())
    options <- ""
    if (!vec_is_empty(x@entries)) options <- glue::glue_collapse(purrr::map_chr(x@entries, render_component), sep = " ")
    glue::glue(
      "${toupper(x@name)}", options, .sep = " "
    )
  }
)

# $INPUT ------------------------------------------------------------------


NmInputEntry <- setClass(
  "NmInputEntry",
  slots = c(type = "character"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmInputEntryFacet")
)

NmInputEntryFacet <- setClass("NmInputEntryFacet",
                         contains = "NamedFacet",
                         prototype = prototype(entry_class = "NmInputEntry"))

setMethod(
  f = "render_component",
  signature = c(x = "NmInputEntryFacet"),
  definition = function(x, ...) {
    glue::glue("$INPUT {rcrds}\n", rcrds = paste(toupper(names(x)), collapse = " "))
  }
)

#' @export
nm_input <- function(name, type = NA_character_){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  NmInputEntry(name = name, type = type)
}

# $DATA ------------------------------------------------------------------


NmData <- setClass(
  "NmData",
  slots = c(path = "character"),
  contains = "FacetEntry",
  prototype = prototype(facet_class = "NmDataFacet")
)

NmDataFacet <- setClass("NmDataFacet",
                        contains = "NamedFacet",
                        prototype = prototype(entry_class = "NmData"))

setMethod(
  f = "render_component",
  signature = c(x = "NmDataFacet"),
  definition = function(x, ...) {
    path <- "data.csv"
    if (!vec_is_empty(x@entries)) path <- x@entries[[1]]@path
    glue::glue("$DATA {path} IGNORE=@")

  }
)

#' @export
nm_data <- function(path){
  if (!is.character(path)) stop("'name' needs to be a character vector")
  NmData(path = path)
}




# $SUBROUTINES ------------------------

NmSubroutines <- setClass(
  "NmSubroutines",
  slots = c(tol = "integer"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmSubroutinesFacet")
)

NmSubroutinesFacet <- setClass(
  "NmSubroutinesFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "NmSubroutines")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmSubroutines"),
  definition = function(x, ...) {
    tol <- ifelse(is.na(x@tol), "", paste0("tol=",x@tol))
    toupper(paste(x@name, tol, sep = " "))
  }
)


setMethod(
  f = "render_component",
  signature = c(x = "NmSubroutinesFacet"),
  definition = function(x, ...) {
    if(vec_is_empty(x@entries)) return(character())
    purrr::map_chr(x@entries, render_component) %>%
      paste(collapse = " ") %>%
      glue::glue(
        "$SUBROUTINES ", .)
  }
)


#' @export
nm_subroutine <- function(name, tol = NA_integer_) {
  NmSubroutines(name = name, tol = tol)
}

# $MODEL ------------------------------------------------------------------



NmCompartment <- setClass(
  "NmCompartment",
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmCompartmentFacet")
)


NmCompartmentFacet <- setClass(
  "NmCompartmentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "NmCompartment")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmCompartmentFacet"),
  definition = function(x, ...) {
    glue::glue("COMP=({cmp})", cmp = toupper(names(x))) %>%
      glue::glue_collapse(sep = " ") %>%
      glue::glue(
        "$MODEL ", .)
  }
)

#' @export
nm_compartment <- function(name) {
  NmCompartment(name = name)
}

NmAbbrivatedCode <- setClass(
  "NmAbbrivatedCode",
  slots = c(statement = "assemblerr_statement"),
  contains = "FacetEntry",
  prototype = prototype(facet_class = "NmInputFacet")
)

NmAbbriviatedCodeFacet <- setClass(
  "NmAbbriviatedCodeFacet",
  contains = "Facet",
  prototype = prototype(entry_class = "NmAbbrivatedCode")
)


setMethod(
  f = "render_component",
  signature = c(x = "NmAbbriviatedCodeFacet"),
  definition = function(x, ...) {
    if (vec_is_empty(x@entries)) return(character())
    purrr::map(x@entries, "statement") %>%
      {vec_c(!!!.)} %>%
      render_component()
  }
)

# $PK ---------------------------------------------------------------------



NmPkCode <- setClass(
  "NmPkCode",
  contains = "NmAbbrivatedCode",
  prototype = prototype(facet_class = "NmPkCodeFacet")
)

NmPkCodeFacet <- setClass(
  "NmPkCodeFacet",
  contains = "NmAbbriviatedCodeFacet",
  prototype = prototype(entry_class = "NmPkCode")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmPkCodeFacet"),
  definition = function(x, is_pred, ...) {
    if (is_pred) {
      glue::glue(
        "$PRED\n",
        callNextMethod(x)
      )
    }else{
      glue::glue(
        "$PK\n",
        callNextMethod(x)
      )
    }
  }
)


#' Create model code entry
#'
#' @param statement Code statement
#'
#' @return A facet
#' @export
nm_pk <- function(statement){
  NmPkCode(statement = statement)
}


# $DES --------------------------------------------------------------------



NmDesCode <- setClass(
  "NmDesCode",
  contains = "NmAbbrivatedCode",
  prototype = prototype(facet_class = "NmDesCodeFacet")
)

NmDesCodeFacet <- setClass(
  "NmDesCodeFacet",
  contains = "NmAbbriviatedCodeFacet",
  prototype = prototype(entry_class = "NmDesCode")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmDesCodeFacet"),
  definition = function(x, is_pred, ...) {
    if (is_pred || vec_is_empty(x@entries)) {
      callNextMethod(x)
    }else{
      glue::glue(
        "$DES\n",
        callNextMethod(x)
      )
    }
  }
)


#' @export
nm_des <- function(statement){
  NmDesCode(statement = statement)
}

# $ERROR ------------------------------------------------------------------



NmErrorCode <- setClass(
  "NmErrorCode",
  contains = "NmAbbrivatedCode",
  prototype = prototype(facet_class = "NmErrorCodeFacet")
)

NmErrorCodeFacet <- setClass(
  "NmErrorCodeFacet",
  contains = "NmAbbriviatedCodeFacet",
  prototype = prototype(entry_class = "NmErrorCode")
)


setMethod(
  f = "render_component",
  signature = c(x = "NmErrorCodeFacet"),
  definition = function(x, is_pred, ...) {
    if (is_pred) {
      callNextMethod(x)
    }else{
      glue::glue(
        "$ERROR\n",
        callNextMethod(x)
      )
    }
  }
)


#' @export
nm_error <- function(statement){
  NmErrorCode(statement = statement)
}


# $ESTIMATION -------------------------------------------------------------



NmEstimationStepOption <- setClass(
  "NmEstimationStepOption",
  contains = "NmRecordOption",
  prototype = prototype(facet_class = "NmEstimationFacet")
)


NmEstimationStepFacet <- setClass(
  "NmEstimationStepFacet",
  contains = "NmRecord",
  prototype = prototype(entry_class = "NmEstimationStepOption", name = "estimation")
)

#' @export
nm_estimation <- function(method = "cond inter"){
  NmEstimationStepFacet(options = list(method = method))
}

# $COV --------------------------------------------------------------

NmCovarianceStepOption <- setClass(
  "NmCovarianceStepOption",
  contains = "NmRecordOption",
  prototype = prototype(facet_class = "NmCovarianceStepFacet")
)


NmCovarianceStepFacet <- setClass(
  "NmCovarianceStepFacet",
  contains = "NmRecord",
  prototype = prototype(entry_class = "NmCovarianceStepOption", name = "covariance")
)

#' @export
nm_covariance <- function(print = 'E', matrix = NULL){
  NmCovarianceStepFacet(options = list(print = print, matrix = matrix))
}

# $THETA ------------------------------------------------------------------



NmThetaParameter <- setClass(
  "NmThetaParameter",
  slots = c(initial = "numeric", lbound = "numeric", ubound = "numeric"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmThetaParameterFacet")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmThetaParameter"),
  definition = function(x, ...) {
    glue::glue("$THETA ({x@lbound}, {x@initial}, {x@ubound}) ; POP_{toupper(x@name)}")
  }
)


NmThetaParameterFacet <- setClass(
  "NmThetaParameterFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "NmThetaParameter")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmThetaParameterFacet"),
  definition = function(x, ...) {
    glue::glue_collapse(purrr::map_chr(x@entries, render_component), sep = "\n")
  }
)


#' Create facet for initial values
#'
#' @param name Parameter name
#' @param initial Initial value
#' @param lbound Lower bound
#' @param ubound Upper bound
#'
#' @return A NONMEM Theta parameter
#' @export
nm_theta <- function(name, initial = 1.0, lbound = -Inf, ubound = Inf){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  NmThetaParameter(name = name, initial = initial, lbound = lbound, ubound = ubound)
}


# $OMEGA ------------------------------------------------------------------



NmOmegaParameter <- setClass(
  "NmOmegaParameter",
  slots = c(initial = "numeric"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmOmegaParameterFacet")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmOmegaParameter"),
  definition = function(x, ...) {
    glue::glue("$OMEGA {x@initial}; IIV_{toupper(x@name)}")
  }
)

NmOmegaParameterFacet <- setClass(
  "NmOmegaParameterFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "NmOmegaParameter")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmOmegaParameterFacet"),
  definition = function(x, ...) {
    glue::glue_collapse(purrr::map_chr(x@entries, render_component), sep = "\n")
  }
)



#' @export
nm_omega <- function(name, initial = 0.1){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  NmOmegaParameter(name = name, initial = initial)
}


# $SIGMA ------------------------------------------------------------------



NmSigmaParameter <- setClass(
  "NmSigmaParameter",
  slots = c(initial = "numeric"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "NmSigmaParameterFacet")
)

setMethod(
  f = "render_component",
  signature = c(x = "NmSigmaParameter"),
  definition = function(x, ...) {
    glue::glue("$SIGMA {x@initial}; RUV_{toupper(x@name)}")
  }
)


NmSigmaParameterFacet <- setClass(
  "NmSigmaParameterFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "NmSigmaParameter")
)


setMethod(
  f = "render_component",
  signature = c(x = "NmSigmaParameterFacet"),
  definition = function(x, ...) {
    glue::glue_collapse(purrr::map_chr(x@entries, render_component), sep = "\n")
  }
)


#' @export
nm_sigma <- function(name, initial = 0.1){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  NmSigmaParameter(name = name, initial = initial)
}







