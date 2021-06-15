#' @include facet.R
#' @include nm_model.R
#' @include model.R
Compartment <- setClass(
  "Compartment",
  slots = c(volume = "assemblerr_declaration"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "CompartmentFacet", label = "compartment")
)

setMethod(
  f = "description",
  signature = "Compartment",
  definition = function(x) {
      interp("{x@name}: {format(x@volume)}")
  }
)

CompartmentFacet <- setClass(
  "CompartmentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "Compartment", label = "compartments")
)

setMethod(
  f = "check",
  signature = "CompartmentFacet",
  definition = function(x, model, ...) {
    issues <- c(
      IssueList(),
      # 1. variable names in volume definition need to be defined
      check_for_undefined_volume_variables(x, model)
    )
    return(issues)
  }
)

check_for_undefined_volume_variables <- function(compartment_facet, model) {
  if (length(compartment_facet@entries) > 0) {
    v_dcls <- purrr::map(compartment_facet@entries, ~.x@volume)
    dcl <- vec_c(!!!unname(v_dcls))
    defined_vars <-  collect_defined_variables(
      model = model,
      facets = c("ParameterFacet", "InputVariableFacet", "AlgebraicFacet")
    )
    return(
      check_for_undefined_variables(
        dcls = dcl,
        defined_vars = defined_vars,
        facet_label = "compartment"
      )
    )
  }
  return(NULL)
}


setMethod(
  f = "description",
  signature = "CompartmentFacet",
  definition = function(x) {
    interp("compartments: {none(names(x@entries))}")
  }
)

setMethod(
  f = "defined_variables",
  signature = "CompartmentFacet",
  definition = function(x) {
    compartment_names_to_defined_variables(names(x@entries))
  }
)

compartment_names_to_defined_variables <- function(cmp_names) {
  a_vars <- purrr::map(
    .x = cmp_names,
    .f = ~CompartmentAmountVariable(paste0('A["',.x, '"]'))
  )
  c_vars <- purrr::map(
    .x = cmp_names,
    .f = ~CompartmentAmountVariable(paste0('C["',.x, '"]'))
  )
  vars <- vec_c(a_vars, c_vars)
  VariableList(!!!vars)
}

Flow <- setClass(
  "Flow",
  slots = c(
    from = "character",
    to = "character",
    definition = "assemblerr_declaration"
  ),
  contains = "FacetEntry",
  prototype = prototype(facet_class = "FlowFacet", label = "flow")
)

setMethod(
  f = "description",
  signature = "Flow",
  definition = function(x) {
    interp("{x@from}{cli::symbol[['arrow_right']]}{ifelse(is.na(x@to), '<out>', x@to)}: {format(x@definition)}")
  }
)

setMethod(
  f = "compact_description",
  signature = "Flow",
  definition = function(x) {
    interp("{x@from}{cli::symbol[['arrow_right']]}{ifelse(is.na(x@to), '<out>', x@to)}")
  }
)


FlowFacet <- setClass("FlowFacet",
                             contains = "Facet",
                             prototype = prototype(entry_class = "Flow", label = "flows"))




setMethod(
  f = "compact_description",
  signature = "FlowFacet",
  definition = function(x) {
    interp("flows: {none(purrr::map_chr(x@entries, compact_description))}")
  }
)

setMethod(
  f = "check",
  signature = "FlowFacet",
  definition = function(x, model, ...) {
    issues <- c(
      IssueList(),
      # 1. to and from compartment names need to exist
      check_for_undefined_compartments(x, model@facets[['CompartmentFacet']]),
      # 2. variables used in flow definition need to be defined
      check_for_undefined_flow_variables(x, model)
    )
    return(issues)
  }
)

check_for_undefined_compartments <- function(flow_facet, compartment_facet){
  cmps <- names(compartment_facet)
  undefined_cmps <- purrr::map(flow_facet@entries, function(flow){
    ret <- c()
    if (!is.na(flow@from) & !flow@from %in% cmps) ret <- flow@from
    if (!is.na(flow@to) & !flow@to %in% cmps) ret <- vec_c(ret, flow@to)
    return(ret)
  }) %>%
    purrr::flatten_chr()
  if (!vec_is_empty(undefined_cmps)) {
    return(
      CriticalIssue(
        interp("Undefined compartment name{?s} {sq(undefined_cmps)} in flow definition")
      )
    )
  } else {
    return(NULL)
  }
}

check_for_undefined_flow_variables <- function(flow_facet, model) {
  if (length(flow_facet@entries) > 0) {
    flow_dcls <- purrr::map(flow_facet@entries, ~.x@definition)
    dcl <- vec_c(!!!unname(flow_dcls))
    defined_vars <-  collect_defined_variables(
      model = model,
      facets = c("ParameterFacet", "InputVariableFacet", "AlgebraicFacet", "CompartmentFacet"),
      additional_variables = VariableList(CompartmentAmountVariable("C"), CompartmentAmountVariable("A"))
    )
    return(
      check_for_undefined_variables(
        dcls = dcl,
        defined_vars = defined_vars,
        facet_label = "flows"
      )
    )
  }
  return(NULL)
}


#' Compartment
#'
#' Defines name and volume of a compartment
#'
#' @seealso \code{\link{model}}
#' @param name Name of the compartment
#' @param volume Defintion of the compartment volume as a number, formula or declaration
#'
#' @return A compartment building block
#' @export
#' @examples
#' # add a compartment with name "central" and volume Vc
#' m <- model() +
#'  compartment("central", volume = ~vc) +
#'  flow(~cl*C, from = "central") +
#'  prm_log_normal("cl") +
#'  prm_log_normal("vc") +
#'  obs_additive(conc~C["central"])
compartment <- function(name, volume = 1){
  if (!is.character(name) || !is_valid_variable_name(name)) {
    rlang::abort(
      c(
        "Invalid compartment name",
        i = "A compartment name can contain letters and numbers and needs to start with a letter"
      )
    )
  }
  volume <- as_declaration(volume)
  vec_assert(volume, ptype = declaration(), size = 1)
  Compartment(name = name, volume = volume)
}

#' @export
#' @describeIn compartment Is a simple alias for compartment.
cmp <- compartment

#' Flow between compartments
#'
#' This building block describes a flow between compartments.
#'
#' Flows define the connections between compartments and the equations according to which exchanges occur.
#'
#' ## Flow equations
#'
#' The first function argument is the flow equation. It is defined using R formulas that can start with the tilde `~` operator and do not
#' need to have a left-hand side (i.e., `~k0` is a valid flow definition).
#'
#' Flow equations can contains the special variables `A` and `C` which can be used to refer to the amount and concentration in the compartment specified via
#' the `from=` argument. For example, the following code creates a flow building block describing the first-order transfer from the depot to the central
#' compartment
#'
#' ```
#' flow(~ka*A, "depot", "central")
#' ```
#'
#' When the model is rendered, `A` and `C` will get replaced with the corresponding compartment reference. assemblerr will raise an error if `A` or `C` are used
#' without specifying the `from=` compartment (such as in an inflow).
#'
#' ## Compartment connections
#'
#' The connection between compartments can be specified using the `from=` and `to=` arguments of the function. Setting either `from=` or `to=` to `NA` allows
#' the definition of in and outflows without a source or sink. Setting both arguments to `NA` results in error.
#'
#' ## Conversion to differential equations
#'
#' When flows are rendered they are converted to ordinary differential equations (ODEs). The connection between compartments together with the flow equations allow
#' assemblerr to determine whether an analytic solution can be generated. This automatic optimization of differential equations can be disabled via the rendering
#' options.
#'
#' @param definition Equation describing the flow
#' @param from Name of the source compartment (NA for an inflow without source)
#' @param to Name of the sink compartment (NA for an ouflow without sink)
#'
#' @export
#' @md
#' @examples
#' # one-compartment model with first-order elimination
#' m <- model() +
#'      prm_log_normal("v") +
#'      prm_log_normal("cl") +
#'      compartment("central", volume = ~v) +
#'      flow(declaration(~cl*C), from = "central") +
#'      obs_additive(~C["central"])
#' # an analytic solution is generated
#' render(m)
#'
#' # one-compartment model with Michaelis-Menten elimination
#' m2 <- model() +
#'      prm_log_normal("v") +
#'      prm_log_normal("vmax") +
#'      prm_no_var("km") +
#'      compartment("central", volume = ~v) +
#'      flow(declaration(~vmax*C/(km+C)), from = "central") +
#'      obs_additive(~C["central"])
#'
#' # an ODE is generated
#' render(m2)
flow <- function(definition, from = NA_character_, to = NA_character_){
  if (!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  if (is.na(from) && is.na(to)) rlang::abort(c("Invalid flow definition", x = "The 'from' or 'to' compartment need to be specified"))
  definition <- as_declaration(definition)
  vec_assert(definition, ptype = declaration(), size = 1)
  if (is.na(from) && any(c("C", "A") %in% dcl_vars_chr(definition, include_indicies = TRUE, include_lhs = FALSE))) {
    rlang::abort(
      c("Invalid flow definition",
        x = "Flow definitions can not use A/C when 'from' is not specified",
        i = "A or C refer to the amount or concentration in the 'from' compartment")
    )
  }
  Flow(from = from, to = to, definition = definition)
}

