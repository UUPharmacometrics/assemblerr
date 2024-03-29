#' @include facet.R
#' @include model.R
#' @include pk_model.R

setClass("PkComponent",
         slots = c(prm_names = "character"),
         contains = "NamedFacetEntry",
         prototype = prototype(facet_class = "PkComponentFacet", label = "PK component")
)

setMethod(
  f = "defined_variables",
  signature = "PkComponent",
  definition = function(x) VariableList()
)



setMethod(
  f = "initialize",
  signature = "PkComponent",
  definition = function(.Object, parameters = list(), ...) {
    callNextMethod(.Object,
                   prm_names = purrr::compact(parameters) %>%
                     purrr::map_chr("name"),
                   ...)
  }
)

setMethod(
  f = "rename_variables",
  signature = "PkComponent",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@prm_names)) {
      if (x@prm_names[i] %in% names(variable_mapping)) {
        x@prm_names[i] <- variable_mapping[x@prm_names[i]]
      }
    }
    return(x)
  }
)


PkComponentFacet <- setClass(
  "PkComponentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "PkComponent", label = "PK components")
)

setMethod(
  f = "defined_variables",
  signature = "PkComponentFacet",
  definition = function(x) {
    purrr::reduce(
      .x = x@entries,
      .f = ~combine(defined_variables(.y), .x),
      .init = VariableList()
      )
  }
)

setMethod(
  f = "rename_variables",
  signature = "PkComponentFacet",
  definition = function(x, variable_mapping) {
    for (i in seq_along(x@entries)) {
      x@entries[[i]] <- rename_variables(x@entries[[i]], variable_mapping)
    }
    names(x@entries) <- names(x)
    return(x)
  }
)


setMethod(
  f = "check_component",
  signature = signature(x = "PkComponentFacet"),
  definition = function(x, ...) {
    issues <- IssueList()
    if (!"distribution" %in% names(x@entries)) {
      issues <- c(issues, CriticalIssue("A distribution component is missing"))
    }
    if (!"elimination" %in% names(x@entries)) {
      issues <- c(issues, CriticalIssue("An elimination component is missing"))
    }
    return(issues)
  }
)

setMethod(
  f = "compact_description",
  signature = "PkComponentFacet",
  definition = function(x) {
    desc <- list(distribution = character(), elimination = character(), absorption = character())
    desc_children <- purrr::map(x@entries, compact_description)
    desc[names(desc_children)] <- desc_children
    desc <- purrr::compact(desc)
    interp("pk components: {none(desc)}")
  }
)


setClass("PkAbsorptionComponent",
         contains = "PkComponent",
         prototype = prototype(name = "absorption"))

setClass("PkDistributionComponent",
         contains = "PkComponent",
         prototype = prototype(name = "distribution"))

setClass("PkEliminationComponent",
         contains = "PkComponent",
         prototype = prototype(name = "elimination"))

# distribution 1 cmp ------------------------------------------------------


PkDistribution1Cmp <- setClass("PkDistribution1Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "defined_variables",
  signature = "PkDistribution1Cmp",
  definition = function(x) {
    compartment_names_to_defined_variables("central")
  }
)

setMethod(
  f = "description",
  signature = "PkDistribution1Cmp",
  definition = function(x) {
    interp("{x@name}: 1 compartment")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkDistribution1Cmp",
  definition = function(x) {
    "1 cmp"
  }
)


#' PK distribution 1 compartment
#'
#' This building block declares a one compartment distribution component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_vc Parameter model for the central volume of distribution
#'
#' @return A building block of type 'pk_component'
#'
#' @family distribution components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_distribution_1cmp <- function(prm_vc = prm_log_normal("vc", median = 100, var_log = 0.1)) {
  PkDistribution1Cmp(parameters = list(vc = prm_vc)) +
    prm_vc
}


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution1Cmp"),
  definition = function(target, source, component, options) {
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc']))
  }
)

# distribution 2 cmp ------------------------------------------------------


PkDistribution2Cmp <- setClass("PkDistribution2Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "defined_variables",
  signature = "PkDistribution2Cmp",
  definition = function(x) {
    compartment_names_to_defined_variables(c("central", "peripheral"))
  }
)

setMethod(
  f = "description",
  signature = "PkDistribution2Cmp",
  definition = function(x) {
    interp("{x@name}: 2 compartment")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkDistribution2Cmp",
  definition = function(x) {
    "2 cmp"
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution2Cmp"),
  definition = function(target, source, component, options) {
    dcl_q <- dcl_substitute(declaration(~q*C), substitutions = list(q = sym(component@prm_names['q'])))
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc'])) +
      compartment(name = "peripheral", volume =  as_declaration(component@prm_names['vp'])) +
      flow(from = "central", to = "peripheral", definition = dcl_q) +
      flow(from = "peripheral", to = "central", definition = dcl_q)
  }
)

#' PK distribution 2 compartments
#'
#' This building block declares a two compartment distribution component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_vc Parameter model for the central volume of distribution
#' @param prm_vp Parameter model for the peripheral volume of distribution
#' @param prm_q Parameter model for the inter-compartmental clearance
#'
#' @return A building block of type 'pk_component'
#'
#' @family distribution components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_distribution_2cmp <- function(
  prm_vc = prm_log_normal("vc", median = 100, var_log = 0.1),
  prm_vp = prm_log_normal("vp", median = 5, var_log = 0.1),
  prm_q = prm_log_normal("q", median = 50, var_log = 0.1)
) {
  PkDistribution2Cmp(
    parameters = list(
      vc = prm_vc,
      vp = prm_vp,
      q = prm_q
    )
  ) +
    prm_vc +
    prm_vp +
    prm_q
}


# distribution 3 cmp ------------------------------------------------------


PkDistribution3Cmp <- setClass("PkDistribution3Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "defined_variables",
  signature = "PkDistribution3Cmp",
  definition = function(x) {
    compartment_names_to_defined_variables(c("central", "peripheral1", "peripheral2"))
  }
)


setMethod(
  f = "description",
  signature = "PkDistribution3Cmp",
  definition = function(x) {
    interp("{x@name}: 3 compartment")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkDistribution3Cmp",
  definition = function(x) {
    "3 cmp"
  }
)



setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution3Cmp"),
  definition = function(target, source, component, options) {
    dcl_q <- declaration(~q*C)
    dcl_q1 <- dcl_substitute(dcl_q, substitutions = list(q = sym(component@prm_names['q1'])))
    dcl_q2 <- dcl_substitute(dcl_q, substitutions = list(q = sym(component@prm_names['q2'])))
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc'])) +
      compartment(name = "peripheral1", volume =  as_declaration(component@prm_names['vp1'])) +
      compartment(name = "peripheral2", volume =  as_declaration(component@prm_names['vp2'])) +
      flow(from = "central", to = "peripheral1", definition = dcl_q1) +
      flow(from = "peripheral1", to = "central", definition = dcl_q1) +
      flow(from = "central", to = "peripheral2", definition = dcl_q2) +
      flow(from = "peripheral2", to = "central", definition = dcl_q2)
  }
)

#' PK distribution 3 compartments
#'
#' This building block declares a three compartment distribution component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_vc Parameter model for the central volume of distribution
#' @param prm_vp1 Parameter model for the volume of the first peripheral compartment
#' @param prm_vp2 Parameter model for the volume of the second peripheral compartment
#' @param prm_q1 Parameter model for the inter-compartmental clearance between central and first peripheral compartment
#' @param prm_q2 Parameter model for the inter-compartmental clearance between central and second peripheral compartment
#'
#' @return A building block of type 'pk_component'
#'
#' @family distribution components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_distribution_3cmp <- function(
  prm_vc = prm_log_normal("vc", median = 100, var_log = 0.1),
  prm_vp1 = prm_log_normal("vp1", median = 5, var_log = 0.1),
  prm_vp2 = prm_log_normal("vp2", median = 5, var_log = 0.1),
  prm_q1 = prm_log_normal("q1", median = 25, var_log = 0.1),
  prm_q2 = prm_log_normal("q2", median = 25, var_log = 0.1)
) {
  PkDistribution3Cmp(
    parameters = list(
      vc = prm_vc,
      vp1 = prm_vp1,
      vp2 = prm_vp2,
      q1 = prm_q1,
      q2 = prm_q2
    )
  ) +
    prm_vc +
    prm_vp1 +
    prm_vp2 +
    prm_q1 +
    prm_q2
}



# elimination linear ------------------------------------------------------


PkEliminationLinear <- setClass("PkEliminationLinear",
         contains = "PkEliminationComponent")

setMethod(
  f = "description",
  signature = "PkEliminationLinear",
  definition = function(x) {
    interp("{x@name}: linear")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkEliminationLinear",
  definition = function(x) {
    "linear elim."
  }
)



setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkEliminationLinear"),
  definition = function(target, source, component, options) {
    dcl <- declaration(~cl*C) %>%
      dcl_substitute(list(cl = sym(component@prm_names['cl'])))
    target +
      flow(from = "central", definition = dcl)
  }
)

#' PK elimination linear
#'
#' This building block declares a linear elimination component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_cl Parameter model for the clearance
#'
#' @return A building block of type 'pk_component'
#'
#' @family elimination components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_elimination_linear <- function(prm_cl = prm_log_normal("cl", median = 50, var_log = 0.1)) {
  PkEliminationLinear(
    parameters = list(
      cl = prm_cl
    )
  ) +
    prm_cl
}


# elimination non-linear --------------------------------------------------


PkEliminationNL <- setClass("PkEliminationNL",
                            contains = "PkEliminationComponent")

setMethod(
  f = "description",
  signature = "PkEliminationNL",
  definition = function(x) {
    interp("{x@name}: nonlinear")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkEliminationNL",
  definition = function(x) {
    "nonlinear elim."
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkEliminationNL"),
  definition = function(target, source, component, options) {
    if ('vmax' %in% names(component@prm_names)) {
      dcl <- declaration(~vmax*C/(km+C)) %>%
        dcl_substitute(list(vmax = sym(component@prm_names['vmax']), km = sym(component@prm_names['km'])))
    }else{
      dcl <- declaration(~clmm*km/(km+C)) %>%
        dcl_substitute(list(clmm = sym(component@prm_names['clmm']), km = sym(component@prm_names['km'])))
    }
    target +
      flow(from = "central", definition = dcl)
  }
)

#' PK elimination nonlinear
#'
#' This building block declares a nonlinear elimination component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_clmm Parameter model for the clearance
#' @param prm_km Parameter model for KM (the half-maximal concentration)
#' @param prm_vmax Parameter model for Vmax (the maximal elimination rate)
#'
#' @return A building block of type 'pk_component'
#'
#' @family elimination components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_elimination_nl <- function(prm_clmm = prm_log_normal("clmm", median = 25, var_log = 0.1),
                              prm_km = prm_log_normal("km", median = 0.5, var_log = 0.1),
                              prm_vmax = NULL) {
  if (!is.null(prm_clmm) && !is.null(prm_vmax)) {
    prm_clmm <- NULL
    rlang::warn(
      c("Ignoring 'clmm' parameter",
        i = "Only one of 'prm_clmm' and 'prm_vmax' need to be supplied")
    )
  }
  PkEliminationNL(
    parameters = list(
      clmm = prm_clmm,
      km = prm_km,
      vmax = prm_vmax
    )
  ) +
    prm_clmm +
    prm_vmax +
    prm_km
}

# elimination linear/non-linear --------------------------------------------------


PkEliminationLinearNL <- setClass("PkEliminationLinearNL",
                            contains = "PkEliminationComponent")

setMethod(
  f = "description",
  signature = "PkEliminationLinearNL",
  definition = function(x) {
    interp("{x@name}: linear & nonlinear")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkEliminationLinearNL",
  definition = function(x) {
    "lin. + nonlin. elim."
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkEliminationLinearNL"),
  definition = function(target, source, component, options) {

    if ('vmax' %in% names(component@prm_names)) {
      dcl_nl_elim <- declaration(~vmax*C/(km+C)) %>%
        dcl_substitute(list(vmax = sym(component@prm_names['vmax']), km = sym(component@prm_names['km'])))
    }else{
      dcl_nl_elim <- declaration(~clmm*km/(km+C)) %>%
        dcl_substitute(list(clmm = sym(component@prm_names['clmm']), km = sym(component@prm_names['km'])))
    }
    dcl_lin_elim <- declaration(~cllin*C) %>%
      dcl_substitute(list(cllin = sym(component@prm_names['cllin'])))

    target +
      flow(from = "central", definition = dcl_nl_elim) +
      flow(from = "central", definition = dcl_lin_elim)
  }
)

#' PK elimination linear & nonlinear
#'
#' This building block declares a mixed linear and nonlinear elimination component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_cllin Parameter model for the linear clearance
#' @param prm_clmm Parameter model for the non-linear clearance
#' @param prm_km Parameter model for KM (the half-maximal concentration)
#' @param prm_vmax Parameter model for Vmax (the maximal elimination rate)
#'
#' @return A building block of type 'pk_component'
#'
#' @family elimination components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_elimination_linear_nl <- function(prm_cllin = prm_log_normal("cllin", median = 50, var_log = 0.1),
                                     prm_clmm = prm_log_normal("clmm", median = 25, var_log = 0.1),
                              prm_km = prm_log_normal("km", median = 0.5, var_log = 0.1),
                              prm_vmax = NULL
                              ) {
  if (!is.null(prm_clmm) && !is.null(prm_vmax)) {
    prm_clmm <- NULL
    rlang::warn(
      c("Ignoring 'clmm' parameter",
        i = "Only one of 'prm_clmm' and 'prm_vmax' need to be supplied")
    )
  }
  PkEliminationLinearNL(
    parameters = list(
      cllin = prm_cllin,
      clmm = prm_clmm,
      vmax = prm_vmax,
      km = prm_km
    )
  ) +
    prm_cllin +
    prm_clmm +
    prm_vmax +
    prm_km
}

# absorption FO -----------------------------------------------------------

PkAbsorptionFO <- setClass("PkAbsorptionFO",
                               contains = "PkAbsorptionComponent")

setMethod(
  f = "description",
  signature = "PkAbsorptionFO",
  definition = function(x) {
    interp("{x@name}: first-order")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkAbsorptionFO",
  definition = function(x) {
    "FO abs."
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionFO"),
  definition = function(target, source, component, options) {
    dcl <- declaration(ka~1/mat) %>%
      dcl_substitute(list(mat = sym(component@prm_names['mat'])))
      target +
        compartment("depot", volume = ~1) +
        flow(from = "depot", to = "central", definition = ~ka*A) +
        algebraic(dcl)
  }
)

#' PK absorption first-order
#'
#' This building block declares a first-order absorption component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_fo <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1)){
  PkAbsorptionFO(
    parameters = list(
      mat = prm_mat
    )
  ) +
    prm_mat
}

# absorption FO lagtime-----------------------------------------------------------

PkAbsorptionFOLagtime <- setClass(
  "PkAbsorptionFOLagtime",
  contains = "PkAbsorptionComponent"
)

setMethod(
  f = "description",
  signature = "PkAbsorptionFOLagtime",
  definition = function(x) {
    interp("{x@name}: first-order, lag-time ")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkAbsorptionFOLagtime",
  definition = function(x) {
    "FO abs. lag-time"
  }
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionFOLagtime"),
  definition = function(target, source, component, options) {
    dcl <- declaration(alag1~mdt, ka~1/mat) %>%
      dcl_substitute(
        list(
          mdt = sym(component@prm_names['mdt']),
          mat = sym(component@prm_names['mat'])
        )
      )
    target +
      compartment("depot", volume = ~1) +
      flow(from = "depot", to = "central", definition = ~ka*A) +
      algebraic(dcl)
  }
)

#' PK absorption first-order, lag-time
#'
#' This building block declares a first-order absorption with lag-time component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#' @param prm_mdt Parameter model for the mean delay time (MDT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_fo_lag <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1),
                                 prm_mdt = prm_log_normal("mdt", median = 0.5, var_log = 0.1)) {
  PkAbsorptionFOLagtime(
    parameters = list(
      mat = prm_mat,
      mdt = prm_mdt
    )
  ) +
    prm_mdt +
    prm_mat
}

# absorption FO transit-----------------------------------------------------------

PkAbsorptionFOTransit <- setClass(
  "PkAbsorptionFOTransit",
  slots = c(ncompartments = "numeric"),
  contains = "PkAbsorptionComponent"
)

setMethod(
  f = "description",
  signature = "PkAbsorptionFOTransit",
  definition = function(x) {
    interp("{x@name}: first-order, transit-compartments ({x@ncompartments}) ")
  }
)


setMethod(
  f = "compact_description",
  signature = "PkAbsorptionFOTransit",
  definition = function(x) {
    interp("FO abs. transit-cmps({x@ncompartments})")
  }
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionFOTransit"),
  definition = function(target, source, component, options) {
    dcl <- declaration(ktr~n/mdt, ka~1/mat) %>%
      dcl_substitute(
        list(
          mdt = sym(component@prm_names['mdt']),
          n = component@ncompartments,
          mat = sym(component@prm_names['mat'])
        )
      )
    cmp_names <- c("depot", paste0("transit", seq_len(component@ncompartments)))
    cmps <- purrr::map(cmp_names, compartment)
    flows <- purrr::map2(.x = cmp_names[-length(cmp_names)],
                           .y = cmp_names[-1],
                           ~flow(.x, .y, definition = ~ktr*A))

    purrr::reduce(cmps, `+`, .init = target) %>%
      purrr::reduce(flows, `+`, .init = . ) +
      flow(from = paste0("transit", component@ncompartments), to = "central", definition = ~ka*A) +
      algebraic(dcl)
  }
)

#' PK absorption first-order, transit compartment
#'
#' This building block declares a first-order absorption with transit compartments component for
#' a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#' @param transit_compartments Number of transit compartments
#' @param prm_mdt Parameter model for the mean delay time (MDT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_fo_transit <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1),
                                     transit_compartments = 1L,
                                     prm_mdt = prm_log_normal("mdt", median = 0.5, var_log = 0.1)) {
  PkAbsorptionFOTransit(
    ncompartments = transit_compartments,
    parameters = list(
      mat = prm_mat,
      mdt = prm_mdt
    )
  ) +
    prm_mdt +
    prm_mat
}

# absorption ZO-----------------------------------------------------------

PkAbsorptionZO <- setClass("PkAbsorptionZO",
                               contains = "PkAbsorptionComponent")

setMethod(
  f = "description",
  signature = "PkAbsorptionZO",
  definition = function(x) {
    interp("{x@name}: zero-order")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkAbsorptionZO",
  definition = function(x) {
    "ZO abs."
  }
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionZO"),
  definition = function(target, source, component, options) {
    dcl <- declaration(r1~amt/mat/2) %>%
      dcl_substitute(list(mat = sym(component@prm_names['mat'])))
    target +
      algebraic(dcl)
  }
)

#' PK absorption zero-order
#'
#' This building block declares a zero-order absorption component for
#' a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_zo <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1)) {
  PkAbsorptionZO(
    parameters = list(
      mat = prm_mat
    )
  ) +
    prm_mat
}

# absorption ZO lagtime-----------------------------------------------------------

PkAbsorptionZOLagtime <- setClass("PkAbsorptionZOLagtime",
                           contains = "PkAbsorptionComponent")

setMethod(
  f = "description",
  signature = "PkAbsorptionZOLagtime",
  definition = function(x) {
    interp("{x@name}: zero-order, lag-time")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkAbsorptionZOLagtime",
  definition = function(x) {
    "ZO abs. lag-time"
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionZOLagtime"),
  definition = function(target, source, component, options) {
    dcl <- declaration(alag1~mdt, r1~amt/mat/2) %>%
      dcl_substitute(
        list(
          mat = sym(component@prm_names['mat']),
          mdt = sym(component@prm_names['mdt'])
        )
      )
    target +
      algebraic(dcl)
  }
)

#' PK absorption zero-order, lag-time
#'
#' This building block declares a zero-order absorption with lag-time component for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#' @param prm_mdt Parameter model for the mean delay time (MDT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_zo_lag <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1),
                                 prm_mdt = prm_log_normal("mdt", median = 0.5, var_log = 0.1)) {
  PkAbsorptionZOLagtime(
    parameters = list(
      mat = prm_mat,
      mdt = prm_mdt
    )
  ) +
    prm_mat +
    prm_mdt
}

# absorption FO ZO-----------------------------------------------------------

PkAbsorptionFOZO <- setClass("PkAbsorptionFOZO",
                           contains = "PkAbsorptionComponent")

setMethod(
  f = "description",
  signature = "PkAbsorptionFOZO",
  definition = function(x) {
    interp("{x@name}: first-order, zero-order delay")
  }
)

setMethod(
  f = "compact_description",
  signature = "PkAbsorptionFOZO",
  definition = function(x) {
    "FO abs. ZO delay"
  }
)


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionFOZO"),
  definition = function(target, source, component, options) {
    dcl <- declaration(ka~1/mat, r1~amt/mdt/2) %>%
      dcl_substitute(list(mat = sym(component@prm_names['mat']),
                          mdt = sym(component@prm_names['mdt'])))
    target +
      compartment("depot", volume = ~1) +
      flow(from = "depot", to = "central", definition = ~ka*A) +
      algebraic(dcl)
  }
)

#' PK absorption first-order, zero-order delay
#'
#' This building block declares a first-order absorption with zero-order delay component
#' for a pharmacokinetic model.
#'
#' @includeRmd man/rmd/pk-component.Rmd
#'
#' @param prm_mat Parameter model for the mean absorption time (MAT)
#' @param prm_mdt Parameter model for the mean delay time (MDT)
#'
#' @return A building block of type 'pk_component'
#'
#' @family absorption components
#' @seealso [pk_model()] for the creation of PK models
#' @md
#' @export
pk_absorption_fo_zo <- function(prm_mat = prm_log_normal("mat", median = 0.5, var_log = 0.1),
                                prm_mdt = prm_log_normal("mdt", median = 0.5, var_log = 0.1)) {
  PkAbsorptionFOZO(
    parameters = list(
      mat = prm_mat,
      mdt = prm_mdt
    )
  ) +
    prm_mat +
    prm_mdt
}
