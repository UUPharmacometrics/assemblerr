#' @include facet.R
#' @include model.R
#' @include pk_model.R

setClass("PkComponent",
         slots = c(prm_names = "character"),
         contains = "NamedFacetEntry",
         prototype = prototype(facet_class = "PkComponentFacet")
)

setMethod(
  f = "initialize",
  signature = "PkComponent",
  definition = function(.Object, parameters = list(), ...) {
    callNextMethod(.Object,
                   prm_names = purrr::map_chr(parameters, "name"),
                   ...)
  }
)

PkComponentFacet <- setClass(
  "PkComponentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "PkComponent")
)



setClass("PkAbsorptionRateComponent",
         contains = "PkComponent",
         prototype = prototype(name = "absorption-rate"))

setClass("PkAbsorptionDelayComponent",
         contains = "PkComponent",
         prototype = prototype(name = "absorption-delay"))

setClass("PkDistributionComponent",
         contains = "PkComponent",
         prototype = prototype(name = "distribution"))

setClass("PkEliminationComponent",
         contains = "PkComponent",
         prototype = prototype(name = "elimination"))

# distribution 1 cmp ------------------------------------------------------


PkDistribution1Cmp <- setClass("PkDistribution1Cmp",
         contains = "PkDistributionComponent")

#' @export
pk_distribution_1cmp <- function(prm_vc = prm_log_normal("vc")) {
  PkDistribution1Cmp(parameters = list(vc = prm_vc)) +
    prm_vc
}


setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution1Cmp"),
  definition = function(target, source, component) {
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc']))
  }
)

# distribution 2 cmp ------------------------------------------------------


PkDistribution2Cmp <- setClass("PkDistribution2Cmp",
         contains = "PkDistributionComponent")

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution2Cmp"),
  definition = function(target, source, component) {
    dcl_q <- dcl_substitute(declaration(~q*C), substitutions = list(q = sym(component@prm_names['q'])))
    target +
      compartment(name = "central", volume = as_declaration(component@prm_names['vc'])) +
      compartment(name = "peripheral", volume =  as_declaration(component@prm_names['vp'])) +
      flow(from = "central", to = "peripheral", definition = dcl_q) +
      flow(from = "peripheral", to = "central", definition = dcl_q)
  }
)

#' @export
pk_distribution_2cmp <- function(
  prm_vc = prm_log_normal("vc"),
  prm_vp = prm_log_normal("vp"),
  prm_q = prm_log_normal("q")
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
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkDistribution3Cmp"),
  definition = function(target, source, component) {
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

#' @export
#' @rdname pk_component
pk_distribution_3cmp <- function(
  prm_vc = prm_log_normal("vc"),
  prm_vp1 = prm_log_normal("vp1"),
  prm_vp2 = prm_log_normal("vp2"),
  prm_q1 = prm_log_normal("q1"),
  prm_q2 = prm_log_normal("q2")
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
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkEliminationLinear"),
  definition = function(target, source, component) {
    dcl <- declaration(~cl*C) %>%
      dcl_substitute(list(cl = sym(component@prm_names['cl'])))
    target +
      flow(from = "central", definition = dcl)
  }
)

#' @export
pk_elimination_linear <- function(prm_cl = prm_log_normal("cl")) {
  PkEliminationLinear(
    parameters = list(
      cl = prm_cl
    )
  ) +
    prm_cl
}


# elimination Michaelis Menten ------------------------------------------------------


PkEliminationMM <- setClass("PkEliminationMM",
         contains = "PkEliminationComponent")

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkEliminationMM"),
  definition = function(target, source, component) {
    dcl <- declaration(~clmm*km/(km+C)) %>%
      dcl_substitute(list(clmm = sym(component@prm_names['clmm']), km = sym(component@prm_names['km'])))
    target +
      flow(from = "central", definition = dcl)
  }
)

#' @export
pk_elimination_mm <- function(prm_clmm = prm_log_normal("clmm"),
                              prm_km = prm_log_normal("km")) {
  PkEliminationMM(
    parameters = list(
      clmm = prm_clmm,
      km = prm_km
    )
  ) +
    prm_clmm +
    prm_km
}



# absorption rate first-order ------------------------------------------------------


PkAbsorptionRateFO <- setClass("PkAbsorptionRateFO",
         contains = "PkAbsorptionRateComponent")

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionRateFO"),
  definition = function(target, source, component) {
    dcl <- declaration(ka~1/mat) %>%
      dcl_substitute(list(mat = sym(component@prm_names['mat'])))
    if (is(source@facets$PkComponentFacet@entries[['absorption-delay']], "PkAbsorptionDelayTransit")) {
      n <- source@facets$PkComponentFacet@entries[['absorption-delay']]@ncompartments
      target +
        flow(from = paste0("transit", n), to = "central", definition = ~ka*A) +
        algebraic(dcl)
    } else {
      target +
        compartment("depot", volume = ~1) +
        flow(from = "depot", to = "central", definition = ~ka*A) +
        algebraic(dcl)
    }
  }
)

#' @export
pk_absorption_rate_fo <- function(prm_mat = prm_log_normal("mat")) {
  PkAbsorptionRateFO(
    parameters = list(
      mat = prm_mat
    )
  ) +
    prm_mat
}


# absorption rate zero-order ---------------------------------------------------


PkAbsorptionRateZO <- setClass("PkAbsorptionRateZO",
                               contains = "PkAbsorptionRateComponent")

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionRateZO"),
  definition = function(target, source, component) {
    dcl <- declaration(r1~amt/mat/2) %>%
      dcl_substitute(list(mat = sym(component@prm_names['mat'])))
    target +
      algebraic(dcl)
  }
)

#' @export
pk_absorption_rate_zo <- function(prm_mat = prm_log_normal("mat")) {
  PkAbsorptionRateZO(
    parameters = list(
      mat = prm_mat
    )
  ) +
    prm_mat
}


# absorption delay transit ------------------------------------------------------


PkAbsorptionDelayTransit <- setClass(
  "PkAbsorptionDelayTransit",
  slots = c(ncompartments = "numeric"),
  contains = "PkAbsorptionDelayComponent"
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionDelayTransit"),
  definition = function(target, source, component) {

    dcl <- declaration(ktr~n/mdt) %>%
      dcl_substitute(
        list(
          mdt = sym(component@prm_names['mdt']),
          n = component@ncompartments
        )
      )
    cmps <- purrr::map(seq_len(component@ncompartments), ~compartment(paste0("transit", .x)))
    flows <- NULL
    if (component@ncompartments > 1) {
      flows <- purrr::map2(.x = seq(1, component@ncompartments - 1),
                           .y = seq(2, component@ncompartments),
                           ~flow(paste0("transit", .x), paste0("transit", .y), definition = ~ktr*A))

    }

    purrr::reduce(cmps, `+`, .init = target) %>%
      purrr::reduce(flows, `+`, .init = . ) +
      algebraic(dcl)
  }
)

#' @export
pk_absorption_delay_transit <- function(transit_compartments = 1L, prm_mdt = prm_log_normal("mdt")) {
  PkAbsorptionDelayTransit(
    ncompartments = transit_compartments,
    parameters = list(
      mdt = prm_mdt
    )
  ) +
    prm_mdt
}

# absorption delay lagtime ------------------------------------------------------


PkAbsorptionDelayLagtime <- setClass(
  "PkAbsorptionDelayLagtime",
   contains = "PkAbsorptionDelayComponent"
)

setMethod(
  f = "convert",
  signature = c(target = "Model", source = "PkModel", component = "PkAbsorptionDelayLagtime"),
  definition = function(target, source, component) {

    dcl <- declaration(alag1~mdt) %>%
      dcl_substitute(
        list(
          mdt = sym(component@prm_names['mdt'])
        )
      )

    target +
      algebraic(dcl)
  }
)

#' @export
pk_absorption_delay_lagtime <- function(prm_mdt = prm_log_normal("mdt")) {
  PkAbsorptionDelayLagtime(
     parameters = list(
      mdt = prm_mdt
    )
  ) +
    prm_mdt
}