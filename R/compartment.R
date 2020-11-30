#' @include facet.R
#' @include nm_model.R
#' @include model.R
Compartment <- setClass(
  "Compartment",
  slots = c(volume = "assemblerr_declaration"),
  contains = "NamedFacetEntry",
  prototype = prototype(facet_class = "CompartmentFacet")
)

CompartmentFacet <- setClass(
  "CompartmentFacet",
  contains = "NamedFacet",
  prototype = prototype(entry_class = "Compartment")
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "Compartment"),
  definition = function(target, source, component) {
    target + nm_compartment(name = component@name)
  }
)

Flow <- setClass(
  "Flow",
  slots = c(
    from = "character",
    to = "character",
    definition = "assemblerr_declaration"
  ),
  contains = "FacetEntry",
  prototype = prototype(facet_class = "FlowFacet")
)

FlowFacet <- setClass("FlowFacet",
                             contains = "Facet",
                             prototype = prototype(entry_class = "Flow"))
setMethod(
  f = "optimize_for_conversion",
  signature = signature(source = "Model", target = "NmModel", component = "FlowFacet"),
  definition = function(source, target, component, ...) {
    if (vec_is_empty(component@entries)) return(source)
    flows <- collect_flows(component, source)
    # detect special advans
    if (all(dcl_linear_in(flows$definition, quote(A[x])))) {
      special_advan_used <- FALSE
      if (target@options$ode.use_special_advans) {
        m_adj <- construct_adjacency_matrix(
          cmp_names = names(source@facets[["CompartmentFacet"]]),
          flows = flows
        )
        perm_advan <- find_advan_and_permutation(m_adj)
        if (!is.null(perm_advan)) {
          special_advan_used <- TRUE
          source <- change_to_advan(source = source,
                                    flows = flows,
                                    advan = perm_advan$advan,
                                    permutation = perm_advan$permutation,
                                    options = target@options
                                    )
        }
      }
      if (!special_advan_used && target@options$ode.use_general_linear_advans) {
        cmp_name_index_map <- name_index_map(source@facets[["CompartmentFacet"]])
        flows$from_index <- cmp_name_index_map[flows$from]
        flows$to_index <- cmp_name_index_map[flows$to]
        flows$to_index[is.na(flows$to_index)] <- 0
        rate_constants <- dcl_factor_out(flows$definition, quote(A[x]))
        dcl_id(rate_constants) <- rlang::syms(paste0("k", flows$from_index, flows$to_index))
        source <- vec_chop(rate_constants) %>%
          purrr::map(algebraic) %>%
          purrr::reduce(`+`, .init = source)
        source <- source +
          algebraic(dcl_create_library_function_call(target@options$ode.general_linear_advan, list(quote(.k)), quote(A))) +
          algebraic(dcl_create_library_function_call("trans1", dcl_id(rate_constants), quote(.k)))
        # remove flow declarations
        source@facets[["FlowFacet"]]@entries <- list()
      }
    }
    return(source)
  }
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "Model", component = "FlowFacet"),
  definition = function(target, source, component) {
    if (vec_is_empty(component@entries)) return(target)

    flows <- collect_flows(component, source)

    ode_dcl <- purrr::map(source@facets[["CompartmentFacet"]]@entries, function(cmp){
      outflow <- dcl_sum(flows$definition[flows$from == cmp@name & !is.na(flows$from)])
      inflow <- dcl_sum(flows$definition[flows$to == cmp@name & !is.na(flows$to)])
      dcl_substract(inflow, outflow, lhs = bquote(dadt[.(cmp@name)]))
    }) %>%
      purrr::set_names(NULL) %>%
      {vec_c(!!!.)} %>%
      replace_compartment_references(target, source)

    advan <- target@options$ode.general_nonlinear_advan
    target +
      nm_subroutine(advan, tol = 6L) +
      nm_des(as_statement(ode_dcl))
  }
)

collect_flows <- function(flow_facet, source_model) {
  purrr::map_dfr(flow_facet@entries, function(flow){
    dcl <- flow@definition
    cmp_from <- source_model@facets[["CompartmentFacet"]]@entries[[flow@from]]
    if ("C" %in% dcl_vars_chr(dcl)) {
      volume <- dcl_def(cmp_from@volume)[[1]]
      dcl <- dcl_substitute(dcl, list(C = bquote(A/.(volume))))
    }
    dcl <- dcl_substitute(dcl, list(A = bquote(A[.(cmp_from@name)])))
    list(definition = dcl, from = flow@from, to = flow@to)
  })
}

construct_adjacency_matrix <- function(cmp_names, flows) {
  cmp_names <- c(cmp_names, "_sink")
  m_adj <- matrix(0,
                  nrow = vec_size(cmp_names),
                  ncol = vec_size(cmp_names),
                  dimnames = list(cmp_names, cmp_names))
  flows$to[is.na(flows$to)] <- "_sink"
  for (row in seq_len(nrow(flows))) {
    m_adj[flows$from[row], flows$to[row]] <- 1
  }
  return(m_adj)
}

find_advan_and_permutation <- function(adjacency_matrix){
  # check graph invariants to narrow down possibilities
  candidate_indicies <- which(nrow(adjacency_matrix) == purrr::map_dbl(advan_definitions, ~nrow(.x[["adjacency_matrix"]])) &
                              sum(adjacency_matrix) == purrr::map_dbl(advan_definitions, ~sum(.x[["adjacency_matrix"]])))
  if (!vec_is_empty(candidate_indicies)) {
    # try all permutations
    permutations <- generate_permutations(vec_seq_along(adjacency_matrix))
    for (p_index in seq_len(nrow(permutations))) {
      p_mat <- permutation_matrix(permutations[p_index,])
      match_index <- which(purrr::map_lgl(advan_definitions[candidate_indicies],
                                          ~all(.x[["adjacency_matrix"]]==p_mat%*%adjacency_matrix%*%t(p_mat))))
      if (!vec_is_empty(match_index)) {
        p <- permutations[p_index,]
        p <- p[-vec_size(p)]
        advan <- names(advan_definitions[candidate_indicies])[match_index]
        return(list(permutation = p, advan = advan))
      }
    }
  }
  return(NULL)
}

change_to_advan <- function(source, flows, advan, permutation, options){
  trans_results <- parameterize_advan(names(source@facets[["CompartmentFacet"]]@entries),flows, advan, permutation, options)
  prm_dcl <- trans_results$prms
  required_prms <- dcl_id(prm_dcl)
  prm_dcl <- dcl_discard_identities(prm_dcl)
  source <- Map(algebraic, prm_dcl) %>%
    purrr::reduce(`+`, .init = source) +
    algebraic(dcl_create_library_function_call(advan, list(quote(.k)), quote(A))) +
    algebraic(dcl_create_library_function_call(trans_results$routine, required_prms, quote(.k)))
  # reorder compartments
  source@facets[["CompartmentFacet"]]@entries <- source@facets[["CompartmentFacet"]]@entries[permutation]
  # remove flow declarations
  source@facets[["FlowFacet"]]@entries <- list()
  return(source)
}

parameterize_advan <- function(compartment_names, flows, advan, permutation, options){
  name_mapping <- rownames(advan_definitions[[advan]]$adjacency_matrix)
  name_mapping <- purrr::set_names(name_mapping, c(compartment_names[permutation], "NA"))
  flows[["from"]] <- name_mapping[flows[["from"]]]
  flows[["to"]][is.na(flows[["to"]])] <- "NA"
  flows[["to"]] <- name_mapping[flows[["to"]]]
  definitions <- flows$definition %>%
    dcl_factor_out(quote(A[x])) %>%
    vec_chop() %>%
    purrr::set_names(nm = paste("k", flows[["from"]], flows[["to"]], sep = "_"))
  suitable_trans <- names(advan_definitions[[advan]][["parameterizations"]])
  preferred_trans <- options$ode.preferred_trans_routines
  trans <- preferred_trans[preferred_trans %in% suitable_trans]
  if (vec_is_empty(trans)) {
    warning("None of the preferred TRANS routines was suitable, resorting to TRANS1.")
    trans <- "trans1"
  }else{
    trans <- trans[1]
  }
  repar_function <- advan_definitions[[advan]][["parameterizations"]][[trans]]
  list(
    routine = trans,
    prms = do.call(repar_function, args = definitions)
  )
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
#' comp1 <- compartment("central", volume = declaration(~Vc))
compartment <- function(name, volume = declaration(~1)){
  if (!is.character(name)) stop("'name' needs to be a character vector")
  volume <- as_declaration(volume)
  vec_assert(volume, ptype = declaration(), size = 1)
  Compartment(name = name, volume = volume)
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
#' f <- flow(from = "depot", to = "central", definition = declaration(~ka*A))
flow <- function(from = NA_character_, to = NA_character_, definition){
  if (!is.character(from) && !is.character(to)) stop("'from' or/and 'to' need to be compartment names")
  definition <- as_declaration(definition)
  vec_assert(definition, ptype = declaration(), size = 1)
  Flow(from = from, to = to, definition = definition)
}

