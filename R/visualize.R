

#' Visualize a model
#'
#' @param model The model
#'
#' @export
visualize <- function(model){
  if(!requireNamespace("DiagrammeR", quietly = TRUE)) stop("The package 'DiagrammeR' needs to be installed to use this functionality.")

  nodes <- model$compartments %>%
    dplyr::transmute(label = .data$name, shape = 'rectangle') %>%
    purrr::invoke(.f = DiagrammeR::create_node_df, n = nrow(.),
                  width = .8, height = .8, fillcolor = "gray70", fontcolor = "black")

  hidden_nodes <- model$flows %>%
    dplyr::filter(is.na(.data$to)) %>%
    dplyr::transmute(label = paste0("output", 1:dplyr::n())) %>%
    purrr::invoke(.f = DiagrammeR::create_node_df, n = nrow(.), style = "invis", width = 0, height = 0, shape = 'point')


  all_nodes <- DiagrammeR::combine_ndfs(nodes, hidden_nodes)

  node_ids <- all_nodes %>% {purrr::set_names(.$id, .$label)}
  edges <- model$flows %>%
    dplyr::group_by(.data$to) %>%
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      to = ifelse(is.na(.data$to), paste0('output', .data$id), .data$to)
    ) %>%
    dplyr::transmute(
      from = node_ids[.data$from], to = node_ids[.data$to],
      label = purrr::map_chr(.data$definition, render_expr, opts = render_opts_viz())) %>%
    purrr::invoke(.f = DiagrammeR::create_edge_df)


  DiagrammeR::create_graph(all_nodes, edges) %>%
    DiagrammeR::add_global_graph_attrs('layout', 'dot', 'graph') %>%
    DiagrammeR::add_global_graph_attrs('rankdir', 'LR', 'graph') %>%
    DiagrammeR::add_global_graph_attrs('ranksep', '0', 'graph') %>%
    DiagrammeR::add_global_graph_attrs('nodesep', '0.15', 'graph') %>%
    DiagrammeR::add_global_graph_attrs('splines', 'polyline', 'graph') %>%
    DiagrammeR::render_graph()

}
