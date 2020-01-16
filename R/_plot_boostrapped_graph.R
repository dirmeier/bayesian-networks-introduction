# R tutorial on Bayesian networks
#
# Copyright (C) Simon Dirmeier (<simon.dirmeier @ bsse.ethz.ch>)
# Date: 17. 01. 2020


suppressMessages({
  library(ggraph)
  library(igraph)
  library(dplyr)
})


plot.bootstrapped <- function(boostrapped.bn)
{
  boostrapped.bn %>%
    dplyr::filter(strength > .50, direction >= .50) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    ggraph::ggraph(layout = "stress") +
    ggraph::geom_edge_link(aes(color=strength,
                               start_cap = label_rect(node1.name),
                               end_cap = label_rect(node2.name)),
                           arrow = arrow(length = unit(4, 'mm'))) +
    ggraph::geom_node_text(aes(label = name), size = 3) +
    ggraph::scale_edge_color_viridis("Strength", end = .8,
                                     limits = c(.5, 1), option = "B") +
    ggraph::theme_graph()
}

