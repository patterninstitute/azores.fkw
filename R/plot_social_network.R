#' Plot false killer whales' social network
#'
#' [plot_social_network()] uses the `individuals` data set to show which
#' whales were sighted together at each sighting event.
#'
#' @param node_text_spacing Use this parameter to tweak the spacing between the
#' node text annotation and the node.
#' @param sighting_colors Set the colours for each sighting.
#'
#' @return Returns a ggplot2 object that draws the aforementioned social
#'   network.
#'
#' @examples
#' plot_social_network()
#'
#' @importFrom rlang .data
#' @export
plot_social_network <- function(node_text_spacing = 1.1, sighting_colors = c(
  "2" = "red",
  "11" = "blue",
  "23" = "#ebcf12",
  "29" = "green",
  "30" = "#f700ff",
  "35" = "#ff6400",
  "43" = "#28d3d1",
  "44" = "#00c124",
  "46" = "gray",
  "8" = "black",
  "15" = "black",
  "16" = "black",
  "19" = "black",
  "20" = "black",
  "21" = "black",
  "25" = "black",
  "31" = "black",
  "33" = "black",
  "36" = "black",
  "37" = "black",
  "38" = "black",
  "40" = "black",
  "41" = "black",
  "45" = "black"
)) {

  individuals <- azores.fkw::individuals

  sn <-
    individuals |>
    dplyr::group_by(.data$sighting_id) |>
    dplyr::group_modify( ~ tidyr::expand_grid(x = .x$individual, y = .x$individual),
                         .keep = TRUE) |>
    dplyr::rowwise() |> # Remove repeated interactions (in reverse)
    dplyr::mutate(key = paste(sort(c(.data$x, .data$y)), collapse = '-')) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data$key, .keep_all = TRUE) |>
    dplyr::select(-'key')

  ind_fac <- as.factor(sort(unique(individuals$individual)))

  nodes <- tibble::tibble(name = levels(ind_fac))

  edges <-
    tibble::tibble(from = match(sn$x, ind_fac), to = match(sn$y, ind_fac))

  sn_graph <-
    tidygraph::tbl_graph(nodes = nodes,
                         edges = edges,
                         directed = FALSE) |>
    tidygraph::activate(edges) |>
    dplyr::mutate(sighting_id = as.factor(sn$sighting_id))

  n_individuals <- length(unique(individuals$individual))
  angle_step <- 360 / n_individuals
  angles <- 90 - seq(0, 360 - angle_step, angle_step) + (angle_step) / 2
  node_text_angle <- ifelse(angles > -90, angles, angles + 180)

  sn_graph |>
    ggraph::ggraph(layout = 'linear', circular = TRUE) +
    ggraph::geom_node_point() +
    ggraph::geom_edge_arc(
      mapping = ggplot2::aes(edge_colour = .data$sighting_id),
      edge_width = 1,
      fold = TRUE
    ) +
    ggraph::scale_edge_colour_manual(values = sighting_colors) +
    ggraph::geom_node_text(ggplot2::aes(
      x = .data$x * node_text_spacing,
      y = .data$y * node_text_spacing,
      label = .data$name,
      angle = node_text_angle
    )) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
