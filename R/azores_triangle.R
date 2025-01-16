#' Azores triangle islands
#'
#' Pico, Faial and São Jorge form the so-called "Triangle" of the Central Group
#' of islands of Azores, Portugal.
#' [`azores_triangle()`] returns the islands' contours as an [sf][sf::sf]
#' object.
#'
#' @return A simple feature geometry list column ([sf][sf::sf]) object
#'   containing three features, one for each of three islands.
#'
#' @importFrom rlang .data
#' @export
azores_triangle <- function() {

  dplyr::filter(azores.fkw::azores_islands, .data$is_triangle)

}
