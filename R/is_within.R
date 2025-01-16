#' Check if points are within a polygon
#'
#' This function determines whether a set of points (defined by their
#' coordinates) lies within a given polygon. It returns a logical vector
#' indicating whether each point is within the polygon.
#'
#' @param x A numeric vector of x-coordinates (e.g., longitude).
#' @param y A numeric vector of y-coordinates (e.g., latitude).
#' @param sf_polygon An `sf` object representing the polygon(s) within which the
#'   points will be checked. The polygon object must include a column named
#'   `name`.
#'
#' @return A logical vector indicating if each point is within the polygon
#'   (`TRUE`) or not (`FALSE`).
#'
#' @details
#' The function converts the input coordinates into an `sf` object and uses
#' `sf::st_within` to check whether each point lies within the provided polygon.
#' The `name` column in the `sf_polygon` object is used to determine membership.
#' Missing (`NA`) values in the `name` column indicate points that are not
#' within any polygon.
#'
#' @export
is_within <- function(x, y, sf_polygon) {

  if (!"name" %in% names(sf_polygon)) {
    stop("`sf_polygon` must contain an attribute named `name`.")
  }

  tbl <- tibble::tibble(x = x, y = y)

  sf <-
    tbl |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = 4326,
                 agr = "constant")

  sf::st_join(sf, sf_polygon, join = sf::st_within) |>
    dplyr::mutate(is_within = !is.na(.data$name)) |>
    dplyr::pull("is_within")

}
