#' Lookout observation area
#'
#' [lookout_obs_area()] returns the lookout observation area as a sf POLYGON.
#' This function defines the observation area as a circle section whose center
#' point is `point_queimada`, whose observation aperture range is defined by
#' the points `point_sao_mateus` to the west and `point_terras` to the east.
#'
#' @param lookout_max_distance Defines the distance to the horizon, i.e. the
#' distance up to which the lookout is able to see. Units are in meters.
#'
#' @param point_sao_mateus The position of São Mateus, Pico Island, Azores.
#' A numeric vector of two elements: latitude and longitude, in decimal degrees.
#'
#' @param point_queimada The position of Vigia da Queimada, Pico Island, Azores.
#' A numeric vector of two elements: latitude and longitude, in decimal degrees.
#'
#' @param point_terras The position of Terras, Pico Island, Azores.
#' A numeric vector of two elements: latitude and longitude, in decimal degrees.
#'
#' @returns An sf object of type POLYGON.
#'
#' @examples
#' (area <- lookout_obs_area())
#'
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = area)
#'
#' @export
lookout_obs_area <- function(
    lookout_max_distance = 22 * 1852,
    point_sao_mateus = c(-28.444493, 38.420308),
    point_queimada = c(-28.23645, 38.38496),
    point_terras = c(-28.231384, 38.385289)
    ) {

  # Lookout max distance in meters
  # 1 nm is 1852 meters.
  # lookout_max_distance <- 22 * 1852

  # Point with coordinates (Longitude, Latitude)
  point_sao_mateus <- sf::st_point(point_sao_mateus)
  point_queimada <- sf::st_point(point_queimada)
  point_terras <- sf::st_point(point_terras)

  bearing_west <- geosphere::bearing(sf::st_coordinates(point_queimada), sf::st_coordinates(point_sao_mateus))
  bearing_east <- geosphere::bearing(sf::st_coordinates(point_queimada), sf::st_coordinates(point_terras))

  bearing_west_max_point <- geosphere::destPoint(p = sf::st_coordinates(point_queimada), bearing_west, lookout_max_distance)
  bearing_east_max_point <- geosphere::destPoint(p = sf::st_coordinates(point_queimada), bearing_east, lookout_max_distance)

  bearings <- seq(from = bearing_east, to = bearing_west + 360, by = (bearing_west + 360 - bearing_east) / 1000)
  polygon_points <- geosphere::destPoint(p = sf::st_coordinates(point_queimada), b = bearings, d = lookout_max_distance)
  polygon_points2 <-
    rbind(
      sf::st_coordinates(point_queimada),
      polygon_points,
      sf::st_coordinates(point_queimada)
    )

  # Create the polygon geometry
  lookout_obs_area <- sf::st_polygon(list(polygon_points2))

  lookout_obs_area_sf <- sf::st_sf(
    name = "Lookout observation area",
    geometry = sf::st_sfc(lookout_obs_area, crs = 4326)
  )

  lookout_obs_area_sf
}
