#' Convert decimal degrees to UTM coordinates
#'
#' [longlat_to_utm()] converts a longitude / latitude in decimal degrees to
#' Universal Transverse Mercator (UTM) coordinates. The northing values are
#' measured from zero at the equator in a northerly direction.
#'
#' @param longlat A [tibble][tibble::tibble-package] of points whose coordinates
#'   are to be converted to UTM. `longlat` must have two columns named
#'   `lon` and `lat`, otherwise it throws an error.
#' @param crs_from Source coordinate reference system (CRS), EPSG code, see the
#'   argument `crs` of [st_transform()][sf::st_transform] for possible
#'   specifications; defaults to `4326` (WGS84).
#' @param crs_to Target coordinate reference system (CRS), EPSG code, see the
#'   argument `crs` of [st_transform()][sf::st_transform] for possible
#'   specifications; defaults to `32626` (UTM zone 26N).
#'
#' @return A [tibble][tibble::tibble-package] with the same number of rows as
#'   `longlat`, one row per point, and two columns:
#' \describe{
#' \item{`northing`}{Corresponding to the conversion of latitude values.}
#' \item{`easting`}{Corresponding to the conversion of longitude values.}
#' }
#'
#' @importFrom rlang .data
#' @export
longlat_to_utm <- function(longlat, crs_from = 4326, crs_to = 32626) {
  if (!rlang::has_name(longlat, "lon"))
    stop("`longlat` must have a column named `lon`")

  if (!rlang::has_name(longlat, "lat"))
    stop("`longlat` must have a column named `lat`")

  # Create an sf POINT object with longitude/latitude coordinates
  lonlat_points <-
    sf::st_as_sf(longlat, coords = c("lon", "lat"), crs = crs_from)  # EPSG:4326 for WGS84

  # Transform the points to UTM projection (Azores region - UTM Zone 26N)
  utm_points <- sf::st_transform(lonlat_points, crs = crs_to)

  # Convert the UTM points back to a tibble
  utm_coords <- sf::st_coordinates(utm_points) |>
    tibble::as_tibble() |>
    dplyr::rename(easting = "X", northing = "Y")

  utm_coords

}

#' Convert UTM coordinates to decimal degrees
#'
#' [utm_to_longlat()] converts Universal Transverse Mercator (UTM) coordinates
#' to longitude / latitude in decimal degrees.
#'
#' @param utm A [tibble][tibble::tibble-package] of points whose coordinates
#'   are to be converted to UTM. `utm` must have two columns named
#'   `easting` and `northing`, otherwise it throws an error.
#' @param crs_from Source coordinate reference system (CRS), EPSG code, see the
#'   argument `crs` of [st_transform()][sf::st_transform] for possible
#'   specifications; defaults to `32626` (UTM zone 26N).
#' @param crs_to Target coordinate reference system (CRS), EPSG code, see the
#'   argument `crs` of [st_transform()][sf::st_transform] for possible
#'   specifications; defaults to `4326` (WGS84).
#'
#' @return A [tibble][tibble::tibble-package] with the same number of rows as
#'   `utm`, one row per point, and two columns:
#' \describe{
#' \item{`lon`}{Corresponding to the conversion of northing values.}
#' \item{`lat`}{Corresponding to the conversion of easting values.}
#' }
#'
#' @importFrom rlang .data
#' @export
utm_to_longlat <- function(utm, crs_from = 32626, crs_to = 4326) {
  if (!rlang::has_name(utm, "easting"))
    stop("`utm` must have a column named `easting`")

  if (!rlang::has_name(utm, "northing"))
    stop("`utm` must have a column named `northing`")

  # Create an sf POINT object with easting/northing coordinates
  utm_points <-
    sf::st_as_sf(utm, coords = c("easting", "northing"), crs = crs_from)  # (Azores region - UTM Zone 26N)

  # Transform the points to EPSG:4326 for WGS84
  longlat_points <- sf::st_transform(utm_points, crs = crs_to)

  # Convert the UTM points back to a tibble
  longlatcoords <-
    longlat_points |>
    dplyr::mutate(
      lon = unname(sf::st_coordinates(.data$geometry)[, "X"]),
      lat = unname(sf::st_coordinates(.data$geometry)[, "Y"])
    ) |> sf::st_drop_geometry()

  longlatcoords
}
