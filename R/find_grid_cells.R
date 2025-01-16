#' Find grid cells matching coordinates
#'
#' [find_grid_cells()] find grid cells that match the `points` coordinates.
#'
#' @param points A [tibble][tibble::tibble-package] that must contain the two
#'   columns `latitude` and `longitude`, whose coordinates are to be matched
#'   against the grid cells.
#' @param grid A [RasterLayer][raster::raster] grid. Use [create_grid()] to
#'   create such a grid.
#'
#' @return A [tibble][tibble::tibble-package] of three variables:
#' \describe{
#' \item{`index`}{Grid cell index.}
#' \item{`latitude`}{Latitude in decimal degrees.}
#' \item{`longitude`}{Longitude in decimal degrees.}
#' }
#'
#' @export
find_grid_cells <- function(points, grid = create_grid()) {

  if (!rlang::has_name(points, "longitude"))
    stop("`longlat` must have a column named longitude")

  if (!rlang::has_name(points, "latitude"))
    stop("`longlat` must have a column named latitude")

  latitude <- points[, "latitude", drop = TRUE]
  longitude <- points[, "longitude", drop = TRUE]
  m <- matrix(c(longitude, latitude), ncol = 2L)

  cells_indices <- raster::cellFromXY(grid, m)
  cells_coordinates <- raster::coordinates(grid)[cells_indices, ]

  tibble::tibble(index = as.integer(cells_indices),
                 latitude = cells_coordinates[, "y"],
                 longitude = cells_coordinates[, "x"])
}
