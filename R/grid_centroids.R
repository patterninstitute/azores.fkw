#' Grid centroids
#'
#' [grid_centroids()] get the centroid coordinates from a grid.
#'
#' @param grid A [RasterLayer][raster::raster] object. Use [create_grid()] to
#' easily generate a grid.
#'
#' @return A [tibble][tibble::tibble-package] of three columns: `index`,
#'   `longitude` and `latitude`.
#'
#' @importFrom rlang .data
#' @export
grid_centroids <- function(grid = create_grid()) {

  grid |>
    raster::coordinates() |>
    tibble::as_tibble() |>
    tibble::rowid_to_column("index") |>
    dplyr::rename(latitude = .data$y, longitude = .data$x)
}
