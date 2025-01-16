whale_distances <- function(i, mask, grid_tbl, capture) {

  detector_index <- capture[i, "Detector", drop = TRUE]

  # x-coordinate of whale i
  x_i <- grid_tbl[detector_index, "easting", drop = TRUE]
  # y-coordinate of whale i
  y_i <- grid_tbl[detector_index, "northing", drop = TRUE]

  dx <- x_i - mask[, 1]
  dy <- y_i - mask[, 2]

  # Distance between point (x_i, y_i) and grid cell points
  d <- sqrt(dx^2 + dy^2)

  d

}

# 16 HEX	hazard exponential detection function
# see ?detectfn
hex <- function(d, sigma, lambda0) {

  1 - exp(- lambda0 * exp(-d / sigma))

}

#' Whale encounter probability
#'
#' [whale_prob()] determines whale encounter probabilities.
#'
#' @param mask A [mask][secr::mask] object containing the (x, y) locations of the
#' grid cells for which probabilities are to be calculated.
#' @param grid A [RasterLayer][raster::raster] object.
#' @param capture A data frame of four columns: `Session`, `ID`, `Occasion` and
#'   `Detector`.
#' @param params A named list containing the elements: `sigma` and `lambda0`,
#'   i.e. the fitted parameters obtained with [`secr.fit()`][secr::secr.fit()].
#' @param detectfn A function object expecting three parameters: (i) `d`, a
#'   double vector of distances; (ii) `sigma`, a scalar double of the fitted
#'   \eqn{\sigma}; and (iii) `lambda0`, a scalar fitted \eqn{\lambda_0}.
#'
#' @return A [tibble][tibble::tibble()] that is the `mask` object passed as
#'   input with the added column `p` for the encounter probability.
#'
#' @importFrom rlang .data
#' @export
whale_prob <- function(mask, grid, capture, params, detectfn = hex) {

  p <- vector(mode = "double", nrow(mask))
  grid_tbl <-
    raster::coordinates(grid) |>
    tibble::as_tibble() |>
    dplyr::rename(lon = .data$x, lat = .data$y) |>
    longlat_to_utm()


  for (i in seq_len(nrow(capture))) {

    d <- whale_distances(i, mask = mask, grid_tbl = grid_tbl, capture = capture)
    p <- p + detectfn(d, sigma = params$sigma, lambda0 = params$lambda0)
  }

  dplyr::bind_cols(mask, p = p / sum(p)) |>
    tibble::as_tibble()
}
