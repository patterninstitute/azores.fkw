#' Import elevation data
#'
#' [`get_elevation()`] reads elevation data from a gray scale TIFF image file
#' for the Azores region. This function assumes that the coordinate reference
#' system used in the image file is `"+proj=longlat +datum=WGS84 +ellps=WGS84
#' +towgs84=0,0,0"`. The (x, y) coordinates in the output are in UTM Zone 26N.
#'
#' @param file The path to the TIFF file.
#' @param resolution The resolution in meters.
#' @param xmin Minimum easting boundary, in meters.
#' @param xmax Maximum easting boundary, in meters.
#' @param ymin Minimum northing boundary, in meters.
#' @param ymax Maximum northing boundary, in meters.
#'
#' @return A [tibble][tibble::tibble] of five columns:
#' \describe{
#' \item{`x`}{Easting (x) coordinate, in metres.}
#' \item{`y`}{Northing (y) coordinate, in metres.}
#' \item{`elevation`}{Elevation. Units will depend on the units in the TIFF file
#' but is typically in metres.}
#' \item{`altitude`}{Same as elevation but negative values (below water surface
#' level) have been floored to zero.}
#' \item{`depth`}{The negative of elevation, original positive values (above
#' water surface level) are topped to zero.}
#' }
#'
#' @importFrom rlang .data
#' @export
get_elevation <-
  function(file,
           resolution = 1000,
           xmin = 300000,
           ymin = 4100000,
           xmax = 500000,
           ymax = 4400000) {

    relief <- stars::read_stars(file) |>
      dplyr::rename(elevation = 1)

    lonlat_points <-
      sf::st_as_sf(relief,
                   as_points = TRUE,
                   merge = FALSE,
                   long = TRUE)

    cropping_bbox <-
      sf::st_bbox(
        c(
          xmin = 300000,
          ymin = 4100000,
          xmax = 500000,
          ymax = 4400000
        )
      )

    utm_points <- sf::st_transform(lonlat_points, crs = 32626)

    utm_points_cropped <- sf::st_crop(utm_points, cropping_bbox)

    grid_template <-
      utm_points_cropped |>
      sf::st_bbox() |>
      stars::st_as_stars(dx = resolution, dy = resolution)

    # `utm_points2` is `utm_points` after "resampling" with a certain resolution
    relief2 <-
      stars::st_rasterize(utm_points_cropped, grid_template)

    utm_points2 <-
      sf::st_as_sf(relief2,
                   as_points = TRUE,
                   merge = FALSE,
                   long = TRUE)

    elevation <-
      dplyr::bind_cols(sf::st_drop_geometry(utm_points2),
                       sf::st_coordinates(utm_points2)) |>
      tibble::as_tibble() |>
      dplyr::relocate(x = .data$X, y = .data$Y, .before = 1L) |>
      tidyr::drop_na(elevation) |>
      dplyr::mutate(
        altitude = ifelse(elevation >= 0, elevation, 0),
        depth = ifelse(elevation < 0, -elevation, 0)
      )

    elevation
}
