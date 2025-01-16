create_utm_grid <-
  function(x_range = range(azores.fkw::boat_trips_by_min$longitude),
           y_range = range(azores.fkw::boat_trips_by_min$latitude),
           utm_resolution = 1000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           x_offset = -utm_resolution/2,
           y_offset = -utm_resolution/2) {

    # Padding
    x_padding <- x_pad_perc * diff(x_range)
    y_padding <- y_pad_perc * diff(y_range)

    x_range2 <- c(x_range[1] - x_padding, x_range[2] + x_padding)
    y_range2 <- c(y_range[1] - y_padding, y_range[2] + y_padding)

    longlat_ranges <-
      data.frame(lon = x_range2,
                 lat = y_range2)

    utm_ranges <- longlat_to_utm(longlat_ranges)

    nx <-
      round((utm_ranges$easting[2] - utm_ranges$easting[1]) / utm_resolution) + 1
    ny <-
      round((utm_ranges$northing[2] - utm_ranges$northing[1]) / utm_resolution) + 1

    utm_grid <-
      secr::make.grid(
        nx = nx,
        ny = ny,
        spacing = utm_resolution,
        originxy = c(utm_ranges$easting[1], utm_ranges$northing[1])
      )

    attr(utm_grid, "nx") <- nx
    attr(utm_grid, "ny") <- ny

    utm_grid$x <- utm_grid$x + x_offset
    utm_grid$y <- utm_grid$y + y_offset

    utm_grid
  }




#' Create a grid
#'
#' [create_grid()] creates a grid encoded as a [RasterLayer][raster::raster]
#' object. All arguments are optional, having defaults that match the
#' specification required for the analysis provided by this R compendium.
#'
#' @param x_range A numeric vector of two values, specifying the minimum and
#'   maximum values of longitude in decimal degrees.
#' @param y_range A numeric vector of two values, specifying the minimum and
#'   maximum values of latitude in decimal degrees.
#' @param utm_resolution Grid resolution in metres.
#' @param x_pad_perc Padding percentage along the x-direction
#'   (longitude/easting), i.e. how much of percentage of the `x_range` should
#'   the grid be extended. This percentage is added to both ends, meaning that
#'   effectively the `x_range` is increased two fold of the `x_pad_perc`.
#' @param y_pad_perc Padding percentage along the y-direction
#'   (latitude/northing).
#' @param x_offset Offset along the x-direction in metres. A negative value translates
#' the grid location towards west.
#' @param y_offset Offset along the y-direction in metres. A negative value translates
#' the grid location towards south.
#'
#' @return A [RasterLayer][raster::raster] object. By default returns a grid
#'   encompassing the study area, i.e., off the coast of Pico, Faial and São
#'   Jorge, Azores, Portugal.
#'
#' @importFrom rlang .data
#' @export
create_grid <-
  function(x_range = range(azores.fkw::boat_trips_by_min$longitude),
           y_range = range(azores.fkw::boat_trips_by_min$latitude),
           utm_resolution = 5000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           x_offset = 0,
           y_offset = 0) {

    utm_grid <-
      create_utm_grid(
        x_range = x_range,
        y_range = y_range,
        utm_resolution = utm_resolution,
        x_pad_perc = x_pad_perc,
        y_pad_perc = y_pad_perc,
        x_offset = x_offset,
        y_offset = y_offset
      )

    # Define the UTM zone for your data
    utm_zone <- "+proj=utm +zone=26 +datum=WGS84 +units=m +no_defs"

    # Create an sf object from the data frame using the UTM zone
    utm_sf <- sf::st_as_sf(utm_grid, coords = c("x", "y"), crs = utm_zone)

    # Transform the sf object to the WGS84 projection
    wgs84_sf <- sf::st_transform(utm_sf, "+proj=longlat +datum=WGS84 +no_defs")

    # Transform the sf object to the WGS84 projection and convert to RasterLayer
    raster_layer <- raster::raster(wgs84_sf, ncol = attr(utm_grid, "nx"), nrow = attr(utm_grid, "ny"))

    raster_layer
  }

