#' Create a habitat mask
#'
#' [create_mask()] constructs a habitat mask object for spatially explicit
#' capture-recapture. A mask object is a set of points with optional attributes.
#' All arguments are optional, having defaults that match the specification
#' required for the analysis provided by this R compendium.
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
#' @param buffer Width of buffer in metres.
#' @param exclude_regions Regions to be excluded from the habitat mask. To be
#' supplied as a sf object. Coordinate units should be UTM.
#'
#' @return An object of class mask. See [make.mask()][secr::make.mask] for more
#'   details.
#' @export
create_mask <-
  function(x_range = range(azores.fkw::boat_trips_by_min$longitude),
           y_range = range(azores.fkw::boat_trips_by_min$latitude),
           utm_resolution = 1000,
           x_pad_perc = 0.05,
           y_pad_perc = 0.05,
           buffer = 12000,
           exclude_regions = NULL) {

    trapping_grid <-
      create_utm_grid(
        x_range = x_range,
        y_range = y_range,
        utm_resolution = utm_resolution,
        x_pad_perc = x_pad_perc,
        y_pad_perc = y_pad_perc,
        x_offset = -utm_resolution/2,
        y_offset = -utm_resolution/2
      )

    if (!is.null(exclude_regions)) {
    mask <-
      secr::make.mask(
        trapping_grid,
        nx = attr(trapping_grid, "nx"),
        ny = attr(trapping_grid, "ny"),
        spacing = utm_resolution,
        buffer = buffer,
        poly = exclude_regions,
        poly.habitat = FALSE
      )
    } else {
      mask <-
        secr::make.mask(
          trapping_grid,
          nx = attr(trapping_grid, "nx"),
          ny = attr(trapping_grid, "ny"),
          spacing = utm_resolution,
          buffer = buffer
        )
    }

    mask
  }
