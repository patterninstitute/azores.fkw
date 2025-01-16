#' Latitude and longitude by minute
#'
#' [mean_pos_by_min()] takes a [tibble][tibble::tibble-package] with track data
#' (boat trips) and computes the mean latitude and longitude by aggregating
#' coordinates by the minute. The following columns must exist in `trips`:
#' \describe{
#' \item{`time`}{A datetime column, used for grouping by the minute.}
#' \item{`latitude`}{Latitude in decimal degrees.}
#' \item{`longitude`}{Longitude in decimal degrees.}
#' }
#'
#' @param trips A [tibble][tibble::tibble-package] of track data, each row is a
#'   position in time and space. By default uses [azores.fkw::boat_trips]
#'   bundled with this package.
#'
#' @importFrom rlang .data
#' @export
mean_pos_by_min <- function(trips = azores.fkw::boat_trips) {

  trips_by_min <-
    dplyr::mutate(
      trips,
      year = as.integer(lubridate::year(.data$time)),
      month = as.integer(lubridate::month(.data$time)),
      day = as.integer(lubridate::day(.data$time)),
      hour = as.integer(lubridate::hour(.data$time)),
      min = as.integer(lubridate::minute(.data$time))
    ) |>
    dplyr::group_by(.data$trip, .data$year, .data$month, .data$day, .data$hour, .data$min) |>
    dplyr::summarise(latitude = mean(.data$latitude), longitude = mean(.data$longitude), .groups = "drop")

  trips_by_min
}
