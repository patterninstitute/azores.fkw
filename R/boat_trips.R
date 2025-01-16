#' Boat trips
#'
#' Track data of boat while performing sightseeing in the Azores' study region.
#' [`boat_trips`] provides a [tibble][tibble::tibble-package] of the raw track
#' data where each row is a tracked point. [`boat_trips_by_min`] is an
#' aggregated version by minute. These data sets are used to define the survey
#' effort in this R compendium.
#'
#' @format `boat_trips`:
#'
#' - `year`: Year (integer).
#' - `trip`: Name of the trip.
#' - `time`: Date time.
#' - `latitude`: Latitude in decimal degrees.
#' - `longitude`: Longitude in decimal degrees.
#'
#' @examples
#' boat_trips
#'
#' boat_trips_by_min
#'
#' @source The raw data underlying these tidy data sets is a set of GPX files
#'   (one per year) provided as an archive in
#'   \url{https://github.com/patterninstitute/azores.fkw/blob/main/data-raw/gpx.zip}.
#'
"boat_trips"

#' @format `boat_trips_by_min`:
#'
#' - `trip`: Name of the trip.
#' - `year`: Year (integer).
#' - `month`: Month (integer).
#' - `day`: Month day (integer).
#' - `hour`: Hour in 24h format.
#' - `min`: Minute.
#' - `latitude`: Latitude in decimal degrees.
#' - `longitude`: Longitude in decimal degrees.
#'
#' @rdname boat_trips
"boat_trips_by_min"
