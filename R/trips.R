#' Create a trips table
#'
#' [trips()] generates a tibble where each row is for a trip.
#'
#' @param boat_effort_per_trip Boat effort per trip. See [boat_effort_per_trip()].
#'
#' @returns A tibble with the following columns: `primary_session`,
#'   `secondary_session`, `trip_id`, `trip`, `date` and `time`.
#'
#' @export
trips <- function(boat_effort_per_trip = azores.fkw::boat_effort_per_trip()) {

  # All columns except `index`, `easting`/`longitude` and `northing`/`latitude`.
  trips <- colnames(boat_effort_per_trip)[-(1:3)]

  trips <-
    tibble::tibble(trip = trips) |>
    tibble::rowid_to_column("trip_id") |>
    dplyr::mutate(
      date = lubridate::as_date(.data$trip),
      time = hms::as_hms(lubridate::ymd_hms(.data$trip))
    )

  trips |>
    add_primary_session() |>
    add_secondary_session()

}

# @export
# trips <- function(boat_effort_per_sec_session = azores.fkw::boat_effort_per_week()) {
#
#   # All columns except `index`, `easting`/`longitude` and `northing`/`latitude`.
#   sec_sessions <- colnames(boat_effort_per_sec_session)[-(1:3)]
#
#   trips <-
#     tibble::tibble(trip = trips) |>
#     tibble::rowid_to_column("trip_id") |>
#     dplyr::mutate(
#       date = lubridate::as_date(.data$trip),
#       time = hms::as_hms(lubridate::ymd_hms(.data$trip))
#     )
#
#   trips |>
#     add_primary_session() |>
#     add_secondary_session()
#
# }

#' Defines the primary session
#'
#' [add_primary_session()] adds a primary session column (`primary_session`)
#' where the year is used to encode each primary session. Only trips falling
#' within the range of March and November get assgined a primary session
#' value which is the corresponding year; other dates get assigned to `NA`.
#'
#' @param trips A tibble containinig, at least, a column named `date` (trip
#'   date), whose rows are each individual trip.
#'
#' @returns A tibble with a new column named `primary_session`.
#'
#' @export
add_primary_session <- function(trips = azores.fkw::trips()) {

  start_month <- 3
  end_month <- 11

  trips |>
    dplyr::mutate(primary_session = dplyr::if_else(
      lubridate::month(.data$date) >= start_month &
        lubridate::month(.data$date) <= end_month,
      as.character(lubridate::year(.data$date)),
      NA_character_
    ),
    .before = 1L)

}

#' Defines the secondary session
#'
#' [add_secondary_session()] adds a secondary session column
#' (`secondary_session`) which is equal to the values in the column `trip_id`.
#'
#' @param trips_with_prm_sessions A tibble with trips (each row), and containing the column `trip_id`.
#'
#' @returns A tibble with a new column named `secondary_session`.
#'
#' @importFrom rlang .data
#' @export
add_secondary_session <- function(trips_with_prm_sessions = azores.fkw::add_primary_session()) {

  trips_with_prm_sessions |>
    dplyr::mutate(secondary_session = .data$trip_id) |>
    dplyr::relocate("secondary_session", .after = "primary_session")
}

# add_secondary_session <- function(trips_with_prm_sessions = azores.fkw::add_primary_session()) {
#   start_date <- lubridate::ymd("2012-03-01")
#   end_date <- lubridate::ymd("2012-11-30")
#   date_sequence <- seq(start_date, end_date, by = "day")
#
#   secondary_session_within_a_year <-
#     tibble::tibble(
#       date_sequence,
#       month = lubridate::month(date_sequence),
#       day = lubridate::day(date_sequence)
#     ) |>
#     tibble::rowid_to_column("secondary_session") |>
#     dplyr::select(-"date_sequence")
#
#   trips_with_prm_sessions |>
#     dplyr::mutate(month = lubridate::month(date), day = lubridate::day(date)) |>
#     dplyr::left_join(secondary_session_within_a_year, by = c("month", "day")) |>
#     dplyr::relocate("secondary_session", .after = "primary_session") |>
#     dplyr::select(-c("month", "day"))
#
# }

