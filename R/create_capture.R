#' Create capture data table
#'
#' [create_capture()] takes the output from [create_capture_data()] and renames
#' and selects columns needed for creating a capture file as required by
#' [secr::read.capthist()].
#'
#' @param capture_data Capture data as a table, see [create_capture_data()] for
#'   format details.
#'
#' @returns A table with capture data and four columns: `Session`, `ID`,
#'   `Occasion` and `Detector`.
#'
#' @export
create_capture <- function(capture_data = create_capture_data()) {

  capture_data |>
    dplyr::rename(
      Session = "primary_session",
      ID = "individual",
      Occasion = "secondary_session",
      Detector = "index"
      ) |>
    dplyr::select(c("Session", "ID", "Occasion", "Detector")) |>
    dplyr::arrange(.data$ID)

}

#' Create capture data table
#'
#' [create_capture_data()] creates a table of unique sighting events of each
#' individual.
#'
#' @param individuals A table listing all sighted indivuals. See [individuals] for format.
#' @param trips A table of trips. See [trips()] for format.
#' @param grid A RasterLayer object providing the grid that is used to match
#' sighting locations.
#'
#' @returns A table with the following columns: `individual`, `index`,
#'   `trip_id`, `primary_session`, `secondary_session`, `trip`, `date` and
#'   `time`.
#'
#' @export
create_capture_data <-
  function(individuals = azores.fkw::individuals,
           trips = azores.fkw::trips(),
           grid = azores.fkw::create_grid()
  ) {


    detectors <- find_grid_cells(points = individuals, grid = grid)

    sighting_to_trip <- individuals["initial_time"]
    for (i in seq_len(nrow(sighting_to_trip))) {

      date <- dplyr::pull(sighting_to_trip[i, "initial_time"])
      sighting_date <- lubridate::as_date(date)
      sighting_time <- hms::as_hms(lubridate::ymd_hms(date))

      is_same_day <- sighting_date == trips$date
      trip_time <- dplyr::pull(trips[is_same_day, "time"])
      time_diff <- trip_time - sighting_time

      if (all(!is_same_day)) {
        sighting_to_trip[i, "trip_id"] <- NA_real_
        sighting_to_trip[i, "trip"] <- lubridate::as_datetime(NA)
      } else {
        matched_trip <- trips[is_same_day, ][which_min_duration(time_diff), ]
        sighting_to_trip[i, "trip_id"] <- matched_trip$trip_id
        sighting_to_trip[i, "trip"] <- matched_trip$trip
      }
    }

    dplyr::bind_cols(individuals["individual"], detectors["index"], sighting_to_trip["trip_id"]) |>
      dplyr::left_join(trips, by = "trip_id")
  }
