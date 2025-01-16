#' Get the year from date-time
#'
#' [year()] uses a date-time column of `dat` whose name is passed in `dttm_var`
#' to extract the year as character.
#'
#' @param dat A data frame.
#' @param dttm_var A column name in `dat` identifying a date-time variable.
#'
#' @returns A character vector of year values.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Extract years from the column `time`.
#' year(track_example)
#'
#' @export
year <- function(dat, dttm_var = "time") {
  sprintf("%04d", lubridate::year(dat[[dttm_var]]))
}

#' Get the month from date-time
#'
#' [month()] uses a date-time column of `dat` whose name is passed in `dttm_var`
#' to extract the month as character.
#'
#' @param dat A data frame.
#' @param dttm_var A column name in `dat` identifying a date-time variable.
#'
#' @returns A character vector of month values.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Extract months from the column `time`.
#' month(track_example)
#'
#' @export
month <- function(dat, dttm_var = "time") {
  sprintf("%02d", lubridate::month(dat[[dttm_var]]))
}

#' Get the week from date-time
#'
#' [week()] uses a date-time column of `dat` whose name is passed in `dttm_var`
#' to extract the week as character.
#'
#' @param dat A data frame.
#' @param dttm_var A column name in `dat` identifying a date-time variable.
#'
#' @returns A character vector of week values.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Extract weeks from the column `time`.
#' week(track_example)
#'
#' @export
week <- function(dat, dttm_var = "time") {
  sprintf("%02d", lubridate::week(dat[[dttm_var]]))
}

#' Get the day of the month from date-time
#'
#' [mday()] uses a date-time column of `dat` whose name is passed in `dttm_var`
#' to extract the day of the month as character.
#'
#' @param dat A data frame.
#' @param dttm_var A column name in `dat` identifying a date-time variable.
#'
#' @returns A character vector of day of the month values.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Extract day of the months from the column `time`.
#' mday(track_example)
#'
#' @export
mday <- function(dat, dttm_var = "time") {
  sprintf("%02d", lubridate::day(dat[[dttm_var]]))
}

#' Get the day of the year from date-time
#'
#' [yday()] uses a date-time column of `dat` whose name is passed in `dttm_var`
#' to extract the day of the year as character.
#'
#' @param dat A data frame.
#' @param dttm_var A column name in `dat` identifying a date-time variable.
#'
#' @returns A character vector of day of the year values.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Extract day of the years from the column `time`.
#' yday(track_example)
#'
#' @export
yday <- function(dat, dttm_var = "time") {
  sprintf("%03d", lubridate::yday(dat[[dttm_var]]))
}

#' Add session annotation
#'
#' @description
#'
#' [add_sessions()] adds the primary and secondary sessions to an existing data
#' frame of track data.
#'
#' This function takes two other functions, one per session type:
#'
#' - `psession_fn`: This function should be a function taking as first argument
#' the data frame `tracks` and should return a character vector of values
#' indicating the primary session values. This vector length should match the
#' number of rows in `tracks`. This vector becomes a new column in the resulting
#' output whose name is determined by the value in `psession_var`.
#' This defaults to the [year()] function which looks for a date-time variable
#' named `time` and sets the primary session as the extracted year from `time`.
#'
#' - `ssession_fn`: This function should be a function taking as first argument
#' the data frame `tracks` and should return a character vector of values
#' indicating the secondary session values. This vector length should match the
#' number of rows in `tracks`. This vector becomes a new column in the resulting
#' output whose name is determined by the value in `ssession_var`.
#' This defaults to the [week()] function which looks for a date-time variable
#' named `time` and sets the secondary session as the extracted week from
#' `time`.
#'
#' @param tracks A data frame of track data: each row should be for a track
#' point in space and time. The actual columns and their names may vary but the
#' functions passed onto `psession_fn` and `ssession_fn` need to be aware of
#' the available variables in `tracks` if that is needed to determine the
#' primary and secondary sessions.
#'
#' @param psession_fn A function taking as first argument the `tracks` data
#'   frame, whose output should be a character vector of primary session values,
#'   whose length should match the number of rows `tracks`.
#'
#' @param ssession_fn A function taking as first argument the `tracks` data
#'   frame, whose output should be a character vector of secondary session
#'   values, whose length should match the number of rows `tracks`.
#'
#' @param psession_var A character value with the name of the new column storing
#' the primary session values, i.e. the vector resulting from a call to
#' the function passed in `psession_fn`.
#'
#' @param ssession_var A character value with the name of the new column storing
#' the secondary session values, i.e. the vector resulting from a call to
#' the function passed in `ssession_fn`.
#'
#' @returns The `tracks` data frame with two new columns (values are the ones
#'   passed in `psession_var` and `ssession_var`). By default the two new
#'   columns are `primary_session` and `secondary_session`.
#'
#' @examples
#' track_example <-
#'   boat_trips |>
#'     dplyr::group_by(year) |>
#'     dplyr::arrange(trip) |>
#'     dplyr::slice(1:3) |>
#'     dplyr::ungroup()
#'
#' track_example
#'
#' # Set the primary session yearly
#' # and the secondary session weekly
#' add_sessions(track_example)
#'
#' @importFrom rlang :=
#' @export
add_sessions <- function(tracks,
                         psession_fn = year,
                         ssession_fn = week,
                         psession_var = "primary_session",
                         ssession_var = "secondary_session") {

  if (!is.data.frame(tracks))
    cli::cli_abort("{.var tracks} must be a data frame.")

  if (!is.function(psession_fn))
    cli::cli_abort("{.var psession_fn} must be a function.")

  if (!is.function(ssession_fn))
    cli::cli_abort("{.var ssession_fn} must be a function.")

  tracks |>
    dplyr::mutate(
      "{psession_var}" := psession_fn(.data),
      "{ssession_var}" := ssession_fn(.data)
    )
}
