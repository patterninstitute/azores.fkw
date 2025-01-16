#' Caphist interval
#'
#' [caphist_interval()] a helper for the creation of the capthist attribute
#' `intervals`. See section _Data structure and input_ on
#' `vignette("openCR-vignette", package = "openCR")`.
#'
#' @param x An integer vector of consecutive streaks of integer numbers. Each
#'   streak of the same number represents a sequence of secondary sessions
#'   belonging to the same primary session.
#'
#' @examples
#' # One primary session only with three secondary sessions.
#' # The last primary session is always comprising of a streak of 0's in the
#' # output. This is the case in this interval.
#' caphist_interval(c(1, 1, 1))
#'
#' # Two primary sessions, each with two secondary sessions.
#' # Note that the 1's signal the last secondary session in a primary session.
#' # This is true except for the last primary session, which is a streak of 0's
#' # without a 1 at the end.
#' caphist_interval(c(1, 1, 2, 2))
#'
#' @export
caphist_interval <- function(x) {
  if (length(x) <= 1L) {
    return(integer())
  }

  # diff(c(x, x[length(x)]))
  diff(x)
}

#' Determine the caphist intervals
#'
#' [find_caphist_intervals()] takes a data frame of secondary sessions (rows),
#' and two columns: `primary_session` and `secondary_session`; and finds the
#' caphist interval vector. See [caphist_interval()].
#'
#' @param df A data frame of two columns `primary_session` and
#'   `secondary_session`.
#'
#' @returns A data frame with the same data as input `df` with the added
#'   columns:
#'  - `primary_session_index`: An index identifying the primary session
#'  associated with the secondary session at this row.
#'  - `caphist_interval`: A integer vector of 0's and 1's to be used for
#'  assignment of the attribute `intervals` of a caphist object.
#'
#' @export
find_caphist_intervals <- function(df) {
  df |>
    dplyr::arrange(.data$primary_session, .data$secondary_session) |>
    dplyr::group_by(.data$primary_session) |>
    dplyr::mutate(primary_session_index = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(caphist_interval = c(caphist_interval(.data$primary_session_index), NA_integer_))
}
