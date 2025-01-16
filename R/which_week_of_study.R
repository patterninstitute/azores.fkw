#' Find study week for query dates
#'
#' @description
#'
#' [which_week_of_study()] finds the week of study to which the queried dates
#' (`dttm`) match. So this function allows us to assign a week to specific
#' dates.
#'
#' By week of study we mean the number of the week starting in 2012 until the
#' end of 2017.
#'
#' @param dttm A date or date-time vector of points in time to be assigned
#' a week of study.
#'
#' @returns A character vector of elements of the form `"week n"`, where `n` is
#' the number of the week. Note that `"week 1"` is the first week of 2012, and
#' week `"week 313"` is the last week of 2017.
#'
#' @export
which_week_of_study <- function(dttm) {
  first_day <- lubridate::ymd("20120101")
  # 313 weeks ~ 6 years
  date_bins <- first_day + lubridate::weeks(0:313)
  week_intervals <- lubridate::int_diff(date_bins)
  # week_ids <- paste("week", seq_along(week_intervals))
  week_ids <- sprintf("week %03d", seq_along(week_intervals))

  week_indices <- which_interval(dttm, week_intervals)
  week_ids[week_indices]
}

#' @importFrom lubridate %within%
which_interval_ <- function(dttm, intervals) {

  interval_index <- which(dttm %within% intervals)
  if (length(interval_index) == 0L) {
    interval_index <- NA_integer_
  }

  interval_index
}

which_interval <- function(dttm, intervals) {
  sapply(dttm, which_interval_, intervals = intervals)
}
