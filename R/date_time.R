#' Convert month initials to indices
#'
#' [month_initials_to_indices()] converts month initials to indices.
#'
#' @param x A character vector of strings containing month initials to be replaced
#'   by indices.
#' @param initials A character vector of strings to be matched as month
#'   initials. By default assumes [month.abb].
#' @param indices A character vector of month indices. Default is `"01"`,
#'   `"02"`, etc..
#' @param ignore_case Whether to ignore case when matching the month initials
#' passed in `initials`. Default is `TRUE`.
#'
#' @return A character vector where month initials were replaced by respective
#'   month indices.
#'
#' @examples
#' month_initials_to_indices(c("Jan", "Jul", "Dec"))
#'
#' # By default the matching is case insensitive
#' month_initials_to_indices(c("JaN", "JuL", "dec"))
#'
#' # If case sensitive
#' month_initials_to_indices(c("Jan", "JuL", "dec"), ignore_case = FALSE)
#' @export
month_initials_to_indices <-
  function(x,
           initials = month.abb,
           indices = sprintf("%02d", 1:12),
           ignore_case = TRUE) {

  mgsub::mgsub(x,
               pattern = initials,
               replacement = indices,
               ignore.case = ignore_case)

}

#' Swap day with year
#'
#' [swap_day_with_year()] convert a substring representing a date in the format
#' DD-MM-YY to YYYY-MM-DD.
#'
#' @param x A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' swap_day_with_year("01-04-12 05:54:25 PM")
#' swap_day_with_year("31-10-13 12:51:10 PM")
#'
#' @export
swap_day_with_year <- function(x) {

  pattern <- "(^\\d{2})-(\\d{2})-(\\d{2})"
  replacement <- "20\\3-\\2-\\1"

  mgsub::mgsub(x, pattern = pattern, replacement = replacement)

}

#' Add seconds to date
#'
#' [add_seconds()] fills in the implicit zero seconds on date-times strings.
#'
#' @param x A character vector.
#' @return A character vector.
#'
#' @examples
#' add_seconds("2015-08-09 15:17")
#'
#' @export
add_seconds <- function(x) {

  pattern <- "([^:]\\d{2}):(\\d{2})$"
  replacement <- "\\1:\\2:00"

  mgsub::mgsub(x, pattern = pattern, replacement = replacement)

}

#' Convert from 12 to 24-hour time
#'
#' [hour12_to_hour24()] converts time string from 12 to 24-hour time.
#'
#' @param x A character vector whose elements are strings ending in the format
#' hh:mm:ss AM/PM.
#' @return A character vector where 1:00:00 PM is converted 13:00:00, 2:00:00 PM
#'   to 14:00:00, and so on. 12:00:00 PM remains 12:00:00. Times before noon,
#'   i.e. 10:00:00 AM remain the same, except for the AM that is dropped.
#'
#' @examples
#' hour12_to_hour24("2012-04-02 11:52:38 AM")
#' hour12_to_hour24("01-04-12 05:54:25 PM")
#' hour12_to_hour24("2012-10-03 12:54:08 PM")
#' hour12_to_hour24("2012-10-14 01:59:43 PM")
#' hour12_to_hour24("2013-07-06 10:33:12 AM")
#' @export
hour12_to_hour24 <- function(x) {

  pattern = c(
    sprintf("(.+%02d:\\d{2}:\\d{2}) AM$", 0:12),
    sprintf("(.+)(%02d)(:\\d{2}:\\d{2}) PM$", 0:12)
  )

  replacement <- c(rep("\\1", 13),
                   c("\\1{12}\\3", sprintf("\\1{%02d}\\3", 1:11 + 12), "\\1{12}\\3"))

  x1 <- mgsub::mgsub(x, pattern = pattern, replacement = replacement)

  # Remove braces
  mgsub::mgsub(x1, pattern = "[{}]", replacement = "")

}

pt_month_initials <- function(case = c("titlecase", "uppercase", "lowercase")) {

  case <- match.arg(case)

  mo_initials <-
  c("Jan",
    "Fev",
    "Mar",
    "Abr",
    "Mai",
    "Jun",
    "Jul",
    "Ago",
    "Set",
    "Out",
    "Nov",
    "Dez")

  if (identical(case, "titlecase")) return(mo_initials)
  if (identical(case, "uppercase")) return(toupper(mo_initials))
  if (identical(case, "lowercase")) return(tolower(mo_initials))

}

en_month_initials <- function(case = c("titlecase", "uppercase", "lowercase")) {

  case <- match.arg(case)

  mo_initials <-
    c("Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec")

  if (identical(case, "titlecase")) return(mo_initials)
  if (identical(case, "uppercase")) return(toupper(mo_initials))
  if (identical(case, "lowercase")) return(tolower(mo_initials))

}

#' Harmonize date and time
#'
#' [harmonise_date_time()] takes strings containing a date and time and returns
#' the same date and time always formatted in same way: `"YYYY-MM-DD hh:mm:ss"`,
#' e.g. `"2014-05-31 17:57:35"`. It attempts to apply the following
#' transformations:
#' - Uses [month_initials_to_indices()] to convert month initials to indices,
#'   e.g. `"JUL"` to `"07"`. It works both with English and Portuguese month
#'   initials, e.g. both `"MAI"` and `"MAY"` get converted to `"05"`.
#' - Uses [swap_day_with_year()] to convert DD-MM-YY to YYYY-MM-DD, e.g.
#'   `"01-04-12"` to `"2012-04-01"`.
#' - Uses [add_seconds()] to complete those strings missing the seconds components from
#'   the time strings, e.g. `"15:17"` to `"15:17:00"`.
#' - Uses [hour12_to_hour24()] to convert, e.g., `"2012-04-02 11:52:38 AM"` to
#'   `"2012-04-02 11:52:38"` or `"01-04-12 05:54:25 PM"` to
#'   `"01-04-12 17:54:25"`.
#'
#' @param x A character vector of date time strings.
#'
#' @return A character vector.
#'
#' @export
harmonise_date_time <- function(x) {

  # Month initials
  indices <- sprintf("%02d", rep(1:12, 2))

  # Month abbreviations
  initials <- c(pt_month_initials("uppercase"), en_month_initials("uppercase"))

  month_initials_to_indices(x, initials, indices) |>
    swap_day_with_year() |>
    add_seconds() |>
    hour12_to_hour24()
}

#' Assess date time format
#'
#' [`is_date_time()`] is an internal function used to assess if certain date-time variables
#' follow the format `"%Y-%m-%d %H:%M:%S"`.
#'
#' @param x A character vector of date time values to be parsed.
#'
#' @returns A logical vector.
#'
#' @keywords internal
#' @export
is_date_time <- function(x) {
  date_time_format <- "%Y-%m-%d %H:%M:%S"
  is_dttm_str <- is.na(lubridate::parse_date_time(x, orders = date_time_format, quiet = TRUE))
  return(!is_dttm_str)
}
