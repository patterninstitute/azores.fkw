% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sessions.R
\name{month}
\alias{month}
\title{Get the month from date-time}
\usage{
month(dat, dttm_var = "time")
}
\arguments{
\item{dat}{A data frame.}

\item{dttm_var}{A column name in \code{dat} identifying a date-time variable.}
}
\value{
A character vector of month values.
}
\description{
\code{\link[=month]{month()}} uses a date-time column of \code{dat} whose name is passed in \code{dttm_var}
to extract the month as character.
}
\examples{
track_example <-
  boat_trips |>
    dplyr::group_by(year) |>
    dplyr::arrange(trip) |>
    dplyr::slice(1:3) |>
    dplyr::ungroup()

track_example

# Extract months from the column `time`.
month(track_example)

}
