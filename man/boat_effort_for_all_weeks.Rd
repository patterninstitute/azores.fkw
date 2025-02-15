% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effort_by_week.R
\name{boat_effort_for_all_weeks}
\alias{boat_effort_for_all_weeks}
\title{Total boat effort per grid point}
\usage{
boat_effort_for_all_weeks(
  boat_effort_per_week = azores.fkw::boat_effort_per_week()
)
}
\arguments{
\item{boat_effort_per_week}{A tibble as output by \code{\link[=boat_effort_per_week]{boat_effort_per_week()}}.}
}
\value{
A tibble of four columns: \code{index} (index of the grid cell /
detector), UTM coordinates (\code{easting} and \code{northing}), and the total (sum)
boat effort per week per grid point (\code{boat_effort}).
}
\description{
\code{\link[=boat_effort_for_all_weeks]{boat_effort_for_all_weeks()}} calculates the total boat effort per grid
point. This function takes the results per week as generated by
\code{\link[=boat_effort_per_week]{boat_effort_per_week()}} and calculates the sum of all boat efforts across
all weeks per grid cell / detector.
}
\examples{
boat_effort_for_all_weeks()

}
