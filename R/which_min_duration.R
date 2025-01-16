#' Where is the minimum duration
#'
#' [which_min_duration()] takes a numeric vector of durations and returns
#' the minimum (strictly) positive duration.
#'
#' @param duration A numeric vector.
#'
#' @return A logical vector.
#'
#' @export
which_min_duration <- function(duration) {
  min(duration[duration > 0]) == duration
}
