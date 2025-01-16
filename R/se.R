#' Standard error of the mean
#'
#' [`se()`] computes the standard error of the mean.
#'
#' @param x A numeric vector.
#' @param na.rm Whether to remove `NA`.
#'
#' @return A numeric scalar.
#' @export
se <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- stats::na.omit(x)
  sqrt(stats::var(x) / length(x))
}
