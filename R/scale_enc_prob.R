#' Scale encounter probability
#'
#' [scale_enc_prob()] scales the encounter probability for a period that is `n`
#' times as big as the period that `p` refers to.
#'
#' @param p Base encounter probability.
#' @param n Period multiple factor.
#'
#' @examples
#' # Assuming `p` refers to an encounter probability for a duration of 1 minute
#' # Then the probability over one hour (n = 60 times 1 minute) would be given
#' # by:
#' p <- 0.1
#' scale_enc_prob(p = p, n = 60)
#'
#' # Note that increasing the period duration increases the encounter
#' # probability in a non-linear fashion, eventually saturating at probability
#' # 1 for a large `n`.
#'
#' data <-
#' tibble::tibble(
#'   n = 1:100,
#'   p = 1-(1-0.1)^n
#' )
#'
#' data |>
#'  ggplot2::ggplot(ggplot2::aes(x = n, y = p)) +
#'  ggplot2::geom_line() +
#'  ggplot2::ylim(0, 1)
#'
#' @export
scale_enc_prob <- function(p, n) {
  1 - (1 - p) ^ n
}
