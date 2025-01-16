#' Percentage
#'
#' [`perc()`] returns numbers formatted as percentages.
#'
#' @param x A numeric vector.
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal
#'   places of precision. If NULL, the default, uses a heuristic that should
#'   ensure breaks have the minimum number of digits needed to show the
#'   difference between adjacent values.
#' @param scale A scaling factor: `x` will be multiplied by scale before
#'   formatting. This is useful if the underlying data is very small or very
#'   large.
#' @param prefix Additional text to display before the number. The suffix is
#'   applied to absolute value before `style_positive` and `style_negative` are
#'   processed so that `prefix = "$"` will yield (e.g.) `-$1` and (`$1`).
#' @param suffix Additional text to display after the number.
#' @param big.mark Character used between every 3 digits to separate thousands.
#' @param decimal.mark The character to be used to indicate the numeric decimal
#'   point.
#' @param trim Logical, if `FALSE`, values are right-justified to a common width
#'   (see `base::format()`).
#' @param ... Other arguments passed on to `base::format()`.
#'
#' @return A character vector of percentage formatted numbers.
#'
#' @export
perc <- function(x,
                 accuracy = NULL,
                 scale = 100,
                 prefix = "",
                 suffix = "%",
                 big.mark = ",",
                 decimal.mark = ".",
                 trim = TRUE,
                 ...) {
  scales::label_percent(
    accuracy = NULL,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}
