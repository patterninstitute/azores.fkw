#' Write effort to a file
#'
#' [write_effort()] writes a text file with a [tibble][tibble::tibble-package]
#' representing the effort.
#'
#' @param x A [tibble][tibble::tibble-package] encoding the effort. See the
#'   function [effort()] on how to generate this tibble.
#' @param file A path to a file to be exported.
#' @param detector_col Name of the detector column.
#' @param x_col Name of the column indicating the X position.
#' @param y_col Name of the column indicating the Y position.
#'
#' @return Returns the path passed in `file`.
#' @export
write_effort <- function(x,
                         file,
                         detector_col = "index",
                         x_col = "easting",
                         y_col = "northing") {
  readr::write_lines(x = "# Detector X Y Effort", file)

  x_pos <- dplyr::select(x, dplyr::all_of(c(detector_col, x_col, y_col)))
  x_effort <- dplyr::select(x, dplyr::matches("^\\d{4}"))

  x_for_export <- dplyr::bind_cols(x_pos, x_effort)

  readr::write_delim(
    x = x_for_export,
    file = file,
    col_names = FALSE,
    append = TRUE
  )

  return(file)
}
