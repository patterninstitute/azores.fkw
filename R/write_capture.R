#' Default capture file header
#'
#' [`default_cap_header`] returns a character vector of text lines to be used
#' as header of the capture data file.
#'
#' @returns A character vector of text lines to be used as header of the capture
#'   data file.
#'
#' @keywords internal
#' @export
default_cap_header <- function() {
  c("False killer whale sightings",
    "Session ID Occasion Detector")
}

#' Write capture to file
#'
#' [write_capture()] writes a capture [tibble][tibble::tibble-package] to a text
#' file.
#'
#' @param x A [tibble][tibble::tibble-package] containing capture (secr) related
#'   information, namely the four columns: `Session` (block of sampling that may
#'   be treated as independent), `ID` (individual identifier), `Occasion`
#'   (occasion identifier) and `Detector` (detector identifier). Each row
#'   identifies correspond to an individual (`ID`) seen at a certain time
#'   (`Occasion`) on a certain location (`Detector`).
#' @param file The path to a file to saved.
#' @param header A character vector to be used as header in the file. Do not
#' include the `"#"` as these are added automatically to every line. Defaults to
#' the value returned by [default_cap_header()].
#'
#' @return Returns the path passed in `file`.
#'
#' @export
write_capture <- function(x, file, header = default_cap_header()) {

  header <- paste("#", header)
  readr::write_lines(x = header, file)
  readr::write_delim(
    x = x,
    file = file,
    col_names = FALSE,
    append = TRUE
  )

  return(file)
}
