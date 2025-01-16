#' Read tracks from GPX
#'
#' [read_tracks()] reads track data from GPX files.
#'
#' @param file A character vector of paths to GPX files.
#' @param id A character vector of values to distinguish each files' GPX track
#'   data. By default the name of the file (without extension) is used.
#' @param id_name The name of the extra column to distinguish each files' GPX
#'   track data.
#' @param tidy_names Whether to tidy up the column names of the output.
#' @param tidy_fn Function to be applied on the column names to make them tidy.
#' By default, it transforms the column names to snake case. This function
#' should take a character vector of input and return a character vector of
#' the same length as output.
#'
#' @return A [tibble][tibble::tibble-package] of track data.
#'
#' @export
read_tracks <-
  function(file,
           id = tools::file_path_sans_ext(basename(file)),
           id_name = "file",
           tidy_names = TRUE,
           tidy_fn = \(x) snakecase::to_snake_case(x, abbreviations = "ID")) {



    lst <- purrr::map(file, ~ read_tracks_(file = .x))
    lst <- rlang::set_names(lst, nm = id)
    df <- purrr::list_rbind(lst, names_to = id_name)
    tbl <- tibble::as_tibble(df)

    if (tidy_names) {
      colnames(tbl) <- tidy_fn(colnames(tbl))
    }

    tbl
}

read_tracks_ <- function(file) {
  purrr::list_rbind(gpx::read_gpx(file)$tracks, names_to = "track")
}
