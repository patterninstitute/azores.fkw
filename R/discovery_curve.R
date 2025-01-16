#' Individuals per year
#'
#' [individuals_per_year()] takes a data set of individuals (whales) identified
#' at a specific sighting event (`sighting_id`) and counts how many different
#' individuals were identified per year.
#'
#' @param individuals A tibble with the same structure as
#'   [individuals][azores.fkw::individuals] (this is the default).
#'
#' @returns A tibble of two columns:
#' - `year`: Study year, as a character.
#' - `n`: Number of identified individuals (identified whale animals).
#'
#' @examples
#' individuals_per_year()
#'
#' @seealso [new_individuals_per_year()] for newly identified individuals per
#'   year.
#'
#' @importFrom rlang .data
#' @export
individuals_per_year <- function(individuals = azores.fkw::individuals) {
  individuals |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::distinct(.data$individual, .data$year) |>
    dplyr::count(.data$year) |>
    dplyr::mutate(year = as.character(.data$year))
}

#' Newly identified individuals per year
#'
#' [new_individuals_per_year()] takes a data set of individuals (whales)
#' identified at a specific sighting event (`sighting_id`) and reports the newly
#' identified individuals across the years.
#'
#' @param individuals A tibble with the same structure as
#'   [individuals][azores.fkw::individuals] (this is the default).
#'
#' @returns A list of newly identifier individuals. This list has as many
#'   elements as the number of years comprising the study duration. For each
#'   element, i.e. year, the newly identified set of individuals is provided as
#'   a character vector of individual ids (`individual`).
#'
#' @examples
#' new_individuals_per_year()
#'
#' @seealso [individuals_per_year()].
#'
#' @importFrom rlang .data
#' @export
new_individuals_per_year <- function(individuals = azores.fkw::individuals) {
  individuals_year <-
    individuals |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::distinct(.data$individual, .data$year)

  years <-
    individuals_year |>
    dplyr::group_by(.data$year) |>
    dplyr::group_keys() |>
    dplyr::pull()

  cumulative_of_individuals <-
    individuals_year |>
    dplyr::group_split(.data$year) |>
    purrr::accumulate(.f = dplyr::bind_rows) |>
    purrr::map(.f = ~ unique(.x$individual)) |>
    purrr::set_names(as.character(years))

  prior_year_individuals <- list(character())
  names(prior_year_individuals) <- as.character(years[1] - 1L)

  cumulative_of_individuals_lagged <- c(prior_year_individuals, cumulative_of_individuals[-length(cumulative_of_individuals)])
  purrr::map2(cumulative_of_individuals, cumulative_of_individuals_lagged, .f = ~ setdiff(.x, .y))
}


#' Number of newly identified individuals per year
#'
#' [count_of_new_individuals_per_year()] takes a data set of individuals
#' (whales) identified at a specific sighting event (`sighting_id`) and counts
#' how many individuals were newly identified each year.
#'
#' @param individuals A tibble with the same structure as
#'   [individuals][azores.fkw::individuals] (this is the default).
#'
#' @returns A tibble of two columns:
#' - `year`: Study year, as a character.
#' - `n`: Number of newly identified individuals (identified whale animals).
#'
#' @examples
#' count_of_new_individuals_per_year()
#'
#' @seealso [individuals_per_year()] [new_individuals_per_year()].
#'
#' @importFrom rlang .data
#' @export
count_of_new_individuals_per_year <- function(individuals = azores.fkw::individuals) {
  # A list of new sighted individuals per year
  new_ind <- new_individuals_per_year(individuals)
  counts <- sapply(new_ind, length)

  tibble::tibble(year = names(counts), n = counts)
}
