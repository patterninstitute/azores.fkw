#' Load SECR fit results
#'
#' [load_secr_results()] returns SECR fit results.
#'
#' @return A list of several elements:
#' \describe{
#'  \item{`sightings`}{Sightings data frame, each observation is a sighting event.}
#'  \item{`grid`}{A RasterLayer object representing the grid, the region
#'  encompassing the study area, i.e., off the coast of Pico, Faial and São
#'  Jorge, Azores, Portugal.}
#'  \item{`effort`}{A data frame encoding the effort, i.e. how much time spent
#'  on a specific grid cell or detector.}
#'  \item{`occasions`}{A data frame of occasions from the columns in the effort data frame.}
#'  \item{`capture`}{A data frame in the format expected to be saved as a capture file.}
#'  \item{`habitat_mask`}{A habitat mask object for spatially explicit
#'  capture-recapture. See [`create_mask()`] for more details.}
#'  \item{`sightings_loc`}{Sightings' locations, i.e. the location of the nearest grid cell centroids.}
#'  \item{`fit01_params`}{SECR fit (estimated) parameters.}
#'  \item{`fit01_density`}{SECR fit (estimated) density.}
#'  \item{`fit01_lambda0`}{SECR fit (estimated) \eqn{\lambda_0}.}
#'  \item{`fit01_sigma`}{SECR fit (estimated) \eqn{\sigma}.}
#'  \item{`fit01_beta.vcv`}{TODO.}
#'  \item{`fit01_details`}{TODO}
#'  \item{`fit01_link`}{TODO}
#'  \item{`fit01_call`}{TODO}
#'  \item{`encounter_probability_fkw01_per_hour`}{TODO}
#'  \item{`session_info`}{R session info.}
#'  \item{`timestamp`}{Date and time of the SECR fitting.}
#' }
#'
#' @export
load_secr_results <- function() {
  file_path <- system.file("analysis/secr_results.rds", package = "azores.fkw")
  readRDS(file_path)
}
