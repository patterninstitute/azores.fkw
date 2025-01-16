#' False killer whale sightings
#'
#' @description
#'
#' - [`sightings`] is a data set of false killer whale sighting events. At each
#' sighting event one or more whales may be spotted.
#' - [`ethology`] is a data set of observed whale behaviors during each sighting.
#' - [`pod_age_composition`] is a data set of the pod age composition for each
#' sighting. Each row is for a sighting/age group combination.
#' - [`reaction_to_boat`] is a data set of the reactions of whales towards the
#' tour boat.
#' - [`individuals`] is a data set of the photo-identified false killer whale
#' individuals. Each row is for a whale (`individual`) identified at a
#' specific sighting event (`sighting_id`).
#'
#' @format [`sightings`] is a data frame with eight columns:
#' \describe{
#'   \item{`sighting_id`}{Identifier of the sighting event.}
#'   \item{`latitude`}{Latitude in decimal degrees.}
#'   \item{`longitude`}{Longitude in decimal degrees.}
#'   \item{`date`}{Date of the sighting event.}
#'   \item{`initial_time`}{Date-time of the beginning of the sighting.}
#'   \item{`final_time`}{Date-time of the end of the sighting.}
#'   \item{`other_species`}{Other species spotted at the same sighting event.}
#'   \item{`individuals`}{Semi-colon separated list of photo-identified
#'   individuals at each sighting.}
#' }
#'
#' @examples
#' sightings
#'
"sightings"

#' @format [`ethology`] is a data frame with two columns:
#' \describe{
#'   \item{`sighting_id`}{Identifier of the sighting event.}
#'   \item{`behavior`}{Whale behavior category with regards to movement,
#'   socialization, interaction with the boat, feeding and diving.}
#' }
#'
#' @examples
#' ethology
#'
#' @rdname sightings
"ethology"

#' @format [`pod_age_composition`] is a data frame with three columns:
#' \describe{
#'   \item{`sighting_id`}{Identifier of the sighting event.}
#'   \item{`age_group`}{Age category: newborn, calf, juvenile, adult or other.}
#'   \item{`n`}{Number of individuals.}
#' }
#'
#' @examples
#' pod_age_composition
#'
#' @rdname sightings
#'
"pod_age_composition"

#' @format [`reaction_to_boat`] is a data frame with two columns:
#' \describe{
#'   \item{`sighting_id`}{Identifier of the sighting event.}
#'   \item{`reaction`}{Whale reaction towards tour boat.}
#' }
#'
#' @examples
#' reaction_to_boat
#'
#' @rdname sightings
"reaction_to_boat"

#' @format [`individuals`] is a data frame with eight columns:
#' \describe{
#'   \item{`individuals`}{An identifier for each identified false killer whale.
#'   The prefix `"Pc"` is the abbreviation of _Pseudorca crassidens_.}
#'   \item{`sighting_id`}{Identifier of the sighting event.}
#'   \item{`latitude`}{Latitude in decimal degrees.}
#'   \item{`longitude`}{Longitude in decimal degrees.}
#'   \item{`date`}{Date of the sighting event.}
#'   \item{`initial_time`}{Date-time of the beginning of the sighting.}
#'   \item{`final_time`}{Date-time of the end of the sighting.}
#'   \item{`other_species`}{Other species spotted at the same sighting event.}
#' }
#'
#' @examples
#' individuals
#'
#' @rdname sightings
#'
"individuals"

#' Azores bathymetry
#'
#' Bathymetry of the Azores region in long format, i.e. each row is a spatial
#' point.
#'
#' @format [`azores_bathymetry`] is a data frame with three columns:
#' \describe{
#'   \item{`x`}{Easting (x) coordinate, in metres, UTM Zone 26N.}
#'   \item{`y`}{Northing (y) coordinate, in metres, UTM Zone 26N.}
#'   \item{`depth`}{Depth, in metres.}
#' }
#'
#' @examples
#' azores_bathymetry
#'
"azores_bathymetry"
