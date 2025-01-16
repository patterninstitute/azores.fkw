#' Fish catch
#'
#' A data set obtained from the fish market Lotaçor data portal, containing fish
#' catch in the Azores between 2012 and 2017 for six fish species: Thunnus
#' obesus, Thunnus alalunga, Katsuwonus pelamis, Thunnus albacares, Thunnus
#' thynnus, and Coryphaena hippurus. The `catch` is given in kilograms.
#'
#' @format A data frame with five columns:
#' \describe{
#'   \item{`year`}{Year.}
#'   \item{`name_en`}{Fish common name in English.}
#'   \item{`name_pt`}{Fish common name in Portuguese.}
#'   \item{`species`}{Species name.}
#'   \item{`catch`}{Fish catch in kilograms.}
#' }
#'
#' @examples
#' fish_catch
#'
#' @source
#' \url{https://lotacor.pt/pescado-descarregado}
#'
"fish_catch"
