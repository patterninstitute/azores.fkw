#' @importFrom rlang .data
#' @importFrom patchwork plot_layout
#' @export
# plot_boat_effort <- function() {
#   boat_trips_by_min <- azores.fkw::boat_trips_by_min
#   islands <- azores_triangle()
#
#   p1 <-
#     ggplot2::ggplot(data = boat_trips_by_min) +
#     ggplot2::geom_path(
#       ggplot2::aes(x = .data$longitude, y = .data$latitude, group = .data$trip),
#       alpha = 0.5,
#       colour = "#2ab8da",
#       linewidth = 0.1
#     ) +
#     ggplot2::geom_sf(
#       data = islands,
#       fill = "gray",
#       color = "black",
#       linewidth = 0.2
#     ) +
#     ggplot2::scale_fill_continuous(limits = c(0, 0.012),
#                                    breaks = seq(0, 0.012, 0.004)) +
#     ggplot2::theme(
#       axis.title = ggplot2::element_blank(),
#       panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
#       panel.grid.major = ggplot2::element_line(color = "gray")
#     )
#
#   p2 <-
#     ggplot2::ggplot() +
#     ggplot2::geom_path(data = boat_trips_by_min, mapping = ggplot2::aes(x = .data$longitude, y = .data$latitude, group = .data$trip)) +
#     ggplot2::stat_bin_hex(
#       data = boat_trips_by_min,
#       mapping = ggplot2::aes(
#         x = .data$longitude,
#         y = .data$latitude,
#         fill = ggplot2::after_stat(.data$count / 60)
#       ),
#       color = "white",
#       linewidth = 0.1
#     ) +
#     ggplot2::scale_fill_gradientn(
#       name = "Effort (hours)",
#       colours = c("#008671", "#e6e7e8", "#a76109"),
#       values = scales::rescale(c(0, 25, 200)),
#       breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200),
#       guide = "colorbar",
#       limits = c(0, 200)
#     ) +
#     ggplot2::coord_equal() +
#     ggplot2::geom_sf(
#       data = islands,
#       fill = "gray",
#       color = "black",
#       linewidth = 0.2
#     ) +
#     ggplot2::theme(
#       axis.title = ggplot2::element_blank(),
#       panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
#       panel.grid.major = ggplot2::element_line(color = 'gray'),
#       legend.key.height = ggplot2::unit(1.5, "cm"),
#       legend.box.just = "left"
#     )
#
#   p1 + p2 +
#     patchwork::plot_annotation(tag_levels = list(c("(a)", "(b)"))) &
#     ggplot2::theme(plot.tag.position = c(0, 1),
#           plot.tag = ggplot2::element_text(hjust = 0, vjust = 1))
# }

#' #' @export
#' plot_effort <- function(total_effort) {
#'
#'   total_effort_as_counts <-
#'     total_effort |>
#'     dplyr::group_by(.data$easting, .data$northing) |>
#'     dplyr::group_map(~ .x[rep(1, .x$effort_total), ], .keep = TRUE) |>
#'     purrr::list_rbind() |>
#'     utm_to_longlat() |>
#'     dplyr::rename(longitude = "lon", latitude = "lat")
#'
#'   total_effort_as_counts |>
#'     ggplot2::ggplot() +
#'     ggplot2::stat_bin_hex(
#'       mapping = ggplot2::aes(
#'         x = .data$longitude,
#'         y = .data$latitude,
#'         fill = ggplot2::after_stat(.data$count / 60)
#'       ),
#'       color = "white",
#'       linewidth = 0.1
#'     ) +
#'     ggplot2::scale_fill_gradientn(
#'       name = "Effort (hours)",
#'       colours = c("#008671", "#e6e7e8", "#a76109"),
#'       values = scales::rescale(c(0, 25, 300)),
#'       breaks = c(0, 50, 100, 150, 200, 250, 300),
#'       guide = "colorbar",
#'       limits = c(0, 300)
#'     ) +
#'     ggplot2::geom_sf(
#'       data = azores_triangle(),
#'       fill = "gray",
#'       color = "black",
#'       linewidth = 0.2
#'     ) +
#'     ggplot2::theme(
#'       axis.title = ggplot2::element_blank(),
#'       panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
#'       panel.grid.major = ggplot2::element_line(color = 'gray'),
#'       legend.key.height = ggplot2::unit(1.5, "cm"),
#'       legend.box.just = "left"
#'     )
#'
#' }
