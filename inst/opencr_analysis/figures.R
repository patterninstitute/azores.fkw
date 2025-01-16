library(tidyverse)
library(patchwork)
library(metR)
library(sf)
library(secr)
library(openCR)
library(ggspatial)
library(azores.fkw)

path <- here::here("inst/opencr_analysis/")
zze <- sf::st_read(file.path(path, "zze-200-miles.kml"))
figures_path <- file.path(path, "figures")

if (isFALSE(file.exists(file.path(path, "opencr_results.rds")))) {
  cli::cli_abort("{.file opencr_results.rds} is missing! Please run {.file opencr_analysis.R} first.")
} else {
  opencr_results <- read_rds(file = file.path(path, "opencr_results.rds"))
  list2env(opencr_results, envir = .GlobalEnv)
}

#
# Encounter probability figure
#

enc_prob_plot <-
  ggplot2::ggplot() +
  metR::geom_contour_fill(
    data = JSSAsecrb_enc_prob_per_hour,
    mapping = ggplot2::aes(x = x, y = y, z = p),
    show.legend = TRUE,
    alpha = 1,
    na.fill = TRUE
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(title = "Enc. probability\n    (per hour)")) +
  metR::geom_contour2(
    data = JSSAsecrb_enc_prob_per_hour,
    ggplot2::aes(x = x, y = y, z = p),
    color = "black",
    alpha = 0.3,
    show.legend = FALSE,
    na.fill = TRUE
  ) +
  metR::geom_contour2(
    data = roi_bathymetry,
    ggplot2::aes(x = x, y = y, z = depth),
    breaks = seq(200, 2000, 300),
    na.fill = FALSE,
    colour = "white",
    alpha = 0.3,
    linewidth = 0.5
  ) +
  ggplot2::geom_point(
    data = individuals_utm,
    mapping = ggplot2::aes(x = easting, y = northing),
    colour = "white"
  ) +
  ggplot2::geom_sf(data = islands_utm) +
  # ggplot2::coord_sf(default_crs = sf::st_crs(32626), datum = sf::st_crs(32626)) +
  ggplot2::coord_sf(default_crs = sf::st_crs(32626)) +
  ggplot2::scale_fill_continuous(
    type = "viridis",
    limits = c(0, 0.0175),
    breaks = seq(0, 0.0175, 0.005)
  ) +
  ggplot2::scale_x_continuous(
    name = NULL,
    limits = c(339000, 440000),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    name = NULL,
    limits = c(4200000, 4295000),
    expand = c(0, 0)
  )

#
# Discovery curve figure
#

discovery <-
  dplyr::left_join(
    individuals_per_year(azores.fkw::individuals) |>
      dplyr::rename(total = n),
    count_of_new_individuals_per_year(individuals = azores.fkw::individuals) |>
      dplyr::rename(new = n),
    by = "year"
  ) |>
  dplyr::mutate(old = total - new)

discovery_cum <-
  discovery |>
  dplyr::mutate(
    total = cumsum(total),
    new = cumsum(new),
    old = cumsum(old)
  )

# Reshape the data
data_long <- discovery |>
  pivot_longer(cols = c(new, old), names_to = "Individuals", values_to = "count") |>
  dplyr::mutate(Individuals = dplyr::if_else(Individuals == "old", "Recaptured", "Captured")) |>
  dplyr::mutate(Individuals = factor(Individuals, levels = c("Recaptured", "Captured")))

data_long_cum <- discovery_cum |>
  pivot_longer(cols = c(new, old), names_to = "Individuals", values_to = "count") |>
  dplyr::mutate(Individuals = dplyr::if_else(Individuals == "old", "Recaptured", "Captured")) |>
  dplyr::mutate(Individuals = factor(Individuals, levels = c("Recaptured", "Captured")))

# Create the stacked bar plot
discovery_plot1 <-
  ggplot(data_long) +
  geom_bar(stat = "identity", mapping = aes(x = year, y = count, fill = Individuals)) +
  labs(x = "Year", y = "Number of sighted individuals") +
  geom_text(aes(x = year, y = total + 0.5, label = total),
            data = discovery,
            vjust = 0,
            size = 4) +
  theme(legend.position = "none", panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks = element_line(colour = "grey80"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray80")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 24)) +
  scale_x_discrete(name = NULL)

discovery_plot2 <-
  ggplot(data_long_cum) +
  geom_bar(stat = "identity", mapping = aes(x = year, y = count, fill = Individuals)) +
  labs(x = "Year", y = "Cumulative number of sighted individuals") +
  geom_text(aes(x = year, y = total + 1.5, label = total),
            data = discovery_cum,
            vjust = 0,
            size = 4) +
  geom_line(
    mapping = aes(x = year, y = count, group = 1),
    data = dplyr::filter(data_long_cum, Individuals == "Captured")
  ) +
  geom_point(
    mapping = aes(x = year, y = count, group = 1),
    data = dplyr::filter(data_long_cum, Individuals == "Captured")
  ) +
  theme(legend.position = "none", panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks = element_line(colour = "grey80"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray80")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 88)) +
  scale_x_discrete(name = NULL)

discovery_figure <-
  discovery_plot1 + discovery_plot2 + patchwork::plot_annotation(tag_levels = 'A')

#
# Lookout effort intensity (logistic curve as a function of distance to lookout)
#

logistic_data <-
  tibble::tibble(
    x = seq(0, 40, 0.1),
    y = logistic(x)
  )

lookout_plot1 <-
  ggplot() +
  geom_line(data = logistic_data, mapping = aes(x = x, y = y)) +
  ylim(0, 1) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  ylab("Lookout effort intensity") +
  xlab("Distance from lookout (km)")

#
# Lookout observation area
#

lon_range <- range(boat_trips_by_min$longitude)
lat_range <- range(boat_trips_by_min$latitude)

m <- as.matrix(tidyr::expand_grid(
  x = seq(
    from = lon_range[1],
    to = lon_range[2],
    length.out = 500
  ),
  y = seq(
    from = lat_range[1],
    to = lat_range[2],
    length.out = 500
  )
))

lookout_effort_raw <- tibble::tibble(
  x = m[, "x"],
  y = m[, "y"],
  z = logistic_2d(points = m),
  is_within = is_within(
    x = x,
    y = y,
    sf_polygon = lookout_obs_area()
  )
) |>
  dplyr::filter(is_within)

lookout_plot2 <-
  lookout_effort_raw |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = azores_triangle(),
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  metR::geom_contour_fill(
    data = lookout_effort_raw,
    mapping = ggplot2::aes(x = x, y = y, z = z),
    show.legend = TRUE,
    na.fill = FALSE,
    alpha = 1
  ) +
  scale_fill_gradientn(
    colours = c("#FDFCFF", "#F7FBFF", "blue"),
    values = c(0, 0.1, 1),
    guide = "colourbar",
    limits = c(0, 1),
    name = "Lookout effort"
  ) +
  geom_sf(data = lookout_obs_area(), fill = NA, col = "grey80", linewidth = 0.75) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks = element_line(colour = "grey80"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray80")) +
  xlab("") + ylab("")

lookout_plot <- lookout_plot1 + lookout_plot2 + patchwork::plot_annotation(tag_levels = 'A')

#
# Total Effort
#

lookout_score_tbl <- lookout_score(grid = grid_500)
boat_effort_per_week_tbl <- azores.fkw::boat_effort_per_week(grid = grid_500)
lookout_effort_by_week <- azores.fkw::lookout_effort_by_week(lookout_score_tbl = lookout_score_tbl,
                                                             boat_effort_per_week_tbl = boat_effort_per_week_tbl)

boat_effort_for_all_weeks <- boat_effort_for_all_weeks(boat_effort_per_week = boat_effort_per_week_tbl)
lookout_effort_for_all_weeks <- lookout_effort_for_all_weeks(lookout_effort_by_week = lookout_effort_by_week)

combined_effort <- combined_effort_for_all_weeks(boat_effort = boat_effort_for_all_weeks, lookout_effort = lookout_effort_for_all_weeks)

combined_effort_plot <-
  combined_effort |>
  dplyr::filter(combined_effort != 0) |>
  ggplot() +
  stat_summary_hex(
    mapping = aes(x = longitude, y = latitude, z = combined_effort / 60),
    fun = sum,
    binwidth = 0.040,
    color = "white",
    linewidth = 0.1
  ) +
  ggplot2::scale_fill_gradientn(
    name = "Total Effort\n   (hours)",
    colours = c("#008671", "#e6e7e8", "#a76109"),
    values = scales::rescale(c(0, 25, 500)),
    breaks = c(0, 100, 200, 300, 400, 500),
    guide = "colorbar",
    limits = c(0, 500)
  ) +
  ggplot2::geom_sf(
    data = azores_triangle(),
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = 'gray'),
    legend.key.height = ggplot2::unit(1.5, "cm"),
    legend.box.just = "left"
  )

#
# Boat tracks
#

boat_tracks_main_plot <-
  ggplot2::ggplot(data = boat_trips_by_min) +
  ggplot2::geom_path(
    ggplot2::aes(x = longitude, y = latitude, group = trip),
    alpha = 0.5,
    colour = "#2ab8da",
    linewidth = 0.1
  ) +
  ggplot2::geom_sf(
    data = islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_x_continuous(limits = c(-29, -27.5), breaks = c(-28.8, -28.4, -28.0, -27.6)) +
  ggplot2::scale_y_continuous(limits = c(38, 39), breaks = c(38.0, 38.4, 38.8)) +
  ggplot2::scale_fill_continuous(limits = c(0, 0.012),
                                 breaks = seq(0, 0.012, 0.004)) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_x = unit(0.20, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )

boat_tracks_inset_plot <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = zze,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  scale_x_continuous(breaks = c(-32, -28, -24)) +
  scale_y_continuous(breaks = c(34, 38, 42)) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(color = "black"),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_blank(),
  )

boat_tracks_plot <-
  boat_tracks_main_plot + inset_element(
    boat_tracks_inset_plot,
    left = 0.65,
    bottom = 0.65,
    right = 1,
    top = 1.03,
    align_to = "panel"
  )


#
# Export plots
#

ggsave(filename = file.path(figures_path, "enc_prob_plot.pdf"), plot = enc_prob_plot, width = 9, height = 8)
ggsave(filename = file.path(figures_path, "discovery_figure.pdf"), plot = discovery_figure, width = 9, height = 6)
ggsave(filename = file.path(figures_path, "lookout_plot.pdf"), plot = lookout_plot, width = 12, height = 6)
ggsave(filename = file.path(figures_path, "combined_effort_plot.pdf"), plot = combined_effort_plot, width = 9, height = 7)
ggsave(filename = file.path(figures_path, "boat_tracks_plot.pdf"), plot = boat_tracks_plot, width = 9, height = 8)

