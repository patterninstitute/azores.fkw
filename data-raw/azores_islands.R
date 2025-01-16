library(tidyverse)
library(sf)

map_path <- here::here("data-raw", "maps/portugal")
crs <- 32626

portugal_polygons <- sf::st_read(dsn = map_path)
azores_multipolygon <- portugal_polygons[portugal_polygons$NAME_1 == "Azores", ]
azores_polygons <- sf::st_cast(sf::st_geometry(azores_multipolygon), "POLYGON")

island_name <-
  c(
    "Santa Maria",
    "S\u00E3o Miguel",
    "Pico",
    "Faial",
    "S\u00E3o Jorge",
    "Terceira",
    "Graciosa",
    "Flores",
    "Corvo"
  )

island_group <-
  c(
    "Eastern",
    "Eastern",
    "Central",
    "Central",
    "Central",
    "Central",
    "Central",
    "Western",
    "Western"
  )

triangle_islands <- c("Pico", "Faial", "São Jorge")

# `tiny_islet`: indices of tiny islets to be excluded
tiny_islet <- c(2:6, 9, 14, 16)
azores_islands <-
  sf::st_sf(geometry = azores_polygons[-tiny_islet],
            name = island_name,
            group = island_group) |>
  dplyr::mutate(is_triangle = name %in% triangle_islands,
                .before = "geometry")

usethis::use_data(azores_islands, overwrite = TRUE)
