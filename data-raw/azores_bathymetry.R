library(azores.fkw)
library(here)

azores_tiff_path <- here::here("data-raw/azores.tiff")
azores_elevation <- get_elevation(file = azores_tiff_path)
azores_bathymetry <-
  azores_elevation |>
  dplyr::select("x", "y", "depth")

usethis::use_data(azores_bathymetry, overwrite = TRUE)
