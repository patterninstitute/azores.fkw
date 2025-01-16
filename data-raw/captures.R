library(raster)
library(tidyverse)


sightings <- read_sightings(sightings_path())
grid_cells <- find_grid_cells(points = sightings[c("latitude", "longitude")])
grid_centroids <- grid_centroids()
grid_centroids_utm <- longlat_to_utm(grid_centroids)

detectors <- dplyr::rename_with(grid_cells, ~ paste("detector", .x, sep = "_"))
sightings_and_detectors <- dplyr::bind_cols(sightings, detectors)

effort <- effort()
occasions <- occasions()

capture <- create_capture()

write_capture(capture, "~/dwl/my_capture.txt")
