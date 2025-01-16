library(tidyverse)
library(azores.fkw)

gpx_zip <- here::here("data-raw/gpx.zip")
gpx_files <- utils::unzip(zipfile = gpx_zip, list = TRUE)

# Initialise an empty tracks' list
tracks_lst <- setNames(vector(mode = "list", length = nrow(gpx_files)), nm = gpx_files$Name)

# Cycle over each GPX file in the zip archive and read the tracks
for (file in gpx_files$Name) {
  temp_dir <- tempdir()
  utils::unzip(gpx_zip, files = file, exdir = temp_dir)
  tracks_lst[[file]] <- read_tracks(file.path(temp_dir, file))
  file.remove(file.path(temp_dir, file))
}

tracks <- purrr::list_rbind(tracks_lst)

tracks2 <-
  tracks |>
  dplyr::select(-c("extensions", "segment_id", "elevation")) |>
  dplyr::mutate(track = harmonise_date_time(track))

boat_trips <-
  tracks2 |>
  dplyr::rename(year = file, trip = track) |>
  dplyr::mutate(trip = lubridate::as_datetime(trip)) |>
  dplyr::mutate(year = as.integer(year))

boat_trips_by_min <- mean_pos_by_min(trips = boat_trips)

trips <- unique(boat_trips_by_min$trip)
trips_and_weeks <- tibble::tibble(trip = trips, week = which_week_of_study(trip))

boat_trips_by_min <- dplyr::left_join(boat_trips_by_min, trips_and_weeks, by = "trip") |>
  dplyr::relocate(week, .after = "trip")

usethis::use_data(boat_trips, overwrite = TRUE)
usethis::use_data(boat_trips_by_min, overwrite = TRUE)

