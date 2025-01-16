library(tidyverse)

path <- here::here("data-raw/Pc_Data_Raw.xlsx")
pc_data_raw <- readxl::read_xlsx(path, col_names = TRUE, skip = 1L)

#
# `sightings`
#

sightings01 <-
  pc_data_raw |>
  dplyr::mutate(sighting_id = as.integer(Sighting))

sightings02 <-
  sightings01 |>
  dplyr::transmute(
    sighting_id,
    latitude = lat,
    longitude = lon,
    date = as.Date(Date...4),
    initial_time = lubridate::make_datetime(
      year = lubridate::year(Date...4),
      month = lubridate::month(Date...4),
      day = lubridate::day(Date...4),
      hour = lubridate::hour(`Time i`),
      min = lubridate::minute(`Time i`),
      sec = lubridate::second(`Time i`),
      tz = "UTC"
    ),
    final_time = lubridate::make_datetime(
      year = lubridate::year(Date...4),
      month = lubridate::month(Date...4),
      day = lubridate::day(Date...4),
      hour = lubridate::hour(`Time f`),
      min = lubridate::minute(`Time f`),
      sec = lubridate::second(`Time f`),
      tz = "UTC"
    ),
    other_species = dplyr::case_when(
      `OT. Species` == "NO" ~ NA_character_,
      `OT. Species` == "GG" ~ "Grampus griseus",
      `OT. Species` == "TT" ~ "Tursiops truncatus",
      `OT. Species` == "DD" ~ "Delphinus delphis",
      .default = NA_character_
      ),
    dplyr::across(`Individuals ID`:tidyselect::last_col())
  )

# If final time is not available it is because the sighting went too fast. So
# we set the sighting duration to 1 min by defining:
# final_time = initial_time + 60 seconds
sightings03 <-
  sightings02 |>
  dplyr::mutate(final_time = dplyr::if_else(is.na(final_time),
                                            initial_time + 60, # 1 min = 60 seconds
                                            final_time))

sightings04 <-
  sightings03 |>
  tidyr::unite(col = "individuals", `Individuals ID`:tidyselect::last_col(), sep = ";", na.rm = TRUE) |>
  dplyr::mutate(
    individuals = dplyr::na_if(individuals, "")
  )

sightings <- sightings04

#
# `ethology`
#

ethology <-
  sightings01 |>
  dplyr::transmute(sighting_id,
                   dplyr::across(FO:NI...30)) |>
  tidyr::pivot_longer(
    cols = FO:NI...30,
    names_to = "behavior",
    values_drop_na = TRUE
  ) |>
  dplyr::select(-"value") |>
  dplyr::mutate(
    behavior = dplyr::case_when(
      behavior == "FO" ~ "foraging",
      behavior == "M" ~ "milling",
      behavior == "R" ~ "resting",
      behavior == "TF" ~ "travelling fast",
      behavior == "TA" ~ "travelling average",
      behavior == "TS" ~ "travelling slow",
      behavior == "SO" ~ "socializing",
      behavior == "BR" ~ "bow riding",
      behavior == "LB" ~ "lobe tailing",
      behavior == "FL" ~ "fluking",
      behavior == "SP" ~ "spy hopping",
      behavior == "OT" ~ "other",
      behavior == "BS" ~ "breaching side",
      behavior == "BF" ~ "breaching front",
      behavior == "BB" ~ "breaching back",
      behavior == "H" ~ "hiding",
      behavior == "NI" ~ NA_character_,
      .default = NA_character_
    )
  )

#
# `pod_age_composition`
#

pod_age_composition <-
  sightings01 |>
  dplyr::transmute(sighting_id,
                   dplyr::across(Max:NI...36)) |>
  tidyr::pivot_longer(
    cols = Max:NI...36,
    names_to = "age_group",
    values_to = "n",
    values_drop_na = TRUE
  ) |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      age_group == "Max" ~ "total",
      age_group == "Ad" ~ "adult",
      age_group == "J" ~ "juvenile",
      age_group == "C" ~ "calf",
      age_group == "NB" ~ "newborn",
      age_group == "NI" ~ "other",
      .default = NA_character_
    )
  ) |>
  dplyr::filter(age_group != "total") |>
  tidyr::complete(sighting_id,
                  age_group = factor(
                    age_group,
                    levels = c("newborn", "calf", "juvenile", "adult", "other")
                  ),
                  fill = list(n = 0))

reaction_to_boat <-
  sightings01 |>
  dplyr::transmute(sighting_id,
                   dplyr::across(A:Ni)) |>
  tidyr::pivot_longer(
    cols = A:Ni,
    names_to = "reaction",
    values_to = "n",
    values_drop_na = TRUE
  ) |>
  dplyr::select(-n) |>
  dplyr::mutate(
    reaction = dplyr::case_when(
      reaction == "A" ~ "approach",
      reaction == "I" ~ "indifference",
      reaction == "E" ~ "evasive",
      reaction == "Ni" ~ "other",
      .default = NA_character_
    )
  )

individuals <-
  sightings |>
  tidyr::drop_na(individuals) |>
  tidyr::separate_longer_delim(cols = "individuals", delim = ";") |>
  dplyr::rename(individual = individuals) |>
  dplyr::relocate(individual, sighting_id, .before = 1L) |>
  dplyr::arrange(individual, date)

usethis::use_data(sightings, overwrite = TRUE)
usethis::use_data(ethology, overwrite = TRUE)
usethis::use_data(pod_age_composition, overwrite = TRUE)
usethis::use_data(reaction_to_boat, overwrite = TRUE)
usethis::use_data(individuals, overwrite = TRUE)
