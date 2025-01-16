library(tidyverse)


path <- here::here("data-raw/lotacor_fish_catch.xlsx")
fish_catch_raw <- readxl::read_xlsx(path = path)

fish_catch <-
  fish_catch_raw |>
  tidyr::pivot_longer(cols = -1L,
                      names_to = "year",
                      values_to = "catch") |>
  dplyr::rename(name_pt = ...1) |>
  dplyr::mutate(
    name_en =
      dplyr::case_match(
        name_pt,
        "Patudo" ~ "Bigeye tuna",
        "Voador" ~ "Albacore",
        "Bonito" ~ "Skipjack tuna",
        "Galha-á-ré" ~ "Yellowfin tuna",
        "Rabilo" ~ "Atlantic bluefin tuna",
        "Dourado" ~ "Common dolphinfish"
      )
  ) |>
  dplyr::mutate(
    species =
      dplyr::case_match(
        name_pt,
        "Patudo" ~ "Thunnus obesus",
        "Voador" ~ "Thunnus alalunga",
        "Bonito" ~ "Katsuwonus pelamis",
        "Galha-á-ré" ~ "Thunnus albacares",
        "Rabilo" ~ "Thunnus thynnus",
        "Dourado" ~ "Coryphaena hippurus"
      )
  ) |>
  dplyr::relocate(year,
                  name_en,
                  name_pt,
                  species,
                  catch)

usethis::use_data(fish_catch, overwrite = TRUE)
