library(azores.fkw)
library(secr)

# Initial setup
path <- here::here("inst/analysis/")
set.seed(1)
analysis <- list()

# SECR setup
grid <- create_grid()

effort <- effort(boat_trips_by_min, grid)
occasions <- occasions(effort)
trapfile <- write_effort(effort, file = file.path(path, "effort.txt"))

capture <- create_capture(individuals, occasions, grid)
captfile <- write_capture(capture, file = file.path(path, "capture.txt"))

habitat_mask <- create_mask()

# The location of the nearest grid cell centroids to sighting events.
detectors_with_sightings <- find_grid_cells(points = individuals, grid = grid)

caphist <- secr::read.capthist(
  captfile = captfile,
  trapfile = trapfile,
  binary.usage = FALSE,
  detector = "multi"
)

# Fitting
#
# `detectfn = 16` corresponds to the hazard exponential detection function.
fit01 <-
  secr::secr.fit(
    capthist = caphist,
    model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
    mask = habitat_mask,
    detectfn = 16
  )

## Encounter probabilities
# habitat_mask_wo_islands <- create_mask(exclude_islands = FALSE)

fit01_params <- exp(fit01$fit$par)
fit01_density <- fit01_params[1]
fit01_lambda0 <- fit01_params[2]
fit01_sigma <- fit01_params[3]

encounter_probability_fkw01 <-
  whale_prob(
    mask = habitat_mask,
    grid = grid,
    capture = capture,
    params = list(sigma = fit01_sigma, lambda0 = fit01_lambda0)
  )

encounter_probability_fkw01_per_hour <-
  encounter_probability_fkw01 |>
  dplyr::mutate(p = p * 60)

# Populate the analysis object
analysis$sightings <- sightings
analysis$grid <- grid
analysis$effort <- effort
analysis$occasions <- occasions
analysis$capture <- capture
analysis$habitat_mask <- habitat_mask
analysis$detectors_with_sightings <- detectors_with_sightings

analysis$fit01_params <- fit01_params
analysis$fit01_density <- fit01_density
analysis$fit01_lambda0 <- fit01_lambda0
analysis$fit01_sigma <- fit01_sigma
analysis$fit01_beta.vcv <- fit01$beta.vcv
analysis$fit01_details <- fit01$details
analysis$fit01_link <- fit01$link
analysis$fit01_call <- fit01$call
analysis$fit01_prediction <- predict(fit01)
analysis$fit01_maskarea <- maskarea(fit01$mask)

analysis$encounter_probability_fkw01_per_hour <- encounter_probability_fkw01_per_hour
analysis$session_info <- sessionInfo()
analysis$timestamp <- lubridate::now()

saveRDS(analysis, file = file.path(path, "secr_results.rds"))

