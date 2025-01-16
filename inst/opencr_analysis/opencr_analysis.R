library(tidyverse)
library(patchwork)
library(metR)
library(sf)
library(secr)
library(openCR)
library(ggspatial)
library(azores.fkw)

path <- here::here("inst/opencr_analysis/")

set.seed(1)
utm_resolution <- 4000
buffer <- 40000
ncores <- 20
trace <- TRUE

boat_trips_by_min2 <-
  azores.fkw::boat_trips_by_min |>
  add_sessions(
    psession_fn = \(x) azores.fkw::year(dat = x, dttm_var = "trip"),
    ssession_fn = \(x) azores.fkw::week(dat = x, dttm_var = "trip")
  )

# Create grid.
grid <- azores.fkw::create_grid(
  x_range = range(boat_trips_by_min2$longitude),
  y_range = range(boat_trips_by_min2$latitude),
  utm_resolution = utm_resolution
)

individuals2 <-
  azores.fkw::individuals |>
  add_sessions(
    psession_fn = \(x) azores.fkw::year(dat = x, dttm_var = "initial_time"),
    ssession_fn = \(x) azores.fkw::week(dat = x, dttm_var = "initial_time")
  )

capture_data <- dplyr::bind_cols(individuals2, find_grid_cells(individuals2, grid = grid)["index"])
capture <- create_capture(capture_data = capture_data)


# Lookout and boat efforts.
lookout_score <- lookout_score(grid = grid)
boat_effort_per_week <- boat_effort_per_week(boat_trips = boat_trips_by_min2, grid = grid)

lookout_effort_by_week <-
  lookout_effort_by_week(
    lookout_score_tbl = lookout_score,
    boat_effort_per_week_tbl = boat_effort_per_week
  )

boat_effort_per_week <- boat_effort_per_week(boat_trips = boat_trips_by_min2, grid = grid)

# Effort per week in minutes.
effort <-
  combined_effort_per_week(
    boat_effort_per_week = boat_effort_per_week,
    lookout_effort_by_week = lookout_effort_by_week
  )

# Create capture history object.
caphist <- secr::read.capthist(
  captfile = write_capture(capture, file = file.path(path, "capture.txt")),
  trapfile = write_effort(effort, file = file.path(path, "effort.txt")),
  binary.usage = FALSE,
  detector = "multi"
)

# Create mask.
exclude_regions <- sf::st_transform(azores.fkw::azores_islands, crs = 32626)
habitat_mask <- create_mask(
  utm_resolution = utm_resolution,
  buffer = buffer,
  exclude_regions = exclude_regions
)

# Range in meters along easting (`x`)
x_length <- diff(range(habitat_mask$x)) + utm_resolution

# Range in meters along northing (`y`)
y_length <- diff(range(habitat_mask$y)) + utm_resolution

# Full area (including area of islands) in hectares
full_area_ha <- x_length * y_length / 10000

# Study area (excludes area of islands) in hectares
study_area_ha <- (nrow(habitat_mask) * utm_resolution^2) / 10000

JSSAsecrb <- openCR::openCR.fit(
  caphist,
  type = 'JSSAsecrb',
  mask = habitat_mask,
  ncores = ncores,
  trace = trace
)

JSSAsecrf <- openCR::openCR.fit(
  caphist,
  type = 'JSSAsecrf',
  mask = habitat_mask,
  ncores = ncores,
  trace = trace
)

JSSAsecrl <- openCR::openCR.fit(
  caphist,
  type = 'JSSAsecrl',
  mask = habitat_mask,
  ncores = ncores,
  trace = trace
)

# Estimate density
density_estimates_JSSAsecrb <- predict(JSSAsecrb, type = 'D')
density_estimates_JSSAsecrf <- predict(JSSAsecrf, type = 'D')
density_estimates_JSSAsecrl <- predict(JSSAsecrl, type = 'D')

(superD_JSSAsecrb <- density_estimates_JSSAsecrb$superD * study_area_ha)
(superD_JSSAsecrf <- density_estimates_JSSAsecrf$superD * study_area_ha)
(superD_JSSAsecrl <- density_estimates_JSSAsecrl$superD * study_area_ha)

aic <- AIC(JSSAsecrb, JSSAsecrf, JSSAsecrl)

# General details
setup <- list(utm_resolution = utm_resolution,
                          buffer = buffer)


#
# Encounter probability
#

grid_500 <- azores.fkw::create_grid(
  x_range = range(boat_trips_by_min2$longitude),
  y_range = range(boat_trips_by_min2$latitude),
  utm_resolution = 500
)

habitat_mask_500 <- create_mask(
  utm_resolution = 500,
  buffer = buffer,
  exclude_regions = exclude_regions
)

capture_data_500 <- dplyr::bind_cols(individuals2, find_grid_cells(individuals2, grid = grid_500)["index"])
capture_500 <- create_capture(capture_data = capture_data_500)

JSSAsecrb_params <- as.list(exp(JSSAsecrb$fit$par))
JSSAsecrb_params <- setNames(JSSAsecrb_params, JSSAsecrb$betanames)

JSSAsecrb_enc_prob <-
  whale_prob(
    mask = habitat_mask_500,
    grid = grid_500,
    capture = capture_500,
    params = JSSAsecrb_params
  )

islands <- azores.fkw::azores_triangle()

bathymetry <- azores.fkw::azores_bathymetry
utm_lims <- tibble::tibble(
  northing = range(habitat_mask_500$y),
  easting = range(habitat_mask_500$x)
)

longlat_lims <- utm_to_longlat(utm_lims)

roi_bathymetry <- dplyr::filter(
  bathymetry,
  dplyr::between(x, utm_lims$easting[1], utm_lims$easting[2]) &
    dplyr::between(y, utm_lims$northing[1], utm_lims$northing[2])
)

individuals_utm <-
  individuals2 |>
  dplyr::rename(lat = latitude, lon = longitude) |>
  longlat_to_utm()

islands_utm <- sf::st_transform(islands, crs = 32626)

JSSAsecrb_enc_prob_per_hour <-
  JSSAsecrb_enc_prob |>
  dplyr::mutate(p = scale_enc_prob(p = p, n = 60))

#
# Export OpenCR Analysis
#

opencr_results <-
  list(
    boat_trips_by_min = boat_trips_by_min2,
    utm_resolution = utm_resolution,
    buffer = buffer,
    capture_data = capture_data,
    lookout_effort_by_week = lookout_effort_by_week,
    boat_effort_per_week = boat_effort_per_week,
    combined_effort_per_week = combined_effort_per_week,
    habitat_mask = habitat_mask,
    x_length = x_length,
    y_length = y_length,
    full_area_ha = full_area_ha,
    study_area_ha = study_area_ha,
    # JSSAsecrb  =JSSAsecrb,
    # JSSAsecrf = JSSAsecrf,
    # JSSAsecrl = JSSAsecrl,
    density_estimates_JSSAsecrb = density_estimates_JSSAsecrb,
    density_estimates_JSSAsecrf = density_estimates_JSSAsecrf,
    density_estimates_JSSAsecrl = density_estimates_JSSAsecrl,
    superD_JSSAsecrb = superD_JSSAsecrb,
    superD_JSSAsecrf = superD_JSSAsecrf,
    superD_JSSAsecrl = superD_JSSAsecrl,
    aic = aic,
    grid_500 = grid_500,
    habitat_mask_500 = habitat_mask_500,
    capture_data_500 = capture_data_500,
    JSSAsecrb_params = JSSAsecrb_params,
    JSSAsecrb_enc_prob = JSSAsecrb_enc_prob,
    JSSAsecrb_enc_prob_per_hour = JSSAsecrb_enc_prob_per_hour,
    islands = islands,
    islands_utm = islands_utm,
    bathymetry = bathymetry,
    roi_bathymetry = roi_bathymetry,
    utm_lims = utm_lims,
    longlat_lims = longlat_lims,
    individuals_utm = individuals_utm
    )

readr::write_rds(x = opencr_results, file = file.path(path, "opencr_results.rds"), compress = "xz")
