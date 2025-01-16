test_that("create_utm_grid() basic usage", {

  # bb: bounding box points in lon/lat
  bb <- utm_to_longlat(data.frame(easting = c(0, 100000), northing = c(0, 100000)))

  # Creating a grid starting at UTM point (0, 0) in 26N zone, ending at point
  # easting = 100,000 m and northing 100,000 m. The ranges provided to create_utm_grid are
  # in decimal degrees, whose conversion was done earlier and saved in variable `bb`.
  # The UTM resolution is in metres, however.
  utm_grid <- create_utm_grid(x_range = bb$lon, y_range = bb$lat, utm_resolution = 10000, 0, 0, 0, 0)
  # Removing attributes using as.data.frame()
  utm_grid <- as.data.frame(utm_grid)
  row.names(utm_grid) <- NULL

  expected_grid <- expand.grid(x = seq(0, 100000, 10000), y = seq(0, 100000, 10000))
  expected_grid <- as.data.frame(expected_grid)
  attr(expected_grid, "out.attrs") <- NULL
  row.names(expected_grid) <- NULL

  expect_equal(object = utm_grid, expected = expected_grid)
})

test_that("create_utm_grid() testing padding", {

  # bb: bounding box points in lon/lat
  bb <- utm_to_longlat(data.frame(easting = c(0, 100000), northing = c(0, 100000)))

  # Creating a grid starting at UTM point (0, 0) in 26N zone, ending at point
  # easting = 100,000 m and northing 100,000 m. The ranges provided to create_utm_grid are
  # in decimal degrees, whose conversion was done earlier and saved in variable `bb`.
  # The UTM resolution is in metres, however.
  utm_grid <- create_utm_grid(x_range = bb$lon, y_range = bb$lat, utm_resolution = 10000, 0.10, 0.10, 0, 0)
  # Removing attributes using as.data.frame()
  utm_grid <- as.data.frame(utm_grid)
  row.names(utm_grid) <- NULL

  expected_grid <- expand.grid(x = seq(-10000, 110000, 10000), y = seq(-10000, 110000, 10000))
  expected_grid <- as.data.frame(expected_grid)
  attr(expected_grid, "out.attrs") <- NULL
  row.names(expected_grid) <- NULL

  expect_equal(object = utm_grid, expected = expected_grid, tolerance = 1e-3)
})

test_that("create_utm_grid() testing offset", {

  # bb: bounding box points in lon/lat
  bb <- utm_to_longlat(data.frame(easting = c(0, 100000), northing = c(0, 100000)))

  # Creating a grid starting at UTM point (0, 0) in 26N zone, ending at point
  # easting = 100,000 m and northing 100,000 m. The ranges provided to create_utm_grid are
  # in decimal degrees, whose conversion was done earlier and saved in variable `bb`.
  # The UTM resolution is in metres, however.
  utm_grid <- create_utm_grid(x_range = bb$lon, y_range = bb$lat, utm_resolution = 10000, 0, 0, -5000, -5000)
  # Removing attributes using as.data.frame()
  utm_grid <- as.data.frame(utm_grid)
  row.names(utm_grid) <- NULL

  expected_grid <- expand.grid(x = seq(-5000, 95000, 10000), y = seq(-5000, 95000, 10000))
  expected_grid <- as.data.frame(expected_grid)
  attr(expected_grid, "out.attrs") <- NULL
  row.names(expected_grid) <- NULL

  expect_equal(object = utm_grid, expected = expected_grid, tolerance = 1e-3)
})
