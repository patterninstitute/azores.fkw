test_that("longlat_to_utm() and utm_to_longlat()", {
  # These are Pico's mountain top coordinates:
  #  - longitude = -28.39949181505262
  #  - latitude = 38.467722300624146
  #
  # that converted to UTM zone N26 gives:
  #  - easting = 377910.0763028669
  #  - northing = 4258639.51087624
  #
  # according to:
  # https://products.aspose.app/gis/transformation/wgs84-to-utm.

  # Now we check that the function longlat_to_utm() performs the same conversion.
  expected_utm <- tibble::tibble(easting = 377910.0763028669, northing = 4258639.51087624)
  expected_longlat <- tibble::tibble(lon = -28.39949181505262, lat = 38.467722300624146)
  utm <- longlat_to_utm(longlat = expected_longlat)
  longlat <- utm_to_longlat(expected_utm)

  expect_identical(utm, expected_utm, tolerance = 0.001)
  expect_identical(longlat, expected_longlat, tolerance = 0.00001)
})
