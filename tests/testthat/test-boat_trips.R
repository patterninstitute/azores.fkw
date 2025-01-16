test_that("`trip` column has dates in expected format", {
  testthat::expect_true(all(is_date_time(azores.fkw::boat_trips$trip)))
})
