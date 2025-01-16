data_frame_to_capthist <- function(df,
                                   individual = "ID",
                                   primary_session = "Session",
                                   secondary_session = "Occasion") {

  cols_of_interest <- c(individual, primary_session, secondary_session)
  df2 <- dplyr::select(df, dplyr::all_of(cols_of_interest))

  tidyr::pivot_wider(df2, names_from = secondary_session, values_from = 1)

}
