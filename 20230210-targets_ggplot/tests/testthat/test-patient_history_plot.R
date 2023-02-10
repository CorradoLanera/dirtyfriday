test_that("patient_history_plot returns the correct plot", {
  # setup
  baseline <- read_patient_data("baseline.csv")
  track <- read_patient_data("track.csv")
  log <- read_patient_data("log.csv")
  daily <- read_patient_data("daily.csv")

  patient_history_plot("TS015", baseline, track, log, daily) |>
    expect_snapshot_value(style = "serialize")

  patient_history_plot("NO004", baseline, track, log, daily) |>
    expect_snapshot_value(style = "serialize")

  patient_history_plot("TS012", baseline, track, log, daily) |>
    expect_snapshot_value(style = "serialize")
})
