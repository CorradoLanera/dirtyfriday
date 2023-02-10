get_dirtyfriday_file <- function(
  filename,
  dirpath = here::here("data-raw")
) {
  file.path(dirpath, filename)
}


read_patient_data <- function(
  filename,
  dirpath = here::here("data-raw")
) {
  readr::read_csv(
    get_dirtyfriday_file(filename, dirpath),
    show_col_types = FALSE
  )
}

filter_patient_data <- function(db, id_univoco) {
  db |>
    dplyr::filter(.data[["id_univoco"]] == .env[["id_univoco"]])
}
