import_data <- function() {
  readxl::read_excel(here::here("data-raw/Nuove valutazioni.xlsx")) |>
    janitor::clean_names()
}

get_var_id <- function(var_name) {
  as.numeric(stringr::str_extract(var_name, "\\d$"))
}

get_sym_id <- function(var_name) {
  dplyr::if_else(get_var_id(var_name) != 9, "≥", "=")
}

convert_to_improvement_db <- function(raw_data) {
  raw_data |>
    dplyr::transmute(
      dplyr::across(
        .cols = dplyr::starts_with("amr_cathegory_p_s6rp_"),
        .fns = ~ {amr_cathegory_solo_c4d - .x},
        .names = "{get_sym_id(.col)} {get_var_id(.col)}"
      )
    )
}


summary_table <- function(db, what = "categorical") {
  print(gtsummary::tbl_summary(db, type = everything() ~ what))
  invisible(db)
}



produce_dia_prop_db <- function(db) {
  db |>
   tidyr:: pivot_longer(everything()) |>
    dplyr::transmute(
      diagnosis = value == 1,
      name = factor(name, levels = paste(c(rep("≥", 8), "="), 1:9))
    ) |>
    dplyr::with_groups(
      name,
      dplyr::mutate,
      prop = dplyr::if_else(
        diagnosis, mean(diagnosis), mean(!diagnosis)
      )
    )
}

gg_diagnoses <- function(dat) {
  dat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = name,
        y = prop,
        fill = diagnosis,
        label = scales::percent(prop))
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(1), vjust = -0.4
    ) +
    ggplot2::labs(
      title = "Diagnoses improvement by PS6RP total score",
      x = "PS6RP Total Score",
      y = "Percent variation, %",
      fill = "Diagnosis improved",
      caption = "Data and original script provided by LV"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme_minimal()
}


save_plot_if_interactive <- function(
  p,
  where = here::here('output/Diagnosis improvement.tif'),
  device = "tiff",
  do_the_save = interactive()
) {
  if (do_the_save) {
    ggplot2::ggsave(
      where,
      device = device,
      compression = 'lzw',
      dpi = 600,
      scale = 1.5
    )
  }
  p
}
