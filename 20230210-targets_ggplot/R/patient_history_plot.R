#' Build a patient history plot
#'
#' Show on a single plot all relevant data about a single patient:
#' - TRD time density plot
#' - LOG time density plot
#' - ICU-stay time span
#' - Mechanical ventilation time span
#' - SBT readiness
#' - if the patient on a certain day was weaned or not
#'
#' @param id_univoco (chr) 2 letter + 3 digits string of the hospital
#'   and patient id
#'
#' @return a [ggplot][ggplot2::ggplot2-package] showing the patient
#'  history plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   baseline <- read_patient_data("baseline.csv")
#'   track <- read_patient_data("track.csv")
#'   log <- read_patient_data("log.csv")
#'   daily <- read_patient_data("daily.csv")
#'
#'  patient_history_plot("TS015", baseline, track, log, daily)
#'  patient_history_plot("NO004", baseline, track, log, daily)
#'  patient_history_plot("TS012", baseline, track, log, daily)
#'
#' }
#'
patient_history_plot <- function(
  id_univoco, baseline, track, log, daily
) {

  checkmate::assert_character(id_univoco)

  pat_trd <- track |>
    filter_patient_data(id_univoco) |>
    dplyr::mutate(time = lubridate::as_datetime(paste(date, ora)))

  pat_log <- log |>
    filter_patient_data(id_univoco)

  pat_names <- baseline |>
    filter_patient_data(id_univoco)
    checkmate::assert_tibble(pat_names, min.rows = 1)

  pat_registry <- daily |>
    filter_patient_data(id_univoco)


  # Step 3 - ggplot

  ggplot2::ggplot() |>
    geom_logtrd(pat_trd, "TRD") |>
    geom_logtrd(pat_log, "LOG") +

    # Registry file
    ggplot2::geom_point( data = pat_registry,
                         ggplot2::aes( x = as.POSIXct(data_lettura),
                              y = "Registry",
                              size = susp_tot,
                              color = (susp_tot == 12)),
                         show.legend = c(size = FALSE,
                                         color = TRUE)) +
    ggplot2::scale_radius(range = c(2,4)) +

    # Patient names file
    ## ICU
    ggplot2::geom_segment( data = pat_names,
                           ggplot2::aes( y = "ICU", yend = "ICU",
                                x = as.POSIXct(icu_in),
                                xend = as.POSIXct(icu_out)),
                           color = "dark grey",
                           linewidth = 2) +
    ggplot2::geom_point( data = pat_names,
                         ggplot2::aes( y = "ICU",
                              x = as.POSIXct(icu_in)),
                         color = "dark grey",
                         size = 4) +
    ggplot2::geom_point( data = pat_names,
                         ggplot2::aes( y = "ICU",
                              x = as.POSIXct(icu_out)),
                         color = "dark grey",
                         size = 4) +
    ggrepel::geom_text_repel( data = pat_names,
                              ggplot2::aes( x = as.POSIXct(icu_out),
                                   y = "ICU",
                                   label = "in ICU"),
                              nudge_y = -0.3,
                              color = "dark grey") +
    ## MV
    ggplot2::geom_segment( data = pat_names,
                           ggplot2::aes( y = "ICU", yend = "ICU",
                                x = as.POSIXct(vm_inizio),
                                xend = as.POSIXct(vm_fine)),
                           color = "black",
                           linewidth = 1) +
    ggplot2::geom_point( data = pat_names,
                         ggplot2::aes( y = "ICU",
                              x = as.POSIXct(vm_inizio)),
                         color = "black",
                         size = 2) +
    ggplot2::geom_point( data = pat_names,
                         ggplot2::aes( y = "ICU",
                              x = as.POSIXct(vm_fine)),
                         color = "black",
                         size = 2) +
    ggrepel::geom_text_repel( data = pat_names,
                              ggplot2::aes(x = as.POSIXct(vm_inizio +1),
                                   y = "ICU",
                                   label = "in Ventilazione Meccanica"),
                              nudge_y = 0.3,
                              color = "black") +

    # Estubazioni
    ggplot2::geom_point( data = pat_registry %>%
                           dplyr::filter(estubato == TRUE),
                         ggplot2::aes( y = "Registry",
                              x = as.POSIXct(data_lettura)),
                         color = "black",
                         shape = 4,
                         size = 4) +

    # Title and Labels
    ggplot2::labs( title = "Plot of patient history",
                   subtitle = paste(
                     "Patient",
                     id_univoco,
                     "[ X = estubato ]"
                  ),
                   x = "",
                   y = "",
                   color = "Ready for SBT",
                   size = "n. of suspect criteria") +
    ggplot2::scale_y_discrete(limits = c("LOG",
                                         "TRD",
                                         "Registry",
                                         "ICU"))

}
