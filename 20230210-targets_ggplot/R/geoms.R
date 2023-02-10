geom_logtrd <- function(gg, db, type = c("TRD", "LOG")) {
  stopifnot("time" %in% names(db))
  type <- match.arg(type)

  violin_col <- switch(
    type,
    TRD = "blue",
    LOG = "violet"
  )
  lollipop_col <- switch(
    type,
    TRD = "dark blue",
    LOG = "purple"
  )

  gg_aux <- gg +
    ggplot2::geom_violin(
      data = db,
      ggplot2::aes(x = time, y = type),
      scale = "count",
      alpha = 0.2,
      fill = violin_col
    )

  geom_lollipop(gg_aux, db, type, lollipop_col)
}

geom_lollipop <- function(
    gg, db, type = c("TRD", "LOG"), col = "purple"
) {
  gg_aux <- gg +
    ggplot2::geom_segment(
      data = db,
      ggplot2::aes(
        x = min(time),
        y = type,
        xend = max(time),
        yend = type
      ),
        color = col,
        linewidth = 1
    )

  gg_aux |>
    geom_extreme_point(db, type, col, "min") |>
    geom_extreme_point(db, type, col, "max")
}


geom_extreme_point <- function(
    gg, db, type, col, extreme = c("min", "max")
) {
  extreme <- match.arg(extreme)
  f_extreme <- switch(extreme,
    min = min,
    max = max
  )

  gg +
    ggplot2::geom_point(
      data = db,
      ggplot2::aes(x = f_extreme(time), y = type),
      color = col,
      size = 2
    )
}

