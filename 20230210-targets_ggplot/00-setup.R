usethis::use_description(check_name = FALSE)
usethis::use_testthat()

proj_pkgs <- c(
  "dplyr", "ggplot2", "ggrepel", "here", "lubridate", "readr"
)

dev_pkgs <-  c("checkmate", "devtools", "purrr", "testthat", "usethis")

install.packages(c(proj_pkgs, dev_pkgs))

purrr::walk(proj_pkgs, usethis::use_package)
purrr::walk(dev_pkgs, usethis::use_package, type = "Suggest")
usethis::use_tidy_description()
renv::status()


usethis::use_r("utils")
usethis::use_r("geoms")

