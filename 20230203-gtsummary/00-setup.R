renv::init()

install.packages("devtools")
install.packages("checkmate")
renv::status()
renv::snapshot()

usethis::use_description(check_name = FALSE)
usethis::use_testthat()
