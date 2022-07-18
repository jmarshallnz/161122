# stuff for Lab C04

# Consumption from: https://www.rbnz.govt.nz/statistics/m2
consumption <- readxl::read_excel("~/Downloads/hm2.xlsx", skip=4)
consumption %>% dplyr::select(1:2) %>% purrr::set_names("date", "x_m") %>%
  readr::write_csv(here::here("data/consumption.csv"))

# also need the housing one
