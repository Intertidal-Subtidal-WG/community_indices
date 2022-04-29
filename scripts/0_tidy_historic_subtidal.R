#'---------------------------------------------------
#' Make historic subtidal percent cover data tidy
#'---------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)


cover_raw <- read_excel("data/PercentCover.xlsx", sheet = 2) %>%
  rename(organism = `...1`)


cover <- cover_raw %>%
  pivot_longer(cols = -organism,
               values_to = "perc_cover") %>%
  mutate(year = gsub(".(.*)", "\\1", name),
         year = as.numeric(year),
         year = ifelse(year==14, 2014, year),
         year = ifelse(year==96, 1996, year),
         site = case_when(
           grepl("A", name) ~ "Appledore", #Babb's cove
           grepl("S", name) ~ "Star Island",
           grepl("L", name) ~ "Lunging",
           grepl("W", name) ~ "White Island"
         )
  ) %>%
  select(site, year, organism, perc_cover)


readr::write_csv(cover, "data/subtidal_historic_cover.csv")
