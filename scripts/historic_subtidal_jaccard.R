#'-----------------------------------------------------
#' @title Historic Subtidal  turnover through time analysis
#'-----------------------------------------------------


library(tidyverse)
library(lme4)
library(emmeans)
library(readr)
theme_set(theme_bw())
source("scripts/helpers.R")
source("scripts/vegan_helpers.R")


# ----------------- Setup Subtidal Cover Data --------------- #

subtidal <- read_csv("data/subtidal_historic_cover.csv")

subtidal_jaccard <- subtidal %>%
  rename(value = perc_cover) %>%
  pivot_wider(values_from = value, names_from = organism,
              values_fill = 0)  %>% #assume missing = 0
  group_by(site) %>%
  make_jaccard %>%
  mutate(year_zeroed = year - 1986,
         year_zeroed_sq = year_zeroed^2) %>% 
  arrange(site, year) %>%
  #get rid of 1st year to reduce bias
  group_by(site) %>%
  arrange(year) %>%
  slice(-1L) %>%
  ungroup()



# ----------------- Setup Subtidal Cover Data --------------- #

ggplot(subtidal_jaccard,
       aes(x = year, y = jaccard, color = site)) +
  geom_point(alpha  = 0.4) + geom_line(alpha  = 0.4) +
  scale_color_brewer(palette = "Set3") +
  stat_smooth(method = "lm", fill = NA) +
  ylim(-0.1,0.5)

