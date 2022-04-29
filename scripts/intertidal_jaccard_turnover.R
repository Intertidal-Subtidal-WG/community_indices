#'-----------------------------------------------------
#' @title Intertidal sessile turnover through time analysis
#'-----------------------------------------------------


library(tidyverse)
library(smldata)
library(emmeans)
theme_set(theme_bw())
source("scripts/helpers.R")
source("scripts/vegan_helpers.R")

# ----------------- Setup Intertidal Cover Data --------------- #

data("sml_intertidal_cover")

# filter to consistently sampled transectsm, tide levels, and years
cover_dat <- sml_intertidal_cover %>%
  filter_intertidal_for_analysis


intertidal_turnover <- cover_dat %>%
  filter(height != "high") %>%
  make_jaccard_timeseries %>% 
  filter(year != 1986) #get rid of 1st year to reduce bias



# plot it!

ggplot(intertidal_turnover,
       aes(x = year, y = jaccard, color = height)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_brewer(palette = "Dark2") +
  stat_smooth(method = "lm", fill = NA, 
              mapping = aes(group = paste(exposure, height)))



#exposure model
mod_height_j <- lm(jaccard ~ year*exposure*height, 
                 data = intertidal_turnover)

itsadug::acf_resid(mod_height_j, split_pred = c("exposure","height"),
                   n=4)


car::Anova(mod_height_j)
summary(mod_height_j)
coef(mod_height_j)[2]*1e3



emtrends(mod_height_j, ~ height + exposure, "year") %>%
  broom::tidy() %>%
  ggplot(aes(x = exposure, y = year.trend, color = height,
             ymin = year.trend - std.error*2,
             ymax = year.trend  + std.error*2)) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_brewer(palette = "Dark2") +
   coord_flip() 

