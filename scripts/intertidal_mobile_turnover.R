#'-----------------------------------------------------
#' @title Intertidal sessile turnover through time analysis
#'-----------------------------------------------------


library(tidyverse)
library(smldata)
library(lme4)
library(emmeans)
theme_set(theme_bw())
source("scripts/helpers.R")
source("scripts/vegan_helpers.R")


# ----------------- Setup Intertidal Count Data --------------- #

data("sml_intertidal_count")

# filter to consistently sampled transectsm, tide levels, and years
count_dat <- sml_intertidal_count %>%
  filter_intertidal_for_analysis %>%
  filter_intertidal_for_mobile

# ----------------- Create Jaccard Timeseries --------------- #
count_jaccard <- count_dat %>%
  filter(height != "high") %>%
  make_jaccard_timeseries %>%
  mutate(year_zeroed = year - 1986,
         year_zeroed_sq = year_zeroed^2) %>% 
  filter(year != 1986) #get rid of 1st year to reduce bias




ggplot(count_jaccard,
       aes(x = year, y = jaccard, color = height)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_brewer(palette = "Dark2") +
  stat_smooth(method = "lm", fill = NA, 
              mapping = aes(group = paste(exposure, height)))



#exposure model
mod_height_j <- lmer(jaccard ~ year_zeroed*exposure*height + 
                       (1|intertidal_transect), 
                   data = count_jaccard)

# 
# mod_height_j_sq <- lm(jaccard ~ year_zeroed*exposure*height +
#                         year_zeroed_sq*exposure*height, 
#                    data = count_jaccard)
# 
# AIC(mod_height_j, mod_height_j_sq)

itsadug::acf_resid(mod_height_j, split_pred = c("exposure","height"))


car::Anova(mod_height_j)
summary(mod_height_j)

pred_j <- emmeans(mod_height_j, ~ exposure+height) %>%
  broom::tidy(conf.int = TRUE)

ggplot(count_jaccard,
       aes(x = year, y = jaccard, 
           color = height, group = intertidal_transect)) +
  geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(vars(exposure)) +
  geom_hline(data = pred_j,
             aes(yintercept = estimate, color = height),
             lwd = 1.5) +
  geom_hline(data = pred_j,
             aes(yintercept = conf.low, color = height),
              lty = 2) +
  geom_hline(data = pred_j,
             aes(yintercept = conf.high, color = height),
             lty = 2) 
              

