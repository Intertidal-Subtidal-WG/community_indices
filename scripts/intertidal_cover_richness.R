#'-----------------------------------------------------
#' @title Intertidal richness through time analysis
#'-----------------------------------------------------


library(tidyverse)
library(smldata)
library(lme4)
library(emmeans)
theme_set(theme_bw())
source("scripts/helpers.R")

# ----------------- Setup Intertidal Cover Data --------------- #

data("sml_intertidal_cover")

# filter to consistently sampled transectsm, tide levels, and years
cover_dat <- sml_intertidal_cover %>%
  filter_intertidal_for_analysis
  
# Richness by exposure, no height
intertidal_cover_richness <- cover_dat %>%
  mutate(organism = ifelse(value==0, NA, organism)) %>% #so they are not counted, but rows are kept
  group_by(site, intertidal_transect, year, exposure, measure) %>%
  summarize(richness = n_distinct(organism, na.rm=TRUE))

# plot!
ggplot(intertidal_cover_richness,
       aes(x = year, y = richness, color = exposure)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_manual(values = c("orange", "purple")) +
  stat_smooth()

# a model looking at transect to examine temporal autocorrelation
transect_mod <- lm(richness ~ year*intertidal_transect,
          data = intertidal_cover_richness)
itsadug::acf_resid(transect_mod, split_pred = "intertidal_transect", n = 6)
par(mfrow=c(1,1))

# all-island exposure model
mod <- lmer(richness ~ year*exposure + (1|intertidal_transect), 
             data = intertidal_cover_richness)
performance::check_model(mod)  
summary(mod)
car::Anova(mod, test = "F")


# So, what does this look like from a emmeans 

# ---------- Richness by exposure and height ---------

# Richness by exposure!
intertidal_richness <- cover_dat %>%
  mutate(organism = ifelse(value==0, NA, organism)) %>% #so they are not counted, but rows are kept
  group_by(site, intertidal_transect, year, exposure, height) %>%
  summarize(richness = n_distinct(organism, na.rm=TRUE)) 


ggplot(intertidal_richness,
       aes(x = year, y = richness, color = height)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_brewer(palette = "Dark2") +
  stat_smooth()


#exposure model
mod_height <- lmer(richness ~ year*exposure*height + (1|intertidal_transect), 
          data = intertidal_richness)

car::Anova(mod_height, test = "F")
emtrends(mod_height, ~ height + exposure, "year") %>%
  broom::tidy() %>%
  ggplot(aes(x = exposure, y = year.trend, color = height,
             ymin = year.trend - std.error*2,
             ymax = year.trend  + std.error*2)) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() 
