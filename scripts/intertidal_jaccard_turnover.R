#'-----------------------------------------------------
#' @title Intertidal richness through time analysis
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
  group_by(site, intertidal_transect, 
           replicate, year, exposure, level, height, organism) %>%
  summarize(value = sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(site, intertidal_transect, 
           year, exposure, height, organism) %>% #height as the group
  summarize(value = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_wider(values_from = value, names_from = organism,
              values_fill = 0)  %>% #assume missing = 0
  group_by(site, intertidal_transect, 
           exposure, height) %>%
  arrange(year) %>%
  nest() %>%
  mutate(
    data = purrr::map(data, add_data_year_rownames), #get it into vegan
    data = purrr::map(data, remove_null_cols), #remove null species
    data = purrr::map(data, remove_null_rows), #remove years in bare patches
    jaccard_mat = purrr::map(data, ~1 - 
                               as.matrix(
                                 vegdist(.x, method = "jaccard"))),
    j = purrr::map(jaccard_mat,
                            ~data.frame(year = rownames(.x),
                                        jaccard = .x[,1]
                            ))) %>%
  unnest(j) %>%
  mutate(year = as.numeric(year))%>% 
  filter(year != 1986) #get rid of 1st year to reduce bias


# plot it!

ggplot(intertidal_turnover,
       aes(x = year, y = jaccard, color = height)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_brewer(type = "qual") +
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
  scale_color_brewer(type = "qual") + coord_flip() 

