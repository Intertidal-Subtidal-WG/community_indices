library(tidyverse)
library(smldata)
theme_set(theme_bw())
source("helpers.R")

# ----------------- Setup Intertidal Cover Data --------------- #

data("sml_intertidal_count")

# filter to consistently sampled transectsm, tide levels, and years
count_dat <- sml_intertidal_count %>%
  filter_intertidal_for_analysis %>%
  mutate(year_sq = year^2,
         year_zeroed = year - 1986,
         year_zeroed_sq = year_zeroed^2) %>%
  filter(!(organism %in% #get out the sessile species
             c("Anomia simplex",
               "Aplidium",
               "Crisia eburna",
               "Diadumene lineata" ,
               "Flustrellidra hispida",
               "Halichondria",
               "Metridium senile" ,
               "Modiolus modiolus",
               "Molgula",
               "Mussel",
               "Mytilus edulis" ,
               "Ostrea edulis",
               "Semibalanus balanoides",
               "Spirorbis"
               )))


# Richness by exposure, no height
intertidal_count_richness <- count_dat %>%
  mutate(organism = ifelse(value==0, NA, organism)) %>% #so they are not counted, but rows are kept
  group_by(site, intertidal_transect, 
           year, year_sq, year_zeroed, year_zeroed_sq,
           exposure, measure) %>%
  summarize(richness = n_distinct(organism, na.rm=TRUE))


# plot!
ggplot(intertidal_count_richness,
       aes(x = year, y = richness, color = exposure)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_manual(values = c("orange", "purple")) +
  stat_smooth(method = "lm")



# a model looking at transect to examine temporal autocorrelation
transect_mod <- lm(richness ~ year_zeroed*intertidal_transect,
                   data = intertidal_count_richness)
itsadug::acf_resid(transect_mod, split_pred = "intertidal_transect")
par(mfrow=c(1,1))

transect_mod_sq <- lm(richness ~ year_zeroed*intertidal_transect +
                        year_zeroed_sq*intertidal_transect,
                   data = intertidal_count_richness)

AIC(transect_mod, transect_mod_sq)

# all-island exposure model
exposure_mod <- lmer(richness ~ year_zeroed*exposure + 
                       year_zeroed_sq*exposure +
                       (1|intertidal_transect), 
            data = intertidal_count_richness)
performance::check_model(exposure_mod)  
car::Anova(exposure_mod, test = "F")
summary(exposure_mod)

broom.mixed::tidy(exposure_mod) %>%
  filter(effect == "fixed") %>%
  select(-group)

#model predictions
exposure_est_rich <- emmeans(exposure_mod, ~year_zeroed +  year_zeroed_sq + exposure,
        at = list(year_zeroed = 0:34, year_zeroed_sq = c(0:34)^2)) %>%
  broom::tidy(conf.int = TRUE) %>%
  filter(year_zeroed_sq == year_zeroed^2) %>%
  mutate(year = year_zeroed + 1986) 

ggplot(data = exposure_est_rich,
       aes(x = year, y = estimate, color = exposure)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  group = exposure),
              alpha = 0.1, color = "grey") +
  labs(x="", y = "estimated mobile\nspecies richness") +
  scale_color_manual(values = c("orange", "purple")) 
  

# ---------- Richness by exposure and height ---------

# Richness by exposure!
intertidal_richness <- count_dat %>%
  mutate(organism = ifelse(value==0, NA, organism)) %>% #so they are not counted, but rows are kept
  group_by(site, intertidal_transect, 
           year, year_sq, year_zeroed, year_zeroed_sq,
           exposure, height) %>%
  summarize(richness = n_distinct(organism, na.rm=TRUE)) 


ggplot(intertidal_richness,
       aes(x = year, y = richness, color = height)) +
  geom_point() + geom_line() +
  facet_wrap(vars(exposure, intertidal_transect)) +
  scale_color_brewer(palette = "Dark2") +
  stat_smooth()


#exposure model
mod_height <- lm(richness ~ year_zeroed*exposure*height, 
                 data = intertidal_richness)


mod_height_sq <- lm(richness ~ year_zeroed*exposure*height +
                      year_zeroed_sq*exposure*height, 
                 data = intertidal_richness)

AIC(mod_height, mod_height_sq)

car::Anova(mod_height_sq)


#model predictions
height_est_rich <- emmeans(mod_height_sq, 
                           ~year_zeroed +  year_zeroed_sq + 
                             exposure + height,
                             at = list(year_zeroed = 0:34, 
                                       year_zeroed_sq = c(0:34)^2)) %>%
  broom::tidy(conf.int = TRUE) %>%
  filter(year_zeroed_sq == year_zeroed^2) %>%
  mutate(year = year_zeroed + 1986,
         height = factor(height, levels = c("low", "mid", "high"))) 



ggplot(data = height_est_rich,
       aes(x = year, y = estimate, color = height)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(exposure)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  group = height),
              alpha = 0.1, color = "grey") +
  labs(x="", y = "estimated mobile\nspecies richness") +
  scale_color_brewer(palette = "Dark2") 
  