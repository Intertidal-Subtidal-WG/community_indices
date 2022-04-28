filter_intertidal_for_analysis <- . %>%
  filter(year >= 1986,
         level >= 0,
         level <= 13,
         intertidal_transect %in% c(5,7,26,15,20,22),
         !organism %in% c("Bare rock",
                          "Black zone",
                          "Brown ground",
                          "Little round green things",
                          "Shell hash")) %>%
  select(site, intertidal_transect, year, replicate, level, tide_height_rel_mllw, 
         organism, value, measure)%>%
  mutate(exposure = ifelse(intertidal_transect %in% c(5,7,26), 
                           "sheltered", "exposed"),
         intertidal_transect = as.character(intertidal_transect)) %>%
  
  #low, mid, high - barnacle max or asco median
  #1.5 and below as low based on upper edge of Chondrus on SE 
  # 1.5 - 2.5 as mid #based on max barnacles on SE appledore
  # 3 + as high
  
  mutate(height = case_when(
    tide_height_rel_mllw < 1.5 ~ "low",
    tide_height_rel_mllw >= 1.5 & tide_height_rel_mllw < 2.5 ~ "mid",
    TRUE ~ "high"),
        height = factor(height, levels = c("low", "mid", "high"))
      )
