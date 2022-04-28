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


### Filters out sessile species in count data
### and inconsistently sampled species
filter_intertidal_for_mobile <- . %>%
  mutate(year_sq = year^2,
         year_zeroed = year - 1986,
         year_zeroed_sq = year_zeroed^2) %>%
  #get rid of non-sessile species
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
               "Spirorbis",
               "Hiatella arctica",
               "Mya arenaria"
             ))) %>%
  #things sampled in early 2000s in scrapes, but not elsewhere
  filter(!(organism %in% c(
    "Acoela",                            
    "Cirratulus",                        
    "Euplana",                           
    "Lepidonotus",                       
    "Lycastopsis",                      
    "Micrura affinis",
    "Nematodes",
    "Nemertean",                         
    "Nicolea",
    "Notoplana",                                              
    "Oligochaete",
    "Ophiopholis",                       
    "platyhelminthes",                   
    "Polychaeta",
    "Skeneopsis planorbis",              
    "Turbellarid")))

# 
# POSSIBLE COUNT METHOD CHANGES OVERTIME
# "Amphipoda"
# "Anurida maritima"
# "Colus stimpsoni"
# "copepods"                          
# "Gammarid"
# "Halacarus"
# "Idotea balthica"                   
# "Idotea baltica"
# "Isopods"                           
# "Lacuna vincta"  
