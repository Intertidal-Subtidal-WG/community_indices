#'---------------------------------------
#' Helper functions/workflows used
#' in multiple files
#'---------------------------------------

library(vegan)

# nest() makes a tibble
# but for vegdist, you need a data frame
# with rownames
add_data_year_rownames <- function(adf){
  new_names <- adf %>% pull(year)
  adf <- adf[,-1]
  adf <- as.data.frame(adf)
  rownames(adf) <- new_names
  adf
}


# after nesting data that has been
# pivoted wide, often there are species
# which are not present, which can throw
# vegan functions
remove_null_cols <- function(adf){
  idx <- which(colSums(adf)==0)
  adf[,-idx]
}


# after nesting data that has been
# pivoted wide, often there are years
# with all 0s (bare space), which can throw
# vegan functions
remove_null_rows <- function(adf){
  idx <- which(rowSums(adf)==0)
  if(length(idx)==0) return(adf)
  adf[-idx,]
}


make_jaccard_timeseries <- . %>%
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

