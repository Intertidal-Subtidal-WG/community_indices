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

