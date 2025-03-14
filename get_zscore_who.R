# Shane Ridoux
# 250314
# look up zscore for wlz, waz, laz

library(tidyverse)
library(readxl)
library(dplyr)

# #read in who tables
# zm_wfl <- read_excel("wfl_boys_0-to-2-years_zscores.xlsx") %>% as.data.frame()
# zf_wfl <- read_excel("wfl_girls_0-to-2-years_zscores.xlsx") %>% as.data.frame()

get_wlz <- function(data_boys, data_girls, Sex, Wt, Len){
  # Check if Sex is valid
  if (!(Sex %in% c(1, 2))) {
    return(NA)  # Return NA for invalid sex values
  }
  
  # Select dataset based on Sex
  if (Sex == 2) {
    df <- data_boys  # Boys dataset
  } else {
    df <- data_girls  # Girls dataset
  }
  
  # Find closest length
  closest_row <- df %>%
    slice_min(abs(Length - Len), n = 1, with_ties = FALSE)
  
  # Check if row exists
  if (nrow(closest_row) == 0) {
    return(NA)  # Return NA if no matching length found
  }
  
  # Extract LMS values
  L <- closest_row$L
  M <- closest_row$M
  S <- closest_row$S
  X <- Wt
  
  # Compute WLZ using the LMS formula
  WLZ <- ((X / M)^L - 1) / (L * S)
  
  return(WLZ)
}


# id <- which(is.na(demo[which(demo$study_id=="MINT-177"),"wlz"])) 
# obs_weight <- as.numeric(demo[which(demo$study_id=="MINT-177"),"weight_average"][id,])
# sex <- as.integer(demo[which(demo$study_id=="MINT-177"),"gender"][id,])
# l <- as.numeric(demo[which(demo$study_id=="MINT-177"),"length_measurement_average"][id,])
# demo[which(demo$study_id=="MINT-177"),"wlz"] <- get_wlz(data_boys = zm_wfl,
#                                                         data_girls = zf_wfl,
#                                                         Sex = 2,
#                                                         Wt = obs_weight,
#                                                         Len = 82.2)
