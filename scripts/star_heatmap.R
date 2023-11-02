
#Star Heatmap Plot #########

#Import Raw Data
stars <- read_csv("data/clean/stars.csv")


all_stars_relative_abundance <- stars %>% 
  mutate(total_stars = rowSums(.[6:18]))


temp <- all_stars_relative_abundance %>% 
  
  

         