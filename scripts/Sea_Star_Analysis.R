# 
#     Gulf Watch Sea Star Data Analysis
#
#     Author: Frankie Gerraty
#     Date: July 31, 2022
# 
# ------------------------------------------------------------------------------

# Step 1: Import Data and Prepare the Console for Data Manipulation and Analysis

# Load packages
library(readr)
library(tidyverse)

#Set working directory
setwd("~/Documents/R/Gulf_Watch_Alaska/Sea_Star")

# Import raw dataset 
GWA_Sea_Star_Raw_Data <- read.csv("KATMKEFJPWS_2006-2021_Sea_Star_Count.csv")
GWA_Sea_Star_Raw_Data <- rename(GWA_Sea_Star_Raw_Data, Density = Density..individuals.per.200.square.m.) 

# -------------------------------------------------------------------------------

# Step 2: Data Manipulation - Change in Sea Star Density (All Species) Over Time

All_Species_Summary <- GWA_Sea_Star_Raw_Data %>% 
  select(Site_Code, Site_Name, Sample_Year, Species_Name, Density) %>% #Choose relevant columns to bring into new dataframe
  group_by(Site_Code, Site_Name, Sample_Year) %>% #group by site and year
  summarise(Density = sum(Density)) #add the densities of all sea star species together for each site/year combination
.groups="drop"
  
  
All_Sites_Plot1 <- ggplot(All_Species_Summary, aes(x=Sample_Year, y=Density)) + 
  geom_bar(stat = "identity") +
  facet_wrap(vars(Site_Name), scales = "free_y")
All_Sites_Plot1


