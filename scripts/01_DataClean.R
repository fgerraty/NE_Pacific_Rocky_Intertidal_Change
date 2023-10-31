
##########################################
# Part 1: Import Raw Datasets ############
##########################################

stars_GWA_raw <- read_csv("data/raw/KATMKEFJPWS_2006-2021_Sea_Star_Count.csv")

stars_MARINe_raw <- read_csv("data/raw/seastarkat_size_count_zeroes_totals.csv")

percent_cover_GWA_raw <- read_csv("data/raw/KATMKEFJPWS_2006-2021_Rocky_Intertidal_PercentCover.csv")

percent_cover_MARINe_raw <- read_csv("data/raw/photoplot_and_transects_summary.csv")



##################################################
# Part 2: Clean Sea Star Datasets ################
##################################################

#Part 2A: Gulf Watch Alaska Sea Star Datasets ----------------------------------

#Create a key for GWA sites and georegions. These include 21 sites in 4 georegions in Alaska (Prince William Sound, Kachemak Bay, Kenai Fjords NP, Katmai NP)
GWA_site_names = c("Amalik Bay", "Kaflia Bay", "Kinak Bay", "Kukak Bay", "Takli Island", "Bishop's Beach", "Bluff Point", "Cohen Island", "Elephant Island", "Outside Beach", "Port Graham", "Aialik Bay", "Harris Bay", "McCarty Fjord", "Nuka Bay", "Nuka Passage", "Herring Bay", "Hogan Bay", "Iktua Bay", "Johnson Bay", "Whale Bay")

GWA_site_georegion_key <- data.frame(
  site = GWA_site_names, 
  georegion = c(rep("AK Prince William Sound",5), 
  rep("AK Kachemak Bay",6), 
  rep("AK Kenai Fjords",5),
  rep("AK Katmai",5)))


#Clean GWA sea star dataset
stars_GWA_a <- stars_GWA_raw %>% 
  clean_names() %>% 
  #select and rename important variables 
  select(site_code, 
         site = site_name, 
         year = sample_year,
         species = species_name, 
         density = density_individuals_per_200_square_m) %>% 
  #remove sites that were only surveyes a few times (non-primary GWA sites)
  filter(site %in% GWA_site_names) %>% 
  #Append georegion to dataframe
  left_join(., GWA_site_georegion_key, by = "site") %>% 
  #Append season to dataframe
  mutate(season = "Summer",
         num_plots_sampled = 1) %>% 
  #Pivot wider to have one row for each survey
  pivot_wider(names_from = species, values_from = density) %>% 
  #Rename columns with sea star 6-letter codes
  rename("DERIMB" = "Dermasterias imbricata",
         "EVATRO" = "Evasterias troschelii",
         "HENSPP" = "Henricia leviuscula",
         "MEDAEQ" = "Mediaster aequalis",
         "ORTKOE" = "Orthasterias koehleri",
         "PISOCH" = "Pisaster ochraceus",
         "PYCHEL" = "Pycnopodia helianthoides",
         "SOLSPP" = "Solaster stimpsoni")

#Part 2B: MARINe sea star dataset ----------------------------------------------

stars_MARINe <- stars_MARINe_raw %>% 
  clean_names() %>% unique() %>% 
  #select and rename key columns
  select(site_code = site_code,
         georegion,
         site = marine_site_name, 
         year = marine_common_year,
         season = season_name,
         num_plots_sampled,
         species = species_code, 
         size_bin, total) %>% 
  #group by survey metadata and calculate the total number of stars per survey (remove size class bins)
  group_by(site_code, georegion, site, year, season, num_plots_sampled, species) %>% 
  summarise(total = sum(total), .groups = "drop") %>% 
  #divide total number of stars counted by number of plots surveyed
  mutate(density_per_plot = total/num_plots_sampled) %>% 
  #remove unnecessary columns
  select(-total,
         #-num_plots_sampled #Keeping due to data error! 
         ) %>% 
  #pivot wider to determine have one row per survey, fill blanks with 0s
  pivot_wider(names_from = species, 
              values_from = density_per_plot, 
              values_fill = 0) %>% 
  #Remove katharina (not an echinoderm) and leptasterias (cryptic and not surveyed by other programs)
  select(-KATTUN, -LEPTAS)


# Combine sea star datasets together -------------------------------------------

#Create new "stars" dataframe by combining all sea star datasets
stars <- bind_rows(stars_GWA, stars_MARINe) %>% 
  #replace NA values from non-shared columns with zeros.
  replace_na(list(MEDAEQ = 0, SOLSPP = 0, PISGIG = 0, PATMIN = 0, PISBRE = 0))





# Export combined sea star dataset in "clean" data folder
write_csv(stars, "data/clean/stars.csv")

#######################################################################
# Part 2: Clean "Photo Quadrat" Percent Cover Datasets ################
#######################################################################

# Part 1: Gulf Watch Alaska Percent Cover dataset -----------------------------

percent_cover_GWA <- percent_cover_GWA_raw %>% 
  clean_names() %>%
  #select and rename important columns
  select(site_code, 
         site = site_name, 
         year = year_sampled,
         quadrat = quadrat_num, 
         elevation,
         species = species_name, 
         percent_cover) %>% 
  #Append georegion to dataframe
  left_join(., GWA_site_georegion_key, by = "site")
  

percent_cover_MARINe <- percent_cover_MARINe_raw %>% 
  clean_names()