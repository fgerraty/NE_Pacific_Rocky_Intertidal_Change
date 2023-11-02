##########################################################################
# Northeast Pacific Rocky Intertidal Change Project ######################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Clean Data ##################################################
#-------------------------------------------------------------------------

##########################################
# Part 1: Import Raw Datasets ############
##########################################

stars_GWA_raw <- read_csv("data/raw/KATMKEFJPWS_2006-2021_Sea_Star_Count.csv")

stars_GWA_b_raw <- read_csv("data/raw/KBAY2012-2023_Sea_Star_Anemone_Count.csv")

stars_MARINe_raw <- read_csv("data/raw/seastarkat_size_count_zeroes_totals.csv")

percent_cover_GWA_raw <- read_csv("data/raw/KATMKEFJPWS_2006-2021_Rocky_Intertidal_PercentCover.csv")

percent_cover_MARINe_raw <- read_csv("data/raw/MARINe_photoplots/photoplot_and_transects_summary.csv")



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


#Clean GWA sea star dataset A (Katmai NP, Kenai Fjords NP, and Western Prince William Sound)
stars_GWA_a <- stars_GWA_raw %>% 
  clean_names() %>% 
  #remove sites that were only surveyes a few times (non-primary GWA sites)
  filter(site_name %in% GWA_site_names) %>% 
  #remove surveys that were conducted across 100m of shoreline, since stretches of intertidal zone are not equivalent in terms of their habitat for sea stars
  filter(transect_length_m == 50) %>% 
  #select and rename important variables 
  select(site = site_name, 
         year = sample_year,
         species = species_name, 
         total = density_individuals_per_200_square_m) %>% 
  #Append georegion to dataframe
  left_join(., GWA_site_georegion_key, by = "site") %>% 
  #Append season to dataframe
  mutate(season = "Summer",
         num_plots_sampled = 1) %>% 
  #Pivot wider to have one row for each survey
  pivot_wider(names_from = species, values_from = total) %>% 
  #Rename columns with sea star 6-letter codes
  rename("DERIMB" = "Dermasterias imbricata",
         "EVATRO" = "Evasterias troschelii",
         "HENSPP" = "Henricia leviuscula",
         "MEDAEQ" = "Mediaster aequalis",
         "ORTKOE" = "Orthasterias koehleri",
         "PISOCH" = "Pisaster ochraceus",
         "PYCHEL" = "Pycnopodia helianthoides",
         "SOLSPP" = "Solaster stimpsoni")

#Clean GWA sea star dataset A (Kachemak Bay) #######

#Clean stars
stars_GWA_b_temp <- stars_GWA_b_raw %>% 
  clean_names() %>% 
  #Remove non-echinoderms
  filter(phylum == "Echinodermata") %>% 
  #Select relevant columns
  select(site, year, stratum, species,
         count = abundance_number_ind_100_m2) %>% 
  #Add totals of all transects together
  group_by(site, year, species) %>% 
  summarise(count = sum(count), .groups = "drop") %>% 
  #Add variables for clean data merge
  mutate(season = "Summer",
         num_plots_sampled = 4,
         georegion = "AK Kachemak Bay") %>% 
  #Pivot wider to have one row for each survey
  pivot_wider(names_from = species, 
              values_from = count,
              values_fill = 0) %>% 
  #Rename columns with sea star 6-letter codes
  rename("DERIMB" = "Dermasterias imbricata",
         "EVATRO" = "Evasterias troschelii",
         "HENSPP" = "Henricia leviuscula",
         "ORTKOE" = "Orthasterias koehleri",
         "PISOCH" = "Pisaster ochraceus",
         "PYCHEL" = "Pycnopodia helianthoides",
         "SOLSPP" = "Solaster spp.",
         "SOLSTI" = "Solaster stimpsoni",
         "LETNAN" = "Lethasterias nanimensis",
         "ASTSPP" = "Asterias sp.") %>% 
  #Combine all Solaster species into one variable
  mutate(SOLSPP = SOLSPP+SOLSTI) %>% 
  select(-SOLSTI)


#First, we have to make blank survey rows for the surveys without any stars detected

#To account for surveys in which no stars were detected, we will filter for all survey site/year combos in the raw data and then merge the datasets
all_KBAY_surveys <- stars_GWA_b_raw %>% 
  clean_names() %>% 
  select(site, year) %>% 
  unique() %>% 
  mutate(site_year = paste0(site, "_", year),
         georegion = "AK Kachemak Bay",
         season = "Summer",
         num_plots_sampled = 4,
         DERIMB = 0,EVATRO = 0, HENSPP = 0, ORTKOE = 0, 
         PISOCH = 0,PYCHEL = 0, SOLSPP = 0, LETNAN = 0,
         ASTSPP = 0)

#Select all the site/year combos for surveys with stars detected
KBAY_surveys_with_stars <- stars_GWA_b_temp %>% 
  select(site, year) %>% 
  unique() %>% 
  mutate(site_year = paste0(site, "_", year))

#Create dataframe with surveys that no stars were detected
KBAY_empty_surveys <- subset(all_KBAY_surveys, 
                             !(site_year %in% KBAY_surveys_with_stars$site_year)) %>% 
  select(-site_year)


#Finally, bind the blank and non-blank survey rows together for KBAY surveys
stars_GWA_b <- bind_rows(stars_GWA_b_temp, KBAY_empty_surveys)



#Part 2B: MARINe sea star dataset ----------------------------------------------

stars_MARINe <- stars_MARINe_raw %>% 
  clean_names() %>% unique() %>% 
  #remove katharina tunicata (not a star)
  filter(species_code != "KATTUN") %>% 
  #select and rename key columns
  select(georegion,
         site = marine_site_name, 
         year = marine_common_year,
         season = season_name,
         num_plots_sampled,
         species = species_code, 
         size_bin, total) %>% 
  #group by survey metadata and calculate the total number of stars per survey (remove size class bins)
  group_by(georegion, site, year, season, num_plots_sampled, species) %>% 
  summarise(total = sum(total), .groups = "drop") %>% 
  #pivot wider to determine have one row per survey, fill blanks with 0s
  pivot_wider(names_from = species, 
              values_from = total, 
              values_fill = 0) %>% 
  #Remove leptasterias (cryptic and not surveyed by other programs)
  select(-LEPTAS) %>% 
  #Filter out 5-plot surveys at mussel shoals (2019-2021)
  filter(!(site == "Mussel Shoals" & num_plots_sampled == 5)) %>% 
  #Filter out surveys in which not all the plots were surveyed
  group_by(site) %>% 
  filter(num_plots_sampled == max(num_plots_sampled)) %>% 
  ungroup()

  


# Combine sea star datasets together -------------------------------------------

#Create new "stars" dataframe by combining all sea star datasets
stars <- bind_rows(stars_GWA_a, stars_GWA_b, stars_MARINe) %>% 
  #replace NA values from non-shared columns with zeros.
  replace_na(list(MEDAEQ = 0, SOLSPP = 0, 
                  PISGIG = 0, PATMIN = 0, 
                  PISBRE = 0, ASTSPP = 0, 
                  LETNAN = 0)) %>% 
  #remove irrelevant column
  select(-num_plots_sampled)

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

