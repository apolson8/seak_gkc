# K.Palof & A.Olson 

# Objective: Summarise and visualize Southeast AK Golden King crab data
# This code contains data processing and summarization and resulting code to create figures.
# (similar to but reduced version of harv_cpue.R)

# SEAK GKC Harvest and CPUE trends -----------

# load ---------
source("./r/helper.R")

# global ---------
cur_yr = 2019 # most recent year of data
fig_path <- paste0('figures/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', cur_yr) # output and results
dir.create(output_path) 

# Select for mgt areas #
target <- c("East Central GKC", "Icy Strait GKC", "Lower Chatham Strait GKC", 
            "Mid-Chatham Strait GKC","North Stephens Passage GKC", "Northern GKC",
            "Southern GKC")

# load data ------------
# Import fishticket and logbook data from ALEX
gkc_fish <- read.csv("data/fishery/gkc_fishticket.csv")
gkc_log <- read.csv("data/fishery/gkc_logbook.csv")
# here or in readme need how to pull this data **FIX**

# process data ----------------
# lbs per pot day -----------
# Use fishticket data #
head(gkc_fish)

# Active fishing season -----------
# based on first and last haul dates
gkc_fish %>% 
  filter(!is.na(CATCH_DATE), !is.na(SELL_DATE), I_FISHERY %in% target) %>%
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                           ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(I_FISHERY == "Northern GKC", "Northern",
                                                              ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc")))))))) %>% 
  group_by(YEAR, mgt_area) %>% 
  mutate(CATCH_DATE = as.character(CATCH_DATE), mdy = mdy(CATCH_DATE)) %>% 
  select(YEAR, mgt_area, mdy) %>%
  summarise(min = min(mdy), max = max(mdy), diff = max-min) %>% 
  mutate(diff = as.numeric(diff, units = "days")) %>% 
  mutate(diff = replace(diff, which(diff == 0), 14)) -> season_leng 
# change diff that are 0 to 14 days - average "trip" based on tides. pers.comm A.Olson, K.Palof
#Convert difference in season days to a numeric value for further calculations

# Harvest by mgt area and year ----------
gkc_fish %>% 
  filter(!is.na(CATCH_DATE), !is.na(SELL_DATE), !is.na(POUNDS), I_FISHERY %in% target) %>% 
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                           ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(I_FISHERY == "Northern GKC", "Northern",
                                                              ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc")))))))) %>%
  group_by(YEAR, mgt_area) %>%
  summarise(total_lbs = sum(POUNDS), 
            permits = length(unique(ADFG_NO))) -> harv # add permits using number of uniqeu ADF&G no

harv %>% 
  full_join(season_leng) %>% 
  select(YEAR, mgt_area, diff, total_lbs, permits)  %>% 
  mutate(cpue = total_lbs / diff, 
         cpue2 = total_lbs / diff / permits) -> lbs_per_day

# lbs per active fishing day figure ------------------
# make sure functions are loaded from helper.R file
lbs_per_day_graph(1983, 2010, "East Central", lbs_per_day, cur_yr)
lbs_per_day_graph(1983, 2011, "Icy Strait", lbs_per_day, cur_yr)
lbs_per_day_graph(1983, 2017, "Lower Chatham", lbs_per_day, cur_yr) 
lbs_per_day_graph(1983, 2007, "Mid-Chatham", lbs_per_day, cur_yr)
lbs_per_day_graph(1983, 2017, "North Stephens Passage", lbs_per_day, cur_yr)
lbs_per_day_graph(1983, 2017, "Northern", lbs_per_day, cur_yr)
lbs_per_day_graph(1983, 2017, "Southern", lbs_per_day, cur_yr)

# Logbook CPUE -----------
gkc_log %>% filter(TARGET_SPECIES_CODE == 923, !is.na(TARGET_SPECIES_RETAINED),
                   !is.na(NUMBER_POTS_LIFTED), !is.na(I_FISHERY)) %>%
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                           ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(I_FISHERY == "Northern GKC", "Northern",
                                                              ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc"))))))),
         cpue = TARGET_SPECIES_RETAINED / NUMBER_POTS_LIFTED) %>% 
  select(YEAR, mgt_area, cpue, NUMBER_POTS_LIFTED) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(YEAR, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(NUMBER_POTS_LIFTED)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se) -> cpue_log

# Figure Logbook CPUE ------------------------------
logbk_cpue(2000, 2017, "East Central", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Icy Strait", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2012, "Lower Chatham", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Mid-Chatham", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2001, 2017, "North Stephens Passage", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Northern", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Southern", cpue_log, 0.75, 0.50, cur_yr)

