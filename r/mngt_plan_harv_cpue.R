# K.Palof & A.Olson 

# Objective: Summarise and visualize Southeast AK Golden King crab data
# This code contains data processing and summarization and resulting code to create figures.
# (similar to but reduced version of harv_cpue.R)

# SEAK GKC Harvest and CPUE trends -----------

# load ---------
source("./r/helper.R")

# global ---------
cur_yr = 2020 # most recent year of data
fig_path <- paste0('figures/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', cur_yr) # output and results
dir.create(output_path) 



# load data ------------
# Import fishticket and logbook data from ALEX
read.csv("data/fishery/gkc_fishticket.csv") %>% 
  clean_names () -> gkc_fish
read.csv("data/fishery/gkc_logbook.csv") %>%
  clean_names() -> gkc_log
read.csv("data/fishery/gkc_ghls.csv") %>%
  clean_names() -> gkc_ghl

# Select for mgt areas #
target <- c("East Central GKC", "Icy Strait GKC", "Lower Chatham Strait GKC", 
            "Mid-Chatham Strait GKC","North Stephens Passage GKC", "Northern GKC",
            "Southern GKC")

# here or in readme need how to pull this data **FIX**


# process data ----------------
# lbs per pot day -----------
# Use fishticket data #
head(gkc_fish)

# Active fishing season -----------
# based on first and last haul dates
gkc_fish %>% 
  filter(!is.na(catch_date), !is.na(sell_date), i_fishery %in% target) %>%
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) %>% 
  group_by(year, mgt_area) %>% 
  mutate(catch_date = as.character(catch_date), mdy = mdy(catch_date)) %>% 
  select(year, mgt_area, mdy) %>%
  summarise(min = min(mdy), max = max(mdy), diff = max-min) %>% 
  mutate(diff = as.numeric(diff, units = "days")) %>% 
  mutate(diff = replace(diff, which(diff == 0), 14)) -> season_leng 
# change diff that are 0 to 14 days - average "trip" based on tides. pers.comm A.Olson, K.Palof
#Convert difference in season days to a numeric value for further calculations

# Harvest by mgt area and year ----------
gkc_fish %>% 
  filter(!is.na(catch_date), !is.na(sell_date), !is.na(pounds), i_fishery %in% target) %>% 
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) %>%
  group_by(year, mgt_area) %>%
  summarise(total_lbs = sum(pounds), 
            permits = length(unique(adfg_no))) -> harv # add permits using number of uniqeu ADF&G no

harv %>%
  full_join(gkc_ghl) %>%
  select(year, mgt_area, ghl, total_lbs) %>%
  mutate(ghl = as.numeric(ghl)) -> harv_ghl



#harvest by mgt area figures -----
#need to add GHLs to figures
hvst_area("Northern", harv_ghl, cur_yr)
hvst_area("East Central", harv, cur_yr)
hvst_area("Icy Strait", harv, cur_yr)
hvst_area("Lower Chatham", harv, cur_yr)
hvst_area("Mid-Chatham", harv, cur_yr)
hvst_area("Southern", harv, cur_yr)
hvst_area("North Stephens Passage", harv, cur_yr)

harv %>% 
  full_join(season_leng) %>% 
  select(year, mgt_area, diff, total_lbs, permits)  %>% 
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
gkc_log %>% filter(target_species_code == 923, !is.na(target_species_retained),
                   !is.na(number_pots_lifted), !is.na(i_fishery)) %>%
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc"))))))),
         cpue = target_species_retained / number_pots_lifted) %>% 
  select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
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


# panel figure  ------------
# has both of the above figures together 
## panel figure for both - need both summary files here -------
panel_figure(1983, 2010, 2000, 2017, "East Central", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2011, 2000, 2017, "Icy Strait", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2012, "Lower Chatham", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2007, 2000, 2017, "Mid-Chatham", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2001, 2017, "North Stephens Passage", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2017, "Northern", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2017, "Southern", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

