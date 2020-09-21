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
read.csv("data/fishery/tanner_logbook.csv") %>%
  clean_names() -> tanner_log

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
hvst_area("East Central", harv_ghl, cur_yr)
hvst_area("Icy Strait", harv_ghl, cur_yr)
hvst_area("Lower Chatham", harv_ghl, cur_yr)
hvst_area("Mid-Chatham", harv_ghl, cur_yr)
hvst_area("Southern", harv_ghl, cur_yr)
hvst_area("North Stephens Passage", harv_ghl, cur_yr)

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
         ul = cpue + 2 * se,
         outlier = ifelse(mgt_area == "Lower Chatham" & #excludes outlier for Lower Chatham
                            year == 2013, "yes", "no")) %>%
  filter(outlier != "yes") -> cpue_log

cpue_log %>%
  group_by(mgt_area) %>%
  filter(year <= 2017) %>%
  summarise(mean = mean(cpue),
            seventy_five = mean *0.75,
            fifty = mean * 0.5)

# Figure Logbook CPUE ------------------------------
logbk_cpue(2000, 2017, "East Central", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Icy Strait", cpue_log, 0.75, 0.50, cur_yr)
logbk_cpue(2000, 2017, "Lower Chatham", cpue_log, 0.75, 0.50, cur_yr)
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


#Tanner and GKC logbook to remove Tanner bias-----
tanner_log %>%
  group_by(year, adfg_no, effort_date, district, sub_district) %>%
  summarise(num_tanner = sum(target_species_retained)) -> tanner_log

gkc_log %>%
  mutate(num_gkc = target_species_retained,
         mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) -> gkc_log

left_join(gkc_log, tanner_log, by = c("year", "adfg_no", "effort_date", "district", "sub_district")) -> gkc_tanner_log

gkc_tanner_log %>%
  mutate(num_tanner = replace_na(num_tanner, 0), 
         prop_gkc = num_gkc/(num_tanner + num_gkc),
         prop_gkc = replace_na(prop_gkc, 1)) -> log_summary

#need to look at CPUE scenarios based on proportion of harvest being GKC


gkc_dir_100 <-log_summary %>%
  filter(prop_gkc == 1,
         target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = "=100%") 

#90% GKC harvest
gkc_dir_90 <-log_summary %>%
  filter(prop_gkc >= 0.9,
         target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = ">=90%") 

#80% GKC Harvest
gkc_dir_80 <-log_summary %>%
  filter(prop_gkc >= 0.8,
         target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = ">=80%") 

#70% GKC harvest
gkc_dir_70 <-log_summary %>%
  filter(prop_gkc >= 0.7,
         target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = ">=70%") 

#60% GKC harvest
gkc_dir_60 <-log_summary %>%
  filter(prop_gkc >= 0.6,
         target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = ">=60%") 

#Standard GKC harvst
gkc_dir_standard <-log_summary %>%
  filter(target_species_code == 923, 
         !is.na(target_species_retained),
         !is.na(number_pots_lifted), 
         !is.na(i_fishery)) %>%
  mutate(cpue = num_gkc / number_pots_lifted) %>% 
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se,
         prop_cpue = "No Change") 

bind_rows(gkc_dir_standard, gkc_dir_60)-> gkc_cpue_prop
  #bind_rows(gkc_dir_90, gkc_dir_80) %>%
 # bind_rows(gkc_dir_70, gkc_dir_60) 

write.csv(gkc_cpue_prop, paste0(output_path, '/gkc_logbook_cpue_proportions.csv'))

#Change order of proportions
gkc_cpue_prop$prop_cpue <- factor(gkc_cpue_prop$prop_cpue, 
                                  levels = c("No Change", ">=60%"))

gkc_cpue_prop %>%
  filter(mgt_area == "North Stephens Passage") %>%
  ggplot(aes(year, cpue, fill = prop_cpue)) + 
  geom_line() + 
  geom_point(aes(color = prop_cpue), size = 2) +
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.25) +
  labs(y="Mean CPUE of GKC (crab/pot)", 
       x ="Year",
       title = "North Stephens Passage",
       subtitle = "Removing Tanner bycatch based on proportion of harvest from logbooks") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  theme(legend.title = element_blank()) +
  facet_wrap(~prop_cpue) -> cpue_prop_plot


#Analyzing pot lifts based on proportion of GKC vs TC harvest
gkc_cpue_prop %>%
  filter(mgt_area == "North Stephens Passage") %>%
  ggplot(aes(year, total_pots, fill = prop_cpue)) +
  geom_col() +
  labs(y = "Total Pot Lifts (GKC logbook)",
       x = "Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 15000, 500)) +
  #scale_fill_viridis_d() +
  theme(legend.title = element_blank()) +
  facet_wrap(~prop_cpue) -> pots_prop_plot

cpue_prop_plot / pots_prop_plot

ggsave(paste0(fig_path, '/North_Stephens_Passage_gkc_cpue_proportion.png'), 
       width = 10, height = 9, units = "in", dpi = 200)


#GKC data from Tanner stock assessment survey ----
#GKC filtered for prior to export in OceanAK
read.csv("data/survey/tanner_survey_specimen.csv") %>% 
  clean_names () -> gkc_bio

gkc_bio %>%
  filter(location == "Holkham Bay") %>%
  mutate(recruit_status = ifelse(recruit_status == "Recruit", "Recruit",
                          ifelse(recruit_status == "PR1", "Post-Recruit",
                          ifelse(recruit_status == "PR2", "Post-Recruit",
                          ifelse(recruit_status == "PR3", "Post-Recruit",
                          ifelse(recruit_status == "PR4", "Post-Recruit",
                          ifelse(recruit_status == "PR5", "Post-Recruit",
                          ifelse(recruit_status == "PR6", "Post-Recruit",       
                          ifelse(recruit_status == "Pre_Recruit", "Pre-Recruit",
                          ifelse(sex == "Female", "Female",
                          ifelse(recruit_status == "Juvenile", "Juvenile", "Unknown"))))))))))) %>%
  group_by(i_year, location, recruit_status) %>%
  summarise(total_crab = sum(number_of_specimens)) %>%
  ggplot(aes(i_year, total_crab, fill = recruit_status)) +
  geom_col(position = "dodge") +
  labs(x = "Year",
       y = "Number of Crab",
       title = "Holkham Bay",
       subtitle = "Number of GKC caught during the Tanner survey",
       fill = "Recruit Status") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  #scale_fill_colorblind() +
  theme(legend.position = c(0.15, 0.8))

ggsave(paste0(fig_path, '/holkham_bay_gkc_recruit_status.png'), 
       width = 7, height = 6, units = "in", dpi = 200)


#soak time-----
gkc_log %>% filter(target_species_code == 923, !is.na(target_species_retained),
                   !is.na(number_pots_lifted), !is.na(i_fishery)) %>%
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) %>%
           group_by(year, mgt_area) -> soak_time


soak_time %>%
  filter(year >= 2020) %>%
ggplot(aes(mgt_area, soak_time_hours, color = mgt_area)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar') +
  geom_jitter() +
  scale_y_continuous(breaks = seq(0, 500, 25))

