# a.olson and k.palof ADF&G juneau, ak 
# date modfied: 2019-10-4
# Objective:
# explore and develop management indicators for Southeast Alaska GKC 

# SEAK GKC Harvest and CPUE trends -----------

# load ---------
source("./r/helper.R")

# global ---------
cur_yr = 2020
year <- 2020 # most recent year of data
fig_path <- paste0('figures/', year) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', year) # output and results
dir.create(output_path) 


## data -------------------------
# Import fishticket and logbook data from ALEX
# Import fishticket and logbook data from ALEX
read.csv("data/fishery/gkc_fishticket.csv") %>% 
  clean_names () -> gkc_fish
read.csv("data/fishery/gkc_logbook.csv") %>%
  clean_names() -> gkc_log
read.csv("data/fishery/tanner_logbook.csv") %>%
  clean_names() -> tanner_log
# here or in readme need how to pull this data **FIX**


gkc_fish %>% 
  filter(!is.na(catch_date), !is.na(sell_date), !is.na(pounds)) %>% 
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) -> gkc_fish


# Annual harvest regionwide and by mgt area ####
#remove NAs and misc. i_fishery areas

gkc_fish %>%
  group_by(year, season_ref, mgt_area) %>%
  na_if("") %>%
  summarize(total_lbs = sum(pounds)) -> harvest

factor(harvest$season_ref, levels = c("68-69", "69-70", "70-71", "71-72", "72-73", "73-74", "74-75", "75-76",
                                         "76-77", "77-78", "78-79", "79-80", "80-81", "81-82", "82-83", "83-84", "84-85", "85-86", "86-87",
                                         "87-88", "88-89", "89-90", "90-91", "91-92", "92-93", "93-94", "94-95", "95-96", "96-97", "97-98",
                                         "98-99", "99-00", "00-01", "01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09",
                                         "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20")) -> harvest$season_ref

harvest %>%
  filter(season_ref != "19-20", 
         mgt_area != "Misc" & mgt_area != "") %>%
ggplot(aes(season_ref, total_lbs)) + geom_col(aes(fill = mgt_area), color = "white") +
  ylab("Harvest (lbs)") + xlab("Season") +
  #scale_x_continuous(breaks = seq(0, cur_yr+1, 5)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 2000000, 100000)) + 
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.75)) +
  scale_fill_viridis_d() +
  #scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #labs(caption = "*Closures: East Central in 2018 & Northern in 2019")


ggsave(paste0(fig_path, '/gkc_fishery_harvest_season.png'), width = 10, height = 8, units = "in", dpi = 400)

#non-confidential version of harvest data-----
gkc_fish %>%
  group_by(year, season_ref, mgt_area) %>%
  summarise(no_permits = n_distinct(cfec_no), 
            total_lbs = sum(pounds)) %>%
  filter(no_permits >= 3) %>%
  na_if("") -> harv_nonconf


factor(harv_nonconf$season_ref, levels = c("68-69", "69-70", "70-71", "71-72", "72-73", "73-74", "74-75", "75-76",
                                      "76-77", "77-78", "78-79", "79-80", "80-81", "81-82", "82-83", "83-84", "84-85", "85-86", "86-87",
                                      "87-88", "88-89", "89-90", "90-91", "91-92", "92-93", "93-94", "94-95", "95-96", "96-97", "97-98",
                                      "98-99", "99-00", "00-01", "01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09",
                                      "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20")) -> harv_nonconf$season_ref


harv_nonconf %>%
  filter(season_ref != "19-20", 
         mgt_area != "Misc" & mgt_area != "") %>%
  ggplot(aes(season_ref, total_lbs)) + geom_col(aes(fill = mgt_area)) +
  ylab("Harvest (lbs)") + xlab("Season") +
  #scale_x_continuous(breaks = seq(0, cur_yr+1, 5)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 2000000, 100000)) + 
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.75)) +
  scale_fill_viridis_d() +
  #scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.8, 0.85),
        axis.title = element_text(face = "bold"))
#labs(caption = "*Closures: East Central in 2018 & Northern in 2019")

ggsave(paste0(fig_path, '/gkc_fishery_harvest_nonconf_season.png'), 
       width = 10, height = 8, units = "in", dpi = 600)

ggplot(harvest, aes(year, total_lbs)) + geom_bar(stat = "identity") +
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 10)) +
  scale_y_continuous(label = scales::comma) + facet_wrap(~i_fishery, scales = "free_y")


#mgt_area GHLs compared to harvest
ec_ghl <- data.frame(year = c(2010:2019), 
                     ghl = c(260000, 260000, 260000, 285000, 200000, 115000, 
                             30000, 15000, 0, 15000))

gkc_fish %>% 
  filter(year >= 2010, i_fishery == "East Central GKC", !is.na(pounds)) %>%
  full_join(ec_ghl) %>%
  group_by(year) %>%
  summarise(total_harvest = sum(pounds),
            ghl = mean(ghl)) %>%
  drop_na() %>%
  ggplot(aes(year, total_harvest)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = ghl), linetype = "dashed", size = 2) +
  ylab("Harvest (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(0, 2020, 2)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 310000, 5000)) +
  ggtitle("East Central")

ggsave(paste0(fig_path, '/ghl vs harvest_ec.png'), width = 10, height = 8, units = "in", dpi = 200)

n_ghl <- data.frame(YEAR = c(2010:2019), 
                     ghl = c(145000, 145000, 145000, 105000, 105000, 65000, 15000,
                             10000, 7500, NA))

gkc_fish %>% 
  filter(year >= 2010, i_fishery == "Northern GKC", !is.na(pounds)) %>%
  full_join(n_ghl) %>%
  group_by(year) %>%
  summarise(total_harvest = sum(pounds),
            ghl = mean(ghl)) %>%
  drop_na() %>%
  ggplot(aes(year, total_harvest)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = ghl), size = 2) +
  ylab("Harvest (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(0, 2020, 2)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 310000, 5000)) +
  ggtitle("Northern")

ggsave(paste0(fig_path, '/ghl vs harvest_n.png'), width = 10, height = 8, units = "in", dpi = 200)

icy_ghl <- data.frame(YEAR = c(2010:2019), 
                    ghl = c(45000, 45000, 45000, 30000, 20000, 18000, 12000, 10000,
                            7500, 7500))

gkc_fish %>% 
  filter(year >= 2010, i_fishery == "Icy Strait GKC", !is.na(pounds)) %>%
  full_join(icy_ghl) %>%
  group_by(year) %>%
  summarise(total_harvest = sum(pounds),
            ghl = mean(ghl)) %>%
  drop_na() %>%
  ggplot(aes(year, total_harvest)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = ghl), size = 2) +
  ylab("Harvest (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(0, 2020, 2)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 310000, 5000)) +
  ggtitle("Icy Strait")

ggsave(paste0(fig_path, '/ghl vs harvest_icy.png'), width = 10, height = 8, units = "in", dpi = 200)

lc_ghl <- data.frame(YEAR = c(2010:2019), 
                      ghl = c(25000, 25000, 25000, 28000, 28000, 28000, 28000,
                              23000, 16000, 14000))

gkc_fish %>% 
  filter(year >= 2010, i_fishery == "Lower Chatham Strait GKC", !is.na(pounds)) %>%
  full_join(lc_ghl) %>%
  group_by(year) %>%
  summarise(total_harvest = sum(pounds),
            ghl = mean(ghl)) %>%
  drop_na() %>%
  ggplot(aes(year, total_harvest)) +
  geom_bar(stat = "identity") +
  geom_point(aes(y = ghl), size = 2) +
  ylab("Harvest (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(0, 2020, 2)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 310000, 5000)) +
  ggtitle("Lower Chatham Strait")

ggsave(paste0(fig_path, '/ghl vs harvest_lc.png'), width = 10, height = 8, units = "in", dpi = 200)


###non-confidential harvest, less than 3 permit holders
gkc_fish %>% 
  group_by(year, i_fishery) %>% 
  filter(!is.na(pounds)) %>%
  summarise(no_permits = n_distinct(cfec_no), total_lbs = sum(pounds)) %>%
  filter(no_permits >= 3) -> gkc_nonconf

ggplot(gkc_nonconf, aes(year, total_lbs)) +
  geom_col(aes(fill = i_fishery)) + 
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 5)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 2000000, 100000)) + 
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.75)) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(caption = "*Closures: East Central in 2018 & Northern in 2019")

###east central harvest
gkc_nonconf %>%
  filter(i_fishery == "East Central GKC") %>%
  ggplot(aes(year, total_lbs)) +
  geom_bar(stat = "identity") + 
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 5)) +
  scale_y_continuous(label = scales::comma, breaks = seq(0, 2000000, 25000)) + 
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.75)) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(caption = "*Closed in 2018") 

ggsave(paste0(fig_path, '/gkc_fishery_harvest_ec.png'), width = 8, height = 4, units = "in", dpi = 200)

gkc_nonconf %>%
  filter(i_fishery == "East Central GKC") %>%
  ggplot(aes(year, no_permits)) +
  geom_line(size = 0.5) +
  geom_point(size = 2.5) +
  ylab("Number of Permits") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 3), limits = c(0, 40)) +
  labs(caption = "*Closed in 2018")

ggsave(paste0(fig_path, '/permit_effort_ec.png'), width = 8, height = 4, units = "in", dpi = 200)

###Avg Ex-Vessel Value  -----------
head(gkc_fish)

gkc_fish %>% 
  group_by(YEAR) %>% 
  summarise(total_value = sum(value)) -> fish_value

ggplot(fish_value, aes(year, total_value)) + geom_line(lwd = 1) + 
  geom_point(size = 3, color = "dodgerblue") + ylab("Ex-vessel value") + xlab("Year")

gkc_fish %>% 
  group_by(year) %>% 
  filter(value > 0, !is.na(value), !is.na(pounds)) %>%
  mutate(price_per_lb = value / pounds) %>%
  summarise(mean = mean(price_per_lb), sd = sd(price_per_lb)) -> price_lb

ggplot(price_lb, aes(year, mean)) + geom_line(lwd = 1) + 
  geom_point(size = 3, color = "dodgerblue") +
  geom_ribbon(aes(YEAR, ymin = mean - sd, ymax = mean + sd),
              alpha = 0.3, fill = "gray") + ylab("Average $/lb") + xlab("Year") +
  scale_y_continuous(breaks = seq(-20, 20, 2))


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
  dplyr::select(year, mgt_area, cpue, number_pots_lifted) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(year, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(number_pots_lifted)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se) -> cpue_log

write.csv(cpue_log, paste0(output_path, '/gkc_logbook_cpue_summary.csv'))

ggplot(data = cpue_log, aes(year, cpue)) + 
  geom_line() + 
  geom_point(color = "dodgerblue1", size = 2) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 0.3, fill = "gray") +
  ylab("Mean CPUE (crab/pot)") + xlab("Year") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  facet_wrap(~mgt_area, scales = "free_y") 

#with rolling avg 3yr & 5 yr
ggplot(cpue_log, aes(year, cpue)) +
  geom_line() +
  geom_point(color = "dodgerblue1", size = 2) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 0.3, fill = "gray") +
  geom_ma(ma_fun = SMA, n = 3, color = "red") +
  geom_ma(ma_fun = SMA, n = 5, color = "black") +
  scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  facet_wrap(~mgt_area, scales = "free_y")

ggsave(paste0(fig_path, '/gkc_logbook_cpue.png'), width = 10, height = 8, units = "in", dpi = 200)

head(cpue_log)

#### Cumulative Avg Logbook CPUE ------
gkc_log %>% filter(target_species_code == 923, !is.na(target_species_retained),
                   !is.na(number_pots_lifted), !is.na(i_fishery)) %>%
  mutate(mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                           ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(i_fishery == "Northern GKC", "Northern",
                                                              ifelse(i_fishery == "Southern GKC", "Southern", "Misc"))))))),
         cpue = target_species_retained / number_pots_lifted,
         mdy = mdy(effort_date),
         doy = yday(mdy),
         cumcpue = cummean(cpue)) %>%
  select(year, mgt_area, doy, cpue, cumcpue) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  arrange(mgt_area, year, doy) -> daily_cpue

daily_cpue %>%
  filter(mgt_area == "East Central", year >= 2012) %>%
  ggplot(aes(doy, cumcpue)) +
  geom_line () +
  geom_point() 

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

bind_rows(gkc_dir_standard, gkc_dir_100) %>%
  bind_rows(gkc_dir_90, gkc_dir_80) %>%
  bind_rows(gkc_dir_70, gkc_dir_60) -> gkc_cpue_prop

write.csv(gkc_cpue_prop, paste0(output_path, '/gkc_logbook_cpue_proportions.csv'))

#Change order of proportions
gkc_cpue_prop$prop_cpue <- factor(gkc_cpue_prop$prop_cpue, 
                                  levels = c("No Change", ">=60%", ">=70%",
                                  ">=80%", ">=90%", "=100%"))

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

ggsave(paste0(fig_path, '/North_Stephens_Passage_gkc_cpue_proportion.png'), width = 10, height = 9, units = "in", dpi = 200)

# lbs per pot day -----------
# Use fishticket data #
head(gkc_fish)

# Select for mgt areas #
target <- c("East Central GKC", "Icy Strait GKC", "Lower Chatham Strait GKC", 
            "Mid-Chatham Strait GKC","North Stephens Passage GKC", "Northern GKC",
                 "Southern GKC")

#Fishing season based on first and last haul dates#
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
  mutate(CATCH_DATE = as.character(catch_date), mdy = mdy(catch_date)) %>% 
  select(YEAR, mgt_area, mdy) %>%
  summarise(min = min(mdy), max = max(mdy), diff = max-min) %>% 
  mutate(diff = as.numeric(diff, units = "days")) %>% 
  mutate(diff = replace(diff, which(diff == 0), 14)) -> season_leng 
# change diff that are 0 to 14 days - average "trip" based on tides. pers.comm A.Olson, K.Palof

#Convert difference in season days to a numeric value for further calculations
  
head(season_leng)

ggplot(season_leng, aes(YEAR, diff, color = mgt_area)) + 
  geom_line(lwd = 1) + 
  geom_point(size = 2) +
  facet_wrap(~ mgt_area) + 
  ylab("Season Length (number of days)") + xlab("Year") +
  theme(legend.position = "none")

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
  full_join(season_leng) %>% 
  select(year, mgt_area, diff, total_lbs, permits)  %>% 
  mutate(cpue = total_lbs / diff, 
         cpue2 = total_lbs / diff / permits) -> lbs_per_day

head(lbs_per_day)

# All mgt areas -----------------
ggplot(lbs_per_day, aes(YEAR, cpue, color = mgt_area)) + 
  geom_line(lwd = 1) + 
  geom_point(size = 2) +
  facet_wrap(~ mgt_area, scales = "free_y") + 
  ylab("CPUE (lbs/pot day)") + 
  xlab("Year") + 
  theme(legend.position = "none")

ggsave(paste0(fig_path, '/gkc_cpue_lbs_day.png'), width = 10, height = 8, units = "in", dpi = 200)


## panel figure for both - need both summary files here -------
panel_figure(1983, 2017, 2000, 2017, "East Central", lbs_per_day, cpue_log, 0.50, 0.20, cur_yr)
panel_figure(1983, 2010, 2000, 2017, "East Central", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2011, 2000, 2017, "Icy Strait", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)
panel_figure(1983, 2017, 2000, 2012, "Lower Chatham", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2007, 2000, 2017, "Mid-Chatham", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2017, "North Stephens Passage", lbs_per_day, cpue_log, 0.50, 0.20, cur_yr)
panel_figure(1983, 2017, 2001, 2017, "North Stephens Passage", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2017, "Northern", lbs_per_day, cpue_log, 0.75, 0.50, cur_yr)

panel_figure(1983, 2017, 2000, 2017, "Southern", lbs_per_day, cpue_log, 0.75, 0.50)


# lbs per pot day per permit -----------
# these use cpue 2, see seperate function
lbs_per_day_permit_graph(1983, 2017, "East Central GKC", lbs_per_day)
lbs_per_day_permit_graph(1983, 2017, "Icy Strait GKC", lbs_per_day)
lbs_per_day_permit_graph(1983, 2017, "Lower Chatham Strait GKC", lbs_per_day) 
lbs_per_day_permit_graph(1983, 2017, "Mid-Chatham Strait GKC", lbs_per_day)
lbs_per_day_permit_graph(1983, 2017, "North Stephens Passage GKC", lbs_per_day)
lbs_per_day_permit_graph(1983, 2017, "Northern GKC", lbs_per_day)
lbs_per_day_permit_graph(1983, 2017, "Southern GKC", lbs_per_day)

#logbook cpue
logbk_cpue(2000, 2017, "East Central", cpue_log, 0.75, 0.50, cur_yr)


#### old code ----------------------------
# lbs per day additional calcs ---
# these are curretnly in the function call 
lbs_per_day %>% 
  group_by(i_fishery) %>% 
  summarise(mean = mean(cpue, na.rm = TRUE)) -> hist_avg

lbs_per_day %>% 
  group_by(i_fishery) %>% 
  filter(year >= 1983 & 2017) %>%
  summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 

#East Central --- andrew's -----------
ec <- lbs_per_day %>% filter(i_fishery == "East Central GKC")
ggplot(ec, aes(year, cpue)) + geom_line(lwd = 1) + 
  geom_hline(yintercept = 4797, lwd = 0.5, color = "green") +
  geom_text(aes(1977, 4797, label = "Target Reference Point (avg 1983-2017)", vjust = -1)) +
  geom_hline(yintercept = 2399, lwd = 0.5, linetype = "dashed",color = "orange") +
  geom_text(aes(1977, 2399, label = "Trigger (50% of target)", vjust = -1)) +
  geom_hline(yintercept = 959, lwd = 0.5, color = "red") +
  geom_text(aes(1976.5, 959, label = "Limit Reference Point  (20% of target)", vjust = -1)) +
  geom_vline(xintercept = 1983, linetype = "dashed") +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  annotate("rect", xmin = 1983, xmax = 2017, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
  geom_point(size = 3, color = "dodgerblue") + 
  ylab("CPUE (lbs/pot day)") + xlab("Year") + ggtitle("East Central-active fishing season")

   