# k.palof
# katie.palof@alaska.gov

# Objective: Illustrate usefulness in collecting soak time information for GKC


source("./r/helper.R")

# global ---------
YEAR <- 2019 # most recent year of data
fig_path <- paste0('figures/', YEAR) # folder to hold all figs for a given year

## data -------------------------
# Import logbook data from ALEX
gkc_log <- read.csv("data/fishery/gkc_logbook.csv")



## east central by logbk entry -----------
gkc_log %>% 
  filter(TARGET_SPECIES_CODE == 923, !is.na(TARGET_SPECIES_RETAINED),
         !is.na(NUMBER_POTS_LIFTED), !is.na(I_FISHERY)) %>%
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                           ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                                  ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                                         ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                                                       ifelse(I_FISHERY == "Northern GKC", "Northern",
                                                              ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc")))))))) %>% 
  filter(mgt_area == "East Central") %>% 
  select(mgt_area, YEAR, CFEC_NO, TICKET_NO, EFFORT_NO, NUMBER_POTS_LIFTED, TARGET_SPECIES_RETAINED) -> gkc_log_ec_raw
  
# East central soak simulation -----------
gkc_log_ec_raw %>% 
  mutate(soak14 = 14, 
         soak7 = sample(2:14, 5986, replace = T), # randomly generated soak time 
         soak21 = sample(17:25, 5986, replace = T),
         soak2 = sample(1:3, 5986, replace = T),
         cpue = TARGET_SPECIES_RETAINED / NUMBER_POTS_LIFTED,
         cpue_14day = (TARGET_SPECIES_RETAINED/soak14)/NUMBER_POTS_LIFTED,  # assumed 14 days
         cpue_7day = (TARGET_SPECIES_RETAINED/soak7)/NUMBER_POTS_LIFTED, 
         cpue_21day = (TARGET_SPECIES_RETAINED/soak21)/NUMBER_POTS_LIFTED, 
         cpue_2day = (TARGET_SPECIES_RETAINED/soak2)/NUMBER_POTS_LIFTED) %>% 
  filter(!is.na(cpue)) %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(YEAR, mgt_area) %>%
  summarise(cpue = mean(cpue),
            cpue_14day = mean(cpue_14day), 
            cpue_7day = mean(cpue_7day),
            cpue_21day = mean(cpue_21day),
            cpue_2day = mean(cpue_2day),
            avg_soak = mean(soak7),
            avg_soak3 = mean(soak21),
            n = n(),
            total_pots = sum(NUMBER_POTS_LIFTED)) %>% 
  select(YEAR, mgt_area, avg_soak, cpue_14day, cpue_7day, cpue_21day, cpue_2day, cpue) %>% 
  gather("type", "value", 4:8) %>% 
  ggplot(aes(YEAR, value, group = type, color = type)) + 
  geom_line() + 
  geom_point(size = 2) +
  #geom_ribbon(aes(ymin = ll, ymax = ul),
  #            alpha = 0.3, fill = "gray") +
  ylab("Mean CPUE (crab/day/pot)") + xlab("Year") 
  #scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +
  
ggsave(paste0(fig_path, '/gkc_soak_time_sim_all.png'), width = 10, height = 6, units = "in", dpi = 200)

# East central soak simulation high soaks -----------
gkc_log_ec_raw %>% 
  mutate(soak14 = 14, 
         soak7 = sample(2:14, 5986, replace = T), # randomly generated soak time 
         soak21 = sample(17:25, 5986, replace = T),
         soak2 = sample(1:3, 5986, replace = T),
         cpue = TARGET_SPECIES_RETAINED / NUMBER_POTS_LIFTED,
         cpue_14day = (TARGET_SPECIES_RETAINED/soak14)/NUMBER_POTS_LIFTED,  # assumed 14 days
         cpue_7day = (TARGET_SPECIES_RETAINED/soak7)/NUMBER_POTS_LIFTED, 
         cpue_21day = (TARGET_SPECIES_RETAINED/soak21)/NUMBER_POTS_LIFTED, 
         cpue_2day = (TARGET_SPECIES_RETAINED/soak2)/NUMBER_POTS_LIFTED) %>% 
  filter(!is.na(cpue)) %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(YEAR, mgt_area) %>%
  summarise(cpue = mean(cpue),
            cpue_14day = mean(cpue_14day), 
            cpue_7day = mean(cpue_7day),
            cpue_21day = mean(cpue_21day),
            cpue_2day = mean(cpue_2day),
            avg_soak = mean(soak7),
            avg_soak3 = mean(soak21),
            n = n(),
            total_pots = sum(NUMBER_POTS_LIFTED)) %>% 
  select(YEAR, mgt_area, avg_soak, cpue_14day, cpue_7day, cpue_21day) %>% 
  gather("type", "value", 4:6) %>% 
  ggplot(aes(YEAR, value, group = type, color = type)) + 
  geom_line() + 
  geom_point(size = 2) +
  #geom_ribbon(aes(ymin = ll, ymax = ul),
  #            alpha = 0.3, fill = "gray") +
  ylab("Mean CPUE (crab/day/pot)") + xlab("Year") 
#scale_x_continuous(breaks = seq(0, cur_yr+1, 3)) +

  





  
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


## east central ----------
cpue_log %>% 
  filter(mgt_area == "East Central") -> cpue_log_ec
