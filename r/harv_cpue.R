#SEAK GKC Harvest and CPUE trends#
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)


##THEMES FOR GRAPHS##
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Import fishticket and logbook data from ALEX
gkc_fish <- read.csv("data/fishery/gkc_fishticket.csv")

gkc_log <- read.csv("data/fishery/gkc_logbook.csv")


#Annual harvest regionwide and by mgt area

ggplot(gkc_fish, aes(YEAR, POUNDS)) + geom_bar(stat = "identity") +
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = pretty(gkc_fish$YEAR, n = 5), limits = c(1970, 2020)) +
  scale_y_continuous(label = scales::comma)


ggplot(gkc_fish, aes(YEAR, POUNDS)) + geom_bar(stat = "identity") +
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = pretty(gkc_fish$YEAR, n = 5), limits = c(1970, 2020)) +
  scale_y_continuous(label = scales::comma) + facet_wrap(~I_FISHERY)


#Logbook CPUE
cpue_log <- gkc_log %>% filter(TARGET_SPECIES_CODE == 923, !is.na(TARGET_SPECIES_RETAINED),
                               !is.na(NUMBER_POTS_LIFTED), !is.na(I_FISHERY)) %>%
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                    ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                    ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                    ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                    ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                    ifelse(I_FISHERY == "Northern GKC", "Northern",
                    ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc"))))))),
         cpue = TARGET_SPECIES_RETAINED / NUMBER_POTS_LIFTED) %>% select(YEAR, mgt_area, cpue) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(YEAR, mgt_area) %>%
  summarise(mean_cpue = mean(cpue), sd = sd(cpue))

ggplot(data = cpue_log) + 
  geom_line(aes(YEAR, mean_cpue)) + 
  geom_point(aes(YEAR, mean_cpue), color = "dodgerblue1") +
  geom_ribbon(aes(YEAR, ymin = mean_cpue - sd, ymax = mean_cpue + sd),
              alpha = 0.3, fill = "gray") +
  ylab("Mean CPUE (crab/pot)") + xlab("Year") +
  facet_wrap(~mgt_area) 


