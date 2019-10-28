# a.olson and k.palof ADF&G juneau, ak 
# date modfied: 2019-10-4
# Objective:
# explore and develop management indicators for Southeast Alaska GKC 

# SEAK GKC Harvest and CPUE trends -----------

# load ---------
source("./r/helper.R")

# global ---------
cur_yr = 2019


## data -------------------------
# Import fishticket and logbook data from ALEX
gkc_fish <- read.csv("data/fishery/gkc_fishticket.csv")
gkc_log <- read.csv("data/fishery/gkc_logbook.csv")

# here or in readme need how to pull this data **FIX**

# Annual harvest regionwide and by mgt area ####

ggplot(gkc_fish, aes(YEAR, POUNDS)) + geom_bar(stat = "identity") +
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = pretty(gkc_fish$YEAR, n = 5), limits = c(1970, cur_yr+1)) +
  scale_y_continuous(label = scales::comma)


ggplot(gkc_fish, aes(YEAR, POUNDS)) + geom_bar(stat = "identity") +
  ylab("Harvest (lbs)") + xlab("Year") +
  scale_x_continuous(breaks = pretty(gkc_fish$YEAR, n = 5), limits = c(1970, cur_yr+1)) +
  scale_y_continuous(label = scales::comma) + facet_wrap(~I_FISHERY, scales = "free_y")

###Avg Ex-Vessel Value  -----------
head(gkc_fish)

fish_value <- gkc_fish %>% group_by(YEAR) %>% 
  summarise(total_value = sum(VALUE))

ggplot(fish_value, aes(YEAR, total_value)) + geom_line(lwd = 1) + 
  geom_point(size = 3, color = "dodgerblue") + ylab("Ex-vessel value") + xlab("Year")


price_lb <-gkc_fish %>% group_by(YEAR) %>% filter(VALUE > 0, !is.na(VALUE), !is.na(POUNDS)) %>%
  mutate(price_per_lb = VALUE / POUNDS) %>%
  summarise(mean = mean(price_per_lb), sd = sd(price_per_lb))

ggplot(price_lb, aes(YEAR, mean)) + geom_line(lwd = 1) + geom_point(size = 3, color = "dodgerblue") +
  geom_ribbon(aes(YEAR, ymin = mean - sd, ymax = mean + sd),
              alpha = 0.3, fill = "gray") + ylab("Average $/lb") + xlab("Year") 


# Logbook CPUE -----------
cpue_log <- gkc_log %>% filter(TARGET_SPECIES_CODE == 923, !is.na(TARGET_SPECIES_RETAINED),
                               !is.na(NUMBER_POTS_LIFTED), !is.na(I_FISHERY)) %>%
  mutate(mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                    ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                    ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                    ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                    ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                    ifelse(I_FISHERY == "Northern GKC", "Northern",
                    ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc"))))))),
         cpue = TARGET_SPECIES_RETAINED / NUMBER_POTS_LIFTED) %>% select(YEAR, mgt_area, cpue, NUMBER_POTS_LIFTED) %>%
  filter(!is.na(cpue), mgt_area != "Misc") %>% #have to add this here since 0 pots lifts for 0 crab is included here
  group_by(YEAR, mgt_area) %>%
  summarise(sd = sd(cpue),
            cpue = mean(cpue),
            n = n(),
            se = sd / sqrt (n),
            total_pots = sum(NUMBER_POTS_LIFTED)) %>%
  mutate(ll = cpue - 2 * se,
         ul = cpue + 2 * se)

ggplot(data = cpue_log, aes(YEAR, cpue)) + 
  geom_line() + 
  geom_point(color = "dodgerblue1", size = 2) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 0.3, fill = "gray") +
  ylab("Mean CPUE (crab/pot)") + xlab("Year") +
  facet_wrap(~mgt_area, scales = "free_y") 

# lbs per pot day -----------
# Use fishticket data #
head(gkc_fish)

# Select for mgt areas #
target <- c("East Central GKC", "Icy Strait GKC", "Lower Chatham Strait GKC", 
            "Mid-Chatham Strait GKC","North Stephens Passage GKC", "Northern GKC",
                 "Southern GKC")

#Fishing season based on first and last haul dates#
season_leng <- gkc_fish %>% filter(!is.na(CATCH_DATE), !is.na(SELL_DATE), I_FISHERY %in% target) %>% 
  group_by(YEAR, I_FISHERY) %>% 
  mutate(CATCH_DATE = as.character(CATCH_DATE), mdy = mdy(CATCH_DATE)) %>% 
  select(YEAR, I_FISHERY, mdy) %>%
  summarise(min = min(mdy), max = max(mdy), diff = max-min)
  
season_leng

#Convert difference in season days to a numeric value for further calculations
season_leng$diff <- as.numeric(season_leng$diff, units = "days")


ggplot(season_leng, aes(YEAR, diff, color = I_FISHERY)) + geom_line(lwd = 1) + geom_point(size = 2) +
  facet_wrap(~ I_FISHERY) + ylab("Season Length (number of days)") + xlab("Year") +
  theme(legend.position = "none")

#Harvest by mgt area and year#
harv <- gkc_fish %>% filter(!is.na(CATCH_DATE), !is.na(SELL_DATE), !is.na(POUNDS), I_FISHERY %in% target) %>% group_by(YEAR, I_FISHERY) %>%
  summarise(total_lbs = sum(POUNDS))

lbs_per_day <- bind_cols(season_leng, harv) %>% 
  select(YEAR, I_FISHERY, diff, total_lbs)  %>% group_by(YEAR, I_FISHERY, diff, total_lbs) %>%
  summarise(cpue = total_lbs / diff)

lbs_per_day  

View(lbs_per_day)

hist_avg <- lbs_per_day %>% group_by(I_FISHERY) %>% summarise(mean = mean(cpue))

avg_ten <- lbs_per_day %>% group_by(I_FISHERY) %>% filter(YEAR >= 1983 & 2017) %>%
  summarise(mean = mean(cpue))

avg_ten

#All mgt areas#
ggplot(lbs_per_day, aes(YEAR, cpue, color = I_FISHERY)) + geom_line(lwd = 1) + geom_point(size = 2) +
  facet_wrap(~ I_FISHERY, scales = "free_y") + ylab("CPUE (lbs/pot day)") + xlab("Year") + 
  theme(legend.position = "none")

ec <- lbs_per_day %>% filter(I_FISHERY == "East Central GKC")

#East Central#
ggplot(ec, aes(YEAR, cpue)) + geom_line(lwd = 1) + 
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

   