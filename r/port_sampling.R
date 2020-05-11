# A.Olson 
# Objective: Summarize and visualize port sampling data for GKC in SE Alaska 

source("./r/helper.R")

# global ---------
cur_yr = 2019
YEAR <- 2019 # most recent year of data
fig_path <- paste0('figures/', YEAR) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', YEAR) # output and results
dir.create(output_path) 

#Import port sampling data
gkc_port1 <- read.csv("data/fishery/gkc_port_sampling 1970-1999.csv")

gkc_port2 <- read.csv("data/fishery/gkc_port_sampling 2000-2019.csv")

#Join both sets of port sampling data
bind_rows(gkc_port1, gkc_port2) -> gkc_port

gkc_port %>% mutate(recruit_status = ifelse(RECRUIT_STATUS == "Recruit", "Recruit",
                                                     ifelse(RECRUIT_STATUS == "PR1", "Post-Recruit",
                                                     ifelse(RECRUIT_STATUS == "PR2", "Post-Recruit",
                                                     ifelse(RECRUIT_STATUS == "PR3", "Post-Recruit",
                                                     ifelse(RECRUIT_STATUS == "PR4", "Post-Recruit",
                                                     ifelse(RECRUIT_STATUS == "PR5", "Post-Recruit", "Misc")))))),
                                     mgt_area = ifelse(I_FISHERY == "East Central GKC", "East Central",
                                                ifelse(I_FISHERY == "Icy Strait GKC", "Icy Strait", 
                                                ifelse(I_FISHERY == "Lower Chatham Strait GKC", "Lower Chatham",
                                                ifelse(I_FISHERY == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(I_FISHERY == "North Stephens Passage GKC", "North Stephens Passage",
                                                ifelse(I_FISHERY == "Northern GKC", "Northern",
                                                ifelse(I_FISHERY == "Southern GKC", "Southern", "Misc")))))))) %>%
  filter(recruit_status != "Misc", mgt_area != "Misc") -> port_summary

### Sample size ---------
# adds sample size as a column by year and mgt_area
port_summary %>% 
  group_by(mgt_area, YEAR) %>% 
  summarise(count = n()) -> sample_size

port_summary %>% 
  left_join(sample_size)-> port_summary2

# Figures ------------
# Figure with sample size --------------
# deal with small sample size in 2014 for ICY and graphing issues. 
port_summary2 %>% 
  filter(mgt_area == "Icy Strait",
         YEAR == 2014) %>%
  filter(recruit_status == "Recruit") %>% 
  mutate(TICKET_NO = 12345) -> fill_icy


port_summary2 %>%
  bind_rows(fill_icy) %>% 
  filter(mgt_area == "Icy Strait",
         YEAR > 1999) %>%
  mutate(YEAR = fct_rev(as.factor(YEAR))) %>%
  ggplot(aes(y = YEAR)) + 
  geom_density_ridges(
    aes(x = LENGTH_MILLIMETERS, fill = paste(YEAR, recruit_status)), 
    alpha = 0.7, color = "white"
  ) + 
  scale_fill_cyclical(
    breaks = c("2000 Recruit", "2000 Post-Recruit"),
    labels = c(`2000 Recruit`= "Recruit", `2000 Post-Recruit` = "Post Recruit"),
    values = c("dark blue", "dark orange", "blue", "orange"),
    name = "Recruit_status", guide = "legend"
  ) + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(breaks = seq(0, 200, 10), limits = c(150, 200)) +
  geom_text(aes(x = 188, y = YEAR, label = count), position = position_nudge(y = 0.3)) + # comment this out to remove sample size
  ylab("Year") +
  xlab("Carapace Length (mm)") +
  #facet_wrap(~mgt_area, scales = "free_y") +
  theme(strip.background = element_blank(),
        legend.position = c(0.9, 0.9))

ggsave(paste0(fig_path, '/gkc_icystrait_lengths.png'), width = 7, height = 8, units = "in", dpi = 200)


# Histogram -----------

port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 1999) %>%
 ggplot(aes(LENGTH_MILLIMETERS)) + 
 geom_histogram() +
facet_wrap(~YEAR) +
  ggtitle("East Central GKC CL Frequencies")


port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 2004) %>%
  mutate(YEAR = fct_rev(as.factor(YEAR))) %>%
  ggplot(aes(LENGTH_MILLIMETERS, color = YEAR)) + 
  geom_freqpoly(size = 1.2) +
  ggtitle("East Central GKC CL Frequencies")


# Stacked Bar Chart -----------
port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 1999) %>%
  ggplot(aes(YEAR)) +
  geom_bar(aes(fill = recruit_status)) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))
  

port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 1999) %>% 
  group_by(YEAR) %>%
  count(recruit_status) %>%
  ggplot(aes(YEAR, n, color = recruit_status)) +
  geom_line() + 
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))



