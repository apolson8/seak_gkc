# A.Olson 
# Objective: Summarize and visualize port sampling data for GKC in SE Alaska 

source("./r/helper.R")

# global ---------
cur_yr = 2021
YEAR <- 2021 # most recent year of data
fig_path <- paste0('figures/', YEAR) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', YEAR) # output and results
dir.create(output_path) 

#Import port sampling data
gkc_port1 <- read.csv("data/fishery/gkc_port_sampling 1970-1999.csv")

gkc_port2 <- read.csv("data/fishery/gkc_port_sampling 2000-2021.csv")

#Join both sets of port sampling data
bind_rows(gkc_port1, gkc_port2) %>%
  clean_names () -> gkc_port

#Create filter target for B. callosus
target <- c(1, 7)

gkc_port %>% mutate(recruit_status = ifelse(recruit_status == "Recruit", "Recruit",
                                                     ifelse(recruit_status == "PR1", "Post-Recruit",
                                                     ifelse(recruit_status == "PR2", "Post-Recruit",
                                                     ifelse(recruit_status == "PR3", "Post-Recruit",
                                                     ifelse(recruit_status == "PR4", "Post-Recruit",
                                                     ifelse(recruit_status == "PR5", "Post-Recruit",
                                                     ifelse(recruit_status == "Pre_Recruit", "Pre-Recruit",
                                                     ifelse(recruit_status == "Juvenile", "Juvenile",
                                                            "Misc")))))))),
                                     mgt_area = ifelse(i_fishery == "East Central GKC", "East Central",
                                                ifelse(i_fishery == "Icy Strait GKC", "Icy Strait", 
                                                ifelse(i_fishery == "Lower Chatham Strait GKC", "Lower Chatham",
                                                ifelse(i_fishery == "Mid-Chatham Strait GKC", "Mid-Chatham",
                                                ifelse(i_fishery == "North Stephens Passage GKC", "North Stephens Passage",
                                                ifelse(i_fishery == "Northern GKC", "Northern",
                                                ifelse(i_fishery == "Southern GKC", "Southern", "Misc")))))))) %>%
  filter(recruit_status != "Misc",
         parasite_code %in% target, #filter out B. callosus scar and externa
         mgt_area != "Misc") %>%
  mutate(year = i_year, season_num = as.numeric(str_sub(season, 4, 7)), #this changes the season ref to a numerica variable
         season_num = season_num + 1) -> port_summary

#Reorder factor levels by recruit status
port_summary$recruit_status <- factor(port_summary$recruit_status,
                                      levels = c("Juvenile",
                                                 "Pre-Recruit",
                                                 "Recruit",
                                                 "Post-Recruit"))

### Sample size ---------
# adds sample size as a column by year and mgt_area
port_summary %>% 
  group_by(mgt_area, season, season_num) %>% 
  summarise(count = n()) -> sample_size

port_summary %>% 
  left_join(sample_size)-> port_summary2

# Figures ------------
#Function for producting length frequency historgrams for set years and mgt area
lngth_freq <- function(str_yr, end_yr, mg_area, port_summary, cur_yr){
  
  #target <- c(2000:2021) #manually select years to make graphs non-conf from fishticket data
  
  port_summary %>%
    filter(mgt_area == mg_area,
           #year %in% target, 
           year >= str_yr & year <= end_yr) %>%
  ggplot(aes(length_millimeters, 
             fill = recruit_status, 
             color = recruit_status)) +
    geom_histogram(alpha = 0.3, 
                   bins = 30) +
    scale_x_continuous(breaks = seq(0, 250, by = 20),
                       name = "Carapace Length (mm)") +
    ylab("Count") +
    ggtitle(paste0(mg_area)) +
    facet_wrap(~season_num) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5)) ->fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_length_freq.png'), fig1,  
          dpi = 600, width = 10, height = 8)
}


lngth_freq(2000, 2021, "East Central", port_summary, cur_yr)
lngth_freq(2000, 2021, "Icy Strait", port_summary, cur_yr)
lngth_freq(2000, 2021, "Lower Chatham", port_summary, cur_yr)
lngth_freq(2000, 2021, "Mid-Chatham", port_summary, cur_yr)
lngth_freq(2000, 2021, "North Stephens Passage", port_summary, cur_yr)
lngth_freq(2000, 2021, "Northern", port_summary, cur_yr)
lngth_freq(2000, 2021, "Southern", port_summary, cur_yr)

#use gganimate to show change over year via histograms
port_summary %>%
  filter(mgt_area == "East Central",
         year > 1990) %>%
  ggplot(aes(length_millimeters, 
             fill = recruit_status, 
             color = recruit_status)) +
  geom_histogram(alpha = 0.3, bins = 30) +
  facet_wrap(~year) +
  ggtitle("East Central")
  transition_time(year) +
  #labs(title = "Year: {frame_time}",
       #x = "Carapace Length (mm)", y = "Count")
  
#animate(port_summary_anim,fps = 5,
        #renderer = gifski_renderer("gkc_recruit.gif"))





# deal with small sample size in 2014 for ICY and graphing issues. 
port_summary2 %>% 
  filter(mgt_area == "Icy Strait",
         year == 2014) %>%
  filter(recruit_status == "Recruit") %>% 
  mutate(TICKET_NO = 12345) -> fill_icy


port_summary2 %>%
  bind_rows(fill_icy) %>% 
  filter(mgt_area == "Icy Strait",
         YEAR > 1999) %>%
  mutate(YEAR = fct_rev(as.factor(year))) %>%
  ggplot(aes(y = year)) + 
  geom_density_ridges(
    aes(x = length_millimeters, fill = paste(year, recruit_status)), 
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
  geom_text(aes(x = 188, y = year, label = count), position = position_nudge(y = 0.3)) + # comment this out to remove sample size
  ylab("Year") +
  xlab("Carapace Length (mm)") +
  #facet_wrap(~mgt_area, scales = "free_y") +
  theme(strip.background = element_blank(),
        legend.position = c(0.9, 0.9))

ggsave(paste0(fig_path, '/gkc_icystrait_lengths.png'), width = 7, height = 8, units = "in", dpi = 200)


# Histogram -----------

port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>%
 ggplot(aes(length_millimeters)) + 
 geom_histogram() +
facet_wrap(~year) +
  ggtitle("East Central GKC CL Frequencies")


port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 2004) %>%
  mutate(YEAR = fct_rev(as.factor(year))) %>%
  ggplot(aes(length_millimeters, color = year)) + 
  geom_freqpoly(size = 1.2) +
  ggtitle("East Central GKC CL Frequencies")


# Stacked Bar Chart -----------
port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>%
  ggplot(aes(year)) +
  geom_bar(aes(fill = recruit_status)) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))
  

port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>% 
  group_by(year) %>%
  count(recruit_status) %>%
  ggplot(aes(year, n, color = recruit_status)) +
  geom_line() + 
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))



port_summary2 %>% select(year, length_millimeters) %>%
  group_by(length_millimeters) %>% tally()

