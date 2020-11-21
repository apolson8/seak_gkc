# R helper file for SEGKC 

# k.palof  katie.palof@alaska.gov  
# updated: 10-23-19

# load -----------
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)
library(RColorBrewer)
library(cowplot)
library(ggridges)
library(rgeos)
library(maptools)
library(rgdal)
library(ggmap)
library(ggrepel)
library(PBSmapping)
library(janitor)
library(here)
library(tidyquant)
library(DiagrammeR)
library(patchwork)
library(gganimate)
library(gifski)

##THEMES FOR GRAPHS ---------
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='serif')
          +theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text = element_text(size = 14)))

#COLOR BLIND PALETTE --------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#harvest by area --------------
hvst_area <- function(mg_area, harv_ghl, cur_yr) {
  
  harv_ghl %>%
  filter(mgt_area == mg_area) %>%
  ggplot(aes(season_num, total_lbs)) + 
    geom_col() +
    geom_point(aes(y = ghl),
               size = 2) + #need to draw GHL line across bar plot
    ylab("Harvest (lbs)") + xlab("Season") +
    scale_x_continuous(breaks = seq(0, cur_yr+1, 2)) +
    scale_y_continuous(label = scales::comma, breaks =scales::pretty_breaks(n = 10)) + 
    ggtitle(paste0(mg_area)) +
    theme(legend.title = element_blank(), 
          legend.position = c(0.75, 0.75),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5)) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_harvest.png'), fig1,  
         dpi = 600, width = 10, height = 5)

}

#harvest by area non-conf----
#excludes harvest by management with less than 3 permit holders
hvst_area_nonconf <-function(mg_area, harv_ghl_nonconf, cur_yr) {
  harv_ghl_nonconf %>% 
    filter(mgt_area == mg_area) %>%
    ggplot(aes(season_num, total_lbs)) + 
    geom_col() +
    geom_point(aes(y = ghl),
               size = 2) + #need to draw GHL line across bar plot
    ylab("Harvest (lbs)") + xlab("Season") +
    scale_x_continuous(breaks = seq(0, cur_yr+1, 2)) +
    scale_y_continuous(label = scales::comma, breaks =scales::pretty_breaks(n = 10)) + 
    ggtitle(paste0(mg_area)) +
    theme(legend.title = element_blank(), 
          legend.position = c(0.75, 0.75),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5)) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_harvest_nonconf.png'), fig1,  
         dpi = 600, width = 10, height = 5.5)
  
}


# target ref lbs per fishing day -----------

#str_yr = 1983
#end_yr = 2017
#mg_area = "Lower Chatham Strait GKC"

lbs_per_day_graph <- function(str_yr, end_yr, str_yr2, end_yr2, mg_area, lbs_per_day, cur_yr){

lbs_per_day %>% 
  group_by(mgt_area) %>% 
  filter(season_num >= str_yr & season_num <= end_yr) %>%
  summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 

avg_ten %>% 
  filter(mgt_area == mg_area) %>% 
  mutate(fifty = mean*0.75, twenty = mean*0.50) -> avg_ten2

lbs_per_day %>%
  group_by(mgt_area) %>%
  filter(season_num >= str_yr2 & season_num <= end_yr2) %>%
  summarise(mean2 = mean(cpue, na.rm = TRUE)) -> avg_lgterm

avg_lgterm %>%
  filter(mgt_area == mg_area) %>%
  mutate(fifty = mean2*0.75, twenty = mean2*0.50) -> avg_lgterm2

lbs_per_day %>% 
  filter(mgt_area == mg_area) %>% 
  ggplot(aes(season_num, cpue)) + 
  geom_line(lwd = 1) + 
  #geom_ma(ma_fun = SMA, n = 3) + #adds 3 yr simple moving average
  geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "blue", linetype = "dashed") +
  #geom_text(aes(1970, avg_ten2$mean, 
                #label = paste0("lbs/pot day (avg ", str_yr, "-", end_yr, ") industry request"), vjust = -1, hjust = 0.05)) +
  geom_hline(yintercept = avg_lgterm2$mean2, lwd = 0.5, color = "green") +
  #geom_text(aes(1970, avg_lgterm2$mean2,
                #label = paste0("lbs/pot day (avg ", str_yr2, "-", end_yr2, ")"), vjust = -1, hjust = 0.05)) +
  
  #geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
  #geom_text(aes(1977, avg_ten2$fifty, label = "Trigger (75% of target)", vjust = -1, hjust = 0.05)) +
  #geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
  #geom_text(aes(1976.5, avg_ten2$twenty, label = "Limit Reference Point  (50% of target)", vjust = -1, hjust = 0.05)) +
  #geom_vline(xintercept = str_yr, linetype = "dashed") +
  #geom_vline(xintercept = end_yr, linetype = "dashed") +
  #annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
  geom_point(size = 3, color = "dodgerblue") + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1965, 2022, 2)) +
  ylab("CPUE (lbs/pot day)") + 
  xlab("Season") + 
  ggtitle(paste0(mg_area, " -active fishing season")) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))-> fig1
fig1
ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_lbs_activeF_industry_request.png'), fig1,  
       dpi = 600, width = 8, height = 5.5)
}

# nonconf target ref lbs per fishing day -----------
lbs_per_day_nonconf <- function(str_yr, end_yr, str_yr2, end_yr2, mg_area, lbs_per_day, cur_yr){
  
  lbs_per_day %>% 
    group_by(mgt_area) %>% 
    filter(season_num >= str_yr & season_num <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(fifty = mean*0.75, twenty = mean*0.50) -> avg_ten2
  
  lbs_per_day %>%
    group_by(mgt_area) %>%
    filter(season_num >= str_yr2 & season_num <= end_yr2) %>%
    summarise(mean2 = mean(cpue, na.rm = TRUE)) -> avg_lgterm
  
  avg_lgterm %>%
    filter(mgt_area == mg_area) %>%
    mutate(fifty = mean2*0.75, twenty = mean2*0.50) -> avg_lgterm2
  
  lbs_per_day %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(cpue = ifelse(permits >= 3, cpue, NA)) %>%
    ggplot(aes(season_num, cpue)) + 
    geom_line(lwd = 1) + 
    #geom_ma(ma_fun = SMA, n = 3) + #adds 3 yr simple moving average
    #geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "blue", linetype = "dashed") +
    #geom_text(aes(1970, avg_ten2$mean, 
    #label = paste0("lbs/pot day (avg ", str_yr, "-", end_yr, ") industry request"), vjust = -1, hjust = 0.05)) +
    #geom_hline(yintercept = avg_lgterm2$mean2, lwd = 0.5, color = "green") +
    #geom_text(aes(1970, avg_lgterm2$mean2,
    #label = paste0("lbs/pot day (avg ", str_yr2, "-", end_yr2, ")"), vjust = -1, hjust = 0.05)) +
    
    #geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
    #geom_text(aes(1977, avg_ten2$fifty, label = "Trigger (75% of target)", vjust = -1, hjust = 0.05)) +
    #geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
    #geom_text(aes(1976.5, avg_ten2$twenty, label = "Limit Reference Point  (50% of target)", vjust = -1, hjust = 0.05)) +
    #geom_vline(xintercept = str_yr, linetype = "dashed") +
    #geom_vline(xintercept = end_yr, linetype = "dashed") +
    #annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
    geom_point(size = 3, color = "dodgerblue") + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1965, 2022, 2)) +
    ylab("CPUE (lbs/pot day)") + 
    xlab("Season") + 
    ggtitle(paste0(mg_area, " -active fishing season")) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5))-> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_lbs_activeF_nonconf.png'), fig1,  
         dpi = 600, width = 8, height = 5.5)
}

lbs_per_day_permit_graph <- function(str_yr, end_yr, mg_area, lbs_per_day, cur_yr){
  
  lbs_per_day %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue2, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(fifty = mean*0.50, twenty = mean*0.20) -> avg_ten2
  
  lbs_per_day %>% 
    filter(mgt_area == mg_area) %>% 
    ggplot(aes(year, cpue2)) + 
    geom_line(lwd = 1) + 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
    geom_text(aes(1977, avg_ten2$mean, 
                  label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
    geom_text(aes(1977, avg_ten2$fifty, label = "Trigger (50% of target)", vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
    geom_text(aes(1976.5, avg_ten2$twenty, label = "Limit Reference Point  (20% of target)", vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
    geom_point(size = 3, color = "dodgerblue") + 
    ylab("CPUE (lbs/pot day/permit)") + 
    xlab("Year") + 
    ggtitle(paste0(mg_area, " -active fishing season")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' _lb_activeF_by_permit.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}

### logbook data -------------
logbk_cpue <- function(str_yr, end_yr, mg_area, log_cpue, Lper1, Lper2, cur_yr){
  
  log_cpue %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(seventy_five = mean*Lper1, fifty = mean*Lper2) -> avg_ten2
  
  
  log_cpue %>% 
    filter(mgt_area == mg_area) %>% 
    ggplot(aes(year, cpue)) + 
    geom_line(lwd = 1) + 
    geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3, fill = "dodgerblue") +
    #geom_ma(ma_fun = SMA, n = 3) + #adds 3yr simple moving average 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
    geom_text(aes((str_yr-10), avg_ten2$mean, 
                  label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$seventy_five, lwd = 0.5, linetype = "dashed",color = "orange") +
    geom_text(aes((str_yr-10), avg_ten2$seventy_five, 
                  label = paste0("Trigger (", Lper1*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, color = "red") +
    geom_text(aes((str_yr-10), avg_ten2$fifty, 
                  label = paste0("Limit Reference Point (", Lper2*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey70") +
    geom_point(size = 3, color = "dodgerblue") + 
    scale_y_continuous(breaks = seq(0.0, 15.0, 0.5)) +
    scale_x_continuous(breaks = seq(1983, 2020, 2)) +
    expand_limits(y = 0) +
    ylab("Logbook CPUE (no. of crab/pot)") + 
    xlab("Season") + 
    ggtitle(paste0(mg_area, " logbook data")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' logbook_cpue.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}

### logbook data -------------
#industry request for 75%, 60%, and 40%
logbk_cpue_indstry <- function(str_yr, end_yr, mg_area, log_cpue, Lper1, Lper2, Lper3, cur_yr){
  
  log_cpue %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(seventy_five = mean*Lper1, sixty = seventy_five*Lper2, forty = seventy_five*Lper3) -> avg_ten2
  
  
  log_cpue %>% 
    filter(mgt_area == mg_area) %>% 
    ggplot(aes(year, cpue)) + 
    geom_line(lwd = 1) + 
    geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3, fill = "dodgerblue") +
    #geom_ma(ma_fun = SMA, n = 3) + #adds 3yr simple moving average 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "blue") +
    geom_text(aes((str_yr-10), avg_ten2$mean, 
                  label = paste0("Logbook CPUE (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$seventy_five, lwd = 0.5, color = "green") +
    geom_text(aes((str_yr-10), avg_ten2$seventy_five, 
                  label = paste0("Target (", Lper1*100, "% of avg)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$sixty, lwd = 0.5, linetype = "dashed", color = "orange") +
    geom_text(aes((str_yr-10), avg_ten2$sixty, 
                  label = paste0("Trigger (", Lper2*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$forty, lwd = 0.5, color = "red") +
    geom_text(aes((str_yr-10), avg_ten2$forty, 
                  label = paste0("Limit Reference Point (", Lper3*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey70") +
    geom_point(size = 3, color = "dodgerblue") + 
    scale_y_continuous(breaks = seq(0.0, 15.0, 0.5)) +
    scale_x_continuous(breaks = seq(1983, 2020, 2)) +
    expand_limits(y = 0) +
    ylab("Logbook CPUE (no. of crab/pot)") + 
    xlab("Season") + 
    ggtitle(paste0(mg_area, " logbook data")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' logbook_cpue_60_40.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}




### logbook data non-confidential-------------
#less than 3 permit holders in a given management area
logbk_cpue_nonconf <- function(str_yr, end_yr, mg_area, log_cpue, Lper1, Lper2, cur_yr){
  
  log_cpue %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(seventy_five = mean*Lper1, fifty = mean*Lper2) -> avg_ten2
  
  log_cpue %>% 
    mutate(cpue = ifelse(permits >= 3, cpue, NA)) %>% #removes confidential CPUE if less than 3 permits holders and breaks line segments due to NA values
    filter(mgt_area == mg_area) %>%
    ggplot(aes(year, cpue)) + 
    geom_line(lwd = 1) + 
    geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3, fill = "dodgerblue") +
    #geom_ma(ma_fun = SMA, n = 3) + #adds 3yr simple moving average 
    #geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
    #geom_text(aes((str_yr-10), avg_ten2$mean, 
                  #label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    #geom_hline(yintercept = avg_ten2$seventy_five, lwd = 0.5, linetype = "dashed",color = "orange") +
    #geom_text(aes((str_yr-10), avg_ten2$seventy_five, 
                  #label = paste0("Trigger (", Lper1*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    #geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, color = "red") +
    #geom_text(aes((str_yr-10), avg_ten2$fifty, 
                  #label = paste0("Limit Reference Point (", Lper2*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    #geom_vline(xintercept = str_yr, linetype = "dashed") +
    #geom_vline(xintercept = end_yr, linetype = "dashed") +
    #annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey70") +
    geom_point(size = 3, color = "dodgerblue") +
    scale_y_continuous(breaks = seq(0.0, 15.0, 0.5)) +
    scale_x_continuous(breaks = seq(1983, 2020, 2)) +
    expand_limits(y = 0) +
    ylab("Logbook CPUE (no. of crab/pot)") + 
    xlab("Season") + 
    ggtitle(paste0(mg_area, " logbook data")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' logbook_cpue_nonconf.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}


### logbook data non-confidential-------------
#less than 3 permit holders in a given management area
#industry request for 75%, 60%, and 40%
logbk_cpue_nonconf_indstry <- function(str_yr, end_yr, mg_area, log_cpue, Lper1, Lper2, Lper3, cur_yr){
  
  log_cpue %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(seventy_five = mean*Lper1, sixty = seventy_five*Lper2, forty = seventy_five*Lper3) -> avg_ten2
  
  log_cpue %>% 
    mutate(cpue = ifelse(permits >= 3, cpue, NA)) %>% #removes confidential CPUE if less than 3 permits holders and breaks line segments due to NA values
    filter(mgt_area == mg_area) %>%
    ggplot(aes(year, cpue)) + 
    geom_line(lwd = 1) + 
    geom_ribbon(aes(ymin = ll, ymax = ul), alpha = 0.3, fill = "dodgerblue") +
    #geom_ma(ma_fun = SMA, n = 3) + #adds 3yr simple moving average 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "blue") +
    geom_text(aes((str_yr-10), avg_ten2$mean, 
                  label = paste0("Logbook CPUE (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$seventy_five, lwd = 0.5, color = "green") +
    geom_text(aes((str_yr-10), avg_ten2$seventy_five, 
                  label = paste0("Target (", Lper1*100, "% of avg)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$sixty, lwd = 0.5, linetype = "dashed", color = "orange") +
    geom_text(aes((str_yr-10), avg_ten2$sixty, 
                  label = paste0("Trigger (", Lper2*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$forty, lwd = 0.5, color = "red") +
    geom_text(aes((str_yr-10), avg_ten2$forty, 
                  label = paste0("Limit Reference Point (", Lper3*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "grey70") +
    geom_point(size = 3, color = "dodgerblue") +
    scale_y_continuous(breaks = seq(0.0, 15.0, 0.5)) + #need to add ylim for Southern (0,7)
    scale_x_continuous(breaks = seq(1983, 2020, 2)) +
    expand_limits(y = 0) +
    ylab("Logbook CPUE (no. of crab/pot)") + 
    xlab("Season") + 
    ggtitle(paste0(mg_area, " logbook data")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' logbook_cpue_nonconf_60_40.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}



# panel figure ---------------
# str_yr - start year for lbs per day mean time frame
# end_yr - end year for lbs per day mean time frame
# str_yr2 - start year for logbk cpue mean time frame
# end_yr2 - end year for logbk cpue mean time frame
# mg_area - for fish ticket data AND logbk data
# lbs_per_day - summarized harvest data by active fishing season
# log_cpue - summarized logbook cpue
# Lper1 - for logbook cpue - percent for trigger reference pt
# Lper2 - for logbook cpue - percent for limit reference pt

panel_figure <- function(str_yr, end_yr, str_yr2, end_yr2, mg_area, lbs_per_day, log_cpue, Lper1, Lper2, cur_yr){
  
  lbs_per_day %>% 
    group_by(mgt_area) %>% 
    filter(year >= str_yr & year <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(fifty = mean*0.50, twenty = mean*0.20) -> avg_ten2
  
  lbs_per_day %>% 
    filter(mgt_area == mg_area) %>% 
    ggplot(aes(year, cpue)) + 
    geom_line(lwd = 1) + 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
    geom_text(aes(1970, avg_ten2$mean, 
                  label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
    geom_text(aes(1970, avg_ten2$fifty, label = "Trigger (50% of target)", vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
    geom_text(aes(1970.5, avg_ten2$twenty, label = "Limit Reference Point  (20% of target)", vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
    geom_point(size = 3, color = "dodgerblue") + 
    scale_x_continuous(breaks = seq(min(1970),max(2020), by =5), limits = c(1970, 2020)) +
    ylab("CPUE (lbs/pot day)") + 
    xlab("Year") + 
    ggtitle(paste0(mg_area, " -active fishing season")) -> fig1
  fig1
  
  log_cpue %>% 
      group_by(mgt_area) %>% 
      filter(year >= str_yr2 & year <= end_yr2) %>%
      summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_tenL 
    
    avg_tenL %>% 
      filter(mgt_area == mg_area) %>% 
      mutate(fifty = mean*Lper1, twenty = mean*Lper2) -> avg_ten2L
    
    log_cpue %>% 
      filter(mgt_area == mg_area) %>% 
      ggplot(aes(year, cpue)) + 
      geom_line(lwd = 1) + 
      geom_hline(yintercept = avg_ten2L$mean, lwd = 0.5, color = "green") +
      geom_text(aes((str_yr-10), avg_ten2L$mean, 
                    label = paste0("Target Reference Point (avg ", str_yr2, "-", end_yr2, ")"), vjust = -1, hjust = 0.05)) +
      geom_hline(yintercept = avg_ten2L$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
      geom_text(aes((str_yr-10), avg_ten2L$fifty, label = paste0("Trigger (", Lper1*100, "% of target)"), vjust = -1, hjust = 0.05)) +
      geom_hline(yintercept = avg_ten2L$twenty, lwd = 0.5, color = "red") +
      geom_text(aes((str_yr-10), avg_ten2L$twenty, label = paste0("Limit Reference Point  (", Lper2*100,"% of target)"), vjust = -1, hjust = 0.05)) +
      geom_vline(xintercept = str_yr2, linetype = "dashed") +
      geom_vline(xintercept = end_yr2, linetype = "dashed") +
      annotate("rect", xmin = str_yr2, xmax = end_yr2, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
      geom_point(size = 3, color = "dodgerblue") + 
      scale_x_continuous(breaks = seq(min(1970),max(2020), by =5), limits = c(1970, 2020)) +
      expand_limits(y = 0) +
      ylab("Logbook CPUE (lbs/pot)") + 
      xlab("Year") + 
      #ylim(0, range) +
      ggtitle(paste0(mg_area, " logbook data")) -> fig2
    fig2
    panel <- plot_grid(fig1, fig2, ncol = 1, align = 'v')
    ggsave(paste0('./figures/', cur_yr, '/', mg_area, '_panel_activeF_and_logbk.png'), panel,  
           dpi = 550, width = 8, height = 9.5)
    
  }

