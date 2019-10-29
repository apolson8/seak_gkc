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

##THEMES FOR GRAPHS ---------
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=14,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE --------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# target ref lbs per fishing day -----------

str_yr = 1983
end_yr = 2017
mg_area = "East Central GKC"

lbs_per_day_graph <- function(str_yr, end_yr, mg_area){

avg_ten %>% 
  filter(I_FISHERY == mg_area) %>% 
  mutate(fifty = mean*0.50, twenty = mean*0.20) -> avg_ten2

lbs_per_day %>% 
  filter(I_FISHERY == mg_area) %>% 
  ggplot(aes(YEAR, cpue)) + 
  geom_line(lwd = 1) + 
  geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
  geom_text(aes(1977, avg_ten2$mean, 
                label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1)) +
  geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
  geom_text(aes(1977, avg_ten2$fifty, label = "Trigger (50% of target)", vjust = -1)) +
  geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
  geom_text(aes(1976.5, avg_ten2$twenty, label = "Limit Reference Point  (20% of target)", vjust = -1)) +
  geom_vline(xintercept = str_yr, linetype = "dashed") +
  geom_vline(xintercept = end_yr, linetype = "dashed") +
  annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
  geom_point(size = 3, color = "dodgerblue") + 
  ylab("CPUE (lbs/pot day)") + 
  xlab("Year") + 
  ggtitle(paste0(mg_area, " -active fishing season"))
}