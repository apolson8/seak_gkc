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

target <- c("Mid-Chatham", "Lower Chatham")

mc <- port_summary %>% filter(mgt_area == target)

#Figures
  
port_summary %>% 
  filter(mgt_area == "North Stephens Passage") %>%
  ggplot(aes(LENGTH_MILLIMETERS, YEAR, group = YEAR)) +
  geom_density_ridges(aes(point_fill = recruit_status, 
                          point_color = recruit_status,
                          point_shape = recruit_status),
                      jittered_points = TRUE, scale = 3.0, alpha= 0.3, 
                      point_alpha = 1, 
                      point_size = 0.9, lwd = 0.75) +
  ylab("Year") + xlab("Carapace Length (mm)") + scale_y_reverse() +
  facet_wrap(~mgt_area) +
  ggridges::scale_discrete_manual(aesthetics = "point_color", 
                                  values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ggridges::scale_discrete_manual(aesthetics = "point_fill", 
                                  values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ggridges::scale_discrete_manual(aesthetics = "point_shape", values = c(22, 24))

ggsave(paste0(fig_path, '/nsp_length_comps.png'), width = 10, height = 8, units = "in", dpi = 200)





