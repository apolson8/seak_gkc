library(leaflet)
library(rgdal)
library(sp)
library(ggmap)
library(maptools)

#Import survey and fishery data
read.csv("data/fishery/gkc_logbook.csv") %>%
  clean_names() -> gkc_log

read.csv("data/survey/tanner_survey_specimen.csv") %>% #data pre-filtered for GKC in OceanAK
  clean_names() -> bio_survey

read.csv("data/survey/tanner_survey_pot.csv") %>%
  clean_names() -> pot_survey

read.csv("data/survey/tanner_survey_pot_and_specimen.csv") %>%
  clean_names() -> pot_bio_survey

#Summarize and join tables
bio_survey %>%
  group_by(year, location, species, pot_no) %>%
  summarise(total_crab = sum(number_of_specimens))-> bio_summary

#rename(bio_summary, year = i_year) -> bio_summary


left_join(pot_survey, bio_summary, by = c("year", "location", "pot_no")) %>%
  select(year, location, pot_no, latitude_decimal_degrees,
         longitude_decimal_degrees, species, total_crab) %>%
  filter(year > 2013) -> gkc_survey

#Import stat-area shapefiles
readOGR("data/shape_files/cf_SE_stats_area.shp") -> stat_area

fortify(stat_area) -> stat_area_df
 
#Using Tanner pot and specimen data combined from OceanAK
#Set location for Holkham Bay
hlk_bay <- c(-133.7750, 57.6739, -133.4, 57.8354)

pot_bio_survey %>%
  filter(year > 2013,
         location == "Holkham Bay",
         number_of_specimens != 0) -> pot_survey_summary

ggmap(get_stamenmap(bbox = hlk_bay,
                    maptype = "terrain",
                    color = "bw",
                    force = TRUE)) +
  geom_point(data = gkc_survey,
             aes(x = longitude_decimal_degrees,
                 y = latitude_decimal_degrees,
                 size = total_crab,
                 color = total_crab),
             alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 11), "no. of crab") +
  scale_color_continuous(type = "viridis", "no. of crab") +
  #scale_color_viridis_d("no. of crab") +
  ylab("Latitude (Decimal Degrees)") + 
  xlab("Longitude (Decimal Degrees)") + 
  labs(title ="Holkham Bay",
       subtitle = "Number of GKC caught during the Tanner survey") +
  facet_wrap(~year, ncol = 3) +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  
ggsave(paste0(fig_path, '/holkham_bay_gkc_survey_bycatch.png'), 
       width = 9, height = 12, units = "in", dpi = 200)  

  
  
  
  

 
