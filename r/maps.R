#Maps#
# load ---------
source("./r/helper.R")


#import shellfish stat-areas

stat_area <- st_read("data/shape_files/cf_SE_stats_area.shp")

read.csv("data/fishery/gkc_logbook.csv") %>%
  clean_names() -> gkc_log

tm_shape(stat_area) +
  tm_fill() +
  tm_borders()

tmap_mode("view")
tm_shape(stat_area) +
  tm_polygons(col = "AREA")



gkc_log %>%
  dplyr:: select("district", "sub_district", "target_species_retained") %>%
  group_by("district") %>%
  summarise(total_crab = sum(target_species_retained))-> gkc_log1

gkc_log1$DISTRICT <- as.character(gkc_log1$DISTRICT)  
gkc_log1$SUB_DIST <- as.character(gkc_log1$SUB_DIST)  

gkc_map = left_join(stat_area, gkc_log1)

str(gkc_map)


gkc_map %>%
  dplyr::select(geometry, STAT_AREA, target_species_retained) %>%
  summarise(total_crab = sum(target_species_retained)) -> crab_sum_map

ggplot(gkc_map) +
  geom_sf(aes(fill = "target_species_retained"))

tm_shape(gkc_map) +
  tm_fill(col = "target_species_retained")


tm_shape(gkc_map) +
  tm_borders() +
  tm_scale_bar(breaks = c(0,100, 200), text.size = 1) +
  tm_compass(type = "8star", position = c("right", "bottom"))


