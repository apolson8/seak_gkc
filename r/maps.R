#Maps#
# load ---------
source("./r/helper.R")

# global ---------
cur_yr = 2019
YEAR <- 2019 # most recent year of data
fig_path <- paste0('figures/', YEAR) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', YEAR) # output and results
dir.create(output_path) 

#add southeast salmon/shellfish stat-area shapefiles
#map code from Ben Williams ben.williams@alaska.gov
shellfish_stat <-readOGR("data/shape_files", layer = "cf_SE_stats_area")
shellfish_stat@data$id = rownames(shellfish_stat@data)
shellfish_stat.points = fortify(shellfish_stat, region = "id")
shellfish_stat.df = left_join(shellfish_stat.points, shellfish_stat@data, by = "id")

data('nepacLLhigh') #load PBSmapping data set for N Pacific - much better resolution than worldHighres...
nepacLLhigh %>% 
  dplyr::select(group=PID, POS=POS,long=X,lat=Y) -> ak1


ggplot() +
  geom_polygon(data = ak1, aes(long, lat, group = group), fill = "lightgray") +
  coord_map(xlim = c(-170, -139), ylim = c(49, 62))+
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W'))) +
  geom_path(data = shellfish_stat.df, aes(long, lat, group = group))



shellfish_stat.df %>%
  ggplot() +
  geom_path(data = shellfish_stat.df, aes(long, lat, group = group)) 



shellfish_stat.df %>%
  mutate(mgt_area = ifelse(STAT_AREA == 11021, "EC", "Other")) -> mgt_area_map

  ggplot() +
  geom_polygon(data = mgt_area_map, aes(long, lat, group = group, fill = mgt_area)) +
  geom_path(data = shellfish_stat.df, aes(long, lat, group = group)) 

  
ec_loc <- c(-134.5, 56, -132.5, 57.5)
  
ggmap(get_stamenmap(bbox = ec_loc, maptype = "toner-lite", zoom = 9)) +
  geom_polygon(data = mgt_area_map, aes(long, lat, group = group, fill = mgt_area)) +
  geom_path(data = shellfish_stat.df, aes(long, lat, group = group), color = 'black', size = 0.5) 
  


shellfish_stat_df <-fortify(shellfish_stat)

summary(shellfish_stat@data)

se_map <-readOGR("data/shape_files/p4_se_alaska.shp")

se_map_df <-fortify(se_map)

map <- ggplot() +
  geom_polygon(data = shellfish_stat_df,
            aes(x = long, y = lat, group = group),
            color = 'black', fill = NA)

map + theme_void()




ggplot() + geom_polygon(data = shellfish_stat_df, aes(x = long, y = lat, group = group, fill = id))
