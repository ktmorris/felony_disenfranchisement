bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")



d <- fread("C:/Users/morrisk/Desktop/Export_Output_2.txt") %>% 
  filter(MONTH2 %in% c("January", "February", "March", "April", "May", "June",
                       "July", "August", "September", "October"))


pings  <- SpatialPoints(d[, c('longitude','latitude')], proj4string = bg_shp@proj4string)
d$bg <- over(pings, bg_shp)$GEOID


bg_sf <- d %>% 
  group_by(bg) %>% 
  tally() %>% 
  rename(sf = n) %>% 
  filter(!is.na(bg))


saveRDS(bg_sf, "./temp/bg_sf.rds")

########
d <- read.csv("C:/Users/morrisk/Desktop/Export_Output_3.txt") %>% 
  mutate(d = str_pad(datestop, width = 8, side = "left", pad = "0"),
         d = as.Date(d, "%m%d%Y")) %>% 
  filter(d < "2016-11-01")


pings  <- SpatialPoints(d[, c('longitude','latitude')], proj4string = bg_shp@proj4string)
d$bg <- over(pings, bg_shp)$GEOID


bg_sf <- d %>% 
  group_by(bg) %>% 
  tally() %>% 
  rename(sf = n) %>% 
  filter(!is.na(bg))


saveRDS(bg_sf, "./temp/bg_sf_16.rds")
