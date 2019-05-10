## identify lost voters

matches <- readRDS("./temp/matched_ids_in_17.rds")
elects <- fread("./raw_data/misc/elects.csv")

### grab lats and longs

locs <- readRDS("./temp/nys_0418_geocoded.rds")

###

history <- left_join(dbGetQuery(db, "select nys_id, history from nys_roll_0418"),
                     readRDS("./temp/nys_0418_geocoded.rds"), by = "nys_id") %>%  
  filter(nys_id %in% matches$nys_id) # just keep formerly incarcerated, registered folks


history <- cSplit(history, "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history") %>% 
  group_by(nys_id) %>% 
  mutate(voted = max(year > 2006 & year <= 2016 & !is.na(year))) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

history$voted <- ifelse(history$voted == 1, "Cast Ballot in Past 10 Years", "Didn't Cast Ballot in Past 10 Years")

lost_ids <- history[history$voted == "Cast Ballot in Past 10 Years", c("nys_id")]
saveRDS(lost_ids, "./temp/ids_of_lost_voters.rds")

count_by_bg <- history %>% 
  filter(voted == "Cast Ballot in Past 10 Years") %>% 
  group_by(bg) %>% 
  tally() %>% 
  rename(lost_voters = n)

saveRDS(count_by_bg, "./temp/disen_by_bg.rds")

# get borough boundaries for projection
bb <- readOGR("./raw_data/shapefiles/Borough Boundaries", "geo_export_14dc9d2c-e65a-48c5-ac4d-1bcd90c6758d")
#

council_districts <- readOGR("./raw_data/shapefiles/nycc_19a", "nycc")
council_districts <- spTransform(council_districts, CRS(bb@proj4string@projargs))
pings  <- SpatialPoints(history[c('longitude','latitude')], proj4string = council_districts@proj4string)
history$district   <- over(pings, council_districts)$CounDist

history <- filter(history, longitude != 0) %>% 
  arrange(voted)

dists <- fortify(council_districts)

city_map <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = dists, aes(x = long, y = lat, group = group), fill = "#bfbfbf") +
  geom_path(data = dists, aes(x = long, y = lat, group = group), color = "black") +
  geom_point(data = filter(history, !is.na(district), voted == "Cast Ballot in Past 10 Years"), aes(x = longitude, y = latitude), shape = 21, color = "black", fill = "red") +
  coord_map() +
  labs(x = NULL, y = NULL)

saveRDS(city_map, "./output/city_map.RDS")

ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Gadugi"),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = dists, aes(x = long, y = lat, group = group), fill = "#bfbfbf") +
  geom_path(data = dists, aes(x = long, y = lat, group = group), color = "black") +
  geom_point(data = filter(nys, !is.na(district)), aes(x = longitude, y = latitude, color = as.factor(voted_16)), size = 2) +
  coord_map(xlim = c(-73.975, -73.9), ylim = c(40.75, 40.83)) +
  labs(x = NULL, y = NULL, color = "Voted in 2016", caption = "Sources: NYSBOE, NYSDOCCS") +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(title = "Voted in 2016?", title.position = "top", title.hjust = 0.5)) +
  ggtitle("2017 Prison Admissions Who Were\nRegistered to Vote in 2016")
ggsave("./output/harlem_bronx.png")

ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Gadugi"),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = dists, aes(x = long, y = lat, group = group), fill = "#bfbfbf") +
  geom_path(data = dists, aes(x = long, y = lat, group = group), color = "black") +
  geom_point(data = filter(nys, !is.na(district)), aes(x = longitude, y = latitude, color = as.factor(voted_16)), size = 2) +
  coord_map(xlim = c(-74.05, -73.93), ylim = c(40.679, 40.72)) +
  labs(x = NULL, y = NULL, color = "Voted in 2016", caption = "Sources: NYSBOE, NYSDOCCS") +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(title = "Voted in 2016?", title.position = "top", title.hjust = 0.5)) +
  ggtitle("2017 Prison Admissions Who Were\nRegistered to Vote in 2016")
ggsave("./output/brooklyn_heights.png")
