## geocode matched individuals

matches <- readRDS("./temp/matched_ids_17.rds")
elects <- fread("./temp/elects.csv")

## NYS
db <- dbConnect(SQLite(), "D:/rolls.db")
nys <- dbGetQuery(db, "select res_house_number, res_pre_street, res_street_name, res_post_street_dir, res_city, zip5, nys_id, voter_status, history
                  from nys_roll_0319") %>%
  filter(nys_id %in% matches$nys_id) %>%
  mutate_at(vars(res_house_number, res_pre_street, res_street_name, res_post_street_dir), funs(ifelse(is.na(.), "", .))) %>% 
  mutate(street = paste(res_house_number, res_pre_street, res_street_name, res_post_street_dir),
         street = gsub("\\s+", " ", street),
         city = res_city,
         zip = zip5,
         state = "NY") %>%
  select(street, city, zip, state, nys_id, voter_status, history)

nys <- nys %>%
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

nys <- setorder(nys, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
nys <- nys[!duplicated(nys$nys_id),]
nys <- select(nys, -a, -b, -c)

nys <- geocode(nys) %>%
  filter(longitude != 0)
saveRDS(nys, "./temp/geocoded_17.rds")

nys <- readRDS("./temp/geocoded_17.rds")


nys <- cSplit(nys, "history", sep = ";", direction = "long", type.convert = F)

nys <- left_join(nys, elects, by = "history") %>% 
  group_by(nys_id) %>% 
  mutate(voted = max(year > 2006 & !is.na(year))) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

nys$voted <- ifelse(nys$voted == 1, "Cast Ballot in Past 10 Years", "Didn't Cast Ballot in Past 10 Years")

bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
pings  <- SpatialPoints(nys[c('longitude','latitude')], proj4string = bg_shp@proj4string)
nys$bg <- over(pings, bg_shp)$GEOID

count_by_bg <- nys %>% 
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
pings  <- SpatialPoints(nys[c('longitude','latitude')], proj4string = council_districts@proj4string)
nys$district   <- over(pings, council_districts)$CounDist

nys <- filter(nys, longitude != 0) %>% 
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
  geom_point(data = filter(nys, !is.na(district), voted == "Cast Ballot in Past 10 Years"), aes(x = longitude, y = latitude), shape = 21, color = "black", fill = "red") +
  coord_map() +
  labs(x = NULL, y = NULL, caption = "Sources: NYSBOE, NYSDOCCS")

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
