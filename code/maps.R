## geocode matched individuals

matches <- readRDS("./temp/matched_ids_17.rds")
e16 <- fread("D:/rolls/new_york/misc/2016_elections.csv")

# ## NYS
# db <- dbConnect(SQLite(), "D:/rolls.db")
# nys <- dbGetQuery(db, "select res_house_number, res_pre_street, res_street_name, res_post_street_dir, res_city, zip5, nys_id, voter_status, history
#                   from nys_roll_0319") %>% 
#   filter(nys_id %in% matches$nys_id) %>% 
#   mutate(street = paste("-", res_house_number, res_pre_street, res_street_name, res_post_street_dir, "-", sep = "-"),
#          street = gsub("\\s+", " ", str_trim(gsub("-", " ", gsub("-NA-", " ", street)))),
#          city = res_city,
#          zip = zip5,
#          state = "NY") %>% 
#   select(street, city, zip, state, nys_id, voter_status, history)
# 
# nys <- nys %>% 
#   mutate(a = as.integer(voter_status == "ACTIVE"),
#          b = as.integer(voter_status == "INACTIVE"),
#          c = as.integer(voter_status == "PREREG"))
# 
# nys <- setorder(nys, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
# nys <- nys[!duplicated(nys$nys_id),]
# nys <- select(nys, -a, -b, -c)
# 
# nys <- geocode(nys) %>% 
#   filter(longitude != 0)
# saveRDS(nys, "./temp/geocoded_17.rds")

nys <- readRDS("./temp/geocoded_17.rds")


nys <- cSplit(nys, "history", sep = ";", direction = "long", type.convert = F) %>% 
  group_by(nys_id) %>% 
  mutate(v16 = max(history %in% e16$election)) %>% 
  filter(row_number() == 1)

nys$voted_16 <- ifelse(nys$v16 == 1, "Cast Ballot in 2016", "Didn't Cast Ballot in 2016")


council_districts <- readOGR("./raw_data/shapefiles/nycc_19a", "nycc")
council_districts <- spTransform(council_districts, CRS("+proj=longlat +datum=WGS84"))
pings  <- SpatialPoints(nys[c('longitude','latitude')], proj4string = council_districts@proj4string)
nys$district   <- over(pings, council_districts)$CounDist

nys <- filter(nys, longitude != 0) %>% 
  arrange(v16)

dists <- fortify(council_districts)

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
  geom_point(data = filter(nys, !is.na(district)), aes(x = longitude, y = latitude, color = as.factor(voted_16))) +
  coord_equal(ratio = 1) +
  labs(x = NULL, y = NULL, color = "Voted in 2016", caption = "Sources: NYSBOE, NYSDOCCS") +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(title = "Voted in 2016?", title.position = "top", title.hjust = 0.5)) +
  ggtitle("2017 Prison Admissions Who Were\nRegistered to Vote in 2016")
ggsave("./output/citywide_map.png")

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
