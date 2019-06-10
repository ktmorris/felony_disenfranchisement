### GEOCODE WHOLE STATE
### APRIL 2018 voter file to start


nys <- dbGetQuery(db, "select res_house_number, res_pre_street, res_street_name, res_post_street_dir, dob,
                       res_city, zip5, nys_id, voter_status
                       from nys_roll_0418") %>%
  mutate_at(vars(res_house_number, res_pre_street, res_street_name, res_post_street_dir), ~ ifelse(is.na(.), "", .)) %>%
  mutate(street = paste(res_house_number, res_pre_street, res_street_name, res_post_street_dir),
         street = gsub("\\s+", " ", trimws(street)),
         city = res_city,
         zip = zip5,
         state = "NY") %>%
  select(street, city, zip, state, nys_id, voter_status)

nys <- nys %>%
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

nys <- setorder(nys, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
nys <- nys[!duplicated(nys$nys_id),]
nys <- select(nys, -a, -b, -c)

nys <- geocode(nys)

bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
pings  <- SpatialPoints(nys[c('longitude','latitude')], proj4string = bg_shp@proj4string)
nys$bg <- over(pings, bg_shp)$GEOID

nys <- nys %>% 
  select(nys_id, longitude, latitude, bg)

saveRDS(nys, "./temp/nys_0418_geocoded.rds")