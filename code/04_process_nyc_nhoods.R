## spatial join nyc
# 
# nyc <- dbGetQuery(db, "select dob, nys_id, voter_status, history, county_code, gender, political_party, last_name, zip5
#                        from nys_roll_0418 where county_code in (24, 41, 31, 3, 43)")
# 
# nyc <- nyc %>%
#   mutate(a = as.integer(voter_status == "ACTIVE"),
#          b = as.integer(voter_status == "INACTIVE"),
#          c = as.integer(voter_status == "PREREG"))
# 
# nyc <- setorder(nyc, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
# nyc <- nyc[!duplicated(nyc$nys_id),]
# nyc <- select(nyc, -a, -b, -c)
# 
# nyc <- left_join(nyc, readRDS("./temp/nys_0418_geocoded.rds"), by = "nys_id")
# saveRDS(nyc, "./temp/nyc.rds")

nyc <- readRDS("./temp/nyc.rds")

nyc$lost_voter <- nyc$nys_id %in% readRDS("./temp/ids_of_lost_voters.rds")$nys_id

lost_nyc <- sum(nyc$lost_voter)
saveRDS(lost_nyc, "./temp/lost_count_nyc.rds")
##### 2017 ballots bg
# read election names
elects <- fread("./raw_data/misc/elects.csv")

history <- cSplit(dplyr::select(nyc, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history")
history <- filter(history, year == 2017, election_type == "general")
nyc$v2017 <- nyc$nys_id %in% history$nys_id
##### arrests
arrests <- fread("./raw_data/NYPD_Arrests_Data__Historic_.csv") %>% 
  filter(LAW_CAT_CD == "F")

bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
pings  <- SpatialPoints(arrests[c('Longitude','Latitude')], proj4string = bg_shp@proj4string)
arrests$bg <- over(pings, bg_shp)$GEOID

arrests_bg <- arrests %>% 
  group_by(GEOID = bg) %>% 
  summarize(arrests = n())

arrests_tract <- arrests %>% 
  group_by(GEOID = substring(bg, 1, 11)) %>% 
  summarize(arrests = n())

## share dem
share_dem_bg <- nyc %>% 
  group_by(GEOID = bg) %>% 
  summarize(share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            lost_voters = sum(lost_voter))

share_dem_tract <- nyc %>% 
  group_by(GEOID = substring(bg, 1, 11)) %>% 
  summarize(share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            lost_voters = sum(lost_voter))

share_dem_zip <- nyc %>% 
  group_by(GEOID = zip5) %>% 
  summarize(share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            lost_voters = sum(lost_voter))

#block group / tract level
# geos <- lapply(c("tract", "block group", "zcta"), function(var) {
#   units <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c, run = run){
#     if(var != "zcta" | c == "KINGS"){
#       if(var == "zcta"){
#         c = NULL
#         s = NULL
#       }else{
#         s = "NY"
#       }
#       income <- census_income(var, state = s, year = 2017, county = c) %>%
#         dplyr::select(-NAME)
#   
#       race <- census_race_ethnicity(var, state = s, year = 2017, county = c) %>%
#         dplyr::select(-NAME)
#   
#       education <- census_education(var, state = s, year = 2017, county = c) %>%
#         dplyr::select(-NAME)
#   
#       age <- census_median_age(var, state = s, year = 2017, county = c)
#   
#       vap <- census_vap(var, state = s, year = 2017, county = c)
#       
#       noncit <- census_non_citizen(var, state = s, year = 2017, county = c)
#   
#       units <- left_join(income,
#                          left_join(race, left_join(education,
#                                                    left_join(age,
#                                                              left_join(vap, noncit)))))
#       }else{
#         units = data.frame("x" = NULL)
#       }
#     return(units)
#   }))
#   return(units)
#   })
# 
# #### noncit not available at block group level
# noncit <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
#   noncit <- census_non_citizen("tract", state = "NY", year = 2017, county = c)
# }))
# 
# geos[[2]]$tract <- substring(geos[[2]]$GEOID, 1, 11)
# geos[[2]] <- dplyr::select(geos[[2]], -share_non_citizen)
# geos[[2]] <- left_join(geos[[2]], noncit, by = c("tract" = "GEOID"))
# 
# 
# save(geos, file = "./temp/census_data_nyc_bgs_tracts.RData")

load("./temp/census_data_nyc_bgs_tracts.RData")

tracts <- left_join(geos[[1]], left_join(share_dem_tract, arrests_tract)) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

block_groups <- left_join(geos[[2]], left_join(share_dem_bg, arrests_bg)) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

tracts <- tracts[complete.cases(tracts), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / vap)

saveRDS(tracts, "./temp/tract_pre_match.rds")

block_groups <- block_groups[complete.cases(block_groups), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / vap)


saveRDS(block_groups, "./temp/block_group_pre_match.rds")


zips <- inner_join(mutate(geos[[3]], GEOID = as.integer(GEOID)), share_dem_zip, by = "GEOID") %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters)) %>% 
  filter(v2017 > 100)

saveRDS(zips, "./temp/zips_pre_match.rds")

###### maps
tracts <- readRDS("./temp/tract_pre_match.rds")
block_groups <- readRDS("./temp/block_group_pre_match.rds")

tract_shp <- readOGR("./raw_data/shapefiles/nyct2010_19a", "nyct2010")
tract_shp <- spTransform(tract_shp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
tract_shp@data$id <- rownames(tract_shp@data)
temp <- fortify(tract_shp)
tract_shp <- inner_join(temp, tract_shp@data, by = "id")
rm(temp)

codes <- data.frame("BoroCode" = as.character(c(1:5)), "cc" = c("061", "005", "047", "081", "085"))

tract_shp <- left_join(tract_shp, codes, by = "BoroCode")

tract_shp$GEOID <- with(tract_shp, paste0("36", cc, CT2010))

tract_shp <- left_join(tract_shp, tracts, by = "GEOID")

tract_shp$dec <- with(tract_shp, lost_voters > 0 & nh_black > (0.45))

ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = tract_shp, aes(x = long, y = lat, group = group, fill = dec), color = "black") +
  coord_map() +
  labs(x = NULL, y = NULL) + scale_fill_manual(values = c("gray", "red"), na.translate = T, na.value = "gray") +
  guides(fill = F)

## bg

block_groups$decrease <- block_groups$lost_voters > 0
dec <- block_groups[block_groups$lost_voters > 0, "GEOID"]
bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
bg_shp <- spTransform(bg_shp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
bg_shp@data$id <- rownames(bg_shp@data)
temp <- fortify(bg_shp)
bg_shp <- inner_join(temp, bg_shp@data, by = "id")
rm(temp)

bg_shp$dec <- bg_shp$GEOID %in% dec

dep_to <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = tract_shp, aes(x = long, y = lat, group = group), fill = "gray", color = "black", size = 0.01) +
  geom_polygon(data = filter(bg_shp, GEOID %in% block_groups$GEOID), aes(x = long, y = lat, group = group, fill = dec), color = "black", size = 0.01) +
  coord_map() +
  labs(x = NULL, y = NULL) + scale_fill_manual(values = c("gray", "red")) +
  guides(fill = F)

saveRDS(dep_to, "./output/dep_to_map.RDS")
ggsave("./output/depressed_turnout.png", plot = dep_to)
