## spatial join nyc
# 
# nyc <- dbGetQuery(db, "select dob, nys_id, voter_status, history, county_code, gender,
#                        political_party, last_name, zip5, registration_date
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
# 
# council_districts <- readOGR("./raw_data/shapefiles/nycc_19a", "nycc")
# council_districts <- spTransform(council_districts, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
# pings  <- SpatialPoints(nyc[c('longitude','latitude')], proj4string = council_districts@proj4string)
# nyc$cc_district <- over(pings, council_districts)$CounDist
# saveRDS(nyc, "./temp/nyc.rds")

nyc <- readRDS("./temp/nyc.rds") %>% 
  filter(substring(bg, 1, 5) %in% c("36047", "36081", "36061",
                                    "36005", "36085"))

## find lost voters
nyc$lost_voter <- nyc$nys_id %in% readRDS("./temp/ids_of_lost_voters_16.rds")$nys_id

lost_nyc <- sum(nyc$lost_voter)

nyc <- filter(nyc, match %in% c("Zip8", "Zip9"))

lost_bg <- nyc %>% 
  group_by(GEOID = bg) %>% 
  summarize(lost_voters = sum(lost_voter))

lost_tract <- nyc %>% 
  group_by(GEOID = substring(bg, 1, 11)) %>% 
  summarize(lost_voters = sum(lost_voter))

##### 2016 ballots bg
# read election names
elects <- fread("./raw_data/misc/elects.csv")

history <- cSplit(dplyr::select(nyc, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history")
history <- filter(history, year == 2016, election_type == "general")
nyc$v2016 <- nyc$nys_id %in% history$nys_id


## share dem
nyc <- nyc %>% 
  filter(voter_status != "PURGED")

share_dem_bg <- nyc %>% 
  rename(GEOID = bg) %>% 
  group_by(GEOID, congressional_district) %>% 
  mutate(count_d = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_d = max(count_d),
         biggest_district = ifelse(count_d == biggest_d, congressional_district, 0)) %>% 
  summarize(district = max(biggest_district),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2016 = sum(v2016),
            vcount = n())

share_dem_tract <- nyc %>% 
  mutate(GEOID = substring(bg, 1, 11)) %>% 
  group_by(GEOID, congressional_district) %>% 
  mutate(count_d = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_d = max(count_d),
         biggest_district = ifelse(count_d == biggest_d, congressional_district, 0)) %>% 
  summarize(district = max(biggest_district),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2016 = sum(v2016),
            vcount = n())

# 
# #block group / tract level
# geos <- lapply(c("tract", "block group", "zcta"), function(var) {
#   units <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c, run = run){
#     if(var != "zcta" | c == "KINGS"){
#       if(var == "zcta"){
#         c = NULL
#         s = NULL
#       }else{
#         s = "NY"
#       }
#       income <- census_income(var, state = s, year = 2016, county = c) %>%
#         dplyr::select(-NAME)
# 
#       race <- census_race_ethnicity(var, state = s, year = 2016, county = c) %>%
#         dplyr::select(-NAME)
# 
#       education <- census_education(var, state = s, year = 2016, county = c) %>%
#         dplyr::select(-NAME)
# 
#       age <- census_median_age(var, state = s, year = 2016, county = c)
# 
#       if(var == "tract"){
#         cvap <- fread("./raw_data/CVAP_2013-2017_ACS_csv_files/Tract.csv") %>%
#           filter(lntitle == "Total") %>%
#           mutate(GEOID = substring(geoid, 8)) %>%
#           select(GEOID, cvap = CVAP_EST)
#       }
#       if(var == "block group"){
#         cvap <- fread("./raw_data/CVAP_2013-2017_ACS_csv_files/BlockGr.csv") %>%
#           filter(lntitle == "Total") %>%
#           mutate(GEOID = substring(geoid, 8)) %>%
#           select(GEOID, cvap = CVAP_EST)
#       }
#       if(var == "zcta"){
#         cvap <- census_vap(var, state = s, year = 2016, county = c)
#       }
# 
#       noncit <- census_non_citizen(var, state = s, year = 2016, county = c)
# 
#       units <- left_join(income,
#                          left_join(race, left_join(education,
#                                                    left_join(age,
#                                                              left_join(cvap, noncit)))))
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
#   noncit <- census_non_citizen("tract", state = "NY", year = 2016, county = c)
# }))
# 
# geos[[2]]$tract <- substring(geos[[2]]$GEOID, 1, 11)
# geos[[2]] <- dplyr::select(geos[[2]], -share_non_citizen)
# geos[[2]] <- left_join(geos[[2]], noncit, by = c("tract" = "GEOID"))
# 
# 
# save(geos, file = "./temp/census_data_nyc_bgs_tracts_16.RData")

load("./temp/census_data_nyc_bgs_tracts_16.RData")

tracts <- left_join(geos[[1]], left_join(share_dem_tract, lost_tract)) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

tracts <- tracts[complete.cases(tracts), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2016 / cvap,
         reg_rate = vcount / cvap)

saveRDS(tracts, "./temp/tract_pre_match_16.rds")

block_groups <- left_join(geos[[2]], left_join(share_dem_bg, lost_bg)) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

sf <- readRDS("./temp/bg_sf_16.rds")

block_groups <- left_join(block_groups, sf, by = c("GEOID" = "bg")) %>% 
  mutate(sf = ifelse(is.na(sf), 0, sf / (population / 1000)))

block_groups <- block_groups[complete.cases(block_groups), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2016 / cvap,
         reg_rate = vcount / cvap)

saveRDS(block_groups, "./temp/block_group_pre_match_16.rds")

rm(reg_output)
