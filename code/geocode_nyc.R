## geocode nyc

# nyc <- dbGetQuery(db, "select dob, res_house_number, res_pre_street, res_street_name, res_post_street_dir, res_city,
#                        zip5, nys_id, voter_status, last_name, county_code, gender, political_party, history
#                   from nys_roll_0319
#                   where voter_status == 'ACTIVE' and
#                   county_code in (24, 41, 31, 3, 43)") %>%
#   mutate(age = floor(dob/10000),
#          gender = ifelse(gender == "F", 0, ifelse(gender == "M", 1, 2)),
#          party = ifelse(toupper(political_party) == "DEM", 0, ifelse(toupper(political_party) == "REP", 1, 2)))
# 
# nyc <- nyc %>%
#   mutate(a = as.integer(voter_status == "ACTIVE"),
#          b = as.integer(voter_status == "INACTIVE"),
#          c = as.integer(voter_status == "PREREG"))
# 
# nyc <- setorder(nyc, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
# nyc <- nyc[!duplicated(nyc$nys_id),]
# nyc <- dplyr::select(nyc, -a, -b, -c)
# 
# nyc <- nyc %>%
#   mutate_at(vars(res_house_number, res_pre_street, res_street_name, res_post_street_dir), funs(ifelse(is.na(.), "", .))) %>% 
#   mutate(street = paste(res_house_number, res_pre_street, res_street_name, res_post_street_dir),
#              street = gsub("\\s+", " ", street),
#              city = res_city,
#              zip = zip5,
#              state = "NY") %>%
#   select(nys_id, county_code, street, city, zip, state, party, gender, age, history)
# 
# 
# 
# nyc <- geocode(nyc)
# 
# bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
# pings  <- SpatialPoints(nyc[c('longitude','latitude')], proj4string = bg_shp@proj4string)
# nyc$bg <- over(pings, bg_shp)$GEOID
# 
# saveRDS(nyc, "./temp/nyc.rds")

nyc <- readRDS("./temp/nyc.rds")

##### 2017 ballots bg
# read election names
elects <- fread("./temp/elects.csv")

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

### lost voters 
lost_voters_bg <- readRDS("./temp/disen_by_bg.rds") %>% 
  rename(GEOID = bg)

lost_voters_tract <- lost_voters_bg %>% 
  group_by(GEOID = substring(GEOID, 1, 11)) %>% 
  summarize(lost_voters = sum(lost_voters))

## share dem
share_dem_bg <- nyc %>% 
  group_by(GEOID = bg) %>% 
  summarize(share_dem = mean(party == 0),
            v2017 = sum(v2017),
            vcount = n())

share_dem_tract <- nyc %>% 
  group_by(GEOID = substring(bg, 1, 11)) %>% 
  summarize(share_dem = mean(party == 0),
            v2017 = sum(v2017),
            vcount = n())

### block group / tract level
geos <- lapply(c("tract", "block group"), function(var) {
  income <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
    income <- census_income(var, state = "NY", year = 2017, county = c) %>% 
      dplyr::select(-NAME)
  }))
  
  race <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
    race <- census_race_ethnicity(var, state = "NY", year = 2017, county = c) %>% 
      dplyr::select(-NAME)
  }))
  
  education <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
    education <- census_education(var, state = "NY", year = 2017, county = c) %>% 
      dplyr::select(-NAME)
  }))
  
  age <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
    age <- census_median_age(var, state = "NY", year = 2017, county = c)
  }))
  
  vap <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
    vap <- census_vap(var, state = "NY", year = 2017, county = c) 
  }))
  
  
  units <- left_join(income,
                            left_join(race, left_join(education,
                                                      left_join(age,vap))))
  return(units)

  })

tracts <- left_join(geos[[1]], left_join(share_dem_tract, left_join(arrests_tract, lost_voters_tract))) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))
block_groups <- left_join(geos[[2]], left_join(share_dem_bg, left_join(arrests_bg, lost_voters_bg))) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

tracts <- tracts[complete.cases(tracts), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / vap)

saveRDS(tracts, "./temp/tract_pre_match.rds")

block_groups <- block_groups[complete.cases(block_groups), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / vap)


saveRDS(block_groups, "./temp/block_group_pre_match.rds")




#### regression
summary(lm(to ~ arrests + median_income + latino + nh_black + nh_white + some_college + median_age + vap + share_dem,
           data = block_groups))
