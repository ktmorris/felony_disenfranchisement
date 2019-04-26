## geocode nyc

# nyc <- dbGetQuery(db, "select dob, res_house_number, res_pre_street, res_street_name, res_post_street_dir, res_city,
#                        zip5, nys_id, voter_status, last_name, county_code, gender, political_party
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
# nyc <- select(nyc, -a, -b, -c)
# 
# 
# nyc <- nyc %>%
#   mutate(street = paste("-", res_house_number, res_pre_street, res_street_name, res_post_street_dir, "-", sep = "-"),
#              street = gsub("\\s+", " ", str_trim(gsub("-", " ", gsub("-NA-", " ", street)))),
#              city = res_city,
#              zip = zip5,
#              state = "NY") %>%
#   select(nys_id, county_code, street, city, zip, state, party, gender, age)
# 
# nyc <- geocode(nyc)
# 
# bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
# pings  <- SpatialPoints(nyc[c('longitude','latitude')], proj4string = bg_shp@proj4string)
# nyc$bg <- over(pings, bg_shp)$GEOID
# 
# saveRDS(nyc, "./temp/nyc.rds")
# 

nyc <- readRDS("./temp/nyc.rds")

## pull in arrests data
arrests <- fread("./raw_data/NYPD_Arrests_Data__Historic_.csv") %>% 
  filter(LAW_CAT_CD == "F")
bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
pings  <- SpatialPoints(arrests[c('Longitude','Latitude')], proj4string = bg_shp@proj4string)
arrests$bg <- over(pings, bg_shp)$GEOID

arrests <- arrests %>% 
  group_by(bg) %>% 
  tally()

##
income <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  income <- census_income("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))

race <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  race <- census_race_ethnicity("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))

education <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  education <- census_education("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))


nyc <- left_join(nyc, arrests, by = "bg")
nyc <- left_join(nyc, income, by = c("bg" = "GEOID"))
nyc <- left_join(nyc, race, by = c("bg" = "GEOID"))
nyc <- left_join(nyc, education, by = c("bg" = "GEOID"))
nyc <- nyc %>% 
  mutate(arrests = ifelse(is.na(n), 0, n),
         arrests_pop = arrests / population) %>% 
  select(-n)

rm(bg_shp, arrests, education, income, pings, race)

nyc_s <- nyc[complete.cases(nyc), ]

bg_count <- length(unique(nyc_s$bg))

bgs <- nyc_s %>% 
  group_by(bg) %>% 
  filter(row_number() == 1) %>% 
  arrange(arrests_pop) %>% 
  ungroup() %>% 
  mutate(control = (row_number() / bg_count) < 0.5,
         treat = (row_number() / bg_count) > 0.9) %>% 
  filter(control == T | treat == T) %>% 
  select(bg, treat, control)

nyc_s <- filter(nyc_s, bg %in% bgs$bg)
nyc_s <- left_join(nyc_s, bgs, by = "bg") %>% 
  select(nys_id, party, gender, age, median_income, latino, nh_black, nh_white, some_college, treat, control, arrest_pop)

### block group level

share_dem <- nyc %>% 
  group_by(bg) %>% 
  summarize(share_dem = mean(party == 0))

income <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  income <- census_income("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))

race <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  race <- census_race_ethnicity("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))

education <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  education <- census_education("block group", state = "NY", year = 2017, county = c) %>% 
    select(-NAME)
}))

age <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  age <- census_median_age("block group", state = "NY", year = 2017, county = c)
}))

vap <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  vap <- census_vap("block group", state = "NY", year = 2017, county = c) 
}))


block_groups <- left_join(income, left_join(race, left_join(education, left_join(age, left_join(vap, share_dem, by = c("GEOID" = "bg"))))))

lost_voters <- readRDS("./temp/disen_by_bg.rds")

block_groups <- left_join(block_groups, lost_voters, by = c("GEOID" = "bg")) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

block_groups <- block_groups[complete.cases(block_groups), ] %>% 
  mutate(treat = lost_voters > 0)

match_data <- block_groups %>% 
  dplyr::select(median_income, latino, nh_black, nh_white, some_college, median_age, vap, share_dem)

genout <- GenMatch(Tr = block_groups$treat, X = match_data,
                   M = 3, replace = T, pop.size = 1000)