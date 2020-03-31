## spatial join nyc
# 
# nyc <- dbGetQuery(db, "select dob, nys_id, voter_status, history, county_code, gender, congressional_district,
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

nyc <- readRDS("./temp/nyc.rds")

## find lost voters
nyc$lost_voter <- nyc$nys_id %in% readRDS("./temp/ids_of_lost_voters.rds")$nys_id

lost_nyc <- sum(nyc$lost_voter)
saveRDS(lost_nyc, "./temp/lost_count_nyc.rds")

bad_geocode_nyc <- mean(!(filter(nyc, voter_status != "PURGED")$match %in% c("Zip8", "Zip9")))
saveRDS(bad_geocode_nyc, "./temp/bad_geocode_nyc.rds")

nyc <- filter(nyc, match %in% c("Zip8", "Zip9"))

lost_bg <- nyc %>% 
  group_by(GEOID = bg) %>% 
  summarize(lost_voters = sum(lost_voter))

lost_tract <- nyc %>% 
  group_by(GEOID = substring(bg, 1, 11)) %>% 
  summarize(lost_voters = sum(lost_voter))

lost_zip <- nyc %>% 
  group_by(GEOID = zip5) %>% 
  summarize(lost_voters = sum(lost_voter))

## read in city council races to control for competitiveness
races <- readRDS("./temp/council_competitiveness.rds")

nyc <- left_join(nyc, races, by = c("cc_district" = "district"))

##### 2017 ballots bg
# read election names
elects <- fread("./raw_data/misc/elects.csv")

history <- cSplit(dplyr::select(nyc, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history")
history <- filter(history, year == 2017, election_type == "general")
nyc$v2017 <- nyc$nys_id %in% history$nys_id
# count ballots in 17

vf_ballots_17 <- sum(nyc$v2017)
saveRDS(vf_ballots_17, "./temp/vf_ballots_17.rds")
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

arrests_precinct <- arrests %>% 
  group_by(ARREST_PRECINCT) %>% 
  filter(substring(ARREST_DATE, 7) == "2017") %>% 
  summarize(share_black = mean(PERP_RACE == "BLACK", na.rm = T),
            n = n()) %>% 
  arrange(desc(n))

arrests_precinct$running <- cumsum(arrests_precinct$n)
arrests_precinct$running_share <- arrests_precinct$running / sum(arrests_precinct$n)

arrests_race <- arrests %>% 
  filter(substring(ARREST_DATE, 7) == "2017") %>% 
  summarize(share_black = mean(PERP_RACE == "BLACK", na.rm = T))

## share dem
nyc <- nyc %>% 
  filter(voter_status != "PURGED")

share_dem_bg <- nyc %>% 
  rename(GEOID = bg) %>% 
  group_by(GEOID, cc_district) %>% 
  mutate(count_d = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_d = max(count_d),
         biggest_district = ifelse(count_d == biggest_d, cc_district, 0)) %>% 
  group_by(GEOID, congressional_district) %>% 
  mutate(count_dcd = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_dcd = max(count_dcd),
         biggest_districtcd = ifelse(count_dcd == biggest_dcd, congressional_district, 0)) %>% 
  summarize(district = max(biggest_district),
            districtcd = max(biggest_districtcd),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            share_winner = mean(share_winner))

share_dem_tract <- nyc %>% 
  mutate(GEOID = substring(bg, 1, 11)) %>% 
  group_by(GEOID, cc_district) %>% 
  mutate(count_d = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_d = max(count_d),
         biggest_district = ifelse(count_d == biggest_d, cc_district, 0)) %>% 
  group_by(GEOID, congressional_district) %>% 
  mutate(count_dcd = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_dcd = max(count_dcd),
         biggest_districtcd = ifelse(count_dcd == biggest_dcd, congressional_district, 0)) %>% 
  summarize(district = max(biggest_district),
            districtcd = max(biggest_districtcd),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            share_winner = mean(share_winner))

share_dem_zip <- nyc %>% 
  rename(GEOID = zip5) %>% 
  group_by(GEOID, cc_district) %>% 
  mutate(count_d = n()) %>% 
  group_by(GEOID) %>% 
  mutate(biggest_d = max(count_d),
         biggest_district = ifelse(count_d == biggest_d, cc_district, 0)) %>% 
  summarize(district = max(biggest_district),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            v2017 = sum(v2017),
            vcount = n(),
            share_winner = mean(share_winner))

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
#         cvap <- census_vap(var, state = s, year = 2017, county = c)
#       }
# 
#       noncit <- census_non_citizen(var, state = s, year = 2017, county = c)
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

#### noncit not available at block group level
noncit <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  noncit <- census_non_citizen("tract", state = "NY", year = 2017, county = c)
}))

geos[[2]]$tract <- substring(geos[[2]]$GEOID, 1, 11)
geos[[2]] <- dplyr::select(geos[[2]], -share_non_citizen)
geos[[2]] <- left_join(geos[[2]], noncit, by = c("tract" = "GEOID"))


save(geos, file = "./temp/census_data_nyc_bgs_tracts.RData")

load("./temp/census_data_nyc_bgs_tracts.RData")

tracts <- left_join(geos[[1]], left_join(share_dem_tract, left_join(arrests_tract, lost_tract))) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

tracts <- tracts[complete.cases(tracts), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / cvap,
         reg_rate = vcount / cvap)

saveRDS(tracts, "./temp/tract_pre_match.rds")

block_groups <- left_join(geos[[2]], left_join(share_dem_bg, left_join(arrests_bg, lost_bg))) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters))

sf <- readRDS("./temp/bg_sf.rds")

block_groups <- left_join(block_groups, sf, by = c("GEOID" = "bg")) %>% 
  mutate(sf = ifelse(is.na(sf), 0, sf / (population / 1000)))

block_groups <- block_groups[complete.cases(block_groups), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / cvap,
         reg_rate = vcount / cvap)

saveRDS(block_groups, "./temp/block_group_pre_match.rds")


zips <- inner_join(mutate(geos[[3]], GEOID = as.integer(GEOID)), left_join(share_dem_zip, lost_zip)) %>% 
  mutate(lost_voters = ifelse(is.na(lost_voters), 0, lost_voters)) %>% 
  filter(v2017 > 100)

zips <- zips[complete.cases(zips), ] %>% 
  mutate(treat = lost_voters > 0,
         to = v2017 / vap,
         reg_rate = vcount / vap)

saveRDS(zips, "./temp/zips_pre_match.rds")

### distributions
block_groups <- readRDS("./temp/block_group_pre_match.rds") %>% 
  group_by(lost_voters = as.character(lost_voters)) %>% 
  summarize(count_bg = n())

tracts <- readRDS("./temp/tract_pre_match.rds") %>% 
  group_by(lost_voters = as.character(lost_voters)) %>% 
  summarize(count_tract = n())

combined <- full_join(block_groups, tracts, by = "lost_voters") %>% 
  arrange(as.integer(lost_voters))

col_sum <- c("Total", sum(combined$count_bg, na.rm = T), sum(combined$count_tract, na.rm = T))

combined <- rbind(combined, col_sum) %>% 
  mutate(count_bg = as.numeric(count_bg),
         count_tract = as.numeric(count_tract))

colnames(combined) <- c("Number of Lost Voters", "Count of Block Groups", "Count of Tracts")
saveRDS(combined, "./temp/distribution_of_lost_voters.rds")
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
  geom_polygon(data = tract_shp, aes(x = long, y = lat, group = group, fill = arrests), color = "black") +
  coord_map() +
  labs(x = NULL, y = NULL) + 
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


#############
bgs_new <- readRDS("./temp/block_group_pre_match.rds")

bgs_new <- rbind(
  bgs_new %>% 
    mutate(group = "former_inc",
           weight = lost_voters),
  bgs_new %>% 
    mutate(group = "overall",
           weight = population)
)

######

tot <- rbindlist(lapply(c("median_income", "median_age", "some_college",
                          "nh_white", "nh_black", "latino",
                          "share_non_citizen", "share_dem"), function(m){
                            ints <- rbindlist(lapply(unique(bgs_new$group), function(r){
                              r <- as.character(r)
                              t <- bgs_new %>% 
                                filter(group == r) %>% 
                                select(weight, measure = m) %>% 
                                filter(!is.na(measure))
                              j <- weighted.ttest.ci((t$measure), weights = t$weight,)
                              j <- data.table(group = c(r),
                                              measure = m,
                                              lower = j[1],
                                              upper = j[2])
                            }))
                            d <- data.table(sig = (ints$lower[1] > ints$upper[2]) | (ints$lower[2] > ints$upper[1]),
                                            measure = m)
                            return(d)
                          }))



ll <- bgs_new %>% 
  group_by(group) %>% 
  summarize_at(vars("median_income", "median_age", "some_college",
                    "nh_white", "nh_black", "latino",
                    "share_non_citizen", "share_dem"),
               ~ weighted.mean(., weight, na.rm = T)) %>% 
  mutate(median_income = dollar(median_income, accuracy = 1),
         median_age = round(median_age, digits = 1)) %>% 
  mutate_at(vars(some_college,
                 nh_white, nh_black, latino,
                 share_non_citizen, share_dem
                 ), ~ percent(., accuracy = 0.1))

ll <- transpose(ll)
ll$var <- c("measure", "median_income", "median_age", "some_college",
            "nh_white", "nh_black", "latino",
            "share_non_citizen", "share_dem")

colnames(ll) <- ll[1,]
ll <- ll[2:nrow(ll),]

ll <- left_join(ll, tot)

ll$measure <- c( "Median Income", "Median Age", "% with Some College",
                 "% Non-Hispanic White", "% Non-Hispanic Black", "% Latino",
                 "% Non-Citizen", "% Democrats")

ll <- ll %>% 
  mutate(measure = ifelse(sig, paste0(measure, "*"), measure)) %>% 
  select(measure, overall, former_inc)

colnames(ll) <- c("Measure", "Average Neighborhood", "Average Neighborhood\\\\for Lost Voters")

saveRDS(ll, "./temp/demos_nhoods.rds")
