## geocode nyc

<<<<<<< HEAD
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
### block group level

share_dem <- nyc %>% 
  group_by(bg) %>% 
  summarize(share_dem = mean(party == 0),
            v2017 = sum(v2017),
            vcount = n())

income <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  income <- census_income("block group", state = "NY", year = 2017, county = c) %>% 
    dplyr::select(-NAME)
}))

race <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  race <- census_race_ethnicity("block group", state = "NY", year = 2017, county = c) %>% 
    dplyr::select(-NAME)
}))

education <- rbindlist(lapply(c("KINGS", "QUEENS", "BRONX", "NEW YORK", "RICHMOND"), function(c){
  education <- census_education("block group", state = "NY", year = 2017, county = c) %>% 
    dplyr::select(-NAME)
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
  mutate(treat = lost_voters > 0,
         to = v2017 / vap)

match_data <- block_groups %>% 
  dplyr::select(median_income, latino, nh_black, nh_white, some_college, median_age, vap, share_dem)

genout <- GenMatch(Tr = block_groups$treat, X = match_data,
                   M = 3, replace = T, pop.size = 1000)
saveRDS(genout, "./temp/genout.rds")

genout <- readRDS("./temp/genout.rds")

treat <- block_groups$treat

X <- block_groups %>% 
  dplyr::select(median_income, latino, nh_black, nh_white, some_college, median_age, vap, share_dem)

mout <- Match(Tr = treat, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 30)
summary(mout)

save(mout, file = "./temp/mout_full.RData")

matches <- data.frame("treated" = mout[["index.treated"]],
                      "control" = mout[["index.control"]])

block_groups <- block_groups %>% 
  mutate(id = row_number())

treat_row <- block_groups %>% 
  filter(treat) %>% 
  select(id, GEOID)

untreat_row <- block_groups %>% 
  filter(!treat) %>% 
  select(id, control_GEOID = GEOID)

matches <- left_join(matches, treat_row, by = c("treated" = "id"))
matches <- left_join(matches, untreat_row, by = c("control" = "id"))

saveRDS(matches, file = "./temp/full_match_output.rds")

matches_v <- as.data.frame(c(matches$GEOID, matches$control_GEOID))
colnames(matches_v) = "GEOID"

match_w <- matches_v %>% 
  group_by(GEOID) %>% 
  summarize(weight = n())

reg <- inner_join(block_groups, match_w, by = "GEOID")

summary(lm(to ~ treat, data = reg, weights = weight))

# 
# ###########
# load("./temp/mout_full.RData")
# load("./temp/post_match_census_data.RData")
# order <- fread("./misc_data/var_orders.csv")
# 
# balance <- MatchBalance(treated ~ latino + nh_black + nh_white + median_income + some_college + cvap_17 +
#                           unem + median_age + share_non_citizen + share_moved + share_no_car +
#                           pop_change + share_republican_2016 + share_reg_14 + total_13, match.out = mout,
#                         data = census_data)
# TrMean <- c()
# PreMean <- c()
# PreQQmed <- c()
# PreQQmean <- c()
# PreQQmax <- c()
# PostMean <- c()
# PostQQmed <- c()
# PostQQmean <- c()
# PostQQmax <- c()
# 
# for(i in c(1:length(balance$BeforeMatching))){
#   TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
#   PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
#   PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
#   PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
#   PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
#   
#   PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
#   PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
#   PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
#   PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
# }
# 
# varnames <- c("latino", "nh_black", "nh_white", "median_income", "some_college", "cvap_17",
#               "unem", "median_age", "share_non_citizen", "share_moved", "share_no_car",
#               "pop_change", "share_republican_2016", "share_reg_14", "total_13")
# 
# 
# df <- data.frame("TrMean" = TrMean,
#                  "TrMean2" = TrMean,
#                  "PreMean" = PreMean,
#                  "PreQQmed" = PreQQmed,
#                  "PreQQmean" = PreQQmean,
#                  "PreQQmax" = PreQQmax,
#                  "PostMean" = PostMean,
#                  "PostQQmed" = PostQQmed,
#                  "PostQQmean" = PostQQmean,
#                  "PostQQmax" = PostQQmax,
#                  "names" = varnames) %>% 
#   mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
#          change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
#          change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
#          change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>% 
#   mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>% 
#   mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2)))
# 
# df <- full_join(df, order, by = c("names" = "variable")) %>%
#   arrange(order) %>% 
#   select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>% 
#   mutate(name = ifelse(name == "County-Level Variables", "State-Level Variables", name)) %>% 
#   filter(!is.na(TrMean))
# 
# colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")
# 
# kable(df, escape = FALSE, align = c('l', rep('c', 99))) %>% 
#   add_header_above(c(" " = 1, "Means: Unmatched Data" = 2, "Means: Matched Data" = 2, "Percent Improvement" = 4), align = "c") %>% 
#   group_rows("Tract-Level Variables", 1, 13) %>% 
#   group_rows("County-Level Variables", 14, 15) %>% 
#   kable_styling(font_size = 12, full_width = F) %>% 
#   save_kable(file = "./output/matches_all.html", self_contained = T)
