## try to understand over / undercount by ed

election_districts <- dbGetQuery(db, "select nys_id, voter_status, election_district, assembly_district from nys_roll_0418")


election_districts <- election_districts %>%
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

election_districts <- setorder(election_districts, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
election_districts <- election_districts[!duplicated(election_districts$nys_id),]
election_districts <- dplyr::select(election_districts, -a, -b, -c)


nyc <- readRDS("./temp/nyc.rds")


nyc <- inner_join(nyc, election_districts)
# read election names
elects <- fread("./raw_data/misc/elects.csv")

history <- cSplit(dplyr::select(nyc, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history")
history <- filter(history, year == 2017, election_type == "general")
nyc$v2017 <- nyc$nys_id %in% history$nys_id

## read in city council races to control for competitiveness
races <- readRDS("./temp/council_competitiveness.rds")
nyc <- left_join(nyc, races, by = c("cc_district" = "district"))

### get census data
load("./temp/census_data_nyc_bgs_tracts.RData")
bgs <- geos[[2]]
nyc <- left_join(nyc, bgs, by = c("bg" = "GEOID"))

####

ed_level <- nyc %>% 
  filter(voter_status != "PURGED") %>% 
  group_by(election_district, assembly_district) %>% 
  summarize(median_income = mean(median_income, na.rm = T),
            latino = mean(latino, na.rm = T),
            nh_black = mean(nh_black, na.rm = T),
            nh_white = mean(nh_white, na.rm = T),
            some_college = mean(some_college, na.rm = T),
            median_age = mean(median_age, na.rm = T),
            share_dem = mean(political_party == "DEM" & !is.na(political_party)),
            share_non_citizen = mean(share_non_citizen, na.rm = T),
            share_winner = mean(share_winner),
            vcount = n(),
            vap = mean(vap, na.rm = T),
            ballots = sum(v2017)) %>% 
  mutate(reg_rate = vcount / vap)

lvs <- nyc %>% 
  filter(nys_id %in% readRDS("./temp/ids_of_lost_voters.rds")$nys_id) %>% 
  group_by(election_district, assembly_district) %>% 
  summarize(lost_voters = n())


ed_level <- left_join(ed_level, lvs, by = c("election_district", "assembly_district"))

ed_level$lost_voters <- ifelse(is.na(ed_level$lost_voters), 0, ed_level$lost_voters)

#################### city level results

results <- fread("https://www.vote.nyc.ny.us/downloads/csv/election_results/2017/20171107General%20Election/00001100000Citywide%20Mayor%20Citywide%20EDLevel.csv")
colnames(results) <- gsub("[.]", "_", make.unique(make.names(colnames(results))))

results <- results[grepl("\\(", results$Unit_Name), ] ## drop records that aren't candidates (all candidates have party in parentheses)

ed_results <- results %>% 
  group_by(election_district = ED,
           assembly_district = AD) %>% 
  summarize(votes = sum(Tally))

ballots <- sum(ed_results$votes)
saveRDS(ballots, "./temp/mayoral_ballots.rds")

ed_comp <- inner_join(ed_level, ed_results)


ed_comp$share <- ed_comp$ballots / ed_comp$votes

ed_comp <- ed_comp[complete.cases(ed_comp), ]
ed_comp <- filter(ed_comp, share != Inf)


ed_reg <- lm(share ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = ed_comp)
ed_comp_ses <- data.frame(summary(lm_robust(share ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = ed_comp, clusters = assembly_district))$coefficients)[, 2]


save(ed_reg, ed_comp_ses, file = "./temp/compare_ballots_off_results.RData")