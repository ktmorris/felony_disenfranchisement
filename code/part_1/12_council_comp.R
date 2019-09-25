#### any change


dists <- fread("./raw_data/misc/council_dists.csv")

url_1 <- "https://www.vote.nyc.ny.us/downloads/csv/election_results/2017/20170912Primary%20Election/01"
url_2 <- "022000"
url_3 <- "%20Democratic%20Member%20of%20the%20City%20Council%20"
url_4 <- "%20Council%20District%20EDLevel.csv"

dists$url <- with(dists, paste0(url_1, county_num, url_2, str_pad(as.character(district), width = 2, pad = "0", side = "left"), county, url_3, district, suffix, url_4))

results <- rbindlist(lapply(dists$url, function(url){
  if(grepl("3rd|11th|15th|16th|22nd|25th|26th|29th|31st|36th|37th|39th|44th|46th|47th|50th|51st", url)){
    return(data.table(x = ""))
  }else{
    print(url)
    j <- fread(url)
  }
}), fill = T) %>% 
  select(-x)

colnames(results) <- gsub("[.]", "_", make.unique(make.names(colnames(results))))

results <- results %>% 
  filter(!(Unit_Name %in% c("Public Counter", "Manually Counted Emergency", "Affidavit"	, "Absentee / Military"	, "Scattered")))

## get rid of party names in parentheses - don't care about party
results$Unit_Name <- gsub("\\s*\\([^\\)]+\\)", "", results$Unit_Name)

results_ll <- results %>%  
  group_by(District_Key, Unit_Name, AD, ED) %>% ## votes by candidate, district
  summarize(cand_votes = sum(Tally, na.rm = T)) %>% 
  group_by(District_Key, AD, ED) %>% 
  mutate(share = cand_votes / sum(cand_votes),
         vote_count = sum(cand_votes))
  
##### create ed demos
nyc <- readRDS("./temp/nyc.rds")

eds <- dbGetQuery(db, "select nys_id, voter_status, election_district, assembly_district
                       from nys_roll_0418 where county_code in (24, 41, 31, 3, 43)")

eds <- eds %>%
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

eds <- setorder(eds, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
eds <- eds[!duplicated(eds$nys_id),]
eds <- select(eds, -a, -b, -c)

nyc <- left_join(nyc, eds)
nyc$lost_voter <- nyc$nys_id %in% readRDS("./temp/ids_of_lost_voters.rds")$nys_id
nyc <- filter(nyc, match %in% c("Zip8", "Zip9"), (voter_status != "PURGED" | !lost_voter))

load("./temp/census_data_nyc_bgs_tracts.RData")
bgs_census <- geos[[2]]

cleanup("bgs_census", "nyc", "results_ll", "results")

nyc <- left_join(nyc, bgs_census, by = c("bg" = "GEOID"))

nyc <- nyc %>% 
  group_by(bg) %>% 
  mutate(diff = n() / cvap)

eds <- nyc %>% 
  filter(!is.infinite(diff)) %>% 
  group_by(bg) %>% 
  mutate(reg_rate = n() / cvap) %>% 
  group_by(assembly_district, election_district) %>% 
  summarize_at(vars("median_income", "latino", "nh_black", "nh_white", "some_college", "diff", "reg_rate",
                    "share_non_citizen"),
               mean, na.rm = T)

eds <- left_join(eds,
                 nyc %>% 
                   group_by(assembly_district, election_district) %>% 
                   summarize(age = mean(floor(dob / 10000)),
                             lost_voters = sum(lost_voter),
                             voters = n(),
                             share_dem = mean(political_party == "DEM"))) %>% 
  mutate(cvap_est = voters / reg_rate)

results_ll <- left_join(results_ll, eds, by = c("AD" = "assembly_district",
                                                "ED" = "election_district")) %>% 
  mutate(to = vote_count / cvap_est)

results_ll$new_votes <- results_ll$share * results_ll$nh_black * 0.016 * results_ll$lost_voters * results_ll$cvap_est

new <- results_ll %>% 
  filter(vote_count != 0,
         !is.na(new_votes)) %>% 
  group_by(District_Key, Unit_Name) %>% 
  summarize(votes_new = sum(new_votes + cand_votes)) %>% 
  group_by(district = District_Key) %>% 
  mutate(share_new = votes_new / sum(votes_new)) %>% 
  filter(votes_new == max(votes_new))

ds <- results_ll %>% 
  group_by(District_Key, Unit_Name) %>% 
  summarize(votes = sum(new_votes, na.rm = T))

#####
dist_winner <- results %>% 
  group_by(District_Key, Unit_Name) %>% ## votes by candidate, district
  summarize(vote_count = sum(Tally, na.rm = T)) %>% 
  group_by(District_Key) %>% 
  mutate(total_votes = sum(vote_count), ## total votes cast in district
         share_winner = vote_count / total_votes) %>% ## candidate share
  filter(vote_count == max(vote_count)) %>% ## only keep whoever won
  dplyr::select(district = District_Key, share_winner, winner = Unit_Name, vote_count) ## just keep district name, share won by winner

####

tot <- left_join(new, dist_winner, by = "district")
View(filter(tot, Unit_Name != winner))


#####
d30 <- filter(results_ll, District_Key == 30,
              Unit_Name == "Elizabeth S. Crowley",
              vote_count > 0) %>% 
  mutate(share_r = 1 - share,
         other = 1 - nh_black)


sum(filter(d30, lost_voters == 0)$vote_count) / sum(filter(d30, lost_voters == 0)$cvap_est)
sum(filter(d30, lost_voters == 1)$vote_count) / sum(filter(d30, lost_voters == 1)$cvap_est)
#####