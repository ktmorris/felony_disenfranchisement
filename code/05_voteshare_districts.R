### get city council race numbers
### probably don't ever re-run - who knows when / if urls will change

dists <- fread("./raw_data/misc/council_dists.csv")

url_1 <- "https://www.vote.nyc.ny.us/downloads/csv/election_results/2017/20171107General%20Election/00"
url_2 <- "022000"
url_3 <- "%20Member%20of%20the%20City%20Council%20"
url_4 <- "%20Council%20District%20EDLevel.csv"

dists$url <- with(dists, paste0(url_1, county_num, url_2, str_pad(as.character(district), width = 2, pad = "0", side = "left"), county, url_3, district, suffix, url_4))

results <- rbindlist(lapply(dists$url, function(url){
  j <- fread(url)
}))

colnames(results) <- gsub("[.]", "_", make.unique(make.names(colnames(results))))

results <- results[grepl("\\(", results$Unit_Name), ] ## drop records that aren't candidates (all candidates have party in parentheses)

## get rid of party names in parentheses - don't care about party
results$Unit_Name <- gsub("\\s*\\([^\\)]+\\)", "", results$Unit_Name)

results_ll <- results %>% 
  group_by(District_Key, Unit_Name) %>% ## votes by candidate, district
  summarize(vote_count = sum(Tally, na.rm = T)) %>% 
  group_by(District_Key) %>% 
  mutate(total_votes = sum(vote_count), ## total votes cast in district
         share_winner = vote_count / total_votes) %>% ## candidate share
  filter(vote_count == max(vote_count)) %>% ## only keep whoever won
  dplyr::select(district = District_Key, share_winner, winner = Unit_Name) ## just keep district name, share won by winner

saveRDS(results_ll, "./temp/council_competitiveness.rds")


######
eds <- results %>% 
  group_by(AD, ED, Unit_Name, District_Key) %>% 
  summarize(tot = sum(Tally, na.rm = T)) %>% 
  group_by(AD, ED, District_Key) %>% 
  mutate(share = tot / sum(tot))

eds <- left_join(eds, results_ll, by = c("District_Key" = "district")) %>% 
  filter(Unit_Name == winner,
         share_winner < 1)

hist(eds$share, breaks = 50)



may <- fread("https://www.vote.nyc.ny.us/downloads/csv/election_results/2018/20180626Primary%20Election/01302000009Kings%20Democratic%20Representative%20in%20Congress%209th%20Congressional%20District%20EDLevel.csv")

may <- may %>% 
  mutate(AD = V12,
         ED = V13,
         Unit_Name = V21,
         Tally = as.integer(gsub(",", "", V22)))

colnames(may) <- gsub("[.]", "_", make.unique(make.names(colnames(may))))

#may2 <- may[grepl("\\(", may$Unit_Name), ] ## drop records that aren't candidates (all candidates have party in parentheses)
may2 <- may %>% filter(Unit_Name %in% c("Adem Bunkeddeko", "Yvette D. Clarke"))

## get rid of party names in parentheses - don't care about party
may2$Unit_Name <- gsub("\\s*\\([^\\)]+\\)", "", may2$Unit_Name)

eds_m <- may2 %>% 
  group_by(AD, ED, Unit_Name) %>% 
  summarize(votes = sum(Tally)) %>% 
  group_by(AD, ED) %>% 
  mutate(share_bdb = votes / sum(votes),
         tot = sum(votes)) %>% 
  filter(Unit_Name == "Adem Bunkeddeko")

hist(filter(eds_m, tot > 10)$share_bdb, breaks = 40)