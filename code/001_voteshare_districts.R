


### get city council race numbers

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

results <- results[grepl("\\(", results$Unit_Name), ]

results$Unit_Name <- gsub("\\s*\\([^\\)]+\\)", "", results$Unit_Name)

results_ll <- results %>% 
  group_by(District_Key, Unit_Name) %>% 
  summarize(vote_count = sum(Tally, na.rm = T)) %>% 
  group_by(District_Key) %>% 
  mutate(total_votes = sum(vote_count),
         share_winner = vote_count / total_votes) %>% 
  filter(vote_count == max(vote_count)) %>% 
  dplyr::select(district = District_Key, share_winner)
  

saveRDS(results_ll, "./temp/council_competitiveness.rds")