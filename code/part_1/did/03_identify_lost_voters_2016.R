## identify lost voters

matches <- readRDS("./temp/matched_ids_in_16.rds")
elects <- fread("./raw_data/misc/elects.csv")

###

history <- left_join(dbGetQuery(db, "select nys_id, history, county_code from nys_roll_0418"),
                     readRDS("./temp/nys_0418_geocoded.rds"), by = "nys_id") %>%  
  filter(nys_id %in% matches$nys_id) # just keep formerly incarcerated, registered folks


history <- cSplit(history, "history", sep = ";", direction = "long", type.convert = F)

history <- left_join(history, elects, by = "history") %>% 
  group_by(nys_id) %>% 
  mutate(voted = max(year > 2005 & year <= 2015 & !is.na(year))) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

history$voted <- ifelse(history$voted == 1, "Cast Ballot in Past 10 Years", "Didn't Cast Ballot in Past 10 Years")


lost_ids <- history[history$voted == "Cast Ballot in Past 10 Years", c("nys_id", "match")]
saveRDS(lost_ids, "./temp/ids_of_lost_voters_16.rds")
