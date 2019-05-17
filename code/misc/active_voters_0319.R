#### count active voters


count <- dbGetQuery(db, "select count(*) from nys_roll_0319 where voter_status == 'ACTIVE'") %>% 
  pull()

saveRDS(count, "./temp/active_voters_0319.rds")