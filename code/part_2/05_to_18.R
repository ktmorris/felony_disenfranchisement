
elects <- fread("./raw_data/misc/elects.csv")

############# READ IN NYS VOTER FILES #############
db <- dbConnect(SQLite(), "D:/rolls.db")
nys_roll <- dbGetQuery(db, "select history, voter_status, nys_id from nys_roll_0319")

nys_roll <- nys_roll %>% 
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

nys_roll <- setorder(nys_roll, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
nys_roll <- nys_roll[!duplicated(nys_roll$nys_id),]
nys_roll <- select(nys_roll, -a, -b, -c)



nys_roll <- nys_roll[nys_roll$nys_id %in% readRDS("./temp/din_nys_parolees.rds")$nys_id, ]
nys_roll <- left_join(nys_roll, readRDS("./temp/din_nys_parolees.rds"), by = "nys_id")

nys_roll <- cSplit(nys_roll, "history", sep = ";", direction = "long", type.convert = F)

nys_roll <- left_join(nys_roll, elects, by = "history")


nys_roll <- nys_roll %>% 
  group_by(nys_id) %>% 
  mutate(year = ifelse(is.na(year), 0, year),
         v2018 = max(year == 2018 & election_type == "general")) %>% 
  filter(row_number() == 1)

saveRDS(nys_roll, "./temp/parolee_to_18.rds")


parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din")
parolees$v2018 <- ifelse(is.na(parolees$v2018), 0, parolees$v2018)

ll <- parolees %>% 
  filter(parole_status == "DISCHARGED",
         !is.na(restored)) %>% 
  mutate(parole_status_date = as.Date(parole_status_date, "%m/%d/%Y"),
         month_done = make_date(year = year(parole_status_date), month = month(parole_status_date), day = 1)) %>% 
  group_by(month_done, restored) %>% 
  summarize(to = mean(v2018),
            count = n())


ggplot(filter(ll, year(month_done) >= 2018), aes(x = month_done, y = to, color = restored)) + geom_line()

ll2 <- parolees %>% 
  filter(parole_status == "DISCHARGED") %>% 
  mutate(parole_status_date = as.Date(parole_status_date, "%m/%d/%Y"),
         month_done = make_date(year = year(parole_status_date), month = month(parole_status_date), day = 1)) %>% 
  group_by(month_done) %>% 
  summarize(to = mean(v2018),
            count = n())


ggplot(filter(ll2, year(month_done) >= 2018), aes(x = month_done, y = to)) + geom_line()