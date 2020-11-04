

############# READ IN NYS VOTER FILES #############
db <- dbConnect(SQLite(), "D:/rolls.db")
nys_roll <- dbGetQuery(db, "select political_party, last_name, first_name, middle_name, dob,
                            voter_status, nys_id from nys_roll_0319")

nys_roll <- nys_roll %>% 
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

nys_roll <- setorder(nys_roll, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
nys_roll <- nys_roll[!duplicated(nys_roll$nys_id),]
nys_roll <- select(nys_roll, -a, -b, -c)


nys_roll <- nys_roll %>%
  mutate_at(vars(last_name, first_name, middle_name),
            ~ tolower(gsub("[[:punct:]]| ", "", ifelse(. == "", NA, .)))) %>% 
  mutate(dob = as.Date(as.character(dob), "%Y%m%d"))

#### MATCH DOC TO VOTER FILE ####

doccs_to_rolls <- readRDS("./temp/parolees_with_restoration.rds")
doccs_to_rolls <- doccs_to_rolls %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ tolower(gsub("[[:punct:]]| ", "", ifelse(. == "", NA, .)))) %>% 
  rename(dob = dob_parole)

merge_list <- match_rolls_to_doc(doccs_to_rolls, "din", nys_roll, "nys_id")

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))
  
saveRDS(small, "./temp/din_nys_parolees.rds")
rm(small, merge_list)

#### plus minus 35 ####

doccs_to_rolls <- doccs_to_rolls %>% 
  mutate(dob = dob + 35)

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(nrow(small), "./temp/p35_2.rds")
rm(small, merge_list)
##
doccs_to_rolls <- doccs_to_rolls %>% 
  mutate(dob = dob - 70)

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(nrow(small), "./temp/m35_2.rds")
rm(small, merge_list)

cleanup()