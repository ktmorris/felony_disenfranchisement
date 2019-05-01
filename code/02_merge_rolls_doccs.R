## read voter file
# pull everyone who is active now (and registered before 2017 deadline)
# or who is purged now but was registered prior to deadline and purged after election
nys_roll <- dbGetQuery(db, "select last_name, first_name, middle_name, dob, voter_status, nys_id
                       from nys_roll_0319
                       where (voter_status != 'PURGED' and registration_date <= 20171013) or 
                       (voter_status == 'PURGED' and registration_date <= 20171013 and date_purged > 20171107)")

nys_roll <- nys_roll %>% 
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

nys_roll <- setorder(nys_roll, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
nys_roll <- nys_roll[!duplicated(nys_roll$nys_id),]
nys_roll <- select(nys_roll, -a, -b, -c)


nys_roll <- nys_roll %>%
  mutate(middle_name = gsub("[[:punct:]]", "", ifelse(middle_name != "", middle_name, NA))) %>%
  mutate(dob = as.Date(as.character(dob), "%Y%m%d"))

## read doccs data - entered in 2017

doccs_to_rolls <- readRDS("./temp/admit_2017.rds") %>% 
  rename(first_name = first,
         middle_name = middle,
         last_name = last) %>% 
  mutate(middle_name = gsub("[[:punct:]]", "", ifelse(middle_name != "", middle_name, NA)))

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(small, "./temp/matched_ids_enter_17.rds")

## read doccs data - in at all in 2017

doccs_to_rolls <- readRDS("./temp/in_2017.rds") %>% 
  rename(first_name = first,
         middle_name = middle,
         last_name = last) %>% 
  mutate(middle_name = gsub("[[:punct:]]", "", ifelse(middle_name != "", middle_name, NA)))

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(small, "./temp/matched_ids_in_17.rds")