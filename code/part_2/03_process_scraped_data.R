

### AFTER THE WEBSCRAPER FINISHES, THIS PROGRAM LOOKS TO SEE WHO'S HAD THEIR RIGHTS RESTORED

m <- fread("./temp/nys_parole_codes.csv")

ids <- m %>% 
  filter(V1 == "DIN:") %>% 
  group_by(V2) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  select(V2, id)

full <- full_join(m, ids, by = "V2")


for (i in 1:100){
  full <- full %>% 
    mutate(id = if_else(is.na(id), lag(id), id))
}

full <- full %>% 
  select(-V3) %>% 
  group_by(V1, id) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(V1 = gsub(" ", "_", gsub(" /", "", gsub(":","",V1)))) %>% 
  spread(V1, V2) %>% 
  mutate(Release_to_parole_supervision = as.Date(Release_to_parole_supervision, "%m/%d/%Y"),
         restored = tolower(Voting_pardon_issued) == "yes") %>% 
  select(din = DIN, restored, prison_release = Release_to_parole_supervision,
         status = Parole_status, effective_date = Effective_date)

rm(m, ids, i)
#### READ IN PAROLEE DATA

names <- fread("./raw_data/doccs_data/parolee_column_names.csv", header = F)$V1

parolees <- read_fwf(
  file = "./raw_data/doccs_data/parolee_data/Parolee.011419.txt",
  fwf_widths(c(1, 9, 3, 10, 3, 7, 3, 30, 3, 6, 3, 10, 3, 15, 3,
               19, 3, 11, 3, 1, 3, 22, 3, 10, 3, 15, 2))
)

colnames(parolees) <- names
rm(names)

parolees <- parolees %>% 
  select(din,
         parole_status = status,
         parole_status_date = status_date,
         release_date_parole = release_date,
         dob_parole = dob,
         parole_name = name,
         -starts_with("filler"),
         county,
         sex,
         race) %>% 
  group_by(din) %>% 
  arrange(desc(parole_status_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

parolees <- cSplit(parolees, "parole_name", sep = ",", direction = "wide", drop = T, type.convert = F)
parolees <- rename(parolees, last_name = parole_name_1, first_name = parole_name_2)
x <- ncol(parolees)
parolees <- cSplit(parolees, "first_name", sep = " ", direction = "wide", drop = T, type.convert = F)
parolees <- parolees[ , 1:(x+1)]
parolees <- rename(parolees, first_name = first_name_1, middle_name = first_name_2)

parolees_with_restoration <- left_join(parolees, full, by = "din")



##### read in parolee crimes

names <- fread("./raw_data/doccs_data/parolee_crimes_column_names.csv", header = F)$V1

parolee_crimes <- read_fwf(
  file = "./raw_data/doccs_data/parolee_data/Parole.Crimes.011419.txt",
  fwf_widths(c(1, 9, 3, 7, 2, 3, 2, 44, 3, 1, 3, 11, 2))
)

colnames(parolee_crimes) <- names
parolee_crimes <- select(parolee_crimes, -starts_with("filler")) %>% 
  mutate(current_felon = crime_class %in% c("1", "2", "3", "B", "C", "D", "E")) %>% 
  group_by(din) %>% 
  summarize(current_felon = max(current_felon),
            counts = n())

parolees_with_restoration <- inner_join(parolees_with_restoration, parolee_crimes, by = "din") %>% 
  filter(current_felon == 1)

saveRDS(parolees_with_restoration, "./temp/parolees_with_restoration.rds")
cleanup()

