######################################################################################################################################
# READ IN DOCCS DATA

names = read.csv("./raw_data/doccs_data/incarcerated_column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

nys_released = read_fwf(
  file = "./raw_data/doccs_data/releasees/Released.010190.113018.txt",
  fwf_widths(c(7, 18, 11, 1, 3, 8, 1, 8, 1, 1, 1, 8, 8, # PDF FROM FOIL REQUEST INCLUDES WIDTH OF EACH VARIABLE
               3, 12, 01, 3, 1, 1, 4, 30, 1, 1, 3, 2, 
               12, 14, 1, 1, 4, 30, 1, 1, 3, 2, 12, 14, 1, 
               1, 4, 30, 1, 1, 3, 2, 12, 14, 8, 8, 6, 
               4, 8, 8, 6, 4, 8, 8, 3, 27, 8))
)

colnames(nys_released) = names
rm(names)

nys_released <- nys_released %>%
  mutate(dob = as.Date(as.character(dob), "%Y%m%d"),
         original_reception_date = as.Date(as.character(original_reception_date), "%Y%m%d"),
         latest_reception_date = as.Date(as.character(latest_reception_date), "%Y%m%d"),
         parole_eligible_date = as.Date(as.character(parole_eligible_date), "%Y%m%d"),
         release_date = as.Date(as.character(release_date), "%Y%m%d")) %>% 
  select(-starts_with("filler"))

#########################################
######### NOW READ PEOPLE CURRENTLY INCARCERATED
names = read.csv("./raw_data/doccs_data/currently_incarcerated/column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

incarcerated = read_fwf(
  file = "./raw_data/doccs_data/currently_incarcerated/UnderCustody.120118.txt",
  fwf_widths(c(7, 18, 11, 1, 3, 8, 1, 8, 1, 1, 1, 8, 8, # PDF FROM FOIL REQUEST INCLUDES WIDTH OF EACH VARIABLE
               3, 12, 01, 3, 1, 1, 4, 30, 1, 1, 3, 2, 
               12, 14, 1, 1, 4, 30, 1, 1, 3, 2, 12, 14, 1, 
               1, 4, 30, 1, 1, 3, 2, 12, 14, 8, 8, 8, 6, 
               4, 8, 8, 6, 4, 8, 8, 9))
)
colnames(incarcerated) = names
rm(names)

saveRDS(nrow(incarcerated), "./temp/number_incarcerated.rds")

share_black <- mean(incarcerated$race == "B", na.rm = T)

saveRDS(share_black, "./temp/share_black_incarcerated.rds")

incarcerated <- incarcerated %>%
  mutate(dob = as.Date(as.character(dob), "%Y%m%d"),
         original_reception_date = as.Date(as.character(original_reception_date), "%Y%m%d"),
         latest_reception_date = as.Date(as.character(latest_reception_date), "%Y%m%d"),
         parole_eligible_date = as.Date(as.character(parole_eligible_date), "%Y%m%d"),
         parole_hearing_date = as.character(parole_hearing_date),
         conditional_release_date = as.character(conditional_release_date))

all_doccs <- bind_rows(incarcerated, nys_released)%>% 
  mutate(current_felon = crime_1_class %in% c("1", "2", "3", "B", "C", "D", "E") |
           crime_2_class %in% c("1", "2", "3", "B", "C", "D", "E") |
           crime_3_class %in% c("1", "2", "3", "B", "C", "D", "E"),
         ex_felon = 1*(most_serious_prior == 1 | current_felon == T),
         ex_felon = ifelse(is.na(ex_felon), 0, ex_felon)) %>% 
  group_by(din) %>% 
  mutate(ex_felon = max(ex_felon)) %>% 
  ungroup()

saveRDS(all_doccs, "./temp/all_doccs.rds")
rm(incarcerated, nys_released)


entered_in_2017 <- all_doccs %>% 
  filter(gsub("[-]", "", latest_reception_date) >= "20170000",
         gsub("[-]", "", latest_reception_date) < "20171107",
         current_felon == T) %>% 
  group_by(din) %>% 
  filter(row_number() == 1)

saveRDS(entered_in_2017, "./temp/admit_2017.rds")


### INDIVIDUALS WHO WERE INCARCERATED AS OF 2017 ELECTION
in_2017 <- all_doccs %>% 
  filter(gsub("[-]", "", latest_reception_date) < "20171107",
         (release_date > "2017-11-07" | is.na(release_date)),
         current_felon == T) %>% 
  group_by(din) %>% 
  filter(row_number() == 1) %>% 
  select(din, last, first, middle, dob)

### INDIVIDUALS ON PAROLE AS OF 2017 ELECTION
names <- read.csv("./raw_data/doccs_data/parolee_column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

parolees <- read_fwf(
  file = "./raw_data/doccs_data/parolee_data/Parolee.011419.txt",
  fwf_widths(c(1, 9, 3, 10, 3, 7, 3, 30, 3, 6, 3, 10, 3, 15, 3,
               19, 3, 11, 3, 1, 3, 22, 3, 10, 3, 15, 2))
)

colnames(parolees) = names
rm(names)

parolees <- select(parolees, -starts_with("filler"))

###
names <- read.csv("./raw_data/doccs_data/parolee_crimes_column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

parolee_crimes <- read_fwf(
  file = "./raw_data/doccs_data/parolee_data/Parole.Crimes.011419.txt",
  fwf_widths(c(1, 9, 3, 7, 2, 3, 2, 44, 3, 1, 3, 11, 2))
)

colnames(parolee_crimes) = names
rm(names)

parolee_crimes <- parolee_crimes[parolee_crimes$crime_class %in% c("A", "B", "C", "D", "E"), ]$din

parolees_felons <- parolees %>% 
  filter(din %in% parolee_crimes,
         release_date <= "2017-11-07",
         (status == "ACTIVE" | (status_date > "2017-11-07")))

parolees_felons <- cSplit(parolees_felons, "name", ",")
parolees_felons <- cSplit(parolees_felons, "name_2", " ")

parolees_felons <- parolees_felons %>% 
  select(-name_2_3) %>% 
  rename(last = name_1,
         first = name_2_1,
         middle = name_2_2)

rm(parolee_crimes, parolees)
  
parolees_felons <- parolees_felons %>% 
  select(din, dob, first, last, middle)

## COMBINE TO FIND ALL DISENFRANCHISED AS OF ELECTION IN 2017
in_2017 <- bind_rows(in_2017, parolees_felons)

unique_17 <- in_2017 %>% 
  group_by(dob, first, last, middle) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

saveRDS(unique_17, "./temp/in_2017.rds")