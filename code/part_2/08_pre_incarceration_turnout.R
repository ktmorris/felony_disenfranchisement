#### incarceration history


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
  select(-starts_with("filler")) %>% 
  group_by(din) %>% 
  arrange(desc(release_date)) %>% 
  filter(row_number() == 1)

nys_released_time <- nys_released %>% 
  mutate(time = release_date - latest_reception_date) %>% 
  select(din, time, last, first, middle, dob)


######

parolees <- readRDS("./temp/parolees_with_restoration.rds")

parolees <- left_join(parolees, nys_released_time)

nys_roll <- readRDS("./temp/parolee_to_18.rds")

parolees <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2012-01-01",
         parole_status_date <= "2018-10-12",
         !is.na(dob_parole)) %>% 
  mutate(age = as.numeric((as.Date("2018-11-06") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2018 = ifelse(is.na(v2018), 0, v2018),
         finished_post = parole_status_date >= "2018-05-18",
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008))

model1 <- glm(v2008 ~ I(race == "BLACK") + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2018,
                                                 !finished_post))

model2 <- glm(v2008 ~ I(race == "BLACK") + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = filter(parolees, restored))




parolees$b <- parolees$race == "BLACK"


test <- parolees %>% 
  filter(year(parole_status_date) >= 2017) %>% 
  group_by(finished_post, b) %>% 
  summarize(m = mean(v2008))


summary(glm(I(political_party == "REP") ~ restored,
            data = filter(parolees, year(parole_status_date) >= 2017), family = "binomial"))